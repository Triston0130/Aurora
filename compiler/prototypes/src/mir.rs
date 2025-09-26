//! Mid-level intermediate representation (MIR) prototype.
//!
//! This mirrors the current toy HIR surface while moving a step closer to the
//! block-based form we will eventually optimise and lower to the backend.  The
//! implementation intentionally handles the constructs present in the current
//! prototypes (variable bindings, calls, simple control flow) so we can exercise
//! the IR pipeline end-to-end.

use std::collections::{HashMap, HashSet};

use aurora_effect_solver::EffectRow;

use crate::ast::Literal;
use crate::constraints::SolvedAnalysis;
use crate::hir::{HirBlock, HirExpr, HirExprKind, HirFunction, HirModule, HirStmt};
use crate::span::Span;
use crate::types::Type;
use crate::zone::ZoneDescriptor;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegionId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct MirPhi {
    pub target: String,
    pub sources: Vec<(BlockId, MirOperand)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirModule {
    pub functions: Vec<MirFunction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirFunction {
    pub name: String,
    pub params: Vec<MirParam>,
    pub return_type: Type,
    pub effects: EffectRow,
    pub zone: Option<ZoneDescriptor>,
    pub body: MirBody,
    pub trait_obligations: Vec<MirTraitObligation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirBody {
    pub locals: Vec<MirLocal>,
    pub entry: BlockId,
    pub blocks: Vec<MirBlock>,
    pub cfg: ControlFlowGraph,
    pub drop_schedule: DropSchedule,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirTraitObligation {
    pub subject: Type,
    pub trait_path: Vec<String>,
    pub generics: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirLocal {
    pub name: String,
    pub ty: Option<Type>,
    pub region: RegionId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirBlock {
    pub id: BlockId,
    pub phi_nodes: Vec<MirPhi>,
    pub statements: Vec<MirStatement>,
    pub terminator: MirTerminator,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ControlFlowGraph {
    pub predecessors: Vec<Vec<BlockId>>,
    pub successors: Vec<Vec<BlockId>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DropSchedule {
    pub per_block: Vec<Vec<DropAction>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DropAction {
    pub local: String,
    pub region: RegionId,
    pub span: Span,
    pub reason: DropReason,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DropReason {
    ScopeEnd,
    ZoneTransfer { zone: String },
}

impl MirTerminator {
    fn successors(&self) -> Vec<BlockId> {
        match self {
            MirTerminator::Return(_) => Vec::new(),
            MirTerminator::Jump { target } => vec![*target],
            MirTerminator::Branch {
                then_block,
                else_block,
                ..
            } => vec![*then_block, *else_block],
        }
    }
}

impl ControlFlowGraph {
    fn from_body(_entry: BlockId, blocks: &[MirBlock]) -> Self {
        let mut predecessors = vec![Vec::new(); blocks.len()];
        let mut successors = vec![Vec::new(); blocks.len()];
        for block in blocks {
            let idx = block.id.0;
            for succ in block.terminator.successors() {
                successors[idx].push(succ);
                if let Some(preds) = predecessors.get_mut(succ.0) {
                    preds.push(block.id);
                }
            }
        }
        ControlFlowGraph {
            predecessors,
            successors,
        }
    }
}

impl DropSchedule {
    fn from_body(locals: &[MirLocal], blocks: &[MirBlock]) -> Self {
        let mut per_block = vec![Vec::new(); blocks.len()];
        for local in locals {
            let needs_drop = match &local.ty {
                Some(ty) => !is_copy_type(ty),
                None => true,
            };
            if !needs_drop {
                continue;
            }
            let idx = local.region.0;
            if idx >= per_block.len() {
                continue;
            }
            let span = blocks[idx].span;
            per_block[idx].push(DropAction {
                local: local.name.clone(),
                region: local.region,
                span,
                reason: DropReason::ScopeEnd,
            });
        }
        DropSchedule { per_block }
    }
}

fn is_copy_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Primitive(
            crate::types::PrimitiveType::Bool
                | crate::types::PrimitiveType::Int32
                | crate::types::PrimitiveType::Int64
                | crate::types::PrimitiveType::Float32
                | crate::types::PrimitiveType::Float64
                | crate::types::PrimitiveType::String
                | crate::types::PrimitiveType::Unit
        )
    )
}

fn insert_phi_nodes(blocks: &mut [MirBlock], cfg: &ControlFlowGraph) {
    for (block_index, preds) in cfg.predecessors.iter().enumerate() {
        if preds.len() < 2 {
            continue;
        }

        let mut phi_sources: HashMap<String, Vec<(BlockId, MirOperand)>> = HashMap::new();
        for pred in preds {
            let pred_idx = pred.0;
            if let Some(MirStatement::Assign { target, value }) = blocks[pred_idx].statements.last()
            {
                if let Some(op) = value.as_operand() {
                    phi_sources
                        .entry(target.clone())
                        .or_default()
                        .push((*pred, op));
                }
            }
        }

        if phi_sources.is_empty() {
            continue;
        }

        let mut candidates: Vec<(String, Vec<(BlockId, MirOperand)>)> = Vec::new();
        for (target, sources) in phi_sources.into_iter() {
            if sources.len() != preds.len() {
                continue;
            }
            if blocks[block_index]
                .phi_nodes
                .iter()
                .any(|existing| existing.target == target)
            {
                continue;
            }
            candidates.push((target, sources));
        }

        if candidates.is_empty() {
            continue;
        }

        for (target, sources) in candidates {
            for (pred_id, _) in &sources {
                let pred_idx = pred_id.0;
                let statements = &mut blocks[pred_idx].statements;
                if matches!(
                    statements.last(),
                    Some(MirStatement::Assign {
                        target: stmt_target,
                        ..
                    }) if stmt_target == &target
                ) {
                    statements.pop();
                }
            }
            blocks[block_index]
                .phi_nodes
                .push(MirPhi { target, sources });
        }
    }
}

pub struct MirOptimizer;

impl MirOptimizer {
    pub fn run(function: &mut MirFunction) {
        DeadCodeEliminator::run(&mut function.body);
        RefCountElision::run(&mut function.body);
        ZoneTransformer::run(function);
    }
}

struct DeadCodeEliminator;

impl DeadCodeEliminator {
    fn run(body: &mut MirBody) {
        let usage = Self::collect_usage(body);
        for block in &mut body.blocks {
            block.statements.retain(|stmt| match stmt {
                MirStatement::Assign { target, value } => {
                    let used = usage.get(target).copied().unwrap_or(0) > 0;
                    used || !Self::is_effect_free(value)
                }
                MirStatement::Expr(value) => !Self::is_effect_free(value),
            });
        }
    }

    fn collect_usage(body: &MirBody) -> HashMap<String, usize> {
        let mut usage = HashMap::new();
        for block in &body.blocks {
            for phi in &block.phi_nodes {
                for (_, operand) in &phi.sources {
                    Self::record_operand(operand, &mut usage);
                }
            }
            for stmt in &block.statements {
                match stmt {
                    MirStatement::Assign { value, .. } => Self::record_rvalue(value, &mut usage),
                    MirStatement::Expr(value) => Self::record_rvalue(value, &mut usage),
                }
            }
            Self::record_terminator(&block.terminator, &mut usage);
        }
        for actions in &body.drop_schedule.per_block {
            for action in actions {
                *usage.entry(action.local.clone()).or_insert(0) += 1;
            }
        }
        usage
    }

    fn record_rvalue(value: &MirRvalue, usage: &mut HashMap<String, usize>) {
        match value {
            MirRvalue::Literal(_) => {}
            MirRvalue::Local(name) => {
                *usage.entry(name.clone()).or_insert(0) += 1;
            }
            MirRvalue::Call { args, .. } => {
                for arg in args {
                    Self::record_operand(arg, usage);
                }
            }
            MirRvalue::Binary { left, right, .. } => {
                Self::record_operand(left, usage);
                Self::record_operand(right, usage);
            }
        }
    }

    fn record_operand(operand: &MirOperand, usage: &mut HashMap<String, usize>) {
        if let MirOperand::Local(name) = operand {
            *usage.entry(name.clone()).or_insert(0) += 1;
        }
    }

    fn record_terminator(terminator: &MirTerminator, usage: &mut HashMap<String, usize>) {
        match terminator {
            MirTerminator::Return(Some(op)) => Self::record_operand(op, usage),
            MirTerminator::Branch { cond, .. } => Self::record_operand(cond, usage),
            MirTerminator::Jump { .. } | MirTerminator::Return(None) => {}
        }
    }

    fn is_effect_free(value: &MirRvalue) -> bool {
        matches!(value, MirRvalue::Literal(_) | MirRvalue::Local(_))
    }
}

struct RefCountElision;

impl RefCountElision {
    fn run(body: &mut MirBody) {
        let local_map: HashMap<&str, &MirLocal> = body
            .locals
            .iter()
            .map(|local| (local.name.as_str(), local))
            .collect();
        for actions in &mut body.drop_schedule.per_block {
            actions.retain(|action| {
                if let Some(local) = local_map.get(action.local.as_str()) {
                    local.ty.is_some()
                } else {
                    true
                }
            });
        }
    }
}

struct ZoneTransformer;

impl ZoneTransformer {
    fn run(function: &mut MirFunction) {
        let descriptor = match &function.zone {
            Some(zone) => zone.clone(),
            None => return,
        };
        for actions in &mut function.body.drop_schedule.per_block {
            for action in actions {
                if matches!(action.reason, DropReason::ScopeEnd) {
                    action.reason = DropReason::ZoneTransfer {
                        zone: descriptor.zone_type.clone(),
                    };
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirStatement {
    Assign { target: String, value: MirRvalue },
    Expr(MirRvalue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirRvalue {
    Literal(Literal),
    Local(String),
    Call {
        callee: String,
        args: Vec<MirOperand>,
    },
    Binary {
        op: crate::ast::BinaryOp,
        left: MirOperand,
        right: MirOperand,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirOperand {
    Literal(Literal),
    Local(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirTerminator {
    Return(Option<MirOperand>),
    Jump {
        target: BlockId,
    },
    Branch {
        cond: MirOperand,
        then_block: BlockId,
        else_block: BlockId,
    },
}

pub struct HirToMir<'a> {
    _analysis: &'a SolvedAnalysis,
}

impl<'a> HirToMir<'a> {
    pub fn new(analysis: &'a SolvedAnalysis) -> Self {
        Self {
            _analysis: analysis,
        }
    }

    pub fn lower_module(&self, module: &HirModule) -> MirModule {
        MirModule {
            functions: module
                .functions
                .iter()
                .map(|func| self.lower_function(func))
                .collect(),
        }
    }

    fn lower_function(&self, func: &HirFunction) -> MirFunction {
        let params = func
            .params
            .iter()
            .enumerate()
            .map(|(index, param)| MirParam {
                name: param.name.clone(),
                ty: func
                    .param_types
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| Type::Primitive(crate::types::PrimitiveType::Unit)),
            })
            .collect();

        let mut ctx = FunctionLoweringContext::new();
        for (param, ty) in func.params.iter().zip(func.param_types.iter()) {
            ctx.declare_local(param.name.clone(), Some(ty.clone()));
        }

        self.lower_block_statements(&mut ctx, &func.body);

        if !ctx.is_current_terminated() {
            if let Some(expr) = &func.body.result {
                let operand = self.lower_expr_operand(&mut ctx, expr.as_ref());
                ctx.set_terminator(MirTerminator::Return(Some(operand)));
            } else {
                ctx.set_terminator(MirTerminator::Return(None));
            }
        }

        let trait_obligations = func
            .trait_obligations
            .iter()
            .map(|obligation| MirTraitObligation {
                subject: obligation.subject.clone(),
                trait_path: obligation.trait_path.clone(),
                generics: obligation.generics.clone(),
            })
            .collect();

        let mut function = MirFunction {
            name: func.name.clone(),
            params,
            return_type: func.return_type.clone(),
            effects: func.inferred_effects.clone(),
            zone: func.zone.clone(),
            body: ctx.finish(),
            trait_obligations,
        };

        MirOptimizer::run(&mut function);

        function
    }

    fn lower_block_statements(&self, ctx: &mut FunctionLoweringContext, block: &HirBlock) {
        ctx.set_current_block_span(block.span);
        for stmt in &block.statements {
            if ctx.is_current_terminated() {
                break;
            }
            self.lower_stmt(ctx, stmt);
        }
    }

    fn lower_stmt(&self, ctx: &mut FunctionLoweringContext, stmt: &HirStmt) {
        match stmt {
            HirStmt::Let { name, ty, expr, .. } => {
                ctx.declare_local(name.clone(), Some(ty.clone()));
                let operand = self.lower_expr_operand(ctx, expr);
                let rvalue = MirRvalue::from_operand(operand);
                ctx.emit_assign(name.clone(), rvalue);
            }
            HirStmt::Expr { expr, .. } => self.lower_expr_stmt(ctx, expr),
            HirStmt::Return { expr, .. } => {
                let operand = self.lower_expr_operand(ctx, expr);
                ctx.set_terminator(MirTerminator::Return(Some(operand)));
            }
            HirStmt::Item { .. } => {}
        }
    }

    fn lower_expr_stmt(&self, ctx: &mut FunctionLoweringContext, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.lower_if(
                    ctx,
                    condition.as_ref(),
                    then_branch.as_ref(),
                    else_branch.as_deref(),
                    None,
                );
            }
            HirExprKind::Block(block) => {
                self.lower_block_expr(ctx, block);
            }
            HirExprKind::Handle { body, .. } => {
                let _ = self.lower_expr_operand(ctx, body.as_ref());
            }
            _ => {
                let _ = self.lower_expr_operand(ctx, expr);
            }
        }
    }

    fn lower_if(
        &self,
        ctx: &mut FunctionLoweringContext,
        condition: &HirExpr,
        then_block: &HirBlock,
        else_branch: Option<&HirExpr>,
        dest: Option<String>,
    ) -> Option<String> {
        let cond = self.lower_expr_operand(ctx, condition);
        let then_block_id = ctx.new_block();
        let join_block_id = ctx.new_block();
        let needs_else_block = else_branch.is_some() || dest.is_some();
        let else_block_id = if needs_else_block {
            ctx.new_block()
        } else {
            join_block_id
        };

        ctx.set_terminator(MirTerminator::Branch {
            cond,
            then_block: then_block_id,
            else_block: else_block_id,
        });

        ctx.switch_to(then_block_id);
        self.lower_block_statements(ctx, then_block);
        if !ctx.is_current_terminated() {
            if let Some(dest_name) = dest.as_ref() {
                let value = if let Some(expr) = then_block.result.as_ref() {
                    self.lower_expr_operand(ctx, expr.as_ref())
                } else {
                    MirOperand::Literal(Literal::Unit)
                };
                ctx.emit_assign(dest_name.clone(), MirRvalue::from_operand(value));
            }
            ctx.set_terminator(MirTerminator::Jump {
                target: join_block_id,
            });
        }

        if else_block_id != join_block_id {
            ctx.switch_to(else_block_id);
            match else_branch {
                Some(expr) => match &expr.kind {
                    HirExprKind::Block(block) => {
                        self.lower_block_statements(ctx, block);
                        if !ctx.is_current_terminated() {
                            if let Some(dest_name) = dest.as_ref() {
                                let value = if let Some(result_expr) = block.result.as_ref() {
                                    self.lower_expr_operand(ctx, result_expr.as_ref())
                                } else {
                                    MirOperand::Literal(Literal::Unit)
                                };
                                ctx.emit_assign(dest_name.clone(), MirRvalue::from_operand(value));
                            }
                        }
                    }
                    HirExprKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        self.lower_if(
                            ctx,
                            condition.as_ref(),
                            then_branch.as_ref(),
                            else_branch.as_deref(),
                            dest.clone(),
                        );
                    }
                    _ => {
                        let value = self.lower_expr_operand(ctx, expr);
                        if let Some(dest_name) = dest.as_ref() {
                            ctx.emit_assign(dest_name.clone(), MirRvalue::from_operand(value));
                        }
                    }
                },
                None => {
                    if let Some(dest_name) = dest.as_ref() {
                        ctx.emit_assign(dest_name.clone(), MirRvalue::Literal(Literal::Unit));
                    }
                }
            }
            if !ctx.is_current_terminated() {
                ctx.set_terminator(MirTerminator::Jump {
                    target: join_block_id,
                });
            }
        }

        ctx.switch_to(join_block_id);
        dest
    }

    fn lower_block_expr(&self, ctx: &mut FunctionLoweringContext, block: &HirBlock) -> MirOperand {
        self.lower_block_statements(ctx, block);
        if ctx.is_current_terminated() {
            MirOperand::Literal(Literal::Unit)
        } else if let Some(expr) = &block.result {
            self.lower_expr_operand(ctx, expr.as_ref())
        } else {
            MirOperand::Literal(Literal::Unit)
        }
    }

    fn lower_expr_operand(&self, ctx: &mut FunctionLoweringContext, expr: &HirExpr) -> MirOperand {
        match &expr.kind {
            HirExprKind::Literal(lit) => MirOperand::Literal(lit.clone()),
            HirExprKind::Variable(name) => MirOperand::Local(name.clone()),
            HirExprKind::Tuple(elements) => {
                for element in elements {
                    self.lower_expr_operand(ctx, element);
                }
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Array(elements) => {
                for element in elements {
                    self.lower_expr_operand(ctx, element);
                }
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Call { callee, args } => {
                let callee_name = match &callee.kind {
                    HirExprKind::Variable(name) => name.clone(),
                    _ => "<unsupported-callee>".to_string(),
                };
                let lowered_args = args
                    .iter()
                    .map(|arg| self.lower_expr_operand(ctx, arg))
                    .collect();
                let temp = ctx.make_temp(None);
                ctx.emit_assign(
                    temp.clone(),
                    MirRvalue::Call {
                        callee: callee_name,
                        args: lowered_args,
                    },
                );
                MirOperand::Local(temp)
            }
            HirExprKind::Binary { op, left, right } => {
                let left_op = self.lower_expr_operand(ctx, left);
                let right_op = self.lower_expr_operand(ctx, right);
                let temp = ctx.make_temp(None);
                ctx.emit_assign(
                    temp.clone(),
                    MirRvalue::Binary {
                        op: *op,
                        left: left_op,
                        right: right_op,
                    },
                );
                MirOperand::Local(temp)
            }
            HirExprKind::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                let mut lowered_args: Vec<MirOperand> = Vec::new();
                lowered_args.push(self.lower_expr_operand(ctx, receiver));
                lowered_args.extend(args.iter().map(|arg| self.lower_expr_operand(ctx, arg)));
                let temp = ctx.make_temp(None);
                ctx.emit_assign(
                    temp.clone(),
                    MirRvalue::Call {
                        callee: method.clone(),
                        args: lowered_args,
                    },
                );
                MirOperand::Local(temp)
            }
            HirExprKind::Unary { expr: inner, .. } => self.lower_expr_operand(ctx, inner),
            HirExprKind::Field { base, .. } => self.lower_expr_operand(ctx, base),
            HirExprKind::Index { base, .. } => self.lower_expr_operand(ctx, base),
            HirExprKind::Block(block) => self.lower_block_expr(ctx, block),
            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let temp = ctx.make_temp(None);
                self.lower_if(
                    ctx,
                    condition.as_ref(),
                    then_branch.as_ref(),
                    else_branch.as_deref(),
                    Some(temp.clone()),
                );
                MirOperand::Local(temp)
            }
            HirExprKind::Handle { body, .. } => self.lower_expr_operand(ctx, body),
            HirExprKind::Loop(body) => {
                self.lower_block_statements(ctx, body);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::While { condition, body } => {
                self.lower_expr_operand(ctx, condition);
                self.lower_block_statements(ctx, body);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::For { iterator, body, .. } => {
                self.lower_expr_operand(ctx, iterator);
                self.lower_block_statements(ctx, body);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Actor(_) => MirOperand::Literal(Literal::Unit),
            HirExprKind::Match { scrutinee, arms } => {
                self.lower_expr_operand(ctx, scrutinee);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.lower_expr_operand(ctx, guard);
                    }
                    self.lower_expr_operand(ctx, &arm.body);
                }
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Async(block) => {
                self.lower_block_statements(ctx, block);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Await(inner) => self.lower_expr_operand(ctx, inner),
            HirExprKind::Spawn(task) => {
                self.lower_expr_operand(ctx, task);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Zone { body, .. } => {
                self.lower_block_statements(ctx, body);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Assignment { target, value, .. } => {
                self.lower_expr_operand(ctx, target);
                self.lower_expr_operand(ctx, value);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Break(value) => {
                if let Some(expr) = value {
                    self.lower_expr_operand(ctx, expr);
                }
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Continue => MirOperand::Literal(Literal::Unit),
            HirExprKind::Closure { body, .. } => {
                self.lower_block_statements(ctx, body);
                MirOperand::Literal(Literal::Unit)
            }
            HirExprKind::Opaque(_) => MirOperand::Literal(Literal::Unit),
        }
    }
}

#[derive(Debug)]
struct FunctionLoweringContext {
    blocks: Vec<BlockState>,
    current: BlockId,
    locals: Vec<MirLocal>,
    local_set: HashSet<String>,
    temp_counter: usize,
}

impl FunctionLoweringContext {
    fn new() -> Self {
        FunctionLoweringContext {
            blocks: vec![BlockState::new(BlockId(0))],
            current: BlockId(0),
            locals: Vec::new(),
            local_set: HashSet::new(),
            temp_counter: 0,
        }
    }

    fn declare_local(&mut self, name: String, ty: Option<Type>) {
        if self.local_set.insert(name.clone()) {
            self.locals.push(MirLocal {
                name,
                ty,
                region: RegionId(self.current.0),
            });
        }
    }

    fn emit_assign(&mut self, target: String, value: MirRvalue) {
        self.current_block_mut()
            .statements
            .push(MirStatement::Assign { target, value });
    }

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(BlockState::new(id));
        id
    }

    fn switch_to(&mut self, id: BlockId) {
        self.current = id;
    }

    fn set_terminator(&mut self, term: MirTerminator) {
        self.current_block_mut().terminator = Some(term);
    }

    fn is_current_terminated(&self) -> bool {
        self.current_block().terminator.is_some()
    }

    fn make_temp(&mut self, ty: Option<Type>) -> String {
        let name = format!("_tmp{}", self.temp_counter);
        self.temp_counter += 1;
        self.declare_local(name.clone(), ty);
        name
    }

    fn current_block_mut(&mut self) -> &mut BlockState {
        &mut self.blocks[self.current.0]
    }

    fn current_block(&self) -> &BlockState {
        &self.blocks[self.current.0]
    }

    fn set_current_block_span(&mut self, span: Span) {
        self.current_block_mut().set_span(span);
    }

    fn finish(self) -> MirBody {
        let FunctionLoweringContext { blocks, locals, .. } = self;
        let mut blocks: Vec<MirBlock> =
            blocks.into_iter().map(|state| state.into_block()).collect();
        let cfg = ControlFlowGraph::from_body(BlockId(0), &blocks);
        insert_phi_nodes(&mut blocks, &cfg);
        let drop_schedule = DropSchedule::from_body(&locals, &blocks);
        MirBody {
            locals,
            entry: BlockId(0),
            blocks,
            cfg,
            drop_schedule,
        }
    }
}

#[derive(Debug)]
struct BlockState {
    id: BlockId,
    phi_nodes: Vec<MirPhi>,
    statements: Vec<MirStatement>,
    terminator: Option<MirTerminator>,
    span: Span,
}

impl BlockState {
    fn new(id: BlockId) -> Self {
        BlockState {
            id,
            phi_nodes: Vec::new(),
            statements: Vec::new(),
            terminator: None,
            span: Span::default(),
        }
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn into_block(mut self) -> MirBlock {
        let terminator = self
            .terminator
            .take()
            .unwrap_or(MirTerminator::Return(None));
        MirBlock {
            id: self.id,
            phi_nodes: self.phi_nodes,
            statements: self.statements,
            terminator,
            span: self.span,
        }
    }
}

impl MirRvalue {
    fn from_operand(op: MirOperand) -> MirRvalue {
        match op {
            MirOperand::Literal(lit) => MirRvalue::Literal(lit),
            MirOperand::Local(name) => MirRvalue::Local(name),
        }
    }

    fn as_operand(&self) -> Option<MirOperand> {
        match self {
            MirRvalue::Literal(lit) => Some(MirOperand::Literal(lit.clone())),
            MirRvalue::Local(name) => Some(MirOperand::Local(name.clone())),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Attribute, AttributeArg, Block, EffectExpr, EffectLabel, Expr, ExprKind, FunctionDecl,
        FunctionSig, GenericParams, Item, Literal, Module, NodeId, PathExpr, PathSegment, PathType,
        Pattern, Statement, TypeExpr, ZoneDecl,
    };
    use crate::constraints::Generator;
    use crate::hir::HirBuilder;
    use crate::span::Span;
    use crate::types::{InferenceContext, PrimitiveType, TypeEnv};
    use aurora_effect_solver::EffectRow;

    fn build_sample_module() -> Module {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        Module {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Zone(ZoneDecl {
                    attributes: Vec::new(),
                    docs: Vec::new(),
                    name: "gpu".into(),
                    args: Vec::new(),
                    body: Block {
                        statements: Vec::new(),
                        tail: None,
                        span: Span::default(),
                    },
                    span: Span::default(),
                    id: fresh_id(),
                }),
                Item::Function(FunctionDecl {
                    attributes: vec![Attribute {
                        name: "zone".into(),
                        args: vec![AttributeArg::Expr(zone_ident("gpu"))],
                        span: Span::default(),
                    }],
                    docs: Vec::new(),
                    visibility: crate::ast::Visibility::Private,
                    signature: FunctionSig {
                        constness: false,
                        asyncness: false,
                        name: "kernel".into(),
                        generics: GenericParams::default(),
                        params: vec![],
                        return_type: Some(crate::ast::TypeExpr::Unit),
                        effect: Some(EffectExpr {
                            labels: vec![EffectLabel {
                                name: "Compute".into(),
                                args: Vec::new(),
                            }],
                            tail: None,
                        }),
                        where_clause: Vec::new(),
                    },
                    body: Block {
                        statements: vec![Statement::Expr(
                            Expr::dummy(ExprKind::Call {
                                callee: Box::new(Expr::path(PathExpr {
                                    leading_colon: false,
                                    segments: vec![PathSegment {
                                        ident: "launch".into(),
                                        generics: Vec::new(),
                                    }],
                                    span: Span::default(),
                                })),
                                args: vec![Expr::literal(Literal::Integer("1".into()))],
                            }),
                            Span::default(),
                        )],
                        tail: Some(Box::new(Expr::literal(Literal::Unit))),
                        span: Span::default(),
                    },
                    span: Span::default(),
                    id: fresh_id(),
                }),
            ],
        }
    }

    #[test]
    fn lower_hir_to_mir() {
        let module = build_sample_module();
        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let hir_builder = HirBuilder::new(&solved);
        let hir = hir_builder.build_module(&module);

        let lowerer = HirToMir::new(&solved);
        let mir = lowerer.lower_module(&hir);

        assert_eq!(mir.functions.len(), 1);
        let func = &mir.functions[0];
        assert_eq!(func.name, "kernel");
        assert_eq!(func.body.blocks.len(), 1);
        assert_eq!(func.body.entry, BlockId(0));
        let entry = &func.body.blocks[0];
        assert_eq!(entry.id, BlockId(0));
        assert_eq!(entry.statements.len(), 1);
        match &entry.statements[0] {
            MirStatement::Assign { target, value } => {
                assert!(target.starts_with("_tmp"));
                match value {
                    MirRvalue::Call { callee, args } => {
                        assert_eq!(callee, "launch");
                        assert_eq!(args.len(), 1);
                    }
                    other => panic!("unexpected rvalue: {other:?}"),
                }
            }
            other => panic!("unexpected statement: {other:?}"),
        }
        match &entry.terminator {
            MirTerminator::Return(Some(MirOperand::Literal(Literal::Unit))) => {}
            other => panic!("unexpected terminator: {other:?}"),
        }
    }

    #[test]
    fn lower_if_statement_produces_branching_blocks() {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let module = Module {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: Vec::new(),
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "branch".into(),
                    generics: GenericParams::default(),
                    params: vec![],
                    return_type: Some(crate::ast::TypeExpr::Unit),
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![Statement::Expr(
                        Expr::dummy(ExprKind::If {
                            condition: Box::new(Expr::literal(Literal::Boolean(true))),
                            then_branch: Block {
                                statements: vec![Statement::Expr(
                                    Expr::literal(Literal::Unit),
                                    Span::default(),
                                )],
                                tail: None,
                                span: Span::default(),
                            },
                            else_branch: None,
                        }),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: fresh_id(),
            })],
        };

        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let hir_builder = HirBuilder::new(&solved);
        let hir = hir_builder.build_module(&module);

        let mir = HirToMir::new(&solved).lower_module(&hir);
        let func = &mir.functions[0];
        assert!(func.body.blocks.len() >= 3); // entry, then, join (else optional)
        let entry = &func.body.blocks[0];
        match entry.terminator {
            MirTerminator::Branch { .. } => {}
            _ => panic!("expected branch terminator"),
        }
    }

    #[test]
    fn lower_if_expression_assigns_result() {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let module = Module {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: Vec::new(),
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "select".into(),
                    generics: GenericParams::default(),
                    params: vec![crate::ast::Param {
                        name: "flag".into(),
                        ty: TypeExpr::Path(PathType::from(ident_path("Bool"))),
                    }],
                    return_type: Some(TypeExpr::Path(PathType::from(ident_path("Int32")))),
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![
                        Statement::Let {
                            pattern: Pattern::Ident("value".into()),
                            ty: None,
                            value: Some(Expr::dummy(ExprKind::If {
                                condition: Box::new(Expr::path(ident_path("flag"))),
                                then_branch: Block {
                                    statements: Vec::new(),
                                    tail: Some(Box::new(Expr::literal(Literal::Integer(
                                        "1".into(),
                                    )))),
                                    span: Span::default(),
                                },
                                else_branch: Some(Box::new(Expr::literal(Literal::Integer(
                                    "2".into(),
                                )))),
                            })),
                            span: Span::default(),
                        },
                        Statement::Return {
                            value: Some(Expr::path(ident_path("value"))),
                            span: Span::default(),
                        },
                    ],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: fresh_id(),
            })],
        };

        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let hir = HirBuilder::new(&solved).build_module(&module);

        let mir = HirToMir::new(&solved).lower_module(&hir);
        let func = &mir.functions[0];

        // Expect entry + then + join + else blocks.
        assert!(func.body.blocks.len() >= 4);

        assert!(func
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .any(|stmt| matches!(stmt, MirStatement::Assign { target, .. } if target == "value")));

        let then_block = func
            .body
            .blocks
            .iter()
            .find(|b| b.id == BlockId(1))
            .expect("then block");
        assert!(then_block.statements.is_empty());

        let else_block = func
            .body
            .blocks
            .iter()
            .find(|b| b.id == BlockId(3))
            .expect("else block");
        assert!(else_block.statements.is_empty());

        let join_block = func
            .body
            .blocks
            .iter()
            .find(|b| b.id == BlockId(2))
            .expect("join block");
        let phi = join_block
            .phi_nodes
            .iter()
            .find(|phi| phi.target.starts_with("_tmp"))
            .expect("phi node for temp result");
        assert_eq!(phi.sources.len(), 2);
        let mut preds: Vec<BlockId> = phi.sources.iter().map(|(block, _)| *block).collect();
        preds.sort_by_key(|block| block.0);
        assert_eq!(preds, vec![BlockId(1), BlockId(3)]);
    }

    #[test]
    fn optimizer_eliminates_dead_assignments() {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let module = Module {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: Vec::new(),
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "cleanup".into(),
                    generics: GenericParams::default(),
                    params: vec![],
                    return_type: Some(TypeExpr::Path(PathType::from(ident_path("Int32")))),
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![
                        Statement::Let {
                            pattern: Pattern::Ident("unused".into()),
                            ty: None,
                            value: Some(Expr::literal(Literal::Integer("1".into()))),
                            span: Span::default(),
                        },
                        Statement::Let {
                            pattern: Pattern::Ident("value".into()),
                            ty: None,
                            value: Some(Expr::literal(Literal::Integer("2".into()))),
                            span: Span::default(),
                        },
                        Statement::Return {
                            value: Some(Expr::path(ident_path("value"))),
                            span: Span::default(),
                        },
                    ],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: fresh_id(),
            })],
        };

        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let hir = HirBuilder::new(&solved).build_module(&module);
        let mir = HirToMir::new(&solved).lower_module(&hir);
        let func = &mir.functions[0];
        let entry = &func.body.blocks[0];
        assert!(entry.statements.iter().all(
            |stmt| !matches!(stmt, MirStatement::Assign { target, .. } if target == "unused")
        ));
    }

    #[test]
    fn optimizer_marks_zone_drop_actions() {
        let mut function = MirFunction {
            name: "zone_fn".into(),
            params: Vec::new(),
            return_type: Type::Primitive(PrimitiveType::Unit),
            effects: EffectRow::empty(),
            zone: Some(ZoneDescriptor {
                zone_type: "gpu".into(),
                params: Vec::new(),
                origin: Span::default(),
            }),
            body: MirBody {
                locals: vec![MirLocal {
                    name: "data".into(),
                    ty: Some(Type::TraitObject {
                        path: vec!["Buffer".into()],
                        generics: Vec::new(),
                    }),
                    region: RegionId(0),
                }],
                entry: BlockId(0),
                blocks: vec![MirBlock {
                    id: BlockId(0),
                    phi_nodes: Vec::new(),
                    statements: Vec::new(),
                    terminator: MirTerminator::Return(None),
                    span: Span::default(),
                }],
                cfg: ControlFlowGraph {
                    predecessors: vec![Vec::new()],
                    successors: vec![Vec::new()],
                },
                drop_schedule: DropSchedule {
                    per_block: vec![vec![DropAction {
                        local: "data".into(),
                        region: RegionId(0),
                        span: Span::default(),
                        reason: DropReason::ScopeEnd,
                    }]],
                },
            },
            trait_obligations: Vec::new(),
        };

        MirOptimizer::run(&mut function);

        let actions = &function.body.drop_schedule.per_block[0];
        assert!(
            matches!(actions[0].reason, DropReason::ZoneTransfer { ref zone } if zone == "gpu")
        );
    }

    #[test]
    fn refcount_elision_removes_temp_drops() {
        let mut function = MirFunction {
            name: "temp".into(),
            params: Vec::new(),
            return_type: Type::Primitive(PrimitiveType::Unit),
            effects: EffectRow::empty(),
            zone: None,
            body: MirBody {
                locals: vec![MirLocal {
                    name: "_tmp0".into(),
                    ty: None,
                    region: RegionId(0),
                }],
                entry: BlockId(0),
                blocks: vec![MirBlock {
                    id: BlockId(0),
                    phi_nodes: Vec::new(),
                    statements: Vec::new(),
                    terminator: MirTerminator::Return(None),
                    span: Span::default(),
                }],
                cfg: ControlFlowGraph {
                    predecessors: vec![Vec::new()],
                    successors: vec![Vec::new()],
                },
                drop_schedule: DropSchedule {
                    per_block: vec![vec![DropAction {
                        local: "_tmp0".into(),
                        region: RegionId(0),
                        span: Span::default(),
                        reason: DropReason::ScopeEnd,
                    }]],
                },
            },
            trait_obligations: Vec::new(),
        };

        MirOptimizer::run(&mut function);

        assert!(function.body.drop_schedule.per_block[0].is_empty());
    }

    fn zone_ident(name: &str) -> Expr {
        Expr::path(PathExpr {
            leading_colon: false,
            segments: vec![PathSegment {
                ident: name.into(),
                generics: Vec::new(),
            }],
            span: Span::default(),
        })
    }

    fn ident_path(name: &str) -> PathExpr {
        PathExpr {
            leading_colon: false,
            segments: vec![PathSegment {
                ident: name.into(),
                generics: Vec::new(),
            }],
            span: Span::default(),
        }
    }
}
