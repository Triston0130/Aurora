//! High-level intermediate representation scaffolding.

use crate::ast::{
    ActorLiteral, AssignmentOp, Block, EffectExpr, Expr, ExprKind, FunctionDecl, Item, Literal,
    Module, Pattern, Statement, UnaryOp, ZoneArg,
};
use crate::borrow::BorrowChecker;
use crate::constraints::{ExprInfo, FunctionTypeInfo, SolvedAnalysis};
use crate::diagnostics::Diagnostic;
use crate::span::Span;
use crate::trait_solver::TraitSolver;
use crate::types::{PrimitiveType, Type};
use crate::zone::ZoneDescriptor;
use aurora_effect_solver::EffectRow;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub functions: Vec<HirFunction>,
    pub borrow_diagnostics: Vec<Diagnostic>,
    pub trait_diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFunction {
    pub name: String,
    pub params: Vec<HirParam>,
    pub declared_effects: EffectRow,
    pub inferred_effects: EffectRow,
    pub param_types: Vec<Type>,
    pub return_type: Type,
    pub zone: Option<ZoneDescriptor>,
    pub is_const: bool,
    pub body: HirBlock,
    pub trait_obligations: Vec<HirTraitObligation>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub name: String,
    pub annotation: Option<crate::ast::TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBlock {
    pub statements: Vec<HirStmt>,
    pub result: Option<Box<HirExpr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmt {
    Let {
        name: String,
        ty: Type,
        expr: HirExpr,
        span: Span,
    },
    Expr {
        expr: HirExpr,
        span: Span,
    },
    Return {
        expr: HirExpr,
        span: Span,
    },
    Item {
        item: HirItem,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    Function(HirFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub ty: Type,
    pub effects: EffectRow,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    Literal(Literal),
    Variable(String),
    Call {
        callee: Box<HirExpr>,
        args: Vec<HirExpr>,
    },
    Tuple(Vec<HirExpr>),
    Array(Vec<HirExpr>),
    MethodCall {
        receiver: Box<HirExpr>,
        method: String,
        turbofish: Vec<crate::ast::TypeExpr>,
        args: Vec<HirExpr>,
    },
    Binary {
        op: crate::ast::BinaryOp,
        left: Box<HirExpr>,
        right: Box<HirExpr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<HirExpr>,
    },
    Field {
        base: Box<HirExpr>,
        field: String,
    },
    Index {
        base: Box<HirExpr>,
        index: Box<HirExpr>,
    },
    Block(Box<HirBlock>),
    Actor(HirActorLiteral),
    If {
        condition: Box<HirExpr>,
        then_branch: Box<HirBlock>,
        else_branch: Option<Box<HirExpr>>,
    },
    Loop(HirBlock),
    While {
        condition: Box<HirExpr>,
        body: HirBlock,
    },
    For {
        pattern: Pattern,
        iterator: Box<HirExpr>,
        body: HirBlock,
    },
    Match {
        scrutinee: Box<HirExpr>,
        arms: Vec<HirMatchArm>,
    },
    Handle {
        body: Box<HirExpr>,
        handlers: Vec<HirEffectHandler>,
    },
    Async(HirBlock),
    Await(Box<HirExpr>),
    Spawn(Box<HirExpr>),
    Zone {
        name: String,
        args: Vec<ZoneArg>,
        body: HirBlock,
    },
    Assignment {
        target: Box<HirExpr>,
        op: AssignmentOp,
        value: Box<HirExpr>,
    },
    Break(Option<Box<HirExpr>>),
    Continue,
    Closure {
        params: Vec<String>,
        body: HirBlock,
    },
    Opaque(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirActorLiteral {
    pub path: Vec<String>,
    pub fields: Vec<(String, HirExpr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEffectHandler {
    pub label: String,
    pub body: HirBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirMatchArm {
    pub pattern: Pattern,
    pub guard: Option<HirExpr>,
    pub body: HirExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTraitObligation {
    pub span: Span,
    pub subject: Type,
    pub trait_path: Vec<String>,
    pub generics: Vec<Type>,
}

struct LocalCursor {
    entries: HashMap<String, Vec<Type>>,
    indices: HashMap<String, usize>,
}

impl LocalCursor {
    fn new(entries: HashMap<String, Vec<Type>>) -> Self {
        Self {
            entries,
            indices: HashMap::new(),
        }
    }

    fn next_ty(&mut self, name: &str) -> Option<Type> {
        let values = self.entries.get(name)?;
        let index = self.indices.entry(name.to_string()).or_insert(0);
        if *index < values.len() {
            let ty = values[*index].clone();
            *index += 1;
            Some(ty)
        } else {
            None
        }
    }
}

struct ExprCursor {
    entries: Vec<ExprInfo>,
    index: usize,
}

impl ExprCursor {
    fn new(entries: Vec<ExprInfo>) -> Self {
        Self { entries, index: 0 }
    }

    fn next(&mut self) -> ExprInfo {
        if self.index < self.entries.len() {
            let info = self.entries[self.index].clone();
            self.index += 1;
            info
        } else {
            ExprInfo {
                ty: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::empty(),
                span: Span::default(),
            }
        }
    }
}

pub struct HirBuilder<'a> {
    analysis: &'a SolvedAnalysis,
}

impl<'a> HirBuilder<'a> {
    pub fn new(analysis: &'a SolvedAnalysis) -> Self {
        Self { analysis }
    }

    pub fn build_module(&self, module: &Module) -> HirModule {
        let mut functions = Vec::new();
        for item in &module.items {
            if let crate::ast::Item::Function(func) = item {
                functions.push(self.build_function(func));
            }
        }
        let borrow_diagnostics = BorrowChecker::check(&functions);
        let trait_diagnostics =
            TraitSolver::new(&self.analysis.trait_env).solve(&self.analysis.trait_constraints);
        HirModule {
            functions,
            borrow_diagnostics,
            trait_diagnostics,
        }
    }

    fn build_function(&self, func: &FunctionDecl) -> HirFunction {
        let signature = &func.signature;
        let params: Vec<HirParam> = signature
            .params
            .iter()
            .map(|param| HirParam {
                name: param.name.clone(),
                annotation: Some(param.ty.clone()),
            })
            .collect();
        let fallback_param_types: Vec<Type> =
            vec![Type::Primitive(PrimitiveType::Unit); params.len()];
        let declared_effects = convert_effect(signature.effect.as_ref());
        let zone = self.analysis.zones.get(&signature.name).cloned();
        let info = self
            .analysis
            .functions
            .get(&signature.name)
            .cloned()
            .unwrap_or_else(|| FunctionTypeInfo {
                param_types: fallback_param_types.clone(),
                return_type: Type::Primitive(PrimitiveType::Unit),
                effects: declared_effects.clone(),
            });
        let local_map = self
            .analysis
            .locals
            .get(&signature.name)
            .cloned()
            .unwrap_or_default();
        let mut local_cursor = LocalCursor::new(local_map);
        let expr_entries = self
            .analysis
            .exprs
            .get(&signature.name)
            .cloned()
            .unwrap_or_default();
        let mut expr_cursor = ExprCursor::new(expr_entries);
        let body = self.build_block(&func.body, &mut local_cursor, &mut expr_cursor);
        let trait_obligations = self
            .analysis
            .trait_constraints
            .iter()
            .filter(|obligation| {
                obligation
                    .function
                    .as_deref()
                    .map(|name| name == signature.name)
                    .unwrap_or(false)
            })
            .map(|obligation| HirTraitObligation {
                span: obligation.span,
                subject: obligation.subject.clone(),
                trait_path: obligation.trait_path.clone(),
                generics: obligation.generics.clone(),
            })
            .collect();
        HirFunction {
            name: signature.name.clone(),
            params,
            declared_effects,
            inferred_effects: info.effects,
            param_types: info.param_types,
            return_type: info.return_type,
            zone,
            is_const: signature.constness,
            body,
            trait_obligations,
            span: func.span,
        }
    }

    fn build_block(
        &self,
        block: &Block,
        locals: &mut LocalCursor,
        exprs: &mut ExprCursor,
    ) -> HirBlock {
        let mut statements = Vec::new();
        for stmt in &block.statements {
            match stmt {
                Statement::Let {
                    pattern,
                    value,
                    span,
                    ..
                } => {
                    if let (Some(name), Some(expr)) =
                        (Self::extract_simple_binding(pattern), value.as_ref())
                    {
                        let ty = locals
                            .next_ty(&name)
                            .unwrap_or(Type::Primitive(PrimitiveType::Unit));
                        statements.push(HirStmt::Let {
                            name,
                            ty,
                            expr: self.build_expr(expr, locals, exprs),
                            span: *span,
                        });
                    }
                }
                Statement::Expr(expr, span) => statements.push(HirStmt::Expr {
                    expr: self.build_expr(expr, locals, exprs),
                    span: *span,
                }),
                Statement::Item(item, span) => {
                    if let Some(hir_item) = self.build_item(item) {
                        statements.push(HirStmt::Item {
                            item: hir_item,
                            span: *span,
                        });
                    }
                }
                Statement::Return {
                    value: expr_opt,
                    span,
                } => {
                    let expr = expr_opt
                        .as_ref()
                        .map(|expr| self.build_expr(expr, locals, exprs))
                        .unwrap_or_else(Self::unit_expr);
                    statements.push(HirStmt::Return { expr, span: *span });
                }
            }
        }
        let result = block
            .tail
            .as_ref()
            .map(|expr| Box::new(self.build_expr(expr, locals, exprs)));
        HirBlock {
            statements,
            result,
            span: block.span,
        }
    }

    fn build_item(&self, item: &Item) -> Option<HirItem> {
        match item {
            Item::Function(func) => Some(HirItem::Function(self.build_function(func))),
            _ => None,
        }
    }

    fn build_expr(&self, expr: &Expr, locals: &mut LocalCursor, exprs: &mut ExprCursor) -> HirExpr {
        let kind = match &expr.kind {
            ExprKind::Literal(lit) => HirExprKind::Literal(lit.clone()),
            ExprKind::Path(path) => {
                let ident = path
                    .segments
                    .last()
                    .map(|seg| seg.ident.clone())
                    .unwrap_or_default();
                HirExprKind::Variable(ident)
            }
            ExprKind::Tuple(elems) => HirExprKind::Tuple(
                elems
                    .iter()
                    .map(|elem| self.build_expr(elem, locals, exprs))
                    .collect(),
            ),
            ExprKind::Array(elems) => HirExprKind::Array(
                elems
                    .iter()
                    .map(|elem| self.build_expr(elem, locals, exprs))
                    .collect(),
            ),
            ExprKind::MacroCall { .. } => HirExprKind::Literal(Literal::Unit),
            ExprKind::MethodCall {
                receiver,
                method,
                turbofish,
                args,
                ..
            } => HirExprKind::MethodCall {
                receiver: Box::new(self.build_expr(receiver, locals, exprs)),
                method: method.clone(),
                turbofish: turbofish.clone(),
                args: args
                    .iter()
                    .map(|a| self.build_expr(a, locals, exprs))
                    .collect(),
            },
            ExprKind::Binary { op, left, right } => HirExprKind::Binary {
                op: *op,
                left: Box::new(self.build_expr(left, locals, exprs)),
                right: Box::new(self.build_expr(right, locals, exprs)),
            },
            ExprKind::Unary { op, expr: inner } => HirExprKind::Unary {
                op: *op,
                expr: Box::new(self.build_expr(inner, locals, exprs)),
            },
            ExprKind::Field { base, field } => HirExprKind::Field {
                base: Box::new(self.build_expr(base, locals, exprs)),
                field: field.clone(),
            },
            ExprKind::Index { base, index } => HirExprKind::Index {
                base: Box::new(self.build_expr(base, locals, exprs)),
                index: Box::new(self.build_expr(index, locals, exprs)),
            },
            ExprKind::Call { callee, args } => HirExprKind::Call {
                callee: Box::new(self.build_expr(callee, locals, exprs)),
                args: args
                    .iter()
                    .map(|a| self.build_expr(a, locals, exprs))
                    .collect(),
            },
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => HirExprKind::If {
                condition: Box::new(self.build_expr(condition, locals, exprs)),
                then_branch: Box::new(self.build_block(then_branch, locals, exprs)),
                else_branch: else_branch
                    .as_ref()
                    .map(|expr| Box::new(self.build_expr(expr, locals, exprs))),
            },
            ExprKind::Actor(actor) => {
                HirExprKind::Actor(self.build_actor_literal(actor, locals, exprs))
            }
            ExprKind::Spawn(task) => {
                HirExprKind::Spawn(Box::new(self.build_expr(task, locals, exprs)))
            }
            ExprKind::Zone { name, args, body } => HirExprKind::Zone {
                name: name.clone(),
                args: args.clone(),
                body: self.build_block(body, locals, exprs),
            },
            ExprKind::Block(block) => {
                HirExprKind::Block(Box::new(self.build_block(block, locals, exprs)))
            }
            ExprKind::Loop(body) => HirExprKind::Loop(self.build_block(body, locals, exprs)),
            ExprKind::While { condition, body } => HirExprKind::While {
                condition: Box::new(self.build_expr(condition, locals, exprs)),
                body: self.build_block(body, locals, exprs),
            },
            ExprKind::For {
                pattern,
                iterator,
                body,
            } => HirExprKind::For {
                pattern: pattern.clone(),
                iterator: Box::new(self.build_expr(iterator, locals, exprs)),
                body: self.build_block(body, locals, exprs),
            },
            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_expr = self.build_expr(scrutinee, locals, exprs);
                let built_arms = arms
                    .iter()
                    .map(|arm| HirMatchArm {
                        pattern: arm.pattern.clone(),
                        guard: arm
                            .guard
                            .as_ref()
                            .map(|g| self.build_expr(g, locals, exprs)),
                        body: self.build_expr(&arm.body, locals, exprs),
                    })
                    .collect();
                HirExprKind::Match {
                    scrutinee: Box::new(scrutinee_expr),
                    arms: built_arms,
                }
            }
            ExprKind::Handle { body, handlers } => {
                let body_expr = self.build_expr(body, locals, exprs);
                let handler_blocks = handlers
                    .iter()
                    .map(|handler| HirEffectHandler {
                        label: handler.label.clone(),
                        body: self.build_block(&handler.body, locals, exprs),
                    })
                    .collect();
                HirExprKind::Handle {
                    body: Box::new(body_expr),
                    handlers: handler_blocks,
                }
            }
            ExprKind::Async(block) => HirExprKind::Async(self.build_block(block, locals, exprs)),
            ExprKind::Await(inner) => {
                HirExprKind::Await(Box::new(self.build_expr(inner, locals, exprs)))
            }
            ExprKind::Break(value) => HirExprKind::Break(
                value
                    .as_ref()
                    .map(|expr| Box::new(self.build_expr(expr, locals, exprs))),
            ),
            ExprKind::Continue => HirExprKind::Continue,
            ExprKind::Assignment { left, op, right } => HirExprKind::Assignment {
                target: Box::new(self.build_expr(left, locals, exprs)),
                op: *op,
                value: Box::new(self.build_expr(right, locals, exprs)),
            },
            ExprKind::Closure { params, body } => HirExprKind::Closure {
                params: params.clone(),
                body: self.build_block(body, locals, exprs),
            },
            _ => HirExprKind::Opaque(expr.clone()),
        };
        let info = exprs.next();
        HirExpr {
            kind,
            ty: info.ty,
            effects: info.effects,
            span: expr.span,
        }
    }

    fn build_actor_literal(
        &self,
        actor: &ActorLiteral,
        locals: &mut LocalCursor,
        exprs: &mut ExprCursor,
    ) -> HirActorLiteral {
        HirActorLiteral {
            path: actor
                .path
                .segments
                .iter()
                .map(|seg| seg.ident.clone())
                .collect(),
            fields: actor
                .fields
                .iter()
                .map(|field| {
                    (
                        field.name.clone(),
                        self.build_expr(&field.value, locals, exprs),
                    )
                })
                .collect(),
        }
    }

    fn unit_expr() -> HirExpr {
        HirExpr {
            kind: HirExprKind::Literal(Literal::Unit),
            ty: Type::Primitive(PrimitiveType::Unit),
            effects: EffectRow::empty(),
            span: Span::default(),
        }
    }

    fn extract_simple_binding(pattern: &Pattern) -> Option<String> {
        match pattern {
            Pattern::Ident(name) => Some(name.clone()),
            Pattern::Binding {
                name, subpattern, ..
            } => match subpattern.as_ref() {
                Pattern::Ident(inner) if inner == name => Some(name.clone()),
                Pattern::Wildcard => Some(name.clone()),
                _ => None,
            },
            Pattern::Move(inner) => Self::extract_simple_binding(inner),
            _ => None,
        }
    }
}

fn convert_effect(effect: Option<&EffectExpr>) -> EffectRow {
    match effect {
        Some(expr) => {
            let labels: Vec<String> = expr.labels.iter().map(|label| label.name.clone()).collect();
            let mut row = EffectRow::from_effects(labels);
            if let Some(tail) = &expr.tail {
                row = row.union(&EffectRow::with_tail(Vec::<String>::new(), tail.clone()));
            }
            row
        }
        None => EffectRow::empty(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Attribute, AttributeArg, Block, EffectExpr, EffectLabel, Expr, ExprKind, FunctionDecl,
        FunctionSig, GenericParams, Item, Literal, Module, NodeId, PathExpr, PathSegment, PathType,
        Statement, ZoneDecl,
    };
    use crate::constraints::{AnalysisResult, ConstraintSet, FunctionTypeInfo};
    use crate::hir::HirExprKind;
    use crate::span::Span;
    use crate::trait_solver::TraitEnvironment;
    use crate::types::PrimitiveType;
    use std::collections::HashMap;

    fn sample_module() -> (Module, HashMap<String, ZoneDescriptor>) {
        let func = FunctionDecl {
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
            id: NodeId::new(1),
        };
        let module = Module {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![
                Item::Zone(ZoneDecl {
                    attributes: vec![Attribute {
                        name: "doc".into(),
                        args: vec![AttributeArg::Expr(Expr::literal(Literal::String(
                            "GPU zone".into(),
                        )))],
                        span: Span::default(),
                    }],
                    docs: Vec::new(),
                    name: "gpu".into(),
                    args: Vec::new(),
                    body: Block {
                        statements: Vec::new(),
                        tail: None,
                        span: Span::default(),
                    },
                    span: Span::default(),
                    id: NodeId::new(2),
                }),
                Item::Function(func),
            ],
        };
        let mut zones = HashMap::new();
        zones.insert(
            "kernel".into(),
            ZoneDescriptor {
                zone_type: "gpu".into(),
                params: vec![],
                origin: Span::default(),
            },
        );
        (module, zones)
    }

    #[test]
    fn builder_captures_zone_and_effects() {
        let (module, zones) = sample_module();
        let mut functions = HashMap::new();
        functions.insert(
            "kernel".into(),
            FunctionTypeInfo {
                param_types: vec![],
                return_type: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::from_effects(vec![String::from("Compute")]),
            },
        );
        let analysis = AnalysisResult {
            constraints: ConstraintSet::default(),
            zones,
            functions,
            locals: HashMap::new(),
            exprs: HashMap::new(),
            trait_env: TraitEnvironment::new(),
        };
        let solved = analysis.solve().expect("analysis solved");
        let builder = HirBuilder::new(&solved);
        let hir = builder.build_module(&module);
        assert!(hir.borrow_diagnostics.is_empty());
        assert!(hir.trait_diagnostics.is_empty());
        assert_eq!(hir.functions.len(), 1);
        let func = &hir.functions[0];
        assert_eq!(func.name, "kernel");
        assert!(func.zone.is_some());
        assert_eq!(func.declared_effects.fixed().len(), 1);
        assert_eq!(func.inferred_effects.fixed().len(), 1);
        assert_eq!(func.param_types.len(), 0);
        assert_eq!(func.return_type, Type::Primitive(PrimitiveType::Unit));
        assert!(func.trait_obligations.is_empty());
        match func.body.statements.first() {
            Some(HirStmt::Expr { expr, .. }) => match &expr.kind {
                HirExprKind::Call { .. } => {}
                other => panic!("unexpected expr: {other:?}"),
            },
            other => panic!("unexpected stmt: {other:?}"),
        }
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

    #[test]
    fn builder_records_trait_obligations() {
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
                    name: "render".into(),
                    generics: GenericParams {
                        params: vec![crate::ast::GenericParam {
                            name: "T".into(),
                            kind: crate::ast::GenericParamKind::Type {
                                bounds: vec![PathType {
                                    leading_colon: false,
                                    segments: vec![PathSegment {
                                        ident: "Display".into(),
                                        generics: Vec::new(),
                                    }],
                                }],
                            },
                        }],
                    },
                    params: vec![crate::ast::Param {
                        name: "value".into(),
                        ty: crate::ast::TypeExpr::Path(PathType {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "T".into(),
                                generics: Vec::new(),
                            }],
                        }),
                    }],
                    return_type: Some(crate::ast::TypeExpr::Unit),
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: Vec::new(),
                    tail: Some(Box::new(Expr::literal(Literal::Unit))),
                    span: Span::default(),
                },
                span: Span::default(),
                id: fresh_id(),
            })],
        };

        let mut env = crate::types::TypeEnv::new();
        env.push_scope();
        let mut ctx = crate::types::InferenceContext::default();
        let generator = crate::constraints::Generator::new(&mut env, &mut ctx);
        let mut analysis = generator.generate_module(&module);
        if let Some(info) = analysis.functions.get_mut("move_twice") {
            info.param_types = vec![Type::TraitObject {
                path: vec!["Owned".into()],
                generics: Vec::new(),
            }];
        }
        let solved = analysis.solve().expect("analysis solved");
        let builder = HirBuilder::new(&solved);
        let hir = builder.build_module(&module);

        let func = hir
            .functions
            .iter()
            .find(|f| f.name == "render")
            .expect("render function lowered");
        assert!(hir.borrow_diagnostics.is_empty());
        assert!(hir.trait_diagnostics.is_empty());
        assert_eq!(func.trait_obligations.len(), 1);
        let obligation = &func.trait_obligations[0];
        assert_eq!(obligation.trait_path, vec![String::from("Display")]);
        assert!(matches!(obligation.subject, Type::Var(_)));
    }

    #[test]
    fn builder_reports_borrow_diagnostics() {
        let mut next_id = 0u32;
        let mut fresh_id = || {
            let id = NodeId::new(next_id);
            next_id += 1;
            id
        };

        let func = FunctionDecl {
            attributes: Vec::new(),
            docs: Vec::new(),
            visibility: crate::ast::Visibility::Private,
            signature: FunctionSig {
                constness: false,
                asyncness: false,
                name: "move_twice".into(),
                generics: GenericParams::default(),
                params: vec![crate::ast::Param {
                    name: "value".into(),
                    ty: crate::ast::TypeExpr::Path(PathType {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "OwnedType".into(),
                            generics: Vec::new(),
                        }],
                    }),
                }],
                return_type: Some(crate::ast::TypeExpr::Unit),
                effect: None,
                where_clause: Vec::new(),
            },
            body: Block {
                statements: vec![
                    Statement::Let {
                        pattern: Pattern::Ident("a".into()),
                        ty: None,
                        value: Some(Expr::path(PathExpr {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "value".into(),
                                generics: Vec::new(),
                            }],
                            span: Span::default(),
                        })),
                        span: Span::default(),
                    },
                    Statement::Let {
                        pattern: Pattern::Ident("b".into()),
                        ty: None,
                        value: Some(Expr::path(PathExpr {
                            leading_colon: false,
                            segments: vec![PathSegment {
                                ident: "value".into(),
                                generics: Vec::new(),
                            }],
                            span: Span::default(),
                        })),
                        span: Span::default(),
                    },
                ],
                tail: Some(Box::new(Expr::literal(Literal::Unit))),
                span: Span::default(),
            },
            span: Span::default(),
            id: fresh_id(),
        };

        let module = Module {
            id: fresh_id(),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(func)],
        };

        let mut env = crate::types::TypeEnv::new();
        env.push_scope();
        let mut ctx = crate::types::InferenceContext::default();
        let generator = crate::constraints::Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let solved = analysis.solve().expect("analysis solved");
        let builder = HirBuilder::new(&solved);
        let hir = builder.build_module(&module);
        let func = &hir.functions[0];
        assert_eq!(func.params.len(), 1);
        assert!(
            matches!(
                func.param_types.first(),
                Some(Type::Var(_)) | Some(Type::TraitObject { .. })
            ),
            "unexpected param type: {:?}",
            func.param_types.first()
        );
        let borrow_reports = crate::borrow::BorrowChecker::check(&hir.functions);
        assert_eq!(borrow_reports.len(), 1);
        assert_eq!(hir.borrow_diagnostics.len(), 1);
        assert!(hir.trait_diagnostics.is_empty());
        assert!(hir.borrow_diagnostics[0]
            .message
            .contains("value `value` was moved"));
    }
}
