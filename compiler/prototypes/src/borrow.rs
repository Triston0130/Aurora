//! Minimal borrow checker and region scaffolding.

use std::collections::HashMap;

use crate::diagnostics::{primary_label, Diagnostic};
use crate::hir::{HirBlock, HirExpr, HirExprKind, HirFunction, HirStmt};
use crate::span::Span;
use crate::types::Type;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BorrowState {
    Owned,
    Moved,
}

#[derive(Clone)]
struct BorrowEnv {
    locals: HashMap<String, BorrowState>,
}

impl BorrowEnv {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    fn insert(&mut self, name: String, state: BorrowState) {
        self.locals.insert(name, state);
    }

    fn mark_moved(&mut self, name: &str) {
        if let Some(entry) = self.locals.get_mut(name) {
            *entry = BorrowState::Moved;
        }
    }

    fn state(&self, name: &str) -> Option<BorrowState> {
        self.locals.get(name).copied()
    }

    fn merge(self, other: BorrowEnv) -> BorrowEnv {
        let mut merged = BorrowEnv::new();
        for (name, state) in self.locals.into_iter() {
            let other_state = other
                .locals
                .get(&name)
                .copied()
                .unwrap_or(BorrowState::Owned);
            let merged_state = if state == BorrowState::Moved || other_state == BorrowState::Moved {
                BorrowState::Moved
            } else {
                BorrowState::Owned
            };
            merged.locals.insert(name, merged_state);
        }
        for (name, state) in other.locals.into_iter() {
            merged.locals.entry(name).or_insert(state);
        }
        merged
    }
}

pub struct BorrowChecker;

impl BorrowChecker {
    pub fn check(functions: &[HirFunction]) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for function in functions {
            Self::check_function(function, &mut diagnostics);
        }
        diagnostics
    }

    fn check_function(function: &HirFunction, diagnostics: &mut Vec<Diagnostic>) {
        let mut env = BorrowEnv::new();
        for (param, ty) in function.params.iter().zip(function.param_types.iter()) {
            if !is_copy_type(ty) {
                env.insert(param.name.clone(), BorrowState::Owned);
            }
        }
        Self::check_block(function, &function.body, env, diagnostics);
    }

    fn check_block(
        function: &HirFunction,
        block: &HirBlock,
        mut env: BorrowEnv,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> BorrowEnv {
        for stmt in &block.statements {
            match stmt {
                HirStmt::Let { name, ty, expr, .. } => {
                    let consumed = Self::consume_expr(function, expr, &mut env, diagnostics);
                    if let Some(var) = consumed {
                        env.mark_moved(&var);
                    }
                    if !is_copy_type(ty) {
                        env.insert(name.clone(), BorrowState::Owned);
                    }
                }
                HirStmt::Expr { expr, .. } => {
                    Self::consume_expr(function, expr, &mut env, diagnostics);
                }
                HirStmt::Return { expr, .. } => {
                    let consumed = Self::consume_expr(function, expr, &mut env, diagnostics);
                    if let Some(var) = consumed {
                        env.mark_moved(&var);
                    }
                }
                HirStmt::Item { .. } => {
                    // Nested items are ignored for now.
                }
            }
        }
        if let Some(result_expr) = block.result.as_ref() {
            let consumed = Self::consume_expr(function, result_expr, &mut env, diagnostics);
            if let Some(var) = consumed {
                env.mark_moved(&var);
            }
        }
        env
    }

    fn consume_expr(
        function: &HirFunction,
        expr: &HirExpr,
        env: &mut BorrowEnv,
        diagnostics: &mut Vec<Diagnostic>,
    ) -> Option<String> {
        match &expr.kind {
            HirExprKind::Literal(_) => None,
            HirExprKind::Variable(name) => {
                match env.state(name.as_str()) {
                    Some(BorrowState::Moved) => {
                        diagnostics.push(Self::moved_use_diagnostic(function, name, expr.span))
                    }
                    Some(BorrowState::Owned) | None => {}
                }
                Some(name.clone())
            }
            HirExprKind::Tuple(elements) | HirExprKind::Array(elements) => {
                for element in elements {
                    Self::consume_expr(function, element, env, diagnostics);
                }
                None
            }
            HirExprKind::MethodCall { receiver, args, .. } => {
                Self::consume_expr(function, receiver, env, diagnostics);
                for arg in args {
                    if let Some(var) = Self::consume_expr(function, arg, env, diagnostics) {
                        env.mark_moved(&var);
                    }
                }
                None
            }
            HirExprKind::Call { callee, args } => {
                Self::consume_expr(function, callee, env, diagnostics);
                for arg in args {
                    if let Some(var) = Self::consume_expr(function, arg, env, diagnostics) {
                        env.mark_moved(&var);
                    }
                }
                None
            }
            HirExprKind::Binary { left, right, .. } => {
                Self::consume_expr(function, left, env, diagnostics);
                Self::consume_expr(function, right, env, diagnostics);
                None
            }
            HirExprKind::Unary { expr: inner, .. } => {
                Self::consume_expr(function, inner, env, diagnostics)
            }
            HirExprKind::Field { base, .. } => Self::consume_expr(function, base, env, diagnostics),
            HirExprKind::Index { base, index } => {
                Self::consume_expr(function, base, env, diagnostics);
                Self::consume_expr(function, index, env, diagnostics);
                None
            }
            HirExprKind::Block(block) => {
                let inner_env = env.clone();
                let _ = Self::check_block(function, block, inner_env, diagnostics);
                None
            }
            HirExprKind::Actor(actor) => {
                for (_name, value) in &actor.fields {
                    if let Some(var) = Self::consume_expr(function, value, env, diagnostics) {
                        env.mark_moved(&var);
                    }
                }
                None
            }
            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                Self::consume_expr(function, condition, env, diagnostics);
                let then_env = Self::check_block(function, then_branch, env.clone(), diagnostics);
                let else_env = if let Some(else_expr) = else_branch {
                    match &else_expr.kind {
                        HirExprKind::Block(block) => {
                            Self::check_block(function, block, env.clone(), diagnostics)
                        }
                        _ => {
                            let mut branch_env = env.clone();
                            if let Some(var) = Self::consume_expr(
                                function,
                                else_expr,
                                &mut branch_env,
                                diagnostics,
                            ) {
                                branch_env.mark_moved(&var);
                            }
                            branch_env
                        }
                    }
                } else {
                    env.clone()
                };
                let merged = then_env.merge(else_env);
                *env = merged;
                None
            }
            HirExprKind::Handle { body, handlers } => {
                if let Some(var) = Self::consume_expr(function, body, env, diagnostics) {
                    env.mark_moved(&var);
                }
                for handler in handlers {
                    let _ = Self::check_block(function, &handler.body, env.clone(), diagnostics);
                }
                None
            }
            HirExprKind::Loop(body) => {
                let _ = Self::check_block(function, body, env.clone(), diagnostics);
                None
            }
            HirExprKind::While { condition, body } => {
                Self::consume_expr(function, condition, env, diagnostics);
                let _ = Self::check_block(function, body, env.clone(), diagnostics);
                None
            }
            HirExprKind::For { iterator, body, .. } => {
                if let Some(var) = Self::consume_expr(function, iterator, env, diagnostics) {
                    env.mark_moved(&var);
                }
                let _ = Self::check_block(function, body, env.clone(), diagnostics);
                None
            }
            HirExprKind::Match { scrutinee, arms } => {
                Self::consume_expr(function, scrutinee, env, diagnostics);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        let mut guard_env = env.clone();
                        Self::consume_expr(function, guard, &mut guard_env, diagnostics);
                    }
                    let _ = Self::consume_expr(function, &arm.body, &mut env.clone(), diagnostics);
                }
                None
            }
            HirExprKind::Async(block) => {
                let _ = Self::check_block(function, block, env.clone(), diagnostics);
                None
            }
            HirExprKind::Await(inner) => Self::consume_expr(function, inner, env, diagnostics),
            HirExprKind::Spawn(task) => {
                if let Some(var) = Self::consume_expr(function, task, env, diagnostics) {
                    env.mark_moved(&var);
                }
                None
            }
            HirExprKind::Zone { body, .. } => {
                let _ = Self::check_block(function, body, env.clone(), diagnostics);
                None
            }
            HirExprKind::Assignment { target, value, .. } => {
                Self::consume_expr(function, target, env, diagnostics);
                Self::consume_expr(function, value, env, diagnostics);
                None
            }
            HirExprKind::Break(value) => {
                if let Some(expr) = value {
                    Self::consume_expr(function, expr, env, diagnostics);
                }
                None
            }
            HirExprKind::Continue => None,
            HirExprKind::Closure { body, .. } => {
                let _ = Self::check_block(function, body, env.clone(), diagnostics);
                None
            }
            HirExprKind::Opaque(_) => None,
        }
    }

    fn moved_use_diagnostic(function: &HirFunction, name: &str, span: Span) -> Diagnostic {
        Diagnostic::error(format!(
            "value `{}` was moved and cannot be used again",
            name
        ))
        .with_label(primary_label(
            span,
            format!("`{}` used here after move", name),
        ))
        .with_note(format!("in function `{}`", function.name))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{HirBlock, HirExpr, HirExprKind, HirFunction, HirParam, HirStmt};
    use crate::span::Span;
    use aurora_effect_solver::EffectRow;

    fn owned_function(name: &str, body: HirBlock) -> HirFunction {
        HirFunction {
            name: name.into(),
            params: vec![HirParam {
                name: "value".into(),
                annotation: None,
            }],
            declared_effects: EffectRow::empty(),
            inferred_effects: EffectRow::empty(),
            param_types: vec![Type::TraitObject {
                path: vec!["Owned".into()],
                generics: Vec::new(),
            }],
            return_type: Type::Primitive(crate::types::PrimitiveType::Unit),
            zone: None,
            is_const: false,
            body,
            trait_obligations: Vec::new(),
            span: Span::default(),
        }
    }

    #[test]
    fn detects_use_after_move_in_simple_let() {
        let env_body = HirBlock {
            statements: vec![
                HirStmt::Let {
                    name: "x".into(),
                    ty: Type::Primitive(crate::types::PrimitiveType::Int32),
                    expr: HirExpr {
                        kind: HirExprKind::Variable("value".into()),
                        ty: Type::Primitive(crate::types::PrimitiveType::Int32),
                        effects: EffectRow::empty(),
                        span: Span::default(),
                    },
                    span: Span::default(),
                },
                HirStmt::Let {
                    name: "y".into(),
                    ty: Type::Primitive(crate::types::PrimitiveType::Int32),
                    expr: HirExpr {
                        kind: HirExprKind::Variable("value".into()),
                        ty: Type::Primitive(crate::types::PrimitiveType::Int32),
                        effects: EffectRow::empty(),
                        span: Span::default(),
                    },
                    span: Span::default(),
                },
            ],
            result: None,
            span: Span::default(),
        };
        let function = owned_function("move", env_body);
        let diagnostics = BorrowChecker::check(&[function]);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("value `value` was moved"));
    }
}
