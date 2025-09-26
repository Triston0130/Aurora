use std::collections::HashMap;

use crate::ast::Expr;
use crate::const_eval::{apply_binary, apply_unary, ConstValue};
use crate::diagnostics::{primary_label, Diagnostic};
use crate::hir::{HirBlock, HirExpr, HirExprKind, HirModule, HirStmt};
use crate::span::Span;

#[derive(Debug, Default)]
pub struct CtfeResult {
    pub values: HashMap<String, ConstValue>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn evaluate_module(module: &HirModule) -> CtfeResult {
    let mut context = CtfeContext::new(module);
    context.evaluate_all();
    CtfeResult {
        values: context.cache,
        diagnostics: context.diagnostics,
    }
}

struct CtfeContext<'a> {
    module: &'a HirModule,
    functions: HashMap<&'a str, usize>,
    cache: HashMap<String, ConstValue>,
    diagnostics: Vec<Diagnostic>,
    stack: Vec<String>,
}

impl<'a> CtfeContext<'a> {
    fn new(module: &'a HirModule) -> Self {
        let functions = module
            .functions
            .iter()
            .enumerate()
            .map(|(idx, func)| (func.name.as_str(), idx))
            .collect();
        Self {
            module,
            functions,
            cache: HashMap::new(),
            diagnostics: Vec::new(),
            stack: Vec::new(),
        }
    }

    fn evaluate_all(&mut self) {
        for (index, function) in self.module.functions.iter().enumerate() {
            if function.is_const && !self.cache.contains_key(&function.name) {
                match self.evaluate_function(index, &[]) {
                    Ok(value) => {
                        self.cache.insert(function.name.clone(), value);
                    }
                    Err(err) => self.diagnostics.push(err.into_diagnostic(&function.name)),
                }
            }
        }
    }

    fn evaluate_function(
        &mut self,
        index: usize,
        args: &[ConstValue],
    ) -> Result<ConstValue, CtfeError> {
        let function = &self.module.functions[index];
        if function.params.len() != args.len() {
            return Err(CtfeError::new(
                function.span,
                format!(
                    "const function `{}` expected {} argument(s) but found {}",
                    function.name,
                    function.params.len(),
                    args.len()
                ),
            ));
        }
        if self.stack.contains(&function.name) {
            return Err(CtfeError::new(
                function.span,
                format!("const function `{}` recursively evaluated", function.name),
            ));
        }
        self.stack.push(function.name.clone());
        let mut env = Environment::new();
        let bindings: Vec<(String, ConstValue)> = function
            .params
            .iter()
            .zip(args.iter())
            .map(|(param, value)| (param.name.clone(), value.clone()))
            .collect();
        let outcome = self.eval_block(&function.body, &mut env, Some(&bindings))?;
        self.stack.pop();
        Ok(outcome.into_value())
    }

    fn invoke_function(
        &mut self,
        name: &str,
        args: &[ConstValue],
        span: Span,
    ) -> Result<ConstValue, CtfeError> {
        if args.is_empty() {
            if let Some(value) = self.cache.get(name) {
                return Ok(value.clone());
            }
        }
        let index =
            self.functions.get(name).copied().ok_or_else(|| {
                CtfeError::new(span, format!("unknown const function `{}`", name))
            })?;
        let function = &self.module.functions[index];
        if !function.is_const {
            return Err(CtfeError::new(
                span,
                format!("function `{}` is not `const`", name),
            ));
        }
        let value = self.evaluate_function(index, args)?;
        if args.is_empty() {
            self.cache.insert(function.name.clone(), value.clone());
        }
        Ok(value)
    }

    fn eval_block(
        &mut self,
        block: &HirBlock,
        env: &mut Environment,
        bindings: Option<&[(String, ConstValue)]>,
    ) -> Result<EvalOutcome, CtfeError> {
        env.push_scope();
        if let Some(bindings) = bindings {
            for (name, value) in bindings {
                env.insert(name.clone(), value.clone());
            }
        }
        for stmt in &block.statements {
            match stmt {
                HirStmt::Let { name, expr, .. } => {
                    let value = self
                        .eval_expr(expr, env)?
                        .expect_value(expr.span, "let initializer")?;
                    env.insert(name.clone(), value);
                }
                HirStmt::Expr { expr, .. } => {
                    if let EvalOutcome::Return(value) = self.eval_expr(expr, env)? {
                        env.pop_scope();
                        return Ok(EvalOutcome::Return(value));
                    }
                }
                HirStmt::Return { expr, .. } => {
                    let value = self
                        .eval_expr(expr, env)?
                        .expect_value(expr.span, "return expression")?;
                    env.pop_scope();
                    return Ok(EvalOutcome::Return(value));
                }
                HirStmt::Item { span, .. } => {
                    env.pop_scope();
                    return Err(CtfeError::new(
                        *span,
                        "nested items unsupported in const evaluation",
                    ));
                }
            }
        }
        let value = if let Some(result_expr) = &block.result {
            match self.eval_expr(result_expr, env)? {
                EvalOutcome::Value(value) => value,
                EvalOutcome::Return(value) => {
                    env.pop_scope();
                    return Ok(EvalOutcome::Return(value));
                }
            }
        } else {
            ConstValue::Unit
        };
        env.pop_scope();
        Ok(EvalOutcome::Value(value))
    }

    fn eval_expr(
        &mut self,
        expr: &HirExpr,
        env: &mut Environment,
    ) -> Result<EvalOutcome, CtfeError> {
        match &expr.kind {
            HirExprKind::Literal(lit) => {
                let expr_value = Expr::literal(lit.clone());
                let value = crate::const_eval::eval(&expr_value).ok_or_else(|| {
                    CtfeError::new(expr.span, "unsupported literal in const evaluation")
                })?;
                Ok(EvalOutcome::Value(value))
            }
            HirExprKind::Variable(name) => {
                if let Some(value) = env.get(name) {
                    Ok(EvalOutcome::Value(value))
                } else {
                    Err(CtfeError::new(
                        expr.span,
                        format!("unknown const variable `{}`", name),
                    ))
                }
            }
            HirExprKind::Tuple(elems) => {
                let mut values = Vec::new();
                for elem in elems {
                    values.push(
                        self.eval_expr(elem, env)?
                            .expect_value(elem.span, "tuple element")?,
                    );
                }
                Ok(EvalOutcome::Value(ConstValue::Tuple(values)))
            }
            HirExprKind::Array(elems) => {
                let mut values = Vec::new();
                for elem in elems {
                    values.push(
                        self.eval_expr(elem, env)?
                            .expect_value(elem.span, "array element")?,
                    );
                }
                Ok(EvalOutcome::Value(ConstValue::Array(values)))
            }
            HirExprKind::Binary { op, left, right } => {
                let lhs = self
                    .eval_expr(left, env)?
                    .expect_value(left.span, "binary left operand")?;
                let rhs = self
                    .eval_expr(right, env)?
                    .expect_value(right.span, "binary right operand")?;
                let value = apply_binary(*op, lhs, rhs).ok_or_else(|| {
                    CtfeError::new(expr.span, "unsupported binary operation in const context")
                })?;
                Ok(EvalOutcome::Value(value))
            }
            HirExprKind::Unary { op, expr: inner } => {
                let value = self
                    .eval_expr(inner, env)?
                    .expect_value(inner.span, "unary operand")?;
                let value = apply_unary(*op, value).ok_or_else(|| {
                    CtfeError::new(expr.span, "unsupported unary operation in const context")
                })?;
                Ok(EvalOutcome::Value(value))
            }
            HirExprKind::Block(block) => self.eval_block(block, env, None),
            HirExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self
                    .eval_expr(condition, env)?
                    .expect_value(condition.span, "if condition")?;
                let cond_bool = match cond_value {
                    ConstValue::Bool(value) => value,
                    _ => {
                        return Err(CtfeError::new(
                            condition.span,
                            "if condition must be a boolean in const context",
                        ))
                    }
                };
                if cond_bool {
                    self.eval_block(then_branch, env, None)
                } else if let Some(else_expr) = else_branch {
                    match &else_expr.kind {
                        HirExprKind::Block(block) => self.eval_block(block, env, None),
                        _ => self.eval_expr(else_expr, env),
                    }
                } else {
                    Ok(EvalOutcome::Value(ConstValue::Unit))
                }
            }
            HirExprKind::Call { callee, args } => {
                let func_name = match &callee.kind {
                    HirExprKind::Variable(name) => name.clone(),
                    _ => {
                        return Err(CtfeError::new(
                            callee.span,
                            "const calls must reference a function name",
                        ))
                    }
                };
                let mut values = Vec::new();
                for arg in args {
                    values.push(
                        self.eval_expr(arg, env)?
                            .expect_value(arg.span, "call argument")?,
                    );
                }
                let value = self.invoke_function(&func_name, &values, expr.span)?;
                Ok(EvalOutcome::Value(value))
            }
            _ => Err(CtfeError::new(
                expr.span,
                "expression not supported in const evaluation",
            )),
        }
    }
}

struct Environment {
    scopes: Vec<HashMap<String, ConstValue>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, value: ConstValue) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }

    fn get(&self, name: &str) -> Option<ConstValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }
}

#[derive(Debug)]
struct CtfeError {
    span: Span,
    message: String,
}

impl CtfeError {
    fn new(span: Span, message: impl Into<String>) -> Self {
        CtfeError {
            span,
            message: message.into(),
        }
    }

    fn into_diagnostic(self, function: &str) -> Diagnostic {
        Diagnostic::error(format!("compile-time evaluation failed in `{}`", function))
            .with_label(primary_label(self.span, self.message))
    }
}

#[derive(Debug)]
enum EvalOutcome {
    Value(ConstValue),
    Return(ConstValue),
}

impl EvalOutcome {
    fn into_value(self) -> ConstValue {
        match self {
            EvalOutcome::Value(value) | EvalOutcome::Return(value) => value,
        }
    }

    fn expect_value(self, span: Span, context: &str) -> Result<ConstValue, CtfeError> {
        match self {
            EvalOutcome::Value(value) => Ok(value),
            EvalOutcome::Return(_) => Err(CtfeError::new(
                span,
                format!("`return` not allowed in {context}"),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{HirExpr, HirExprKind, HirFunction, HirModule, HirStmt};
    use crate::span::Span;
    use crate::types::{PrimitiveType, Type};
    use aurora_effect_solver::EffectRow;

    fn const_function(name: &str, body: HirBlock, return_type: Type) -> HirFunction {
        HirFunction {
            name: name.into(),
            params: Vec::new(),
            declared_effects: EffectRow::empty(),
            inferred_effects: EffectRow::empty(),
            param_types: Vec::new(),
            return_type,
            zone: None,
            is_const: true,
            body,
            trait_obligations: Vec::new(),
            span: Span::default(),
        }
    }

    fn int_literal(value: i32) -> HirExpr {
        HirExpr {
            kind: HirExprKind::Literal(crate::ast::Literal::Integer(value.to_string())),
            ty: Type::Primitive(PrimitiveType::Int32),
            effects: EffectRow::empty(),
            span: Span::default(),
        }
    }

    #[test]
    fn evaluates_simple_const_function() {
        let block = HirBlock {
            statements: vec![HirStmt::Let {
                name: "x".into(),
                ty: Type::Primitive(PrimitiveType::Int32),
                expr: HirExpr {
                    kind: HirExprKind::Literal(crate::ast::Literal::Integer("2".into())),
                    ty: Type::Primitive(PrimitiveType::Int32),
                    effects: EffectRow::empty(),
                    span: Span::default(),
                },
                span: Span::default(),
            }],
            result: Some(Box::new(HirExpr {
                kind: HirExprKind::Binary {
                    op: crate::ast::BinaryOp::Add,
                    left: Box::new(HirExpr {
                        kind: HirExprKind::Variable("x".into()),
                        ty: Type::Primitive(PrimitiveType::Int32),
                        effects: EffectRow::empty(),
                        span: Span::default(),
                    }),
                    right: Box::new(HirExpr {
                        kind: HirExprKind::Literal(crate::ast::Literal::Integer("3".into())),
                        ty: Type::Primitive(PrimitiveType::Int32),
                        effects: EffectRow::empty(),
                        span: Span::default(),
                    }),
                },
                ty: Type::Primitive(PrimitiveType::Int32),
                effects: EffectRow::empty(),
                span: Span::default(),
            })),
            span: Span::default(),
        };
        let function = const_function("five", block, Type::Primitive(PrimitiveType::Int32));
        let module = HirModule {
            functions: vec![function.clone()],
            borrow_diagnostics: Vec::new(),
            trait_diagnostics: Vec::new(),
        };
        let result = evaluate_module(&module);
        assert_eq!(result.values.get("five"), Some(&ConstValue::Int(5)));
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn reports_unsupported_expr() {
        let block = HirBlock {
            statements: Vec::new(),
            result: Some(Box::new(HirExpr {
                kind: HirExprKind::Call {
                    callee: Box::new(HirExpr {
                        kind: HirExprKind::Variable("f".into()),
                        ty: Type::Primitive(PrimitiveType::Unit),
                        effects: EffectRow::empty(),
                        span: Span::default(),
                    }),
                    args: Vec::new(),
                },
                ty: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::empty(),
                span: Span::default(),
            })),
            span: Span::default(),
        };
        let function = const_function("bad", block, Type::Primitive(PrimitiveType::Unit));
        let module = HirModule {
            functions: vec![function.clone()],
            borrow_diagnostics: Vec::new(),
            trait_diagnostics: Vec::new(),
        };
        let result = evaluate_module(&module);
        assert!(result.values.is_empty());
        assert!(result
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("compile-time evaluation failed")));
    }

    #[test]
    fn evaluates_tuple_and_array_literals() {
        let tuple_block = HirBlock {
            statements: Vec::new(),
            result: Some(Box::new(HirExpr {
                kind: HirExprKind::Tuple(vec![int_literal(1), int_literal(2)]),
                ty: Type::Tuple(vec![
                    Type::Primitive(PrimitiveType::Int32),
                    Type::Primitive(PrimitiveType::Int32),
                ]),
                effects: EffectRow::empty(),
                span: Span::default(),
            })),
            span: Span::default(),
        };

        let array_block = HirBlock {
            statements: Vec::new(),
            result: Some(Box::new(HirExpr {
                kind: HirExprKind::Array(vec![int_literal(3), int_literal(4)]),
                ty: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::empty(),
                span: Span::default(),
            })),
            span: Span::default(),
        };

        let tuple_fn = const_function(
            "pair",
            tuple_block,
            Type::Tuple(vec![
                Type::Primitive(PrimitiveType::Int32),
                Type::Primitive(PrimitiveType::Int32),
            ]),
        );
        let array_fn = const_function("array", array_block, Type::Primitive(PrimitiveType::Unit));

        let module = HirModule {
            functions: vec![tuple_fn, array_fn],
            borrow_diagnostics: Vec::new(),
            trait_diagnostics: Vec::new(),
        };

        let result = evaluate_module(&module);
        assert_eq!(
            result.values.get("pair"),
            Some(&ConstValue::Tuple(vec![
                ConstValue::Int(1),
                ConstValue::Int(2)
            ]))
        );
        assert_eq!(
            result.values.get("array"),
            Some(&ConstValue::Array(vec![
                ConstValue::Int(3),
                ConstValue::Int(4)
            ]))
        );
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn reports_recursive_const_function() {
        let block = HirBlock {
            statements: Vec::new(),
            result: Some(Box::new(HirExpr {
                kind: HirExprKind::Call {
                    callee: Box::new(HirExpr {
                        kind: HirExprKind::Variable("loopy".into()),
                        ty: Type::Primitive(PrimitiveType::Unit),
                        effects: EffectRow::empty(),
                        span: Span::default(),
                    }),
                    args: Vec::new(),
                },
                ty: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::empty(),
                span: Span::default(),
            })),
            span: Span::default(),
        };

        let function = const_function("loopy", block, Type::Primitive(PrimitiveType::Unit));
        let module = HirModule {
            functions: vec![function],
            borrow_diagnostics: Vec::new(),
            trait_diagnostics: Vec::new(),
        };

        let result = evaluate_module(&module);
        assert!(result.values.is_empty());
        assert!(result.diagnostics.iter().any(|diag| {
            diag.labels.iter().any(|label| {
                label
                    .message
                    .as_ref()
                    .is_some_and(|msg| msg.contains("recursively evaluated"))
            })
        }));
    }
}
