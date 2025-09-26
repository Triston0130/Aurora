use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{
    Block, Expr, ExprKind, ImplDecl, ImplItem, Item, Literal, MacroDecl, MacroFragmentKind,
    MacroPattern, MacroRepeatKind, MacroRule, Module, Pattern, Statement, TraitDecl, TraitItem,
    ZoneArg,
};
use crate::diagnostics::{primary_label, Diagnostic};
use crate::span::Span;

pub fn expand_module(module: &Module) -> (Module, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();
    let mut scopes: Vec<HashMap<String, MacroDecl>> = Vec::new();
    let mut stack = Vec::new();
    let expanded = expand_module_inner(module, &mut scopes, &mut stack, &mut diagnostics);
    (expanded, diagnostics)
}

fn expand_module_inner(
    module: &Module,
    scopes: &mut Vec<HashMap<String, MacroDecl>>,
    stack: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Module {
    scopes.push(HashMap::new());
    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        match item {
            Item::Macro(decl) => {
                scopes
                    .last_mut()
                    .expect("scope stack not empty")
                    .insert(decl.name.clone(), decl.clone());
                items.push(Item::Macro(decl.clone()));
            }
            Item::Module(decl) => {
                let mut decl_clone = decl.clone();
                if let Some(body) = &decl.body {
                    decl_clone.body = Some(expand_module_inner(body, scopes, stack, diagnostics));
                }
                items.push(Item::Module(decl_clone));
            }
            Item::Function(func) => {
                let mut func = func.clone();
                func.body = expand_block(&func.body, scopes, stack, diagnostics);
                items.push(Item::Function(func));
            }
            Item::Zone(zone) => {
                let mut zone = zone.clone();
                zone.args = zone
                    .args
                    .into_iter()
                    .map(|mut arg| {
                        arg.value = expand_expr(&arg.value, scopes, stack, diagnostics);
                        arg
                    })
                    .collect();
                zone.body = expand_block(&zone.body, scopes, stack, diagnostics);
                items.push(Item::Zone(zone));
            }
            Item::Impl(decl) => {
                items.push(Item::Impl(expand_impl_decl(
                    decl,
                    scopes,
                    stack,
                    diagnostics,
                )));
            }
            Item::Trait(decl) => {
                items.push(Item::Trait(expand_trait_decl(
                    decl,
                    scopes,
                    stack,
                    diagnostics,
                )));
            }
            Item::Struct(decl) => items.push(Item::Struct(decl.clone())),
            Item::Enum(decl) => items.push(Item::Enum(decl.clone())),
            Item::Use(decl) => items.push(Item::Use(decl.clone())),
            Item::Extern(block) => items.push(Item::Extern(block.clone())),
        }
    }
    scopes.pop();
    Module {
        id: module.id,
        span: module.span,
        docs: module.docs.clone(),
        items,
    }
}

fn expand_block(
    block: &Block,
    scopes: &mut Vec<HashMap<String, MacroDecl>>,
    stack: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Block {
    scopes.push(HashMap::new());
    let mut statements = Vec::with_capacity(block.statements.len());
    for stmt in &block.statements {
        match stmt {
            Statement::Let {
                pattern,
                ty,
                value,
                span,
            } => {
                let expr = value
                    .as_ref()
                    .map(|value| expand_expr(value, scopes, stack, diagnostics));
                statements.push(Statement::Let {
                    pattern: pattern.clone(),
                    ty: ty.clone(),
                    value: expr,
                    span: *span,
                });
            }
            Statement::Expr(expr, span) => {
                let expr = expand_expr(expr, scopes, stack, diagnostics);
                statements.push(Statement::Expr(expr, *span));
            }
            Statement::Item(item, span) => {
                if let Item::Macro(decl) = item {
                    scopes
                        .last_mut()
                        .expect("scope stack not empty")
                        .insert(decl.name.clone(), decl.clone());
                }
                let expanded_item = expand_item(item, scopes, stack, diagnostics);
                statements.push(Statement::Item(expanded_item, *span));
            }
            Statement::Return { value, span } => {
                let value = value
                    .as_ref()
                    .map(|expr| expand_expr(expr, scopes, stack, diagnostics));
                statements.push(Statement::Return { value, span: *span });
            }
        }
    }
    let result = block
        .tail
        .as_ref()
        .map(|expr| Box::new(expand_expr(expr, scopes, stack, diagnostics)));
    scopes.pop();
    Block {
        statements,
        tail: result,
        span: block.span,
    }
}

fn expand_item(
    item: &Item,
    scopes: &mut Vec<HashMap<String, MacroDecl>>,
    stack: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Item {
    match item {
        Item::Function(func) => {
            let mut func = func.clone();
            func.body = expand_block(&func.body, scopes, stack, diagnostics);
            Item::Function(func)
        }
        Item::Module(decl) => {
            let mut decl = decl.clone();
            if let Some(body) = &decl.body {
                decl.body = Some(expand_module_inner(body, scopes, stack, diagnostics));
            }
            Item::Module(decl)
        }
        Item::Zone(zone) => {
            let mut zone = zone.clone();
            zone.args = zone
                .args
                .into_iter()
                .map(|mut arg| {
                    arg.value = expand_expr(&arg.value, scopes, stack, diagnostics);
                    arg
                })
                .collect();
            zone.body = expand_block(&zone.body, scopes, stack, diagnostics);
            Item::Zone(zone)
        }
        Item::Macro(decl) => Item::Macro(decl.clone()),
        Item::Struct(decl) => Item::Struct(decl.clone()),
        Item::Enum(decl) => Item::Enum(decl.clone()),
        Item::Trait(decl) => Item::Trait(expand_trait_decl(decl, scopes, stack, diagnostics)),
        Item::Impl(decl) => Item::Impl(expand_impl_decl(decl, scopes, stack, diagnostics)),
        Item::Use(decl) => Item::Use(decl.clone()),
        Item::Extern(block) => Item::Extern(block.clone()),
    }
}

fn expand_impl_decl(
    decl: &ImplDecl,
    scopes: &mut Vec<HashMap<String, MacroDecl>>,
    stack: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> ImplDecl {
    let mut decl = decl.clone();
    decl.items = decl
        .items
        .into_iter()
        .map(|item| match item {
            ImplItem::Function {
                signature,
                body,
                attributes,
                docs,
                span,
                id,
            } => ImplItem::Function {
                signature,
                body: Box::new(expand_block(&body, scopes, stack, diagnostics)),
                attributes,
                docs,
                span,
                id,
            },
            other => other,
        })
        .collect();
    decl
}

fn expand_trait_decl(
    decl: &TraitDecl,
    scopes: &mut Vec<HashMap<String, MacroDecl>>,
    stack: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> TraitDecl {
    let mut decl = decl.clone();
    decl.items = decl
        .items
        .into_iter()
        .map(|item| match item {
            TraitItem::Function {
                signature,
                default,
                attributes,
                docs,
                span,
                id,
            } => TraitItem::Function {
                signature,
                default: default
                    .map(|block| Box::new(expand_block(&block, scopes, stack, diagnostics))),
                attributes,
                docs,
                span,
                id,
            },
            other => other,
        })
        .collect();
    decl
}

fn expand_expr(
    expr: &Expr,
    scopes: &mut Vec<HashMap<String, MacroDecl>>,
    stack: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Expr {
    let mut expr = expr.clone();
    expr.kind = match &expr.kind {
        ExprKind::MacroCall { name, args } => match lookup_macro(scopes, name).cloned() {
            Some(decl) => {
                if stack.contains(name) {
                    diagnostics.push(
                        Diagnostic::error(format!("macro `{}` expands infinitely", name))
                            .with_label(primary_label(expr.span, "recursive macro invocation")),
                    );
                    ExprKind::Literal(Literal::Unit)
                } else {
                    stack.push(name.clone());
                    let expanded_args: Vec<Expr> = args
                        .iter()
                        .map(|arg| expand_expr(arg, scopes, stack, diagnostics))
                        .collect();
                    let mut expansion = None;
                    let mut fragment_error: Option<(String, MacroFragmentKind, Span)> = None;
                    let mut need_more_error: Option<String> = None;
                    let mut group_error = false;

                    for rule in &decl.rules {
                        match bind_macro_rule(rule, &expanded_args, expr.span) {
                            Ok((mut bindings, mut renames)) => {
                                let mut expanded_body =
                                    expand_block(&rule.body, scopes, stack, diagnostics);
                                rename_idents_in_block(&mut expanded_body, &renames);
                                assign_hygienic_names(&mut expanded_body, &mut renames);
                                bindings.extend(expanded_body.statements.into_iter());
                                expanded_body.statements = bindings;
                                expansion = Some(ExprKind::Block(expanded_body));
                                break;
                            }
                            Err(BindFailure::Fragment {
                                name: param_name,
                                kind,
                                span,
                            }) => {
                                fragment_error = Some((param_name, kind, span));
                            }
                            Err(BindFailure::NeedMore { name: param_name }) => {
                                need_more_error = Some(param_name);
                            }
                            Err(BindFailure::NeedMoreGroup) => {
                                group_error = true;
                            }
                            Err(BindFailure::Arity) | Err(BindFailure::ZeroProgress) => {
                                continue;
                            }
                        }
                    }

                    let result = if let Some(kind) = expansion {
                        kind
                    } else if let Some((param_name, kind, span)) = fragment_error {
                        diagnostics.push(
                            Diagnostic::error(format!(
                                "macro `{}` parameter `${}` expects {}",
                                name,
                                param_name,
                                fragment_name(kind)
                            ))
                            .with_label(primary_label(span, "argument does not match")),
                        );
                        ExprKind::Literal(Literal::Unit)
                    } else if let Some(param_name) = need_more_error {
                        diagnostics.push(
                            Diagnostic::error(format!(
                                "macro `{}` parameter `${}` requires at least one argument",
                                name, param_name
                            ))
                            .with_label(primary_label(expr.span, "missing argument")),
                        );
                        ExprKind::Literal(Literal::Unit)
                    } else if group_error {
                        diagnostics.push(
                            Diagnostic::error(format!(
                                "macro `{}` repetition group requires at least one iteration",
                                name
                            ))
                            .with_label(primary_label(expr.span, "missing repetition")),
                        );
                        ExprKind::Literal(Literal::Unit)
                    } else {
                        diagnostics.push(
                            Diagnostic::error(format!(
                                "macro `{}` has no rule for {} argument(s)",
                                name,
                                args.len()
                            ))
                            .with_label(primary_label(expr.span, "argument mismatch")),
                        );
                        ExprKind::Literal(Literal::Unit)
                    };
                    stack.pop();
                    result
                }
            }
            None => {
                diagnostics.push(
                    Diagnostic::error(format!("unknown macro `{}`", name))
                        .with_label(primary_label(expr.span, "macro not found")),
                );
                ExprKind::Literal(Literal::Unit)
            }
        },
        ExprKind::Block(block) => ExprKind::Block(expand_block(block, scopes, stack, diagnostics)),
        ExprKind::Literal(_) | ExprKind::Path(_) | ExprKind::Continue => expr.kind.clone(),
        ExprKind::Tuple(elems) => ExprKind::Tuple(
            elems
                .iter()
                .map(|elem| expand_expr(elem, scopes, stack, diagnostics))
                .collect(),
        ),
        ExprKind::Array(elems) => ExprKind::Array(
            elems
                .iter()
                .map(|elem| expand_expr(elem, scopes, stack, diagnostics))
                .collect(),
        ),
        ExprKind::Struct(struct_expr) => {
            let mut struct_expr = struct_expr.clone();
            struct_expr.fields = struct_expr
                .fields
                .into_iter()
                .map(|mut field| {
                    if let Some(expr) = field.expr {
                        field.expr = Some(expand_expr(&expr, scopes, stack, diagnostics));
                    }
                    field
                })
                .collect();
            if let Some(spread) = struct_expr.spread {
                struct_expr.spread =
                    Some(Box::new(expand_expr(&spread, scopes, stack, diagnostics)));
            }
            ExprKind::Struct(struct_expr)
        }
        ExprKind::Call { callee, args } => ExprKind::Call {
            callee: Box::new(expand_expr(callee, scopes, stack, diagnostics)),
            args: args
                .iter()
                .map(|arg| expand_expr(arg, scopes, stack, diagnostics))
                .collect(),
        },
        ExprKind::MethodCall {
            receiver,
            method,
            turbofish,
            args,
        } => ExprKind::MethodCall {
            receiver: Box::new(expand_expr(receiver, scopes, stack, diagnostics)),
            method: method.clone(),
            turbofish: turbofish.clone(),
            args: args
                .iter()
                .map(|arg| expand_expr(arg, scopes, stack, diagnostics))
                .collect(),
        },
        ExprKind::Field { base, field } => ExprKind::Field {
            base: Box::new(expand_expr(base, scopes, stack, diagnostics)),
            field: field.clone(),
        },
        ExprKind::Index { base, index } => ExprKind::Index {
            base: Box::new(expand_expr(base, scopes, stack, diagnostics)),
            index: Box::new(expand_expr(index, scopes, stack, diagnostics)),
        },
        ExprKind::Binary { op, left, right } => ExprKind::Binary {
            op: *op,
            left: Box::new(expand_expr(left, scopes, stack, diagnostics)),
            right: Box::new(expand_expr(right, scopes, stack, diagnostics)),
        },
        ExprKind::Unary { op, expr: inner } => ExprKind::Unary {
            op: *op,
            expr: Box::new(expand_expr(inner, scopes, stack, diagnostics)),
        },
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => ExprKind::If {
            condition: Box::new(expand_expr(condition, scopes, stack, diagnostics)),
            then_branch: expand_block(then_branch, scopes, stack, diagnostics),
            else_branch: else_branch
                .as_ref()
                .map(|expr| Box::new(expand_expr(expr, scopes, stack, diagnostics))),
        },
        ExprKind::Match { scrutinee, arms } => ExprKind::Match {
            scrutinee: Box::new(expand_expr(scrutinee, scopes, stack, diagnostics)),
            arms: arms
                .iter()
                .map(|arm| {
                    let mut arm = arm.clone();
                    if let Some(guard) = &arm.guard {
                        arm.guard = Some(expand_expr(guard, scopes, stack, diagnostics));
                    }
                    arm.body = expand_expr(&arm.body, scopes, stack, diagnostics);
                    arm
                })
                .collect(),
        },
        ExprKind::Loop(body) => ExprKind::Loop(expand_block(body, scopes, stack, diagnostics)),
        ExprKind::While { condition, body } => ExprKind::While {
            condition: Box::new(expand_expr(condition, scopes, stack, diagnostics)),
            body: expand_block(body, scopes, stack, diagnostics),
        },
        ExprKind::For {
            pattern,
            iterator,
            body,
        } => ExprKind::For {
            pattern: pattern.clone(),
            iterator: Box::new(expand_expr(iterator, scopes, stack, diagnostics)),
            body: expand_block(body, scopes, stack, diagnostics),
        },
        ExprKind::Async(block) => ExprKind::Async(expand_block(block, scopes, stack, diagnostics)),
        ExprKind::Await(expr) => {
            ExprKind::Await(Box::new(expand_expr(expr, scopes, stack, diagnostics)))
        }
        ExprKind::Spawn(task) => {
            ExprKind::Spawn(Box::new(expand_expr(task, scopes, stack, diagnostics)))
        }
        ExprKind::Actor(literal) => {
            let mut literal = literal.clone();
            literal.fields = literal
                .fields
                .into_iter()
                .map(|mut field| {
                    field.value = expand_expr(&field.value, scopes, stack, diagnostics);
                    field
                })
                .collect();
            ExprKind::Actor(literal)
        }
        ExprKind::Zone { name, args, body } => {
            let args = args
                .iter()
                .map(|arg| ZoneArg {
                    key: arg.key.clone(),
                    value: expand_expr(&arg.value, scopes, stack, diagnostics),
                })
                .collect();
            ExprKind::Zone {
                name: name.clone(),
                args,
                body: expand_block(body, scopes, stack, diagnostics),
            }
        }
        ExprKind::Handle { body, handlers } => {
            let body = Box::new(expand_expr(body, scopes, stack, diagnostics));
            let handlers = handlers
                .iter()
                .map(|handler| {
                    let mut handler = handler.clone();
                    handler.body = expand_block(&handler.body, scopes, stack, diagnostics);
                    handler
                })
                .collect();
            ExprKind::Handle { body, handlers }
        }
        ExprKind::Closure { params, body } => ExprKind::Closure {
            params: params.clone(),
            body: expand_block(body, scopes, stack, diagnostics),
        },
        ExprKind::Break(value) => ExprKind::Break(
            value
                .as_ref()
                .map(|expr| Box::new(expand_expr(expr, scopes, stack, diagnostics))),
        ),
        ExprKind::Assignment { left, op, right } => ExprKind::Assignment {
            left: Box::new(expand_expr(left, scopes, stack, diagnostics)),
            op: *op,
            right: Box::new(expand_expr(right, scopes, stack, diagnostics)),
        },
    };
    expr
}

fn lookup_macro<'a>(scopes: &'a [HashMap<String, MacroDecl>], name: &str) -> Option<&'a MacroDecl> {
    for scope in scopes.iter().rev() {
        if let Some(decl) = scope.get(name) {
            return Some(decl);
        }
    }
    None
}

fn rename_idents_in_block(block: &mut Block, renames: &HashMap<String, String>) {
    for stmt in &mut block.statements {
        rename_idents_in_statement(stmt, renames);
    }
    if let Some(tail) = &mut block.tail {
        rename_idents_in_expr(tail, renames);
    }
}

fn rename_idents_in_statement(stmt: &mut Statement, renames: &HashMap<String, String>) {
    match stmt {
        Statement::Let { value, .. } => {
            if let Some(expr) = value {
                rename_idents_in_expr(expr, renames);
            }
        }
        Statement::Return { value, .. } => {
            if let Some(expr) = value {
                rename_idents_in_expr(expr, renames);
            }
        }
        Statement::Expr(expr, _) => rename_idents_in_expr(expr, renames),
        Statement::Item(_, _) => {}
    }
}

fn rename_idents_in_expr(expr: &mut Expr, renames: &HashMap<String, String>) {
    match &mut expr.kind {
        ExprKind::Literal(_) => {}
        ExprKind::Path(path) => {
            if !path.leading_colon
                && path.segments.len() == 1
                && path.segments[0].generics.is_empty()
            {
                if let Some(new) = renames.get(&path.segments[0].ident) {
                    path.segments[0].ident = new.clone();
                }
            }
        }
        ExprKind::Tuple(elems) | ExprKind::Array(elems) => {
            for elem in elems {
                rename_idents_in_expr(elem, renames);
            }
        }
        ExprKind::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                if let Some(expr) = &mut field.expr {
                    rename_idents_in_expr(expr, renames);
                }
            }
            if let Some(spread) = &mut struct_expr.spread {
                rename_idents_in_expr(spread, renames);
            }
        }
        ExprKind::Call { callee, args } => {
            rename_idents_in_expr(callee, renames);
            for arg in args {
                rename_idents_in_expr(arg, renames);
            }
        }
        ExprKind::MacroCall { args, .. } => {
            for arg in args {
                rename_idents_in_expr(arg, renames);
            }
        }
        ExprKind::MethodCall { receiver, args, .. } => {
            rename_idents_in_expr(receiver, renames);
            for arg in args {
                rename_idents_in_expr(arg, renames);
            }
        }
        ExprKind::Field { base, .. } => rename_idents_in_expr(base, renames),
        ExprKind::Index { base, index } => {
            rename_idents_in_expr(base, renames);
            rename_idents_in_expr(index, renames);
        }
        ExprKind::Binary { left, right, .. } => {
            rename_idents_in_expr(left, renames);
            rename_idents_in_expr(right, renames);
        }
        ExprKind::Unary { expr: inner, .. } => rename_idents_in_expr(inner, renames),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            rename_idents_in_expr(condition, renames);
            rename_idents_in_block(then_branch, renames);
            if let Some(else_expr) = else_branch {
                rename_idents_in_expr(else_expr, renames);
            }
        }
        ExprKind::Block(block) => rename_idents_in_block(block, renames),
        ExprKind::Loop(body) => rename_idents_in_block(body, renames),
        ExprKind::While { condition, body } => {
            rename_idents_in_expr(condition, renames);
            rename_idents_in_block(body, renames);
        }
        ExprKind::For { iterator, body, .. } => {
            rename_idents_in_expr(iterator, renames);
            rename_idents_in_block(body, renames);
        }
        ExprKind::Match { scrutinee, arms } => {
            rename_idents_in_expr(scrutinee, renames);
            for arm in arms {
                if let Some(guard) = &mut arm.guard {
                    rename_idents_in_expr(guard, renames);
                }
                rename_idents_in_expr(&mut arm.body, renames);
            }
        }
        ExprKind::Handle { body, handlers } => {
            rename_idents_in_expr(body, renames);
            for handler in handlers {
                rename_idents_in_block(&mut handler.body, renames);
            }
        }
        ExprKind::Async(block) => rename_idents_in_block(block, renames),
        ExprKind::Await(inner) => rename_idents_in_expr(inner, renames),
        ExprKind::Spawn(task) => rename_idents_in_expr(task, renames),
        ExprKind::Zone { args, body, .. } => {
            for arg in args {
                rename_idents_in_expr(&mut arg.value, renames);
            }
            rename_idents_in_block(body, renames);
        }
        ExprKind::Assignment { left, right, .. } => {
            rename_idents_in_expr(left, renames);
            rename_idents_in_expr(right, renames);
        }
        ExprKind::Break(value) => {
            if let Some(expr) = value {
                rename_idents_in_expr(expr, renames);
            }
        }
        ExprKind::Continue => {}
        ExprKind::Closure { body, .. } => rename_idents_in_block(body, renames),
        ExprKind::Actor(literal) => {
            for field in &mut literal.fields {
                rename_idents_in_expr(&mut field.value, renames);
            }
        }
    }
}

static MACRO_BINDING_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn fresh_binding_name(base: &str) -> String {
    let id = MACRO_BINDING_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__macro_{}_{}", base, id)
}

#[derive(Clone)]
struct CaptureInfo {
    repeat: MacroRepeatKind,
    values: Vec<Expr>,
}

type CaptureMap = HashMap<String, CaptureInfo>;

fn bind_macro_rule(
    rule: &MacroRule,
    args: &[Expr],
    macro_span: Span,
) -> Result<(Vec<Statement>, HashMap<String, String>), BindFailure> {
    let mut captures: CaptureMap = HashMap::new();
    let mut idx = 0usize;
    bind_macro_pattern_seq(&rule.pattern, args, &mut idx, &mut captures)?;
    if idx != args.len() {
        return Err(BindFailure::Arity);
    }
    Ok(captures_to_statements(captures, macro_span))
}

fn bind_macro_pattern_seq(
    patterns: &[MacroPattern],
    args: &[Expr],
    idx: &mut usize,
    captures: &mut CaptureMap,
) -> Result<(), BindFailure> {
    for pattern in patterns {
        match pattern {
            MacroPattern::Fragment { name, kind, repeat } => {
                bind_fragment(name, *kind, *repeat, args, idx, captures)?;
            }
            MacroPattern::Group {
                elements,
                separator: _,
                repeat,
            } => {
                bind_group(elements, *repeat, args, idx, captures)?;
            }
        }
    }
    Ok(())
}

fn bind_fragment(
    name: &str,
    kind: MacroFragmentKind,
    repeat: MacroRepeatKind,
    args: &[Expr],
    idx: &mut usize,
    captures: &mut CaptureMap,
) -> Result<(), BindFailure> {
    let entry = captures
        .entry(name.to_string())
        .or_insert_with(|| CaptureInfo {
            repeat,
            values: Vec::new(),
        });
    match repeat {
        MacroRepeatKind::None => {
            if *idx >= args.len() {
                return Err(BindFailure::Arity);
            }
            let expr = args[*idx].clone();
            if !fragment_matches(kind, &expr) {
                return Err(BindFailure::Fragment {
                    name: name.to_string(),
                    kind,
                    span: args[*idx].span,
                });
            }
            entry.values.push(expr);
            *idx += 1;
        }
        MacroRepeatKind::Optional => {
            if *idx < args.len() {
                let expr = args[*idx].clone();
                if fragment_matches(kind, &expr) {
                    entry.values.push(expr);
                    *idx += 1;
                } else {
                    entry
                        .values
                        .push(Expr::dummy(ExprKind::Literal(Literal::Unit)));
                }
            } else {
                entry
                    .values
                    .push(Expr::dummy(ExprKind::Literal(Literal::Unit)));
            }
        }
        MacroRepeatKind::ZeroOrMore => {
            while *idx < args.len() {
                let expr = args[*idx].clone();
                if fragment_matches(kind, &expr) {
                    entry.values.push(expr);
                    *idx += 1;
                } else {
                    break;
                }
            }
        }
        MacroRepeatKind::OneOrMore => {
            let mut count = 0;
            while *idx < args.len() {
                let expr = args[*idx].clone();
                if fragment_matches(kind, &expr) {
                    entry.values.push(expr);
                    *idx += 1;
                    count += 1;
                } else {
                    break;
                }
            }
            if count == 0 {
                return Err(BindFailure::NeedMore {
                    name: name.to_string(),
                });
            }
        }
    }
    Ok(())
}

fn bind_group(
    elements: &[MacroPattern],
    repeat: MacroRepeatKind,
    args: &[Expr],
    idx: &mut usize,
    captures: &mut CaptureMap,
) -> Result<(), BindFailure> {
    match repeat {
        MacroRepeatKind::None => {
            if !match_group_iteration(elements, args, idx, captures)? {
                return Err(BindFailure::Arity);
            }
        }
        MacroRepeatKind::Optional => {
            let mut temp_captures = captures.clone();
            let mut temp_idx = *idx;
            if match_group_iteration(elements, args, &mut temp_idx, &mut temp_captures)? {
                *captures = temp_captures;
                *idx = temp_idx;
            }
        }
        MacroRepeatKind::ZeroOrMore => loop {
            let mut temp_captures = captures.clone();
            let mut temp_idx = *idx;
            match match_group_iteration(elements, args, &mut temp_idx, &mut temp_captures)? {
                true => {
                    if temp_idx == *idx {
                        return Err(BindFailure::ZeroProgress);
                    }
                    *captures = temp_captures;
                    *idx = temp_idx;
                }
                false => break,
            }
        },
        MacroRepeatKind::OneOrMore => {
            let mut count = 0;
            loop {
                let mut temp_captures = captures.clone();
                let mut temp_idx = *idx;
                match match_group_iteration(elements, args, &mut temp_idx, &mut temp_captures)? {
                    true => {
                        if temp_idx == *idx {
                            return Err(BindFailure::ZeroProgress);
                        }
                        *captures = temp_captures;
                        *idx = temp_idx;
                        count += 1;
                    }
                    false => break,
                }
            }
            if count == 0 {
                return Err(BindFailure::NeedMoreGroup);
            }
        }
    }
    Ok(())
}

fn match_group_iteration(
    elements: &[MacroPattern],
    args: &[Expr],
    idx: &mut usize,
    captures: &mut CaptureMap,
) -> Result<bool, BindFailure> {
    if *idx >= args.len() {
        return Ok(false);
    }
    let mut temp_captures = captures.clone();
    let mut temp_idx = *idx;
    match bind_macro_pattern_seq(elements, args, &mut temp_idx, &mut temp_captures) {
        Ok(()) => {
            *captures = temp_captures;
            *idx = temp_idx;
            Ok(true)
        }
        Err(BindFailure::Arity) | Err(BindFailure::Fragment { .. }) => Ok(false),
        Err(err) => Err(err),
    }
}

fn captures_to_statements(
    captures: CaptureMap,
    macro_span: Span,
) -> (Vec<Statement>, HashMap<String, String>) {
    let mut names: Vec<_> = captures.keys().cloned().collect();
    names.sort();
    let mut statements = Vec::new();
    let mut renames = HashMap::new();
    for name in names {
        let info = captures.get(&name).unwrap();
        let unique = fresh_binding_name(&name);
        renames.insert(name.clone(), unique.clone());
        let repeated = matches!(
            info.repeat,
            MacroRepeatKind::ZeroOrMore | MacroRepeatKind::OneOrMore
        ) || info.values.len() > 1;
        let value_expr = if repeated {
            Expr::dummy(ExprKind::Array(info.values.clone()))
        } else {
            info.values
                .first()
                .cloned()
                .unwrap_or_else(|| Expr::dummy(ExprKind::Literal(Literal::Unit)))
        };
        statements.push(Statement::Let {
            pattern: Pattern::Ident(unique),
            ty: None,
            value: Some(value_expr),
            span: macro_span,
        });
    }
    (statements, renames)
}

fn assign_hygienic_names(block: &mut Block, renames: &mut HashMap<String, String>) {
    for stmt in &mut block.statements {
        match stmt {
            Statement::Let { pattern, value, .. } => {
                if let Pattern::Ident(name) = pattern {
                    if !renames.contains_key(name) {
                        let fresh = fresh_binding_name(name);
                        renames.insert(name.clone(), fresh.clone());
                        if let Some(expr) = value {
                            rename_idents_in_expr(expr, renames);
                        }
                        *name = fresh;
                        continue;
                    }
                }
                if let Some(expr) = value {
                    rename_idents_in_expr(expr, renames);
                }
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    rename_idents_in_expr(expr, renames);
                }
            }
            Statement::Expr(expr, _) => rename_idents_in_expr(expr, renames),
            Statement::Item(_, _) => {}
        }
    }
    if let Some(tail) = &mut block.tail {
        rename_idents_in_expr(tail, renames);
    }
}

fn fragment_matches(kind: MacroFragmentKind, expr: &Expr) -> bool {
    match kind {
        MacroFragmentKind::Expr => true,
        MacroFragmentKind::Ident => matches!(
            &expr.kind,
            ExprKind::Path(path)
                if !path.leading_colon
                    && path.segments.len() == 1
                    && path.segments[0].generics.is_empty()
        ),
        MacroFragmentKind::Block => matches!(&expr.kind, ExprKind::Block(_)),
    }
}

fn fragment_name(kind: MacroFragmentKind) -> &'static str {
    match kind {
        MacroFragmentKind::Expr => "an expression",
        MacroFragmentKind::Ident => "an identifier",
        MacroFragmentKind::Block => "a block",
    }
}

enum BindFailure {
    Arity,
    Fragment {
        name: String,
        kind: MacroFragmentKind,
        span: Span,
    },
    NeedMore {
        name: String,
    },
    NeedMoreGroup,
    ZeroProgress,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Block, Expr, ExprKind, FunctionDecl, FunctionSig, GenericParams, Item, Literal, MacroDecl,
        MacroFragmentKind, MacroPattern, MacroRule, Module, NodeId, PathExpr, PathSegment, Pattern,
        Statement, TypeExpr, Visibility,
    };
    use crate::span::Span;

    fn empty_block() -> Block {
        Block {
            statements: Vec::new(),
            tail: None,
            span: Span::default(),
        }
    }

    fn macro_decl(name: &str, pattern: Vec<MacroPattern>, body: Block) -> MacroDecl {
        MacroDecl {
            attributes: Vec::new(),
            docs: Vec::new(),
            visibility: Visibility::Private,
            name: name.into(),
            rules: vec![MacroRule {
                pattern,
                body,
                span: Span::default(),
            }],
            span: Span::default(),
            id: NodeId::new(1),
        }
    }

    fn fragment(name: &str, kind: MacroFragmentKind, repeat: MacroRepeatKind) -> MacroPattern {
        MacroPattern::Fragment {
            name: name.into(),
            kind,
            repeat,
        }
    }

    fn group(
        elements: Vec<MacroPattern>,
        repeat: MacroRepeatKind,
        separator: Option<&str>,
    ) -> MacroPattern {
        MacroPattern::Group {
            elements,
            separator: separator.map(|s| s.to_string()),
            repeat,
        }
    }

    fn function_with_body(name: &str, body: Block) -> FunctionDecl {
        FunctionDecl {
            attributes: Vec::new(),
            docs: Vec::new(),
            visibility: Visibility::Private,
            signature: FunctionSig {
                constness: false,
                asyncness: false,
                name: name.into(),
                generics: GenericParams::default(),
                params: Vec::new(),
                return_type: Some(TypeExpr::Unit),
                effect: None,
                where_clause: Vec::new(),
            },
            body,
            span: Span::default(),
            id: NodeId::new(2),
        }
    }

    fn module_with_items(items: Vec<Item>) -> Module {
        Module {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items,
        }
    }

    #[test]
    fn expands_macro_arguments_into_block() {
        let lhs = Expr::path(PathExpr {
            leading_colon: false,
            segments: vec![PathSegment {
                ident: "lhs".into(),
                generics: Vec::new(),
            }],
            span: Span::default(),
        });
        let rhs = Expr::path(PathExpr {
            leading_colon: false,
            segments: vec![PathSegment {
                ident: "rhs".into(),
                generics: Vec::new(),
            }],
            span: Span::default(),
        });
        let body = Block {
            statements: Vec::new(),
            tail: Some(Box::new(Expr::dummy(ExprKind::Tuple(vec![lhs, rhs])))),
            span: Span::default(),
        };
        let macro_item = Item::Macro(macro_decl(
            "pair",
            vec![
                fragment("lhs", MacroFragmentKind::Expr, MacroRepeatKind::None),
                fragment("rhs", MacroFragmentKind::Expr, MacroRepeatKind::None),
            ],
            body,
        ));

        let call_expr = Expr::new(
            ExprKind::MacroCall {
                name: "pair".into(),
                args: vec![
                    Expr::literal(Literal::Integer("1".into())),
                    Expr::literal(Literal::Integer("2".into())),
                ],
            },
            Span::default(),
        );

        let function_body = Block {
            statements: vec![Statement::Expr(call_expr, Span::default())],
            tail: None,
            span: Span::default(),
        };
        let function_item = Item::Function(function_with_body("main", function_body));

        let module = module_with_items(vec![macro_item, function_item]);
        let (expanded, diagnostics) = expand_module(&module);
        assert!(diagnostics.is_empty());

        let expanded_func = match &expanded.items[1] {
            Item::Function(func) => func,
            other => panic!("expected function, found {:?}", other),
        };

        let expr_stmt = match &expanded_func.body.statements[0] {
            Statement::Expr(expr, _) => expr,
            other => panic!("expected expression statement, found {:?}", other),
        };

        match &expr_stmt.kind {
            ExprKind::Block(block) => {
                assert_eq!(block.statements.len(), 2);
                match &block.statements[0] {
                    Statement::Let {
                        pattern: Pattern::Ident(name),
                        ..
                    } => {
                        assert!(name.starts_with("__macro_lhs_"));
                    }
                    other => panic!("expected let statement, found {:?}", other),
                }
                match &block.statements[1] {
                    Statement::Let {
                        pattern: Pattern::Ident(name),
                        ..
                    } => {
                        assert!(name.starts_with("__macro_rhs_"));
                    }
                    other => panic!("expected let statement, found {:?}", other),
                }
            }
            other => panic!("expected macro expansion to yield block, found {:?}", other),
        }
    }

    #[test]
    fn reports_macro_recursion() {
        let recursive_body = Block {
            statements: Vec::new(),
            tail: Some(Box::new(Expr::new(
                ExprKind::MacroCall {
                    name: "self".into(),
                    args: Vec::new(),
                },
                Span::default(),
            ))),
            span: Span::default(),
        };
        let module = module_with_items(vec![
            Item::Macro(macro_decl("self", Vec::new(), recursive_body)),
            Item::Function(function_with_body(
                "main",
                Block {
                    statements: vec![Statement::Expr(
                        Expr::new(
                            ExprKind::MacroCall {
                                name: "self".into(),
                                args: Vec::new(),
                            },
                            Span::default(),
                        ),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
            )),
        ]);
        let (_, diagnostics) = expand_module(&module);
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message.contains("macro `self` expands infinitely")));
    }

    #[test]
    fn reports_unknown_macro_invocation() {
        let call_expr = Expr::new(
            ExprKind::MacroCall {
                name: "missing".into(),
                args: Vec::new(),
            },
            Span::default(),
        );

        let function_body = Block {
            statements: vec![Statement::Expr(call_expr, Span::default())],
            tail: None,
            span: Span::default(),
        };

        let module = module_with_items(vec![Item::Function(function_with_body(
            "main",
            function_body,
        ))]);
        let (_, diagnostics) = expand_module(&module);
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message.contains("unknown macro `missing`")));
    }

    #[test]
    fn reports_argument_mismatch() {
        let body = empty_block();
        let module = module_with_items(vec![
            Item::Macro(macro_decl(
                "m",
                vec![
                    fragment("x", MacroFragmentKind::Ident, MacroRepeatKind::None),
                    fragment("y", MacroFragmentKind::Ident, MacroRepeatKind::None),
                ],
                body,
            )),
            Item::Function(function_with_body(
                "main",
                Block {
                    statements: vec![Statement::Expr(
                        Expr::new(
                            ExprKind::MacroCall {
                                name: "m".into(),
                                args: vec![Expr::literal(Literal::Unit)],
                            },
                            Span::default(),
                        ),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
            )),
        ]);

        let (_, diagnostics) = expand_module(&module);
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message.contains("expects an identifier")));
    }

    #[test]
    fn optional_param_defaults_to_unit() {
        let body = Block {
            statements: vec![Statement::Expr(
                Expr::new(
                    ExprKind::Path(PathExpr {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "value".into(),
                            generics: Vec::new(),
                        }],
                        span: Span::default(),
                    }),
                    Span::default(),
                ),
                Span::default(),
            )],
            tail: None,
            span: Span::default(),
        };
        let module = module_with_items(vec![
            Item::Macro(macro_decl(
                "opt",
                vec![fragment(
                    "value",
                    MacroFragmentKind::Expr,
                    MacroRepeatKind::Optional,
                )],
                body,
            )),
            Item::Function(function_with_body(
                "main",
                Block {
                    statements: vec![Statement::Expr(
                        Expr::new(
                            ExprKind::MacroCall {
                                name: "opt".into(),
                                args: Vec::new(),
                            },
                            Span::default(),
                        ),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
            )),
        ]);

        let (expanded, diagnostics) = expand_module(&module);
        assert!(diagnostics.is_empty());

        let function = match &expanded.items[1] {
            Item::Function(func) => func,
            other => panic!("expected function, found {:?}", other),
        };

        let expr = match &function.body.statements[0] {
            Statement::Expr(expr, _) => expr,
            other => panic!("expected expression statement, found {:?}", other),
        };

        let block = match &expr.kind {
            ExprKind::Block(block) => block,
            other => panic!("expected block expression, found {:?}", other),
        };

        match &block.statements[0] {
            Statement::Let {
                pattern: Pattern::Ident(name),
                value: Some(expr),
                ..
            } => {
                assert!(name.starts_with("__macro_value_"));
                assert!(matches!(expr.kind, ExprKind::Literal(Literal::Unit)));
            }
            other => panic!("expected binding statement, found {:?}", other),
        }
    }

    #[test]
    fn zero_or_more_collects_arguments() {
        let body = Block {
            statements: vec![Statement::Expr(
                Expr::new(
                    ExprKind::Path(PathExpr {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "items".into(),
                            generics: Vec::new(),
                        }],
                        span: Span::default(),
                    }),
                    Span::default(),
                ),
                Span::default(),
            )],
            tail: None,
            span: Span::default(),
        };
        let module = module_with_items(vec![
            Item::Macro(macro_decl(
                "collect",
                vec![fragment(
                    "items",
                    MacroFragmentKind::Expr,
                    MacroRepeatKind::ZeroOrMore,
                )],
                body,
            )),
            Item::Function(function_with_body(
                "main",
                Block {
                    statements: vec![Statement::Expr(
                        Expr::new(
                            ExprKind::MacroCall {
                                name: "collect".into(),
                                args: vec![
                                    Expr::literal(Literal::Integer("1".into())),
                                    Expr::literal(Literal::Integer("2".into())),
                                    Expr::literal(Literal::Integer("3".into())),
                                ],
                            },
                            Span::default(),
                        ),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
            )),
        ]);

        let (expanded, diagnostics) = expand_module(&module);
        assert!(diagnostics.is_empty());

        let function = match &expanded.items[1] {
            Item::Function(func) => func,
            other => panic!("expected function, found {:?}", other),
        };

        let expr = match &function.body.statements[0] {
            Statement::Expr(expr, _) => expr,
            other => panic!("expected expression statement, found {:?}", other),
        };

        let block = match &expr.kind {
            ExprKind::Block(block) => block,
            other => panic!("expected block expression, found {:?}", other),
        };

        match &block.statements[0] {
            Statement::Let {
                pattern: Pattern::Ident(name),
                value: Some(expr),
                ..
            } => {
                assert!(name.starts_with("__macro_items_"));
                if let ExprKind::Array(elements) = &expr.kind {
                    assert_eq!(elements.len(), 3);
                } else {
                    panic!("expected array capture, found {:?}", expr.kind);
                }
            }
            other => panic!("expected binding statement, found {:?}", other),
        }
    }

    #[test]
    fn one_or_more_requires_argument() {
        let body = empty_block();
        let module = module_with_items(vec![
            Item::Macro(macro_decl(
                "atleast",
                vec![fragment(
                    "expr",
                    MacroFragmentKind::Expr,
                    MacroRepeatKind::OneOrMore,
                )],
                body,
            )),
            Item::Function(function_with_body(
                "main",
                Block {
                    statements: vec![Statement::Expr(
                        Expr::new(
                            ExprKind::MacroCall {
                                name: "atleast".into(),
                                args: Vec::new(),
                            },
                            Span::default(),
                        ),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
            )),
        ]);

        let (_, diagnostics) = expand_module(&module);
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message.contains("requires at least one argument")));
    }

    #[test]
    fn nested_group_collects_pairs() {
        let body = Block {
            statements: vec![Statement::Expr(
                Expr::new(
                    ExprKind::Path(PathExpr {
                        leading_colon: false,
                        segments: vec![PathSegment {
                            ident: "pairs".into(),
                            generics: Vec::new(),
                        }],
                        span: Span::default(),
                    }),
                    Span::default(),
                ),
                Span::default(),
            )],
            tail: None,
            span: Span::default(),
        };

        let pattern = vec![group(
            vec![
                fragment("key", MacroFragmentKind::Ident, MacroRepeatKind::None),
                fragment("value", MacroFragmentKind::Expr, MacroRepeatKind::None),
            ],
            MacroRepeatKind::OneOrMore,
            Some(","),
        )];

        let module = module_with_items(vec![
            Item::Macro(macro_decl("pairs", pattern, body)),
            Item::Function(function_with_body(
                "main",
                Block {
                    statements: vec![Statement::Expr(
                        Expr::new(
                            ExprKind::MacroCall {
                                name: "pairs".into(),
                                args: vec![
                                    Expr::path(PathExpr {
                                        leading_colon: false,
                                        segments: vec![PathSegment {
                                            ident: "foo".into(),
                                            generics: Vec::new(),
                                        }],
                                        span: Span::default(),
                                    }),
                                    Expr::literal(Literal::Integer("1".into())),
                                    Expr::path(PathExpr {
                                        leading_colon: false,
                                        segments: vec![PathSegment {
                                            ident: "bar".into(),
                                            generics: Vec::new(),
                                        }],
                                        span: Span::default(),
                                    }),
                                    Expr::literal(Literal::Integer("2".into())),
                                ],
                            },
                            Span::default(),
                        ),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
            )),
        ]);

        let (expanded, diagnostics) = expand_module(&module);
        assert!(diagnostics.is_empty());

        let function = match &expanded.items[1] {
            Item::Function(func) => func,
            other => panic!("expected function, found {:?}", other),
        };

        let expr = match &function.body.statements[0] {
            Statement::Expr(expr, _) => expr,
            other => panic!("expected expression statement, found {:?}", other),
        };

        let block = match &expr.kind {
            ExprKind::Block(block) => block,
            other => panic!("expected block expression, found {:?}", other),
        };

        match &block.statements[0] {
            Statement::Let {
                pattern: Pattern::Ident(name),
                value: Some(expr),
                ..
            } => {
                assert!(name.starts_with("__macro_key_"));
                if let ExprKind::Array(elements) = &expr.kind {
                    assert_eq!(elements.len(), 2);
                } else {
                    panic!("expected array capture, found {:?}", expr.kind);
                }
            }
            other => panic!("expected key binding, found {:?}", other),
        }

        match &block.statements[1] {
            Statement::Let {
                pattern: Pattern::Ident(name),
                value: Some(expr),
                ..
            } => {
                assert!(name.starts_with("__macro_value_"));
                if let ExprKind::Array(elements) = &expr.kind {
                    assert_eq!(elements.len(), 2);
                } else {
                    panic!("expected array capture, found {:?}", expr.kind);
                }
            }
            other => panic!("expected value binding, found {:?}", other),
        }
    }
}
