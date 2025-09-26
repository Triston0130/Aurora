//! Constraint generation scaffolding for Aurora's type/effect inference.

use crate::ast::*;
use crate::diagnostics::{primary_label, Diagnostic};
use crate::solver::{SolveError, SolverResult, TypeSubstitution};
use crate::span::Span;
use crate::trait_solver::TraitEnvironment;
use crate::types::*;
use crate::zone::{expr_to_simple_string, ZoneDescriptor, ZoneParam};
use aurora_effect_solver::{EffectRow, Substitution as EffectSubstitution};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConstraint {
    Equal(Type, Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectConstraint {
    Equal(EffectRow, EffectRow),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitConstraint {
    pub function: Option<String>,
    pub span: Span,
    pub subject: Type,
    pub trait_path: Vec<String>,
    pub generics: Vec<Type>,
}

#[derive(Debug, Default, Clone)]
pub struct ConstraintSet {
    pub type_constraints: Vec<TypeConstraint>,
    pub effect_constraints: Vec<EffectConstraint>,
    pub zone_diagnostics: Vec<Diagnostic>,
    pub trait_constraints: Vec<TraitConstraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTypeInfo {
    pub param_types: Vec<Type>,
    pub return_type: Type,
    pub effects: EffectRow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprInfo {
    pub ty: Type,
    pub effects: EffectRow,
    pub span: Span,
}

impl FunctionTypeInfo {
    pub fn apply_substitutions(
        &self,
        types: &TypeSubstitution,
        effects: &EffectSubstitution,
    ) -> FunctionTypeInfo {
        FunctionTypeInfo {
            param_types: self.param_types.iter().map(|t| types.apply(t)).collect(),
            return_type: types.apply(&self.return_type),
            effects: self.effects.apply_substitution(effects),
        }
    }
}

impl ExprInfo {
    pub fn apply_substitutions(
        &self,
        types: &TypeSubstitution,
        effects: &EffectSubstitution,
    ) -> ExprInfo {
        ExprInfo {
            ty: types.apply(&self.ty),
            effects: self.effects.apply_substitution(effects),
            span: self.span,
        }
    }
}

impl TraitConstraint {
    pub fn apply_substitutions(&self, types: &TypeSubstitution) -> TraitConstraint {
        TraitConstraint {
            function: self.function.clone(),
            span: self.span,
            subject: types.apply(&self.subject),
            trait_path: self.trait_path.clone(),
            generics: self.generics.iter().map(|ty| types.apply(ty)).collect(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct AnalysisResult {
    pub constraints: ConstraintSet,
    pub zones: HashMap<String, ZoneDescriptor>,
    pub functions: HashMap<String, FunctionTypeInfo>,
    pub locals: HashMap<String, HashMap<String, Vec<Type>>>,
    pub exprs: HashMap<String, Vec<ExprInfo>>,
    pub trait_env: TraitEnvironment,
}

#[derive(Debug, Clone)]
pub struct SolvedAnalysis {
    pub constraints: ConstraintSet,
    pub zones: HashMap<String, ZoneDescriptor>,
    pub functions: HashMap<String, FunctionTypeInfo>,
    pub locals: HashMap<String, HashMap<String, Vec<Type>>>,
    pub type_subst: TypeSubstitution,
    pub effect_subst: EffectSubstitution,
    pub exprs: HashMap<String, Vec<ExprInfo>>,
    pub trait_constraints: Vec<TraitConstraint>,
    pub trait_env: TraitEnvironment,
}

struct CapabilityRule {
    effect_prefix: &'static str,
    capability: &'static str,
    message: &'static str,
}

struct ZoneCapability {
    disallowed_effects: &'static [CapabilityRule],
}

fn zone_capability(zone: &str) -> ZoneCapability {
    match zone {
        "realtime" => ZoneCapability {
            disallowed_effects: &[
                CapabilityRule {
                    effect_prefix: "IO",
                    capability: "NoIO",
                    message: "blocking IO is not permitted in realtime zones",
                },
                CapabilityRule {
                    effect_prefix: "Async",
                    capability: "NoAsync",
                    message: "asynchronous tasks must complete before leaving a realtime zone",
                },
            ],
        },
        "gpu" => ZoneCapability {
            disallowed_effects: &[
                CapabilityRule {
                    effect_prefix: "IO",
                    capability: "NoIO",
                    message: "GPU kernels cannot perform host IO",
                },
                CapabilityRule {
                    effect_prefix: "State",
                    capability: "NoHostState",
                    message: "GPU kernels may not mutate host state",
                },
            ],
        },
        "sandbox" => ZoneCapability {
            disallowed_effects: &[
                CapabilityRule {
                    effect_prefix: "IO",
                    capability: "NoIO",
                    message: "sandboxed code cannot access IO facilities",
                },
                CapabilityRule {
                    effect_prefix: "State",
                    capability: "NoStateMutation",
                    message: "sandboxed code cannot mutate shared state",
                },
                CapabilityRule {
                    effect_prefix: "Unsafe",
                    capability: "NoUnsafe",
                    message: "sandbox forbids unsafe operations",
                },
            ],
        },
        _ => ZoneCapability {
            disallowed_effects: &[],
        },
    }
}

impl ConstraintSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_type(&mut self, constraint: TypeConstraint) {
        self.type_constraints.push(constraint);
    }

    pub fn push_effect(&mut self, constraint: EffectConstraint) {
        self.effect_constraints.push(constraint);
    }

    pub fn push_zone_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.zone_diagnostics.push(diagnostic);
    }

    pub fn push_trait(&mut self, constraint: TraitConstraint) {
        self.trait_constraints.push(constraint);
    }
}

impl AnalysisResult {
    pub fn solve(self) -> Result<SolvedAnalysis, SolveError> {
        let SolverResult {
            type_subst,
            effect_subst,
        } = SolverResult::solve(&self.constraints)?;

        let functions = self
            .functions
            .into_iter()
            .map(|(name, info)| {
                let resolved = info.apply_substitutions(&type_subst, &effect_subst);
                (name, resolved)
            })
            .collect();

        let locals = self
            .locals
            .into_iter()
            .map(|(function, bindings)| {
                let resolved = bindings
                    .into_iter()
                    .map(|(name, types)| {
                        let resolved_types =
                            types.into_iter().map(|ty| type_subst.apply(&ty)).collect();
                        (name, resolved_types)
                    })
                    .collect();
                (function, resolved)
            })
            .collect();

        let exprs = self
            .exprs
            .into_iter()
            .map(|(function, infos)| {
                let resolved = infos
                    .into_iter()
                    .map(|info| info.apply_substitutions(&type_subst, &effect_subst))
                    .collect();
                (function, resolved)
            })
            .collect();

        let trait_constraints = self
            .constraints
            .trait_constraints
            .iter()
            .map(|constraint| constraint.apply_substitutions(&type_subst))
            .collect();

        Ok(SolvedAnalysis {
            constraints: self.constraints,
            zones: self.zones,
            functions,
            locals,
            type_subst,
            effect_subst,
            exprs,
            trait_constraints,
            trait_env: self.trait_env,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprResult {
    pub ty: Type,
    pub effects: EffectRow,
}

impl ExprResult {
    fn default() -> Self {
        ExprResult {
            ty: Type::Primitive(PrimitiveType::Unit),
            effects: EffectRow::empty(),
        }
    }

    fn from_literal(lit: &Literal) -> Self {
        match lit {
            Literal::Integer(_) => ExprResult {
                ty: Type::Primitive(PrimitiveType::Int32),
                effects: EffectRow::empty(),
            },
            Literal::Float(_) => ExprResult {
                ty: Type::Primitive(PrimitiveType::Float32),
                effects: EffectRow::empty(),
            },
            Literal::String(_) => ExprResult {
                ty: Type::Primitive(PrimitiveType::String),
                effects: EffectRow::empty(),
            },
            Literal::Char(_) => ExprResult {
                ty: Type::Primitive(PrimitiveType::Int32),
                effects: EffectRow::empty(),
            },
            Literal::Boolean(_) => ExprResult {
                ty: Type::Primitive(PrimitiveType::Bool),
                effects: EffectRow::empty(),
            },
            Literal::Unit => ExprResult {
                ty: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::empty(),
            },
        }
    }
}

fn union_effects(a: EffectRow, b: EffectRow) -> EffectRow {
    a.union(&b)
}

fn effect_expr_to_row(expr: &EffectExpr) -> EffectRow {
    let names: Vec<String> = expr.labels.iter().map(|label| label.name.clone()).collect();
    let mut row = EffectRow::from_effects(names);
    if let Some(tail) = &expr.tail {
        let tail_row = EffectRow::with_tail(Vec::<String>::new(), tail.clone());
        row = row.union(&tail_row);
    }
    row
}

fn effect_without_label(row: &EffectRow, label: &str) -> EffectRow {
    let names: Vec<String> = row
        .fixed()
        .iter()
        .filter(|effect| effect.as_str() != label)
        .map(|effect| effect.as_str().to_string())
        .collect();
    if let Some(tail) = row.tail() {
        EffectRow::with_tail(names, tail.as_str().to_string())
    } else {
        EffectRow::from_effects(names)
    }
}

#[derive(Debug)]
pub struct Generator<'env> {
    pub env: &'env mut TypeEnv,
    pub ctx: &'env mut InferenceContext,
    pub constraints: ConstraintSet,
    pub zones: HashMap<String, ZoneDescriptor>,
    pub functions: HashMap<String, FunctionTypeInfo>,
    pub locals: HashMap<String, HashMap<String, Vec<Type>>>,
    pub exprs: HashMap<String, Vec<ExprInfo>>,
    function_stack: Vec<String>,
    generic_types: Vec<HashMap<String, TypeVarId>>,
    trait_env: TraitEnvironment,
}

impl<'env> Generator<'env> {
    pub fn new(env: &'env mut TypeEnv, ctx: &'env mut InferenceContext) -> Self {
        Self {
            env,
            ctx,
            constraints: ConstraintSet::new(),
            zones: HashMap::new(),
            functions: HashMap::new(),
            locals: HashMap::new(),
            exprs: HashMap::new(),
            function_stack: Vec::new(),
            generic_types: Vec::new(),
            trait_env: TraitEnvironment::new(),
        }
    }

    pub fn generate_module(mut self, module: &Module) -> AnalysisResult {
        self.generic_types.push(HashMap::new());
        self.env.push_scope();
        for item in &module.items {
            self.generate_item(item);
        }
        self.env.pop_scope();
        self.generic_types.pop();
        AnalysisResult {
            constraints: self.constraints,
            zones: self.zones,
            functions: self.functions,
            locals: self.locals,
            exprs: self.exprs,
            trait_env: self.trait_env,
        }
    }

    fn generate_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => self.generate_function(func),
            Item::Zone(zone) => self.generate_zone_decl(zone),
            Item::Impl(impl_block) => self.generate_impl(impl_block),
            _ => {}
        }
    }

    fn generalize_scheme(&self, ty: &Type, effects: &EffectRow) -> Scheme {
        Scheme::generalize(self.env, ty, effects)
    }

    fn record_expr(&mut self, expr: &Expr, result: &ExprResult) {
        if let Some(current) = self.function_stack.last() {
            self.exprs
                .entry(current.clone())
                .or_default()
                .push(ExprInfo {
                    ty: result.ty.clone(),
                    effects: result.effects.clone(),
                    span: expr.span,
                });
        }
    }

    fn push_generic_scope(&mut self) {
        self.generic_types.push(HashMap::new());
    }

    fn pop_generic_scope(&mut self) {
        self.generic_types.pop();
    }

    fn bind_generic_params(&mut self, generics: &GenericParams, span: Span) {
        let mut new_bindings: Vec<(String, TypeVarId, Vec<PathType>)> = Vec::new();
        for param in &generics.params {
            if let GenericParamKind::Type { bounds } = &param.kind {
                let var_id = self.ctx.fresh_type_var();
                new_bindings.push((param.name.clone(), var_id, bounds.clone()));
            }
        }

        if let Some(scope) = self.generic_types.last_mut() {
            for (name, var_id, _) in &new_bindings {
                scope.insert(name.clone(), *var_id);
            }
        }

        for (_name, var_id, bounds) in new_bindings {
            let subject = Type::Var(var_id);
            for bound in bounds {
                self.emit_trait_bound(subject.clone(), &bound, span);
            }
        }
    }

    fn bind_where_clause(&mut self, predicates: &[WherePredicate], span: Span) {
        for predicate in predicates {
            match predicate {
                WherePredicate::Bound { target, bounds } => {
                    let subject = self.fresh_type_from_annotation(target);
                    for bound in bounds {
                        self.emit_trait_bound(subject.clone(), bound, span);
                    }
                }
                WherePredicate::Equality { .. } => {
                    // TODO: equality predicates will be handled when associated types land.
                }
            }
        }
    }

    fn emit_trait_bound(&mut self, subject: Type, bound: &PathType, span: Span) {
        let trait_path: Vec<String> = bound.segments.iter().map(|seg| seg.ident.clone()).collect();
        let generics = bound
            .segments
            .last()
            .map(|seg| {
                seg.generics
                    .iter()
                    .map(|ty| self.fresh_type_from_annotation(ty))
                    .collect()
            })
            .unwrap_or_default();
        let function = self.function_stack.last().cloned();
        self.constraints.push_trait(TraitConstraint {
            function,
            span,
            subject,
            trait_path,
            generics,
        });
    }

    fn lookup_generic_var(&self, name: &str) -> Option<TypeVarId> {
        for scope in self.generic_types.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(*id);
            }
        }
        None
    }

    fn generate_impl(&mut self, impl_block: &ImplDecl) {
        let trait_ref = match &impl_block.trait_ref {
            Some(trait_ref) => trait_ref,
            None => return,
        };
        let subject = match self.concrete_type_from_annotation(&impl_block.for_type) {
            Some(ty) => ty,
            None => return,
        };
        let trait_path: Vec<String> = trait_ref
            .segments
            .iter()
            .map(|seg| seg.ident.clone())
            .collect();
        self.trait_env.add_impl(trait_path, subject);
    }

    fn collect_concrete_generics(&mut self, generics: &[TypeExpr]) -> Option<Vec<Type>> {
        let mut result = Vec::new();
        for ty in generics {
            result.push(self.concrete_type_from_annotation(ty)?);
        }
        Some(result)
    }

    fn concrete_type_from_annotation(&mut self, ty: &TypeExpr) -> Option<Type> {
        match ty {
            TypeExpr::Path(path) => {
                if let Some(last) = path.segments.last() {
                    match last.ident.as_str() {
                        "Int32" => Some(Type::Primitive(PrimitiveType::Int32)),
                        "Int64" => Some(Type::Primitive(PrimitiveType::Int64)),
                        "Float32" => Some(Type::Primitive(PrimitiveType::Float32)),
                        "Float64" => Some(Type::Primitive(PrimitiveType::Float64)),
                        "Bool" => Some(Type::Primitive(PrimitiveType::Bool)),
                        "String" => Some(Type::Primitive(PrimitiveType::String)),
                        "Unit" => Some(Type::Primitive(PrimitiveType::Unit)),
                        _ident => Some(Type::TraitObject {
                            path: path.segments.iter().map(|seg| seg.ident.clone()).collect(),
                            generics: self
                                .collect_concrete_generics(&last.generics)
                                .unwrap_or_default(),
                        }),
                    }
                } else {
                    None
                }
            }
            TypeExpr::TraitObject(path) => Some(Type::TraitObject {
                path: path.segments.iter().map(|seg| seg.ident.clone()).collect(),
                generics: Vec::new(),
            }),
            _ => None,
        }
    }

    fn fresh_effect_row(&mut self) -> EffectRow {
        let EffectVarId(id) = self.ctx.fresh_effect_var();
        EffectRow::with_tail(Vec::<String>::new(), format!("e{id}"))
    }

    fn record_local(&mut self, name: &str, ty: Type) {
        if let Some(current) = self.function_stack.last() {
            let entry = self
                .locals
                .entry(current.clone())
                .or_default()
                .entry(name.to_string())
                .or_default();
            entry.push(ty);
        }
    }

    fn generate_zone_decl(&mut self, zone: &ZoneDecl) {
        let mut params = Vec::new();
        for arg in &zone.args {
            if let Some(value) = expr_to_simple_string(&arg.value) {
                params.push(ZoneParam::KeyValue {
                    key: arg.key.clone(),
                    value,
                });
            }
        }
        let descriptor = ZoneDescriptor {
            zone_type: zone.name.clone(),
            params,
            origin: zone.span,
        };
        self.zones.insert(zone.name.clone(), descriptor.clone());

        // Expose the zone as implementing its capability trait so user code can
        // write `where Zone::foo: ZoneCapability::foo` style bounds.
        let trait_path = vec!["ZoneCapability".into(), zone.name.clone()];
        let subject = Type::TraitObject {
            path: vec!["Zone".into(), zone.name.clone()],
            generics: Vec::new(),
        };
        self.trait_env.add_impl(trait_path, subject);

        self.env.push_scope();
        let body_result = self.generate_block(&zone.body);
        self.env.pop_scope();

        if let Some(diag) =
            self.validate_zone_capabilities(&zone.name, &descriptor, &body_result.effects)
        {
            self.constraints.push_zone_diagnostic(diag);
        }
    }

    fn generate_function(&mut self, func: &FunctionDecl) {
        let signature = &func.signature;
        self.locals.insert(signature.name.clone(), HashMap::new());
        self.function_stack.push(signature.name.clone());
        self.push_generic_scope();
        self.bind_generic_params(&signature.generics, func.span);
        self.bind_where_clause(&signature.where_clause, func.span);
        let zone_descriptor = func
            .attributes
            .iter()
            .find_map(ZoneDescriptor::from_attribute);
        if let Some(descriptor) = zone_descriptor.clone() {
            self.zones.insert(signature.name.clone(), descriptor);
        }

        if let Some(descriptor) = &zone_descriptor {
            let trait_path = vec!["ZoneCapability".into(), descriptor.zone_type.clone()];
            let subject = Type::TraitObject {
                path: vec!["Zone".into(), descriptor.zone_type.clone()],
                generics: Vec::new(),
            };
            self.constraints.push_trait(TraitConstraint {
                function: Some(signature.name.clone()),
                span: func.span,
                subject,
                trait_path,
                generics: Vec::new(),
            });
        }

        let mut param_types = Vec::new();
        self.env.push_scope();
        for param in &signature.params {
            let ty = self.fresh_type_from_annotation(&param.ty);
            self.env
                .insert(param.name.clone(), Scheme::monomorphic(ty.clone()));
            param_types.push(ty);
        }
        let return_type = signature
            .return_type
            .as_ref()
            .map(|ty| self.fresh_type_from_annotation(ty))
            .unwrap_or_else(|| Type::Primitive(PrimitiveType::Unit));

        let declared_effects = signature
            .effect
            .as_ref()
            .map(effect_expr_to_row)
            .unwrap_or_else(EffectRow::empty);

        let body_result = self.generate_block(&func.body);
        let combined_effects = declared_effects.union(&body_result.effects);

        if let Some(descriptor) = &zone_descriptor {
            if let Some(diag) =
                self.validate_zone_capabilities(&signature.name, descriptor, &combined_effects)
            {
                self.constraints.push_zone_diagnostic(diag);
            }
        }

        self.constraints.push_effect(EffectConstraint::Equal(
            body_result.effects.clone(),
            declared_effects.clone(),
        ));

        let fn_param_types = param_types.clone();
        let fn_type = Type::Function {
            params: param_types,
            ret: Box::new(return_type.clone()),
            effects: combined_effects.clone(),
        };
        self.env.pop_scope();
        self.pop_generic_scope();
        let scheme = self.generalize_scheme(&fn_type, &combined_effects);
        self.env.insert(signature.name.clone(), scheme);

        self.constraints.push_type(TypeConstraint::Equal(
            body_result.ty.clone(),
            return_type.clone(),
        ));

        self.functions.insert(
            signature.name.clone(),
            FunctionTypeInfo {
                param_types: fn_param_types,
                return_type,
                effects: combined_effects,
            },
        );
        self.function_stack.pop();
    }

    fn validate_zone_capabilities(
        &self,
        function: &str,
        descriptor: &ZoneDescriptor,
        effects: &EffectRow,
    ) -> Option<Diagnostic> {
        let caps = zone_capability(&descriptor.zone_type);
        for effect in effects.fixed() {
            let name = effect.as_str();
            if let Some(rule) = caps
                .disallowed_effects
                .iter()
                .find(|rule| name.starts_with(rule.effect_prefix))
            {
                let diagnostic = Diagnostic::error(format!(
                    "zone `{}` requires capability `{}`",
                    descriptor.zone_type, rule.capability
                ))
                .with_label(primary_label(
                    descriptor.origin,
                    format!("{} (effect `{}`)", rule.message, name),
                ))
                .with_note(format!("while checking function `{}`", function));
                return Some(diagnostic);
            }
        }

        if let Some(tail) = effects.tail() {
            if !caps.disallowed_effects.is_empty() {
                let diagnostic = Diagnostic::error(format!(
                    "zone `{}` cannot ensure capability coverage",
                    descriptor.zone_type
                ))
                .with_label(primary_label(
                    descriptor.origin,
                    format!(
                        "effect tail `{}` prevents verifying zone capabilities",
                        tail.as_str()
                    ),
                ))
                .with_note(format!("while checking function `{}`", function));
                return Some(diagnostic);
            }
        }

        None
    }

    fn generate_block(&mut self, block: &Block) -> ExprResult {
        self.env.push_scope();
        let mut result = ExprResult::default();
        for stmt in &block.statements {
            match stmt {
                Statement::Let {
                    pattern, ty, value, ..
                } => {
                    let annotation_ty = ty
                        .as_ref()
                        .map(|annotation| self.fresh_type_from_annotation(annotation));
                    if let Some(expr) = value {
                        let value_res = self.generate_expr(expr);
                        result.effects = union_effects(result.effects, value_res.effects.clone());
                        if let Some(annot_ty) = annotation_ty.clone() {
                            self.constraints.push_type(TypeConstraint::Equal(
                                value_res.ty.clone(),
                                annot_ty.clone(),
                            ));
                            self.bind_pattern(pattern, annot_ty);
                        } else {
                            self.bind_pattern(pattern, value_res.ty);
                        }
                    } else if let Some(annot_ty) = annotation_ty {
                        self.bind_pattern(pattern, annot_ty);
                    }
                }
                Statement::Expr(expr, _) => {
                    let expr_res = self.generate_expr(expr);
                    result.effects = union_effects(result.effects, expr_res.effects);
                }
                Statement::Item(item, _) => {
                    self.generate_item(item);
                }
                Statement::Return {
                    value: expr_opt, ..
                } => {
                    if let Some(expr) = expr_opt {
                        let expr_res = self.generate_expr(expr);
                        result.ty = expr_res.ty;
                        result.effects = union_effects(result.effects, expr_res.effects);
                    } else {
                        result.ty = Type::Primitive(PrimitiveType::Unit);
                    }
                    self.env.pop_scope();
                    return result;
                }
            }
        }
        if let Some(tail) = &block.tail {
            let tail_res = self.generate_expr(tail);
            result.ty = tail_res.ty;
            result.effects = union_effects(result.effects, tail_res.effects);
        }
        self.env.pop_scope();
        result
    }

    fn generate_expr(&mut self, expr: &Expr) -> ExprResult {
        let result = match &expr.kind {
            ExprKind::Literal(lit) => ExprResult::from_literal(lit),
            ExprKind::MacroCall { args, .. } => {
                let mut effects = EffectRow::empty();
                for arg in args {
                    let res = self.generate_expr(arg);
                    effects = union_effects(effects, res.effects);
                }
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects,
                }
            }
            ExprKind::Path(path) => self.generate_path_expr(path),
            ExprKind::Tuple(elems) => {
                let mut effects = EffectRow::empty();
                let mut types = Vec::new();
                for elem in elems {
                    let res = self.generate_expr(elem);
                    effects = union_effects(effects, res.effects);
                    types.push(res.ty);
                }
                ExprResult {
                    ty: Type::Tuple(types),
                    effects,
                }
            }
            ExprKind::Array(elems) => {
                let mut effects = EffectRow::empty();
                for elem in elems {
                    let res = self.generate_expr(elem);
                    effects = union_effects(effects, res.effects);
                }
                ExprResult {
                    ty: Type::Var(self.ctx.fresh_type_var()),
                    effects,
                }
            }
            ExprKind::Struct(struct_expr) => self.generate_struct_expr(struct_expr),
            ExprKind::Actor(actor) => {
                let mut effects = EffectRow::empty();
                for field in &actor.fields {
                    let res = self.generate_expr(&field.value);
                    effects = union_effects(effects, res.effects);
                }
                ExprResult {
                    ty: Type::Var(self.ctx.fresh_type_var()),
                    effects,
                }
            }
            ExprKind::Call { callee, args } => {
                let callee_res = self.generate_expr(callee);
                let mut effects = callee_res.effects.clone();
                let mut param_types = Vec::new();
                for arg in args {
                    let res = self.generate_expr(arg);
                    effects = union_effects(effects, res.effects.clone());
                    param_types.push(res.ty);
                }
                let return_ty = Type::Var(self.ctx.fresh_type_var());
                let call_effects = self.fresh_effect_row();
                let fn_type = Type::Function {
                    params: param_types,
                    ret: Box::new(return_ty.clone()),
                    effects: call_effects.clone(),
                };
                self.constraints
                    .push_type(TypeConstraint::Equal(callee_res.ty.clone(), fn_type));
                effects = union_effects(effects, call_effects);
                ExprResult {
                    ty: return_ty,
                    effects,
                }
            }
            ExprKind::MethodCall { receiver, args, .. } => {
                let mut effects = EffectRow::empty();
                let recv_res = self.generate_expr(receiver);
                effects = union_effects(effects, recv_res.effects);
                for arg in args {
                    let res = self.generate_expr(arg);
                    effects = union_effects(effects, res.effects);
                }
                ExprResult {
                    ty: Type::Var(self.ctx.fresh_type_var()),
                    effects,
                }
            }
            ExprKind::Field { base, .. } => self.generate_expr(base),
            ExprKind::Index { base, index } => {
                let base_res = self.generate_expr(base);
                let index_res = self.generate_expr(index);
                ExprResult {
                    ty: Type::Var(self.ctx.fresh_type_var()),
                    effects: union_effects(base_res.effects, index_res.effects),
                }
            }
            ExprKind::Binary { left, right, .. } => {
                let left_res = self.generate_expr(left);
                let right_res = self.generate_expr(right);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Int32),
                    effects: union_effects(left_res.effects, right_res.effects),
                }
            }
            ExprKind::Unary { expr, .. } => self.generate_expr(expr),
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_res = self.generate_expr(condition);
                let then_res = self.generate_block(then_branch);
                let mut effects = union_effects(cond_res.effects, then_res.effects.clone());
                let ty = if let Some(else_expr) = else_branch {
                    let else_res = self.generate_expr(else_expr);
                    effects = union_effects(effects, else_res.effects);
                    then_res.ty
                } else {
                    Type::Primitive(PrimitiveType::Unit)
                };
                ExprResult { ty, effects }
            }
            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_res = self.generate_expr(scrutinee);
                let mut effects = scrutinee_res.effects;
                let mut ty = Type::Primitive(PrimitiveType::Unit);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        let guard_res = self.generate_expr(guard);
                        effects = union_effects(effects, guard_res.effects);
                    }
                    let body_res = self.generate_expr(&arm.body);
                    effects = union_effects(effects, body_res.effects.clone());
                    ty = body_res.ty;
                }
                ExprResult { ty, effects }
            }
            ExprKind::Loop(body) => {
                let res = self.generate_block(body);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects: res.effects,
                }
            }
            ExprKind::While { condition, body } => {
                let cond = self.generate_expr(condition);
                let body_res = self.generate_block(body);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects: union_effects(cond.effects, body_res.effects),
                }
            }
            ExprKind::For {
                pattern,
                iterator,
                body,
            } => {
                let iter_res = self.generate_expr(iterator);
                let body_res = self.generate_block(body);
                let element_ty = Type::Var(self.ctx.fresh_type_var());
                self.bind_pattern(pattern, element_ty);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects: union_effects(iter_res.effects, body_res.effects),
                }
            }
            ExprKind::Async(block) => {
                let res = self.generate_block(block);
                ExprResult {
                    ty: Type::Var(self.ctx.fresh_type_var()),
                    effects: union_effects(res.effects, EffectRow::from_effects(["Async"])),
                }
            }
            ExprKind::Await(expr) => self.generate_expr(expr),
            ExprKind::Spawn(task) => {
                let res = self.generate_expr(task);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects: union_effects(res.effects, EffectRow::from_effects(["Async"])),
                }
            }
            ExprKind::Handle { body, handlers } => {
                let mut result = self.generate_expr(body);
                for handler in handlers {
                    let handler_res = self.generate_block(&handler.body);
                    self.constraints.push_type(TypeConstraint::Equal(
                        handler_res.ty.clone(),
                        result.ty.clone(),
                    ));
                    let remaining = effect_without_label(&result.effects, &handler.label);
                    result.effects = union_effects(remaining, handler_res.effects.clone());
                }
                result
            }
            ExprKind::Zone { body, .. } => {
                let res = self.generate_block(body);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects: res.effects,
                }
            }
            ExprKind::Block(block) => self.generate_block(block),
            ExprKind::Closure { params, body } => {
                let body_res = self.generate_block(body);
                let param_types = params
                    .iter()
                    .map(|_| Type::Var(self.ctx.fresh_type_var()))
                    .collect();
                let fn_type = Type::Function {
                    params: param_types,
                    ret: Box::new(body_res.ty.clone()),
                    effects: body_res.effects.clone(),
                };
                ExprResult {
                    ty: fn_type,
                    effects: EffectRow::empty(),
                }
            }
            ExprKind::Break(expr) => {
                if let Some(value) = expr.as_ref() {
                    let res = self.generate_expr(value);
                    ExprResult {
                        ty: Type::Primitive(PrimitiveType::Unit),
                        effects: res.effects,
                    }
                } else {
                    ExprResult {
                        ty: Type::Primitive(PrimitiveType::Unit),
                        effects: EffectRow::empty(),
                    }
                }
            }
            ExprKind::Continue => ExprResult {
                ty: Type::Primitive(PrimitiveType::Unit),
                effects: EffectRow::empty(),
            },
            ExprKind::Assignment { left, right, .. } => {
                let left_res = self.generate_expr(left);
                let right_res = self.generate_expr(right);
                ExprResult {
                    ty: Type::Primitive(PrimitiveType::Unit),
                    effects: union_effects(left_res.effects, right_res.effects),
                }
            }
        };
        self.record_expr(expr, &result);
        result
    }

    fn generate_path_expr(&mut self, path: &PathExpr) -> ExprResult {
        if !path.leading_colon && path.segments.len() == 1 {
            let name = &path.segments[0].ident;
            if let Some(scheme) = self.env.lookup(name) {
                let (ty, effects) = scheme.instantiate(self.ctx);
                ExprResult { ty, effects }
            } else {
                ExprResult {
                    ty: Type::Var(self.ctx.fresh_type_var()),
                    effects: EffectRow::empty(),
                }
            }
        } else {
            ExprResult {
                ty: Type::Var(self.ctx.fresh_type_var()),
                effects: EffectRow::empty(),
            }
        }
    }

    fn generate_struct_expr(&mut self, expr: &StructExpr) -> ExprResult {
        let mut effects = EffectRow::empty();
        for field in &expr.fields {
            if let Some(value) = &field.expr {
                let res = self.generate_expr(value);
                effects = union_effects(effects, res.effects);
            } else {
                let shorthand_path = PathExpr {
                    leading_colon: false,
                    segments: vec![PathSegment {
                        ident: field.name.clone(),
                        generics: Vec::new(),
                    }],
                    span: Span::default(),
                };
                let res = self.generate_path_expr(&shorthand_path);
                effects = union_effects(effects, res.effects);
            }
        }
        if let Some(spread) = &expr.spread {
            let spread_res = self.generate_expr(spread);
            effects = union_effects(effects, spread_res.effects);
        }
        ExprResult {
            ty: Type::Var(self.ctx.fresh_type_var()),
            effects,
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, expected_ty: Type) {
        self.bind_pattern_internal(pattern, expected_ty);
    }

    fn bind_pattern_internal(&mut self, pattern: &Pattern, expected_ty: Type) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Ident(name) => {
                let scheme = self.generalize_scheme(&expected_ty, &EffectRow::empty());
                self.env.insert(name.clone(), scheme);
                self.record_local(name, expected_ty);
            }
            Pattern::Binding {
                name, subpattern, ..
            } => {
                let scheme = self.generalize_scheme(&expected_ty, &EffectRow::empty());
                self.env.insert(name.clone(), scheme);
                self.record_local(name, expected_ty.clone());
                self.bind_pattern_internal(subpattern, expected_ty);
            }
            Pattern::Move(inner) => {
                self.bind_pattern_internal(inner, expected_ty);
            }
            Pattern::Or(alternatives) => {
                for alt in alternatives {
                    self.bind_pattern_internal(alt, expected_ty.clone());
                }
            }
            Pattern::Tuple(elements) => {
                let element_types: Vec<Type> = elements
                    .iter()
                    .map(|_| Type::Var(self.ctx.fresh_type_var()))
                    .collect();
                self.constraints.push_type(TypeConstraint::Equal(
                    expected_ty.clone(),
                    Type::Tuple(element_types.clone()),
                ));
                for (pat, ty) in elements.iter().zip(element_types) {
                    self.bind_pattern_internal(pat, ty);
                }
            }
            Pattern::Slice {
                prefix,
                rest,
                suffix,
            } => {
                let element_ty = Type::Var(self.ctx.fresh_type_var());
                for pat in prefix {
                    self.bind_pattern_internal(pat, element_ty.clone());
                }
                if let Some(rest_pat) = rest {
                    self.bind_pattern_internal(rest_pat, expected_ty.clone());
                }
                for pat in suffix {
                    self.bind_pattern_internal(pat, element_ty.clone());
                }
            }
            Pattern::Struct { fields, .. } => {
                for field in fields {
                    if let Some(subpattern) = &field.pattern {
                        let field_ty = Type::Var(self.ctx.fresh_type_var());
                        self.bind_pattern_internal(subpattern, field_ty);
                    } else {
                        let field_ty = Type::Var(self.ctx.fresh_type_var());
                        let scheme = self.generalize_scheme(&field_ty, &EffectRow::empty());
                        self.env.insert(field.name.clone(), scheme);
                        self.record_local(&field.name, field_ty);
                    }
                }
            }
            Pattern::Enum { inner, .. } => {
                if let Some(subpattern) = inner.as_ref() {
                    let inner_ty = Type::Var(self.ctx.fresh_type_var());
                    self.bind_pattern_internal(subpattern, inner_ty);
                }
            }
            Pattern::Literal(lit) => {
                let lit_ty = match lit {
                    Literal::Integer(_) => Type::Primitive(PrimitiveType::Int32),
                    Literal::Float(_) => Type::Primitive(PrimitiveType::Float32),
                    Literal::String(_) => Type::Primitive(PrimitiveType::String),
                    Literal::Boolean(_) => Type::Primitive(PrimitiveType::Bool),
                    Literal::Char(_) => Type::Primitive(PrimitiveType::Int32),
                    Literal::Unit => Type::Primitive(PrimitiveType::Unit),
                };
                self.constraints
                    .push_type(TypeConstraint::Equal(expected_ty, lit_ty));
            }
            Pattern::Reference { mutable, inner } => {
                let inner_ty = Type::Var(self.ctx.fresh_type_var());
                let reference_ty = Type::Reference {
                    lifetime: self.ctx.fresh_lifetime_var(),
                    mutable: *mutable,
                    inner: Box::new(inner_ty.clone()),
                };
                self.constraints
                    .push_type(TypeConstraint::Equal(expected_ty, reference_ty));
                self.bind_pattern_internal(inner, inner_ty);
            }
        }
    }

    fn fresh_type_from_annotation(&mut self, ty: &TypeExpr) -> Type {
        match ty {
            TypeExpr::Path(path) => {
                if let Some(last) = path.segments.last() {
                    if let Some(var_id) = self.lookup_generic_var(&last.ident) {
                        return Type::Var(var_id);
                    }
                    match last.ident.as_str() {
                        "Int32" => Type::Primitive(PrimitiveType::Int32),
                        "Float32" => Type::Primitive(PrimitiveType::Float32),
                        "Bool" => Type::Primitive(PrimitiveType::Bool),
                        "String" => Type::Primitive(PrimitiveType::String),
                        "Unit" => Type::Primitive(PrimitiveType::Unit),
                        _ => Type::Var(self.ctx.fresh_type_var()),
                    }
                } else {
                    Type::Var(self.ctx.fresh_type_var())
                }
            }
            TypeExpr::Reference { mutable, inner, .. } => Type::Reference {
                lifetime: self.ctx.fresh_lifetime_var(),
                mutable: *mutable,
                inner: Box::new(self.fresh_type_from_annotation(inner)),
            },
            TypeExpr::RawPointer { mutable, inner } => Type::RawPointer {
                mutable: *mutable,
                inner: Box::new(self.fresh_type_from_annotation(inner)),
            },
            TypeExpr::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|elem| self.fresh_type_from_annotation(elem))
                    .collect(),
            ),
            TypeExpr::Function { params, ret, .. } => Type::Function {
                params: params
                    .iter()
                    .map(|param| self.fresh_type_from_annotation(param))
                    .collect(),
                ret: Box::new(self.fresh_type_from_annotation(ret)),
                effects: EffectRow::empty(),
            },
            TypeExpr::Array { element, .. } => self.fresh_type_from_annotation(element),
            TypeExpr::Slice(element) => self.fresh_type_from_annotation(element),
            TypeExpr::TraitObject(path) => {
                let path_idents = path.segments.iter().map(|seg| seg.ident.clone()).collect();
                let generics = path
                    .segments
                    .last()
                    .map(|seg| {
                        seg.generics
                            .iter()
                            .map(|ty| self.fresh_type_from_annotation(ty))
                            .collect()
                    })
                    .unwrap_or_default();
                Type::TraitObject {
                    path: path_idents,
                    generics,
                }
            }
            TypeExpr::Unit => Type::Primitive(PrimitiveType::Unit),
            TypeExpr::Never => Type::Primitive(PrimitiveType::Unit),
            TypeExpr::Lifetime(_) => Type::Var(self.ctx.fresh_type_var()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Attribute, AttributeArg, EffectLabel, Expr, Literal, NodeId, PathExpr, PathSegment,
    };
    use crate::span::Span;

    fn simple_module() -> Module {
        Module {
            id: NodeId::new(0),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: vec![Attribute {
                    name: "zone".into(),
                    args: vec![AttributeArg::Expr(zone_ident("realtime"))],
                    span: Span::default(),
                }],
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: crate::ast::FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "main".into(),
                    generics: crate::ast::GenericParams::default(),
                    params: vec![],
                    return_type: Some(crate::ast::TypeExpr::Unit),
                    effect: Some(crate::ast::EffectExpr {
                        labels: vec![EffectLabel {
                            name: "IOError".into(),
                            args: Vec::new(),
                        }],
                        tail: None,
                    }),
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![Statement::Return {
                        value: Some(Expr::literal(Literal::Unit)),
                        span: Span::default(),
                    }],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: NodeId::new(1),
            })],
        }
    }

    #[test]
    fn generate_function_adds_constraint() {
        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&simple_module());
        assert_eq!(analysis.constraints.effect_constraints.len(), 1);
        assert!(analysis.zones.contains_key("main"));
        assert_eq!(analysis.constraints.zone_diagnostics.len(), 1);
        let diag = &analysis.constraints.zone_diagnostics[0];
        assert!(diag.message.contains("NoIO"));
        assert!(diag
            .labels
            .iter()
            .any(|label| label.span == Span::default()));
        let fn_info = analysis.functions.get("main").expect("function info");
        assert_eq!(fn_info.param_types.len(), 0);
        assert_eq!(fn_info.return_type, Type::Primitive(PrimitiveType::Unit));
        let solved = analysis.clone().solve().expect("solve analysis");
        let solved_fn = solved.functions.get("main").expect("solved function");
        assert_eq!(solved_fn.return_type, Type::Primitive(PrimitiveType::Unit));
        assert!(solved.effect_subst.is_empty());
    }

    #[test]
    fn async_expression_adds_async_effect() {
        let module = Module {
            id: NodeId::new(10),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: Vec::new(),
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: crate::ast::FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "async_main".into(),
                    generics: crate::ast::GenericParams::default(),
                    params: vec![],
                    return_type: Some(crate::ast::TypeExpr::Unit),
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![Statement::Expr(
                        Expr::dummy(ExprKind::Async(Block {
                            statements: Vec::new(),
                            tail: Some(Box::new(Expr::literal(Literal::Unit))),
                            span: Span::default(),
                        })),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: NodeId::new(11),
            })],
        };

        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let info = analysis.functions.get("async_main").expect("function info");
        let effects: Vec<_> = info.effects.fixed().into_iter().cloned().collect();
        assert!(effects.iter().any(|label| label.as_str() == "Async"));
    }

    #[test]
    fn handler_expression_removes_handled_effect() {
        let handler = EffectHandler {
            label: "Async".into(),
            body: Block {
                statements: Vec::new(),
                tail: Some(Box::new(Expr::literal(Literal::Unit))),
                span: Span::default(),
            },
            span: Span::default(),
        };

        let module = Module {
            id: NodeId::new(12),
            span: Span::default(),
            docs: Vec::new(),
            items: vec![Item::Function(FunctionDecl {
                attributes: Vec::new(),
                docs: Vec::new(),
                visibility: crate::ast::Visibility::Private,
                signature: crate::ast::FunctionSig {
                    constness: false,
                    asyncness: false,
                    name: "handle_async".into(),
                    generics: crate::ast::GenericParams::default(),
                    params: vec![],
                    return_type: Some(crate::ast::TypeExpr::Unit),
                    effect: None,
                    where_clause: Vec::new(),
                },
                body: Block {
                    statements: vec![Statement::Expr(
                        Expr::dummy(ExprKind::Handle {
                            body: Box::new(Expr::dummy(ExprKind::Async(Block {
                                statements: Vec::new(),
                                tail: Some(Box::new(Expr::literal(Literal::Unit))),
                                span: Span::default(),
                            }))),
                            handlers: vec![handler],
                        }),
                        Span::default(),
                    )],
                    tail: None,
                    span: Span::default(),
                },
                span: Span::default(),
                id: NodeId::new(13),
            })],
        };

        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let generator = Generator::new(&mut env, &mut ctx);
        let analysis = generator.generate_module(&module);
        let info = analysis
            .functions
            .get("handle_async")
            .expect("function info");
        assert!(info.effects.fixed().is_empty());
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
}
