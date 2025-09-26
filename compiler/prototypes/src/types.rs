use std::collections::{HashMap, HashSet};

use aurora_effect_solver::{EffectRow, RowVar};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectVarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LifetimeVarId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Bool,
    Int32,
    Int64,
    Float32,
    Float64,
    String,
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Var(TypeVarId),
    Primitive(PrimitiveType),
    Tuple(Vec<Type>),
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
        effects: EffectRow,
    },
    Reference {
        lifetime: LifetimeVarId,
        mutable: bool,
        inner: Box<Type>,
    },
    RawPointer {
        mutable: bool,
        inner: Box<Type>,
    },
    TraitObject {
        path: Vec<String>,
        generics: Vec<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scheme {
    pub type_vars: Vec<TypeVarId>,
    pub effect_vars: Vec<EffectVarId>,
    pub lifetime_vars: Vec<LifetimeVarId>,
    pub ty: Type,
    pub effects: EffectRow,
}

#[derive(Debug, Default)]
pub struct TypeEnv {
    scopes: Vec<HashMap<String, Scheme>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, scheme: Scheme) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, scheme);
        } else {
            let mut scope = HashMap::new();
            scope.insert(name, scheme);
            self.scopes.push(scope);
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Scheme> {
        for scope in self.scopes.iter().rev() {
            if let Some(scheme) = scope.get(name) {
                return Some(scheme);
            }
        }
        None
    }

    pub fn depth(&self) -> usize {
        self.scopes.len()
    }
}

#[derive(Debug, Default)]
pub struct InferenceContext {
    next_type: usize,
    next_effect: usize,
    next_lifetime: usize,
}

impl InferenceContext {
    pub fn fresh_type_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.next_type);
        self.next_type += 1;
        id
    }

    pub fn fresh_effect_var(&mut self) -> EffectVarId {
        let id = EffectVarId(self.next_effect);
        self.next_effect += 1;
        id
    }

    pub fn fresh_lifetime_var(&mut self) -> LifetimeVarId {
        let id = LifetimeVarId(self.next_lifetime);
        self.next_lifetime += 1;
        id
    }
}

impl Scheme {
    pub fn monomorphic(ty: Type) -> Self {
        Scheme {
            type_vars: Vec::new(),
            effect_vars: Vec::new(),
            lifetime_vars: Vec::new(),
            ty,
            effects: EffectRow::empty(),
        }
    }

    pub fn generalize(env: &TypeEnv, ty: &Type, effects: &EffectRow) -> Self {
        let mut free_type_vars = free_type_vars_type(ty);
        let env_type_vars = env.free_type_vars();
        free_type_vars.retain(|var| !env_type_vars.contains(var));
        let mut type_vars: Vec<_> = free_type_vars.into_iter().collect();
        type_vars.sort_by_key(|var| var.0);

        let mut free_effect_vars = free_effect_vars_type(ty);
        if let Some(tail) = effects.tail() {
            if let Some(id) = effect_var_from_row_var(tail) {
                free_effect_vars.insert(id);
            }
        }
        let env_effect_vars = env.free_effect_vars();
        free_effect_vars.retain(|var| !env_effect_vars.contains(var));
        let mut effect_vars: Vec<_> = free_effect_vars.into_iter().collect();
        effect_vars.sort_by_key(|var| var.0);

        Scheme {
            type_vars,
            effect_vars,
            lifetime_vars: Vec::new(),
            ty: ty.clone(),
            effects: effects.clone(),
        }
    }

    pub fn instantiate(&self, ctx: &mut InferenceContext) -> (Type, EffectRow) {
        let mut type_mapping = HashMap::new();
        for var in &self.type_vars {
            type_mapping.insert(*var, Type::Var(ctx.fresh_type_var()));
        }

        let mut effect_mapping = HashMap::new();
        for var in &self.effect_vars {
            effect_mapping.insert(*var, RowVar::new(effect_var_name(ctx.fresh_effect_var())));
        }

        let instantiated = substitute_type(&self.ty, &type_mapping, &effect_mapping);
        let effects = substitute_effect_row(&self.effects, &effect_mapping);
        (instantiated, effects)
    }

    fn free_type_vars(&self) -> HashSet<TypeVarId> {
        let mut vars = free_type_vars_type(&self.ty);
        for bound in &self.type_vars {
            vars.remove(bound);
        }
        vars
    }

    fn free_effect_vars(&self) -> HashSet<EffectVarId> {
        let mut vars = free_effect_vars_type(&self.ty);
        if let Some(tail) = self.effects.tail() {
            if let Some(id) = effect_var_from_row_var(tail) {
                vars.insert(id);
            }
        }
        for bound in &self.effect_vars {
            vars.remove(bound);
        }
        vars
    }
}

fn substitute_type(
    ty: &Type,
    mapping: &HashMap<TypeVarId, Type>,
    effect_mapping: &HashMap<EffectVarId, RowVar>,
) -> Type {
    match ty {
        Type::Var(id) => mapping.get(id).cloned().unwrap_or(Type::Var(*id)),
        Type::Primitive(p) => Type::Primitive(*p),
        Type::Tuple(elems) => Type::Tuple(
            elems
                .iter()
                .map(|t| substitute_type(t, mapping, effect_mapping))
                .collect(),
        ),
        Type::Function {
            params,
            ret,
            effects,
        } => Type::Function {
            params: params
                .iter()
                .map(|t| substitute_type(t, mapping, effect_mapping))
                .collect(),
            ret: Box::new(substitute_type(ret, mapping, effect_mapping)),
            effects: substitute_effect_row(effects, effect_mapping),
        },
        Type::Reference {
            lifetime,
            mutable,
            inner,
        } => Type::Reference {
            lifetime: *lifetime,
            mutable: *mutable,
            inner: Box::new(substitute_type(inner, mapping, effect_mapping)),
        },
        Type::RawPointer { mutable, inner } => Type::RawPointer {
            mutable: *mutable,
            inner: Box::new(substitute_type(inner, mapping, effect_mapping)),
        },
        Type::TraitObject { path, generics } => Type::TraitObject {
            path: path.clone(),
            generics: generics
                .iter()
                .map(|t| substitute_type(t, mapping, effect_mapping))
                .collect(),
        },
    }
}

fn substitute_effect_row(
    row: &EffectRow,
    effect_mapping: &HashMap<EffectVarId, RowVar>,
) -> EffectRow {
    if let Some(tail) = row.tail() {
        if let Some(id) = effect_var_from_row_var(tail) {
            if let Some(new_tail) = effect_mapping.get(&id) {
                return rebuild_effect_row(row, Some(new_tail.clone()));
            }
        }
    }
    row.clone()
}

fn free_type_vars_type(ty: &Type) -> HashSet<TypeVarId> {
    let mut vars = HashSet::new();
    collect_free_type_vars(ty, &mut vars);
    vars
}

fn collect_free_type_vars(ty: &Type, vars: &mut HashSet<TypeVarId>) {
    match ty {
        Type::Var(id) => {
            vars.insert(*id);
        }
        Type::Primitive(_) => {}
        Type::Tuple(elems) => {
            for elem in elems {
                collect_free_type_vars(elem, vars);
            }
        }
        Type::Function { params, ret, .. } => {
            for param in params {
                collect_free_type_vars(param, vars);
            }
            collect_free_type_vars(ret, vars);
        }
        Type::Reference { inner, .. } => collect_free_type_vars(inner, vars),
        Type::RawPointer { inner, .. } => collect_free_type_vars(inner, vars),
        Type::TraitObject { generics, .. } => {
            for ty in generics {
                collect_free_type_vars(ty, vars);
            }
        }
    }
}

fn free_effect_vars_type(ty: &Type) -> HashSet<EffectVarId> {
    let mut vars = HashSet::new();
    collect_free_effect_vars(ty, &mut vars);
    vars
}

fn collect_free_effect_vars(ty: &Type, vars: &mut HashSet<EffectVarId>) {
    match ty {
        Type::Var(_) | Type::Primitive(_) => {}
        Type::Tuple(elems) => {
            for elem in elems {
                collect_free_effect_vars(elem, vars);
            }
        }
        Type::Function {
            params,
            ret,
            effects,
        } => {
            for param in params {
                collect_free_effect_vars(param, vars);
            }
            collect_free_effect_vars(ret, vars);
            if let Some(tail) = effects.tail() {
                if let Some(id) = effect_var_from_row_var(tail) {
                    vars.insert(id);
                }
            }
        }
        Type::Reference { inner, .. } => collect_free_effect_vars(inner, vars),
        Type::RawPointer { inner, .. } => collect_free_effect_vars(inner, vars),
        Type::TraitObject { generics, .. } => {
            for ty in generics {
                collect_free_effect_vars(ty, vars);
            }
        }
    }
}

fn rebuild_effect_row(row: &EffectRow, new_tail: Option<RowVar>) -> EffectRow {
    let effects: Vec<String> = row
        .fixed()
        .iter()
        .map(|effect| effect.as_str().to_string())
        .collect();
    match new_tail {
        Some(tail) => EffectRow::with_tail(effects, tail.as_str().to_string()),
        None => EffectRow::from_effects(effects),
    }
}

fn effect_var_name(id: EffectVarId) -> String {
    format!("e{}", id.0)
}

fn effect_var_from_row_var(row: &RowVar) -> Option<EffectVarId> {
    row.as_str()
        .strip_prefix('e')
        .and_then(|rest| rest.parse::<usize>().ok())
        .map(EffectVarId)
}

impl TypeEnv {
    pub fn free_type_vars(&self) -> HashSet<TypeVarId> {
        let mut vars = HashSet::new();
        for scope in &self.scopes {
            for scheme in scope.values() {
                vars.extend(scheme.free_type_vars());
            }
        }
        vars
    }

    pub fn free_effect_vars(&self) -> HashSet<EffectVarId> {
        let mut vars = HashSet::new();
        for scope in &self.scopes {
            for scheme in scope.values() {
                vars.extend(scheme.free_effect_vars());
            }
        }
        vars
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_scheme() -> Scheme {
        Scheme {
            type_vars: Vec::new(),
            effect_vars: Vec::new(),
            lifetime_vars: Vec::new(),
            ty: Type::Primitive(PrimitiveType::Unit),
            effects: EffectRow::empty(),
        }
    }

    #[test]
    fn type_env_scoping() {
        let mut env = TypeEnv::new();
        env.push_scope();
        env.insert("x".into(), empty_scheme());
        assert!(env.lookup("x").is_some());
        env.push_scope();
        env.insert("y".into(), empty_scheme());
        assert!(env.lookup("y").is_some());
        env.pop_scope();
        assert!(env.lookup("y").is_none());
        assert_eq!(env.depth(), 1);
    }

    #[test]
    fn fresh_variable_generators_increment() {
        let mut ctx = InferenceContext::default();
        assert_eq!(ctx.fresh_type_var(), TypeVarId(0));
        assert_eq!(ctx.fresh_type_var(), TypeVarId(1));
        assert_eq!(ctx.fresh_effect_var(), EffectVarId(0));
        assert_eq!(ctx.fresh_lifetime_var(), LifetimeVarId(0));
    }

    #[test]
    fn function_type_holds_effect_row() {
        let effects = EffectRow::from_effects(["IOError"]);
        let ty = Type::Function {
            params: vec![Type::Primitive(PrimitiveType::Int32)],
            ret: Box::new(Type::Primitive(PrimitiveType::Unit)),
            effects: effects.clone(),
        };
        if let Type::Function { effects: eff, .. } = ty {
            assert_eq!(eff.fixed(), effects.fixed());
        } else {
            panic!("expected function type");
        }
    }

    #[test]
    fn generalize_and_instantiate_produces_fresh_vars() {
        let mut env = TypeEnv::new();
        env.push_scope();
        let mut ctx = InferenceContext::default();
        let alpha = Type::Var(ctx.fresh_type_var());
        let scheme = Scheme::generalize(&env, &alpha, &EffectRow::empty());
        assert_eq!(scheme.type_vars, vec![TypeVarId(0)]);

        let (inst_ty, _) = scheme.instantiate(&mut ctx);
        match inst_ty {
            Type::Var(id) => assert_ne!(id, TypeVarId(0)),
            other => panic!("expected type variable, got {other:?}"),
        }

        env.insert("id".into(), scheme);
        if let Some(stored) = env.lookup("id") {
            let (inst2, _) = stored.instantiate(&mut ctx);
            match inst2 {
                Type::Var(id) => assert!(id.0 >= 2),
                other => panic!("expected type variable, got {other:?}"),
            }
        } else {
            panic!("scheme not inserted into environment");
        }
    }
}
