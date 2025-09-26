use std::collections::HashMap;

use aurora_effect_solver::{self, EffectRow, Substitution as EffectSubstitution};
use thiserror::Error;

use crate::constraints::{ConstraintSet, EffectConstraint, TypeConstraint};
use crate::diagnostics::{primary_label, Diagnostic};
use crate::span::Span;
use crate::types::{LifetimeVarId, Type, TypeVarId};

#[derive(Debug, Default, Clone)]
pub struct TypeSubstitution {
    map: HashMap<TypeVarId, Type>,
}

impl TypeSubstitution {
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => self
                .map
                .get(id)
                .map(|replacement| self.apply(replacement))
                .unwrap_or(Type::Var(*id)),
            Type::Primitive(p) => Type::Primitive(*p),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|t| self.apply(t)).collect()),
            Type::Function {
                params,
                ret,
                effects,
            } => Type::Function {
                params: params.iter().map(|t| self.apply(t)).collect(),
                ret: Box::new(self.apply(ret)),
                effects: effects.clone(),
            },
            Type::Reference {
                lifetime,
                mutable,
                inner,
            } => Type::Reference {
                lifetime: *lifetime,
                mutable: *mutable,
                inner: Box::new(self.apply(inner)),
            },
            Type::RawPointer { mutable, inner } => Type::RawPointer {
                mutable: *mutable,
                inner: Box::new(self.apply(inner)),
            },
            Type::TraitObject { path, generics } => Type::TraitObject {
                path: path.clone(),
                generics: generics.iter().map(|ty| self.apply(ty)).collect(),
            },
        }
    }

    pub fn insert(&mut self, var: TypeVarId, ty: Type) {
        self.map.insert(var, ty);
    }

    pub fn get(&self, id: &TypeVarId) -> Option<&Type> {
        self.map.get(id)
    }
}

#[derive(Debug, Default, Clone)]
pub struct SolverResult {
    pub type_subst: TypeSubstitution,
    pub effect_subst: EffectSubstitution,
}

#[derive(Debug, Error, PartialEq)]
pub enum SolveError {
    #[error("type mismatch: {lhs:?} vs {rhs:?}")]
    TypeMismatch { lhs: Box<Type>, rhs: Box<Type> },
    #[error("tuple arity mismatch: expected {expected}, found {found}")]
    TupleArityMismatch { expected: usize, found: usize },
    #[error("function arity mismatch: expected {expected}, found {found}")]
    FunctionArityMismatch { expected: usize, found: usize },
    #[error("lifetime mismatch: {lhs:?} vs {rhs:?}")]
    LifetimeMismatch {
        lhs: LifetimeVarId,
        rhs: LifetimeVarId,
    },
    #[error("occurs check failed for type variable {0:?}")]
    OccursCheck(TypeVarId),
    #[error("effect unification failed: {0}")]
    Effect(#[from] aurora_effect_solver::UnifyError),
}

impl SolverResult {
    pub fn solve(constraints: &ConstraintSet) -> Result<Self, SolveError> {
        let mut solver = ConstraintSolver {
            type_subst: TypeSubstitution::default(),
            effect_subst: EffectSubstitution::default(),
        };
        solver.solve(constraints)?;
        Ok(SolverResult {
            type_subst: solver.type_subst,
            effect_subst: solver.effect_subst,
        })
    }
}

impl SolveError {
    pub fn into_diagnostic(self) -> Diagnostic {
        Diagnostic::error(self.to_string())
            .with_label(primary_label(Span::default(), "type inference failed"))
    }
}

struct ConstraintSolver {
    type_subst: TypeSubstitution,
    effect_subst: EffectSubstitution,
}

impl ConstraintSolver {
    fn solve(&mut self, constraints: &ConstraintSet) -> Result<(), SolveError> {
        for constraint in &constraints.type_constraints {
            match constraint {
                TypeConstraint::Equal(lhs, rhs) => {
                    self.unify_types(lhs.clone(), rhs.clone())?;
                }
            }
        }

        for constraint in &constraints.effect_constraints {
            match constraint {
                EffectConstraint::Equal(lhs, rhs) => {
                    let lhs = lhs.apply_substitution(&self.effect_subst);
                    let rhs = rhs.apply_substitution(&self.effect_subst);
                    let outcome = aurora_effect_solver::unify(&lhs, &rhs)?;
                    self.effect_subst.merge(outcome.subst)?;
                }
            }
        }
        Ok(())
    }

    fn unify_types(&mut self, lhs: Type, rhs: Type) -> Result<(), SolveError> {
        let lhs = self.type_subst.apply(&lhs);
        let rhs = self.type_subst.apply(&rhs);
        match (lhs, rhs) {
            (Type::Var(id), ty) => self.bind_var(id, ty),
            (ty, Type::Var(id)) => self.bind_var(id, ty),
            (Type::Primitive(a), Type::Primitive(b)) if a == b => Ok(()),
            (Type::Tuple(a), Type::Tuple(b)) => {
                if a.len() != b.len() {
                    return Err(SolveError::TupleArityMismatch {
                        expected: a.len(),
                        found: b.len(),
                    });
                }
                for (lhs, rhs) in a.into_iter().zip(b.into_iter()) {
                    self.unify_types(lhs, rhs)?;
                }
                Ok(())
            }
            (
                Type::Function {
                    params: p1,
                    ret: r1,
                    effects: e1,
                },
                Type::Function {
                    params: p2,
                    ret: r2,
                    effects: e2,
                },
            ) => {
                if p1.len() != p2.len() {
                    return Err(SolveError::FunctionArityMismatch {
                        expected: p1.len(),
                        found: p2.len(),
                    });
                }
                for (lhs, rhs) in p1.into_iter().zip(p2.into_iter()) {
                    self.unify_types(lhs, rhs)?;
                }
                self.unify_effect_rows(e1, e2)?;
                self.unify_types(*r1, *r2)
            }
            (
                Type::Reference {
                    lifetime: l1,
                    mutable: m1,
                    inner: i1,
                },
                Type::Reference {
                    lifetime: l2,
                    mutable: m2,
                    inner: i2,
                },
            ) => {
                if m1 != m2 {
                    return Err(SolveError::TypeMismatch {
                        lhs: Box::new(Type::Reference {
                            lifetime: l1,
                            mutable: m1,
                            inner: i1,
                        }),
                        rhs: Box::new(Type::Reference {
                            lifetime: l2,
                            mutable: m2,
                            inner: i2,
                        }),
                    });
                }
                if l1 != l2 {
                    return Err(SolveError::LifetimeMismatch { lhs: l1, rhs: l2 });
                }
                self.unify_types(*i1, *i2)
            }
            (
                Type::RawPointer {
                    mutable: m1,
                    inner: i1,
                },
                Type::RawPointer {
                    mutable: m2,
                    inner: i2,
                },
            ) => {
                if m1 != m2 {
                    return Err(SolveError::TypeMismatch {
                        lhs: Box::new(Type::RawPointer {
                            mutable: m1,
                            inner: i1,
                        }),
                        rhs: Box::new(Type::RawPointer {
                            mutable: m2,
                            inner: i2,
                        }),
                    });
                }
                self.unify_types(*i1, *i2)
            }
            (
                Type::TraitObject {
                    path: p1,
                    generics: g1,
                },
                Type::TraitObject {
                    path: p2,
                    generics: g2,
                },
            ) => {
                if p1 != p2 || g1.len() != g2.len() {
                    return Err(SolveError::TypeMismatch {
                        lhs: Box::new(Type::TraitObject {
                            path: p1,
                            generics: g1,
                        }),
                        rhs: Box::new(Type::TraitObject {
                            path: p2,
                            generics: g2,
                        }),
                    });
                }
                for (lhs, rhs) in g1.into_iter().zip(g2.into_iter()) {
                    self.unify_types(lhs, rhs)?;
                }
                Ok(())
            }
            (lhs, rhs) => Err(SolveError::TypeMismatch {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
        }
    }

    fn unify_effect_rows(&mut self, lhs: EffectRow, rhs: EffectRow) -> Result<(), SolveError> {
        let lhs = lhs.apply_substitution(&self.effect_subst);
        let rhs = rhs.apply_substitution(&self.effect_subst);
        let outcome = aurora_effect_solver::unify(&lhs, &rhs)?;
        self.effect_subst.merge(outcome.subst)?;
        Ok(())
    }

    fn bind_var(&mut self, var: TypeVarId, ty: Type) -> Result<(), SolveError> {
        if let Type::Var(id) = &ty {
            if *id == var {
                return Ok(());
            }
        }
        if self.occurs(var, &ty) {
            return Err(SolveError::OccursCheck(var));
        }
        let ty = self.type_subst.apply(&ty);
        self.type_subst.insert(var, ty);
        Ok(())
    }

    fn occurs(&self, var: TypeVarId, ty: &Type) -> bool {
        match self.type_subst.apply(ty) {
            Type::Var(id) => id == var,
            Type::Primitive(_) => false,
            Type::Tuple(elems) => elems.iter().any(|elem| self.occurs(var, elem)),
            Type::Function { params, ret, .. } => {
                params.iter().any(|p| self.occurs(var, p)) || self.occurs(var, &ret)
            }
            Type::Reference { inner, .. } => self.occurs(var, &inner),
            Type::RawPointer { inner, .. } => self.occurs(var, &inner),
            Type::TraitObject { generics, .. } => {
                generics.into_iter().any(|g| self.occurs(var, &g))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraints::{ConstraintSet, EffectConstraint, TypeConstraint};
    use crate::types::{PrimitiveType, Type};

    #[test]
    fn solves_simple_primitive_equality() {
        let mut constraints = ConstraintSet::new();
        constraints.push_type(TypeConstraint::Equal(
            Type::Primitive(PrimitiveType::Int32),
            Type::Primitive(PrimitiveType::Int32),
        ));
        let result = SolverResult::solve(&constraints).expect("solver result");
        assert!(result.type_subst.get(&TypeVarId(0)).is_none());
        assert!(result.effect_subst.is_empty());
    }

    #[test]
    fn binds_type_variables() {
        let mut constraints = ConstraintSet::new();
        constraints.push_type(TypeConstraint::Equal(
            Type::Var(TypeVarId(0)),
            Type::Primitive(PrimitiveType::Bool),
        ));
        let result = SolverResult::solve(&constraints).expect("solver result");
        let ty = result
            .type_subst
            .get(&TypeVarId(0))
            .expect("binding for t0");
        assert_eq!(ty, &Type::Primitive(PrimitiveType::Bool));
    }

    #[test]
    fn unifies_effect_rows() {
        let mut constraints = ConstraintSet::new();
        constraints.push_effect(EffectConstraint::Equal(
            EffectRow::from_effects(["IO"]),
            EffectRow::from_effects(["IO"]),
        ));
        let result = SolverResult::solve(&constraints).expect("solver result");
        assert!(result.effect_subst.is_empty());
    }
}
