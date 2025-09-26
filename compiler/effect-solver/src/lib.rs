//! Prototype algebraic effect row solver.
//! The implementation purposefully targets a restricted subset to validate
//! data-structure choices and API ergonomics before integrating with the
//! main type inference engine.

use indexmap::{IndexMap, IndexSet};
use std::fmt;
use thiserror::Error;

/// Canonical representation of a single effect identifier.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Effect(String);

impl Effect {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Effect(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Effect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Row variable used for polymorphic effect tails.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RowVar(String);

impl RowVar {
    pub fn new<S: Into<String>>(name: S) -> Self {
        RowVar(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RowVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Effect rows are composed of a set of concrete effects and an optional tail variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EffectRow {
    fixed: IndexSet<Effect>,
    tail: Option<RowVar>,
}

impl EffectRow {
    pub fn empty() -> Self {
        EffectRow {
            fixed: IndexSet::new(),
            tail: None,
        }
    }

    pub fn from_effects<I, S>(effects: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut fixed = IndexSet::new();
        for effect in effects {
            fixed.insert(Effect::new(effect));
        }
        EffectRow { fixed, tail: None }
    }

    pub fn with_tail<I, S, T>(effects: I, tail: T) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
        T: Into<String>,
    {
        let mut row = Self::from_effects(effects);
        row.tail = Some(RowVar::new(tail));
        row
    }

    pub fn fixed(&self) -> &IndexSet<Effect> {
        &self.fixed
    }

    pub fn tail(&self) -> Option<&RowVar> {
        self.tail.as_ref()
    }

    pub fn union(&self, other: &EffectRow) -> EffectRow {
        let mut fixed = self.fixed.clone();
        fixed.extend(other.fixed.iter().cloned());
        let tail = match (&self.tail, &other.tail) {
            (Some(a), Some(b)) if a == b => Some(a.clone()),
            (Some(a), _) => Some(a.clone()),
            (_, Some(b)) => Some(b.clone()),
            (None, None) => None,
        };
        EffectRow { fixed, tail }
    }

    pub fn is_empty(&self) -> bool {
        self.fixed.is_empty() && self.tail.is_none()
    }

    pub fn apply_substitution(&self, subst: &Substitution) -> EffectRow {
        let mut fixed = self.fixed.clone();
        let mut tail = self.tail.clone();
        let mut seen = IndexSet::new();

        while let Some(var) = tail.clone() {
            if !seen.insert(var.clone()) {
                break;
            }

            if let Some(row) = subst.get(&var) {
                fixed.extend(row.fixed.iter().cloned());
                tail = row.tail.clone();
            } else {
                break;
            }
        }

        EffectRow { fixed, tail }
    }
}

/// Result of unifying two effect rows.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnifyOutcome {
    pub row: EffectRow,
    pub subst: Substitution,
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum UnifyError {
    #[error("row variable mismatch: {0} vs {1}")]
    IncompatibleTails(RowVar, RowVar),
    #[error("rigid rows cannot absorb effects: {0:?}")]
    RigidRowMismatch(Vec<String>),
    #[error("occurs check failed for row variable {0}")]
    OccursCheck(RowVar),
    #[error("conflicting substitution for row variable {0}")]
    ConflictingSubstitution(RowVar),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Substitution {
    map: IndexMap<RowVar, EffectRow>,
}

impl Substitution {
    pub fn iter(&self) -> impl Iterator<Item = (&RowVar, &EffectRow)> {
        self.map.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn get(&self, var: &RowVar) -> Option<&EffectRow> {
        self.map.get(var)
    }

    fn insert(&mut self, var: RowVar, row: EffectRow) -> Result<(), UnifyError> {
        if row.is_empty() {
            return Ok(());
        }
        if row.tail.as_ref() == Some(&var) {
            return Err(UnifyError::OccursCheck(var));
        }
        match self.map.get(&var) {
            Some(existing) if existing == &row => Ok(()),
            Some(_) => Err(UnifyError::ConflictingSubstitution(var)),
            None => {
                self.map.insert(var, row);
                Ok(())
            }
        }
    }

    pub fn merge(&mut self, mut other: Substitution) -> Result<(), UnifyError> {
        for (var, row) in other.map.drain(..) {
            self.insert(var, row)?;
        }
        Ok(())
    }
}

/// Compute the most general effect row that satisfies both operands.
///
/// Currently supports at most one tail variable between the inputs; future
/// iterations will generalise this into a full constraint solver with
/// substitution maps.
pub fn unify(lhs: &EffectRow, rhs: &EffectRow) -> Result<UnifyOutcome, UnifyError> {
    let mut fixed = lhs.fixed.clone();
    fixed.extend(rhs.fixed.iter().cloned());

    let extra_lhs = difference(rhs.fixed(), lhs.fixed());
    let extra_rhs = difference(lhs.fixed(), rhs.fixed());

    let mut subst = Substitution::default();

    if let Some(var) = lhs.tail() {
        let row = EffectRow {
            fixed: extra_lhs.clone(),
            tail: None,
        };
        subst.insert(var.clone(), row)?;
    }

    if let Some(var) = rhs.tail() {
        let tail_target = lhs.tail().filter(|other| *other != var).cloned();
        let row = EffectRow {
            fixed: extra_rhs.clone(),
            tail: tail_target,
        };
        subst.insert(var.clone(), row)?;
    }

    if let (Some(a), Some(b)) = (lhs.tail(), rhs.tail()) {
        if a != b && subst.is_empty() && extra_lhs.is_empty() && extra_rhs.is_empty() {
            subst.insert(
                b.clone(),
                EffectRow {
                    fixed: IndexSet::new(),
                    tail: Some(a.clone()),
                },
            )?;
        }
    }

    let tail = match (lhs.tail(), rhs.tail()) {
        (Some(a), Some(b)) if a == b => Some(a.clone()),
        (Some(a), _) => Some(a.clone()),
        (_, Some(b)) => Some(b.clone()),
        (None, None) => None,
    };

    Ok(UnifyOutcome {
        row: EffectRow { fixed, tail },
        subst,
    })
}

fn difference(from: &IndexSet<Effect>, other: &IndexSet<Effect>) -> IndexSet<Effect> {
    let mut diff = IndexSet::new();
    for eff in from.iter() {
        if !other.contains(eff) {
            diff.insert(eff.clone());
        }
    }
    diff
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unify_pure_rows() {
        let lhs = EffectRow::from_effects(["IOError", "Timeout"]);
        let rhs = EffectRow::from_effects(["Timeout", "ParseError"]);
        let result = unify(&lhs, &rhs).unwrap();
        let names: IndexSet<&str> = result.row.fixed().iter().map(|e| e.as_str()).collect();
        let expected: IndexSet<&str> = IndexSet::from_iter(["IOError", "Timeout", "ParseError"]);
        assert_eq!(names, expected);
        assert!(result.row.tail().is_none());
        assert!(result.subst.is_empty());
    }

    #[test]
    fn unify_with_tail_variable() {
        let lhs = EffectRow::with_tail(["IOError"], "ε");
        let rhs = EffectRow::from_effects(["Timeout"]);
        let result = unify(&lhs, &rhs).unwrap();
        let names: IndexSet<&str> = result.row.fixed().iter().map(|e| e.as_str()).collect();
        let expected: IndexSet<&str> = IndexSet::from_iter(["IOError", "Timeout"]);
        assert_eq!(names, expected);
        assert_eq!(result.row.tail().unwrap().as_str(), "ε");
        let subst_row = result
            .subst
            .get(&RowVar::new("ε"))
            .expect("substitution for ε");
        let subst_names: IndexSet<&str> = subst_row.fixed().iter().map(|e| e.as_str()).collect();
        let subst_expected: IndexSet<&str> = IndexSet::from_iter(["Timeout"]);
        assert_eq!(subst_names, subst_expected);
        assert!(subst_row.tail().is_none());
    }

    #[test]
    fn tail_aliasing_produces_substitution() {
        let lhs = EffectRow::with_tail(["IOError"], "ε1");
        let rhs = EffectRow::with_tail(["IOError"], "ε2");
        let result = unify(&lhs, &rhs).unwrap();
        assert_eq!(result.row.tail().unwrap().as_str(), "ε1");
        let alias = result.subst.get(&RowVar::new("ε2")).expect("alias for ε2");
        assert!(alias.fixed().is_empty());
        assert_eq!(alias.tail().unwrap().as_str(), "ε1");
    }

    #[test]
    fn duplicate_effects_eliminated() {
        let lhs = EffectRow::from_effects(["IOError", "IOError"]);
        let rhs = EffectRow::empty();
        let result = unify(&lhs, &rhs).unwrap();
        assert_eq!(result.row.fixed().len(), 1);
        assert!(result.subst.is_empty());
    }

    use proptest::prelude::*;

    proptest! {
        #[test]
        fn union_contains_all_unique_effects(a in prop::collection::vec("[A-Z][a-z]{0,3}", 0..5),
                                            b in prop::collection::vec("[A-Z][a-z]{0,3}", 0..5)) {
            let lhs = EffectRow::from_effects(a.clone());
            let rhs = EffectRow::from_effects(b.clone());
            let result = unify(&lhs, &rhs).unwrap();
            for effect in a.iter().chain(b.iter()) {
                assert!(result.row.fixed().iter().any(|e| e.as_str() == effect));
            }
        }
    }
}
