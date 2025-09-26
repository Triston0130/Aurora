import AuroraCalc.Syntax
import AuroraCalc.Typing
import AuroraCalc.Reduction

namespace AuroraCalc

set_option autoImplicit false

open Context

@[simp] def isValue (t : Term) : Bool :=
  match t with
  | Term.unit => true
  | Term.lam .. => true
  | _ => false

lemma value_implies_prop {t : Term} (h : isValue t = true) : Value t := by
  cases t <;> try cases h
  · exact Value.vUnit
  · exact Value.vLam

/-- Statement of progress: a well-typed closed term is a value or can take a step. -/
theorem progress {t τ ε} (h : HasType empty t τ ε) :
    Value t ∨ ∃ t', Step t t' := by
  -- TODO: full proof forthcoming
  sorry

/-- Statement of preservation: types are stable under reduction. -/
theorem preservation {Γ t t' τ ε}
    (ht : HasType Γ t τ ε) (step : Step t t') : HasType Γ t' τ ε := by
  -- TODO: full proof forthcoming
  sorry

end AuroraCalc
