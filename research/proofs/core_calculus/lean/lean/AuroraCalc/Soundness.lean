import AuroraCalc.Syntax
import AuroraCalc.Typing
import AuroraCalc.Reduction

namespace AuroraCalc

set_option autoImplicit false

open Context

lemma lookup_empty_none (x : String) :
    Context.lookup Context.empty x = none := by rfl

lemma canonical_form_arrow {t τ σ ε ε'}
    (hv : Value t)
    (ht : HasType Context.empty t (Ty.arrow τ ε σ) ε') :
    ∃ x body, t = Term.lam x τ ε body := by
  cases hv with
  | vUnit => cases ht
  | vLam =>
      cases ht with
      | lam _ => exact ⟨_, _, rfl⟩

/-- Progress for the core fragment (unit, lambda, application, let). -/
theorem progress_core {t τ ε}
    (pure : PureTerm t)
    (ht : HasType Context.empty t τ ε) :
    Value t ∨ ∃ t', Step t t' := by
  revert pure
  induction ht with
  | var hlookup =>
      intro pure
      have hx : Context.lookup Context.empty _ = none := lookup_empty_none _
      simpa [hx] using hlookup
  | unit =>
      intro pure
      cases pure with
      | unit => exact .inl Value.vUnit
      | _ => cases pure
  | lam _ ih =>
      intro pure
      cases pure with
      | lam _ => exact .inl Value.vLam
      | _ => cases pure
  | app hf ha ihf iha =>
      intro pure
      cases pure with
      | app pf pa =>
          have hf_progress := ihf pf
          have ha_progress := iha pa
          cases hf_progress with
          | inl vf =>
              have ⟨x, body, hx⟩ := canonical_form_arrow vf hf
              subst hx
              cases ha_progress with
              | inl va =>
                  exact .inr ⟨Term.subst x a body, Step.beta⟩
              | inr ha_step =>
                  obtain ⟨a', ha'⟩ := ha_step
                  exact .inr ⟨Term.app (Term.lam x τ ε body) a', Step.appArg Value.vLam ha'⟩
          | inr hf_step =>
              obtain ⟨f', hf'⟩ := hf_step
              exact .inr ⟨Term.app f' a, Step.appFun hf'⟩
      | _ => cases pure
  | let_ hv hb ihv ihb =>
      intro pure
      cases pure with
      | let_ pv pb =>
          have hv_progress := ihv pv
          cases hv_progress with
          | inl vv =>
              exact .inr ⟨Term.subst x value body, Step.letValue vv⟩
          | inr hv_step =>
              obtain ⟨value', hv'⟩ := hv_step
              exact .inr ⟨Term.let_ x value' body, Step.letStep hv'⟩
      | _ => cases pure
  | regionIntro =>
      intro pure
      cases pure
  | regionElim =>
      intro pure
      cases pure

/-- Placeholder for the substitution lemma over the core fragment. -/
lemma substitution_core
    {Γ x τ σ ε body value}
    (hvalue : HasType Γ value τ ε)
    (hbody  : HasType (Context.extend Γ x τ) body σ EffectRow.empty) :
    HasType Γ (Term.subst x value body) σ (EffectRow.union ε EffectRow.empty) := by
  -- Proof to be completed in future work.
  sorry

/-- Preservation for the core fragment (pending full substitution proof). -/
theorem preservation_core {t t' τ ε}
    (pure : PureTerm t)
    (ht : HasType Context.empty t τ ε)
    (step : Step t t') :
    HasType Context.empty t' τ ε := by
  -- Proof will rely on `substitution_core`; left as future work.
  sorry

end AuroraCalc
