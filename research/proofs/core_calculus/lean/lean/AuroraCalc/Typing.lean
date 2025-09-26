import AuroraCalc.Syntax

namespace AuroraCalc

set_option autoImplicit false

inductive HasType : Context → Term → Ty → EffectRow → Prop where
  | var {Γ x τ} {ε} :
      Context.lookup Γ x = some τ →
      HasType Γ (Term.var x) τ ε
  | unit {Γ} : HasType Γ Term.unit Ty.unit EffectRow.empty
  | lam {Γ x τ ε body σ}
      (h : HasType (Context.extend Γ x τ) body σ ε) :
      HasType Γ (Term.lam x τ ε body) (Ty.arrow τ ε σ) EffectRow.empty
  | app {Γ f a τ σ εf εa _ε} :
      HasType Γ f (Ty.arrow τ εf σ) _ε →
      HasType Γ a τ εa →
      HasType Γ (Term.app f a) σ (EffectRow.union εf εa)
  | let_ {Γ x value body τ σ εv εb} :
      HasType Γ value τ εv →
      HasType (Context.extend Γ x τ) body σ εb →
      HasType Γ (Term.let_ x value body) σ (EffectRow.union εv εb)
  | regionIntro {Γ r term τ ε} :
      HasType Γ term τ ε →
      HasType Γ (Term.regionIntro r term) (Ty.handle r τ) ε
  | regionElim {Γ r x handle body τ σ εh εb} :
      HasType Γ handle (Ty.handle r τ) εh →
      HasType (Context.extend Γ x τ) body σ εb →
      HasType Γ (Term.regionElim r x handle body) σ (EffectRow.union εh εb)

end AuroraCalc
