import AuroraCalc.Syntax

namespace AuroraCalc

set_option autoImplicit false

/-- Small-step reduction relation. -/
inductive Step : Term → Term → Prop where
  | beta {x τ ε body arg} :
      Step (Term.app (Term.lam x τ ε body) arg)
           (Term.subst x arg body)
  | appFun {f f' a} :
      Step f f' → Step (Term.app f a) (Term.app f' a)
  | appArg {f a a'} :
      Value f → Step a a' → Step (Term.app f a) (Term.app f a')
  | letValue {x v body} :
      Value v → Step (Term.let_ x v body)
        (Term.subst x v body)
  | letStep {x t t' body} :
      Step t t' → Step (Term.let_ x t body) (Term.let_ x t' body)

/-- Multi-step closure of the reduction relation. -/
inductive StepStar : Term → Term → Prop where
  | refl {t} : StepStar t t
  | trans {t u v} : Step t u → StepStar u v → StepStar t v

end AuroraCalc
