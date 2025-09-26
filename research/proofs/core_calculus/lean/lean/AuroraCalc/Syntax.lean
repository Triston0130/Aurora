namespace AuroraCalc

set_option autoImplicit false

/-- Effect labels identify observable side-effects (IO, GPU, etc.). -/
inductive EffectLabel where
  | mk : String → EffectLabel
  deriving DecidableEq, Repr

/-- Effect rows capture the effects produced by a term. -/
structure EffectRow where
  labels : List EffectLabel
  deriving DecidableEq, Repr

namespace EffectRow

@[simp] def empty : EffectRow := ⟨[]⟩

@[simp] def union (σ τ : EffectRow) : EffectRow := ⟨σ.labels ++ τ.labels⟩

end EffectRow

/-- Regions name the capability domain of a resource (cpu, gpu, realtime...). -/
structure Region where
  name : String
  deriving DecidableEq, Repr

/-- Types for the core calculus. -/
inductive Ty where
  | unit : Ty
  | primitive : String → Ty
  | arrow : Ty → EffectRow → Ty → Ty
  | handle : Region → Ty → Ty
  deriving DecidableEq, Repr

/-- Core terms. -/
inductive Term where
  | var : String → Term
  | unit : Term
  | lam : String → Ty → EffectRow → Term → Term
  | app : Term → Term → Term
  | let_ : String → Term → Term → Term
  | regionIntro : Region → Term → Term
  | regionElim : Region → String → Term → Term → Term
  deriving Repr

namespace Term

@[simp] def subst (s : String) (r : Term) : Term → Term
  | var x => if x = s then r else var x
  | unit => unit
  | lam x τ ε body =>
      if x = s then lam x τ ε body else lam x τ ε (subst s r body)
  | app f a => app (subst s r f) (subst s r a)
  | let_ x t body =>
      let body' := if x = s then body else subst s r body
      let_ x (subst s r t) body'
  | regionIntro R t => regionIntro R (subst s r t)
  | regionElim R x handle body =>
      let body' := if x = s then body else subst s r body
      regionElim R x (subst s r handle) body'

end Term

inductive PureTerm : Term → Prop where
  | var {x} : PureTerm (Term.var x)
  | unit : PureTerm Term.unit
  | lam {x τ ε body} (hb : PureTerm body) : PureTerm (Term.lam x τ ε body)
  | app {f a} (hf : PureTerm f) (ha : PureTerm a) : PureTerm (Term.app f a)
  | let_ {x value body} (hv : PureTerm value) (hb : PureTerm body) :
      PureTerm (Term.let_ x value body)

namespace PureTerm

@[simp] lemma of_var {x} : PureTerm (Term.var x) := PureTerm.var

@[simp] lemma of_unit : PureTerm Term.unit := PureTerm.unit

@[simp] lemma lam_body {x τ ε body} :
    PureTerm body → PureTerm (Term.lam x τ ε body) := PureTerm.lam

@[simp] lemma app_left {f a} (hf : PureTerm f) (ha : PureTerm a) :
    PureTerm (Term.app f a) := PureTerm.app hf ha

@[simp] lemma let_binding {x value body}
    (hv : PureTerm value) (hb : PureTerm body) :
    PureTerm (Term.let_ x value body) := PureTerm.let_ hv hb

lemma subst {body value : Term} {x : String}
    (hb : PureTerm body) (hv : PureTerm value) :
    PureTerm (Term.subst x value body) := by
  revert value x
  induction hb with
  | var y =>
      intro value x
      by_cases h : y = x
      · subst h
        simp [Term.subst, hv]
      · simp [Term.subst, h, PureTerm.var]
  | unit =>
      intro value x
      simp [Term.subst]
  | lam y τ ε body hb ih =>
      intro value x
      by_cases h : y = x
      · subst h
        simp [Term.subst]
      · simp [Term.subst, h, ih]
  | app hf ha ihf iha =>
      intro value x
      simp [Term.subst, ihf, iha]
  | let_ y value' body hv' hb' ihv ihb =>
      intro value x
      by_cases h : y = x
      · subst h
        simp [Term.subst, hv']
      · simp [Term.subst, h, ihv, ihb]

end PureTerm

/-- Values are canonical forms that evaluation cannot reduce further. -/
inductive Value : Term → Prop where
  | vUnit : Value Term.unit
  | vLam {x : String} {τ : Ty} {ε : EffectRow} {body : Term} :
      Value (Term.lam x τ ε body)

/-- Typing contexts map variables to types. -/
structure Context where
  entries : List (String × Ty)
  deriving Repr

namespace Context

@[simp] def empty : Context := ⟨[]⟩

@[simp] def extend (Γ : Context) (x : String) (τ : Ty) : Context :=
  ⟨(x, τ) :: Γ.entries⟩

@[simp] def lookup (Γ : Context) (x : String) : Option Ty :=
  Γ.entries.findSome? (fun (y, τ) => if x = y then some τ else none)

end Context

end AuroraCalc
