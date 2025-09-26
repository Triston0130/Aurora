# Type Inference Integration Plan (Draft)

## Objectives
- Integrate lexer/parser outputs with a Hindley–Milner-style inference engine extended with lifetime and effect tracking.
- Define core data structures (environments, symbols, inference variables) and the workflow for constraint generation/solving.
- Establish a roadmap for staged implementation, coordinating with borrow checking, effect solver, and future IR lowering.

## Architectural Components
1. **Front-End Driver (`compiler/frontend`)**
   - Responsible for orchestrating lexing, parsing, AST validation, and inference.
   - Maintains module-level symbol tables and diagnostic emitters.

2. **AST Annotation Phase**
   - Enriches AST nodes with span information, identifiers, and attribute metadata (including `@zone` annotations once parsed).
   - Normalises syntactic sugar before inference.

3. **Constraint Generator**
   - Traverses the AST producing constraints over types, lifetimes, and effects.
   - Emits:
     - `TypeConstraint`: relationships like `τ1 = τ2`, function signatures, trait obligations.
     - `EffectConstraint`: rows produced by expression evaluation, leveraging `aurora-effect-solver`.
     - `RegionConstraint`: lifetime outlives relations, ownership moves.

4. **Unifier / Solver**
   - Core Hindley–Milner unifier extended with:
     - Type variables (`α`), effect variables (`ϵ`), lifetime variables (`'ℓ`).
     - Substitution maps for all three domains.
   - Uses `aurora-effect-solver` for effect row unification; merges with type substitutions.

5. **Borrow Checker Hooks**
   - After type inference, run a dataflow analysis using the annotated types/lifetimes to ensure ownership and borrowing rules.
   - Borrow checker depends on inference results to know usage types and effectful drops.

## Data Structures
- `TypeVarId`, `EffectVarId`, `LifetimeVarId`: newtypes wrapping usize counters.
- `Type`: algebraic data type covering primitives, function types, tuples, references, smart pointers, generics.
- `Scheme`: quantifies over type/effect/lifetime variables for generalisation at let-bindings.
- `ConstraintGraph`: adjacency between expressions and constraints, enabling incremental solving.
- `TypeEnv (Γ)`: stack of scopes mapping identifiers to `Scheme`s.
- `TraitEnv (Δ)`: pending trait obligations with source spans for diagnostics.
- `EffectEnv`: per-scope required effect handlers.

## Algorithm Overview
1. **Inference**
   - For each expression `e`, produce `(τ, ε, constraints)`.
   - Apply Algorithm W variant:
     - Fresh type/effect variables for declarations.
     - Use unifier to resolve constraints; on failure, produce diagnostics referencing AST spans.
   - Generalise at `let` boundaries (only generalise variables not captured by outer constraints/regions).

2. **Effect Handling**
   - Propagate effect sets upward; ensure declared effect clauses (`fn foo() ! εdecl`) subsume inferred effects.
   - For `try`/`catch` constructs, subtract handled effects from propagation.

3. **Lifetimes/Regions**
   - Associate each reference with a lifetime variable; unify via outlives constraints derived from scopes.
   - Region blocks introduce fresh region vars; ensure references/pointers do not escape.

4. **Trait Resolution**
   - Accumulate trait obligations; defer actual resolution to trait solver after types/effects are known.
   - Integrate effect capability traits (e.g., `trait IO { effect IO }`) by adding effect constraints when traits are used.

## Staged Implementation Plan
1. **Phase A (Current)**
   - Finalise AST + parser enhancements (done).
   - Implement type data structures and environment skeletons (`compiler/frontend/types.rs`).
   - Integrate effect solver crate via workspace dependency (done).

2. **Phase B**
   - Develop constraint generation for expressions covering literals, variables, function application, let bindings.
   - Implement base unifier producing type/effect substitutions.
   - Emit diagnostics for basic type/effect mismatches.

3. **Phase C**
   - Extend to pattern matching, control flow (`if`, `match`, loops).
   - Add trait obligations, typeclass-style inference for method resolution.
   - Introduce lifetime constraints and preliminary borrow checking analysis.

4. **Phase D**
   - Integrate zone annotations: constraint generator ensures functions respect zone capabilities; unifier includes effect capability checks.
   - Hook inference results into runtime codegen metadata (effect sets, zones).

## Tooling & Testing
- Unit tests for constraint generation (AST snippet → expected constraints).
- Integration tests comparing inference output against golden files (JSON or debug format).
- Property tests leveraging `proptest` to ensure unifier handles commutativity of effect rows.
- Continuous validation via CI (already configured) once modules compiled.

## Dependencies & Next Steps
- Extend parser to capture `@zone` attributes (planned next step).
- Define diagnostic reporting style aligned with LRM (error codes, suggestions).
- Prepare RFC outlining inference semantics for community review.

---
Prepared to guide upcoming implementation tasks; will evolve into full design document as components mature.
