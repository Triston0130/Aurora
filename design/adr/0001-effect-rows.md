# ADR 0001 — Effect Rows as Core Error/Effect Abstraction

- **Status**: Accepted (2025-02-14)
- **Context**: Aurora requires a statically tractable model for propagating and composing computational effects (IO, errors, cancellation, etc.) that integrates with Hindley–Milner type inference and ownership semantics.
- **Decision**:
  - Represent function effects using *row-polymorphic effect sets* of the form `[Effect₁, …, Effectₙ | ε]`, where `ε` is an optional tail variable capturing residual effects.
  - Use order-insensitive sets with duplicate elimination to simplify equivalence checking and diagnostic reporting.
  - Provide a unification-based solver that merges fixed effects via set union and enforces tail compatibility (initial prototype implemented in `compiler/effect-solver`).
  - Reject unification of distinct tail variables at this stage; future work will introduce substitution maps and row variable constraints.
- **Consequences**:
  - Compiler front-end must surface effect annotations alongside type information in symbol tables and diagnostics.
  - Type inference engine requires extension of Algorithm W with effect row constraints; the current prototype validates data structures and error reporting patterns.
  - Diagnostic tooling can leverage canonicalised effect rows for stable messaging order.
  - Additional research tasks: full support for tail substitution, effect capability traits, and interaction with async/actor semantics.
- **Notes**:
  - The effect solver prototype (`aurora-effect-solver`) includes property-based tests ensuring union correctness and duplicate elimination, forming the basis for future integration tests.
  - This ADR supersedes any previous ad-hoc plans for exception-like checked errors.
