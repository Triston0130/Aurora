# Core Calculus Formalisation Plan

Aurora's compiler relies on an effect-aware, region-annotated lambda calculus.
This folder tracks the mechanised proof effort required by Task 26.

## Goals

1. **Formal Syntax:** Encode expressions, types, regions, and effect rows.
2. **Static Semantics:** Implement typing and effect judgement rules.
3. **Dynamic Semantics:** Define small-step reduction with zone/region
   transitions.
4. **Soundness Proof:** Prove progress and preservation (including effect-row
   preservation and region safety).
5. **Capability Invariants:** Extend the proof to ensure zone policies respect
   capability constraints (e.g., GPU actors cannot access realtime-only
   resources).

## Tooling

- **Lean 4** (`lean-toolchain` file forthcoming).
- Alternative Coq scripts under `coq/` if Lean libraries prove limiting.

## Milestones

| Milestone | Description | Owner | Due |
| --- | --- | --- | --- |
| M1 | Syntax + helper lemmas | TBD | |
| M2 | Typing rules & substitution lemmas | TBD | |
| M3 | Reduction semantics & progress proof | TBD | |
| M4 | Preservation including effect rows | TBD | |
| M5 | Capability invariants + combined theorem | TBD | |

## Repository Layout (planned)

```
research/proofs/core_calculus/
  README.md
  lean/
    src/
      syntax.lean
      typing.lean
      reduction.lean
      soundness.lean
    aurora.lean (aggregation file)
  coq/
    README.md
```

Each milestone should land as a PR referencing Task 26 and update the checklist
above.
