# Aurora Borrow Checker & Region Analysis – Design Notes

This document sketches the plan for integrating ownership/borrow checking and region analysis (TODO #8) into the Aurora compiler stack.  The goal is to statically enforce the language’s ownership rules, provide precise diagnostics, and feed region information into later MIR optimisations.

## 1. Goals

1. Compile-time prevention of use-after-move, use-after-drop, and double-mutable-aliasing.
2. Region/zone interaction: track which region/zone a value belongs to and enforce lifetime ordering (which zone drops first, etc.).
3. Diagnostics at AST spans with suggestions.
4. Provide inferred lifetime/region metadata to the MIR lowering stage (for drop scheduling).

## 2. Intermediate IR choice

- Introduce a **Borrow IR (BIR)** pass after type inference but before MIR lowering.
- BIR nodes carry:
  * `ownership` state (`owned`, `borrowed-mut`, `borrowed-imm`, `moved`)
  * `lifetime/region` IDs (mapping to AST spans for diagnostics).

## 3. Ownership tracking strategy

- Use a data-flow analysis over expression trees and control-flow constructs.
- For each variable binding, maintain an `OwnerState` record with:
  * current ownership state (owned, moved, imm-borrowed count, mut-borrowed flag)
  * region/zone metadata (from declaration/constructor zones).
- Provide helper functions to check state transitions (move, copy, borrow).

## 4. Borrow checking algorithm

A. Traverse AST/HIR with environment `BorrowEnv`:
  - Enter new scope: push environment
  - On `let name = expr`: eval `expr` -> state -> assign to `name`
  - On `name` usage: check state per context (move, copy, borrow)
  - On `&expr` (imm borrow): ensure not mutably borrowed
  - On `&mut expr`: ensure no other borrows or moves pending
  - On moves (passing by value, assigning to field): mark `moved`

B. Control flow support (if/loop): compute states for each branch, intersect at merge point.

## 5. Region analysis

- Regions correspond to zones, blocks, or explicit region constructs.
- Each region has a start/end span; values track defining region.
- When value escapes region (return, outer scope), check that lifetime outlives usage.
- Provide zone capabilities (from earlier pipeline) to ensure borrows respect zone transitions.

## 6. Diagnostics

- Provide `BorrowDiagnostic` with:
  * `span` of offending use
  * `notes` for origin (where value was moved/borrowed)
  * `suggestions` (e.g., use `clone`, introduce block)

## 7. Integration steps

1. Extend HIR expressions/statements with span info (already available once AST spans exist).
2. Implement BIR pass (`compiler/prototypes/src/borrow.rs`):
   - BIR building (mirroring HIR, but focusing on ownership transitions)
   - Borrow checker operations (with diagnostic accumulation).
3. Extend `SolvedAnalysis` to carry borrow diagnostics for later phases.
4. Update MIR lowering to use region info from BIR for drop scheduling prep.
5. Provide tests: small programs covering moves, copies, nested borrows, loops, zone interactions.

## 8. Future work

- Integration with trait/Vote solving for `Clone`, `Copy` detection
- Interaction with async/await (holding borrows across `await`)
- Zone-specific region analysis (GPU vs CPU memory) to feed runtime scheduler.

