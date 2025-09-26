# IR Lowering Plan (Draft)

## Goals
- Define an intermediate representation (IR) that captures Aurora's semantic information (types, effects, zones, ownership hints) while remaining suitable for optimisation and LLVM lowering.
- Describe lowering stages from AST → HIR (High-level IR) → MIR (Mid-level IR) → LLVM IR, aligning with constraint/inference outputs.

## Proposed Stages
1. **HIR (High-Level IR)**
   - SSA form per function with explicit control-flow graph.
   - Annotated with resolved `Type`, `EffectRow`, and `ZoneDescriptor` metadata.
   - Ownership markers distinguishing move/borrow semantics for borrow checker diagnostics.
   - Captures pattern matching, async/await, and actor constructs before desugaring.

2. **MIR (Mid-Level IR)**
   - Lowered from HIR with pattern matches expanded into branching, async/await transformed into state machines, and actor sends mapped to runtime calls.
   - Incorporates borrow-check results to insert drop/cleanup operations.
   - Effects represented as metadata scopes for later optimisation (e.g., no unwinding needed if `effects = ∅`).

3. **LIR/LLVM IR**
   - Final lowering stage mapping MIR to LLVM IR (or alternative backend) with zone metadata carried via LLVM attributes/metadata nodes.
   - GPU zones emit separate modules targeting PTX/SPIR-V; real-time zones flagged with `no_unwind`, `no_heap` attributes.

## Data Flow
```
AST --(Constraint Generation)--> Constraints + Zones
      --(Inference/Borrow Check)--> Annotated AST
      --(HIR Builder)--> HIR (typed, effect-aware)
      --(Simplification)--> MIR
      --(Backend)--> LLVM IR / PTX / ...
```

## Key Tasks
- Define HIR node set with typed operands and zone annotations.
- Implement HIR builder consuming AST + inference results.
- Design effect-aware optimisation passes (e.g., eliminate try/catch when effect set empty).
- Determine metadata encoding for zones in LLVM (custom MD nodes).
- Sketch GPU offload path: collect HIR functions tagged `zone=gpu`, emit separate module, generate host stubs for data transfers.

## Next Steps
1. Formalise HIR schema (enums/structs) and integrate with constraint outputs.
2. Prototype HIR builder for basic functions (no actors/zones yet) to validate pipeline.
3. Define zone metadata translation rules for MIR → LLVM.

---
Living document — expand as lowering components are implemented.
