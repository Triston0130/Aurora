# Zone Architecture Draft (2025-02-14)

## Goals
- Provide a language-level abstraction for allocating work to specialised execution zones (GPU, real-time, sandbox, etc.).
- Enable the compiler to attach metadata and perform static validation (effect typing, resource bounds) prior to code generation.
- Allow the runtime to enforce constraints (scheduling, memory limits, GC behaviour) with minimal overhead.

## Zone Descriptors
Each zone is defined by a descriptor emitted during compilation. Proposed fields:
- `name`: symbolic identifier (`gpu`, `realtime`, `io`, custom user zones).
- `capabilities`: set of permitted effects (e.g., `NoHeap`, `AsyncAllowed`, `IORestricted`).
- `resource_limits`: optional static hints (memory budget, deadline, priority).
- `target_backend`: enum describing code generation path (`Cpu`, `Ptx`, `Spirv`, `Wasm`...).
- `supervisor_policy`: error handling semantics (restart, propagate, abort).

Descriptors are created via annotations: `@zone(gpu(memory = "256MB")) fn kernel(...) { ... }`. The parser lowers annotations into descriptor IR nodes attached to functions/actors.

## Compiler Responsibilities
1. **Static Validation**
   - Merge zone capabilities with function effect signatures; emit diagnostics if disallowed effects appear (e.g., `IO` inside `realtime`).
   - Ensure region allocations comply with zone policies (e.g., `NoHeap` forbids `Shared<T>` usage).
   - Propagate zone descriptors through call graph (callee inherits caller zone unless overridden with explicit transition).

2. **Lowering Hooks**
   - Introduce `ZoneTransition` nodes in IR when execution crosses zone boundaries.
   - Emit backend-specific entry points: GPU zones lowered to PTX kernels; CPU zones remain LLVM code but tagged with metadata for runtime scheduling.
   - Generate marshaling stubs for cross-zone data transfer (e.g., copy buffers to GPU memory, enforce lifetime constraints across regions).

3. **Metadata Emission**
   - Include zone descriptors in compiled module manifest (`.aurora.zone` section) for runtime discovery.
   - Encode effect restrictions as attributes so tooling (LSP, formatter) can surface warnings inline.

## Runtime Enforcement
- **Zone Manager** maintains registry mapping descriptors to executors (GPU queue, real-time thread pool, sandboxed process, etc.).
- On entering a zone, runtime consults descriptor to:
  - Acquire necessary resources (GPU context, real-time thread, sandbox environment).
  - Attach supervision policy (e.g., restart actor on failure, escalate error to parent scope).
  - Configure memory allocator (region arena, GC disabled, etc.).
- Zone transitions trigger resource marshalling; descriptors specify whether data is copied, pinned, or shared via unified memory.
- Structured concurrency integrates with zones: parent scope cannot exit until child zone tasks signal completion or failure events.

## Initial Target Zones
1. **GPU Zone** (`@zone(gpu)`)
   - Capabilities: `NoBlockingIO`, `NoHeap`, `AsyncAllowed`.
   - Backend: PTX via LLVM NVPTX backend (Phase 4).
   - Runtime: queue kernels, manage device buffers, propagate errors as `ComputeError` effects.

2. **Real-Time Zone** (`@zone(realtime)`)
   - Capabilities: `NoGC`, `NoBlockingIO`, `DeterministicAlloc`.
   - Runtime: dedicated priority thread with preallocated region arena; supervisors escalate deadline misses as `DeadlineMissed` effects.

3. **Sandbox Zone** (future)
   - Capabilities: restricted FFI, limited heap, optional interpreter backend.

## Roadmap
- Phase 2: finalise annotation syntax and descriptor schema; extend parser to capture zone metadata.
- Phase 3: author IR structures for zone transitions; implement validation passes leveraging effect solver.
- Phase 4: integrate with code generation (LLVM metadata hooks, GPU backend scaffolding).
- Phase 5: implement runtime Zone Manager with GPU and real-time executors; expose diagnostics/logging to tooling.

---
Prepared as reference for ongoing zone-related RFCs and implementation tasks.
