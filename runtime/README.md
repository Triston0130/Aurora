# Aurora Runtime System

The runtime provides structured concurrency, actor scheduling, zone management, and optional garbage collection.

## Components
- `scheduler/`: work-stealing actor and async executor.
- `zone_manager/`: GPU offload orchestration, real-time enforcement, resource budgeting.
- `memory/`: region allocator, smart pointer implementations, cycle detector.
- `ffi/`: boundary shims for interacting with C and native libraries.

## Immediate Tasks
1. Build a prototype actor scheduler in Rust with deterministic testing harness.
2. Specify zone descriptors and runtime metadata layout.
3. Draft region allocator interface and invariants.
4. Evaluate candidate libraries for async IO backends (tokio, glommio, custom epoll loop).

## Metrics of Success
- Sub-millisecond message latency under load.
- Graceful handling of actor failures with supervision hooks.
- Zone transitions with bounded overhead (<5% of task runtime) in prototypes.

---
Runtime experiments should be documented in `research/notes/` with reproducible scripts.
