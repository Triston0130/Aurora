# Aurora Standard Library

The standard library delivers foundational data types, async primitives, and systems interfaces consistent with Aurora’s ownership and effect guarantees.

## Modules
- `core/`: Option, Result, tuples, primitives, smart pointers.
- `collections/`: Vec, Map, Set, deque, iterators.
- `async/`: Futures, channels, timers, structured concurrency helpers.
- `io/`: Async file and network operations, buffered streams.
- `resource/`: Zone/region utilities, metrics collectors.

## Design Tenets
- Zero-cost abstractions validated via benchmarks.
- Explicit effect annotations on APIs that may block, allocate, or perform IO.
- Interoperability layers for C FFI and host OS facilities.

## Short-Term Goals
1. Define API sketches for `Option`, `Result`, and `Vec`.
2. Specify channel semantics (back-pressure, fairness).
3. Outline async IO traits and integration points with runtime executor.
4. Draft documentation templates to keep library references consistent.

*Library code will be authored in Aurora once the self-hosting milestone arrives; interim prototypes may be written in Rust for validation.*
