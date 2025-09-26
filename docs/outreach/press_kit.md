# Aurora Press Kit

**Mission:** Deliver a research-grade systems language that unifies ownership,
effect typing, and structured concurrency across CPUs, GPUs, and real-time
controllers.

## Quick Facts

- **Project Lead:** Prof. Triston Miller (Harvard SEAS)
- **Status:** Research prototype (Task 24 complete, CI operational)
- **License:** Apache 2.0
- **Repository:** https://github.com/Triston0130/Aurora

## Elevator Pitch

Aurora is a new systems language exploring how ownership, effect systems, and
structured concurrency can coexist without sacrificing performance. The runtime
monitors supervision trees, enforces zone policies, and allows zero-copy region
handoffs between CPU, GPU, and realtime executors.

## Feature Highlights

- Effect-aware type checker and trait solver.
- Structured concurrency runtime with supervision + cancellation cascades.
- Zone manager supporting GPU offload, realtime constraints, and sandboxed I/O.
- Region allocator with deterministic ownership transfer.
- Developer tooling ecosystem (package manager, formatter, linter, LSP, debugger
  stub, REPL).

## Suggested Quotes

> "Aurora lets us reason about effects, ownership, and heterogeneity with the
>  same level of rigor." – Prof. Triston Miller

> "Zones turn GPUs and real-time controllers into first-class citizens of the
>  type system." – Aurora Runtime Team

## Visual Assets (to be added)

- Logo (SVG + PNG)
- High-level architecture diagram
- Scheduler supervision tree schematic

## Contact

- Email: research@auroralang.org *(placeholder)*
- GitHub Issues: https://github.com/Triston0130/Aurora/issues
