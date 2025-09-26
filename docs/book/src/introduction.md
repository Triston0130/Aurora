# Welcome to Aurora

Aurora is a research-driven systems programming language that combines
ownership, effect typing, and structured concurrency. The project explores how a
single language and runtime can deliver predictable performance across CPUs,
GPUs, and real-time controllers without sacrificing safety.

This book is the canonical entry point for engineers, researchers, and
collaborators who want to understand Aurora's design and use it in practice. It
collects motivation, language concepts, runtime architecture, and links to the
surrounding ecosystem.

## Why Aurora?

- **Ownership with effects** – track resources, regions, and side-effects in a
  single type system.
- **Structured concurrency** – supervisors, cancellation cascades, and zones are
  first-class.
- **Heterogeneous execution** – one programming model for CPUs, GPUs, and
  deterministic control loops.
- **Research-friendly** – every component is built to test new ideas, from the
  compiler pipeline to the runtime schedulers.

## Project Scope

The repository hosts the entire stack:

- Language specification and reference material.
- Prototype compiler front-end, type/trait solver, and MIR → LLVM backend.
- Runtime services for scheduling, zone management, memory regions, and I/O.
- Standard library modules and async/timer abstractions.
- Developer tooling (package manager, formatter, linter, LSP, debugger stub,
  REPL).

Each chapter of this book points to deeper resources within the tree so you can
move from high-level ideas to concrete implementation details.
