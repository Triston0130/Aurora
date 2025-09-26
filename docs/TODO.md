# Aurora Completion Checklist (Linear Order)

This list enumerates every task required to take Aurora from its current prototype state to a fully usable 1.0 release. Work through the numbers sequentially; each item represents a concrete deliverable.

1. Publish governance artefacts: licence, contribution guidelines, RFC process, decision-making structure, release policy.
~~2. Finish the Language Reference Manual: all chapters (syntax, typing, effects, ownership, concurrency, zones, FFI) plus formal appendix and worked examples.~~ (done)
~~3. Lock down the EBNF grammar and ambiguity tests with a representative program corpus.~~ (done)
~~4. Implement complete parser support for the full syntax surface (modules, imports, traits, generics, pattern matching, async/await, zone annotations).~~ (done)
~~5. Stabilise AST definitions with node IDs, span information, and documentation hooks.~~ (done)
~~6. Build the symbol resolution and module/import system with hygiene and diagnostics.~~ (done)
~~7. Develop the Hindley–Milner style type/trait inference engine integrated with effect rows, lifetimes, and trait constraints.~~ (done)
~~8. Integrate borrow checking and region analysis, enforcing ownership and lifetime rules across the AST.~~ (done)
~~9. Expand the constraint/effect solver to full generality, including trait constraints, region capabilities, and handler desugaring.~~ (done)
~~10. Implement the diagnostics framework: rich error reporting, notes, suggestions, and span rendering.~~ (done)
~~11. Introduce incremental compilation infrastructure (persistent arenas, dependency tracking, caches).~~ (done)
~~12. Extend HIR to cover every language construct, including pattern matching, traits, async/await, actors, and region scopes.~~ (done)
~~13. Grow MIR into SSA form with control-flow graphs, borrow regions, drop scheduling, and optimisation passes (effect-aware DCE, RC elision, zone transforms).~~ (done)
~~14. Implement the LLVM-based backend for CPU targets, then add GPU (PTX/SPIR-V) and realtime attribute emission for zones.~~ (done)
15. Finalise compile-time evaluation: hygienic macro system, procedural macros, const-eval engine, compile-time reflection APIs. *(CTFE MVP underway)*
~~16. Replace the Tokio-based runtime spike with an Aurora-native task scheduler supporting structured concurrency, cancellation, supervision trees, and zone enforcement.~~ (done)
~~17. Deliver the zone manager: GPU offload path, realtime constraints, sandbox policies, data transfer orchestration.~~ (done)
~~18. Implement region allocator, ownership hand-off between zones, and optional cycle collection (Perceus-inspired) in the runtime.~~ (done)
~~19. Build the asynchronous I/O stack (files, networking, timers) integrated with effect types and structured concurrency.~~ (done)
~~20. Provide robust FFI support (C ABI, safe wrappers, panic propagation, capability checks).~~ (done)
~~21. Assemble the standard library core: Option/Result, strings, iterators, collections, smart pointers, region handles, async primitives, zone APIs, error utilities, math/IO/serialization.~~ (done)
~~22. Compile and pass an extensive suite of unit, integration, and doc tests across compiler, runtime, and stdlib.~~ (done)
~~23. Construct developer tooling: package manager (aurorapm), formatter (aurorafmt), linter, Language Server Protocol implementation, debugger adapters, REPL.~~ (done)
24. Set up continuous integration pipelines for linting, formatting, tests, docs, fuzzing, sanitizers, and benchmarking (with dashboards).
25. Produce documentation and outreach material: refreshed README, Aurora Book, API docs, blog/site, sample applications (web server, GPU kernel, realtime controller), academic publications.
26. Complete research validation: mechanised proofs or formal verification of core calculus, empirical performance and safety studies, developer usability feedback.
27. Achieve self-hosting: compile the front-end and MIR passes with Aurora itself, package cross-platform toolchains, and publish the 1.0 release with installers and registry launch.

Track progress by striking numbers as they finish; open issues and PRs should reference the corresponding task number.
