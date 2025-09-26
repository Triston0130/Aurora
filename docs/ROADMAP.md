# Aurora Language Implementation Roadmap

## Vision
Aurora targets a safe, effect-aware systems language that pairs compile-time guarantees with practical concurrency and heterogeneous execution. The guiding principles are:
- **Human-centred design**: surface-level ergonomics suitable for teaching and large-scale collaboration.
- **Soundness first**: preserve formal guarantees around ownership, effects, and concurrency before prioritising convenience features.
- **Progressive disclosure**: allow beginners to be productive quickly while letting experts control every detail of performance, memory, and resource use.

## Programme Phases
Each phase concludes with internal reviews and publishable artefacts (papers, talks, or technical reports), reflecting the academic framing of this project.

### Phase 0 — Foundational Research & Governance (Weeks 1–3)
Deliverables:
- Formalisation of the core calculus capturing types, effects, ownership, and regions.
- Governance charter, contribution guidelines, and RFC process draft.
- Literature survey comparing Aurora’s design against Rust, Koka, Pony, Cyclone, and modern effect systems.

### Phase 1 — Specification Authoring (Weeks 2–6)
Deliverables:
- Language Reference Manual (LRM) draft covering syntax, typing rules, effect system, memory model, and concurrency semantics.
- EBNF grammar validated by test corpus and ambiguity analysis.
- Design notes for zones, compile-time evaluation, and runtime architecture.
- Prototype examples demonstrating idiomatic Aurora patterns.

### Phase 2 — Tooling Infrastructure (Weeks 4–8)
Deliverables:
- Monorepo with workspaces for compiler, runtime, standard library, and tooling.
- Continuous integration pipeline (linting, formatting, doc generation, tests).
- Package manager skeleton with dependency resolution spec.
- Language server scaffolding reusing compiler front-end crates.

### Phase 3 — Prototype Compiler & Runtime (Weeks 6–14)
Deliverables:
- Front-end in a host language (Rust preferred) implementing lexer, parser, AST, and type/effect inference.
- Borrow checker and region analysis passes with regression suite.
- Initial runtime featuring async executor, actor scheduler, and region allocator.
- Standard library core (Option, Result, Vec, String, collections, async primitives).

### Phase 4 — Self-Hosting & Optimisation (Weeks 12–22)
Deliverables:
- Aurora-written compiler front-end and mid-end, bootstrapped via the host compiler.
- LLVM-based back-end with zone-aware code generation hooks.
- Compile-time evaluation engine supporting macros and const functions.
- Benchmarks versus Rust/C++ for representative workloads.

### Phase 5 — Ecosystem & Outreach (Weeks 18–26)
Deliverables:
- Stable release 1.0 manifesting MVP feature set.
- Documentation suite (Aurora Book, Reference Manual, API docs) published online.
- Package registry launch with seed libraries (FFI utilities, math, serialization).
- Academic dissemination: at least one conference submission or journal article detailing Aurora’s type-and-effect system.

## Immediate Milestones (Next 2 Weeks)
1. **Specification Expansion**: flesh out LRM chapters for syntax (full EBNF), type system, and memory model with worked examples.
2. **Parser Prototype**: implement AST + parser crate with golden tests to complement existing lexer.
3. **Effect Solver Integration**: extend row solver with substitution maps and design notes for type inference integration.
4. **Runtime Supervision Design**: extend scheduler spike with supervision/error propagation plan and deterministic tests.
5. **Zone Architecture Draft**: author design note defining zone descriptors, codegen hooks, and runtime enforcement assumptions.

## Operating Rhythm
- Weekly lab meetings synthesising progress, risks, and experiment results.
- Bi-weekly public updates (blog posts, mailing list summaries).
- Triage board classifying work into research, implementation, documentation, and outreach tracks.

## Success Criteria for 1.0
- Compile-time guarantees: no accepted program exhibits data races, use-after-free, or unhandled declared effects.
- Concurrency runtime sustains >1M lightweight actors with acceptable throughput.
- Zones infrastructure can offload a sample numeric kernel to GPU and enforce real-time constraints in a simulated environment.
- Tooling parity: formatter, linter, package manager, and LSP server available on macOS, Linux, and Windows.

## Personal Integration
This project doubles as my capstone contribution: each phase’s artefact is designed to yield publishable material or teaching resources. Phase 0 delivers a course module on modern type-and-effect systems; Phase 3 culminates in a systems programming practicum; Phase 5 supports future graduate theses exploring Aurora’s extensions.
