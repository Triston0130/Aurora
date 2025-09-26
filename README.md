# Aurora Programming Language

Aurora is a research-grade systems language that combines static ownership, effectful typing, and structured concurrency to deliver safe performance across heterogeneous hardware. This repository hosts the full stack: specification, compiler, runtime, standard library, and developer tooling.

## Current Status
- Project inception under the "Bootstrap Initiative".
- Roadmap drafted in [`docs/ROADMAP.md`](docs/ROADMAP.md).
- Repository scaffolding underway; component directories in place.

## Near-Term Objectives
1. Execute the numbered master checklist in [`docs/TODO.md`](docs/TODO.md).
2. Flesh out the Language Reference Manual outline under `docs/spec/`.
3. Implement lexical grammar prototypes inside `compiler/prototypes/`.
4. Spike the actor scheduler within `runtime/scheduler/`.
5. Establish continuous integration scripts and coding standards.

## Repository Layout
```
compiler/           # Front-end, middle-end, and back-end crates
runtime/            # Actor scheduler, zone manager, memory services
stdlib/             # Core collections, async primitives, smart pointers
tools/              # Package manager, formatter, linter, language server, REPL, debugger prototypes
docs/               # Roadmap, specifications, reference manuals, tutorials
research/           # Notes, citations, experiment logs
examples/           # Canonical Aurora programs
```

## Bootstrap Workspace
Early-stage experiments live in a shared Cargo workspace rooted at the repository top-level.

```
cargo test --all                             # run unit/integration tests for every crate
cargo test --workspace --doc                 # execute documentation snippets
./tools/test-all.sh                          # fmt + clippy + unit/integration/doc tests
AURORA_CI_MODE=lint ./tools/ci.sh            # CI lint stage (fmt + clippy)
./tools/ci.sh                                # CI-equivalent full pipeline
```

Rust 1.75+ is recommended; the first build will download Tokio and tracing dependencies. These crates bridge us to native Aurora implementations once self-hosting becomes viable.

## Development Philosophy
- **Academic rigour**: every feature is justified with formal analysis or empirical evidence.
- **Pedagogical clarity**: source code and documentation double as teaching material.
- **Safety without compromise**: compilation rejects programs that violate memory, effect, or concurrency invariants.
- **Interoperability**: seamless FFI with C and gradual support for other ecosystems.

## Governance & Participation
- License: [Apache 2.0](LICENSE)
- Contribution guide: [`docs/reference/CONTRIBUTING.md`](docs/reference/CONTRIBUTING.md)
- Governance charter: [`docs/GOVERNANCE.md`](docs/GOVERNANCE.md)
- RFC process: [`docs/RFC_PROCESS.md`](docs/RFC_PROCESS.md)
- Release policy: [`docs/RELEASES.md`](docs/RELEASES.md)

Open collaboration is central to Aurora. Early adopters can participate in design reviews, contribute to the research notebook, and claim tasks from the master checklist.

---
*Maintained by Prof. Triston Miller (Harvard SEAS) as a capstone research endeavour.*
