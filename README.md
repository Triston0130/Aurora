# Aurora Programming Language

> Effect-aware systems programming across CPUs, GPUs, and realtime controllers.

Aurora is a research-grade language exploring how ownership, effect typing, and
structured concurrency can coexist without sacrificing performance. This
repository hosts the entire stack: specification, compiler prototypes, runtime
services, standard library, developer tooling, sample applications, and outreach
materials.

## Highlights

- **Effect-aware type system** with region ownership and trait constraints.
- **Structured concurrency runtime** featuring supervision trees and zone
  policies.
- **Zone manager** for GPU offload, realtime deadlines, and sandboxed I/O.
- **Developer tooling** (package manager, formatter, linter, LSP, debugger,
  REPL) to make experiments productive.
- **Sample applications** covering web services, GPU data processing, and
  realtime control loops.

## Quick Start

```bash
git clone https://github.com/Triston0130/Aurora.git
cd Aurora
rustup component add rustfmt clippy
./tools/ci.sh                # fmt + clippy + tests + doc tests
```

Use the prototype compiler to run examples:

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/web_server/web_server.aur
```

Target specific CI stages locally:

```
AURORA_CI_MODE=lint ./tools/ci.sh   # lint stage only
AURORA_CI_MODE=test ./tools/ci.sh   # test/doc stage only
```

Rust 1.75+ is recommended. LLVM tools (`rustup component add llvm-tools-preview`)
help inspect emitted IR.

## Documentation

- **Aurora Book:** `docs/book/` (`mdbook serve docs/book`).
- **API Overview:** [`docs/reference/api_overview.md`](docs/reference/api_overview.md).
- **Language Reference:** [`docs/spec/language_reference.md`](docs/spec/language_reference.md).
- **Testing Guide:** [`docs/TESTING.md`](docs/TESTING.md).
- **Outreach Materials:** [`docs/outreach/`](docs/outreach).

## Architecture Overview

```
compiler/           # Front-end experiments, type/effect solver, LLVM backend
runtime/            # Scheduler, zone manager, memory services, async I/O, FFI
stdlib/             # Prelude, async helpers, collections, region utilities
tools/              # aurorapm, aurorafmt, auroralint, aurorals, auroradebug, aurorarepl
examples/           # Web server, GPU kernel, realtime controller samples
docs/               # Book, specs, outreach kits, CI/testing guides
research/           # Notes, citations, experiment logs
```

## Sample Applications

- [`examples/web_server`](examples/web_server) – structured concurrency HTTP
  service in the CPU zone.
- [`examples/gpu_image_filter`](examples/gpu_image_filter) – GPU blur kernel with
  region hand-offs.
- [`examples/realtime_controller`](examples/realtime_controller) – deadline-aware
  control loop in the realtime zone.

Each directory includes runnable `.aur` code and a README covering architecture
choices and experiments.

## Tooling & CI

- `./tools/test-all.sh` – fmt + clippy + unit/integration/doc tests.
- `./tools/ci.sh` – mirrors the GitHub Actions matrix (Ubuntu/macOS/Windows).
- Tooling binaries (`tools/aurora*`) carry their own unit tests (`cargo test -p
  <tool>`).

## Development Philosophy

- **Academic rigour:** features trace back to research questions.
- **Pedagogical clarity:** code and documentation double as teaching material.
- **Safety without compromise:** invariants enforced at compile time or via
  supervisor policies.
- **Interoperability:** FFI capabilities and zone descriptors bring external
  systems into the model.

## Governance & Participation

- License: [Apache 2.0](LICENSE)
- Contribution guide: [`docs/reference/CONTRIBUTING.md`](docs/reference/CONTRIBUTING.md)
- Master checklist: [`docs/TODO.md`](docs/TODO.md)
- Roadmap: [`docs/ROADMAP.md`](docs/ROADMAP.md)

Design records live under `design/adr/`. Research artefacts and citations reside
in `research/`. Outreach kits (press, blog, talk outline) are in
`docs/outreach/`.

---
*Maintained by Prof. Triston Miller (Harvard SEAS) as a capstone research
endeavour.*
