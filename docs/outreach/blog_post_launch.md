# Launching Aurora: Systems Programming for Heterogeneous Futures

Today we are publishing Aurora, a research project that rethinks how systems
languages can deliver safe, predictable performance across heterogeneous
hardware. Aurora unifies ownership, effect typing, and structured concurrency
while providing a runtime that understands GPUs, real-time controllers, and
sandboxed environments.

## Why Now?

Modern workloads span CPUs, GPUs, and dedicated control hardware. Developers
have to juggle disparate APIs and safety models. Aurora asks: *what if a single
language could model these domains without sacrificing performance or safety?*

## What’s Inside the Initial Release

- **Prototype compiler** with AST → HIR → MIR lowering and LLVM IR emission.
- **Effect-aware type system** supporting region ownership and trait constraints.
- **Structured concurrency runtime** with supervision trees and zone policies.
- **Zone manager** that offloads to GPUs, enforces realtime deadlines, and
  fences sandboxed I/O.
- **Standard library** with async/timer wrappers, region helpers, and serde-ready
  collections.
- **Developer tooling suite** (package manager, formatter, linter, LSP, debugger
  stub, REPL) to make experimentation comfortable.

## Try the Samples

Clone the repo and run the examples:

```bash
./tools/ci.sh
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/web_server/web_server.aur
```

Explore GPU and realtime scenarios under `examples/gpu_image_filter` and
`examples/realtime_controller`.

## What’s Next

Task 26 onward covers formal verification, broader documentation, and
self-hosting the compiler. Follow the live checklist in `docs/TODO.md` for the
latest progress.

## Join the Research Effort

We welcome contributors interested in type systems, runtime design, and
heterogeneous computing. Check out `docs/reference/CONTRIBUTING.md` and open a
GitHub issue to introduce yourself.

Let’s build safer, smarter systems software together.
