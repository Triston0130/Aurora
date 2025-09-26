# Aurora API Overview

This document provides a high-level index of the most frequently used crates and
modules. Use it as a map before diving into inline documentation.

## Compiler Crates

| Crate | Highlights |
| --- | --- |
| `aurora-compiler-prototypes` | AST definitions, parser, macro expander, type/effect solver, MIR, LLVM backend. |
| `aurora-effect-solver` | Stand-alone effect row utilities and proptest-based regression suite. |

Key modules:
- `ast` – syntax tree nodes with spans and node IDs.
- `parser` – recursive descent parser emitting `ParseError` diagnostics.
- `hir` / `mir` – intermediate representations used during lowering.
- `backend::llvm` – helper for producing textual LLVM IR.

## Runtime Crates

| Crate | Highlights |
| --- | --- |
| `aurora-runtime-scheduler` | Supervisor tree, cancellation, actor handles, timers. |
| `aurora-zone-manager` | Zone supervisors, GPU/realtime policies, transfer orchestration. |
| `aurora-runtime-io` | Async filesystem/network/timer services built on the scheduler. |
| `aurora-runtime-memory` | Region allocator, recycler, ownership hand-off utilities. |
| `aurora-runtime-ffi` | Safe interface to C dynamic libraries with capability checks. |

## Standard Library

| Module | Highlights |
| --- | --- |
| `stdlib::prelude` | Common imports for async, timers, zone helpers, collections. |
| `stdlib::async_io` | Futures that bridge into runtime timers/IO. |
| `stdlib::collections` | Serde-ready collections respecting effect annotations. |
| `stdlib::region` | Region handle utilities and safe transfers between zones. |
| `stdlib::zone` | Ergonomic zone descriptors and runtime entry helpers. |

## Developer Tooling

- `aurorapm` – manifest initialisation, dependency editing.
- `aurorafmt` – formatting CLI with diff/check modes.
- `auroralint` – hygiene linter with JSON output.
- `aurorals` – JSON-RPC language server skeleton responding to `initialize`,
  `textDocument/hover`, and `shutdown`.
- `auroradebug` – Debug Adapter Protocol stub for IDE debugging experiments.
- `aurorarepl` – command-line evaluator recognising simple `let` bindings.

## Integration Tests

Cross-crate integration scenarios live under `tests/integration`. These tests
exercise:

- Actor pipelines bridging scheduler + stdlib futures.
- Timer futures handing off to region utilities.
- FFI round-trips against the platform C runtime.

Use `cargo test -p aurora-integration-tests -- --nocapture` to reproduce the
full suite.

## Contribution Tips

When introducing new public APIs:

1. Document the module in this overview.
2. Add examples to the relevant chapter in `docs/book/`.
3. Update the integration tests or sample applications if behaviour changes.

Keeping this index current ensures contributors always have an accurate map of
Aurora's surface area.
