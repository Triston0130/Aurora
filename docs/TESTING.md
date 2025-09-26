# Testing Guide

Aurora maintains a cross-crate test suite covering compiler passes, runtime subsystems, the standard library, and integration scenarios.

## Quick Start

Run the bundled helper to execute formatting, clippy, unit, integration, and doc tests:

```
./tools/test-all.sh
```

CI reuses the same checks via `./tools/ci.sh`. Developers can mimic the individual CI stages locally:

```
AURORA_CI_MODE=lint ./tools/ci.sh   # fmt + clippy
AURORA_CI_MODE=test ./tools/ci.sh   # unit/integration/doc tests
./tools/ci.sh                       # full pipeline
```

The script performs the following steps:

1. `cargo fmt --all`
2. `cargo clippy --workspace --all-targets --all-features -- -D warnings`
3. `cargo test --workspace --all-targets --all-features -- --nocapture`
4. `cargo test --workspace --doc -- --nocapture`

## Individual Commands

- **Compiler & runtime unit tests:** `cargo test --all`
- **Doc tests only:** `cargo test --workspace --doc`
- **Integration tests:** `cargo test -p aurora-integration-tests`

## Adding Tests

1. Place unit tests alongside the module they exercise (`#[cfg(test)]`).
2. Integration tests that span multiple crates belong in `tests/integration`.
3. Update documentation with runnable examples; doc tests are executed automatically.
4. Ensure new crates or binaries participate in the workspace test commands.

All pull requests must pass the full suite before review.
