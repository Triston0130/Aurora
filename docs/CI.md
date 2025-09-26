# Continuous Integration Plan

Aurora's CI mirrors local developer workflows while covering the platforms we officially support.

## Runner Matrix

| Job | OS Image | Rust Toolchain | Purpose |
| --- | -------- | -------------- | ------- |
| lint | `ubuntu-latest` | `stable` | Enforces formatting and Clippy lints with all features. |
| test-linux | `ubuntu-latest` | `stable` | Runs full unit/integration/doc tests with all features. |
| test-macos | `macos-latest` | `stable` | Validates host-specific code paths on macOS. |
| test-windows | `windows-latest` | `stable` | Validates Windows support and path handling. |

### Notes
- All jobs use Rustup with the workspace's default `stable` toolchain; nightly is unnecessary for the current codebase.
- Linux jobs enable `--all-features` to exercise optional components consistently.
- macOS and Windows jobs run the core test suite without doc tests to keep runtimes predictable; doc tests are covered on Linux.
- Future GPU runners (CUDA/Metal) will be introduced when the runtime requires vendor toolchains.

## Shared Conventions

- Use `tools/ci.sh` from CI and locally for deterministic linting and testing. Set `AURORA_CI_MODE=lint` to run only formatting and Clippy, `AURORA_CI_MODE=test` to run the test suite, or omit the variable for the full pipeline. MacOS/Windows jobs set `AURORA_SKIP_DOC_TESTS=1` to avoid long-running doc tests already covered on Linux.
- Fail the build on any warning by passing `-D warnings` to Clippy.
- Surface diagnostic logs on failure by printing the last 200 lines of the build directory.
- Cache the Rust toolchain, registry index, and build artifacts per job/OS to minimise runtimes.

Updating this document when the matrix or tooling changes keeps the roadmap aligned with CI coverage.
