# Runtime & Tooling

Aurora's runtime and developer tools are designed together. This chapter maps
out the moving pieces so you can navigate the repo effectively.

## Runtime Components

- **Scheduler (`runtime/scheduler`)** – structured-concurrency executor with
  supervision trees, cancellation, and zone enforcement.
- **Zone Manager (`runtime/zone_manager`)** – exposes GPU, realtime, and sandbox
  policies. Provides channel-like APIs for actors and coordinates capability
  guards.
- **Memory Services (`runtime/memory`)** – region allocator and ownership
  transfer primitives inspired by Perceus.
- **I/O Services (`runtime/io`)** – async filesystem, network, and timers built
  on top of the scheduler and zone manager.
- **FFI Layer (`runtime/ffi`)** – safe wrappers for interacting with C
  libraries, including panic propagation and capability checks.

## Standard Library Highlights

- `stdlib::prelude` – curated imports for Option/Result helpers, async wrappers,
  and timer utilities.
- `stdlib::collections` – serde-aware collections that cooperate with effect
  annotations.
- `stdlib::zone` – ergonomic APIs for describing and entering zones from user
  code.
- `stdlib::async_io` – futures and combinators that pair with the runtime's
  timer/IO supervisors.

## Developer Tooling

| Tool | Location | Purpose |
| --- | --- | --- |
| `aurorapm` | `tools/aurorapm` | Manifest init/add commands and (future) dependency resolution. |
| `aurorafmt` | `tools/aurorafmt` | Formatter stub with `--check` and diff output. |
| `auroralint` | `tools/auroralint` | CLI linter for trailing whitespace and TODO hygiene. |
| `aurorals` | `tools/aurorals` | Minimal JSON-RPC language server prototype. |
| `auroradebug` | `tools/auroradebug` | Debug Adapter Protocol stub handling initialize/launch/breakpoints. |
| `aurorarepl` | `tools/aurorarepl` | Command-line REPL recognising `let` bindings. |

Each binary ships unit tests that can be invoked via `cargo test -p <tool>`.
Workflow automation lives in `.github/workflows/ci.yml` and can be mirrored
locally using `tools/ci.sh`.

## Observability & Diagnostics

- Rich span-based diagnostics originate from `compiler/prototypes/src/diagnostics.rs`.
- Runtime supervisors emit structured events; the examples demonstrate tracing
  and failure cascades.
- `docs/CI.md` describes how the GitHub Actions matrix surfaces logs and cached
  artifacts when pipelines fail.

Whenever you add new features, update these sections so contributors know where
to look and how to extend the system.
