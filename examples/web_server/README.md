# Web Server Sample (Placeholder)

The original long-term goal for this directory is to host a full HTTP server
demonstrating structured concurrency. Until the runtime/tooling stack matures,
the `.aur` file contains a lightweight placeholder that exercises the prototype
parser and code-gen.

## Current Behaviour
- Defines a `start_server` function returning an integer port.
- The `main` function calls `start_server` and evaluates the result.
- Produces no diagnostics when compiled with `aurora_cli`.

## Running

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/web_server/web_server.aur
```

This keeps the documentation examples compiling cleanly while the full server
implementation remains on the roadmap.
