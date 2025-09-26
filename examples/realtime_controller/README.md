# Realtime Controller Sample (Placeholder)

This placeholder mirrors the future realtime controller example. The current
`.aur` program implements a simple arithmetic helper so that the compiler
pipeline remains green while the full realtime runtime is under construction.

## Running

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/realtime_controller/realtime_controller.aur
```

The program produces no output; it simply ensures the parser and type checker
can process the sample without diagnostics.
