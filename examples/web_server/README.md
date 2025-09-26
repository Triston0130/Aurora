# Web Server Sample

This example shows how Aurora's structured concurrency runtime can host an HTTP
server entirely inside the CPU zone while delegating blocking I/O to the async
runtime.

## Highlights
- Supervisors manage worker lifetimes and propagate cancellation on shutdown.
- `aurora_runtime_io::network` handles async TCP accept/send operations.
- Logs are forwarded to a dedicated actor to avoid blocking request handlers.

## Running

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/web_server/web_server.aur
```

By default the server listens on `127.0.0.1:8080`. Use `curl` or a browser to
issue requests and observe structured logs in the console.

## Next Steps
- Swap the inline router for an effect-handler driven router once available.
- Add TLS support using the FFI layer and a native TLS library.
- Forward metrics to the realtime zone for deadline monitoring.
