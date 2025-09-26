# Benchmark Harness

This crate powers the empirical performance and safety studies required by
Task 26. It measures throughput and latency across key runtime components and
verifies safety invariants during execution.

## Building & Running

```bash
cargo bench -p aurora-benchmarks
```

Benchmark results (Criterion JSON + Markdown) are written to
`research/benchmarks/target/criterion/` by default. Copy summary statistics into
`research/benchmarks/results/` when reporting findings.

## Scenarios

| Benchmark | Description |
| --- | --- |
| `actor_spawn_join` | Measures supervisor throughput when spawning and joining actor batches. |
| `zone_handoff` | Times region handle transfers between CPU and GPU regions. |
| `timer_wheel` | Evaluates scheduling overhead for large batches of zero-delay timers. |
| `ffi_roundtrip` | Benchmarks repeated FFI calls (`strlen`) through the capability manager. |
| `gpu_executor` | Executes batched GPU kernel submissions using the simulated executor. |
| `realtime_executor` | Measures realtime task scheduling versus deadline budget. |

Initial implementation covers the first six scenarios; extend `benches/` as new
workloads are identified.

## Result Aggregation

Run the collator to summarise results into Markdown:

```bash
cargo run -p aurora-benchmarks --bin collate -- --output research/benchmarks/results/latest.md --commit "$(git rev-parse HEAD)"
```

The tool scans `target/criterion` for `estimates.json` files and emits a table
of mean/median times and configured throughput values.

## Safety Checks

Benchmarks assert invariants (no leaked handles, deadlines met where
applicable). Failures panic the benchmark and should be investigated before
publishing results.

## Dependencies

- `criterion` for statistical benchmarking.
- Runtime crates under `runtime/` and zone manager.

Add new benchmarks via `benches/<name>.rs` and register them with Criterion.
