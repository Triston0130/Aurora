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
| `actor_spawn_join` | Measures supervisor throughput when spawning N actors and awaiting completion. |
| `zone_handoff` | Times region transfer CPU → GPU → CPU using the zone manager. *(WIP)* |
| `timer_wheel` | Evaluates timer queue latency under concurrent load. *(WIP)* |
| `ffi_roundtrip` | Benchmarks FFI boundary calls with capability checks. *(WIP)* |

Initial implementation focuses on the actor throughput benchmark; additional
scenarios should extend `benches/`.

## Safety Checks

Benchmarks assert invariants (no leaked handles, deadlines met where
applicable). Failures panic the benchmark and should be investigated before
publishing results.

## Dependencies

- `criterion` for statistical benchmarking.
- Runtime crates under `runtime/` and zone manager.

Add new benchmarks via `benches/<name>.rs` and register them with Criterion.
