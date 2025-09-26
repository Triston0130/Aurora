//! Benchmark harness crate for Aurora runtime and zone manager.
//!
//! Individual benchmarks live under `benches/`. Import this crate when
//! constructing composite scenarios (e.g., GPU hand-off pipelines).

pub fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}
