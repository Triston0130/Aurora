use aurora_zone_manager::RealtimeExecutor;
use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use std::time::Duration;

fn realtime_schedule(batch: usize) {
    let executor = RealtimeExecutor::new();
    let budget = Duration::from_micros(200);

    for _ in 0..batch {
        let job = executor.schedule("rt", budget, || {
            // Simulate some deterministic work
            let mut acc = 0u64;
            for i in 0..128 {
                acc = acc.wrapping_add(i * i);
            }
            acc
        });
        let outcome = job.wait();
        assert!(matches!(
            outcome.status,
            aurora_zone_manager::RealtimeStatus::Completed
        ));
    }
}

fn realtime_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("realtime_executor_schedule");
    let batch = 128;
    group.throughput(Throughput::Elements(batch as u64));
    group.bench_function("schedule_batch_128", |b| {
        b.iter(|| realtime_schedule(batch))
    });
    group.finish();
}

criterion_group!(realtime, realtime_benchmark);
criterion_main!(realtime);
