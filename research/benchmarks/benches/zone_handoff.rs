use aurora_runtime_memory::{Region, ZoneHandoff, ZoneTransferEngine};
use criterion::{criterion_group, criterion_main, Criterion, Throughput};

fn handoff_round_trip(batch: usize) {
    let cpu = Region::new("cpu");
    let gpu = Region::new("gpu");
    let engine = ZoneTransferEngine::new();
    let handoff = ZoneHandoff::new(engine);

    for i in 0..batch {
        let payload = vec![i as u8; 128];
        let handle = cpu.allocate(payload).expect("allocate payload");
        let gpu_handle = handoff
            .move_handle(handle, &gpu)
            .expect("cpu -> gpu handoff");
        let cpu_handle = handoff
            .move_handle(gpu_handle, &cpu)
            .expect("gpu -> cpu handoff");
        let _ = cpu_handle.try_take().expect("reclaim payload");
    }

    // reclaim any orphans before dropping regions
    let _ = cpu.reclaim_orphans();
    let _ = gpu.reclaim_orphans();
}

fn zone_handoff_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("zone_handoff_round_trip");
    let batch = 64;
    group.throughput(Throughput::Elements((batch * 2) as u64));
    group.bench_function("handoff_batch_64", |b| b.iter(|| handoff_round_trip(batch)));
    group.finish();
}

criterion_group!(handoff, zone_handoff_benchmark);
criterion_main!(handoff);
