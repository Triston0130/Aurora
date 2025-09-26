use aurora_zone_manager::{DataTransferEngine, GpuExecutor, HostBuffer};
use criterion::{criterion_group, criterion_main, Criterion, Throughput};

fn gpu_identity(batch: usize) {
    let transfers = DataTransferEngine::new();
    let executor = GpuExecutor::new(transfers);
    for i in 0..batch {
        let input = HostBuffer::from((0..1024u32).map(|x| x + i as u32).collect::<Vec<_>>());
        let job = executor.submit("identity", input, |data| Ok(data.to_vec()));
        let result = job.wait().expect("gpu job succeeds");
        assert_eq!(result.output.len(), 1024);
    }
}

fn gpu_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("gpu_executor_identity");
    let batch = 64;
    group.throughput(Throughput::Elements(batch as u64));
    group.bench_function("gpu_batch_64", |b| b.iter(|| gpu_identity(batch)));
    group.finish();
}

criterion_group!(gpu, gpu_benchmark);
criterion_main!(gpu);
