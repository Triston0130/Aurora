use aurora_runtime_scheduler::{ActorConfig, ActorDirective, Message, Scheduler, Zone};
use criterion::{criterion_group, criterion_main, Criterion, Throughput};

fn spawn_and_join(batch: usize) {
    let scheduler = Scheduler::new();
    let (supervisor, _events) = scheduler.root_supervisor("bench-root", Zone::cpu());

    let mut actors = Vec::with_capacity(batch);
    let mut handles = Vec::with_capacity(batch);

    for idx in 0..batch {
        let config = ActorConfig::new(format!("bench-worker-{idx}"), Zone::cpu());
        let actor = supervisor
            .spawn_actor_with(config, |_, msg| match msg {
                Message::Shutdown => ActorDirective::Stop,
                _ => ActorDirective::Continue,
            })
            .expect("spawn actor");
        handles.push(actor.handle());
        actors.push(actor);
    }

    for handle in &handles {
        handle
            .send(Message::Shutdown)
            .expect("send shutdown signal");
    }

    for actor in actors {
        actor.join().expect("join actor");
    }
}

fn actor_spawn_join_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("scheduler_actor_spawn_join");
    let batch = 100;
    group.throughput(Throughput::Elements(batch as u64));
    group.bench_function("spawn_join_batch_100", |b| b.iter(|| spawn_and_join(batch)));
    group.finish();
}

criterion_group!(scheduler, actor_spawn_join_benchmark);
criterion_main!(scheduler);
