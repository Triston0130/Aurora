use aurora_runtime_scheduler::{ActorDirective, Message, Zone};
use aurora_zone_manager::ZoneManager;
use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use crossbeam_channel::bounded;
use std::time::Duration;

fn schedule_timeouts(batch: usize) {
    let manager = ZoneManager::new();
    let (tx, rx) = bounded::<()>(batch);

    let actor = manager
        .spawn_actor("timer-target", Zone::cpu(), move |_, msg| match msg {
            Message::Work(_) => {
                tx.send(()).expect("signal work");
                ActorDirective::Continue
            }
            Message::Shutdown => ActorDirective::Stop,
            _ => ActorDirective::Continue,
        })
        .expect("spawn timer actor");

    let actor_handle = actor.handle();
    let mut handles = Vec::with_capacity(batch);

    for _ in 0..batch {
        handles.push(manager.schedule_timeout(
            Zone::cpu(),
            Duration::from_millis(0),
            actor_handle.clone(),
            Message::Work("tick".into()),
        ));
    }

    for handle in &handles {
        handle.join().expect("join timer");
    }

    for _ in 0..batch {
        rx.recv().expect("receive tick");
    }

    actor_handle
        .send(Message::Shutdown)
        .expect("shutdown timer actor");
    actor.join().expect("join timer actor");
}

fn timer_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("timer_wheel_schedule");
    let batch = 128;
    group.throughput(Throughput::Elements(batch as u64));
    group.bench_function("schedule_batch_128", |b| {
        b.iter(|| schedule_timeouts(batch))
    });
    group.finish();
}

criterion_group!(timers, timer_benchmark);
criterion_main!(timers);
