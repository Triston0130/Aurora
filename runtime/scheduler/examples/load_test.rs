use std::thread;
use std::time::Instant;

use aurora_runtime_scheduler::{demo_pipeline, Message, Scheduler, Zone, ZoneGuard};
use tracing::Level;

const ACTOR_COUNT: usize = 8;
const MESSAGES_PER_ACTOR: usize = 10_000;

fn main() {
    tracing_subscriber::fmt()
        .with_target(false)
        .with_max_level(Level::WARN)
        .compact()
        .init();

    // Run the demo pipeline once as a smoke test.
    demo_pipeline();

    let scheduler = Scheduler::new();
    let (supervisor, _events) = scheduler.root_supervisor("load-test", Zone::cpu());

    let mut actors = Vec::with_capacity(ACTOR_COUNT);
    let mut mailboxes = Vec::with_capacity(ACTOR_COUNT);
    for i in 0..ACTOR_COUNT {
        let actor = supervisor
            .spawn_default_actor(format!("actor-{i}"), Zone::cpu())
            .expect("spawn actor");
        mailboxes.push(actor.handle());
        actors.push(actor);
    }

    let start_total = Instant::now();
    let start_dispatch = Instant::now();
    let mut dispatch_threads = Vec::new();
    for (idx, handle) in mailboxes.into_iter().enumerate() {
        dispatch_threads.push(thread::spawn(move || {
            let _guard = ZoneGuard::enter(Zone::cpu());
            for m in 0..MESSAGES_PER_ACTOR {
                handle
                    .send(Message::Work(format!("task-{idx}-{m}")))
                    .expect("send work");
            }
            handle.send(Message::Shutdown).expect("send shutdown");
        }));
    }

    for thread in dispatch_threads {
        thread.join().expect("sender thread panicked");
    }

    let dispatch_elapsed = start_dispatch.elapsed();

    for actor in actors {
        actor.join().expect("actor execution failed");
    }

    let total_elapsed = start_total.elapsed();
    let total_messages = ACTOR_COUNT * MESSAGES_PER_ACTOR;
    let throughput = total_messages as f64 / dispatch_elapsed.as_secs_f64();

    println!(
        "actors={ACTOR_COUNT} messages_per_actor={MESSAGES_PER_ACTOR} total_messages={total_messages} dispatch_time={:.3}s total_time={:.3}s throughput={:.0} msg/s",
        dispatch_elapsed.as_secs_f64(),
        total_elapsed.as_secs_f64(),
        throughput,
    );
}
