use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RealtimeStatus {
    Completed,
    DeadlineMissed(Duration),
}

#[derive(Debug)]
pub struct RealtimeOutcome<T> {
    pub name: Arc<str>,
    pub elapsed: Duration,
    pub status: RealtimeStatus,
    pub result: Option<T>,
}

#[derive(Default)]
struct RealtimeMetrics {
    scheduled: AtomicUsize,
    completed: AtomicUsize,
    missed_deadline: AtomicUsize,
}

#[derive(Clone)]
pub struct RealtimeExecutor {
    metrics: Arc<RealtimeMetrics>,
}

impl RealtimeExecutor {
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(RealtimeMetrics::default()),
        }
    }

    pub fn schedule<F, T>(
        &self,
        name: impl Into<Arc<str>>,
        budget: Duration,
        task: F,
    ) -> RealtimeTaskHandle<T>
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static,
    {
        self.metrics.scheduled.fetch_add(1, Ordering::SeqCst);

        let (tx, rx) = crossbeam_channel::bounded(1);
        let name = name.into();
        let metrics = self.metrics.clone();

        let worker = thread::spawn(move || {
            let start = Instant::now();
            let result = task();
            let elapsed = start.elapsed();
            let status = if elapsed <= budget {
                metrics.completed.fetch_add(1, Ordering::SeqCst);
                RealtimeStatus::Completed
            } else {
                metrics.missed_deadline.fetch_add(1, Ordering::SeqCst);
                RealtimeStatus::DeadlineMissed(elapsed - budget)
            };
            let _ = tx.send(RealtimeOutcome {
                name,
                elapsed,
                status,
                result: Some(result),
            });
        });

        RealtimeTaskHandle {
            receiver: rx,
            join: Mutex::new(Some(worker)),
        }
    }

    pub fn metrics(&self) -> (usize, usize, usize) {
        (
            self.metrics.scheduled.load(Ordering::SeqCst),
            self.metrics.completed.load(Ordering::SeqCst),
            self.metrics.missed_deadline.load(Ordering::SeqCst),
        )
    }
}

impl Default for RealtimeExecutor {
    fn default() -> Self {
        Self::new()
    }
}

pub struct RealtimeTaskHandle<T> {
    receiver: crossbeam_channel::Receiver<RealtimeOutcome<T>>,
    join: Mutex<Option<thread::JoinHandle<()>>>,
}

impl<T> RealtimeTaskHandle<T> {
    pub fn wait(self) -> RealtimeOutcome<T> {
        if let Some(handle) = self.join.lock().unwrap().take() {
            let _ = handle.join();
        }
        self.receiver
            .recv()
            .expect("realtime worker must send completion result")
    }
}
