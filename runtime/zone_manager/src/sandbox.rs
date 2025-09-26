use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SandboxCapability {
    FileSystem,
    Network,
    Ffi,
    SpawnProcess,
}

#[derive(Debug, Clone)]
pub struct SandboxPolicy {
    pub allowed: HashSet<SandboxCapability>,
    pub memory_limit_bytes: usize,
    pub max_execution_time: Duration,
}

impl SandboxPolicy {
    pub fn new(allowed: HashSet<SandboxCapability>) -> Self {
        Self {
            allowed,
            memory_limit_bytes: 64 * 1024 * 1024,
            max_execution_time: Duration::from_millis(100),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SandboxContext {
    pub memory_limit_bytes: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SandboxStatus {
    Completed,
    CapabilityDenied(SandboxCapability),
    Timeout,
}

#[derive(Debug)]
pub struct SandboxOutcome<T> {
    pub status: SandboxStatus,
    pub result: Option<T>,
    pub duration: Duration,
}

#[derive(Default)]
struct SandboxMetrics {
    executed: AtomicUsize,
    denied: AtomicUsize,
    timeouts: AtomicUsize,
}

#[derive(Clone)]
pub struct SandboxManager {
    metrics: Arc<SandboxMetrics>,
}

impl SandboxManager {
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(SandboxMetrics::default()),
        }
    }

    pub fn run<F, T>(
        &self,
        policy: SandboxPolicy,
        required: &[SandboxCapability],
        task: F,
    ) -> SandboxOutcome<T>
    where
        F: FnOnce(&SandboxContext) -> T + Send + 'static,
        T: Send + 'static,
    {
        for capability in required {
            if !policy.allowed.contains(capability) {
                self.metrics.denied.fetch_add(1, Ordering::SeqCst);
                return SandboxOutcome {
                    status: SandboxStatus::CapabilityDenied(*capability),
                    result: None,
                    duration: Duration::ZERO,
                };
            }
        }

        self.metrics.executed.fetch_add(1, Ordering::SeqCst);
        let context = SandboxContext {
            memory_limit_bytes: policy.memory_limit_bytes,
        };
        let (tx, rx) = crossbeam_channel::bounded(1);
        let join = thread::spawn(move || {
            let start = std::time::Instant::now();
            let result = task(&context);
            let duration = start.elapsed();
            let _ = tx.send((duration, result));
        });

        match rx.recv_timeout(policy.max_execution_time) {
            Ok((duration, result)) => {
                let _ = join.join();
                SandboxOutcome {
                    status: SandboxStatus::Completed,
                    result: Some(result),
                    duration,
                }
            }
            Err(_) => {
                self.metrics.timeouts.fetch_add(1, Ordering::SeqCst);
                SandboxOutcome {
                    status: SandboxStatus::Timeout,
                    result: None,
                    duration: policy.max_execution_time,
                }
            }
        }
    }

    pub fn metrics(&self) -> (usize, usize, usize) {
        (
            self.metrics.executed.load(Ordering::SeqCst),
            self.metrics.denied.load(Ordering::SeqCst),
            self.metrics.timeouts.load(Ordering::SeqCst),
        )
    }
}

impl Default for SandboxManager {
    fn default() -> Self {
        Self::new()
    }
}
