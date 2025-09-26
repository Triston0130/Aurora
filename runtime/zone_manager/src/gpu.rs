use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use crate::transfer::{DataTransferEngine, DeviceBuffer, HostBuffer};

#[derive(Debug)]
pub enum GpuError {
    KernelFailure(String),
}

impl std::fmt::Display for GpuError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GpuError::KernelFailure(msg) => write!(f, "kernel execution failed: {msg}"),
        }
    }
}

impl std::error::Error for GpuError {}

#[derive(Default)]
struct GpuMetrics {
    submitted: AtomicUsize,
    completed: AtomicUsize,
}

#[derive(Clone)]
pub struct GpuExecutor {
    transfers: DataTransferEngine,
    metrics: Arc<GpuMetrics>,
}

impl GpuExecutor {
    pub fn new(transfers: DataTransferEngine) -> Self {
        Self {
            transfers,
            metrics: Arc::new(GpuMetrics::default()),
        }
    }

    pub fn submit<T, F>(
        &self,
        name: impl Into<Arc<str>>,
        input: HostBuffer<T>,
        kernel: F,
    ) -> GpuJobHandle<T>
    where
        T: Clone + Send + Sync + 'static,
        F: Fn(&[T]) -> Result<Vec<T>, String> + Send + Sync + 'static,
    {
        self.metrics.submitted.fetch_add(1, Ordering::SeqCst);
        let transfers = self.transfers.clone();
        let name = name.into();
        let (tx, rx) = crossbeam_channel::bounded(1);
        let kernel = Arc::new(kernel);

        let worker = thread::spawn(move || {
            let start_snapshot = transfers.snapshot();
            let device_in: DeviceBuffer<T> = transfers.host_to_device(&input);
            let start = Instant::now();
            let result = (kernel)(device_in.read().as_slice());
            match result {
                Ok(result_vec) => {
                    let duration = start.elapsed();
                    let device_out = DeviceBuffer::from_vec(result_vec);
                    let host_out = transfers.device_to_host(&device_out);
                    let end_snapshot = transfers.snapshot();
                    let _ = tx.send(Ok(GpuExecution {
                        name,
                        duration,
                        bytes_moved: end_snapshot
                            .bytes_moved
                            .saturating_sub(start_snapshot.bytes_moved),
                        output: host_out,
                    }));
                }
                Err(message) => {
                    let _ = tx.send(Err(GpuError::KernelFailure(message)));
                }
            }
        });

        GpuJobHandle {
            receiver: rx,
            join: Mutex::new(Some(worker)),
            metrics: self.metrics.clone(),
        }
    }

    pub fn stats(&self) -> (usize, usize) {
        (
            self.metrics.submitted.load(Ordering::SeqCst),
            self.metrics.completed.load(Ordering::SeqCst),
        )
    }
}

pub struct GpuExecution<T> {
    pub name: Arc<str>,
    pub duration: Duration,
    pub bytes_moved: usize,
    pub output: HostBuffer<T>,
}

pub struct GpuJobHandle<T> {
    receiver: crossbeam_channel::Receiver<Result<GpuExecution<T>, GpuError>>,
    join: Mutex<Option<thread::JoinHandle<()>>>,
    metrics: Arc<GpuMetrics>,
}

impl<T> GpuJobHandle<T> {
    pub fn wait(self) -> Result<GpuExecution<T>, GpuError> {
        if let Some(handle) = self.join.lock().unwrap().take() {
            let _ = handle.join();
        }
        let result = self
            .receiver
            .recv()
            .expect("gpu worker must send completion result");
        if result.is_ok() {
            self.metrics.completed.fetch_add(1, Ordering::SeqCst);
        }
        result
    }
}
