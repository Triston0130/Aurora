use std::mem::size_of;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct HostBuffer<T> {
    data: Arc<Vec<T>>,
}

impl<T> HostBuffer<T> {
    pub fn new(data: Vec<T>) -> Self {
        Self {
            data: Arc::new(data),
        }
    }

    pub fn as_slice(&self) -> &[T] {
        &self.data
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[derive(Clone)]
pub struct DeviceBuffer<T> {
    data: Arc<Mutex<Vec<T>>>,
}

impl<T> DeviceBuffer<T> {
    pub fn from_vec(data: Vec<T>) -> Self {
        Self {
            data: Arc::new(Mutex::new(data)),
        }
    }

    pub fn len(&self) -> usize {
        self.data.lock().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.lock().unwrap().is_empty()
    }

    pub fn read(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.data.lock().unwrap().clone()
    }

    pub fn write(&self, values: Vec<T>) {
        *self.data.lock().unwrap() = values;
    }
}

#[derive(Default)]
struct DataTransferMetrics {
    bytes_moved: AtomicUsize,
    transfers: AtomicUsize,
}

#[derive(Default)]
pub struct DataTransferSnapshot {
    pub bytes_moved: usize,
    pub transfers: usize,
}

#[derive(Clone)]
pub struct DataTransferEngine {
    metrics: Arc<DataTransferMetrics>,
}

impl DataTransferEngine {
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(DataTransferMetrics::default()),
        }
    }

    pub fn host_to_device<T: Clone + Send + Sync + 'static>(
        &self,
        host: &HostBuffer<T>,
    ) -> DeviceBuffer<T> {
        self.record_transfer::<T>(host.len());
        DeviceBuffer::from_vec(host.as_slice().to_vec())
    }

    pub fn device_to_host<T: Clone + Send + Sync + 'static>(
        &self,
        device: &DeviceBuffer<T>,
    ) -> HostBuffer<T> {
        let data = device.read();
        self.record_transfer::<T>(data.len());
        HostBuffer::new(data)
    }

    pub fn copy_device_to_device<T: Clone + Send + Sync + 'static>(
        &self,
        src: &DeviceBuffer<T>,
        dst: &DeviceBuffer<T>,
    ) {
        let data = src.read();
        self.record_transfer::<T>(data.len());
        dst.write(data);
    }

    fn record_transfer<T>(&self, elements: usize) {
        let bytes = elements.saturating_mul(size_of::<T>());
        self.metrics.bytes_moved.fetch_add(bytes, Ordering::SeqCst);
        self.metrics.transfers.fetch_add(1, Ordering::SeqCst);
    }

    pub fn snapshot(&self) -> DataTransferSnapshot {
        DataTransferSnapshot {
            bytes_moved: self.metrics.bytes_moved.load(Ordering::SeqCst),
            transfers: self.metrics.transfers.load(Ordering::SeqCst),
        }
    }
}

impl Default for DataTransferEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> From<Vec<T>> for HostBuffer<T> {
    fn from(value: Vec<T>) -> Self {
        HostBuffer::new(value)
    }
}
