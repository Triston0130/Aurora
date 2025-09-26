use parking_lot::{Mutex, RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::fmt;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;

static NEXT_REGION_ID: AtomicU64 = AtomicU64::new(1);

pub type RegionId = u64;

pub(crate) trait ErasedAllocation: Send + Sync {
    fn clear(&self);
    fn is_cleared(&self) -> bool;
}

struct Allocation<T: Send + Sync + 'static> {
    data: RwLock<Option<T>>,
}

impl<T: Send + Sync + 'static> Allocation<T> {
    fn new(value: T) -> Self {
        Self {
            data: RwLock::new(Some(value)),
        }
    }

    fn read(&self) -> RwLockReadGuard<'_, Option<T>> {
        self.data.read()
    }

    fn write(&self) -> RwLockWriteGuard<'_, Option<T>> {
        self.data.write()
    }

    fn has_value(&self) -> bool {
        self.data.read().is_some()
    }
}

impl<T: Send + Sync + 'static> ErasedAllocation for Allocation<T> {
    fn clear(&self) {
        *self.data.write() = None;
    }

    fn is_cleared(&self) -> bool {
        self.data.read().is_none()
    }
}

pub(crate) struct RegionInner {
    id: RegionId,
    name: Arc<str>,
    allocations: Mutex<Vec<Arc<dyn ErasedAllocation>>>,
    disposed: AtomicBool,
}

impl RegionInner {
    fn new(name: impl Into<Arc<str>>) -> Self {
        Self {
            id: NEXT_REGION_ID.fetch_add(1, Ordering::SeqCst),
            name: name.into(),
            allocations: Mutex::new(Vec::new()),
            disposed: AtomicBool::new(false),
        }
    }

    fn register_allocation(&self, allocation: Arc<dyn ErasedAllocation>) {
        self.allocations.lock().push(allocation);
    }

    pub(crate) fn dispose(&self) {
        if self
            .disposed
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {
            return;
        }
        let allocations = self.allocations.lock();
        for allocation in allocations.iter() {
            allocation.clear();
        }
    }

    pub(crate) fn is_disposed(&self) -> bool {
        self.disposed.load(Ordering::SeqCst)
    }

    pub(crate) fn reclaim_orphans(&self) -> usize {
        let mut allocations = self.allocations.lock();
        let before = allocations.len();
        allocations.retain(|allocation| !allocation.is_cleared());
        before - allocations.len()
    }
}

impl Drop for RegionInner {
    fn drop(&mut self) {
        self.dispose();
    }
}

#[derive(Clone)]
pub struct Region {
    pub(crate) inner: Arc<RegionInner>,
}

impl Region {
    pub fn new(name: impl Into<Arc<str>>) -> Self {
        Self {
            inner: Arc::new(RegionInner::new(name)),
        }
    }

    pub fn id(&self) -> RegionId {
        self.inner.id
    }

    pub fn name(&self) -> &str {
        &self.inner.name
    }

    pub fn allocate<T>(&self, value: T) -> Result<RegionHandle<T>, AllocationError>
    where
        T: Send + Sync + 'static,
    {
        if self.inner.is_disposed() {
            return Err(AllocationError::RegionDisposed);
        }
        let allocation = Arc::new(Allocation::new(value));
        self.inner.register_allocation(allocation.clone());
        Ok(RegionHandle {
            region: self.inner.clone(),
            allocation,
        })
    }

    pub fn is_disposed(&self) -> bool {
        self.inner.is_disposed()
    }

    pub fn dispose(&self) {
        self.inner.dispose();
    }

    pub fn reclaim_orphans(&self) -> usize {
        self.inner.reclaim_orphans()
    }
}

#[derive(Clone)]
pub struct RegionHandle<T: Send + Sync + 'static> {
    region: Arc<RegionInner>,
    allocation: Arc<Allocation<T>>,
}

impl<T: Send + Sync + 'static> RegionHandle<T> {
    pub fn region_id(&self) -> RegionId {
        self.region.id
    }

    pub fn region_name(&self) -> &str {
        &self.region.name
    }

    fn ensure_active(&self) -> Result<(), AllocationError> {
        if self.region.is_disposed() {
            return Err(AllocationError::RegionDisposed);
        }
        Ok(())
    }

    pub fn with<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, AllocationError> {
        self.ensure_active()?;
        let guard = self.allocation.read();
        guard.as_ref().map(f).ok_or(AllocationError::ValueMoved)
    }

    pub fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, AllocationError> {
        self.ensure_active()?;
        let mut guard = self.allocation.write();
        guard.as_mut().map(f).ok_or(AllocationError::ValueMoved)
    }

    pub fn try_take(&self) -> Result<T, AllocationError> {
        self.ensure_active()?;
        let mut guard = self.allocation.write();
        guard.take().ok_or(AllocationError::ValueMoved)
    }

    pub fn is_available(&self) -> bool {
        !self.region.is_disposed() && self.allocation.has_value()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AllocationError {
    RegionDisposed,
    ValueMoved,
}

impl fmt::Display for AllocationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AllocationError::RegionDisposed => write!(f, "region has been disposed"),
            AllocationError::ValueMoved => write!(f, "value already moved out of region"),
        }
    }
}

impl std::error::Error for AllocationError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn region_allocates_and_reads() {
        let region = Region::new("test");
        let handle = region.allocate(42_u32).expect("allocate");
        let value = handle.with(|v| *v).expect("read");
        assert_eq!(value, 42);

        handle.with_mut(|v| *v = 100).expect("mutate");
        assert_eq!(handle.with(|v| *v).unwrap(), 100);
    }

    #[test]
    fn taking_value_moves_out_of_region() {
        let region = Region::new("test");
        let handle = region.allocate(String::from("hello")).unwrap();
        let taken = handle.try_take().unwrap();
        assert_eq!(taken, "hello");
        assert_eq!(handle.try_take().unwrap_err(), AllocationError::ValueMoved);
        assert_eq!(region.reclaim_orphans(), 1);
    }

    #[test]
    fn disposing_region_clears_allocations() {
        let region = Region::new("dispose");
        let handle = region.allocate(10_u8).unwrap();
        assert!(handle.is_available());
        region.dispose();
        assert!(handle.with(|_| ()).is_err());
        assert!(region.is_disposed());
    }
}
