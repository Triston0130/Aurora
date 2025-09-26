use crate::error::{FfiError, FfiResult};
use aurora_runtime_scheduler::Zone;
use libloading::Library;
use std::marker::PhantomData;
use std::sync::Arc;

pub struct ForeignFunction<T> {
    zone: Zone,
    library: Arc<Library>,
    symbol: Vec<u8>,
    _marker: PhantomData<T>,
}

impl<T> ForeignFunction<T> {
    pub(crate) fn new(zone: Zone, library: Arc<Library>, symbol: Vec<u8>) -> Self {
        Self {
            zone,
            library,
            symbol,
            _marker: PhantomData,
        }
    }

    pub fn zone(&self) -> Zone {
        self.zone.clone()
    }

    /// # Safety
    ///
    /// Caller must ensure the underlying symbol adheres to the signature `T` and
    /// that any invariants enforced by the foreign library are upheld while the
    /// function pointer is in use.
    pub unsafe fn call<R>(&self, f: impl FnOnce(&T) -> R) -> FfiResult<R>
    where
        T: Copy + Send + Sync + 'static,
    {
        let symbol: libloading::Symbol<T> =
            self.library
                .get(&self.symbol)
                .map_err(|_| FfiError::SymbolNotFound {
                    name: String::from_utf8_lossy(&self.symbol).into_owned(),
                })?;

        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&*symbol)))
            .map_err(|_| FfiError::InvocationPanicked)
    }
}
