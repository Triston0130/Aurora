use crate::handoff::HandoffError;
use crate::region::{AllocationError, Region, RegionHandle, RegionId};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

#[derive(Debug)]
pub struct OwnershipToken<T: Send + Sync + 'static> {
    source_region: RegionId,
    payload: T,
}

impl<T: Send + Sync + 'static> OwnershipToken<T> {
    pub(crate) fn new(source_region: RegionId, payload: T) -> Self {
        Self {
            source_region,
            payload,
        }
    }

    pub fn source_region(&self) -> RegionId {
        self.source_region
    }

    pub fn into_inner(self) -> T {
        self.payload
    }
}

#[derive(Default)]
struct TransferMetrics {
    extractions: AtomicUsize,
    deposits: AtomicUsize,
}

#[derive(Clone, Default)]
pub struct ZoneTransferEngine {
    metrics: Arc<TransferMetrics>,
}

impl ZoneTransferEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn extract<T: Send + Sync + 'static>(
        &self,
        handle: RegionHandle<T>,
    ) -> Result<OwnershipToken<T>, HandoffError> {
        self.metrics.extractions.fetch_add(1, Ordering::SeqCst);
        let region_id = handle.region_id();
        let value = handle.try_take().map_err(|err| match err {
            AllocationError::RegionDisposed => HandoffError::SourceDisposed,
            AllocationError::ValueMoved => HandoffError::ValueAlreadyMoved,
        })?;
        Ok(OwnershipToken::new(region_id, value))
    }

    pub fn deposit<T: Send + Sync + 'static>(
        &self,
        token: OwnershipToken<T>,
        target: &Region,
    ) -> Result<RegionHandle<T>, HandoffError> {
        self.metrics.deposits.fetch_add(1, Ordering::SeqCst);
        target
            .allocate(token.into_inner())
            .map_err(|err| match err {
                AllocationError::RegionDisposed => HandoffError::TargetDisposed,
                AllocationError::ValueMoved => HandoffError::InternalInvariant,
            })
    }

    pub fn metrics(&self) -> (usize, usize) {
        (
            self.metrics.extractions.load(Ordering::SeqCst),
            self.metrics.deposits.load(Ordering::SeqCst),
        )
    }
}
