use crate::region::{Region, RegionHandle};
use crate::transfer::{OwnershipToken, ZoneTransferEngine};
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum HandoffError {
    SourceDisposed,
    TargetDisposed,
    ValueAlreadyMoved,
    InternalInvariant,
}

impl fmt::Display for HandoffError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HandoffError::SourceDisposed => write!(f, "source region disposed during handoff"),
            HandoffError::TargetDisposed => write!(f, "target region disposed during handoff"),
            HandoffError::ValueAlreadyMoved => {
                write!(f, "value already moved out of source region")
            }
            HandoffError::InternalInvariant => {
                write!(f, "internal zone handoff invariant violated")
            }
        }
    }
}

impl std::error::Error for HandoffError {}

#[derive(Clone, Default)]
pub struct ZoneHandoff {
    engine: ZoneTransferEngine,
}

impl ZoneHandoff {
    pub fn new(engine: ZoneTransferEngine) -> Self {
        Self { engine }
    }

    pub fn move_handle<T: Send + Sync + 'static>(
        &self,
        handle: RegionHandle<T>,
        target: &Region,
    ) -> Result<RegionHandle<T>, HandoffError> {
        let token = self.engine.extract(handle)?;
        self.engine.deposit(token, target)
    }

    pub fn transfer_with<T, F>(
        &self,
        handle: RegionHandle<T>,
        target: &Region,
        f: F,
    ) -> Result<RegionHandle<T>, HandoffError>
    where
        T: Send + Sync + 'static,
        F: FnOnce(&mut T),
    {
        let token = self.engine.extract(handle)?;
        let source = token.source_region();
        let mut value = token.into_inner();
        f(&mut value);
        self.engine
            .deposit(OwnershipToken::new(source, value), target)
    }

    pub fn engine(&self) -> ZoneTransferEngine {
        self.engine.clone()
    }
}
