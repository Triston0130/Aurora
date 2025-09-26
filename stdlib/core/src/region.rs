use aurora_runtime_memory::{Region, RegionHandle, ZoneHandoff};
use std::fmt;
use std::sync::Arc;

pub struct RegionScope {
    region: Region,
    handoff: ZoneHandoff,
}

impl RegionScope {
    pub fn new(name: impl Into<Arc<str>>, handoff: ZoneHandoff) -> Self {
        Self {
            region: Region::new(name),
            handoff,
        }
    }

    pub fn region(&self) -> &Region {
        &self.region
    }

    pub fn handoff(&self) -> ZoneHandoff {
        self.handoff.clone()
    }
}

impl Drop for RegionScope {
    fn drop(&mut self) {
        self.region.dispose();
    }
}

#[derive(Clone)]
pub struct RegionOwned<T: Send + Sync + 'static> {
    handle: RegionHandle<T>,
}

impl<T: Send + Sync + 'static> RegionOwned<T> {
    pub fn new(region: &Region, value: T) -> Self {
        let handle = region.allocate(value).expect("allocate region value");
        Self { handle }
    }

    pub fn handle(&self) -> &RegionHandle<T> {
        &self.handle
    }

    pub fn transfer(self, target: &Region, handoff: &ZoneHandoff) -> RegionHandle<T> {
        handoff
            .move_handle(self.handle, target)
            .expect("region transfer")
    }

    pub fn into_handle(self) -> RegionHandle<T> {
        self.handle
    }
}

pub trait RegionHandleExt<T: Send + Sync + 'static> {
    fn move_to(self, target: &Region, handoff: &ZoneHandoff) -> RegionHandle<T>;
}

impl<T: Send + Sync + 'static> RegionHandleExt<T> for RegionHandle<T> {
    fn move_to(self, target: &Region, handoff: &ZoneHandoff) -> RegionHandle<T> {
        handoff
            .move_handle(self, target)
            .expect("region handoff succeeds")
    }
}

impl fmt::Debug for RegionScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RegionScope")
            .field("region_id", &self.region.id())
            .finish()
    }
}
