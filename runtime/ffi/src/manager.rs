use crate::capability::{FfiCapability, ZoneCapabilityMap};
use crate::error::{FfiError, FfiResult};
use crate::library::ForeignLibrary;
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use parking_lot::RwLock;
use std::path::Path;
use std::sync::Arc;

#[derive(Clone)]
pub struct FfiManager {
    zone_manager: ZoneManager,
    capabilities: Arc<RwLock<ZoneCapabilityMap>>,
}

impl FfiManager {
    pub fn new(zone_manager: ZoneManager) -> Self {
        let mut map = ZoneCapabilityMap::default();
        map.allow(&Zone::cpu(), FfiCapability::DynamicLibrary);
        map.allow(&Zone::cpu(), FfiCapability::FunctionCall);
        Self {
            zone_manager,
            capabilities: Arc::new(RwLock::new(map)),
        }
    }

    pub fn permit(&self, zone: Zone, capability: FfiCapability) {
        self.capabilities.write().allow(&zone, capability);
    }

    pub fn revoke(&self, zone: &Zone, capability: &FfiCapability) {
        self.capabilities.write().revoke(zone, capability);
    }

    fn ensure_allowed(&self, zone: &Zone, capability: FfiCapability) -> FfiResult<()> {
        if self.capabilities.read().contains(zone, &capability) {
            Ok(())
        } else {
            Err(FfiError::ZoneDenied { zone: zone.clone() })
        }
    }

    pub fn load_library(&self, zone: Zone, path: impl AsRef<Path>) -> FfiResult<ForeignLibrary> {
        self.ensure_allowed(&zone, FfiCapability::DynamicLibrary)?;
        ForeignLibrary::from_path(zone, path)
    }

    pub fn load_self(&self, zone: Zone) -> FfiResult<ForeignLibrary> {
        self.ensure_allowed(&zone, FfiCapability::DynamicLibrary)?;
        ForeignLibrary::this_process(zone)
    }

    pub fn zone_manager(&self) -> ZoneManager {
        self.zone_manager.clone()
    }
}
