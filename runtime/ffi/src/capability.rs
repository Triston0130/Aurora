use aurora_runtime_scheduler::Zone;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FfiCapability {
    DynamicLibrary,
    FunctionCall,
}

#[derive(Clone, Default)]
pub struct CapabilitySet {
    allowed: HashSet<FfiCapability>,
}

impl CapabilitySet {
    pub fn allow(&mut self, capability: FfiCapability) {
        self.allowed.insert(capability);
    }

    pub fn revoke(&mut self, capability: &FfiCapability) {
        self.allowed.remove(capability);
    }

    pub fn contains(&self, capability: &FfiCapability) -> bool {
        self.allowed.contains(capability)
    }
}

#[derive(Clone, Default)]
pub struct ZoneCapabilityMap {
    map: std::collections::HashMap<String, CapabilitySet>,
}

impl ZoneCapabilityMap {
    pub fn allow(&mut self, zone: &Zone, capability: FfiCapability) {
        self.map
            .entry(zone.as_str().to_string())
            .or_default()
            .allow(capability);
    }

    pub fn revoke(&mut self, zone: &Zone, capability: &FfiCapability) {
        if let Some(set) = self.map.get_mut(zone.as_str()) {
            set.revoke(capability);
        }
    }

    pub fn contains(&self, zone: &Zone, capability: &FfiCapability) -> bool {
        self.map
            .get(zone.as_str())
            .map(|set| set.contains(capability))
            .unwrap_or(false)
    }
}
