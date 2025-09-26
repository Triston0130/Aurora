use crate::region::Region;
use parking_lot::Mutex;
use std::sync::{Arc, Weak};
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct CycleCollectorConfig {
    pub sweep_interval: Duration,
    pub max_regions: usize,
}

impl Default for CycleCollectorConfig {
    fn default() -> Self {
        Self {
            sweep_interval: Duration::from_millis(50),
            max_regions: 1024,
        }
    }
}

#[derive(Clone)]
pub struct CycleCollector {
    config: CycleCollectorConfig,
    regions: Arc<Mutex<Vec<Weak<crate::region::RegionInner>>>>,
}

impl CycleCollector {
    pub fn new(config: CycleCollectorConfig) -> Self {
        Self {
            config,
            regions: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn register_region(&self, region: &Region) {
        let mut regions = self.regions.lock();
        if regions.len() >= self.config.max_regions {
            regions.remove(0);
        }
        regions.push(Arc::downgrade(&region.inner));
    }

    pub fn collect(&self) -> usize {
        let mut reclaimed = 0;
        let mut regions = self.regions.lock();
        regions.retain(|weak| {
            if let Some(region) = weak.upgrade() {
                reclaimed += region.reclaim_orphans();
                true
            } else {
                false
            }
        });
        reclaimed
    }
}

impl Default for CycleCollector {
    fn default() -> Self {
        Self::new(CycleCollectorConfig::default())
    }
}
