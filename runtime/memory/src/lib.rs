pub mod handoff;
pub mod recycler;
pub mod region;
pub mod transfer;

pub use handoff::{HandoffError, ZoneHandoff};
pub use recycler::{CycleCollector, CycleCollectorConfig};
pub use region::{AllocationError, Region, RegionHandle, RegionId};
pub use transfer::{OwnershipToken, ZoneTransferEngine};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn handoff_moves_between_regions() {
        let source = Region::new("cpu-zone");
        let target = Region::new("gpu-zone");
        let handle = source.allocate(String::from("payload")).unwrap();

        let handoff = ZoneHandoff::new(ZoneTransferEngine::new());
        let new_handle = handoff
            .move_handle(handle, &target)
            .expect("handoff should succeed");

        let value = new_handle.with(|v| v.clone()).unwrap();
        assert_eq!(value, "payload");
        assert_eq!(new_handle.region_id(), target.id());
        assert!(target.reclaim_orphans() == 0);
    }

    #[test]
    fn cycle_collector_reclaims_orphans() {
        let region = Region::new("collector");
        let handle = region.allocate(vec![1, 2, 3]).unwrap();
        let _ = handle.try_take().unwrap();

        let collector = CycleCollector::default();
        collector.register_region(&region);
        let reclaimed = collector.collect();
        assert!(reclaimed >= 1);
    }
}
