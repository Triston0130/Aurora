use crate::effects::IoEffect;
use crate::error::IoResult;
use crate::task::{spawn_io_worker, IoTask};
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use std::time::Duration;

#[derive(Clone)]
pub struct TimerService {
    #[allow(dead_code)]
    manager: ZoneManager,
    zone: Zone,
}

impl TimerService {
    pub fn new(manager: ZoneManager, zone: Zone) -> Self {
        Self { manager, zone }
    }

    pub fn sleep(&self, duration: Duration) -> IoTask<()> {
        let zone = self.zone.clone();
        spawn_io_worker(IoEffect::Timer, zone, move || {
            std::thread::sleep(duration);
            Ok(()) as IoResult<()>
        })
    }
}
