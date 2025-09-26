use crate::errors::{AurError, AurStdResult};
use aurora_runtime_io::{IoTask, TimerService};
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use std::time::Duration;

pub struct AurTask<T> {
    inner: IoTask<T>,
}

impl<T> AurTask<T> {
    pub fn new(inner: IoTask<T>) -> Self {
        Self { inner }
    }

    pub fn wait(self) -> AurStdResult<T> {
        self.inner.wait().map_err(AurError::from_io).into()
    }
}

pub struct TimerFuture {
    task: IoTask<()>,
}

impl TimerFuture {
    pub fn new(timer: TimerService, duration: Duration) -> Self {
        Self {
            task: timer.sleep(duration),
        }
    }

    pub fn wait(self) -> AurStdResult<()> {
        self.task.wait().map_err(AurError::from_io).into()
    }
}

pub fn timer_service(zone_manager: ZoneManager, zone: Zone) -> TimerService {
    TimerService::new(zone_manager, zone)
}
