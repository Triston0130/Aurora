use crate::effects::IoEffect;
use crate::error::IoResult;
use aurora_runtime_scheduler::{Zone, ZoneGuard};
use crossbeam_channel::Receiver;
use std::sync::Mutex;
use std::thread::{self, JoinHandle};

pub struct IoTask<T> {
    effect: IoEffect,
    zone: Zone,
    receiver: Receiver<IoResult<T>>,
    join: Mutex<Option<JoinHandle<()>>>,
}

impl<T> IoTask<T> {
    pub fn new(
        effect: IoEffect,
        zone: Zone,
        receiver: Receiver<IoResult<T>>,
        join: JoinHandle<()>,
    ) -> Self {
        Self {
            effect,
            zone,
            receiver,
            join: Mutex::new(Some(join)),
        }
    }

    pub fn effect(&self) -> IoEffect {
        self.effect
    }

    pub fn zone(&self) -> Zone {
        self.zone.clone()
    }

    pub fn wait(self) -> IoResult<T> {
        if let Some(handle) = self.join.lock().unwrap().take() {
            let _ = handle.join();
        }
        self.receiver
            .recv()
            .expect("io task worker must send a result")
    }
}

impl<T> Drop for IoTask<T> {
    fn drop(&mut self) {
        if let Some(handle) = self.join.lock().unwrap().take() {
            let _ = handle.join();
        }
    }
}

pub(crate) fn spawn_io_worker<F, T>(effect: IoEffect, zone: Zone, worker: F) -> IoTask<T>
where
    F: FnOnce() -> IoResult<T> + Send + 'static,
    T: Send + 'static,
{
    let (tx, rx) = crossbeam_channel::bounded(1);
    let zone_clone = zone.clone();
    let join = thread::Builder::new()
        .name(format!("aurora-io-{:?}", effect))
        .spawn(move || {
            let _guard = ZoneGuard::enter(zone_clone);
            let result = worker();
            let _ = tx.send(result);
        })
        .expect("failed to spawn io worker");

    IoTask::new(effect, zone, rx, join)
}
