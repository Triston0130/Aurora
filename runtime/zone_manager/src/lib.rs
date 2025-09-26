//! Zone manager built on top of the Aurora runtime scheduler. Provides zone-aware
//! supervisor provisioning and bridges asynchronous I/O helpers (timers, custom
//! reactors) into the core scheduler infrastructure.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

pub use aurora_runtime_memory::{
    AllocationError as RegionAllocationError, CycleCollector, CycleCollectorConfig,
    HandoffError as RegionHandoffError, OwnershipToken as RegionOwnershipToken, Region,
    RegionHandle, RegionId, ZoneHandoff, ZoneTransferEngine as RegionTransferEngine,
};
pub use aurora_runtime_scheduler::{
    Actor, ActorConfig, ActorDirective, ActorExitStatus, ActorHandle, Message, Scheduler,
    SendError, SpawnError, SupervisorEvents, SupervisorHandle, Zone, ZoneGuard, ZonePolicy,
};

mod gpu;
mod io;
mod realtime;
mod sandbox;
mod transfer;

pub use gpu::{GpuError, GpuExecution, GpuExecutor, GpuJobHandle};
pub use io::{IoDispatcher, TimerHandle};
pub use realtime::{RealtimeExecutor, RealtimeOutcome, RealtimeStatus, RealtimeTaskHandle};
pub use sandbox::{
    SandboxCapability, SandboxContext, SandboxManager, SandboxOutcome, SandboxPolicy, SandboxStatus,
};
pub use transfer::{DataTransferEngine, DataTransferSnapshot, DeviceBuffer, HostBuffer};

#[derive(Clone)]
struct Executors {
    transfers: DataTransferEngine,
    gpu: GpuExecutor,
    realtime: RealtimeExecutor,
    sandbox: SandboxManager,
    region_transfer: RegionTransferEngine,
    cycle_collector: CycleCollector,
}

struct ZoneManagerInner {
    scheduler: Scheduler,
    root: SupervisorHandle,
    supervisors: Mutex<HashMap<Zone, SupervisorHandle>>,
    executors: Executors,
}

impl ZoneManagerInner {
    fn new(policy: Option<ZonePolicy>) -> Self {
        let scheduler = policy.map(Scheduler::with_policy).unwrap_or_default();
        let (root, _events) = scheduler.root_supervisor("root", Zone::cpu());
        let mut supervisors = HashMap::new();
        supervisors.insert(Zone::cpu(), root.clone());
        let transfers = DataTransferEngine::new();
        let executors = Executors {
            gpu: GpuExecutor::new(transfers.clone()),
            realtime: RealtimeExecutor::new(),
            sandbox: SandboxManager::new(),
            transfers: transfers.clone(),
            region_transfer: RegionTransferEngine::new(),
            cycle_collector: CycleCollector::new(CycleCollectorConfig::default()),
        };
        Self {
            scheduler,
            root,
            supervisors: Mutex::new(supervisors),
            executors,
        }
    }

    fn ensure_supervisor(&self, zone: &Zone) -> SupervisorHandle {
        if zone == &Zone::cpu() {
            return self.root.clone();
        }
        let mut supervisors = self.supervisors.lock().unwrap();
        if let Some(existing) = supervisors.get(zone) {
            return existing.clone();
        }
        let handle = self
            .root
            .create_child(format!("{}-supervisor", zone.as_str()), zone.clone());
        supervisors.insert(zone.clone(), handle.clone());
        handle
    }
}

/// High-level zone manager orchestrating supervisors and providing hooks for
/// asynchronous subsystems to interact with the scheduler.
#[derive(Clone)]
pub struct ZoneManager {
    inner: Arc<ZoneManagerInner>,
}

impl ZoneManager {
    /// Creates a zone manager with the scheduler's default zone policy.
    pub fn new() -> Self {
        Self {
            inner: Arc::new(ZoneManagerInner::new(None)),
        }
    }

    /// Creates a zone manager using a custom zone policy.
    pub fn with_policy(policy: ZonePolicy) -> Self {
        Self {
            inner: Arc::new(ZoneManagerInner::new(Some(policy))),
        }
    }

    /// Returns a handle to the underlying scheduler.
    pub fn scheduler(&self) -> &Scheduler {
        &self.inner.scheduler
    }

    /// Retrieves (and creates if needed) the supervisor for the given zone.
    pub fn supervisor(&self, zone: Zone) -> SupervisorHandle {
        self.inner.ensure_supervisor(&zone)
    }

    /// Subscribes to supervision events for a zone. Each call returns a fresh
    /// receiver.
    pub fn subscribe(&self, zone: Zone) -> SupervisorEvents {
        self.supervisor(zone).subscribe()
    }

    /// Spawns an actor using the provided configuration and handler.
    pub fn spawn_actor_with<F>(&self, config: ActorConfig, handler: F) -> Result<Actor, SpawnError>
    where
        F: FnMut(&mut aurora_runtime_scheduler::ActorContext, Message) -> ActorDirective
            + Send
            + 'static,
    {
        let supervisor = self.inner.ensure_supervisor(&config.zone);
        supervisor.spawn_actor_with(config, handler)
    }

    /// Spawns an actor with the default mailbox capacity.
    pub fn spawn_actor<F>(
        &self,
        name: impl Into<Arc<str>>,
        zone: Zone,
        handler: F,
    ) -> Result<Actor, SpawnError>
    where
        F: FnMut(&mut aurora_runtime_scheduler::ActorContext, Message) -> ActorDirective
            + Send
            + 'static,
    {
        let config = ActorConfig::new(name, zone.clone());
        self.spawn_actor_with(config, handler)
    }

    /// Spawns a default actor provided by the scheduler library.
    pub fn spawn_default_actor(
        &self,
        name: impl Into<Arc<str>>,
        zone: Zone,
    ) -> Result<Actor, SpawnError> {
        let supervisor = self.inner.ensure_supervisor(&zone);
        supervisor.spawn_default_actor(name, zone)
    }

    /// Provides a dispatcher that can be shared with async I/O primitives to
    /// spawn actors and deliver messages while automatically respecting zone
    /// guards.
    pub fn io_dispatcher(&self, zone: Zone) -> IoDispatcher {
        IoDispatcher::new(Arc::clone(&self.inner), zone)
    }

    /// Sends a message to an actor while entering the appropriate zone guard.
    pub fn dispatch_to_actor(
        &self,
        zone: Zone,
        handle: &ActorHandle,
        message: Message,
    ) -> Result<(), SendError> {
        let dispatcher = self.io_dispatcher(zone);
        dispatcher.send(handle, message)
    }

    /// Schedules a one-shot timer that dispatches a message to the target actor
    /// after the provided delay.
    pub fn schedule_timeout(
        &self,
        zone: Zone,
        delay: Duration,
        target: ActorHandle,
        message: Message,
    ) -> TimerHandle {
        self.io_dispatcher(zone)
            .schedule_timeout(delay, target, message)
    }

    /// Schedules a repeating interval timer that keeps dispatching the message
    /// until cancelled.
    pub fn schedule_interval(
        &self,
        zone: Zone,
        period: Duration,
        target: ActorHandle,
        message: Message,
    ) -> TimerHandle {
        self.io_dispatcher(zone)
            .schedule_interval(period, target, message)
    }

    /// Returns the shared data transfer engine used for host/device staging.
    pub fn transfers(&self) -> DataTransferEngine {
        self.inner.executors.transfers.clone()
    }

    /// Returns the GPU executor responsible for kernel offload.
    pub fn gpu(&self) -> GpuExecutor {
        self.inner.executors.gpu.clone()
    }

    /// Returns the realtime executor enforcing deadline budgets.
    pub fn realtime(&self) -> RealtimeExecutor {
        self.inner.executors.realtime.clone()
    }

    /// Returns the sandbox manager enforcing capability policies.
    pub fn sandbox(&self) -> SandboxManager {
        self.inner.executors.sandbox.clone()
    }

    /// Returns the region ownership transfer engine for moving allocations between zones.
    pub fn region_transfer(&self) -> RegionTransferEngine {
        self.inner.executors.region_transfer.clone()
    }

    /// Creates a zone handoff helper for transferring region-backed payloads.
    pub fn region_handoff(&self) -> ZoneHandoff {
        ZoneHandoff::new(self.inner.executors.region_transfer.clone())
    }

    /// Returns the shared cycle collector used to reclaim orphaned allocations.
    pub fn cycle_collector(&self) -> CycleCollector {
        self.inner.executors.cycle_collector.clone()
    }

    /// Registers a region with the shared cycle collector.
    pub fn register_region(&self, region: &Region) {
        self.inner.executors.cycle_collector.register_region(region);
    }
}

impl Default for ZoneManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aurora_runtime_scheduler::ActorExitStatus;
    use crossbeam_channel::{bounded, TryRecvError};
    use std::collections::HashSet;
    use std::mem;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn spawns_gpu_actor_via_zone_manager() {
        let manager = ZoneManager::new();
        let (tx, rx) = bounded::<String>(1);
        let sender = tx.clone();

        let gpu_supervisor = manager.supervisor(Zone::gpu());
        let events = gpu_supervisor.subscribe();

        let actor = manager
            .spawn_actor("gpu-worker", Zone::gpu(), move |_, msg| match msg {
                Message::Work(payload) => {
                    sender.send(payload).unwrap();
                    ActorDirective::Stop
                }
                Message::Shutdown => ActorDirective::Stop,
                Message::Fail(reason) => ActorDirective::Fail(reason),
            })
            .expect("spawn gpu actor");

        let deadline = std::time::Instant::now() + Duration::from_secs(1);
        loop {
            match events.try_recv() {
                Ok(aurora_runtime_scheduler::SupervisorEvent::ActorStarted { .. }) => break,
                Ok(_) => continue,
                Err(TryRecvError::Empty) => {
                    if std::time::Instant::now() > deadline {
                        panic!("timed out waiting for actor start");
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(TryRecvError::Disconnected) => panic!("supervisor events disconnected"),
            }
        }

        manager
            .dispatch_to_actor(Zone::gpu(), &actor.handle(), Message::Work("task".into()))
            .expect("dispatch work");

        let received = rx.recv_timeout(Duration::from_secs(1)).expect("work");
        assert_eq!(received, "task");

        let status = actor.join().expect("join");
        assert_eq!(status, ActorExitStatus::Completed);
    }

    #[test]
    fn timer_dispatches_message() {
        let manager = ZoneManager::new();
        let (tx, rx) = bounded::<String>(1);
        let sender = tx.clone();

        let actor = manager
            .spawn_actor("timer-target", Zone::gpu(), move |_, msg| match msg {
                Message::Work(payload) => {
                    sender.send(payload).unwrap();
                    ActorDirective::Stop
                }
                Message::Shutdown => ActorDirective::Stop,
                Message::Fail(reason) => ActorDirective::Fail(reason),
            })
            .expect("spawn actor");

        let timer = manager.schedule_timeout(
            Zone::gpu(),
            Duration::from_millis(50),
            actor.handle(),
            Message::Work("tick".into()),
        );

        let received = rx
            .recv_timeout(Duration::from_secs(1))
            .expect("timer message");
        assert_eq!(received, "tick");

        let timer_status = timer.join().expect("join timer");
        assert_eq!(timer_status, ActorExitStatus::Completed);
        let status = actor.join().expect("join");
        assert_eq!(status, ActorExitStatus::Completed);
    }

    #[test]
    fn interval_timer_repeats_until_cancelled() {
        let manager = ZoneManager::new();
        let (tx, rx) = bounded::<String>(3);
        let sender = tx.clone();

        let actor = manager
            .spawn_actor("interval-target", Zone::gpu(), move |_, msg| match msg {
                Message::Work(payload) => {
                    sender.send(payload).unwrap();
                    ActorDirective::Continue
                }
                Message::Shutdown => ActorDirective::Stop,
                Message::Fail(reason) => ActorDirective::Fail(reason),
            })
            .expect("spawn interval actor");

        let timer = manager.schedule_interval(
            Zone::gpu(),
            Duration::from_millis(25),
            actor.handle(),
            Message::Work("tick".into()),
        );

        let mut ticks = Vec::new();
        for _ in 0..3 {
            let tick = rx
                .recv_timeout(Duration::from_secs(1))
                .expect("interval tick");
            ticks.push(tick);
        }
        let expected = vec!["tick".to_string(), "tick".to_string(), "tick".to_string()];
        assert_eq!(ticks, expected);

        timer.cancel();
        let timer_status = timer.join().expect("join interval timer");
        assert_eq!(timer_status, ActorExitStatus::Completed);

        manager
            .dispatch_to_actor(Zone::gpu(), &actor.handle(), Message::Shutdown)
            .expect("shutdown interval actor");
        let status = actor.join().expect("join interval actor");
        assert_eq!(status, ActorExitStatus::Completed);
    }

    #[test]
    fn gpu_offload_transfers_and_computes() {
        let manager = ZoneManager::new();
        let input = HostBuffer::from(vec![1.0_f32, 2.0, 3.0, 4.0]);
        let gpu = manager.gpu();

        let job = gpu.submit("double", input.clone(), |data| {
            Ok(data.iter().map(|value| value * 2.0).collect())
        });
        let result = job.wait().expect("gpu execution");
        assert_eq!(result.output.as_slice(), &[2.0, 4.0, 6.0, 8.0]);
        assert!(result.bytes_moved >= input.len() * mem::size_of::<f32>() * 2);

        let (submitted, completed) = gpu.stats();
        assert_eq!(submitted, 1);
        assert_eq!(completed, 1);
    }

    #[test]
    fn realtime_executor_enforces_deadline() {
        let manager = ZoneManager::new();
        let realtime = manager.realtime();

        let fast = realtime.schedule("fast", Duration::from_millis(10), || {});
        let outcome = fast.wait();
        assert_eq!(outcome.status, RealtimeStatus::Completed);

        let slow = realtime.schedule("slow", Duration::from_millis(5), || {
            thread::sleep(Duration::from_millis(20));
        });
        let outcome = slow.wait();
        assert!(matches!(outcome.status, RealtimeStatus::DeadlineMissed(_)));

        let (scheduled, completed, missed) = realtime.metrics();
        assert_eq!(scheduled, 2);
        assert_eq!(completed, 1);
        assert_eq!(missed, 1);
    }

    #[test]
    fn sandbox_policy_restricts_capabilities() {
        let manager = ZoneManager::new();
        let sandbox = manager.sandbox();

        let mut allowed = HashSet::new();
        allowed.insert(SandboxCapability::FileSystem);
        let policy = SandboxPolicy::new(allowed.clone());

        let outcome = sandbox.run(policy.clone(), &[SandboxCapability::FileSystem], |_| "ok");
        assert_eq!(outcome.status, SandboxStatus::Completed);
        assert_eq!(outcome.result, Some("ok"));

        let denied = sandbox.run(policy, &[SandboxCapability::Network], |_| "nope");
        assert_eq!(
            denied.status,
            SandboxStatus::CapabilityDenied(SandboxCapability::Network)
        );
        assert!(denied.result.is_none());
    }

    #[test]
    fn transfer_engine_records_metrics() {
        let manager = ZoneManager::new();
        let transfers = manager.transfers();
        let before = transfers.snapshot();

        let host = HostBuffer::from(vec![1_u32, 2, 3, 4]);
        let device = transfers.host_to_device(&host);
        let _ = transfers.device_to_host(&device);

        let after = transfers.snapshot();
        assert!(after.bytes_moved > before.bytes_moved);
        assert!(after.transfers >= before.transfers + 2);
    }

    #[test]
    fn region_handoff_moves_between_zones() {
        let manager = ZoneManager::new();
        let source_region = Region::new("cpu-region");
        let target_region = Region::new("gpu-region");
        manager.register_region(&source_region);
        manager.register_region(&target_region);

        let handle = source_region
            .allocate(String::from("frame-buffer"))
            .expect("allocate region value");

        let handoff = manager.region_handoff();
        let transferred = handoff
            .move_handle(handle, &target_region)
            .expect("handoff succeeds");
        let value = transferred.with(|v| v.clone()).expect("read value");
        assert_eq!(value, "frame-buffer");
        assert_eq!(transferred.region_id(), target_region.id());

        // Source region should now contain an orphan that the collector can reclaim.
        let reclaimed = manager.cycle_collector().collect();
        assert!(reclaimed >= 1);
    }
}
