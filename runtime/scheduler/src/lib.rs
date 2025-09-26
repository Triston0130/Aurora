//! Aurora-native scheduler prototype implementing structured concurrency,
//! supervision trees, cancellation, and simple zone enforcement without
//! relying on Tokio.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use crossbeam_channel::{unbounded, Receiver, RecvTimeoutError, Sender};
use tracing::{debug, info, warn};

/// Identifier assigned to actors within a supervisor tree.
pub type ActorId = usize;

/// Identifier assigned to supervisors.
pub type SupervisorId = usize;

/// Message payload handled by the demo actors.
#[derive(Debug, Clone)]
pub enum Message {
    Work(String),
    Fail(String),
    Shutdown,
}

/// Zones describe the capability context an actor executes within.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Zone(Arc<str>);

impl Zone {
    pub fn new<S: Into<Arc<str>>>(name: S) -> Self {
        Self(name.into())
    }

    pub fn cpu() -> Self {
        Self::new("cpu")
    }

    pub fn gpu() -> Self {
        Self::new("gpu")
    }

    pub fn realtime() -> Self {
        Self::new("realtime")
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Zone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

// Thread-local zone context used for enforcement when sending messages.
thread_local! {
    static ZONE_STACK: std::cell::RefCell<Vec<Zone>> = std::cell::RefCell::new(vec![Zone::cpu()]);
}

/// RAII guard representing code executing within a particular zone scope.
pub struct ZoneGuard {
    previous_len: usize,
}

impl ZoneGuard {
    pub fn enter(zone: Zone) -> Self {
        let previous_len = ZONE_STACK.with(|stack| {
            let mut stack = stack.borrow_mut();
            let len = stack.len();
            stack.push(zone);
            len
        });
        Self { previous_len }
    }
}

impl Drop for ZoneGuard {
    fn drop(&mut self) {
        ZONE_STACK.with(|stack| {
            let mut stack = stack.borrow_mut();
            stack.truncate(self.previous_len);
        });
    }
}

fn current_zone() -> Zone {
    ZONE_STACK.with(|stack| stack.borrow().last().cloned().unwrap_or_else(Zone::cpu))
}

/// Policy describing which zones may host children within another zone.
#[derive(Clone)]
pub struct ZonePolicy {
    allowed: HashMap<String, HashSet<String>>,
}

impl ZonePolicy {
    pub fn can_host(&self, parent: &Zone, child: &Zone) -> bool {
        if parent == child {
            return true;
        }
        self.allowed
            .get(parent.as_str())
            .map(|set| set.contains(child.as_str()))
            .unwrap_or(false)
    }
}

impl Default for ZonePolicy {
    fn default() -> Self {
        let mut allowed = HashMap::new();
        // CPU supervisors may create CPU, GPU, or realtime actors.
        allowed
            .entry("cpu".to_string())
            .or_insert_with(HashSet::new)
            .extend(["gpu".to_string(), "realtime".to_string()]);
        // GPU supervisors restrict to GPU actors.
        allowed
            .entry("gpu".to_string())
            .or_insert_with(HashSet::new)
            .insert("gpu".to_string());
        // Realtime supervisors restrict to realtime actors.
        allowed
            .entry("realtime".to_string())
            .or_insert_with(HashSet::new)
            .insert("realtime".to_string());
        Self { allowed }
    }
}

/// Events emitted by the scheduler to supervisors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SupervisorEvent {
    ActorStarted {
        supervisor: SupervisorId,
        actor: ActorId,
        name: Arc<str>,
        zone: Zone,
    },
    ActorStopped {
        supervisor: SupervisorId,
        actor: ActorId,
        name: Arc<str>,
    },
    ActorFailed {
        supervisor: SupervisorId,
        actor: ActorId,
        name: Arc<str>,
        reason: String,
    },
    ActorCancelled {
        supervisor: SupervisorId,
        actor: ActorId,
        name: Arc<str>,
    },
    ZoneViolation {
        supervisor: SupervisorId,
        actor: ActorId,
        name: Arc<str>,
        expected: Zone,
        current: Zone,
    },
}

/// Result returned when joining an actor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActorExitStatus {
    Completed,
    Failed(String),
    Cancelled,
}

#[derive(Debug)]
pub enum ActorJoinError {
    AlreadyJoined,
    Panicked,
}

impl fmt::Display for ActorJoinError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ActorJoinError::AlreadyJoined => f.write_str("actor already joined"),
            ActorJoinError::Panicked => f.write_str("actor panicked"),
        }
    }
}

impl std::error::Error for ActorJoinError {}

#[derive(Debug)]
pub enum SpawnError {
    ZoneDenied {
        parent: Zone,
        requested: Zone,
        supervisor: Arc<str>,
        actor: Arc<str>,
    },
    MailboxCapacityZero,
}

impl fmt::Display for SpawnError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpawnError::ZoneDenied {
                parent,
                requested,
                supervisor,
                actor,
            } => write!(
                f,
                "supervisor `{supervisor}` with zone `{parent}` cannot spawn actor `{actor}` in zone `{requested}`"
            ),
            SpawnError::MailboxCapacityZero => f.write_str("mailbox capacity must be non-zero"),
        }
    }
}

impl std::error::Error for SpawnError {}

#[derive(Debug)]
pub enum SendError {
    MailboxClosed,
    ZoneMismatch { expected: Zone, current: Zone },
}

impl fmt::Display for SendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SendError::MailboxClosed => f.write_str("actor mailbox closed"),
            SendError::ZoneMismatch { expected, current } => write!(
                f,
                "message sent from zone `{current}` but actor expects zone `{expected}`"
            ),
        }
    }
}

impl std::error::Error for SendError {}

/// Directive returned by actor message handlers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActorDirective {
    Continue,
    Stop,
    Fail(String),
}

/// Configuration used when spawning an actor.
#[derive(Debug, Clone)]
pub struct ActorConfig {
    pub name: Arc<str>,
    pub zone: Zone,
    pub mailbox_capacity: usize,
}

impl ActorConfig {
    pub fn new(name: impl Into<Arc<str>>, zone: Zone) -> Self {
        Self {
            name: name.into(),
            zone,
            mailbox_capacity: 64,
        }
    }

    pub fn with_mailbox(mut self, capacity: usize) -> Self {
        self.mailbox_capacity = capacity;
        self
    }
}

struct ActorCell {
    id: ActorId,
    cancel: Arc<AtomicBool>,
    join: Mutex<Option<JoinHandle<ActorExitStatus>>>,
    wake: Sender<Message>,
}

impl ActorCell {
    fn cancel(&self) {
        if self
            .cancel
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_ok()
        {
            let _ = self.wake.send(Message::Shutdown);
        }
    }
}

#[derive(Clone)]
pub struct ActorHandle {
    id: ActorId,
    name: Arc<str>,
    zone: Zone,
    sender: Sender<Message>,
    cancel: Arc<AtomicBool>,
    supervisor: Weak<SupervisorInner>,
}

impl ActorHandle {
    pub fn send(&self, msg: Message) -> Result<(), SendError> {
        let current = current_zone();
        if current != self.zone {
            if let Some(supervisor) = self.supervisor.upgrade() {
                supervisor.emit_event(SupervisorEvent::ZoneViolation {
                    supervisor: supervisor.id,
                    actor: self.id,
                    name: self.name.clone(),
                    expected: self.zone.clone(),
                    current: current.clone(),
                });
            }
            return Err(SendError::ZoneMismatch {
                expected: self.zone.clone(),
                current,
            });
        }
        self.sender.send(msg).map_err(|_| SendError::MailboxClosed)
    }

    pub fn cancel(&self) {
        if self
            .cancel
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_ok()
        {
            let _ = self.sender.send(Message::Shutdown);
        }
    }
}

pub struct Actor {
    handle: ActorHandle,
    cell: Arc<ActorCell>,
}

impl Actor {
    pub fn handle(&self) -> ActorHandle {
        self.handle.clone()
    }

    pub fn cancel(&self) {
        self.cell.cancel();
    }

    pub fn join(self) -> Result<ActorExitStatus, ActorJoinError> {
        let handle = {
            let mut guard = self.cell.join.lock().unwrap();
            guard.take().ok_or(ActorJoinError::AlreadyJoined)?
        };
        match handle.join() {
            Ok(status) => Ok(status),
            Err(_) => Err(ActorJoinError::Panicked),
        }
    }
}

pub struct SupervisorEvents {
    receiver: Receiver<SupervisorEvent>,
}

impl SupervisorEvents {
    pub fn recv(&self) -> Result<SupervisorEvent, crossbeam_channel::RecvError> {
        self.receiver.recv()
    }

    pub fn try_recv(&self) -> Result<SupervisorEvent, crossbeam_channel::TryRecvError> {
        self.receiver.try_recv()
    }
}

struct SupervisorInner {
    id: SupervisorId,
    name: Arc<str>,
    zone: Zone,
    parent: Option<Weak<SupervisorInner>>,
    scheduler: Arc<SchedulerInner>,
    subscribers: Mutex<Vec<Sender<SupervisorEvent>>>,
    children: Mutex<Vec<Weak<ActorCell>>>,
    supervisors: Mutex<Vec<Weak<SupervisorInner>>>,
    cancelled: AtomicBool,
    handle_refs: AtomicUsize,
}

impl SupervisorInner {
    fn emit_event(&self, event: SupervisorEvent) {
        let mut stale = Vec::new();
        {
            let subscribers = self.subscribers.lock().unwrap();
            for (idx, subscriber) in subscribers.iter().enumerate() {
                if subscriber.send(event.clone()).is_err() {
                    stale.push(idx);
                }
            }
        }
        if !stale.is_empty() {
            let mut subscribers = self.subscribers.lock().unwrap();
            for idx in stale.into_iter().rev() {
                subscribers.remove(idx);
            }
        }
        if let Some(parent) = self.parent.as_ref().and_then(|p| p.upgrade()) {
            parent.emit_event(event);
        }
    }

    fn register_child(&self, cell: &Arc<ActorCell>) {
        self.children.lock().unwrap().push(Arc::downgrade(cell));
    }

    fn remove_child(&self, id: ActorId) {
        let mut children = self.children.lock().unwrap();
        children.retain(|weak| weak.upgrade().map(|cell| cell.id != id).unwrap_or(false));
    }

    fn register_supervisor(&self, supervisor: &Arc<SupervisorInner>) {
        self.supervisors
            .lock()
            .unwrap()
            .push(Arc::downgrade(supervisor));
    }

    fn remove_supervisor(&self, id: SupervisorId) {
        let mut supervisors = self.supervisors.lock().unwrap();
        supervisors.retain(|weak| weak.upgrade().map(|sup| sup.id != id).unwrap_or(false));
    }

    fn cancel_children(&self) {
        let children = self.children.lock().unwrap().clone();
        for child in children {
            if let Some(cell) = child.upgrade() {
                cell.cancel();
            }
        }

        let supervisors = self.supervisors.lock().unwrap().clone();
        for supervisor in supervisors {
            if let Some(sup) = supervisor.upgrade() {
                if !sup.cancelled.swap(true, Ordering::SeqCst) {
                    sup.cancel_children();
                }
            }
        }
    }

    fn subscribe(&self) -> SupervisorEvents {
        let (tx, rx) = unbounded();
        self.subscribers.lock().unwrap().push(tx);
        SupervisorEvents { receiver: rx }
    }

    fn spawn_actor_internal<F>(
        self: &Arc<Self>,
        config: ActorConfig,
        mut handler: F,
    ) -> Result<Actor, SpawnError>
    where
        F: FnMut(&mut ActorContext, Message) -> ActorDirective + Send + 'static,
    {
        if config.mailbox_capacity == 0 {
            return Err(SpawnError::MailboxCapacityZero);
        }
        if !self.scheduler.policy.can_host(&self.zone, &config.zone) {
            self.emit_event(SupervisorEvent::ZoneViolation {
                supervisor: self.id,
                actor: 0,
                name: config.name.clone(),
                expected: config.zone.clone(),
                current: self.zone.clone(),
            });
            return Err(SpawnError::ZoneDenied {
                parent: self.zone.clone(),
                requested: config.zone.clone(),
                supervisor: self.name.clone(),
                actor: config.name.clone(),
            });
        }

        let actor_id = self.scheduler.next_actor_id();
        let (tx, rx) = crossbeam_channel::bounded(config.mailbox_capacity);
        let cancel = Arc::new(AtomicBool::new(false));
        let cancel_for_thread = cancel.clone();
        let cell = Arc::new(ActorCell {
            id: actor_id,
            cancel: cancel.clone(),
            join: Mutex::new(None),
            wake: tx.clone(),
        });
        self.register_child(&cell);

        let supervisor = Arc::downgrade(self);
        let actor_name = config.name.clone();
        let actor_zone = config.zone.clone();
        let actor_name_for_handle = actor_name.clone();
        let actor_zone_for_handle = actor_zone.clone();
        let supervisor_for_thread = Arc::clone(self);

        let thread_name = format!("aurora-{}-{}-{}", actor_zone.as_str(), self.id, actor_id);

        let join_handle = thread::Builder::new()
            .name(thread_name)
            .spawn(move || {
                let _zone_guard = ZoneGuard::enter(actor_zone.clone());
                let mut context = ActorContext {
                    id: actor_id,
                    name: actor_name.clone(),
                    zone: actor_zone.clone(),
                    supervisor: supervisor_for_thread.clone(),
                    cancel: cancel_for_thread,
                };
                supervisor_for_thread.emit_event(SupervisorEvent::ActorStarted {
                    supervisor: supervisor_for_thread.id,
                    actor: actor_id,
                    name: actor_name.clone(),
                    zone: actor_zone.clone(),
                });

                let status = run_actor_loop(&mut context, rx, &mut handler);
                match &status {
                    ActorExitStatus::Completed => {
                        supervisor_for_thread.emit_event(SupervisorEvent::ActorStopped {
                            supervisor: supervisor_for_thread.id,
                            actor: actor_id,
                            name: actor_name.clone(),
                        })
                    }
                    ActorExitStatus::Cancelled => {
                        supervisor_for_thread.emit_event(SupervisorEvent::ActorCancelled {
                            supervisor: supervisor_for_thread.id,
                            actor: actor_id,
                            name: actor_name.clone(),
                        })
                    }
                    ActorExitStatus::Failed(reason) => {
                        supervisor_for_thread.emit_event(SupervisorEvent::ActorFailed {
                            supervisor: supervisor_for_thread.id,
                            actor: actor_id,
                            name: actor_name.clone(),
                            reason: reason.clone(),
                        })
                    }
                }

                supervisor_for_thread.remove_child(actor_id);
                status
            })
            .expect("failed to spawn actor thread");

        *cell.join.lock().unwrap() = Some(join_handle);

        let handle = ActorHandle {
            id: actor_id,
            name: actor_name_for_handle,
            zone: actor_zone_for_handle,
            sender: tx,
            cancel,
            supervisor,
        };

        Ok(Actor { handle, cell })
    }
}

pub struct SupervisorHandle {
    inner: Arc<SupervisorInner>,
}

impl SupervisorHandle {
    pub fn zone(&self) -> Zone {
        self.inner.zone.clone()
    }

    pub fn id(&self) -> SupervisorId {
        self.inner.id
    }

    pub fn name(&self) -> Arc<str> {
        self.inner.name.clone()
    }

    pub fn subscribe(&self) -> SupervisorEvents {
        self.inner.subscribe()
    }

    pub fn spawn_actor_with<F>(&self, config: ActorConfig, handler: F) -> Result<Actor, SpawnError>
    where
        F: FnMut(&mut ActorContext, Message) -> ActorDirective + Send + 'static,
    {
        self.inner.spawn_actor_internal(config, handler)
    }

    pub fn spawn_default_actor(
        &self,
        name: impl Into<Arc<str>>,
        zone: Zone,
    ) -> Result<Actor, SpawnError> {
        let config = ActorConfig::new(name, zone);
        self.spawn_actor_with(config, default_actor)
    }

    pub fn create_child(&self, name: impl Into<Arc<str>>, zone: Zone) -> SupervisorHandle {
        let child = Arc::new(SupervisorInner {
            id: self.inner.scheduler.next_supervisor_id(),
            name: name.into(),
            zone,
            parent: Some(Arc::downgrade(&self.inner)),
            scheduler: Arc::clone(&self.inner.scheduler),
            subscribers: Mutex::new(Vec::new()),
            children: Mutex::new(Vec::new()),
            supervisors: Mutex::new(Vec::new()),
            cancelled: AtomicBool::new(false),
            handle_refs: AtomicUsize::new(1),
        });
        self.inner.register_supervisor(&child);
        SupervisorHandle { inner: child }
    }

    pub fn cancel_children(&self) {
        self.inner.cancel_children();
    }
}

impl Clone for SupervisorHandle {
    fn clone(&self) -> Self {
        self.inner.handle_refs.fetch_add(1, Ordering::SeqCst);
        SupervisorHandle {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl Drop for SupervisorHandle {
    fn drop(&mut self) {
        if let Some(parent) = self.inner.parent.as_ref().and_then(|p| p.upgrade()) {
            parent.remove_supervisor(self.inner.id);
        }
        if self.inner.handle_refs.fetch_sub(1, Ordering::SeqCst) == 1
            && !self.inner.cancelled.swap(true, Ordering::SeqCst)
        {
            self.inner.cancel_children();
        }
    }
}

struct SchedulerInner {
    next_actor: AtomicUsize,
    next_supervisor: AtomicUsize,
    policy: ZonePolicy,
}

impl SchedulerInner {
    fn next_actor_id(&self) -> ActorId {
        self.next_actor.fetch_add(1, Ordering::SeqCst)
    }

    fn next_supervisor_id(&self) -> SupervisorId {
        self.next_supervisor.fetch_add(1, Ordering::SeqCst)
    }
}

pub struct Scheduler {
    inner: Arc<SchedulerInner>,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(SchedulerInner {
                next_actor: AtomicUsize::new(1),
                next_supervisor: AtomicUsize::new(1),
                policy: ZonePolicy::default(),
            }),
        }
    }

    pub fn with_policy(policy: ZonePolicy) -> Self {
        Self {
            inner: Arc::new(SchedulerInner {
                next_actor: AtomicUsize::new(1),
                next_supervisor: AtomicUsize::new(1),
                policy,
            }),
        }
    }

    pub fn root_supervisor(
        &self,
        name: impl Into<Arc<str>>,
        zone: Zone,
    ) -> (SupervisorHandle, SupervisorEvents) {
        let handle = SupervisorHandle {
            inner: Arc::new(SupervisorInner {
                id: self.inner.next_supervisor_id(),
                name: name.into(),
                zone,
                parent: None,
                scheduler: Arc::clone(&self.inner),
                subscribers: Mutex::new(Vec::new()),
                children: Mutex::new(Vec::new()),
                supervisors: Mutex::new(Vec::new()),
                cancelled: AtomicBool::new(false),
                handle_refs: AtomicUsize::new(1),
            }),
        };
        let events = handle.subscribe();
        (handle, events)
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ActorContext {
    id: ActorId,
    name: Arc<str>,
    zone: Zone,
    supervisor: Arc<SupervisorInner>,
    cancel: Arc<AtomicBool>,
}

impl ActorContext {
    pub fn id(&self) -> ActorId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn zone(&self) -> &Zone {
        &self.zone
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancel.load(Ordering::Acquire)
    }

    pub fn cancel(&self) {
        self.cancel.store(true, Ordering::SeqCst);
    }

    pub fn spawn_child_with<F>(&self, config: ActorConfig, handler: F) -> Result<Actor, SpawnError>
    where
        F: FnMut(&mut ActorContext, Message) -> ActorDirective + Send + 'static,
    {
        self.supervisor.spawn_actor_internal(config, handler)
    }

    pub fn spawn_child(&self, name: impl Into<Arc<str>>, zone: Zone) -> Result<Actor, SpawnError> {
        let config = ActorConfig::new(name, zone);
        self.spawn_child_with(config, default_actor)
    }
}

fn run_actor_loop<F>(
    context: &mut ActorContext,
    mailbox: Receiver<Message>,
    handler: &mut F,
) -> ActorExitStatus
where
    F: FnMut(&mut ActorContext, Message) -> ActorDirective,
{
    loop {
        if context.is_cancelled() {
            return ActorExitStatus::Cancelled;
        }
        match mailbox.recv_timeout(Duration::from_millis(50)) {
            Ok(message) => match handler(context, message) {
                ActorDirective::Continue => continue,
                ActorDirective::Stop => {
                    if context.is_cancelled() {
                        return ActorExitStatus::Cancelled;
                    }
                    return ActorExitStatus::Completed;
                }
                ActorDirective::Fail(reason) => return ActorExitStatus::Failed(reason),
            },
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => {
                if context.is_cancelled() {
                    return ActorExitStatus::Cancelled;
                }
                return ActorExitStatus::Completed;
            }
        }
    }
}

fn default_actor(ctx: &mut ActorContext, msg: Message) -> ActorDirective {
    match msg {
        Message::Work(payload) => {
            debug!(actor = ctx.name(), zone = %ctx.zone(), %payload, "processing work");
            ActorDirective::Continue
        }
        Message::Fail(reason) => {
            warn!(actor = ctx.name(), zone = %ctx.zone(), %reason, "actor failing");
            ActorDirective::Fail(reason)
        }
        Message::Shutdown => {
            info!(actor = ctx.name(), zone = %ctx.zone(), "shutdown requested");
            ActorDirective::Stop
        }
    }
}

/// Convenience helper replicating the demo pipeline from the Tokio spike using the
/// new scheduler. Returns once both actors finish processing messages.
pub fn demo_pipeline() {
    let scheduler = Scheduler::new();
    let (supervisor, _events) = scheduler.root_supervisor("root", Zone::cpu());

    let ingest = supervisor
        .spawn_default_actor("ingest", Zone::cpu())
        .expect("spawn ingest");
    let transform = supervisor
        .spawn_default_actor("transform", Zone::cpu())
        .expect("spawn transform");

    {
        let _zone = ZoneGuard::enter(Zone::cpu());
        for i in 0..10 {
            ingest
                .handle()
                .send(Message::Work(format!("ingest-{i}")))
                .expect("send work");
        }
        ingest.handle().send(Message::Shutdown).expect("shutdown");

        for i in 0..10 {
            transform
                .handle()
                .send(Message::Work(format!("transform-{i}")))
                .expect("send work");
        }
        transform
            .handle()
            .send(Message::Shutdown)
            .expect("shutdown");
    }

    ingest.join().expect("ingest join");
    transform.join().expect("transform join");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn actors_process_messages() {
        let _ = tracing_subscriber::fmt().with_test_writer().try_init();
        demo_pipeline();
    }

    #[test]
    fn supervisor_receives_events() {
        let scheduler = Scheduler::new();
        let (supervisor, events) = scheduler.root_supervisor("root", Zone::cpu());
        let actor = supervisor
            .spawn_default_actor("worker", Zone::cpu())
            .expect("spawn worker");
        let handle = actor.handle();

        let started = events.recv().expect("started event");
        assert!(matches!(started, SupervisorEvent::ActorStarted { .. }));

        handle
            .send(Message::Fail("boom".into()))
            .expect("send fail");
        actor.join().expect("join");
        let failed = events.recv().expect("failed event");
        match failed {
            SupervisorEvent::ActorFailed { reason, .. } => {
                assert_eq!(reason, "boom");
            }
            other => panic!("unexpected event: {other:?}"),
        }
    }

    #[test]
    fn zone_enforcement_blocks_cross_zone_messages() {
        let scheduler = Scheduler::new();
        let (supervisor, events) = scheduler.root_supervisor("root", Zone::cpu());
        let cpu_actor = supervisor
            .spawn_default_actor("cpu", Zone::cpu())
            .expect("spawn cpu actor");

        let started = events.recv().expect("started event");
        assert!(matches!(started, SupervisorEvent::ActorStarted { .. }));

        let _gpu_scope = ZoneGuard::enter(Zone::gpu());
        let err = cpu_actor
            .handle()
            .send(Message::Work("task".into()))
            .expect_err("send must fail");
        match err {
            SendError::ZoneMismatch { expected, current } => {
                assert_eq!(expected, Zone::cpu());
                assert_eq!(current, Zone::gpu());
            }
            other => panic!("unexpected error: {other}"),
        }

        // Zone violation should emit an event.
        let violation = events.recv().expect("zone violation");
        assert!(matches!(violation, SupervisorEvent::ZoneViolation { .. }));

        // Cleanup
        let _cpu_scope = ZoneGuard::enter(Zone::cpu());
        cpu_actor
            .handle()
            .send(Message::Shutdown)
            .expect("shutdown");
        cpu_actor.join().expect("join");
    }

    #[test]
    fn structured_concurrency_cancels_children_on_drop() {
        let scheduler = Scheduler::new();
        let (root, _events) = scheduler.root_supervisor("root", Zone::cpu());
        let child_supervisor = root.create_child("child", Zone::cpu());
        let actor = child_supervisor
            .spawn_actor_with(
                ActorConfig::new("long-running", Zone::cpu()),
                |ctx, msg| match msg {
                    Message::Shutdown => ActorDirective::Stop,
                    _ => {
                        if ctx.is_cancelled() {
                            ActorDirective::Stop
                        } else {
                            ActorDirective::Continue
                        }
                    }
                },
            )
            .expect("spawn actor");

        drop(child_supervisor);
        let status = actor.join().expect("join");
        assert_eq!(status, ActorExitStatus::Cancelled);
    }

    #[test]
    fn mixed_zone_spawning_respects_policy() {
        let scheduler = Scheduler::new();
        let (root, _events) = scheduler.root_supervisor("root", Zone::cpu());

        // CPU supervisor can host GPU actors.
        let gpu_actor = root
            .spawn_default_actor("gpu-worker", Zone::gpu())
            .expect("cpu supervisor should spawn gpu actor");
        gpu_actor.cancel();
        gpu_actor.join().expect("join gpu");

        // GPU supervisor cannot host CPU actors.
        let gpu_supervisor = root.create_child("gpu-supervisor", Zone::gpu());
        let result = gpu_supervisor.spawn_default_actor("cpu-worker", Zone::cpu());
        match result {
            Ok(_) => panic!("gpu supervisor must reject cpu actor"),
            Err(err) => match err {
                SpawnError::ZoneDenied {
                    parent, requested, ..
                } => {
                    assert_eq!(parent, Zone::gpu());
                    assert_eq!(requested, Zone::cpu());
                }
                other => panic!("unexpected error: {other}"),
            },
        }
    }

    #[test]
    fn supervision_tree_cascades_cancellation() {
        let scheduler = Scheduler::new();
        let (root, _events) = scheduler.root_supervisor("root", Zone::cpu());
        let level_one = root.create_child("level-one", Zone::cpu());
        let level_two = level_one.create_child("level-two", Zone::cpu());

        let actor_one = level_one
            .spawn_actor_with(
                ActorConfig::new("actor-one", Zone::cpu()),
                |ctx, msg| match msg {
                    Message::Shutdown => ActorDirective::Stop,
                    _ if ctx.is_cancelled() => ActorDirective::Stop,
                    _ => ActorDirective::Continue,
                },
            )
            .expect("spawn actor one");

        let actor_two = level_two
            .spawn_actor_with(
                ActorConfig::new("actor-two", Zone::cpu()),
                |ctx, msg| match msg {
                    Message::Shutdown => ActorDirective::Stop,
                    _ if ctx.is_cancelled() => ActorDirective::Stop,
                    _ => ActorDirective::Continue,
                },
            )
            .expect("spawn actor two");

        drop(root);

        assert_eq!(
            actor_one.join().expect("join one"),
            ActorExitStatus::Cancelled
        );
        assert_eq!(
            actor_two.join().expect("join two"),
            ActorExitStatus::Cancelled
        );
    }
}
