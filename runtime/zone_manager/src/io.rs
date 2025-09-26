use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread;
use std::time::Duration;

use aurora_runtime_scheduler::{
    Actor, ActorConfig, ActorContext, ActorDirective, ActorExitStatus, ActorHandle, ActorJoinError,
    Message, SendError, SpawnError, Zone, ZoneGuard,
};

use crate::ZoneManagerInner;

/// Dispatcher intended for asynchronous subsystems (I/O reactors, timers,
/// device drivers) to spawn actors and deliver messages while automatically
/// managing zone guards.
#[derive(Clone)]
pub struct IoDispatcher {
    inner: Arc<ZoneManagerInner>,
    zone: Zone,
}

impl IoDispatcher {
    pub(crate) fn new(inner: Arc<ZoneManagerInner>, zone: Zone) -> Self {
        Self { inner, zone }
    }

    /// Returns the zone associated with this dispatcher.
    pub fn zone(&self) -> Zone {
        self.zone.clone()
    }

    /// Spawns an actor owned by the dispatcher’s zone.
    pub fn spawn_actor_with<F>(&self, config: ActorConfig, handler: F) -> Result<Actor, SpawnError>
    where
        F: FnMut(&mut ActorContext, Message) -> ActorDirective + Send + 'static,
    {
        assert_eq!(
            config.zone, self.zone,
            "actor config must match dispatcher zone"
        );
        let supervisor = self.inner.ensure_supervisor(&self.zone);
        supervisor.spawn_actor_with(config, handler)
    }

    /// Convenience for spawning a default actor within the dispatcher’s zone.
    pub fn spawn_default_actor(&self, name: impl Into<Arc<str>>) -> Result<Actor, SpawnError> {
        let supervisor = self.inner.ensure_supervisor(&self.zone);
        supervisor.spawn_default_actor(name, self.zone.clone())
    }

    /// Sends a message to an actor while entering the dispatcher’s zone guard.
    pub fn send(&self, handle: &ActorHandle, message: Message) -> Result<(), SendError> {
        let _guard = ZoneGuard::enter(self.zone.clone());
        handle.send(message)
    }

    /// Schedules a one-shot timer that dispatches a message after `delay`.
    pub fn schedule_timeout(
        &self,
        delay: Duration,
        target: ActorHandle,
        message: Message,
    ) -> TimerHandle {
        self.spawn_timer(delay, None, target, message)
    }

    /// Schedules a repeating interval timer that continues dispatching until
    /// cancelled.
    pub fn schedule_interval(
        &self,
        period: Duration,
        target: ActorHandle,
        message: Message,
    ) -> TimerHandle {
        self.spawn_timer(period, Some(period), target, message)
    }

    fn spawn_timer(
        &self,
        delay: Duration,
        repeat: Option<Duration>,
        target: ActorHandle,
        message: Message,
    ) -> TimerHandle {
        let cancel = Arc::new(AtomicBool::new(false));
        let cancel_flag = cancel.clone();
        let periodic = repeat;
        let trigger_delay = delay;
        let target_handle = target.clone();
        let dispatch_message = message.clone();
        let self_handle: Arc<OnceLock<ActorHandle>> = Arc::new(OnceLock::new());
        let self_handle_for_actor = Arc::clone(&self_handle);
        let mut first_fire = true;

        let supervisor = self.inner.ensure_supervisor(&self.zone);
        let actor = supervisor
            .spawn_actor_with(
                ActorConfig::new(format!("{}-timer", self.zone.as_str()), self.zone.clone())
                    .with_mailbox(2),
                move |_, msg| match msg {
                    Message::Work(_) => {
                        if cancel_flag.load(Ordering::Acquire) {
                            return ActorDirective::Stop;
                        }

                        let wait = if first_fire {
                            first_fire = false;
                            trigger_delay
                        } else {
                            periodic.unwrap_or(Duration::ZERO)
                        };

                        if wait > Duration::ZERO {
                            thread::sleep(wait);
                        }

                        if cancel_flag.load(Ordering::Acquire) {
                            return ActorDirective::Stop;
                        }

                        if target_handle.send(dispatch_message.clone()).is_err() {
                            return ActorDirective::Stop;
                        }

                        if let Some(period) = periodic {
                            if cancel_flag.load(Ordering::Acquire) {
                                return ActorDirective::Stop;
                            }
                            if period > Duration::ZERO {
                                if let Some(self_handle) = self_handle_for_actor.get() {
                                    let _ = self_handle.send(Message::Work("tick".into()));
                                }
                            }
                            ActorDirective::Continue
                        } else {
                            ActorDirective::Stop
                        }
                    }
                    Message::Shutdown => ActorDirective::Stop,
                    Message::Fail(reason) => ActorDirective::Fail(reason),
                },
            )
            .expect("spawn timer actor");

        let handle = actor.handle();
        if self_handle.set(handle.clone()).is_err() {
            panic!("timer handle already initialized");
        }

        {
            let _guard = ZoneGuard::enter(self.zone.clone());
            handle
                .send(Message::Work("start".into()))
                .expect("start timer");
        }

        TimerHandle::new(cancel, handle, actor, self.zone.clone())
    }
}

/// Handle controlling a timer spawned by the dispatcher. Dropping the handle
/// cancels the timer and joins the worker thread.
pub struct TimerHandle {
    cancel: Arc<AtomicBool>,
    actor_handle: ActorHandle,
    actor: Mutex<Option<Actor>>,
    zone: Zone,
}

impl TimerHandle {
    fn new(cancel: Arc<AtomicBool>, actor_handle: ActorHandle, actor: Actor, zone: Zone) -> Self {
        Self {
            cancel,
            actor_handle,
            actor: Mutex::new(Some(actor)),
            zone,
        }
    }

    /// Cancels the timer. The actor will observe the flag shortly.
    pub fn cancel(&self) {
        self.cancel.store(true, Ordering::SeqCst);
        let _guard = ZoneGuard::enter(self.zone.clone());
        let _ = self.actor_handle.send(Message::Shutdown);
    }

    /// Waits for the timer actor to finish executing.
    pub fn join(&self) -> Result<ActorExitStatus, ActorJoinError> {
        match self.actor.lock().unwrap().take() {
            Some(actor) => actor.join(),
            None => Err(ActorJoinError::AlreadyJoined),
        }
    }
}

impl Drop for TimerHandle {
    fn drop(&mut self) {
        self.cancel();
        let _ = self.join();
    }
}
