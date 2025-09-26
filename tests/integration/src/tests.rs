use aurora_runtime_ffi::{FfiCapability, FfiManager};
use aurora_runtime_scheduler::{Message, Zone};
use aurora_stdlib::prelude::*;
use aurora_zone_manager::{ActorDirective, ActorExitStatus, ZoneManager};
use crossbeam_channel::bounded;
use std::time::Duration;

#[cfg(target_os = "windows")]
const LIBC_NAME: &str = "msvcrt";
#[cfg(not(target_os = "windows"))]
const LIBC_NAME: &str = "c";

#[test]
fn actor_pipeline_uses_stdlib_primitives() {
    let manager = AurZoneManager::new();
    let (tx, rx) = bounded::<AurString>(1);

    let actor = manager
        .spawn_actor("integration-worker", Zone::cpu(), move |_, msg| match msg {
            Message::Work(payload) => {
                let mut value = AurString::from(payload);
                value.push('!');
                tx.send(value).unwrap();
                ActorDirective::Stop
            }
            Message::Shutdown => ActorDirective::Stop,
            Message::Fail(reason) => ActorDirective::Fail(reason),
        })
        .expect("spawn integration actor");

    manager
        .dispatch_to_actor(Zone::cpu(), &actor.handle(), Message::Work("hello".into()))
        .expect("dispatch work");

    let result = rx
        .recv_timeout(Duration::from_secs(1))
        .expect("actor response");
    assert_eq!(result.as_str(), "hello!");

    let status = actor.join().expect("join actor");
    assert_eq!(status, ActorExitStatus::Completed);
}

#[test]
fn timer_future_and_zone_transfer_work_together() {
    let manager = AurZoneManager::new();
    let timer = timer_service(manager.clone(), AurZone::cpu());
    TimerFuture::new(timer, Duration::from_millis(10))
        .wait()
        .unwrap();

    let handoff = manager.region_handoff();
    let scope_src = RegionScope::new("src", handoff.clone());
    let scope_dst = RegionScope::new("dst", handoff.clone());
    manager.register_region(scope_src.region());
    manager.register_region(scope_dst.region());

    let owned = RegionOwned::new(scope_src.region(), AurString::from("payload"));
    let transferred = owned.transfer(scope_dst.region(), &handoff);
    let value = transferred.with(|v| v.clone()).unwrap();
    assert_eq!(value.as_str(), "payload");
}

#[test]
fn ffi_manager_integration_with_stdlib_errors() {
    let manager = FfiManager::new(ZoneManager::new());
    let zone = Zone::cpu();
    manager.permit(zone.clone(), FfiCapability::DynamicLibrary);
    manager.permit(zone.clone(), FfiCapability::FunctionCall);

    let library_path = libloading::library_filename(LIBC_NAME);
    let library = manager
        .load_library(zone.clone(), library_path)
        .expect("load system libc");
    let symbol = b"strlen\0";
    let func: aurora_runtime_ffi::ForeignFunction<
        unsafe extern "C" fn(*const std::os::raw::c_char) -> usize,
    > = unsafe { library.get(symbol) }.expect("lookup strlen");

    let s = AurString::from("aurora");
    let c_string = std::ffi::CString::new(s.as_str()).unwrap();
    let len = unsafe { func.call(|f| f(c_string.as_ptr())).unwrap() };
    assert_eq!(len, 6);
}
