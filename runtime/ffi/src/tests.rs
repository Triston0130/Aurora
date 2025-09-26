use super::*;
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use std::ffi::CString;
use std::os::raw::c_char;

#[cfg(target_os = "windows")]
const LIBC_NAME: &str = "msvcrt";

#[cfg(not(target_os = "windows"))]
const LIBC_NAME: &str = "c";

#[test]
fn ffi_manager_respects_zone_capabilities() {
    let manager = FfiManager::new(ZoneManager::new());
    let gpu_zone = Zone::gpu();
    assert!(matches!(
        manager.load_self(gpu_zone.clone()),
        Err(FfiError::ZoneDenied { zone }) if zone == gpu_zone
    ));

    manager.permit(gpu_zone.clone(), FfiCapability::DynamicLibrary);
    manager.permit(gpu_zone.clone(), FfiCapability::FunctionCall);
    assert!(manager.load_self(gpu_zone).is_ok());
}

#[test]
fn ffi_function_invokes_and_returns() {
    let manager = FfiManager::new(ZoneManager::new());
    let library_path = libloading::library_filename(LIBC_NAME);
    let lib = manager
        .load_library(Zone::cpu(), std::path::PathBuf::from(&library_path))
        .expect("load libc");
    let symbol = b"strlen\0";
    let func: ForeignFunction<unsafe extern "C" fn(*const c_char) -> usize> =
        unsafe { lib.get(symbol) }.expect("lookup strlen");

    let cstr = CString::new("aurora").unwrap();
    let value = unsafe { func.call(|f| f(cstr.as_ptr())).expect("invoke strlen") };
    assert_eq!(value, 6);
}

#[test]
fn ffi_function_panics_produce_error() {
    let manager = FfiManager::new(ZoneManager::new());
    let library_path = libloading::library_filename(LIBC_NAME);
    let lib = manager
        .load_library(Zone::cpu(), std::path::PathBuf::from(&library_path))
        .expect("load libc");
    let symbol = b"strlen\0";
    let func: ForeignFunction<unsafe extern "C" fn(*const c_char) -> usize> =
        unsafe { lib.get(symbol) }.expect("lookup strlen");

    let result = unsafe { func.call(|_| panic!("simulate FFI panic")) };
    assert!(matches!(result, Err(FfiError::InvocationPanicked)));
}

#[test]
fn ffi_manager_denies_function_calls_without_capability() {
    let zone = Zone::gpu();
    let manager = FfiManager::new(ZoneManager::new());
    let result = manager.load_library(
        zone.clone(),
        std::path::PathBuf::from(libloading::library_filename(LIBC_NAME)),
    );
    assert!(matches!(result, Err(FfiError::ZoneDenied { zone: denied }) if denied == zone));
}
