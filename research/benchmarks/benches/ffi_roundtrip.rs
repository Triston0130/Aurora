use aurora_runtime_ffi::{FfiCapability, FfiManager, ForeignFunction};
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use std::ffi::CString;
use std::os::raw::c_char;

#[cfg(target_os = "windows")]
const LIBC_NAME: &str = "msvcrt";

#[cfg(not(target_os = "windows"))]
const LIBC_NAME: &str = "c";

fn ffi_strlen(batch: usize) {
    let manager = FfiManager::new(ZoneManager::new());
    manager.permit(Zone::cpu(), FfiCapability::DynamicLibrary);
    manager.permit(Zone::cpu(), FfiCapability::FunctionCall);

    let libc_path = libloading::library_filename(LIBC_NAME);
    let library = manager
        .load_library(Zone::cpu(), libc_path)
        .expect("load libc");
    let strlen: ForeignFunction<unsafe extern "C" fn(*const c_char) -> usize> =
        unsafe { library.get(b"strlen\0").expect("locate strlen symbol") };

    let payload = CString::new("aurora").unwrap();

    for _ in 0..batch {
        let _len = unsafe { strlen.call(|f| f(payload.as_ptr())).expect("ffi call") };
    }
}

fn ffi_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("ffi_roundtrip_strlen");
    let batch = 512;
    group.throughput(Throughput::Elements(batch as u64));
    group.bench_function("strlen_batch_512", |b| b.iter(|| ffi_strlen(batch)));
    group.finish();
}

criterion_group!(ffi, ffi_benchmark);
criterion_main!(ffi);
