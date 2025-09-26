pub mod async_io;
pub mod collections;
pub mod errors;
pub mod io;
pub mod math;
pub mod option;
pub mod prelude;
pub mod region;
pub mod result;
pub mod smart;
pub mod string;
pub mod zone;

pub use async_io::{timer_service, AurTask, TimerFuture};
pub use collections::{AurMap, AurVec};
pub use errors::{AurError, AurErrorKind};
pub use option::AurOption;
pub use region::{RegionHandleExt, RegionOwned, RegionScope};
pub use result::{AurResult, AurResultExt};
pub use smart::{AurArc, AurCell, AurMutex, AurRwLock};
pub use string::AurString;
pub use zone::{AurZone, AurZoneManager};

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::FromIterator;
    use std::time::Duration;

    #[test]
    fn option_round_trip() {
        let opt = AurOption::some(42);
        assert!(opt.is_some());
        let std_opt: Option<_> = opt.clone().into();
        assert_eq!(std_opt, Some(42));
        let mapped = opt.map(|v| v + 1);
        assert_eq!(mapped.unwrap(), 43);
    }

    #[test]
    fn result_combinators() {
        let res: AurResult<u32, &'static str> = AurResult::ok(7);
        let mapped = res.map(|v| v * 2);
        assert_eq!(mapped.unwrap(), 14);
        let err: AurResult<u32, &'static str> = AurResult::err("boom");
        assert!(err.map(|v| v * 2).is_err());
    }

    #[test]
    fn string_manipulations() {
        let mut s = AurString::from("hello");
        s.push_str(" world");
        assert_eq!(s.as_str(), "hello world");
        assert_eq!(s.len(), 11);
    }

    #[test]
    fn math_helpers() {
        assert_eq!(math::clamp(5, 0, 3), 3);
        assert!(math::approx_eq(0.1_f64 + 0.2_f64, 0.3_f64, 1e-9));
    }

    #[test]
    fn json_round_trip() {
        let data = collections::AurMap::from_iter([("a".to_string(), 1), ("b".to_string(), 2)]);
        let json = io::to_json(&data).unwrap();
        let back: collections::AurMap<String, i32> = io::from_json(&json).unwrap();
        assert_eq!(back.get(&"a".to_string()), Some(&1));
    }

    #[test]
    fn timer_future_completes() {
        let zone_manager = AurZoneManager::new();
        let timer_service =
            aurora_runtime_io::TimerService::new(zone_manager.clone(), AurZone::cpu());
        let task = TimerFuture::new(timer_service.clone(), Duration::from_millis(5));
        task.wait().unwrap();
    }
}
