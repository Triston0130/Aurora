/// Aurora standard prelude. Example:
///
/// ```
/// use aurora_stdlib::prelude::*;
///
/// let value = AurOption::some(5);
/// assert_eq!(value.unwrap_or(0), 5);
/// ```
pub use crate::async_io::{timer_service, AurTask, TimerFuture};
pub use crate::collections::{AurMap, AurVec};
pub use crate::errors::{AurError, AurErrorKind, AurStdResult};
pub use crate::math::{approx_eq, clamp, lerp};
pub use crate::option::AurOption;
pub use crate::region::{RegionHandleExt, RegionOwned, RegionScope};
pub use crate::result::{AurResult, AurResultExt};
pub use crate::smart::{AurArc, AurCell, AurMutex, AurRwLock};
pub use crate::string::AurString;
pub use crate::zone::{AurZone, AurZoneManager};
