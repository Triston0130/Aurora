use aurora_runtime_scheduler::Zone;
use std::fmt;

#[derive(Debug)]
pub enum FfiError {
    ZoneDenied { zone: Zone },
    LoadFailure { path: String, message: String },
    SymbolNotFound { name: String },
    InvocationPanicked,
}

impl fmt::Display for FfiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FfiError::ZoneDenied { zone } => {
                write!(f, "zone {zone} is not permitted to perform FFI")
            }
            FfiError::LoadFailure { path, message } => {
                write!(f, "failed to load library '{path}': {message}")
            }
            FfiError::SymbolNotFound { name } => write!(f, "symbol '{name}' not found"),
            FfiError::InvocationPanicked => write!(f, "foreign function panicked"),
        }
    }
}

impl std::error::Error for FfiError {}

pub type FfiResult<T> = Result<T, FfiError>;
