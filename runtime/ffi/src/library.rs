use crate::error::{FfiError, FfiResult};
use crate::function::ForeignFunction;
use aurora_runtime_scheduler::Zone;
use libloading::Library;
use std::env;
use std::path::Path;
use std::sync::Arc;

#[derive(Clone)]
pub struct ForeignLibrary {
    zone: Zone,
    inner: Arc<Library>,
}

impl ForeignLibrary {
    pub fn new(zone: Zone, library: Library) -> Self {
        Self {
            zone,
            inner: Arc::new(library),
        }
    }

    pub fn zone(&self) -> Zone {
        self.zone.clone()
    }

    /// # Safety
    ///
    /// The caller must guarantee that the requested symbol exists and has a
    /// signature compatible with `T`. The returned function pointer must not be
    /// invoked after the library is unloaded.
    pub unsafe fn get<T>(&self, symbol: &[u8]) -> FfiResult<ForeignFunction<T>>
    where
        T: Copy + Send + Sync + 'static,
    {
        if !symbol.ends_with(&[0]) {
            return Err(FfiError::SymbolNotFound {
                name: String::from_utf8_lossy(symbol).into_owned(),
            });
        }
        // Attempt loading immediately to surface errors early.
        self.inner
            .get::<T>(symbol)
            .map_err(|_| FfiError::SymbolNotFound {
                name: String::from_utf8_lossy(symbol).into_owned(),
            })?;
        Ok(ForeignFunction::new(
            self.zone.clone(),
            self.inner.clone(),
            symbol.to_vec(),
        ))
    }
}

impl ForeignLibrary {
    pub fn from_path(zone: Zone, path: impl AsRef<Path>) -> FfiResult<Self> {
        let path_ref = path.as_ref();
        let path_str = path_ref.display().to_string();
        let library = unsafe { Library::new(path_ref) }.map_err(|err| FfiError::LoadFailure {
            path: path_str,
            message: err.to_string(),
        })?;
        Ok(Self::new(zone, library))
    }

    pub fn this_process(zone: Zone) -> FfiResult<Self> {
        let exe = env::current_exe().map_err(|err| FfiError::LoadFailure {
            path: "<self>".into(),
            message: err.to_string(),
        })?;
        Self::from_path(zone, exe)
    }
}
