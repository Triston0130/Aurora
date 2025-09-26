use crate::effects::IoEffect;
use crate::error::IoError;
use crate::task::{spawn_io_worker, IoTask};
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub struct FileSystem {
    #[allow(dead_code)]
    manager: ZoneManager,
    zone: Zone,
}

impl FileSystem {
    pub fn new(manager: ZoneManager, zone: Zone) -> Self {
        Self { manager, zone }
    }

    pub fn read_to_string(&self, path: impl AsRef<Path> + Send + 'static) -> IoTask<String> {
        let zone = self.zone.clone();
        let path_buf = PathBuf::from(path.as_ref());
        spawn_io_worker(IoEffect::FileRead, zone, move || {
            fs::read_to_string(&path_buf).map_err(IoError::from)
        })
    }

    pub fn write_all(&self, path: impl AsRef<Path> + Send + 'static, data: Vec<u8>) -> IoTask<()> {
        let zone = self.zone.clone();
        let path_buf = PathBuf::from(path.as_ref());
        spawn_io_worker(IoEffect::FileWrite, zone, move || {
            fs::write(&path_buf, &data).map_err(IoError::from)
        })
    }
}
