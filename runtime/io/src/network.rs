use crate::effects::IoEffect;
use crate::error::IoError;
use crate::task::{spawn_io_worker, IoTask};
use aurora_runtime_scheduler::Zone;
use aurora_zone_manager::ZoneManager;
use std::io::{Read, Write};
use std::net::{SocketAddr, TcpStream};
use std::time::Duration;

#[derive(Debug, Clone)]
pub struct TcpRequest {
    pub addr: SocketAddr,
    pub payload: Vec<u8>,
    pub read_bytes: usize,
    pub timeout: Option<Duration>,
}

impl TcpRequest {
    pub fn new(addr: SocketAddr, payload: Vec<u8>) -> Self {
        Self {
            addr,
            payload,
            read_bytes: 1024,
            timeout: Some(Duration::from_secs(1)),
        }
    }

    pub fn with_read_bytes(mut self, bytes: usize) -> Self {
        self.read_bytes = bytes;
        self
    }

    pub fn with_timeout(mut self, timeout: Option<Duration>) -> Self {
        self.timeout = timeout;
        self
    }
}

#[derive(Clone)]
pub struct TcpClient {
    #[allow(dead_code)]
    manager: ZoneManager,
    zone: Zone,
}

impl TcpClient {
    pub fn new(manager: ZoneManager, zone: Zone) -> Self {
        Self { manager, zone }
    }

    pub fn send(&self, request: TcpRequest) -> IoTask<Vec<u8>> {
        let zone = self.zone.clone();
        spawn_io_worker(IoEffect::TcpSend, zone, move || {
            let mut stream = TcpStream::connect(request.addr)?;
            if let Some(timeout) = request.timeout {
                stream.set_read_timeout(Some(timeout))?;
                stream.set_write_timeout(Some(timeout))?;
            }
            stream.write_all(&request.payload)?;
            stream.flush()?;

            let mut buffer = vec![0_u8; request.read_bytes];
            let n = stream.read(&mut buffer)?;
            buffer.truncate(n);
            if buffer.is_empty() {
                return Err(IoError::ConnectionClosed);
            }
            Ok(buffer)
        })
    }
}
