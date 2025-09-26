mod effects;
mod error;
mod filesystem;
mod network;
mod task;
mod timer;

pub use effects::IoEffect;
pub use error::{IoError, IoResult};
pub use filesystem::FileSystem;
pub use network::{TcpClient, TcpRequest};
pub use task::IoTask;
pub use timer::TimerService;

#[cfg(test)]
mod tests {
    use super::*;
    use aurora_runtime_scheduler::Zone;
    use aurora_zone_manager::ZoneManager;
    use std::env;
    use std::fs;
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::thread;
    use std::time::{Duration, Instant};

    fn test_manager() -> ZoneManager {
        ZoneManager::new()
    }

    #[test]
    fn filesystem_round_trip() {
        let manager = test_manager();
        let filesystem = FileSystem::new(manager.clone(), Zone::cpu());

        let mut path = env::temp_dir();
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        path.push(format!(
            "aurora_io_test_{}_{}.txt",
            std::process::id(),
            unique
        ));

        let data = b"aurora async io".to_vec();
        filesystem
            .write_all(path.clone(), data.clone())
            .wait()
            .expect("write succeeds");

        let contents = filesystem
            .read_to_string(path.clone())
            .wait()
            .expect("read succeeds");
        assert_eq!(contents.as_bytes(), &data);

        let _ = fs::remove_file(path);
    }

    #[test]
    fn tcp_client_sends_and_receives() {
        let manager = test_manager();
        let client = TcpClient::new(manager.clone(), Zone::cpu());

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind listener");
        let addr = listener.local_addr().unwrap();

        thread::spawn(move || {
            if let Ok((mut stream, _)) = listener.accept() {
                let mut buffer = [0_u8; 1024];
                if let Ok(n) = stream.read(&mut buffer) {
                    if n > 0 {
                        let _ = stream.write_all(&buffer[..n]);
                    }
                }
            }
        });

        let payload = b"ping".to_vec();
        let request = TcpRequest::new(addr, payload.clone());
        let response = client.send(request).wait().expect("tcp send");
        assert_eq!(response, payload);
    }

    #[test]
    fn timer_service_sleeps_for_duration() {
        let manager = test_manager();
        let timer = TimerService::new(manager.clone(), Zone::cpu());
        let duration = Duration::from_millis(25);
        let start = Instant::now();
        timer.sleep(duration).wait().expect("sleep completes");
        assert!(start.elapsed() >= duration);
    }
}
