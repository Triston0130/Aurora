#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IoEffect {
    FileRead,
    FileWrite,
    TcpConnect,
    TcpSend,
    TcpReceive,
    Timer,
}
