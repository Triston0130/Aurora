use std::fmt;

#[derive(Debug)]
pub enum IoError {
    Io(std::io::Error),
    Timeout,
    ConnectionClosed,
}

impl fmt::Display for IoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IoError::Io(err) => write!(f, "io error: {err}"),
            IoError::Timeout => write!(f, "operation timed out"),
            IoError::ConnectionClosed => write!(f, "connection closed"),
        }
    }
}

impl std::error::Error for IoError {}

impl From<std::io::Error> for IoError {
    fn from(err: std::io::Error) -> Self {
        IoError::Io(err)
    }
}

pub type IoResult<T> = Result<T, IoError>;
