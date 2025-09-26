use aurora_runtime_io::IoError;
use std::fmt;

#[derive(Debug, Clone)]
pub enum AurErrorKind {
    Message,
    Io,
}

#[derive(Debug, Clone)]
pub struct AurError {
    kind: AurErrorKind,
    message: String,
}

impl AurError {
    pub fn new(kind: AurErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    pub fn message(message: impl Into<String>) -> Self {
        Self::new(AurErrorKind::Message, message)
    }

    pub fn from_io(io: IoError) -> Self {
        Self::new(AurErrorKind::Io, io.to_string())
    }

    pub fn kind(&self) -> &AurErrorKind {
        &self.kind
    }

    pub fn message_str(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for AurError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for AurError {}

pub type AurStdResult<T> = crate::result::AurResult<T, AurError>;
