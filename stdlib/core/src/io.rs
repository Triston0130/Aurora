use crate::errors::{AurError, AurErrorKind, AurStdResult};
use serde::de::DeserializeOwned;
use serde::Serialize;

pub fn to_json<T: Serialize>(value: &T) -> AurStdResult<String> {
    serde_json::to_string_pretty(value)
        .map_err(|err| AurError::new(AurErrorKind::Message, err.to_string()))
        .into()
}

pub fn from_json<T: DeserializeOwned>(json: &str) -> AurStdResult<T> {
    serde_json::from_str(json)
        .map_err(|err| AurError::new(AurErrorKind::Message, err.to_string()))
        .into()
}
