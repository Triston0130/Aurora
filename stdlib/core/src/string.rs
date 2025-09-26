use std::fmt;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AurString(String);

impl AurString {
    pub fn new() -> Self {
        Self(String::new())
    }

    pub fn from<S: Into<String>>(value: S) -> Self {
        Self(value.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn push_str(&mut self, s: &str) {
        self.0.push_str(s);
    }

    pub fn push(&mut self, ch: char) {
        self.0.push(ch);
    }
}

impl fmt::Display for AurString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for AurString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for AurString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for AurString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<&str> for AurString {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

impl From<String> for AurString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<AurString> for String {
    fn from(value: AurString) -> Self {
        value.0
    }
}
