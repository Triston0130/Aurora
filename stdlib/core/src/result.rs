use crate::option::AurOption;
use std::fmt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum AurResult<T, E> {
    Ok(T),
    Err(E),
}

impl<T, E> AurResult<T, E> {
    pub fn ok(value: T) -> Self {
        Self::Ok(value)
    }

    pub fn err(err: E) -> Self {
        Self::Err(err)
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok(_))
    }

    pub fn is_err(&self) -> bool {
        matches!(self, Self::Err(_))
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> AurResult<U, E> {
        match self {
            Self::Ok(value) => AurResult::Ok(f(value)),
            Self::Err(err) => AurResult::Err(err),
        }
    }

    pub fn map_err<F, O: FnOnce(E) -> F>(self, f: O) -> AurResult<T, F> {
        match self {
            Self::Ok(value) => AurResult::Ok(value),
            Self::Err(err) => AurResult::Err(f(err)),
        }
    }

    pub fn into_option(self) -> AurOption<T> {
        match self {
            Self::Ok(value) => AurOption::Some(value),
            Self::Err(_) => AurOption::None,
        }
    }

    pub fn into_err(self) -> AurOption<E> {
        match self {
            Self::Ok(_) => AurOption::None,
            Self::Err(err) => AurOption::Some(err),
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Self::Ok(value) => value,
            Self::Err(_) => panic!("called `AurResult::unwrap()` on an `Err` value"),
        }
    }

    pub fn unwrap_or(self, default: T) -> T
    where
        T: Clone,
    {
        match self {
            Self::Ok(value) => value,
            Self::Err(_) => default,
        }
    }
}

pub trait AurResultExt<T, E> {
    fn or_log(self, message: &str) -> AurOption<T>;
}

impl<T, E> AurResultExt<T, E> for AurResult<T, E>
where
    E: fmt::Display,
{
    fn or_log(self, message: &str) -> AurOption<T> {
        match self {
            AurResult::Ok(value) => AurOption::Some(value),
            AurResult::Err(err) => {
                eprintln!("{message}: {err}");
                AurOption::None
            }
        }
    }
}

impl<T, E> From<Result<T, E>> for AurResult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(v) => Self::Ok(v),
            Err(err) => Self::Err(err),
        }
    }
}

impl<T, E> From<AurResult<T, E>> for Result<T, E> {
    fn from(value: AurResult<T, E>) -> Self {
        match value {
            AurResult::Ok(v) => Ok(v),
            AurResult::Err(err) => Err(err),
        }
    }
}

impl<T, E> fmt::Display for AurResult<T, E>
where
    T: fmt::Display,
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ok(value) => write!(f, "Ok({value})"),
            Self::Err(err) => write!(f, "Err({err})"),
        }
    }
}
