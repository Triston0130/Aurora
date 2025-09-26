use std::fmt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum AurOption<T> {
    Some(T),
    None,
}

impl<T> AurOption<T> {
    pub fn some(value: T) -> Self {
        Self::Some(value)
    }

    pub fn none() -> Self {
        Self::None
    }

    pub fn is_some(&self) -> bool {
        matches!(self, Self::Some(_))
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> AurOption<U> {
        match self {
            Self::Some(value) => AurOption::Some(f(value)),
            Self::None => AurOption::None,
        }
    }

    pub fn ok_or<E>(self, err: E) -> crate::result::AurResult<T, E> {
        match self {
            Self::Some(value) => crate::result::AurResult::Ok(value),
            Self::None => crate::result::AurResult::Err(err),
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Self::Some(value) => value,
            Self::None => panic!("called `AurOption::unwrap()` on a `None` value"),
        }
    }

    pub fn unwrap_or(self, default: T) -> T
    where
        T: Clone,
    {
        match self {
            Self::Some(value) => value,
            Self::None => default,
        }
    }
}

impl<T> From<Option<T>> for AurOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => Self::Some(v),
            None => Self::None,
        }
    }
}

impl<T> From<AurOption<T>> for Option<T> {
    fn from(value: AurOption<T>) -> Self {
        match value {
            AurOption::Some(v) => Some(v),
            AurOption::None => None,
        }
    }
}

impl<T> fmt::Display for AurOption<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some(value) => write!(f, "Some({value})"),
            Self::None => write!(f, "None"),
        }
    }
}
