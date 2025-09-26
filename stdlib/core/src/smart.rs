use std::cell::RefCell;
use std::sync::{Arc, Mutex, RwLock};

pub type AurArc<T> = Arc<T>;
pub type AurMutex<T> = Mutex<T>;
pub type AurRwLock<T> = RwLock<T>;
pub type AurCell<T> = RefCell<T>;
