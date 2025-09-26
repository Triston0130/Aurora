use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct AurVec<T>(Vec<T>);

impl<T> AurVec<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value);
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.0.iter()
    }
}

impl<T> Deref for AurVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for AurVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> From<Vec<T>> for AurVec<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

impl<T> From<AurVec<T>> for Vec<T> {
    fn from(value: AurVec<T>) -> Self {
        value.0
    }
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct AurMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    inner: HashMap<K, V>,
}

impl<K, V> AurMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.inner.insert(key, value)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.inner.get(key)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, K, V> {
        self.inner.iter()
    }
}

impl<K, V> Deref for AurMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    type Target = HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<K, V> DerefMut for AurMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<K, V> From<HashMap<K, V>> for AurMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn from(value: HashMap<K, V>) -> Self {
        Self { inner: value }
    }
}

impl<K, V> From<AurMap<K, V>> for HashMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn from(value: AurMap<K, V>) -> Self {
        value.inner
    }
}

impl<K, V> FromIterator<(K, V)> for AurMap<K, V>
where
    K: std::hash::Hash + Eq,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self {
            inner: HashMap::from_iter(iter),
        }
    }
}

impl<T> FromIterator<T> for AurVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}
