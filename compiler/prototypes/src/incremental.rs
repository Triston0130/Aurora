use std::collections::HashMap;
use std::sync::Arc;

use blake3::Hash;

use crate::diagnostics::Diagnostic;
use crate::driver::{self, CompilationResult};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleKey(Arc<str>);

impl ModuleKey {
    pub fn new(name: impl Into<String>) -> Self {
        ModuleKey(Arc::from(name.into()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Default)]
pub struct IncrementalCompiler {
    entries: HashMap<ModuleKey, CacheEntry>,
}

#[derive(Debug)]
enum CacheEntry {
    Success {
        hash: Hash,
        dependencies: Vec<ModuleKey>,
        result: Arc<CompilationResult>,
    },
    Failure {
        hash: Hash,
        dependencies: Vec<ModuleKey>,
        diagnostics: Vec<Diagnostic>,
    },
}

impl IncrementalCompiler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn compile(
        &mut self,
        key: ModuleKey,
        source: &str,
        dependencies: Vec<ModuleKey>,
    ) -> Result<(Arc<CompilationResult>, bool), (Vec<Diagnostic>, bool)> {
        let hash = blake3::hash(source.as_bytes());
        if let Some(entry) = self.entries.get(&key) {
            if entry.hash() == hash && entry.dependencies() == &dependencies {
                return match entry {
                    CacheEntry::Success { result, .. } => Ok((Arc::clone(result), true)),
                    CacheEntry::Failure { diagnostics, .. } => Err((diagnostics.clone(), true)),
                };
            }
        }

        match driver::compile_module(key.as_str(), source) {
            Ok(result) => {
                let arc = Arc::new(result);
                self.entries.insert(
                    key,
                    CacheEntry::Success {
                        hash,
                        dependencies,
                        result: Arc::clone(&arc),
                    },
                );
                Ok((arc, false))
            }
            Err(diagnostics) => {
                self.entries.insert(
                    key,
                    CacheEntry::Failure {
                        hash,
                        dependencies,
                        diagnostics: diagnostics.clone(),
                    },
                );
                Err((diagnostics, false))
            }
        }
    }

    pub fn invalidate(&mut self, key: &ModuleKey) {
        self.entries.remove(key);
    }
}

impl CacheEntry {
    fn hash(&self) -> Hash {
        match self {
            CacheEntry::Success { hash, .. } => *hash,
            CacheEntry::Failure { hash, .. } => *hash,
        }
    }

    fn dependencies(&self) -> &Vec<ModuleKey> {
        match self {
            CacheEntry::Success { dependencies, .. } => dependencies,
            CacheEntry::Failure { dependencies, .. } => dependencies,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile_twice(source: &str) -> (bool, bool) {
        let mut compiler = IncrementalCompiler::new();
        let key = ModuleKey::new("test");
        let first = compiler.compile(key.clone(), source, Vec::new());
        assert!(first.is_ok(), "expected success, got {:?}", first);
        let (_, cached_first) = first.unwrap();
        let second = compiler.compile(key, source, Vec::new());
        assert!(second.is_ok(), "expected success, got {:?}", second);
        let (_, cached_second) = second.unwrap();
        (cached_first, cached_second)
    }

    #[test]
    fn reuses_result_when_source_unchanged() {
        let (first_cached, second_cached) = compile_twice("fn main() {}");
        assert!(!first_cached);
        assert!(second_cached);
    }

    #[test]
    fn recompiles_when_source_changes() {
        let mut compiler = IncrementalCompiler::new();
        let key = ModuleKey::new("test");
        let first = compiler
            .compile(key.clone(), "fn main() {}", Vec::new())
            .unwrap();
        assert!(!first.1);
        let second = compiler
            .compile(key.clone(), "fn main() { let x = 1; }", Vec::new())
            .unwrap();
        assert!(!second.1);
        let third = compiler
            .compile(key, "fn main() { let x = 1; }", Vec::new())
            .unwrap();
        assert!(third.1);
    }

    #[test]
    fn caches_failures() {
        let mut compiler = IncrementalCompiler::new();
        let key = ModuleKey::new("broken");
        let first = compiler.compile(key.clone(), "fn", Vec::new());
        assert!(first.is_err());
        let (_, cached_first) = first.err().unwrap();
        assert!(!cached_first);
        let second = compiler.compile(key, "fn", Vec::new());
        let (_, cached_second) = second.err().unwrap();
        assert!(cached_second);
    }

    #[test]
    fn changing_dependencies_forces_rebuild() {
        let mut compiler = IncrementalCompiler::new();
        let key = ModuleKey::new("dep-test");
        let dep_a = ModuleKey::new("dep-a");
        let dep_b = ModuleKey::new("dep-b");

        let first = compiler
            .compile(key.clone(), "fn main() {}", vec![dep_a.clone()])
            .unwrap();
        assert!(!first.1);

        let second = compiler
            .compile(key.clone(), "fn main() {}", vec![dep_a.clone()])
            .unwrap();
        assert!(second.1);

        let third = compiler
            .compile(key.clone(), "fn main() {}", vec![dep_a, dep_b])
            .unwrap();
        assert!(!third.1);
    }
}
