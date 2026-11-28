//! Bounded in-process reuse of Cranelift compiler stencils across fresh JIT modules.

use cranelift_codegen::{Context, incremental_cache::CacheKvStore, isa::TargetIsa};
use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, VecDeque},
    num::NonZeroUsize,
    sync::{
        Arc, Mutex,
        atomic::{AtomicU64, Ordering},
    },
};

type Key = [u8; 32];

#[derive(Default)]
struct Entries {
    values: HashMap<Key, Vec<u8>>,
    insertion_order: VecDeque<Key>,
    bytes: usize,
}

/// Share compiler-generated relocatable stencils only within this process.
/// Every module defines its own functions and applies its own relocations.
#[derive(Clone)]
pub struct NativeCompilationCache(Store);

#[derive(Clone)]
struct Store {
    entries: Arc<Mutex<Entries>>,
    budget: NonZeroUsize,
}

impl NativeCompilationCache {
    pub fn new(byte_budget: NonZeroUsize) -> Self {
        Self(Store {
            entries: Default::default(),
            budget: byte_budget,
        })
    }

    /// Enable reuse for this synchronous construction and its parallel emission rounds.
    pub fn during<R>(&self, construct: impl FnOnce() -> R) -> (R, NativeCompilationUsage) {
        let scope = CompileScope {
            store: self.0.clone(),
            counts: Default::default(),
        };
        let _restore = Restore(ACTIVE.with(|active| active.replace(Some(scope.clone()))));
        let result = construct();
        (
            result,
            NativeCompilationUsage {
                hits: scope.counts.hits.load(Ordering::Relaxed),
                compiled: scope.counts.compiled.load(Ordering::Relaxed),
            },
        )
    }
}

/// Functions reused and freshly compiled within one construction.
pub struct NativeCompilationUsage {
    pub hits: u64,
    pub compiled: u64,
}

impl CacheKvStore for Store {
    fn get(&self, key: &[u8]) -> Option<Cow<'_, [u8]>> {
        let entries = self.entries.lock().unwrap_or_else(|e| e.into_inner());
        entries.values.get(key).cloned().map(Cow::Owned)
    }

    fn insert(&mut self, key: &[u8], bytes: Vec<u8>) {
        let Ok(key) = Key::try_from(key) else { return };
        let weight = bytes.len() + key.len();
        let Some(available) = self.budget.get().checked_sub(weight) else {
            return;
        };
        let mut entries = self.entries.lock().unwrap_or_else(|e| e.into_inner());
        if entries.values.contains_key(&key) {
            return;
        }
        while entries.bytes > available {
            let oldest = entries
                .insertion_order
                .pop_front()
                .expect("accounted stencil");
            let removed = entries.values.remove(&oldest).expect("queued stencil");
            entries.bytes -= removed.len() + oldest.len();
        }
        entries.bytes += weight;
        entries.insertion_order.push_back(key);
        entries.values.insert(key, bytes);
    }
}

#[derive(Default)]
struct Counts {
    hits: AtomicU64,
    compiled: AtomicU64,
}

#[derive(Clone)]
pub(crate) struct CompileScope {
    store: Store,
    counts: Arc<Counts>,
}

thread_local! {
    static ACTIVE: RefCell<Option<CompileScope>> = const { RefCell::new(None) };
}

pub(crate) fn current() -> Option<CompileScope> {
    ACTIVE.with(|active| active.borrow().clone())
}

struct Restore(Option<CompileScope>);

impl Drop for Restore {
    fn drop(&mut self) {
        ACTIVE.with(|active| active.replace(self.0.take()));
    }
}

pub(crate) fn compile(
    scope: Option<&CompileScope>,
    context: &mut Context,
    target: &dyn TargetIsa,
) -> Result<(), String> {
    let Some(scope) = scope else {
        return context
            .compile(target, &mut Default::default())
            .map(|_| ())
            .map_err(|e| format!("{:?}", e.inner));
    };
    let (_, hit) = context
        .compile_with_cache(target, &mut scope.store.clone(), &mut Default::default())
        .map_err(|e| format!("{:?}", e.inner))?;
    let counter = if hit {
        &scope.counts.hits
    } else {
        &scope.counts.compiled
    };
    counter.fetch_add(1, Ordering::Relaxed);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bounded_stencil_store_evicts_oldest_and_declines_oversized_entries() {
        let cache = NativeCompilationCache::new(NonZeroUsize::new(70).unwrap());
        let mut store = cache.0;
        store.insert(&[1; 32], vec![1; 3]);
        store.insert(&[2; 32], vec![2; 3]);
        assert_eq!(store.get(&[1; 32]).unwrap().as_ref(), &[1; 3]);
        store.insert(&[3; 32], vec![3; 3]);
        assert!(store.get(&[1; 32]).is_none());
        assert!(store.get(&[2; 32]).is_some());
        store.insert(&[4; 32], vec![4; 70]);
        assert!(store.get(&[4; 32]).is_none());
        assert_eq!(store.entries.lock().unwrap().bytes, 70);
    }

    #[test]
    fn nested_or_unwound_scope_restores_the_parent() {
        let outer = NativeCompilationCache::new(NonZeroUsize::new(70).unwrap());
        let inner = NativeCompilationCache::new(NonZeroUsize::new(70).unwrap());
        assert!(current().is_none());
        outer.during(|| {
            let parent = current().unwrap();
            inner.during(|| {
                assert!(!Arc::ptr_eq(
                    &current().unwrap().store.entries,
                    &parent.store.entries
                ));
            });
            assert!(Arc::ptr_eq(
                &current().unwrap().store.entries,
                &parent.store.entries
            ));
            let result = std::panic::catch_unwind(|| inner.during(|| panic!("scope unwind")));
            assert!(result.is_err());
            assert!(Arc::ptr_eq(
                &current().unwrap().store.entries,
                &parent.store.entries
            ));
            std::thread::spawn(|| assert!(current().is_none()))
                .join()
                .unwrap();
        });
        assert!(current().is_none());
    }
}
