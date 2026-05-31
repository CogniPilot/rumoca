use indexmap::IndexMap;
use std::borrow::Borrow;
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
struct LruEntry<T> {
    value: T,
    generation: u64,
}

#[derive(Debug, Clone)]
pub(super) struct LruMap<K, T> {
    entries: HashMap<K, LruEntry<T>>,
    recency: VecDeque<(K, u64)>,
    next_generation: u64,
}

impl<K, T> Default for LruMap<K, T> {
    fn default() -> Self {
        Self {
            entries: HashMap::new(),
            recency: VecDeque::new(),
            next_generation: 0,
        }
    }
}

impl<K, T> LruMap<K, T>
where
    K: Clone + Eq + std::hash::Hash,
{
    pub(super) fn insert(&mut self, key: K, value: T) -> Option<T> {
        let generation = self.allocate_generation();
        let old = self
            .entries
            .insert(key.clone(), LruEntry { value, generation })
            .map(|entry| entry.value);
        self.recency.push_back((key, generation));
        self.compact_recency_if_needed();
        old
    }

    pub(super) fn shift_remove<Q>(&mut self, key: &Q) -> Option<T>
    where
        K: Borrow<Q>,
        Q: Eq + std::hash::Hash + ?Sized,
    {
        self.entries.remove(key).map(|entry| entry.value)
    }

    pub(super) fn get<Q>(&self, key: &Q) -> Option<&T>
    where
        K: Borrow<Q>,
        Q: Eq + std::hash::Hash + ?Sized,
    {
        self.entries.get(key).map(|entry| &entry.value)
    }

    #[cfg(test)]
    pub(super) fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut T>
    where
        K: Borrow<Q>,
        Q: Eq + std::hash::Hash + ?Sized,
    {
        self.entries.get_mut(key).map(|entry| &mut entry.value)
    }

    pub(super) fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Eq + std::hash::Hash + ?Sized,
    {
        self.entries.contains_key(key)
    }

    pub(super) fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.keys()
    }

    #[cfg(test)]
    pub(super) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub(super) fn trim_to(&mut self, max_entries: usize) {
        while self.entries.len() > max_entries {
            let Some((key, generation)) = self.recency.pop_front() else {
                break;
            };
            if self
                .entries
                .get(&key)
                .is_some_and(|entry| entry.generation == generation)
            {
                self.entries.remove(&key);
            }
        }

        if self.entries.is_empty() {
            self.recency.clear();
        }
    }

    fn compact_recency_if_needed(&mut self) {
        let max_markers = self.entries.len().saturating_mul(4).saturating_add(64);
        if self.recency.len() <= max_markers {
            return;
        }

        let compacted = self
            .recency
            .drain(..)
            .filter(|(key, generation)| {
                self.entries
                    .get(key)
                    .is_some_and(|entry| entry.generation == *generation)
            })
            .collect();
        self.recency = compacted;
    }

    fn allocate_generation(&mut self) -> u64 {
        let generation = self.next_generation;
        self.next_generation = self.next_generation.wrapping_add(1);
        generation
    }
}

pub(crate) trait SessionLruCache {
    fn trim_lru_to(&mut self, max_entries: usize);
}

impl<K, T> SessionLruCache for IndexMap<K, T>
where
    K: Clone + std::hash::Hash + Eq,
{
    fn trim_lru_to(&mut self, max_entries: usize) {
        while self.len() > max_entries {
            let Some(oldest) = self.keys().next().cloned() else {
                break;
            };
            self.shift_remove(&oldest);
        }
    }
}

impl<K, T> SessionLruCache for LruMap<K, T>
where
    K: Clone + std::hash::Hash + Eq,
{
    fn trim_lru_to(&mut self, max_entries: usize) {
        self.trim_to(max_entries);
    }
}

#[cfg(test)]
mod tests {
    use super::LruMap;

    #[test]
    fn refreshes_under_limit_compact_stale_recency_markers() {
        let mut cache = LruMap::default();
        cache.insert("hot".to_string(), 1);

        for _ in 0..100 {
            let value = cache
                .shift_remove("hot")
                .expect("hot cache entry should remain present");
            cache.insert("hot".to_string(), value);
        }

        assert_eq!(cache.get("hot"), Some(&1));
        assert!(
            cache.recency.len() <= cache.entries.len().saturating_mul(4).saturating_add(64),
            "recency queue should stay bounded even when refreshes do not trigger eviction"
        );
    }
}
