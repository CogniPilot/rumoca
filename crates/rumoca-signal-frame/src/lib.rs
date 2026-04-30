//! Transport-neutral named-signal frame.
//!
//! Defines the `SignalFrame` shape — a named map of f64 signals — shared
//! by codec impls (`rumoca-codec-flatbuffers`, future
//! `rumoca-codec-protobuf`, etc.) and runtime adapters (`rumoca-input`,
//! `rumoca-sim`). Lives in its own crate so impl crates and consumer
//! crates can share the type without depending on each other.

use std::ops::Index;

use indexmap::IndexMap;

/// A transport-neutral frame of named scalar signals exchanged in lockstep.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SignalFrame {
    values: IndexMap<String, f64>,
}

impl SignalFrame {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: IndexMap::with_capacity(capacity),
        }
    }

    pub fn insert(&mut self, name: impl Into<String>, value: f64) -> Option<f64> {
        self.values.insert(name.into(), value)
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<&f64> {
        self.values.get(name)
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, f64)> {
        self.values
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
    }
}

impl<'a> IntoIterator for &'a SignalFrame {
    type Item = (&'a String, &'a f64);
    type IntoIter = indexmap::map::Iter<'a, String, f64>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.iter()
    }
}

impl Index<&str> for SignalFrame {
    type Output = f64;

    fn index(&self, index: &str) -> &Self::Output {
        &self.values[index]
    }
}
