//! Exact cache keys with lazy dense storage for common plain scalar coordinates.
use std::collections::HashMap;
use std::sync::Arc;

use crate::projection::{FunctionParameterDependency, ScalarExpressionDependency};

type Values = Arc<[FunctionParameterDependency]>;
const DENSE_LIMIT: usize = 65_536;

#[derive(Debug, Default)]
pub(super) struct Cache {
    dense: Vec<Vec<[Option<Values>; 2]>>,
    sparse: HashMap<ScalarExpressionDependency, Values>,
}

impl Cache {
    pub(super) fn get(&self, key: &ScalarExpressionDependency) -> Option<&Values> {
        if plain(key)
            && let Some(values) = self
                .dense
                .get(key.expression as usize)
                .and_then(|slots| slots.get(key.scalar))
                .and_then(|modes| modes[key.activation.index()].as_ref())
        {
            return Some(values);
        }
        self.sparse.get(key)
    }

    pub(super) fn insert(&mut self, key: ScalarExpressionDependency, values: Values) {
        if !plain(&key) {
            self.sparse.insert(key, values);
            return;
        }
        let expression = key.expression as usize;
        if self.dense.len() <= expression {
            self.dense.resize_with(expression + 1, Vec::new);
        }
        let slots = &mut self.dense[expression];
        // A sparse high address must not allocate a whole tensor's capacity.
        // Sequential scalar views grow lazily; isolated gaps retain hashing.
        if key.scalar > slots.len().saturating_add(64) {
            self.sparse.insert(key, values);
            return;
        }
        if slots.len() <= key.scalar {
            slots.resize_with(key.scalar + 1, Default::default);
        }
        slots[key.scalar][key.activation.index()] = Some(values);
    }
}

fn plain(key: &ScalarExpressionDependency) -> bool {
    key.field.is_none()
        && key.domain_context == Default::default()
        && (key.expression as usize) < DENSE_LIMIT
        && key.scalar < DENSE_LIMIT
}

#[derive(Debug, Default)]
pub(super) struct Eligibility {
    dense: Vec<Option<bool>>,
    sparse: HashMap<u32, bool>,
}

impl Eligibility {
    pub(super) fn get(&self, expression: u32) -> Option<bool> {
        self.dense
            .get(expression as usize)
            .copied()
            .flatten()
            .or_else(|| self.sparse.get(&expression).copied())
    }

    pub(super) fn insert(&mut self, expression: u32, value: bool) {
        let index = expression as usize;
        if index >= DENSE_LIMIT {
            self.sparse.insert(expression, value);
            return;
        }
        if self.dense.len() <= index {
            self.dense.resize(index + 1, None);
        }
        self.dense[index] = Some(value);
    }
}
