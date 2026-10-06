//! Complete parameter-relative fragments, shared only within one immutable DAE.
use std::collections::HashMap;
use std::sync::Arc;

use crate::projection::{FunctionParameterDependency, domain_context::Context};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(in crate::projection) struct Key {
    pub(in crate::projection) activation: crate::projection::Activation,
    pub(in crate::projection) function: u32,
    pub(in crate::projection) expression: u32,
    pub(in crate::projection) field: Option<usize>,
    pub(in crate::projection) scalar: usize,
    pub(in crate::projection) parent: Arc<Context>,
}

#[derive(Default)]
pub(in crate::projection) struct Cache {
    values: HashMap<Key, Arc<[FunctionParameterDependency]>>,
    retained_charge: usize,
}

impl Cache {
    pub(in crate::projection) fn get(
        &self,
        key: &Key,
    ) -> Option<Arc<[FunctionParameterDependency]>> {
        self.values.get(key).map(Arc::clone)
    }

    pub(in crate::projection) fn insert(
        &mut self,
        key: Key,
        values: Arc<[FunctionParameterDependency]>,
    ) {
        // A performance cache cannot require unbounded retention as function
        // output cardinality grows. Reaching the cap keeps the original exact
        // uncached walk; it changes neither dependencies nor failure behavior.
        let charge = values
            .len()
            .saturating_mul(std::mem::size_of::<FunctionParameterDependency>())
            .saturating_add(2 * std::mem::size_of::<Key>() + 64);
        if self.values.len() < 65_536
            && self.retained_charge.saturating_add(charge) <= 64 * 1024 * 1024
        {
            self.values.insert(key, values);
            self.retained_charge += charge;
        }
    }
}
