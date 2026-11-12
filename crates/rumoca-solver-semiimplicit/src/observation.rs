//! Optional counters of actual numerical requests; no clocks or model probes.

use std::{cell::Cell, rc::Rc};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CartesianWorkCounts {
    pub derivative_requests: u64,
    pub step_candidates: u64,
}

/// Read-only counterpart of the plugin's optional counters. Guest exports,
/// component-internal mass/constraint work and host observers are outside its
/// coverage and must be counted by their actual execution owner.
#[derive(Clone, Default)]
pub struct CartesianWorkObserver(pub(super) Rc<Cell<CartesianWorkCounts>>);

impl CartesianWorkObserver {
    pub fn snapshot(&self) -> CartesianWorkCounts {
        self.0.get()
    }

    pub(super) fn derivative_request(&self) {
        let mut value = self.0.get();
        value.derivative_requests += 1;
        self.0.set(value);
    }

    pub(super) fn candidate(&self) {
        let mut value = self.0.get();
        value.step_candidates += 1;
        self.0.set(value);
    }
}
