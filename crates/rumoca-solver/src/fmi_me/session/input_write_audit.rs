//! Test-only audit of the two input write paths (SPEC_0044 ME-HOST-001).
//!
//! A changed input batch either stays in Continuous-Time Mode or runs an
//! input event. Tests force the event path on one thread, count which path
//! each changed batch took, and compare the bits both paths leave behind.
//! The surface exists only for this crate's tests and for the dev-only
//! `input-write-audit` feature.

use std::cell::Cell;

use super::{MeSessionError, MeSimulationSession};

thread_local! {
    static FORCE_INPUT_EVENT: Cell<bool> = const { Cell::new(false) };
    static INPUT_WRITES: Cell<[usize; 2]> = const { Cell::new([0; 2]) };
}

/// Route every changed input batch on this thread through the input event.
pub fn force_input_event(forced: bool) {
    FORCE_INPUT_EVENT.with(|force| force.set(forced));
}

pub(crate) fn input_event_forced() -> bool {
    FORCE_INPUT_EVENT.with(Cell::get)
}

pub(crate) fn count_input_write(continuous: bool) {
    INPUT_WRITES.with(|count| {
        let mut writes = count.get();
        writes[usize::from(!continuous)] += 1;
        count.set(writes);
    });
}

/// The changed input batches on this thread so far, as
/// `[continuous, input event]`.
#[must_use]
pub fn input_write_counts() -> [usize; 2] {
    INPUT_WRITES.with(Cell::get)
}

/// The bits a session holds at its accepted point.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InputWriteAudit {
    pub time: u64,
    pub states: Vec<u64>,
    pub parameters: Vec<u64>,
    pub derivatives: Vec<u64>,
    pub indicators: Vec<u64>,
    pub next_event_time: Option<u64>,
    pub visible: Vec<(String, u64)>,
    /// Whether the session stands on a deferred live yield.
    pub deferred_yield: bool,
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

impl MeSimulationSession<'_, '_> {
    /// Whether a write to each declared input scalar, in declaration order,
    /// can reach an event owner in the same instant.
    pub fn input_event_reach(&self) -> Result<Vec<bool>, MeSessionError> {
        Ok(self.host.kernel.borrow()?.audit_input_event_reach())
    }

    /// Read the audit at the session's accepted point.
    pub fn input_write_audit(&self) -> Result<InputWriteAudit, MeSessionError> {
        let visible = self
            .visible_values()?
            .into_iter()
            .map(|(name, value)| (name, value.to_bits()))
            .collect();
        let (parameters, derivatives, indicators) = {
            let kernel = self.host.kernel.borrow()?;
            let mut derivatives = Vec::new();
            kernel.get_continuous_state_derivatives(&mut derivatives)?;
            let mut indicators = Vec::new();
            kernel.get_event_indicators(&mut indicators)?;
            (
                bits(kernel.audit_parameters()),
                bits(&derivatives),
                bits(&indicators),
            )
        };
        Ok(InputWriteAudit {
            time: self.host.time.to_bits(),
            states: bits(&self.host.states),
            parameters,
            derivatives,
            indicators,
            next_event_time: self.host.next_event_time.map(f64::to_bits),
            visible,
            deferred_yield: self.deferred.is_some(),
        })
    }
}
