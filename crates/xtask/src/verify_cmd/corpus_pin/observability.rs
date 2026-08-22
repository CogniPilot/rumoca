//! Whether a simulate row's pins observe the run they are pinned to.
//!
//! # The hole this closes
//!
//! [`super::trace::Trace::value_at`] holds a reading right-continuously: a
//! probe reads the last sample at or before it. That is the correct reading
//! rule, and it is also why a row that pins only variables the run never moves
//! certifies nothing. Every probe on such a row reports the initial value, so a
//! run truncated to its first sample reproduces every pinned number exactly,
//! and so would a compiler that stopped integrating after initialization. The
//! row would stay green through both.
//!
//! So a simulate row is judged observed only when its own pins can tell those
//! runs apart: some pinned variable must span, over the pinned run, more than
//! the widest tolerance that row pins it with. Movement no larger than the
//! tolerance is movement this row's own comparison cannot see, which is the
//! same thing as no movement at all as far as the evidence goes.
//!
//! An unobserved row is red, exactly like a row that pins no reading. Both
//! prove only that the process exited zero.
//!
//! This module is the single owner of that decision. The recorder in
//! [`super::record`] proposes pins through the same predicate the gate judges
//! them with, so a `--record` proposal cannot be vacuous by construction.

use std::collections::BTreeMap;

use super::manifest::PinnedObservation;
use super::trace::Trace;

/// Why a simulate row's pins say nothing about the run.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Unobserved {
    /// The row pins no reading at all.
    NoReadings,
    /// Every pinned variable holds one value across the whole run.
    EveryVariableConstant,
}

impl Unobserved {
    /// The finding text. Both reasons share the headline because they are the
    /// same fact about the evidence: this row proves only that the process
    /// exited zero.
    pub(crate) fn finding(self) -> String {
        let cause = match self {
            Self::NoReadings => "pins no reading".to_string(),
            Self::EveryVariableConstant => {
                "pins only variables that hold one value across the whole run, which a run \
                 truncated to its first sample would reproduce exactly"
                    .to_string()
            }
        };
        format!(
            "pinned to succeed but {cause}, so this row proves only that the process exited \
             zero. Pin observables the run moves, lengthening t_end if nothing moves inside \
             it, then record the readings with `--record` and copy the adjudicated numbers \
             into the manifest."
        )
    }
}

/// `None` when the pins observe the run; the reason otherwise.
pub(crate) fn assess(
    observations: &[PinnedObservation],
    trace: &Trace,
    t_end: f64,
) -> Option<Unobserved> {
    if observations.is_empty() {
        return Some(Unobserved::NoReadings);
    }
    // The widest tolerance pinned on a variable is the one that decides whether
    // this row can see the variable move: a probe whose tolerance swallows the
    // whole span reads the same verdict for a moving run and a frozen one.
    let mut widest: BTreeMap<&str, f64> = BTreeMap::new();
    for observation in observations {
        widest
            .entry(observation.variable.as_str())
            .and_modify(|tolerance| *tolerance = tolerance.max(observation.tolerance))
            .or_insert(observation.tolerance);
    }
    if widest
        .into_iter()
        .any(|(variable, tolerance)| moves(trace, variable, t_end, tolerance))
    {
        return None;
    }
    Some(Unobserved::EveryVariableConstant)
}

/// Whether `variable` spans more than `tolerance` over `[0, t_end]` of `trace`.
///
/// A variable the trace does not carry does not move: the missing-variable
/// finding is the comparison's to report, and treating the absence as movement
/// here would let a vanished variable certify the row.
pub(crate) fn moves(trace: &Trace, variable: &str, t_end: f64, tolerance: f64) -> bool {
    trace
        .spread_until(variable, t_end)
        .is_some_and(|spread| spread > tolerance)
}
