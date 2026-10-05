//! Simulation soundness readings of the band table (SPEC_0033): which
//! completions are neither strict-high nor covered by a typed trace
//! exception, and the package that owns each for triage.

use std::collections::BTreeMap;

use super::{BandLabel, BandRow, BandTable, ExitReason};
use crate::msl_tools::common::{TraceExceptionKind, typed_exception_kind};

/// How the band table classifies the run's completed simulations: verified
/// (strict-high), excepted (a typed trace exception, by kind), or
/// unclassified (the soundness roster). Their sum is the completions the
/// table accounts for.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SimulationOutcomes {
    pub verified: usize,
    pub excepted: BTreeMap<TraceExceptionKind, usize>,
    /// Excluded rows whose recorded reason names no known exception kind.
    pub excepted_untyped: usize,
    pub unclassified: usize,
}

impl SimulationOutcomes {
    /// Every typed or untyped excepted row.
    pub fn excepted_total(&self) -> usize {
        self.excepted.values().sum::<usize>() + self.excepted_untyped
    }

    /// Completions the band table accounts for.
    pub fn classified_total(&self) -> usize {
        self.verified + self.excepted_total() + self.unclassified
    }
}

impl BandRow {
    /// Whether this model simulated without strict-high parity and without a
    /// typed trace exception. A failed or unattempted simulation is not a
    /// completion, and an excluded row carries a typed exception; every other
    /// non-high row, including a pointwise non-identifiable one with no typed
    /// row, is a completion the gate cannot certify.
    pub fn is_unexcepted_non_high(&self) -> bool {
        if self.band == BandLabel::High {
            return false;
        }
        match self.exit_reason {
            Some(ExitReason::SimFailed | ExitReason::NotAttempted | ExitReason::Excluded) => false,
            Some(
                ExitReason::RumocaTraceMissing
                | ExitReason::ReferenceMissing
                | ExitReason::TraceMissingSideUnrecorded
                | ExitReason::ComparatorFailed
                | ExitReason::NoComparableSamples
                | ExitReason::TraceNonidentifiable
                | ExitReason::NotCompared,
            )
            | None => true,
        }
    }
}

impl BandTable {
    /// Models that simulated but are neither strict-high nor carried by a
    /// typed trace exception, in table order. The soundness rule is that this
    /// set is empty; the baseline owns the shrink-only roster of the models
    /// still in it (SPEC_0033).
    pub fn unexcepted_non_high_rows(&self) -> impl Iterator<Item = &BandRow> {
        self.rows.iter().filter(|row| row.is_unexcepted_non_high())
    }

    /// The verified / excepted / unclassified split of the run's completions.
    pub fn simulation_outcomes(&self) -> SimulationOutcomes {
        let mut outcomes = SimulationOutcomes::default();
        for row in &self.rows {
            if row.band == BandLabel::High {
                outcomes.verified += 1;
            } else if row.exit_reason == Some(ExitReason::Excluded) {
                match row.exit_detail.as_deref().and_then(typed_exception_kind) {
                    Some(kind) => *outcomes.excepted.entry(kind).or_default() += 1,
                    None => outcomes.excepted_untyped += 1,
                }
            } else if row.is_unexcepted_non_high() {
                outcomes.unclassified += 1;
            }
        }
        outcomes
    }
}

/// The package that owns a model for triage: its first three name segments
/// (`Modelica.Electrical.Analog`, `Modelica.Mechanics.MultiBody`). Parity work
/// is split by these packages, so roster listings group by them.
pub fn triage_package(model_name: &str) -> &str {
    match model_name.match_indices('.').nth(2) {
        Some((index, _)) => &model_name[..index],
        None => model_name,
    }
}
