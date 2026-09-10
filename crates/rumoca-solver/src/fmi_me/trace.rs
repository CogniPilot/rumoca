//! The one session-owned trace policy (SPEC_0044 §6, SPEC_0050).
//!
//! Roles are conclusions of the master algorithm that produced an observation,
//! not properties of storage, so they are decided here and nowhere else. A
//! numerical plugin cannot label an observation, choose its coordinate, or
//! append a row: two conforming hosts therefore cannot describe the same
//! physical event differently.
//!
//! The recorder reaches its decision from `(role, coordinate)` and the trace's
//! own tail **before** the component is evaluated. SPEC_0050's "exact duplicate
//! nominal suppresses without reevaluation" is therefore satisfied literally: a
//! suppressed row never runs an FMI getter at all.

use super::integrator::canonical_coordinate;
use crate::{
    solver::{SimResult, SimTermination, SimVariableMeta},
    timeline::sample_time_match_with_tol,
};

/// What the master algorithm concluded an observation is.
///
/// Deliberately host-private: public role construction would let a client mint
/// evidence the master algorithm never concluded. The variants carry no derived
/// ordering — the replacement policy is the explicit matrix in
/// [`replacement_decision`], not an enum discriminant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TraceObservationRole {
    /// Produced by the initial event iteration at `startTime`.
    Initialization,
    /// A soft output-grid sample of the continuous trajectory.
    Nominal,
    /// The left limit of an event instant, observed before Event Mode.
    EventLeft,
    /// The settled value at an event instant, observed after the discrete
    /// iteration converged.
    Settled,
}

/// What the recorder concluded about a candidate row, before evaluation.
///
/// Private to this module: a decision is a fact about the trace *tail* at the
/// instant it was reached, so handing a copyable decision to a sibling would
/// let it be applied after the tail changed. [`MeTraceRecorder::record_with`]
/// is the only mutation entry, and it reaches and applies the decision without
/// ever exposing it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TraceDecision {
    /// Evaluate and append a new row.
    Append,
    /// Evaluate and replace the trailing row at the same coordinate.
    ReplaceLast,
    /// Do not evaluate; the trace already carries this instant's evidence.
    Suppress,
}

/// Why a row was not appended, or why the trace is malformed.
///
/// Host-recorder machinery, and therefore host-private: the recorder's concrete
/// failure shape is not part of the common solver API. The session maps an
/// allocation violation onto the public allocation category and every invariant
/// violation onto the public host-contract category.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub(super) enum MeTraceViolation {
    #[error("trace channel names must be unique; '{name}' appears more than once")]
    DuplicateChannel { name: String },

    #[error("trace row at t={time} carries {actual} values for {expected} channels")]
    RowWidth {
        time: f64,
        actual: usize,
        expected: usize,
    },

    #[error("trace row time {time} must be finite")]
    NonFiniteTime { time: f64 },

    #[error("trace row value {index} at t={time} must be finite")]
    NonFiniteValue { index: usize, time: f64 },

    #[error(
        "trace row at t={time} regresses behind t={previous} and the preceding \
         row does not prove one semantic instant"
    )]
    TimeRegression { time: f64, previous: f64 },

    #[error(
        "trace row at t={time} repeats evidence the trace already published \
         there, and SPEC_0050 defines no replacement for it"
    )]
    InconsistentRepeatEvidence { time: f64 },

    #[error("{context} allocation failed for {entries} entries")]
    Allocation {
        context: &'static str,
        entries: usize,
    },
}

/// The normative SPEC_0050 replacement matrix at one exact coordinate.
///
/// It encodes exactly the three authorized cases and nothing else. `None` is a
/// typed rejection, never a repair and never an invented suppression: the
/// master algorithm is responsible for not generating a candidate the catalog
/// does not describe.
///
/// | existing \ incoming | Initialization | Nominal  | EventLeft | Settled |
/// |---|---|---|---|---|
/// | Initialization | reject   | reject   | reject   | replace |
/// | Nominal        | reject   | suppress | reject   | replace |
/// | EventLeft      | reject   | reject   | reject   | replace |
/// | Settled        | reject   | reject   | suppress | reject  |
///
/// - *settled replaces initialization, event-left, or nominal* — SPEC_0050.
/// - *event-left never replaces settled* — SPEC_0050, encoded as suppression.
/// - *exact duplicate nominal suppresses without reevaluation* — SPEC_0050.
///
/// Everything else at one coordinate is repeat evidence the catalog does not
/// authorize. Widening this matrix requires amending SPEC_0050 first.
const fn replacement_decision(
    existing: TraceObservationRole,
    incoming: TraceObservationRole,
) -> Option<TraceDecision> {
    use TraceObservationRole::{EventLeft, Initialization, Nominal, Settled};
    match (existing, incoming) {
        (Settled, Settled) => None,
        (Initialization | Nominal | EventLeft, Settled) => Some(TraceDecision::ReplaceLast),
        (Settled, EventLeft) => Some(TraceDecision::Suppress),
        (Nominal, Nominal) => Some(TraceDecision::Suppress),
        _ => None,
    }
}

/// The session-owned trace evidence store.
///
/// Construction is host-only and checked; the recorder emits a [`SimResult`]
/// only after a successful construction, so a malformed trace cannot become a
/// published result.
pub(super) struct MeTraceRecorder {
    names: Vec<String>,
    meta: Vec<SimVariableMeta>,
    times: Vec<f64>,
    roles: Vec<TraceObservationRole>,
    columns: Vec<Vec<f64>>,
    state_count: usize,
}

impl MeTraceRecorder {
    /// Construct a recorder for `names`, rejecting duplicate channels and a
    /// failed reservation rather than aborting.
    pub(super) fn new(
        names: Vec<String>,
        meta: Vec<SimVariableMeta>,
        state_count: usize,
        capacity: usize,
    ) -> Result<Self, MeTraceViolation> {
        for (index, name) in names.iter().enumerate() {
            if names[..index].contains(name) {
                return Err(MeTraceViolation::DuplicateChannel { name: name.clone() });
            }
        }
        let mut columns: Vec<Vec<f64>> = Vec::new();
        try_reserve(&mut columns, names.len(), "trace columns")?;
        for _ in 0..names.len() {
            let mut column = Vec::new();
            try_reserve(&mut column, capacity, "trace column samples")?;
            columns.push(column);
        }
        let mut times = Vec::new();
        try_reserve(&mut times, capacity, "trace times")?;
        let mut roles = Vec::new();
        try_reserve(&mut roles, capacity, "trace roles")?;
        Ok(Self {
            names,
            meta,
            times,
            roles,
            columns,
            state_count,
        })
    }

    /// The coordinate of the most recently retained row, if any.
    #[must_use]
    pub(super) fn last_time(&self) -> Option<f64> {
        self.times.last().copied()
    }

    /// Discard every retained row, keeping the reserved capacity.
    ///
    /// A session reset clears its own evidence with the rest of its caches; the
    /// reserved columns stay rectangular and reusable.
    pub(super) fn clear(&mut self) {
        self.times.clear();
        self.roles.clear();
        for column in &mut self.columns {
            column.clear();
        }
    }

    /// Conclude what a row at `(role, time)` would do, *before* the component
    /// is evaluated.
    fn decide(
        &self,
        role: TraceObservationRole,
        time: f64,
    ) -> Result<TraceDecision, MeTraceViolation> {
        if !time.is_finite() {
            return Err(MeTraceViolation::NonFiniteTime { time });
        }
        let (Some(previous_time), Some(previous_role)) =
            (self.times.last().copied(), self.roles.last().copied())
        else {
            return Ok(TraceDecision::Append);
        };

        if time < previous_time {
            // SPEC_0050: a regression is admissible only behind a settled row
            // that the shared predicate proves is the same semantic instant,
            // and that settled row stays exactly as it is.
            if previous_role == TraceObservationRole::Settled
                && sample_time_match_with_tol(previous_time, time)
            {
                return Ok(TraceDecision::Suppress);
            }
            return Err(MeTraceViolation::TimeRegression {
                time,
                previous: previous_time,
            });
        }

        if time.to_bits() != previous_time.to_bits() {
            return Ok(TraceDecision::Append);
        }

        replacement_decision(previous_role, role)
            .ok_or(MeTraceViolation::InconsistentRepeatEvidence { time })
    }

    /// Retain one observation under the SPEC_0050 role-aware policy, running
    /// `evaluate` only when the decision actually needs values.
    ///
    /// Returns whether the row entered the trace. A suppressed row is not a
    /// failure: it is the policy's answer for evidence the trace already
    /// carries at that exact instant, and it costs no FMI getter.
    pub(super) fn record_with<E, F>(
        &mut self,
        role: TraceObservationRole,
        time: f64,
        evaluate: F,
    ) -> Result<bool, E>
    where
        E: From<MeTraceViolation>,
        F: FnOnce() -> Result<Vec<f64>, E>,
    {
        let time = canonical_coordinate(time);
        let decision = self.decide(role, time)?;
        if decision == TraceDecision::Suppress {
            return Ok(false);
        }
        let values = evaluate()?;
        self.commit(decision, role, time, &values)?;
        Ok(true)
    }

    /// Retain one observation whose values the host already holds as a slice.
    ///
    /// This is the atomic form of the same operation: it decides, fallibly
    /// reserves, and commits in one call, so a caller that already owns the
    /// values never has to copy them into a fresh `Vec` through an infallible
    /// allocation just to satisfy the closure signature (ME-INT-003). The split
    /// decide/commit protocol stays private.
    pub(super) fn record_slice(
        &mut self,
        role: TraceObservationRole,
        time: f64,
        values: &[f64],
    ) -> Result<bool, MeTraceViolation> {
        let time = canonical_coordinate(time);
        let decision = self.decide(role, time)?;
        if decision == TraceDecision::Suppress {
            return Ok(false);
        }
        self.commit(decision, role, time, values)?;
        Ok(true)
    }

    /// Apply a decision reached by [`Self::decide`] to already-read values.
    fn commit(
        &mut self,
        decision: TraceDecision,
        role: TraceObservationRole,
        time: f64,
        values: &[f64],
    ) -> Result<(), MeTraceViolation> {
        if values.len() != self.names.len() {
            return Err(MeTraceViolation::RowWidth {
                time,
                actual: values.len(),
                expected: self.names.len(),
            });
        }
        if let Some(index) = values.iter().position(|value| !value.is_finite()) {
            return Err(MeTraceViolation::NonFiniteValue { index, time });
        }
        match decision {
            TraceDecision::Append => self.append(role, time, values),
            TraceDecision::ReplaceLast => {
                self.replace_last(role, time, values);
                Ok(())
            }
            TraceDecision::Suppress => Ok(()),
        }
    }

    /// Grow every column and the time/role axes atomically, so a failed
    /// reservation leaves a rectangular trace behind.
    fn append(
        &mut self,
        role: TraceObservationRole,
        time: f64,
        values: &[f64],
    ) -> Result<(), MeTraceViolation> {
        let next = self.times.len() + 1;
        reserve_one(&mut self.times, next, "trace times")?;
        reserve_one(&mut self.roles, next, "trace roles")?;
        for column in &mut self.columns {
            reserve_one(column, next, "trace column samples")?;
        }
        self.times.push(time);
        self.roles.push(role);
        for (column, value) in self.columns.iter_mut().zip(values) {
            column.push(*value);
        }
        Ok(())
    }

    fn replace_last(&mut self, role: TraceObservationRole, time: f64, values: &[f64]) {
        if let Some(slot) = self.times.last_mut() {
            *slot = time;
        }
        if let Some(slot) = self.roles.last_mut() {
            *slot = role;
        }
        for (column, value) in self.columns.iter_mut().zip(values) {
            if let Some(slot) = column.last_mut() {
                *slot = *value;
            }
        }
    }

    /// Emit the published result. Only a successfully constructed and
    /// consistently grown recorder can reach this point.
    #[must_use]
    pub(super) fn finish(self, termination: Option<SimTermination>) -> SimResult {
        SimResult {
            times: self.times,
            names: self.names,
            data: self.columns,
            n_states: self.state_count,
            variable_meta: self.meta,
            termination,
        }
    }
}

fn try_reserve<T>(
    values: &mut Vec<T>,
    entries: usize,
    context: &'static str,
) -> Result<(), MeTraceViolation> {
    values
        .try_reserve(entries)
        .map_err(|_| MeTraceViolation::Allocation { context, entries })
}

fn reserve_one<T>(
    values: &mut Vec<T>,
    entries: usize,
    context: &'static str,
) -> Result<(), MeTraceViolation> {
    if values.len() < values.capacity() {
        return Ok(());
    }
    try_reserve(values, 1, context).map_err(|_| MeTraceViolation::Allocation { context, entries })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    fn recorder() -> MeTraceRecorder {
        MeTraceRecorder::new(vec!["a".to_string(), "b".to_string()], Vec::new(), 0, 4)
            .expect("fixture channels are unique")
    }

    fn record(
        trace: &mut MeTraceRecorder,
        role: TraceObservationRole,
        time: f64,
        values: [f64; 2],
    ) -> Result<bool, MeTraceViolation> {
        trace.record_with(role, time, || Ok(values.to_vec()))
    }

    #[test]
    fn duplicate_channels_are_rejected_at_construction() {
        let duplicate =
            MeTraceRecorder::new(vec!["a".to_string(), "a".to_string()], Vec::new(), 0, 1);
        assert!(matches!(
            duplicate,
            Err(MeTraceViolation::DuplicateChannel { .. })
        ));
    }

    #[test]
    fn malformed_rows_are_rejected_rather_than_repaired() {
        let mut trace = recorder();
        assert!(matches!(
            record(
                &mut trace,
                TraceObservationRole::Nominal,
                f64::NAN,
                [0.0, 0.0]
            ),
            Err(MeTraceViolation::NonFiniteTime { .. })
        ));
        assert!(matches!(
            trace.record_with::<MeTraceViolation, _>(TraceObservationRole::Nominal, 0.0, || Ok(
                vec![0.0]
            )),
            Err(MeTraceViolation::RowWidth { .. })
        ));
        assert!(matches!(
            record(
                &mut trace,
                TraceObservationRole::Nominal,
                0.0,
                [0.0, f64::INFINITY]
            ),
            Err(MeTraceViolation::NonFiniteValue { .. })
        ));
    }

    #[test]
    fn a_settled_row_replaces_a_lower_role_at_the_same_coordinate() {
        let mut trace = recorder();
        assert!(record(&mut trace, TraceObservationRole::Nominal, 0.5, [1.0, 1.0]).unwrap());
        assert!(record(&mut trace, TraceObservationRole::Settled, 0.5, [2.0, 2.0]).unwrap());
        assert_eq!(trace.times, vec![0.5]);
        assert_eq!(trace.columns, vec![vec![2.0], vec![2.0]]);
        assert_eq!(trace.roles, vec![TraceObservationRole::Settled]);
    }

    #[test]
    fn an_event_left_row_never_replaces_a_settled_row() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Settled, 0.5, [2.0, 2.0]).unwrap();
        assert!(!record(&mut trace, TraceObservationRole::EventLeft, 0.5, [1.0, 1.0]).unwrap());
        assert_eq!(trace.columns, vec![vec![2.0], vec![2.0]]);
    }

    #[test]
    fn a_second_settled_row_at_one_coordinate_is_a_typed_rejection() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Settled, 0.5, [2.0, 2.0]).unwrap();
        assert!(matches!(
            record(&mut trace, TraceObservationRole::Settled, 0.5, [3.0, 3.0]),
            Err(MeTraceViolation::InconsistentRepeatEvidence { .. })
        ));
    }

    #[test]
    fn a_repeated_initialization_row_is_a_typed_rejection() {
        let mut trace = recorder();
        record(
            &mut trace,
            TraceObservationRole::Initialization,
            0.0,
            [1.0, 1.0],
        )
        .unwrap();
        assert!(matches!(
            record(
                &mut trace,
                TraceObservationRole::Initialization,
                0.0,
                [1.0, 1.0]
            ),
            Err(MeTraceViolation::InconsistentRepeatEvidence { .. })
        ));
    }

    #[test]
    fn an_exact_duplicate_nominal_is_suppressed_without_reevaluation() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Nominal, 0.5, [1.0, 1.0]).unwrap();
        let evaluations = Cell::new(0usize);
        let retained = trace
            .record_with::<MeTraceViolation, _>(TraceObservationRole::Nominal, 0.5, || {
                evaluations.set(evaluations.get() + 1);
                Ok(vec![9.0, 9.0])
            })
            .expect("a duplicate nominal is suppressed, not rejected");
        assert!(!retained);
        assert_eq!(
            evaluations.get(),
            0,
            "SPEC_0050 requires suppression without reevaluation"
        );
        assert_eq!(trace.times.len(), 1);
        assert_eq!(trace.columns, vec![vec![1.0], vec![1.0]]);
    }

    #[test]
    fn a_drifted_repeat_nominal_never_rewrites_published_evidence() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Nominal, 0.5, [1.0, 1.0]).unwrap();
        assert!(!record(&mut trace, TraceObservationRole::Nominal, 0.5, [1.5, 1.5]).unwrap());
        assert_eq!(trace.columns, vec![vec![1.0], vec![1.0]]);
    }

    #[test]
    fn a_regression_behind_a_settled_row_at_one_instant_keeps_that_row_unchanged() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Settled, 0.5, [2.0, 2.0]).unwrap();
        assert!(
            !record(
                &mut trace,
                TraceObservationRole::Nominal,
                f64::from_bits(0.5_f64.to_bits() - 1),
                [9.0, 9.0]
            )
            .expect("a proven same-instant regression is suppressed")
        );
        assert_eq!(trace.times, vec![0.5]);
        assert_eq!(trace.columns, vec![vec![2.0], vec![2.0]]);
    }

    #[test]
    fn a_real_time_regression_is_a_typed_failure() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Settled, 0.5, [2.0, 2.0]).unwrap();
        assert!(matches!(
            record(&mut trace, TraceObservationRole::Nominal, 0.4, [1.0, 1.0]),
            Err(MeTraceViolation::TimeRegression { .. })
        ));
    }

    #[test]
    fn a_regression_behind_a_non_settled_row_is_a_typed_failure() {
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::EventLeft, 0.5, [2.0, 2.0]).unwrap();
        assert!(matches!(
            record(
                &mut trace,
                TraceObservationRole::Nominal,
                f64::from_bits(0.5_f64.to_bits() - 1),
                [1.0, 1.0]
            ),
            Err(MeTraceViolation::TimeRegression { .. })
        ));
    }

    #[test]
    fn the_canonical_scheduled_event_pair_is_strictly_nondecreasing() {
        let mut trace = recorder();
        let event = 0.5_f64;
        let left = crate::timeline::event_left_limit_time(event);
        record(
            &mut trace,
            TraceObservationRole::EventLeft,
            left,
            [0.0, 0.0],
        )
        .unwrap();
        record(
            &mut trace,
            TraceObservationRole::Settled,
            event,
            [24.0, 24.0],
        )
        .unwrap();
        assert!(trace.times.windows(2).all(|pair| pair[0] <= pair[1]));
        assert_eq!(trace.times, vec![left, event]);
        assert_eq!(trace.columns[0], vec![0.0, 24.0]);
    }

    /// Exactly the three SPEC_0050 cases, and nothing else.
    ///
    /// - settled replaces initialization, event-left, or nominal
    /// - event-left never replaces settled
    /// - exact duplicate nominal suppresses without reevaluation
    fn authorized_decision(
        existing: TraceObservationRole,
        incoming: TraceObservationRole,
    ) -> Option<TraceDecision> {
        use TraceObservationRole::{EventLeft, Initialization, Nominal, Settled};
        match (existing, incoming) {
            (Initialization | Nominal | EventLeft, Settled) => Some(TraceDecision::ReplaceLast),
            (Settled, EventLeft) | (Nominal, Nominal) => Some(TraceDecision::Suppress),
            _ => None,
        }
    }

    #[test]
    fn the_replacement_matrix_encodes_only_the_three_authorized_spec_0050_cases() {
        use TraceObservationRole::{EventLeft, Initialization, Nominal, Settled};
        let roles = [Initialization, Nominal, EventLeft, Settled];
        let mut authorized = 0usize;
        for existing in roles {
            for incoming in roles {
                let expected = authorized_decision(existing, incoming);
                assert_eq!(
                    replacement_decision(existing, incoming),
                    expected,
                    "({existing:?}, {incoming:?}) must not be silently resolved"
                );
                authorized += usize::from(expected.is_some());
            }
        }
        assert_eq!(authorized, 5);
    }

    #[test]
    fn clearing_keeps_the_trace_rectangular_and_reusable() {
        let mut trace = recorder();
        record(
            &mut trace,
            TraceObservationRole::Initialization,
            0.0,
            [1.0, 2.0],
        )
        .unwrap();
        trace.clear();
        assert!(trace.times.is_empty());
        assert_eq!(trace.columns.len(), 2);
        assert!(trace.columns.iter().all(Vec::is_empty));
        assert_eq!(trace.last_time(), None);
        record(
            &mut trace,
            TraceObservationRole::Initialization,
            5.0,
            [3.0, 4.0],
        )
        .unwrap();
        assert_eq!(trace.times, vec![5.0]);
        assert_eq!(trace.columns, vec![vec![3.0], vec![4.0]]);
    }

    #[test]
    fn negative_zero_is_the_same_coordinate_as_positive_zero() {
        // Numerically equal, bitwise different. Without canonicalization the
        // recorder would append two rows at one FMI time.
        let mut trace = recorder();
        record(&mut trace, TraceObservationRole::Nominal, -0.0, [1.0, 1.0]).unwrap();
        assert_eq!(trace.times, vec![0.0]);
        assert_eq!(trace.times[0].to_bits(), 0.0_f64.to_bits());
        assert!(!record(&mut trace, TraceObservationRole::Nominal, 0.0, [1.0, 1.0]).unwrap());
        assert_eq!(trace.times.len(), 1);
        // And the settled replacement still recognises it as one coordinate.
        assert!(record(&mut trace, TraceObservationRole::Settled, -0.0, [7.0, 7.0]).unwrap());
        assert_eq!(trace.times, vec![0.0]);
        assert_eq!(trace.columns, vec![vec![7.0], vec![7.0]]);
    }

    #[test]
    fn the_published_result_carries_the_recorded_shape() {
        let mut trace = recorder();
        record(
            &mut trace,
            TraceObservationRole::Initialization,
            0.0,
            [1.0, 2.0],
        )
        .unwrap();
        let result = trace.finish(None);
        assert_eq!(result.times, vec![0.0]);
        assert_eq!(result.data, vec![vec![1.0], vec![2.0]]);
        assert_eq!(result.names, vec!["a".to_string(), "b".to_string()]);
    }
}
