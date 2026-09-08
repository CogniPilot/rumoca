//! The checked host-facing session vocabulary: experiment options, the soft
//! output cursor, and what one `advance_to` stopped on.
//!
//! Split out of [`super`] so the master algorithm stays readable inside the
//! SPEC_0021 file limits. Every aggregate here has private fields and a checked
//! constructor, so an unproven host option can never enter the session

use super::MeSessionError;
use crate::fmi_me::{MeSolverTolerances, MeToleranceError};

/// The plain host request a checked [`MeSessionOptions`] is built from.
///
/// Public fields make the request ordinary data; the aggregate below is the
/// only thing the master algorithm ever reads, and it exists only after every
/// value has been proven.
///
/// A session request deliberately has no start-time author; the retained FMI
/// component supplies that coordinate when the combined host is constructed.
///
/// ```compile_fail,E0026
/// use rumoca_solver::fmi_me::session::MeSessionOptionsInput;
///
/// fn no_second_start_author(input: MeSessionOptionsInput) {
///     let MeSessionOptionsInput { start_time: _, .. } = input;
/// }
/// ```
#[derive(Debug, Clone)]
pub struct MeSessionOptionsInput {
    /// The defined experiment end, or `None` for an open live session.
    pub stop_time: Option<f64>,
    pub relative_tolerance: f64,
    pub absolute_tolerance: f64,
    /// Requested trace output cadence. It is *not* the root-search resolution.
    pub output_interval: f64,
    /// The width bound on one adjacent sampled interval inside an accepted
    /// step. SPEC_0044 §6 makes this a distinct session option so trace
    /// density cannot change event semantics.
    pub root_scan_resolution: f64,
    /// The bracket width a located root must be refined to.
    pub root_location_tolerance: f64,
    pub max_wall_seconds: Option<f64>,
    /// Whether the session retains trace evidence. A live session that never
    /// publishes a `SimResult` retains none.
    pub records_trace: bool,
}

/// Checked host options the session derives its policies from.
#[derive(Debug, Clone)]
pub struct MeSessionOptions {
    stop_time: Option<f64>,
    tolerances: MeSolverTolerances,
    output_interval: f64,
    root_scan_resolution: f64,
    root_location_tolerance: f64,
    max_wall_seconds: Option<f64>,
    records_trace: bool,
}

impl MeSessionOptions {
    /// Prove every host option before a session can exist.
    pub fn new(input: MeSessionOptionsInput) -> Result<Self, MeSessionError> {
        let reject = |reason: String| MeSessionError::Options { reason };
        let tolerances =
            MeSolverTolerances::check(input.relative_tolerance, input.absolute_tolerance)
                .map_err(|error: MeToleranceError| reject(error.to_string()))?;
        if let Some(stop) = input.stop_time
            && !stop.is_finite()
        {
            return Err(reject(format!(
                "the defined experiment end {stop} must be finite"
            )));
        }
        for (label, value) in [
            ("output interval", input.output_interval),
            ("root scan resolution", input.root_scan_resolution),
            ("root location tolerance", input.root_location_tolerance),
        ] {
            if !value.is_finite() || value <= 0.0 {
                return Err(reject(format!(
                    "the session {label} must be finite and positive, got {value}"
                )));
            }
        }
        if input.root_location_tolerance > input.root_scan_resolution {
            return Err(reject(format!(
                "the root location tolerance {} must not exceed the scan resolution {}",
                input.root_location_tolerance, input.root_scan_resolution
            )));
        }
        if let Some(seconds) = input.max_wall_seconds
            && (!seconds.is_finite() || seconds <= 0.0)
        {
            return Err(reject(format!(
                "the wall-clock budget {seconds} must be finite and positive"
            )));
        }
        Ok(Self {
            stop_time: input.stop_time,
            tolerances,
            output_interval: input.output_interval,
            root_scan_resolution: input.root_scan_resolution,
            root_location_tolerance: input.root_location_tolerance,
            max_wall_seconds: input.max_wall_seconds,
            records_trace: input.records_trace,
        })
    }

    #[must_use]
    pub fn stop_time(&self) -> Option<f64> {
        self.stop_time
    }

    #[must_use]
    pub fn relative_tolerance(&self) -> f64 {
        self.tolerances.relative()
    }

    #[must_use]
    pub fn absolute_tolerance(&self) -> f64 {
        self.tolerances.absolute()
    }

    #[must_use]
    pub(in crate::fmi_me) const fn tolerances(&self) -> MeSolverTolerances {
        self.tolerances
    }

    #[must_use]
    pub fn output_interval(&self) -> f64 {
        self.output_interval
    }

    /// The width bound on one adjacent sampled interval inside an accepted
    /// step. Deliberately independent of [`Self::output_interval`].
    #[must_use]
    pub fn root_scan_resolution(&self) -> f64 {
        self.root_scan_resolution
    }

    #[must_use]
    pub fn root_location_tolerance(&self) -> f64 {
        self.root_location_tolerance
    }

    #[must_use]
    pub fn max_wall_seconds(&self) -> Option<f64> {
        self.max_wall_seconds
    }

    #[must_use]
    pub fn records_trace(&self) -> bool {
        self.records_trace
    }

    /// Prove the sole component-owned start coordinate is compatible with the
    /// host policy before the combined session aggregate exists.
    pub(super) fn admit_start_time(&self, start_time: f64) -> Result<(), MeSessionError> {
        if !start_time.is_finite() {
            return Err(MeSessionError::Options {
                reason: format!("the component start {start_time} must be finite"),
            });
        }
        if let Some(stop) = self.stop_time
            && stop < start_time
        {
            return Err(MeSessionError::Options {
                reason: format!(
                    "the defined experiment end {stop} must not precede the component start \
                     {start_time}"
                ),
            });
        }
        Ok(())
    }
}

/// What a [`super::MeSimulationSession::advance_to`] call stopped on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeAdvanceOutcome {
    /// The requested yield boundary was reached exactly.
    Yielded,
    /// The defined experiment end was reached.
    ReachedStop,
    /// The component terminated the simulation.
    Terminated,
}

/// The soft output schedule a batch call hands to the same incremental
/// session. It is a cursor, not a second loop: SPEC_0044 §6 ME-HOST-002.
/// Construction belongs only to the live driver or an admitted batch.
///
/// ```compile_fail,E0624
/// use rumoca_solver::fmi_me::session::MeOutputCursor;
///
/// let _forged = MeOutputCursor::empty();
/// ```
///
/// ```compile_fail,E0599
/// use rumoca_solver::fmi_me::session::MeOutputCursor;
///
/// fn cannot_clone(cursor: MeOutputCursor) {
///     let _duplicate = cursor.clone();
/// }
/// ```
#[derive(Debug)]
pub struct MeOutputCursor {
    times: Vec<f64>,
    next: usize,
}

impl MeOutputCursor {
    /// Build the cursor, proving the schedule is a finite strictly increasing
    /// coordinate sequence.
    pub(in crate::fmi_me) fn new(times: Vec<f64>) -> Result<Self, MeSessionError> {
        for (index, time) in times.iter().copied().enumerate() {
            if !time.is_finite() {
                return Err(MeSessionError::Options {
                    reason: format!("output coordinate {index} is {time}, which is not finite"),
                });
            }
            if index > 0 && time <= times[index - 1] {
                return Err(MeSessionError::Options {
                    reason: format!(
                        "output coordinates must be sorted and unique; {time} follows {}",
                        times[index - 1]
                    ),
                });
            }
        }
        Ok(Self { times, next: 0 })
    }

    #[must_use]
    pub(in crate::fmi_me) fn empty() -> Self {
        Self {
            times: Vec::new(),
            next: 0,
        }
    }

    #[must_use]
    pub fn peek(&self) -> Option<f64> {
        self.times.get(self.next).copied()
    }

    pub(super) fn advance(&mut self) {
        self.next = self.next.saturating_add(1);
    }

    /// Restart the schedule, so a reset session replays the same soft
    /// observation coordinates it was built with.
    #[cfg(test)]
    fn rewind(&mut self) {
        self.next = 0;
    }

    #[must_use]
    #[cfg(test)]
    fn remaining(&self) -> usize {
        self.times.len().saturating_sub(self.next)
    }
}

pub(super) fn trace_capacity(options: &MeSessionOptions, start_time: f64) -> usize {
    if !options.records_trace() {
        return 0;
    }
    let Some(stop) = options.stop_time() else {
        return 0;
    };
    let width = (stop - start_time).abs();
    let interval = options.output_interval();
    let estimate = (width / interval).ceil();
    if !estimate.is_finite() || estimate < 0.0 {
        return 0;
    }
    (estimate as usize).saturating_add(1).min(1 << 22)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn input(stop_time: Option<f64>, records_trace: bool) -> MeSessionOptionsInput {
        MeSessionOptionsInput {
            stop_time,
            relative_tolerance: 1.0e-6,
            absolute_tolerance: 1.0e-6,
            output_interval: 0.1,
            root_scan_resolution: 0.1,
            root_location_tolerance: 1.0e-9,
            max_wall_seconds: None,
            records_trace,
        }
    }

    fn options(stop_time: Option<f64>, records_trace: bool) -> MeSessionOptions {
        MeSessionOptions::new(input(stop_time, records_trace)).expect("fixture options are checked")
    }

    #[test]
    fn the_output_cursor_is_a_cursor_rather_than_a_loop() {
        let mut cursor = MeOutputCursor::new(vec![0.0, 0.5, 1.0]).expect("sorted unique");
        assert_eq!(cursor.remaining(), 3);
        assert_eq!(cursor.peek(), Some(0.0));
        cursor.advance();
        assert_eq!(cursor.peek(), Some(0.5));
        cursor.advance();
        cursor.advance();
        assert_eq!(cursor.peek(), None);
        assert_eq!(cursor.remaining(), 0);
        cursor.advance();
        assert_eq!(cursor.remaining(), 0);
        cursor.rewind();
        assert_eq!(cursor.peek(), Some(0.0));
    }

    #[test]
    fn an_unsorted_or_non_finite_output_schedule_is_rejected() {
        assert!(MeOutputCursor::new(vec![0.0, 1.0, 0.5]).is_err());
        assert!(MeOutputCursor::new(vec![0.0, 0.0]).is_err());
        assert!(MeOutputCursor::new(vec![f64::NAN]).is_err());
        assert!(MeOutputCursor::new(Vec::new()).is_ok());
    }

    #[test]
    fn host_options_prove_ordering_positivity_and_a_legal_budget() {
        let mut zero_interval = input(Some(1.0), true);
        zero_interval.output_interval = 0.0;
        assert!(MeSessionOptions::new(zero_interval).is_err());

        let mut coarse_location = input(Some(1.0), true);
        coarse_location.root_location_tolerance = 1.0;
        assert!(MeSessionOptions::new(coarse_location).is_err());

        let mut bad_budget = input(Some(1.0), true);
        bad_budget.max_wall_seconds = Some(0.0);
        assert!(MeSessionOptions::new(bad_budget).is_err());

        let mut infinite_stop = input(Some(1.0), true);
        infinite_stop.stop_time = Some(f64::INFINITY);
        assert!(MeSessionOptions::new(infinite_stop).is_err());
    }

    #[test]
    fn an_initialization_only_experiment_has_equal_finite_bounds() {
        let checked = MeSessionOptions::new(input(Some(0.0), true))
            .expect("equal bounds request one settled initialization observation");
        assert_eq!(checked.stop_time(), Some(0.0));
        checked
            .admit_start_time(0.0)
            .expect("the component start equals the defined stop");
        assert_eq!(trace_capacity(&checked, 0.0), 1);
    }

    #[test]
    fn the_root_scan_resolution_is_independent_of_the_output_cadence() {
        let mut fine_scan = input(Some(1.0), true);
        fine_scan.output_interval = 0.5;
        fine_scan.root_scan_resolution = 1.0e-3;
        let checked = MeSessionOptions::new(fine_scan).expect("independent options are legal");
        assert!((checked.output_interval() - 0.5).abs() <= f64::EPSILON);
        assert!((checked.root_scan_resolution - 1.0e-3).abs() <= f64::EPSILON);
    }

    #[test]
    fn a_live_session_reserves_no_trace_capacity() {
        assert_eq!(trace_capacity(&options(None, false), 0.0), 0);
    }

    #[test]
    fn a_batch_session_reserves_one_row_per_output_interval() {
        assert_eq!(trace_capacity(&options(Some(1.0), true), 0.0), 11);
    }

    #[test]
    fn an_unbounded_capacity_request_stays_bounded() {
        let mut unbounded = input(Some(1.0e300), true);
        unbounded.output_interval = f64::MIN_POSITIVE;
        let checked = MeSessionOptions::new(unbounded).expect("checked options");
        assert!(trace_capacity(&checked, 0.0) <= 1 << 22);
    }

    #[test]
    fn component_start_admission_proves_the_experiment_ordering_once() {
        let checked = options(Some(1.0), true);
        assert!(checked.admit_start_time(0.5).is_ok());
        assert!(checked.admit_start_time(2.0).is_err());
        assert!(checked.admit_start_time(f64::INFINITY).is_err());
    }
}
