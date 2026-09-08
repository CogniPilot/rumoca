//! Batch and live drivers over the common session (SPEC_0044 §6, ME-HOST-002).
//!
//! A batch simulation is not a second master algorithm. It builds the output
//! cursor, asks the same incremental [`MeSimulationSession`] to reach its
//! defined end once, and publishes the trace the session owns.

use super::session::{
    MeAdvanceOutcome, MeOutputCursor, MeSessionError, MeSessionOptions, MeSessionOptionsInput,
    MeSimulationSession,
};

/// How the host derives a root-scan resolution when a caller supplies an
/// experiment scale but no explicit scan policy.
///
/// SPEC_0044 §6 keeps event fidelity independent of trace density, so the
/// default is a fixed fraction of the experiment's own scale rather than the
/// output interval: refining the trace must not refine the event search, and
/// coarsening it must not coarsen the event search.
const DEFAULT_SCAN_FRACTION: f64 = 1.0 / 8.0;

/// Derive the default adjacent-sample bound from a checked experiment width.
pub fn default_root_scan_resolution(experiment_width: f64) -> Result<f64, MeSessionError> {
    if !experiment_width.is_finite() || experiment_width < 0.0 {
        return Err(MeSessionError::Options {
            reason: format!(
                "the root-scan experiment width must be finite and nonnegative, got \
                 {experiment_width}"
            ),
        });
    }
    // A zero-duration batch never scans, but the checked session vocabulary
    // intentionally represents every numeric resolution as positive. The
    // smallest positive value is therefore the exact representational lower
    // bound, not a guessed replacement scale.
    Ok((experiment_width * DEFAULT_SCAN_FRACTION).max(f64::MIN_POSITIVE))
}

/// The default bracket width a located root is refined to.
#[must_use]
pub fn default_root_location_tolerance(scan_resolution: f64, absolute_tolerance: f64) -> f64 {
    absolute_tolerance
        .max(f64::MIN_POSITIVE)
        .min(scan_resolution)
}

/// The checked session options a defined experiment implies.
pub fn batch_session_options(
    stop_time: f64,
    scan_scale: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
    output_interval: f64,
    max_wall_seconds: Option<f64>,
) -> Result<MeSessionOptions, MeSessionError> {
    let scan_resolution = default_root_scan_resolution(scan_scale)?;
    MeSessionOptions::new(MeSessionOptionsInput {
        stop_time: Some(stop_time),
        relative_tolerance,
        absolute_tolerance,
        output_interval,
        root_scan_resolution: scan_resolution,
        root_location_tolerance: default_root_location_tolerance(
            scan_resolution,
            absolute_tolerance,
        ),
        max_wall_seconds,
        records_trace: true,
    })
}

/// The checked session options an open incremental host uses.
///
/// An open live session has **no** defined FMI stop metadata: SPEC_0044 §7
/// requires undefined stop plus exact public yield boundaries, and changing a
/// defined experiment requires reset or reconstruction rather than a mutated
/// horizon. `scan_scale` is the coordinate span the
/// default scan resolution is derived from; it is not an experiment end.
pub fn live_session_options(
    relative_tolerance: f64,
    absolute_tolerance: f64,
    scan_scale: f64,
    max_wall_seconds: Option<f64>,
) -> Result<MeSessionOptions, MeSessionError> {
    if scan_scale <= 0.0 {
        return Err(MeSessionError::Options {
            reason: "an open session requires a positive root-scan experiment scale".to_owned(),
        });
    }
    let scan_resolution = default_root_scan_resolution(scan_scale)?;
    MeSessionOptions::new(MeSessionOptionsInput {
        stop_time: None,
        relative_tolerance,
        absolute_tolerance,
        // A live session publishes no trace, so the cadence only has to be a
        // legal positive value; the scan resolution above is what governs event
        // fidelity.
        output_interval: scan_resolution,
        root_scan_resolution: scan_resolution,
        root_location_tolerance: default_root_location_tolerance(
            scan_resolution,
            absolute_tolerance,
        ),
        max_wall_seconds,
        records_trace: false,
    })
}

/// Advance a live session to `target_time` without a soft output schedule.
pub fn advance_live_session(
    session: &mut MeSimulationSession<'_, '_>,
    target_time: f64,
) -> Result<MeAdvanceOutcome, MeSessionError> {
    let mut cursor = MeOutputCursor::empty();
    session.advance_to(target_time, &mut cursor)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_live_session_never_carries_defined_stop_metadata() {
        let options =
            live_session_options(1.0e-6, 1.0e-6, 1.0, None).expect("live options are checked");
        assert_eq!(options.stop_time(), None);
        assert!(!options.records_trace());
    }

    #[test]
    fn the_scan_resolution_does_not_follow_the_output_cadence() {
        let coarse = batch_session_options(1.0, 1.0, 1.0e-6, 1.0e-6, 0.5, None)
            .expect("coarse cadence is legal");
        let fine = batch_session_options(1.0, 1.0, 1.0e-6, 1.0e-6, 1.0e-3, None)
            .expect("fine cadence is legal");

        // The two experiments differ only in trace density.
        let coarse_rows = (1.0 / coarse.output_interval()).ceil() as usize + 1;
        let fine_rows = (1.0 / fine.output_interval()).ceil() as usize + 1;
        assert!(fine_rows > coarse_rows);

        // Event fidelity is therefore identical: a denser trace neither refines
        // nor coarsens the root search.
        assert_eq!(
            coarse.root_scan_resolution().to_bits(),
            fine.root_scan_resolution().to_bits()
        );
        assert_eq!(
            coarse.root_location_tolerance().to_bits(),
            fine.root_location_tolerance().to_bits()
        );
    }

    #[test]
    fn batch_and_live_options_carry_no_second_start_coordinate() {
        let batch =
            batch_session_options(1.0, 1.0, 1.0e-6, 1.0e-6, 0.1, None).expect("checked options");
        assert_eq!(batch.stop_time(), Some(1.0));
        let live = live_session_options(1.0e-6, 1.0e-6, 1.0, None).expect("live options");
        assert_eq!(live.stop_time(), None);
    }

    #[test]
    fn a_non_finite_defined_stop_is_rejected_before_a_session_exists() {
        assert!(batch_session_options(f64::NAN, 1.0, 1.0e-6, 1.0e-6, 0.1, None).is_err());
    }

    #[test]
    fn an_invalid_scan_scale_is_rejected_and_zero_duration_uses_the_type_bound() {
        for scale in [f64::NAN, f64::INFINITY, -1.0] {
            assert!(live_session_options(1.0e-6, 1.0e-6, scale, None).is_err());
        }
        assert!(live_session_options(1.0e-6, 1.0e-6, 0.0, None).is_err());
        assert_eq!(
            default_root_scan_resolution(0.0)
                .expect("a zero-duration experiment never scans")
                .to_bits(),
            f64::MIN_POSITIVE.to_bits()
        );
    }
}
