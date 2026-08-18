//! Batch and live drivers over the common session (SPEC_0044 §6, ME-HOST-002).
//!
//! A batch simulation is not a second master algorithm. It builds the output
//! cursor, asks the same incremental [`MeSimulationSession`] to reach its
//! defined end once, and publishes the trace the session owns.

use super::session::{
    MeAdvanceOutcome, MeOutputCursor, MeSessionError, MeSessionOptions, MeSessionOptionsInput,
    MeSimulationSession,
};
use crate::timeline::try_build_output_times;

/// How the host derives a root-scan resolution when a caller supplies only an
/// output cadence.
///
/// SPEC_0044 §6 keeps event fidelity independent of trace density, so the
/// default is a fixed fraction of the experiment's own scale rather than the
/// output interval: refining the trace must not refine the event search, and
/// coarsening it must not coarsen the event search.
const DEFAULT_SCAN_FRACTION: f64 = 1.0 / 512.0;

/// The default adjacent-sample bound for an experiment of the given width.
#[must_use]
pub fn default_root_scan_resolution(experiment_width: f64) -> f64 {
    let width = experiment_width.abs();
    if width.is_finite() && width > 0.0 {
        (width * DEFAULT_SCAN_FRACTION).max(f64::MIN_POSITIVE)
    } else {
        1.0e-3
    }
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
    start_time: f64,
    stop_time: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
    output_interval: f64,
    max_wall_seconds: Option<f64>,
) -> Result<MeSessionOptions, MeSessionError> {
    let scan_resolution = default_root_scan_resolution(stop_time - start_time);
    MeSessionOptions::new(MeSessionOptionsInput {
        start_time,
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
/// horizon (review finding [337]§8). `scan_scale` is the coordinate span the
/// default scan resolution is derived from; it is not an experiment end.
pub fn live_session_options(
    start_time: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
    scan_scale: f64,
    max_wall_seconds: Option<f64>,
) -> Result<MeSessionOptions, MeSessionError> {
    let scan_resolution = default_root_scan_resolution(scan_scale);
    MeSessionOptions::new(MeSessionOptionsInput {
        start_time,
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

/// The soft output schedule a defined experiment requests.
///
/// An unbuildable grid stays typed host-option data; it is never rendered into
/// a component contract failure.
pub fn batch_output_cursor(options: &MeSessionOptions) -> Result<MeOutputCursor, MeSessionError> {
    let stop = options.stop_time().ok_or_else(|| MeSessionError::Options {
        reason: "a batch experiment requires a defined stop time".to_owned(),
    })?;
    let times = try_build_output_times(options.start_time(), stop, options.output_interval())
        .map_err(|error| MeSessionError::Options {
            reason: error.to_string(),
        })?;
    MeOutputCursor::new(times)
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
            live_session_options(0.0, 1.0e-6, 1.0e-6, 1.0, None).expect("live options are checked");
        assert_eq!(options.stop_time(), None);
        assert!(!options.records_trace());
    }

    #[test]
    fn the_scan_resolution_does_not_follow_the_output_cadence() {
        let coarse = batch_session_options(0.0, 1.0, 1.0e-6, 1.0e-6, 0.5, None)
            .expect("coarse cadence is legal");
        let fine = batch_session_options(0.0, 1.0, 1.0e-6, 1.0e-6, 1.0e-3, None)
            .expect("fine cadence is legal");

        // The two experiments differ only in trace density.
        let coarse_rows = batch_output_cursor(&coarse)
            .expect("coarse grid")
            .remaining();
        let fine_rows = batch_output_cursor(&fine).expect("fine grid").remaining();
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
    fn an_unbuildable_output_grid_is_typed_host_option_data() {
        let options =
            batch_session_options(0.0, 1.0, 1.0e-6, 1.0e-6, 0.1, None).expect("checked options");
        assert!(batch_output_cursor(&options).is_ok());
        let live = live_session_options(0.0, 1.0e-6, 1.0e-6, 1.0, None).expect("live options");
        assert!(matches!(
            batch_output_cursor(&live),
            Err(MeSessionError::Options { .. })
        ));
    }

    #[test]
    fn a_backward_experiment_is_rejected_before_a_session_exists() {
        assert!(batch_session_options(1.0, 0.0, 1.0e-6, 1.0e-6, 0.1, None).is_err());
    }
}
