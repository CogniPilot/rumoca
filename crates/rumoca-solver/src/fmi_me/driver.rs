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

/// The root-location tolerance of `plan` at `start_time`: the host's
/// time-coordinate roundoff over one scan interval, per the plan's rule.
fn root_location_tolerance(
    plan: &rumoca_ir_solve::fmi::RootLocationPlan,
    start_time: f64,
    scan_resolution: f64,
) -> f64 {
    plan.location_tolerance(
        plan.interval_roundoff(start_time, scan_resolution),
        scan_resolution,
    )
}

/// No-trace FMI session options from the actual entered instance request.
pub fn fmi_session_options(
    plan: &rumoca_ir_solve::fmi::RootLocationPlan,
    config: &super::MeInstanceConfig,
) -> Result<MeSessionOptions, MeSessionError> {
    let start_time = config.start_time();
    let stop_time = config.stop_time();
    let width = stop_time.map_or(0.0, |stop| stop - start_time);
    let scan_resolution = plan.scan_resolution(width);
    MeSessionOptions::new(MeSessionOptionsInput {
        start_time,
        stop_time,
        relative_tolerance: config.relative_tolerance(),
        absolute_tolerance: config.relative_tolerance(),
        output_interval: scan_resolution,
        root_scan_resolution: scan_resolution,
        root_location_tolerance: root_location_tolerance(plan, start_time, scan_resolution),
        max_wall_seconds: None,
        records_trace: false,
    })
}

/// The checked session options a defined experiment implies.
pub fn batch_session_options(
    plan: &rumoca_ir_solve::fmi::RootLocationPlan,
    start_time: f64,
    stop_time: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
    output_interval: f64,
    max_wall_seconds: Option<f64>,
) -> Result<MeSessionOptions, MeSessionError> {
    let scan_resolution = plan.scan_resolution(stop_time - start_time);
    MeSessionOptions::new(MeSessionOptionsInput {
        start_time,
        stop_time: Some(stop_time),
        relative_tolerance,
        absolute_tolerance,
        output_interval,
        root_scan_resolution: scan_resolution,
        root_location_tolerance: root_location_tolerance(plan, start_time, scan_resolution),
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
    plan: &rumoca_ir_solve::fmi::RootLocationPlan,
    start_time: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
    scan_scale: f64,
    max_wall_seconds: Option<f64>,
) -> Result<MeSessionOptions, MeSessionError> {
    let scan_resolution = plan.scan_resolution(scan_scale);
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
        root_location_tolerance: root_location_tolerance(plan, start_time, scan_resolution),
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
    use rumoca_ir_solve::fmi::RootLocationPlan;

    const STANDARD: RootLocationPlan = RootLocationPlan::STANDARD;

    #[test]
    fn a_live_session_never_carries_defined_stop_metadata() {
        let options = live_session_options(&STANDARD, 0.0, 1.0e-6, 1.0e-6, 1.0, None)
            .expect("live options are checked");
        assert_eq!(options.stop_time(), None);
        assert!(!options.records_trace());
    }

    #[test]
    fn the_scan_resolution_does_not_follow_the_output_cadence() {
        let coarse = batch_session_options(&STANDARD, 0.0, 1.0, 1.0e-6, 1.0e-6, 0.5, None)
            .expect("coarse cadence is legal");
        let fine = batch_session_options(&STANDARD, 0.0, 1.0, 1.0e-6, 1.0e-6, 1.0e-3, None)
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
    fn default_root_time_accuracy_does_not_follow_state_units() {
        let baseline = batch_session_options(&STANDARD, 0.0, 1.0, 1e-6, 1e-9, 0.1, None).unwrap();
        let rescaled = batch_session_options(&STANDARD, 0.0, 1.0, 1e-6, 1e-3, 0.1, None).unwrap();
        assert_eq!(
            baseline.root_location_tolerance(),
            rescaled.root_location_tolerance(),
            "changing the state unit cannot change a root's time accuracy"
        );
        let live = live_session_options(&STANDARD, 0.0, 1e-6, 1e-9, 1.0, None).unwrap();
        let rescaled_live = live_session_options(&STANDARD, 0.0, 1e-6, 1e-3, 1.0, None).unwrap();
        assert_eq!(
            live.root_location_tolerance(),
            rescaled_live.root_location_tolerance()
        );
    }

    #[test]
    fn default_root_time_accuracy_scales_with_time_units() {
        let baseline = batch_session_options(&STANDARD, 2.0, 3.0, 1e-6, 1e-6, 0.1, None).unwrap();
        let scale = 1024.0;
        let rescaled = batch_session_options(
            &STANDARD,
            2.0 * scale,
            3.0 * scale,
            1e-6,
            1e-6,
            0.1 * scale,
            None,
        )
        .unwrap();
        assert_eq!(
            scale * baseline.root_location_tolerance(),
            rescaled.root_location_tolerance(),
            "a root-location duration must transform with the time coordinate"
        );
    }

    #[test]
    fn an_unbuildable_output_grid_is_typed_host_option_data() {
        let options = batch_session_options(&STANDARD, 0.0, 1.0, 1.0e-6, 1.0e-6, 0.1, None)
            .expect("checked options");
        assert!(batch_output_cursor(&options).is_ok());
        let live =
            live_session_options(&STANDARD, 0.0, 1.0e-6, 1.0e-6, 1.0, None).expect("live options");
        assert!(matches!(
            batch_output_cursor(&live),
            Err(MeSessionError::Options { .. })
        ));
    }

    #[test]
    fn a_backward_experiment_is_rejected_before_a_session_exists() {
        assert!(batch_session_options(&STANDARD, 1.0, 0.0, 1.0e-6, 1.0e-6, 0.1, None).is_err());
    }
}
