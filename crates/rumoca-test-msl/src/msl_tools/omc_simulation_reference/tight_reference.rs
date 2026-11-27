//! The OMC reference simulated again at a tighter tolerance, for the models
//! whose reference has an excursion: a run outside the flat bulk of a channel
//! whose robust statistics see no scale.
//!
//! The comparator needs it to tell an event (a property of the model, which
//! survives a tighter tolerance) from OMC's own integration noise (which
//! shrinks or vanishes); see `rumoca_sim::sim_trace_compare`'s `excursion`
//! module. Only flagged models are simulated again, in the same session right
//! after their normal run, with the same horizon and output grid and only the
//! tolerance changed. The rerun is not timed into the model's OMC result, so
//! the OMC-against-Rumoca speed report keeps measuring one OMC run per model.
//! With several session workers, a rerun can overlap another worker's timed
//! run and slow it slightly, which flatters Rumoca's speed ratio for that
//! model; flagged models are few, so the runs are not serialized.
//!
//! A missing, failed, timed-out or stale tight reference is no evidence: the
//! comparator then scores the model exactly as without one.

use super::omc_session::{OmcEvalError, OmcSession, OmcSimSettings};
use super::{MslPaths, load_omc_csv_trace, normalize_omc_trace_rows};
use rumoca_sim::sim_trace_compare::{SimTrace, reference_excursion_channels};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// How many times tighter than the model's own the tight reference's tolerance is.
///
/// Measured on the two models this rule exists for: at a hundredth of the
/// tolerance `SpeedControlledDCPM`'s 19.6 V saturation is reproduced exactly,
/// while every noise run on `RoomCO2WithControls` vanishes or shrinks to at
/// most 0.26 of its amplitude.
pub(super) const TIGHT_TOLERANCE_DIVISOR: f64 = 100.0;

/// The `fileNamePrefix` of a model's tight reference.
fn tight_file_prefix(model: &str) -> String {
    format!("{model}_tight")
}

/// The result CSV of a model's tight reference in an OMC work directory.
pub(super) fn tight_result_csv(work_dir: &Path, model: &str) -> PathBuf {
    work_dir.join(format!("{}_res.csv", tight_file_prefix(model)))
}

/// The `simulate(...)` arguments of a tight rerun: the normal run's horizon and
/// interval count, so both references share an output grid, at
/// its tolerance divided by [`TIGHT_TOLERANCE_DIVISOR`]. `None` when the normal run did
/// not report its tolerance, since a rerun is then not comparable.
fn tight_simulate_options(settings: &OmcSimSettings) -> Option<String> {
    let tolerance = settings.tolerance?;
    let mut options = format!(", tolerance={:e}", tolerance / TIGHT_TOLERANCE_DIVISOR);
    if let Some(start_time) = settings.start_time {
        options.push_str(&format!(", startTime={start_time}"));
    }
    if let Some(stop_time) = settings.stop_time {
        options.push_str(&format!(", stopTime={stop_time}"));
    }
    if let Some(intervals) = settings.number_of_intervals {
        options.push_str(&format!(", numberOfIntervals={intervals}"));
    }
    Some(options)
}

/// After a model's successful normal run: simulate it again at a tighter
/// tolerance when its reference has an excursion channel. Returns whether a
/// tight reference was written. A timeout or session error is returned so the
/// caller can respawn the session, exactly as for a normal run.
pub(super) fn simulate_tight_reference_if_flagged(
    session: &mut OmcSession,
    work_dir: &Path,
    model: &str,
    settings: &OmcSimSettings,
    sim_timeout: Duration,
) -> Result<bool, OmcEvalError> {
    let normal_csv = work_dir.join(format!("{model}_res.csv"));
    let Ok(reference) = load_omc_csv_trace(model, &normal_csv) else {
        return Ok(false);
    };
    if reference_excursion_channels(&reference).is_empty() {
        return Ok(false);
    }
    let Some(options) = tight_simulate_options(settings) else {
        return Ok(false);
    };
    let outcome =
        session.simulate_with_options(model, &options, &tight_file_prefix(model), sim_timeout)?;
    Ok(outcome.fresh_result.is_some())
}

/// The tight reference of a model, when one was written for the same normal
/// run: present, loadable, and no older than the normal result CSV (an older
/// file belongs to an earlier simulation and is not evidence for this one).
pub(super) fn load_tight_reference(
    paths: &MslPaths,
    model: &str,
    normal_csv: Option<&Path>,
) -> Option<SimTrace> {
    let tight_csv = tight_result_csv(&paths.sim_work_dir, model);
    let modified = |path: &Path| path.metadata().and_then(|meta| meta.modified()).ok();
    let tight_modified = modified(&tight_csv)?;
    if modified(normal_csv?)? > tight_modified {
        return None;
    }
    let mut trace = load_omc_csv_trace(model, &tight_csv).ok()?;
    normalize_omc_trace_rows(&mut trace).ok()?;
    Some(trace)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_tight_rerun_keeps_the_horizon_and_grid_and_tightens_the_tolerance() {
        let settings = OmcSimSettings {
            tolerance: Some(1e-6),
            start_time: Some(0.0),
            stop_time: Some(1.0),
            number_of_intervals: Some(10_000),
            ..OmcSimSettings::default()
        };
        assert_eq!(
            tight_simulate_options(&settings).as_deref(),
            Some(", tolerance=1e-8, startTime=0, stopTime=1, numberOfIntervals=10000")
        );
    }

    #[test]
    fn a_run_without_a_reported_tolerance_is_not_rerun() {
        assert_eq!(tight_simulate_options(&OmcSimSettings::default()), None);
    }
}
