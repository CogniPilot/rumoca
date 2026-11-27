//! The OMC reference simulated again at a tighter tolerance, for the models
//! whose reference has an excursion: a run outside the flat bulk of a channel
//! whose robust statistics see no scale.
//!
//! The comparator needs it to tell an event (a property of the model, which
//! survives a tighter tolerance) from OMC's own integration noise (which
//! shrinks or vanishes); see `rumoca_sim::sim_trace_compare`'s `excursion`
//! module. Only flagged models are simulated again, with the same horizon and
//! output grid and only the tolerance changed, in a pass of their own after
//! every timed run of the shard has finished: no rerun overlaps a timed run,
//! and none is counted in the speed report.
//!
//! A missing, failed, timed-out or stale tight reference is no evidence: the
//! comparator then scores the model exactly as without one.

use super::omc_session::{OmcSession, OmcSimSettings};
use super::session_pool::{SessionJob, run_session_pool};
use super::{
    Args, MslPaths, SimModelResult, load_omc_csv_trace, normalize_omc_trace_rows,
    resolve_result_file_path, session_pool_config,
};
use anyhow::Result;
use rumoca_sim::sim_trace_compare::{SimTrace, reference_excursion_channels};
use std::collections::{BTreeMap, HashMap};
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

/// The tight reference run: one model simulated again with its tight options.
struct TightSimulation {
    options: HashMap<String, String>,
    sim_timeout: Duration,
}

impl SessionJob for TightSimulation {
    type Outcome = ();

    fn run(&self, session: &mut OmcSession, model: &str) -> ((), bool) {
        let options = self.options.get(model).map_or("", String::as_str);
        let outcome = session.simulate_with_options(
            model,
            options,
            &tight_file_prefix(model),
            self.sim_timeout,
        );
        ((), outcome.is_err())
    }

    fn spawn_failed(&self, _error: String) {}
}

/// The tight rerun's options for a model that needs one: a successful normal
/// run that reported its tolerance, whose reference has an excursion channel
/// and has no tight reference from the same normal run yet (so a cached
/// normal result still gets its tight reference).
fn tight_rerun_options(paths: &MslPaths, model: &str, result: &SimModelResult) -> Option<String> {
    if result.status != "success" {
        return None;
    }
    let options = tight_simulate_options(result.omc_settings.as_ref()?)?;
    let normal_csv = resolve_result_file_path(paths, model, result.result_file.as_deref())?;
    if fresh_tight_csv(paths, model, &normal_csv).is_some() {
        return None;
    }
    let reference = load_omc_csv_trace(model, &normal_csv).ok()?;
    (!reference_excursion_channels(&reference).is_empty()).then_some(options)
}

/// The post-timing pass: every flagged model simulated again at a tighter
/// tolerance, after all of the shard's timed runs have finished, on the same
/// session pool. A failed or timed-out rerun leaves no tight reference, and
/// that model is scored exactly as without one.
pub(super) fn simulate_tight_references(
    args: &Args,
    workers: usize,
    paths: &MslPaths,
    results: &BTreeMap<String, SimModelResult>,
) -> Result<()> {
    let options = results
        .iter()
        .filter_map(|(model, result)| {
            tight_rerun_options(paths, model, result).map(|options| (model.clone(), options))
        })
        .collect::<HashMap<_, _>>();
    if options.is_empty() {
        return Ok(());
    }
    let models = options.keys().cloned().collect::<Vec<_>>();
    println!(
        "OMC tight references: {} flagged model(s) at tolerance / {TIGHT_TOLERANCE_DIVISOR}, after timing",
        models.len()
    );
    let config = session_pool_config(args, workers, paths, models.len())?;
    let job = TightSimulation {
        options,
        sim_timeout: Duration::from_secs(args.batch_timeout_seconds.max(1)),
    };
    run_session_pool(&models, &config, &job, |_, ()| {});
    Ok(())
}

/// The tight reference of a model, when one was written for the same normal
/// run: present, loadable, and no older than the normal result CSV (an older
/// file belongs to an earlier simulation and is not evidence for this one).
pub(super) fn load_tight_reference(
    paths: &MslPaths,
    model: &str,
    normal_csv: Option<&Path>,
) -> Option<SimTrace> {
    let tight_csv = fresh_tight_csv(paths, model, normal_csv?)?;
    let mut trace = load_omc_csv_trace(model, &tight_csv).ok()?;
    normalize_omc_trace_rows(&mut trace).ok()?;
    Some(trace)
}

/// The tight result CSV of a model when it is no older than `normal_csv`.
fn fresh_tight_csv(paths: &MslPaths, model: &str, normal_csv: &Path) -> Option<PathBuf> {
    let tight_csv = tight_result_csv(&paths.sim_work_dir, model);
    let modified = |path: &Path| path.metadata().and_then(|meta| meta.modified()).ok();
    (modified(&tight_csv)? >= modified(normal_csv)?).then_some(tight_csv)
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

    fn paths_in(root: &Path) -> MslPaths {
        let results_dir = root.join("results");
        MslPaths {
            repo_root: root.to_path_buf(),
            msl_dir: root.join("msl"),
            results_dir: results_dir.clone(),
            flat_dir: results_dir.join("omc_flat"),
            work_dir: results_dir.join("omc_work"),
            sim_work_dir: results_dir.join("omc_sim_work"),
            omc_trace_dir: results_dir.join("sim_traces/omc"),
            rumoca_trace_dir: results_dir.join("sim_traces/rumoca"),
        }
    }

    /// A normal result CSV on [0, 1] whose channel `u` is flat zero, or flat
    /// with a 31-sample event when `event` is set.
    fn write_csv(path: &Path, event: bool) {
        let mut csv = String::from("\"time\",\"u\"\n");
        for i in 0..=1000 {
            let u = if event && (500..=530).contains(&i) {
                20.0
            } else {
                0.0
            };
            csv.push_str(&format!("{},{u}\n", f64::from(i) / 1000.0));
        }
        std::fs::create_dir_all(path.parent().expect("parent")).expect("work dir");
        std::fs::write(path, csv).expect("write csv");
    }

    fn success(tolerance: Option<f64>) -> SimModelResult {
        SimModelResult {
            status: "success".to_string(),
            omc_settings: Some(OmcSimSettings {
                tolerance,
                ..OmcSimSettings::default()
            }),
            ..super::super::empty_omc_result()
        }
    }

    /// Which models the post-timing pass reruns: a successful, flagged model
    /// with a reported tolerance and no tight reference from the same normal
    /// run, including one whose normal result came from the cache.
    #[test]
    fn the_tight_pass_reruns_only_flagged_models_without_a_fresh_tight_reference() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = paths_in(temp.path());
        let work = &paths.sim_work_dir;
        write_csv(&work.join("Event_res.csv"), true);
        write_csv(&work.join("Flat_res.csv"), false);

        assert!(tight_rerun_options(&paths, "Event", &success(Some(1e-6))).is_some());
        assert!(tight_rerun_options(&paths, "Flat", &success(Some(1e-6))).is_none());
        assert!(tight_rerun_options(&paths, "Event", &success(None)).is_none());
        let failed = SimModelResult {
            status: "error".to_string(),
            ..success(Some(1e-6))
        };
        assert!(tight_rerun_options(&paths, "Event", &failed).is_none());

        write_csv(&tight_result_csv(work, "Event"), true);
        assert!(tight_rerun_options(&paths, "Event", &success(Some(1e-6))).is_none());
        assert!(load_tight_reference(&paths, "Event", Some(&work.join("Event_res.csv"))).is_some());
    }
}
