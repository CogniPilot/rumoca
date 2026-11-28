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
//! The tight reference is part of the model's declared OMC reference: a trace
//! artifact beside the normal one, named in the model's entry with the
//! tolerance it was simulated at ([`TightReference`]). Whether a model is
//! flagged is read from its declared normal trace, so a reference restored
//! from a cache is flagged exactly like a fresh one, and the declaration
//! travels with every other OMC artifact. A flagged model without a usable
//! declaration is not scored: the comparator reports it as a missing reference
//! rather than score its excursion without the evidence the rule requires.

use super::omc_session::{OmcSession, OmcSimSettings};
use super::session_pool::{SessionJob, run_session_pool};
use super::simulation_result::build_session_model_result;
use super::{
    Args, MslPaths, SimModelResult, load_omc_csv_trace, resolve_declared_omc_trace_path,
    resolve_result_file_path, session_pool_config, write_pretty_json,
};
use anyhow::Result;
use rumoca_sim::sim_trace_compare::{SimTrace, load_trace_json, reference_excursion_channels};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::time::Duration;

/// How many times tighter than the model's own the tight reference's tolerance is.
///
/// Measured on the two models this rule exists for: at a hundredth of the
/// tolerance `SpeedControlledDCPM`'s 19.6 V saturation is reproduced exactly,
/// while every noise run on `RoomCO2WithControls` vanishes or shrinks to at
/// most 0.26 of its amplitude.
pub(super) const TIGHT_TOLERANCE_DIVISOR: f64 = 100.0;

/// A flagged model's tight reference, as its OMC reference entry declares it.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub(super) enum TightReference {
    /// The tight rerun's trace (relative to the results directory), simulated
    /// at `tolerance`.
    Trace { file: String, tolerance: f64 },
    /// The tight rerun produced no reference, and why.
    Unavailable { reason: String },
}

/// The `fileNamePrefix` of a model's tight reference.
fn tight_file_prefix(model: &str) -> String {
    format!("{model}_tight")
}

/// The declared trace artifact of a model's tight reference, relative to the
/// results directory.
fn tight_trace_relative(model: &str) -> PathBuf {
    PathBuf::from("sim_traces")
        .join("omc")
        .join(format!("{model}.tight.json"))
}

/// The tolerance a model's tight reference is simulated at: its normal run's
/// divided by [`TIGHT_TOLERANCE_DIVISOR`]. `None` when the normal run did not
/// report its tolerance, since a rerun is then not comparable.
fn tight_tolerance(settings: Option<&OmcSimSettings>) -> Option<f64> {
    Some(settings?.tolerance? / TIGHT_TOLERANCE_DIVISOR)
}

/// The `simulate(...)` arguments of a tight rerun: the normal run's horizon and
/// interval count, so both references share an output grid, at
/// [`tight_tolerance`].
fn tight_simulate_options(settings: &OmcSimSettings) -> Option<String> {
    let tolerance = tight_tolerance(Some(settings))?;
    let mut options = format!(", tolerance={tolerance:e}");
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

/// Whether `reference`, a model's declared normal trace, needs a tight
/// reference: it has an excursion channel.
fn needs_tight_reference(reference: &SimTrace) -> bool {
    !reference_excursion_channels(reference).is_empty()
}

/// The declared tight trace of `result` when it is usable: a trace at the
/// tolerance this rule requires for the model's normal run, whose file exists.
fn declared_tight_trace(paths: &MslPaths, result: &SimModelResult) -> Result<PathBuf, String> {
    let expected = tight_tolerance(result.omc_settings.as_ref())
        .ok_or("the normal OMC run reported no tolerance to tighten")?;
    match &result.tight_reference {
        None => Err("no tight reference is declared".to_string()),
        Some(TightReference::Unavailable { reason }) => {
            Err(format!("the tight rerun produced no reference: {reason}"))
        }
        Some(TightReference::Trace { file, tolerance }) => {
            if *tolerance != expected {
                return Err(format!(
                    "the declared tight reference was simulated at tolerance {tolerance:e}, \
                     not the required {expected:e}"
                ));
            }
            let path = paths.results_dir.join(file);
            if path.is_file() {
                Ok(path)
            } else {
                Err(format!(
                    "the declared tight reference '{file}' does not exist"
                ))
            }
        }
    }
}

/// The tight rerun's options for a model that needs one: a successful normal
/// run that reported its tolerance, whose declared normal trace has an
/// excursion channel, and that declares no usable tight reference yet.
fn tight_rerun_options(paths: &MslPaths, model: &str, result: &SimModelResult) -> Option<String> {
    if result.status != "success" {
        return None;
    }
    let options = tight_simulate_options(result.omc_settings.as_ref()?)?;
    let Some(trace_path) = resolve_declared_omc_trace_path(paths, model, result) else {
        println!("OMC tight reference flagging skipped for {model}: no declared normal trace");
        return None;
    };
    let reference = match load_trace_json(&trace_path) {
        Ok(reference) => reference,
        Err(error) => {
            println!("OMC tight reference flagging skipped for {model}: {error}");
            return None;
        }
    };
    if !needs_tight_reference(&reference) || declared_tight_trace(paths, result).is_ok() {
        return None;
    }
    Some(options)
}

/// The tight reference run: one model simulated again with its tight options.
struct TightSimulation<'a> {
    paths: &'a MslPaths,
    options: HashMap<String, String>,
    tolerances: HashMap<String, f64>,
    sim_timeout: Duration,
}

impl SessionJob for TightSimulation<'_> {
    type Outcome = TightReference;

    fn run(&self, session: &mut OmcSession, model: &str) -> (TightReference, bool) {
        let options = self.options.get(model).map_or("", String::as_str);
        match session.simulate_with_options(
            model,
            options,
            &tight_file_prefix(model),
            self.sim_timeout,
        ) {
            Ok(outcome) => {
                let result = build_session_model_result(&outcome, 0.0);
                (self.declare(model, &result), false)
            }
            Err(error) => (
                TightReference::Unavailable {
                    reason: format!("{error:?}"),
                },
                true,
            ),
        }
    }

    fn spawn_failed(&self, error: String) -> TightReference {
        TightReference::Unavailable { reason: error }
    }
}

impl TightSimulation<'_> {
    /// The declaration for a model's tight rerun `result`: its trace artifact,
    /// written from the result file that same request produced.
    fn declare(&self, model: &str, result: &SimModelResult) -> TightReference {
        let unavailable = |reason: String| TightReference::Unavailable { reason };
        if result.status != "success" {
            return unavailable(
                result
                    .error
                    .clone()
                    .unwrap_or_else(|| "the tight rerun did not succeed".to_string()),
            );
        }
        let Some(csv) = resolve_result_file_path(self.paths, model, result.result_file.as_deref())
        else {
            return unavailable("the tight rerun declared no result file".to_string());
        };
        let relative = tight_trace_relative(model);
        let written = load_omc_csv_trace(model, &csv)
            .and_then(|trace| write_pretty_json(&self.paths.results_dir.join(&relative), &trace));
        match (written, self.tolerances.get(model)) {
            (Ok(()), Some(tolerance)) => TightReference::Trace {
                file: relative.to_string_lossy().replace('\\', "/"),
                tolerance: *tolerance,
            },
            (Err(error), _) => unavailable(format!("failed to write the tight trace: {error}")),
            (Ok(()), None) => unavailable("no tight tolerance for the model".to_string()),
        }
    }
}

/// The post-timing pass: every flagged model without a usable tight reference
/// simulated again at a tighter tolerance, after all of the shard's timed runs
/// have finished, on the same session pool. Each rerun's outcome is declared in
/// the model's entry: its trace, or why there is none.
pub(super) fn simulate_tight_references(
    args: &Args,
    workers: usize,
    paths: &MslPaths,
    results: &mut BTreeMap<String, SimModelResult>,
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
    let tolerances = options
        .keys()
        .filter_map(|model| {
            let tolerance = tight_tolerance(results.get(model)?.omc_settings.as_ref())?;
            Some((model.clone(), tolerance))
        })
        .collect::<HashMap<_, _>>();
    let models = options.keys().cloned().collect::<Vec<_>>();
    println!(
        "OMC tight references: {} flagged model(s) at tolerance / {TIGHT_TOLERANCE_DIVISOR}, after timing",
        models.len()
    );
    let config = session_pool_config(args, workers, paths, models.len())?;
    let job = TightSimulation {
        paths,
        options,
        tolerances,
        sim_timeout: Duration::from_secs(args.batch_timeout_seconds.max(1)),
    };
    let mut declared = Vec::with_capacity(models.len());
    run_session_pool(&models, &config, &job, |index, reference| {
        declared.push((models[index].clone(), reference));
    });
    for (model, reference) in declared {
        if let TightReference::Unavailable { reason } = &reference {
            println!("OMC tight reference unavailable for {model}: {reason}");
        }
        if let Some(result) = results.get_mut(&model) {
            result.tight_reference = Some(reference);
        }
    }
    Ok(())
}

/// The tight reference the comparator scores `model` with: none when its
/// declared normal trace `reference` has no excursion channel, the declared
/// tight trace when it has one, or why that evidence is missing.
pub(super) fn required_tight_reference(
    paths: &MslPaths,
    result: &SimModelResult,
    reference: &SimTrace,
) -> Result<Option<SimTrace>, String> {
    if !needs_tight_reference(reference) {
        return Ok(None);
    }
    let path = declared_tight_trace(paths, result)
        .map_err(|why| format!("the reference has an excursion channel but {why}"))?;
    load_trace_json(&path)
        .map(Some)
        .map_err(|error| format!("failed to load the declared tight reference: {error}"))
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

    fn paths_in(root: &std::path::Path) -> MslPaths {
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

    /// A trace on [0, 1] whose channel `u` is flat zero, or flat with a
    /// 31-sample event when `event` is set.
    fn trace(event: bool) -> SimTrace {
        let times = (0..=1000)
            .map(|i| f64::from(i) / 1000.0)
            .collect::<Vec<_>>();
        let u = (0..=1000)
            .map(|i| {
                Some(if event && (500..=530).contains(&i) {
                    20.0
                } else {
                    0.0
                })
            })
            .collect();
        SimTrace {
            model_name: None,
            times,
            names: vec!["u".to_string()],
            data: vec![u],
            variable_meta: None,
            certification_profile: None,
        }
    }

    /// A successful entry as the OMC cache restores it: a declared normal
    /// trace, a reported tolerance of 1e-6, and no result file.
    fn cached_entry(paths: &MslPaths, model: &str, event: bool) -> SimModelResult {
        let relative = format!("sim_traces/omc/{model}.json");
        write_pretty_json(&paths.results_dir.join(&relative), &trace(event)).expect("write trace");
        SimModelResult {
            status: "success".to_string(),
            trace_file: Some(relative),
            omc_settings: Some(OmcSimSettings {
                tolerance: Some(1e-6),
                ..OmcSimSettings::default()
            }),
            ..super::super::empty_omc_result()
        }
    }

    /// Declare `model`'s tight trace at `tolerance`, writing it when `write`.
    fn declare(
        paths: &MslPaths,
        entry: &mut SimModelResult,
        model: &str,
        tolerance: f64,
        write: bool,
    ) {
        let relative = tight_trace_relative(model);
        if write {
            write_pretty_json(&paths.results_dir.join(&relative), &trace(true))
                .expect("write tight trace");
        }
        entry.tight_reference = Some(TightReference::Trace {
            file: relative.to_string_lossy().into_owned(),
            tolerance,
        });
    }

    /// The regression: a reference restored from the OMC cache has no result
    /// file, and its excursion must still be flagged for a tight rerun.
    #[test]
    fn a_cached_reference_is_flagged_from_its_declared_trace() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = paths_in(temp.path());
        let event = cached_entry(&paths, "Event", true);
        assert!(event.result_file.is_none());
        assert!(tight_rerun_options(&paths, "Event", &event).is_some());
        assert!(
            tight_rerun_options(&paths, "Flat", &cached_entry(&paths, "Flat", false)).is_none()
        );
        let failed = SimModelResult {
            status: "error".to_string(),
            ..cached_entry(&paths, "Failed", true)
        };
        assert!(tight_rerun_options(&paths, "Failed", &failed).is_none());
    }

    /// Only a declaration at the required tolerance whose trace exists is
    /// reused; any other leaves the model flagged for a rerun.
    #[test]
    fn only_a_usable_declared_tight_reference_is_reused() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = paths_in(temp.path());
        let mut entry = cached_entry(&paths, "Event", true);
        declare(&paths, &mut entry, "Event", 1e-8, true);
        assert_eq!(tight_rerun_options(&paths, "Event", &entry), None);

        let mut other_tolerance = entry.clone();
        declare(&paths, &mut other_tolerance, "Event", 1e-7, false);
        assert!(tight_rerun_options(&paths, "Event", &other_tolerance).is_some());

        let mut missing_file = cached_entry(&paths, "Gone", true);
        declare(&paths, &mut missing_file, "Gone", 1e-8, false);
        assert!(tight_rerun_options(&paths, "Gone", &missing_file).is_some());

        let unavailable = SimModelResult {
            tight_reference: Some(TightReference::Unavailable {
                reason: "timed out".to_string(),
            }),
            ..entry.clone()
        };
        assert!(tight_rerun_options(&paths, "Event", &unavailable).is_some());
    }

    /// A flagged reference is scored with its declared tight trace, and
    /// without one it is not scored at all; an unflagged one needs none.
    #[test]
    fn a_flagged_reference_without_its_tight_trace_is_not_scored() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = paths_in(temp.path());
        let flat = cached_entry(&paths, "Flat", false);
        assert!(matches!(
            required_tight_reference(&paths, &flat, &trace(false)),
            Ok(None)
        ));

        let mut event = cached_entry(&paths, "Event", true);
        let missing = required_tight_reference(&paths, &event, &trace(true)).unwrap_err();
        assert!(
            missing.contains("no tight reference is declared"),
            "{missing}"
        );

        let unavailable = SimModelResult {
            tight_reference: Some(TightReference::Unavailable {
                reason: "timed out".to_string(),
            }),
            ..event.clone()
        };
        let why = required_tight_reference(&paths, &unavailable, &trace(true)).unwrap_err();
        assert!(why.contains("timed out"), "{why}");

        declare(&paths, &mut event, "Event", 1e-8, true);
        let tight = required_tight_reference(&paths, &event, &trace(true))
            .expect("a declared tight trace loads")
            .expect("a flagged reference has one");
        assert_eq!(tight.data, trace(true).data);
    }
}
