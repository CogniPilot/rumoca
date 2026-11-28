//! `verify msl-remeasure`: rule B's re-measure pass on one shard.
//!
//! A watchdog kill has no completion time for its stage, so a model the
//! watchdog killed in a stage it completed on the old side is inconclusive,
//! not a regression. This pass re-runs each such model on the same runner,
//! with only the killed stage's budget raised by the runner's speed (at least
//! 1x and at most 3x) and every other stage at its own budget, then replaces
//! the killed row with the re-measured one and records the re-measurement in
//! `msl_remeasured.json`. A stage killed again is then final. A shard whose
//! speed gauge cannot be trusted re-runs nothing: its kills stay inconclusive,
//! never re-judged on absolute time. After rows changed, the shard's
//! comparator tail runs again (`verify msl-parity --refresh-parity`).

use anyhow::{Context, Result, bail};
use clap::Args;
use rumoca_msl_quality::snapshot::{crossed_links, previous_states};
use rumoca_msl_quality::speed::{SpeedGauge, calibration_timings, frame_references};
use rumoca_msl_quality::{ModelStage, ModelStates, Remeasurement, stage_at_kill, stage_of_row};
use serde_json::{Map, Value, json};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// The file a shard's re-measurements are recorded in.
pub(crate) const REMEASURED_FILE: &str = "msl_remeasured.json";
/// The largest factor a stage budget is raised by.
const MAX_FACTOR: f64 = 3.0;

/// Arguments of `verify msl-remeasure`.
#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct VerifyMslRemeasureArgs {
    /// The shard's results directory (msl_results.json and its OMC reference)
    #[arg(long)]
    results_dir: PathBuf,
    /// The baseline the run is judged against (default: the raw promoted
    /// asset, downloaded; in CI a failed download fails the pass)
    #[arg(long)]
    baseline: Option<PathBuf>,
    /// The tested-main run's msl_results.json the PR-vs-main check compares
    /// with: a stage it completed is a candidate too
    #[arg(long, conflicts_with = "tested_main_unavailable")]
    tested_main_results: Option<PathBuf>,
    /// Why tested main has no results (its CI completed without a usable
    /// snapshot): candidates then come from the promoted baseline alone, and
    /// the reason is recorded
    #[arg(long, value_parser = clap::builder::NonEmptyStringValueParser::new())]
    tested_main_unavailable: Option<String>,
    /// Re-measure at most this many models; the rest stay inconclusive
    #[arg(long, default_value_t = 4)]
    max_models: usize,
    /// Arguments every focused re-run of `verify msl-parity` takes (budget
    /// profile, prebuilt binaries), after `--`
    #[arg(last = true)]
    parity_args: Vec<String>,
}

/// Parse `PHASE=FACTOR` for `--raise-stage`: a phase name and a factor in
/// 1..=3.
pub(crate) fn parse_raised_stage(raw: &str) -> std::result::Result<String, String> {
    raised_stage_parts(raw).map(|_| raw.to_string())
}

pub(crate) fn raised_stage_parts(raw: &str) -> std::result::Result<(String, f64), String> {
    let (phase, factor) = raw
        .split_once('=')
        .ok_or_else(|| format!("expected PHASE=FACTOR, got {raw:?}"))?;
    let factor: f64 = factor
        .parse()
        .map_err(|error| format!("{raw:?}: factor: {error}"))?;
    if phase.is_empty() || !(1.0..=MAX_FACTOR).contains(&factor) {
        return Err(format!(
            "{raw:?}: need a phase name and a factor in 1..={MAX_FACTOR}"
        ));
    }
    Ok((phase.to_string(), factor))
}

fn read_json(path: &Path) -> Result<Value> {
    let raw = fs::read(path).with_context(|| format!("read {}", path.display()))?;
    serde_json::from_slice(&raw).with_context(|| format!("parse {}", path.display()))
}

/// A model to re-measure: its killed stage and the factor to raise it by.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Candidate {
    pub(crate) model: String,
    pub(crate) remeasurement: Remeasurement,
}

/// The killed stage of a result row, by the worker's name for it.
fn killed_stage(row: &Value) -> Option<String> {
    if let Some(phase) = row.get("timeout_phase").and_then(Value::as_str) {
        return Some(phase.to_string());
    }
    (row.get("sim_status").and_then(Value::as_str) == Some("sim_timeout"))
        .then(|| "Sim".to_string())
}

/// Each model's stage in a run's msl_results.json rows.
pub(crate) fn row_stages(results: &Value) -> Result<BTreeMap<String, ModelStage>> {
    let rows = results
        .get("model_results")
        .and_then(Value::as_array)
        .context("msl_results.json has no model_results")?;
    rows.iter()
        .map(|row| {
            let model = row
                .get("model_name")
                .and_then(Value::as_str)
                .context("a result row has no model_name")?;
            let text = |key: &str| row.get(key).and_then(Value::as_str);
            let stage = stage_of_row(
                text("phase_reached").unwrap_or(""),
                text("ic_status"),
                text("sim_status"),
            )
            .map_err(|error| anyhow::anyhow!("{model}: {error}"))?;
            Ok((model.to_string(), stage))
        })
        .collect()
}

/// The furthest stage each model reached on either old side: the promoted
/// baseline's states, and the tested-main run the PR-vs-main check compares
/// with. A kill is re-measured when either completed its stage, so both
/// checks judge the same re-measured evidence.
pub(crate) fn old_side_stages(
    baseline: &ModelStates,
    tested_main: Option<&BTreeMap<String, ModelStage>>,
) -> BTreeMap<String, ModelStage> {
    let mut stages = baseline
        .iter()
        .map(|(model, state)| (model.clone(), state.stage))
        .collect::<BTreeMap<_, _>>();
    for (model, stage) in tested_main.into_iter().flatten() {
        let entry = stages.entry(model.clone()).or_insert(*stage);
        *entry = (*entry).max(*stage);
    }
    stages
}

/// The models to re-measure, and the kills that cannot be (with why). A
/// candidate is killed in a stage an old side completed. Its factor is
/// the shard's speed clamped to 1..=3; with no stored frame (the first
/// crossing, whose frame is set only at the merge) it is the cap.
pub(crate) fn candidates(
    results: &Value,
    old: &BTreeMap<String, ModelStage>,
    gauge: Option<&Result<SpeedGauge, String>>,
) -> (Vec<Candidate>, Vec<String>) {
    let mut chosen = Vec::new();
    let mut skipped = Vec::new();
    let rows = results.get("model_results").and_then(Value::as_array);
    for row in rows.into_iter().flatten() {
        let (Some(model), Some(stage)) = (
            row.get("model_name").and_then(Value::as_str),
            killed_stage(row),
        ) else {
            continue;
        };
        let (Some(reached), Some(previous)) = (stage_at_kill(&stage), old.get(model)) else {
            continue;
        };
        if *previous <= reached {
            continue;
        }
        let (factor, speed) = match gauge {
            None => (MAX_FACTOR, None),
            Some(Ok(gauge)) => match gauge.speed_for(model, 0) {
                Ok(speed) => (speed.clamp(1.0, MAX_FACTOR), Some(speed)),
                Err(why) => {
                    skipped.push(format!("{model}: {why}; re-run the shard job"));
                    continue;
                }
            },
            Some(Err(why)) => {
                skipped.push(format!("{model}: {why}; re-run the shard job"));
                continue;
            }
        };
        chosen.push(Candidate {
            model: model.to_string(),
            remeasurement: Remeasurement {
                stage,
                factor,
                speed,
            },
        });
    }
    chosen.sort_by(|a, b| a.model.cmp(&b.model));
    (chosen, skipped)
}

/// Replace `model`'s row in `results` with `row`.
fn replace_row(results: &mut Value, model: &str, row: Value) -> Result<()> {
    let rows = results
        .get_mut("model_results")
        .and_then(Value::as_array_mut)
        .context("msl_results.json has no model_results")?;
    let slot = rows
        .iter_mut()
        .find(|existing| existing.get("model_name").and_then(Value::as_str) == Some(model))
        .with_context(|| format!("{model} is not in the shard's results"))?;
    *slot = row;
    Ok(())
}

/// Re-run `candidate` alone into `dir`, and return its result row with the
/// rumoca trace copied into the shard, or why it produced none.
fn remeasure_one(
    candidate: &Candidate,
    dir: &Path,
    shard_dir: &Path,
    parity_args: &[String],
) -> Result<Option<Value>> {
    let Remeasurement { stage, factor, .. } = &candidate.remeasurement;
    let status = Command::new(std::env::current_exe().context("locate xtask")?)
        .args(["verify", "msl-parity", "--results-dir"])
        .arg(dir)
        .args(["--sim-match-exact", "--sim-match", &candidate.model])
        .arg("--raise-stage")
        .arg(format!("{stage}={factor}"))
        .args(["--no-remote-quality-baseline", "--allow-unmeasured-parity"])
        .args(parity_args)
        .status()
        .context("run a focused msl-parity")?;
    println!(
        "msl-remeasure: {} with {stage} x{factor:.2}: exit {status}",
        candidate.model
    );
    let path = dir.join("msl_results.json");
    if !path.is_file() {
        return Ok(None);
    }
    let results = read_json(&path)?;
    let Some(row) = results
        .get("model_results")
        .and_then(Value::as_array)
        .and_then(|rows| {
            rows.iter()
                .find(|row| row.get("model_name").and_then(Value::as_str) == Some(&candidate.model))
        })
        .cloned()
    else {
        return Ok(None);
    };
    if let Some(trace) = row.get("sim_trace_file").and_then(Value::as_str)
        && !Path::new(trace).is_absolute()
    {
        let target = shard_dir.join(trace);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::copy(dir.join(trace), &target)
            .with_context(|| format!("copy {trace} into the shard"))?;
    }
    Ok(Some(row))
}

/// Which old sides the candidates came from: the promoted baseline always,
/// tested main when its results were given, and why not when it had none.
fn tested_main_source(args: &VerifyMslRemeasureArgs) -> Value {
    match (&args.tested_main_results, &args.tested_main_unavailable) {
        (Some(_), _) => json!({ "state": "results" }),
        (None, Some(reason)) => json!({ "state": "unavailable", "reason": reason }),
        (None, None) => json!({ "state": "not_given" }),
    }
}

fn remeasured_record(
    remeasured: &Map<String, Value>,
    skipped: &[String],
    args: &VerifyMslRemeasureArgs,
) -> Value {
    json!({
        "schema_version": 1,
        "models": remeasured,
        "not_remeasured": skipped,
        "tested_main": tested_main_source(args),
    })
}

pub(crate) fn run(root: &Path, args: &VerifyMslRemeasureArgs) -> Result<()> {
    let baseline_path = match &args.baseline {
        Some(path) => path.clone(),
        None => super::msl_quality_baseline::promoted_baseline_or_offline_fallback(root)?,
    };
    let baseline = read_json(&baseline_path)?;
    // The reviewed chain of this commit, from the version it reaches down to
    // the baseline's: the crossing's pinned evidence and reviewed changes.
    let reviewed = super::msl_quality_baseline::reference_boundary::reviewed_chain_snapshot()?;
    let crossed = crossed_links(&reviewed, &baseline).map_err(anyhow::Error::msg)?;
    let promoted = previous_states(&baseline, &crossed, root).map_err(anyhow::Error::msg)?;
    let tested_main = args
        .tested_main_results
        .as_deref()
        .map(|path| read_json(path).and_then(|results| row_stages(&results)))
        .transpose()?;
    let old = old_side_stages(&promoted, tested_main.as_ref());
    let results_path = args.results_dir.join("msl_results.json");
    let mut results = read_json(&results_path)?;
    let reference = read_json(&args.results_dir.join("omc_simulation_reference.json"))?;
    let timings = calibration_timings(&reference);
    // With a stored frame, the shard's own speed scales the budget; before
    // the frame exists (the first crossing) it is set only at the merge.
    let gauge = baseline
        .get("omc_calibration_references")
        .map(|_| {
            frame_references(&baseline, &timings)
                .map_err(anyhow::Error::msg)
                .map(|references| {
                    references.map(|references| SpeedGauge::new(&timings, &references))
                })
        })
        .transpose()?;
    let (chosen, mut skipped) = candidates(&results, &old, gauge.as_ref());
    let mut remeasured = Map::new();
    for (index, candidate) in chosen.iter().enumerate() {
        if index >= args.max_models {
            skipped.push(format!(
                "{}: over the cap of {} re-measurements; re-run the shard job",
                candidate.model, args.max_models
            ));
            continue;
        }
        let dir = args.results_dir.join("remeasure").join(index.to_string());
        if dir.exists() {
            bail!(
                "{} exists; a re-measurement never reuses a directory",
                dir.display()
            );
        }
        fs::create_dir_all(&dir)?;
        match remeasure_one(candidate, &dir, &args.results_dir, &args.parity_args)? {
            Some(row) => {
                replace_row(&mut results, &candidate.model, row)?;
                remeasured.insert(
                    candidate.model.clone(),
                    serde_json::to_value(&candidate.remeasurement)?,
                );
            }
            None => skipped.push(format!(
                "{}: the re-measurement produced no result; re-run the shard job",
                candidate.model
            )),
        }
    }
    for line in &skipped {
        println!("msl-remeasure: not re-measured: {line}");
    }
    if !remeasured.is_empty() {
        fs::write(&results_path, serde_json::to_vec_pretty(&results)?)
            .with_context(|| format!("write {}", results_path.display()))?;
    }
    let record = remeasured_record(&remeasured, &skipped, args);
    fs::write(
        args.results_dir.join(REMEASURED_FILE),
        serde_json::to_vec_pretty(&record)?,
    )?;
    println!(
        "msl-remeasure: {} re-measured, {} not",
        remeasured.len(),
        skipped.len()
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_msl_quality::speed::{CALIBRATION_MODELS, OmcTiming, bootstrap_references};
    use rumoca_msl_quality::{BandLabel, ModelStage, ModelState, StageSeconds, TraceState};

    fn state(stage: ModelStage) -> ModelState {
        ModelState {
            stage,
            outcome: None,
            trace: TraceState::Scored(BandLabel::High),
            compared_channels: 2,
            trace_violation_mass: Some(0.0),
            mean_channel_bounded_l1: Some(0.0),
            certified: true,
            roster: false,
            stage_seconds: StageSeconds::default(),
        }
    }

    fn gauge(speed: f64) -> SpeedGauge {
        let timings = CALIBRATION_MODELS
            .iter()
            .enumerate()
            .map(|(i, model)| OmcTiming {
                shard: 0,
                model: (*model).to_string(),
                wall_seconds: (2.0 + i as f64) * speed,
                fresh: true,
            })
            .collect::<Vec<_>>();
        let nominal = timings
            .iter()
            .map(|timing| OmcTiming {
                wall_seconds: timing.wall_seconds / speed,
                ..timing.clone()
            })
            .collect::<Vec<_>>();
        SpeedGauge::new(&timings, &bootstrap_references(&nominal).unwrap())
    }

    fn results() -> Value {
        json!({ "model_results": [
            { "model_name": "Killed", "sim_status": "sim_timeout" },
            { "model_name": "KilledCompiling", "timeout_phase": "Flatten" },
            { "model_name": "NewlyKilled", "timeout_phase": "Sim" },
            { "model_name": "WasKilledToo", "timeout_phase": "Sim" },
            { "model_name": "Fine", "sim_status": "sim_ok" },
        ] })
    }

    fn promoted() -> ModelStates {
        ModelStates::from([
            ("Killed".to_string(), state(ModelStage::Simulated)),
            ("KilledCompiling".to_string(), state(ModelStage::Simulated)),
            // It never got past initialization before: not a lost stage.
            ("WasKilledToo".to_string(), state(ModelStage::Initialized)),
            ("Fine".to_string(), state(ModelStage::Simulated)),
        ])
    }

    fn old() -> BTreeMap<String, ModelStage> {
        old_side_stages(&promoted(), None)
    }

    #[test]
    fn a_stage_tested_main_completed_is_re_measured_too() {
        // Tested main simulated WasKilledToo, which the promoted baseline only
        // initialized: progress since the promotion, re-measured all the same.
        let tested_main = row_stages(&json!({ "model_results": [
            { "model_name": "WasKilledToo", "phase_reached": "Success", "ic_status": "ic_ok", "sim_status": "sim_ok" },
            { "model_name": "NewlyKilled", "phase_reached": "Flatten" },
        ] }))
        .unwrap();
        let old = old_side_stages(&promoted(), Some(&tested_main));
        let slow = Ok(gauge(2.46));
        let (chosen, _) = candidates(&results(), &old, Some(&slow));
        let names = chosen.iter().map(|c| c.model.as_str()).collect::<Vec<_>>();
        assert_eq!(names, ["Killed", "KilledCompiling", "WasKilledToo"]);
        // A model tested main never got as far is still not a candidate.
        assert!(!names.contains(&"NewlyKilled"));
        // A row with an unknown phase is refused, not read as anything.
        let bad = json!({ "model_results": [{ "model_name": "X", "phase_reached": "Teleport" }] });
        assert!(row_stages(&bad).is_err());
    }

    #[test]
    fn only_stages_the_old_side_completed_are_re_measured_at_the_shard_speed() {
        let slow = Ok(gauge(2.46));
        let (chosen, skipped) = candidates(&results(), &old(), Some(&slow));
        assert!(skipped.is_empty(), "{skipped:?}");
        let names = chosen.iter().map(|c| c.model.as_str()).collect::<Vec<_>>();
        assert_eq!(names, ["Killed", "KilledCompiling"]);
        let killed = &chosen[0].remeasurement;
        assert_eq!(killed.stage, "Sim");
        assert!((killed.factor - 2.46).abs() < 1e-9);
        assert_eq!(chosen[1].remeasurement.stage, "Flatten");
        // A fast runner never lowers a budget; a very slow one is capped.
        let (fast, _) = candidates(&results(), &old(), Some(&Ok(gauge(0.4))));
        assert_eq!(fast[0].remeasurement.factor, 1.0);
        let (capped, _) = candidates(&results(), &old(), Some(&Ok(gauge(2.99))));
        assert!(capped[0].remeasurement.factor <= MAX_FACTOR);
    }

    #[test]
    fn an_untrusted_gauge_re_measures_nothing_and_says_what_to_do() {
        let untrusted = Err("cannot set the frame: 3 calibration timings are missing".to_string());
        let (chosen, skipped) = candidates(&results(), &old(), Some(&untrusted));
        assert!(chosen.is_empty());
        assert_eq!(skipped.len(), 2);
        assert!(
            skipped
                .iter()
                .all(|line| line.ends_with("re-run the shard job"))
        );
        // Before the frame exists, the first crossing raises by the cap.
        let (first, _) = candidates(&results(), &old(), None);
        assert!(first.iter().all(|c| c.remeasurement.factor == MAX_FACTOR));
        assert!(first.iter().all(|c| c.remeasurement.speed.is_none()));
    }

    #[test]
    fn a_raised_stage_is_a_phase_and_a_factor_from_one_to_three() {
        assert_eq!(
            raised_stage_parts("Sim=2.46"),
            Ok(("Sim".to_string(), 2.46))
        );
        for bad in ["Sim", "=2", "Sim=0.5", "Sim=3.5", "Sim=x"] {
            assert!(raised_stage_parts(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn a_re_measured_row_replaces_the_killed_one() {
        let mut results = results();
        replace_row(
            &mut results,
            "Killed",
            json!({ "model_name": "Killed", "sim_status": "sim_ok" }),
        )
        .unwrap();
        assert_eq!(results["model_results"][0]["sim_status"], "sim_ok");
        assert!(replace_row(&mut results, "Absent", json!({})).is_err());
    }

    #[derive(clap::Parser)]
    struct Cli {
        #[command(flatten)]
        args: VerifyMslRemeasureArgs,
    }

    fn parse(extra: &[&str]) -> std::result::Result<VerifyMslRemeasureArgs, clap::Error> {
        use clap::Parser;
        let argv = ["remeasure", "--results-dir", "r"].iter().chain(extra);
        Cli::try_parse_from(argv).map(|cli| cli.args)
    }

    #[test]
    fn a_tested_main_without_a_snapshot_is_recorded_with_its_reason() {
        let reason = "main baseline unavailable for abc: main CI produced no MSL snapshot";
        let args = parse(&["--tested-main-unavailable", reason]).unwrap();
        let record = remeasured_record(&Map::new(), &[], &args);
        assert_eq!(
            record["tested_main"],
            json!({ "state": "unavailable", "reason": reason })
        );
        let args = parse(&["--tested-main-results", "main.json"]).unwrap();
        assert_eq!(
            remeasured_record(&Map::new(), &[], &args)["tested_main"],
            json!({ "state": "results" })
        );
        let args = parse(&[]).unwrap();
        assert_eq!(
            remeasured_record(&Map::new(), &[], &args)["tested_main"],
            json!({ "state": "not_given" })
        );
    }

    #[test]
    fn tested_main_results_and_unavailable_are_mutually_exclusive() {
        let error = parse(&[
            "--tested-main-results",
            "main.json",
            "--tested-main-unavailable",
            "none",
        ])
        .expect_err("both old-side inputs refuse");
        assert_eq!(error.kind(), clap::error::ErrorKind::ArgumentConflict);
    }

    #[test]
    fn an_empty_unavailable_reason_is_refused() {
        let error = parse(&["--tested-main-unavailable", ""])
            .expect_err("an unavailable main must say why");
        assert_eq!(error.kind(), clap::error::ErrorKind::InvalidValue);
    }
}
