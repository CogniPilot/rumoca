//! Rule B: certification timing normalized by the shard's runner speed
//! (`rumoca_msl_quality::speed`).
//!
//! Every shard OMC-times the same pinned calibration set, so its speed `k` is
//! measured against one common frame: the calibration references the baseline
//! stores, fixed when the frame was set. A strict-high model is certified when
//! its worst phase share, every phase time divided by its shard's `k`, fits the
//! margin, so a slow runner neither uncertifies a model nor lets a fast one
//! certify a near-budget model on luck. When the gauge cannot be trusted the
//! model is not certified and its state says why (naming the shard, the reason
//! and the fix): a transition from certified is then inconclusive, never a
//! judgement on absolute time. A baseline without references sets the frame
//! from this run: each calibration model's median over the shards, so the
//! frame is the run's median runner, and certification and the stored
//! references share it.

use super::*;
use rumoca_msl_quality::speed::{SpeedGauge, calibration_timings, frame_references};
use rumoca_msl_quality::{BandLabel, ModelStates, Remeasurement, TraceState, stage_at_kill};

/// Each OMC model's shard in a (merged) reference file (0 when unsharded).
pub(super) fn model_shards(reference: &serde_json::Value) -> BTreeMap<String, u32> {
    reference
        .get("models")
        .and_then(serde_json::Value::as_object)
        .into_iter()
        .flatten()
        .map(|(model, entry)| {
            let shard = entry
                .get("shard")
                .and_then(serde_json::Value::as_u64)
                .and_then(|shard| u32::try_from(shard).ok())
                .unwrap_or(0);
            (model.clone(), shard)
        })
        .collect()
}

/// Mark each re-measured model's stage times with its re-measurement (the
/// merged reference's `remeasured`), so a stage killed again is final.
pub(super) fn apply_remeasurements(
    states: &mut ModelStates,
    reference: &serde_json::Value,
) -> io::Result<()> {
    let Some(remeasured) = reference.get("remeasured") else {
        return Ok(());
    };
    let remeasured: BTreeMap<String, Remeasurement> = serde_json::from_value(remeasured.clone())
        .map_err(|error| io::Error::other(format!("remeasured: {error}")))?;
    for (model, remeasurement) in remeasured {
        if let Some(state) = states.get_mut(&model) {
            state.stage_seconds.remeasured = Some(remeasurement);
        }
    }
    Ok(())
}

/// Judge every re-measured stage that completed: its own time divided by
/// the shard's speed must fit the stage's configured budget. Over it, the
/// stage counts as killed again (a stage drop); with no trusted speed it
/// cannot conclude (inconclusive). The shard-side factor only bounds wall time,
/// so a raised budget, the cap before the first frame included, never passes a
/// slowdown by itself.
pub(super) fn judge_remeasured_stages(
    states: &mut ModelStates,
    results: &[MslModelResult],
    gauge: Result<&SpeedGauge, &str>,
    shards: &BTreeMap<String, u32>,
    budgets: PhaseBudgets,
) {
    let results = results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect::<HashMap<_, _>>();
    for (model, state) in states.iter_mut() {
        let timing = &state.stage_seconds;
        let (Some(remeasured), None) = (&timing.remeasured, &timing.timeout_phase) else {
            continue;
        };
        let stage = remeasured.stage.clone();
        let measured = results
            .get(model.as_str())
            .and_then(|result| budgets.stage_time_and_budget(result, &stage));
        // An unknown stage has no time or budget: judged below, never skipped.
        let speed = match (gauge, shards.get(model)) {
            (Err(why), _) => Err(format!("{why}; re-run the shard jobs")),
            (Ok(gauge), Some(&shard)) => gauge
                .speed_for(model, shard)
                .map_err(|why| format!("{why}; re-run the shard job")),
            (Ok(_), None) => Err(format!("{model} has no OMC timing to place it on a shard")),
        };
        let verdict = match (measured, speed) {
            (Some((seconds, budget)), Ok(speed))
                if seconds / speed <= budget && stage_at_kill(&stage).is_some() =>
            {
                continue;
            }
            (Some(_), Ok(_)) => None,
            (None, _) => Some(format!(
                "{model}'s re-measured {stage} time was not recorded"
            )),
            (_, Err(why)) => Some(why),
        };
        // A stage name this rule does not know (a renamed worker phase) is
        // never exempt: the model is held at the stage before compiling ends.
        let (reached, verdict) = match stage_at_kill(&stage) {
            Some(reached) => (reached, verdict),
            None => (
                rumoca_msl_quality::ModelStage::Compiled.min(state.stage),
                Some(format!(
                    "{model}'s re-measured stage {stage:?} is not a known stage"
                )),
            ),
        };
        state.stage = reached;
        state.stage_seconds.timeout_phase = Some(stage);
        state.stage_seconds.timing_inconclusive = verdict;
    }
}

/// Re-judge `states`' certification under rule B, with the gauge of the
/// frame, or the reason there is none.
pub(super) fn normalize_certification(
    states: &mut ModelStates,
    results: &[MslModelResult],
    gauge: Result<&SpeedGauge, &str>,
    shards: &BTreeMap<String, u32>,
    budgets: PhaseBudgets,
) {
    let results = results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect::<HashMap<_, _>>();
    for (model, state) in states.iter_mut() {
        if state.trace != TraceState::Scored(BandLabel::High) {
            continue;
        }
        let speed = match (gauge, shards.get(model)) {
            (Err(why), _) => Err(format!("{why}; re-run the shard jobs")),
            (Ok(gauge), Some(&shard)) => gauge
                .speed_for(model, shard)
                .map_err(|why| format!("{why}; re-run the shard job")),
            (Ok(_), None) => Err(format!("{model} has no OMC timing to place it on a shard")),
        };
        match speed {
            Ok(speed) => {
                state.stage_seconds.speed = Some(speed);
                state.certified = results
                    .get(model.as_str())
                    .and_then(|result| budgets.worst_share_at(result, speed))
                    .is_some_and(|worst| worst.budget_share <= CERTIFICATION_TIMING_MARGIN);
            }
            Err(why) => {
                state.certified = false;
                state.stage_seconds.timing_inconclusive = Some(why);
            }
        }
    }
}

/// The certified roster and the timing-margin roster, derived from the
/// states: one definition of certified for the snapshot, whichever path reads
/// it. Every strict-high model is in exactly one.
pub(super) fn rosters_from_states(
    states: &ModelStates,
    results: &[MslModelResult],
    budgets: PhaseBudgets,
) -> (IndexSet<String>, IndexMap<String, TimingMarginEntry>) {
    let results = results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect::<HashMap<_, _>>();
    let mut certified = IndexSet::new();
    let mut held = IndexMap::new();
    for (model, state) in states {
        if state.trace != TraceState::Scored(BandLabel::High) {
            continue;
        }
        if state.certified {
            certified.insert(model.clone());
            continue;
        }
        let timing = &state.stage_seconds;
        let worst = results
            .get(model.as_str())
            .and_then(|result| budgets.worst_share_at(result, timing.speed.unwrap_or(1.0)));
        let (phase, budget_share) = worst.map_or(("unmeasured".to_string(), 0.0), |worst| {
            (worst.phase, worst.budget_share)
        });
        held.insert(
            model.clone(),
            TimingMarginEntry {
                phase,
                budget_share,
                speed: timing.speed,
                inconclusive: timing.timing_inconclusive.clone(),
            },
        );
    }
    (certified, held)
}

/// Read a JSON file that must exist, naming it on any failure: a missing
/// file is never read as an empty value, which would surface far downstream
/// as a misleading complaint about its contents.
pub(super) fn read_required_json(path: &Path, what: &str) -> io::Result<serde_json::Value> {
    let bytes = fs::read(path).map_err(|error| {
        io::Error::new(error.kind(), format!("{what} {}: {error}", path.display()))
    })?;
    serde_json::from_slice(&bytes)
        .map_err(|error| io::Error::other(format!("{what} {}: {error}", path.display())))
}

/// Apply rule B to the `model_states` just written into `root`.
pub(super) fn insert_normalized_certification(
    root: &mut serde_json::Map<String, serde_json::Value>,
    summary: &MslSummary,
    budgets: PhaseBudgets,
) -> io::Result<()> {
    let Some(states) = root.get("model_states") else {
        return Ok(());
    };
    let mut states: ModelStates = serde_json::from_value(states.clone())
        .map_err(|error| io::Error::other(format!("model_states: {error}")))?;
    // Both files must exist here: this runs only when parity was measured, so
    // the OMC reference was written, and every run judges against a baseline.
    let reference = read_required_json(&omc_simulation_reference_path(), "OMC reference")?;
    apply_remeasurements(&mut states, &reference)?;
    if let Some(windows) = reference.get("shard_timing_windows") {
        for window in windows.as_array().into_iter().flatten() {
            println!(
                "MSL rule B: shard {} rumoca {} .. OMC {}: gap {} s",
                window["shard"], window["rumoca"], window["omc"], window["gap_seconds"]
            );
        }
        root.insert("shard_timing_windows".to_string(), windows.clone());
    }
    let baseline = read_required_json(
        &promoted_baseline_file().unwrap_or_else(msl_quality_baseline_path),
        "rule B baseline",
    )?;
    let timings = calibration_timings(&reference);
    let references = frame_references(&baseline, &timings).map_err(io::Error::other)?;
    let gauge = references
        .as_ref()
        .map(|references| SpeedGauge::new(&timings, references));
    let shards = model_shards(&reference);
    judge_remeasured_stages(
        &mut states,
        &summary.model_results,
        gauge.as_ref().map_err(|why| why.as_str()),
        &shards,
        budgets,
    );
    normalize_certification(
        &mut states,
        &summary.model_results,
        gauge.as_ref().map_err(|why| why.as_str()),
        &model_shards(&reference),
        budgets,
    );
    let (certified, held) = rosters_from_states(&states, &summary.model_results, budgets);
    let to_value = |value: serde_json::Result<serde_json::Value>| {
        value.map_err(|error| {
            io::Error::other(format!("failed to serialize rule B output: {error}"))
        })
    };
    root.insert(
        "model_states".to_string(),
        to_value(serde_json::to_value(states))?,
    );
    root.insert(
        "certified_strict_high_models".to_string(),
        to_value(serde_json::to_value(&certified))?,
    );
    root.insert(
        "timing_margin_models".to_string(),
        to_value(serde_json::to_value(&held))?,
    );
    // No frame, no references: a snapshot that could not set its frame does
    // not pass one on.
    if let Ok(references) = &references {
        root.insert(
            "omc_calibration_references".to_string(),
            to_value(serde_json::to_value(references))?,
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_msl_quality::speed::{
        CALIBRATION_MODELS, FRAME_GATE_VERSION, OmcTiming, bootstrap_references,
    };
    use rumoca_msl_quality::{ModelStage, ModelState, Regression, StageSeconds};

    /// Budgets of CI: 45 s per phase, 45 s for the run.
    fn budgets() -> PhaseBudgets {
        PhaseBudgets::new(45.0, 45.0).unwrap()
    }

    fn strict_high() -> ModelState {
        ModelState {
            stage: ModelStage::Simulated,
            outcome: None,
            trace: TraceState::Scored(BandLabel::High),
            compared_channels: 4,
            trace_violation_mass: Some(0.0),
            mean_channel_bounded_l1: Some(0.0),
            certified: true,
            roster: false,
            stage_seconds: StageSeconds::default(),
        }
    }

    fn result(model: &str, run_seconds: f64) -> MslModelResult {
        let mut result = phase_error_result(model.to_string(), "Success", None, None);
        result.sim_run_seconds = Some(run_seconds);
        result
    }

    /// The calibration set timed on `shard` at `speed` times its reference.
    fn calibrated(shard: u32, speed: f64) -> Vec<OmcTiming> {
        CALIBRATION_MODELS
            .iter()
            .enumerate()
            .map(|(i, model)| OmcTiming {
                shard,
                model: (*model).to_string(),
                wall_seconds: (2.0 + i as f64) * speed,
                fresh: true,
            })
            .collect()
    }

    fn judged(
        states: &mut ModelStates,
        results: &[MslModelResult],
        timings: &[OmcTiming],
        references: &BTreeMap<String, f64>,
        shards: &[(&str, u32)],
    ) {
        let shards = shards.iter().map(|(m, s)| (m.to_string(), *s)).collect();
        let gauge = SpeedGauge::new(timings, references);
        normalize_certification(states, results, Ok(&gauge), &shards, budgets());
    }

    #[test]
    fn a_slow_runner_no_longer_uncertifies_a_model() {
        // CauerLowPassSC on a runner 2.46x the frame's: 42.9 s reads 17.4 s.
        let references = bootstrap_references(&calibrated(1, 1.0)).unwrap();
        let mut states = ModelStates::from([("Cauer".to_string(), strict_high())]);
        judged(
            &mut states,
            &[result("Cauer", 42.9)],
            &calibrated(3, 2.46),
            &references,
            &[("Cauer", 3)],
        );
        assert!(
            states["Cauer"].certified,
            "{:?}",
            states["Cauer"].stage_seconds
        );
        let speed = states["Cauer"].stage_seconds.speed.unwrap();
        assert!((speed - 2.46).abs() < 1e-9, "{speed}");
    }

    #[test]
    fn a_genuine_slowdown_on_a_nominal_runner_still_uncertifies() {
        let references = bootstrap_references(&calibrated(1, 1.0)).unwrap();
        let timings = calibrated(1, 1.0);
        let run = |seconds: f64| {
            let mut states = ModelStates::from([("M".to_string(), strict_high())]);
            judged(
                &mut states,
                &[result("M", seconds)],
                &timings,
                &references,
                &[("M", 1)],
            );
            states.remove("M").unwrap()
        };
        // 16 s certifies; a 2x rumoca slowdown (32 s, over the 22.5 s margin) does not.
        assert!(run(16.0).certified);
        let slow = run(32.0);
        assert!(!slow.certified);
        assert_eq!(slow.stage_seconds.timing_inconclusive, None);
        // A fast runner tightens the margin: 16 s at k = 0.4 reads 40 s.
        let mut states = ModelStates::from([("M".to_string(), strict_high())]);
        judged(
            &mut states,
            &[result("M", 16.0)],
            &calibrated(2, 0.4),
            &references,
            &[("M", 2)],
        );
        assert!(!states["M"].certified);
    }

    #[test]
    fn an_untrusted_gauge_is_inconclusive_and_says_what_to_do() {
        let references = bootstrap_references(&calibrated(1, 1.0)).unwrap();
        let mut stale = calibrated(3, 2.46);
        stale.iter_mut().for_each(|timing| timing.fresh = false);
        let mut states = ModelStates::from([("Cauer".to_string(), strict_high())]);
        judged(
            &mut states,
            &[result("Cauer", 10.0)],
            &stale,
            &references,
            &[("Cauer", 3)],
        );
        let state = &states["Cauer"];
        assert!(!state.certified);
        let why = state.stage_seconds.timing_inconclusive.as_deref().unwrap();
        assert!(
            why.contains("shard 3") && why.contains("fewer than 30"),
            "{why}"
        );
        assert!(why.ends_with("re-run the shard job"), "{why}");
    }

    #[test]
    fn the_frame_a_run_sets_is_the_one_it_certifies_in() {
        // No stored references: the frame is this run's median runner, and the
        // next run reading the stored references at the same speeds judges the
        // same model the same way.
        let mut timings = calibrated(1, 1.0);
        timings.extend(calibrated(2, 2.3));
        let references = bootstrap_references(&timings).unwrap();
        let certify = |references: &BTreeMap<String, f64>| {
            let mut states = ModelStates::from([("M".to_string(), strict_high())]);
            judged(
                &mut states,
                &[result("M", 30.0)],
                &timings,
                references,
                &[("M", 2)],
            );
            states.remove("M").unwrap()
        };
        let first = certify(&references);
        let stored: BTreeMap<String, f64> =
            serde_json::from_value(serde_json::to_value(&references).unwrap()).unwrap();
        let next = certify(&stored);
        assert_eq!(first.certified, next.certified);
        assert_eq!(first.stage_seconds.speed, next.stage_seconds.speed);
        // 30 s on the 2.3x runner reads 30 / (2.3 / 1.65) = 21.5 s: certified.
        assert!(first.certified);
    }

    #[test]
    fn a_merged_reference_gives_each_shards_calibration_and_model_shard() {
        let reference = serde_json::json!({
            "calibration_by_shard": {
                "3": { "fresh": true, "models": {
                    "A": { "status": "success", "omc_wall_seconds": 2.0 },
                    "B": { "status": "failed", "omc_wall_seconds": 9.0 },
                } },
                "4": { "fresh": false, "models": { "A": { "status": "success", "omc_wall_seconds": 3.0 } } },
            },
            "models": { "M": { "shard": 3 }, "N": {} },
        });
        let timings = calibration_timings(&reference);
        assert_eq!(timings.len(), 2);
        assert!(
            timings
                .iter()
                .any(|t| t.shard == 3 && t.fresh && t.model == "A")
        );
        assert!(timings.iter().any(|t| t.shard == 4 && !t.fresh));
        let shards = model_shards(&reference);
        assert_eq!((shards["M"], shards["N"]), (3, 0));
        // An unsharded run: its own section, fresh when it carries the file's run.
        let single = serde_json::json!({
            "timing_run": { "run_id": "1" },
            "calibration_timings": { "context": { "run": { "run_id": "1" } }, "models": {
                "A": { "status": "success", "omc_wall_seconds": 2.0 } } },
        });
        let timings = calibration_timings(&single);
        assert_eq!(timings.len(), 1);
        assert!(timings[0].fresh && timings[0].shard == 0);
    }

    #[test]
    fn the_rosters_are_the_states_certification() {
        let references = bootstrap_references(&calibrated(1, 1.0)).unwrap();
        let mut states = ModelStates::from([
            ("Cauer".to_string(), strict_high()),
            ("Slow".to_string(), strict_high()),
        ]);
        let results = [result("Cauer", 42.9), result("Slow", 80.0)];
        judged(
            &mut states,
            &results,
            &calibrated(3, 2.46),
            &references,
            &[("Cauer", 3), ("Slow", 3)],
        );
        let (certified, held) = rosters_from_states(&states, &results, budgets());
        let expected = states
            .iter()
            .filter(|(_, state)| state.certified)
            .map(|(model, _)| model.clone())
            .collect::<IndexSet<_>>();
        assert_eq!(certified, expected);
        assert_eq!(certified, IndexSet::from(["Cauer".to_string()]));
        let slow = &held["Slow"];
        assert_eq!(slow.phase, "Sim");
        assert!((slow.speed.unwrap() - 2.46).abs() < 1e-9);
        assert!((slow.budget_share - 80.0 / slow.speed.unwrap() / 45.0).abs() < 1e-12);
        assert_eq!(certified.len() + held.len(), 2);
    }

    #[test]
    fn only_a_baseline_older_than_the_frame_may_set_one() {
        let timings = calibrated(1, 1.0);
        let at = |version: u64| serde_json::json!({ "quality_gate_version": version });
        // The first crossing into rule B sets the frame from this run.
        assert!(
            frame_references(&at(FRAME_GATE_VERSION - 1), &timings)
                .unwrap()
                .is_ok()
        );
        // A rule-B baseline that lost its references is refused.
        let error = frame_references(&at(FRAME_GATE_VERSION), &timings).unwrap_err();
        assert!(
            error.to_string().contains("must carry its frame"),
            "{error}"
        );
        assert!(frame_references(&serde_json::json!({}), &timings).is_err());
        // Stored references are used as they are, if they are the pinned set.
        let references = bootstrap_references(&timings).unwrap();
        let mut stored = at(FRAME_GATE_VERSION);
        stored["omc_calibration_references"] = serde_json::to_value(&references).unwrap();
        assert_eq!(
            frame_references(&stored, &[]).unwrap(),
            Ok(references.clone())
        );
        let mut drifted = references;
        drifted.remove(CALIBRATION_MODELS[0]);
        stored["omc_calibration_references"] = serde_json::to_value(&drifted).unwrap();
        assert!(frame_references(&stored, &[]).is_err());
    }

    #[test]
    fn a_frame_that_cannot_be_set_leaves_certification_inconclusive() {
        let mut timings = calibrated(1, 1.0);
        timings.retain(|timing| timing.model != CALIBRATION_MODELS[0]);
        let at = serde_json::json!({ "quality_gate_version": FRAME_GATE_VERSION - 1 });
        let why = frame_references(&at, &timings).unwrap().unwrap_err();
        let mut states = ModelStates::from([("M".to_string(), strict_high())]);
        let shards = BTreeMap::from([("M".to_string(), 1)]);
        normalize_certification(
            &mut states,
            &[result("M", 1.0)],
            Err(&why),
            &shards,
            budgets(),
        );
        let state = &states["M"];
        assert!(!state.certified);
        let reason = state.stage_seconds.timing_inconclusive.as_deref().unwrap();
        assert!(
            reason.contains("cannot set the frame") && reason.ends_with("re-run the shard jobs"),
            "{reason}"
        );
    }

    #[test]
    fn a_re_measurement_from_the_merged_reference_marks_the_state() {
        let mut states = ModelStates::from([("Killed".to_string(), strict_high())]);
        let reference = serde_json::json!({ "remeasured": {
            "Killed": { "stage": "Sim", "factor": 2.46, "speed": 2.46 },
            "Elsewhere": { "stage": "Sim", "factor": 3.0 },
        } });
        apply_remeasurements(&mut states, &reference).unwrap();
        let remeasured = states["Killed"].stage_seconds.remeasured.as_ref().unwrap();
        assert_eq!(
            (remeasured.stage.as_str(), remeasured.factor),
            ("Sim", 2.46)
        );
        let typo =
            serde_json::json!({ "remeasured": { "Killed": { "stage": "Sim", "factr": 2.0 } } });
        assert!(apply_remeasurements(&mut states, &typo).is_err());
    }

    /// A strict-high model whose killed Sim was re-measured and completed in
    /// `run_seconds`.
    fn remeasured_sim(run_seconds: f64) -> (ModelState, MslModelResult) {
        let mut state = strict_high();
        state.stage_seconds.remeasured = Some(Remeasurement {
            stage: "Sim".to_string(),
            factor: 3.0,
            speed: None,
        });
        (state, result("M", run_seconds))
    }

    fn judged_remeasured(run_seconds: f64, gauge: Result<&SpeedGauge, &str>) -> ModelState {
        let (state, result) = remeasured_sim(run_seconds);
        let mut states = ModelStates::from([("M".to_string(), state)]);
        let shards = BTreeMap::from([("M".to_string(), 1)]);
        judge_remeasured_stages(&mut states, &[result], gauge, &shards, budgets());
        states.remove("M").unwrap()
    }

    #[test]
    fn a_re_measured_stage_is_judged_on_its_normalized_time_at_the_merge() {
        use rumoca_msl_quality::{ReviewedChanges, Transition, transitions};
        // The first crossing: the frame set at the merge, this shard at k = 1.
        let timings = calibrated(1, 1.0);
        let references = bootstrap_references(&timings).unwrap();
        let gauge = SpeedGauge::new(&timings, &references);
        let before = ModelStates::from([("M".to_string(), strict_high())]);
        let judge = |state: ModelState| {
            let after = ModelStates::from([("M".to_string(), state)]);
            transitions(&before, &after, &ReviewedChanges::default())
                .remove("M")
                .unwrap()
        };
        // Completed at 2.5x the 45 s budget under the 3x cap: a slowdown, refused.
        let slow = judged_remeasured(112.5, Ok(&gauge));
        assert_eq!(slow.stage_seconds.timeout_phase.as_deref(), Some("Sim"));
        assert!(matches!(
            judge(slow),
            Transition::Regressed(Regression::StageDropped { .. })
        ));
        // Completed inside the budget: judged as a completion.
        let fine = judged_remeasured(40.0, Ok(&gauge));
        assert_eq!(fine.stage_seconds.timeout_phase, None);
        assert_eq!(judge(fine), Transition::Unchanged);
        // On a 2.46x runner, 100 s is 40.7 s in the frame: a completion.
        let slow_runner = calibrated(1, 2.46);
        let gauge = SpeedGauge::new(&slow_runner, &references);
        assert_eq!(
            judged_remeasured(100.0, Ok(&gauge))
                .stage_seconds
                .timeout_phase,
            None
        );
        // No trusted speed: it cannot conclude.
        let untrusted = judged_remeasured(40.0, Err("cannot set the frame"));
        assert_eq!(judge(untrusted), Transition::Inconclusive("watchdog kill"));
    }

    #[test]
    fn an_unknown_re_measured_stage_is_never_exempt() {
        use rumoca_msl_quality::{ReviewedChanges, Transition, transitions};
        let timings = calibrated(1, 1.0);
        let references = bootstrap_references(&timings).unwrap();
        let gauge = SpeedGauge::new(&timings, &references);
        let (mut state, result) = remeasured_sim(10.0);
        state.stage_seconds.remeasured.as_mut().unwrap().stage = "Simulate".to_string();
        let mut states = ModelStates::from([("M".to_string(), state)]);
        let shards = BTreeMap::from([("M".to_string(), 1)]);
        judge_remeasured_stages(&mut states, &[result], Ok(&gauge), &shards, budgets());
        let why = states["M"]
            .stage_seconds
            .timing_inconclusive
            .clone()
            .unwrap();
        assert!(why.contains("\"Simulate\" is not a known stage"), "{why}");
        let before = ModelStates::from([("M".to_string(), strict_high())]);
        let transition = transitions(&before, &states, &ReviewedChanges::default()).remove("M");
        assert_eq!(transition, Some(Transition::Inconclusive("watchdog kill")));
    }

    #[test]
    fn a_missing_required_file_is_refused_with_its_path() {
        let dir = std::env::temp_dir().join(format!("msl-rule-b-read-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let missing = dir.join("nix-sandbox/crates/rumoca-test-msl/msl_quality_baseline.json");
        let error = read_required_json(&missing, "rule B baseline").unwrap_err();
        let text = error.to_string();
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
        assert!(
            text.contains("rule B baseline") && text.contains(&missing.display().to_string()),
            "{text}"
        );
        assert!(!text.contains("quality_gate_version"), "{text}");
        let broken = dir.join("broken.json");
        fs::write(&broken, "{").unwrap();
        let text = read_required_json(&broken, "OMC reference")
            .unwrap_err()
            .to_string();
        assert!(
            text.contains("OMC reference") && text.contains("broken.json"),
            "{text}"
        );
        let good = dir.join("good.json");
        fs::write(&good, r#"{"quality_gate_version": 14}"#).unwrap();
        assert_eq!(
            read_required_json(&good, "x").unwrap()["quality_gate_version"],
            14
        );
        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn the_checked_in_baseline_is_found_from_the_checkout_not_the_build_path() {
        let root = std::env::temp_dir().join(format!("msl-checkout-{}", std::process::id()));
        let crate_dir = root.join("crates/rumoca-test-msl");
        let nested = crate_dir.join("target/msl/results");
        fs::create_dir_all(&nested).unwrap();
        fs::write(root.join("Cargo.toml"), "[workspace]\nmembers = []\n").unwrap();
        fs::write(crate_dir.join("Cargo.toml"), "[package]\nname = \"x\"\n").unwrap();
        assert_eq!(workspace_crate_from(&nested), Some(crate_dir.clone()));
        assert_eq!(workspace_crate_from(&root), Some(crate_dir));
        // Outside any checkout there is no runtime answer.
        assert_eq!(
            workspace_crate_from(&std::env::temp_dir().join("nowhere-at-all")),
            None
        );
        let _ = fs::remove_dir_all(&root);
    }
}
