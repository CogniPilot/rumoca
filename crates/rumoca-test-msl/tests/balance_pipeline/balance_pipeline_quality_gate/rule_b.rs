//! Rule B: certification timing normalized by the shard's own OMC speed
//! (`rumoca_msl_quality::speed`).
//!
//! A strict-high model is certified when its worst phase share, with every
//! phase time divided by its shard's speed `k`, fits the margin. `k` reads this
//! run's fresh OMC timings against the reference walls the old side (the one
//! the per-model rule judges against) stores, so a slow runner neither
//! uncertifies a model nor lets a fast one certify a near-budget model on
//! luck. When the gauge cannot be trusted the model is not certified and the
//! state says why: a transition from certified is then inconclusive, never a
//! judgement on absolute time. Each model also stores its OMC wall in the
//! reference frame for the next run's gauge; the first crossing fixes the
//! frame at the median of this run's shard speeds.

use super::*;
use rumoca_msl_quality::snapshot::{crossed_links, has_old_side, previous_states};
use rumoca_msl_quality::speed::{OmcTiming, SpeedGauge};
use rumoca_msl_quality::{BandLabel, ModelStates, TraceState};

/// This run's OMC timings from the (merged) reference file. A merged file
/// carries each model's `shard` and `timing_fresh`; an unsharded run is one
/// shard whose fresh timings carry the file's own `timing_run`.
pub(super) fn omc_timings(reference: &serde_json::Value) -> BTreeMap<String, OmcTiming> {
    let own_run = reference.get("timing_run").cloned().unwrap_or_default();
    let Some(models) = reference
        .get("models")
        .and_then(serde_json::Value::as_object)
    else {
        return BTreeMap::new();
    };
    models
        .iter()
        .filter_map(|(model, entry)| {
            let wall_seconds = entry.get("omc_wall_seconds")?.as_f64()?;
            let shard = entry
                .get("shard")
                .and_then(serde_json::Value::as_u64)
                .and_then(|shard| u32::try_from(shard).ok())
                .unwrap_or(0);
            let fresh = entry
                .get("timing_fresh")
                .and_then(serde_json::Value::as_bool)
                .unwrap_or_else(|| timing_run_of(entry) == own_run);
            Some((
                model.clone(),
                OmcTiming {
                    shard,
                    wall_seconds,
                    fresh,
                },
            ))
        })
        .collect()
}

/// The run provenance a model's OMC timing context carries (null when none).
pub(in super::super) fn timing_run_of(entry: &serde_json::Value) -> serde_json::Value {
    entry
        .get("omc_timing_context")
        .and_then(|context| context.get("run"))
        .cloned()
        .unwrap_or_default()
}

/// The old side's states the gauge reads references from, and whether they
/// came from a first crossing's pinned evidence; none when there is no old
/// side with states (then there is no frame yet).
fn old_side(current: &serde_json::Value) -> io::Result<Option<(ModelStates, bool)>> {
    let path = promoted_baseline_file().unwrap_or_else(msl_quality_baseline_path);
    let baseline: serde_json::Value = serde_json::from_slice(&fs::read(&path)?)
        .map_err(|error| io::Error::other(format!("{}: {error}", path.display())))?;
    let crossed = crossed_links(current, &baseline).map_err(io::Error::other)?;
    if !has_old_side(&baseline, &crossed) {
        return Ok(None);
    }
    let from_evidence = baseline.get("model_states").is_none();
    previous_states(&baseline, &crossed, &msl_workspace_root())
        .map(|states| Some((states, from_evidence)))
        .map_err(io::Error::other)
}

/// Re-judge `states`' certification under rule B and store each model's
/// reference wall. `references` are the old side's walls; with none there is
/// no frame yet, and the run's own walls become the first references.
pub(super) fn normalize_certification(
    states: &mut ModelStates,
    results: &[MslModelResult],
    timings: &BTreeMap<String, OmcTiming>,
    old_side: Option<(&ModelStates, bool)>,
    budgets: PhaseBudgets,
) {
    let references = old_side
        .map(|(states, _)| {
            states
                .iter()
                .filter_map(|(model, state)| {
                    Some((model.clone(), state.stage_seconds.omc_wall_reference?))
                })
                .collect::<BTreeMap<_, _>>()
        })
        .unwrap_or_default();
    if references.is_empty() {
        for (model, state) in states.iter_mut() {
            state.stage_seconds.omc_wall_reference =
                timings.get(model).map(|timing| timing.wall_seconds);
        }
        return;
    }
    let gauge = SpeedGauge::new(timings, &references);
    let first_crossing = old_side.is_some_and(|(_, from_evidence)| from_evidence);
    let frame = if first_crossing {
        gauge.nominal_speed().unwrap_or(1.0)
    } else {
        1.0
    };
    let results = results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect::<HashMap<_, _>>();
    for (model, state) in states.iter_mut() {
        let timing = timings.get(model);
        let speed = timing.map_or_else(
            || {
                Err(format!(
                    "{model} has no OMC timing on its shard to place it"
                ))
            },
            |timing| gauge.speed_for(model, timing.shard),
        );
        state.stage_seconds.omc_wall_reference = match (&speed, timing) {
            (Ok(speed), Some(timing)) => Some(timing.wall_seconds / speed * frame),
            _ => references.get(model).copied(),
        };
        if state.trace != TraceState::Scored(BandLabel::High) {
            continue;
        }
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
    let reference_path = omc_simulation_reference_path();
    let reference: serde_json::Value = match fs::read(&reference_path) {
        Ok(bytes) => serde_json::from_slice(&bytes)
            .map_err(|error| io::Error::other(format!("{}: {error}", reference_path.display())))?,
        Err(error) if error.kind() == io::ErrorKind::NotFound => serde_json::Value::Null,
        Err(error) => return Err(error),
    };
    if let Some(windows) = reference.get("shard_timing_windows") {
        for window in windows.as_array().into_iter().flatten() {
            println!(
                "MSL rule B: shard {} rumoca {} .. OMC {}: gap {} s",
                window["shard"], window["rumoca"], window["omc"], window["gap_seconds"]
            );
        }
        root.insert("shard_timing_windows".to_string(), windows.clone());
    }
    let current = serde_json::Value::Object(root.clone());
    let old = old_side(&current)?;
    normalize_certification(
        &mut states,
        &summary.model_results,
        &omc_timings(&reference),
        old.as_ref().map(|(states, evidence)| (states, *evidence)),
        budgets,
    );
    let (certified, held) = rosters_from_states(&states, &summary.model_results, budgets);
    let to_value = |value: serde_json::Result<serde_json::Value>| {
        value.map_err(|error| io::Error::other(format!("failed to serialize a roster: {error}")))
    };
    root.insert(
        "certified_strict_high_models".to_string(),
        to_value(serde_json::to_value(&certified))?,
    );
    root.insert(
        "timing_margin_models".to_string(),
        to_value(serde_json::to_value(&held))?,
    );
    let value = serde_json::to_value(states)
        .map_err(|error| io::Error::other(format!("failed to serialize model states: {error}")))?;
    root.insert("model_states".to_string(), value);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_msl_quality::{ModelStage, ModelState, StageSeconds};

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

    /// `count` other models on `shard` whose OMC ran `speed` times their
    /// reference, plus the judged model, with its own old-side state.
    fn shard_of(
        shard: u32,
        speed: f64,
        count: usize,
        judged: &str,
    ) -> (BTreeMap<String, OmcTiming>, ModelStates) {
        let mut timings = BTreeMap::new();
        let mut old = ModelStates::new();
        for i in 0..count {
            let model = format!("Other{i}");
            timings.insert(
                model.clone(),
                OmcTiming {
                    shard,
                    wall_seconds: 4.0 * speed,
                    fresh: true,
                },
            );
            let mut state = strict_high();
            state.stage_seconds.omc_wall_reference = Some(4.0);
            old.insert(model, state);
        }
        timings.insert(
            judged.to_string(),
            OmcTiming {
                shard,
                wall_seconds: 2.0 * speed,
                fresh: true,
            },
        );
        let mut state = strict_high();
        state.stage_seconds.omc_wall_reference = Some(2.0);
        old.insert(judged.to_string(), state);
        (timings, old)
    }

    fn judged(
        run_seconds: f64,
        timings: &BTreeMap<String, OmcTiming>,
        old: &ModelStates,
        from_evidence: bool,
    ) -> ModelState {
        let mut states = ModelStates::from([("Cauer".to_string(), strict_high())]);
        normalize_certification(
            &mut states,
            &[result("Cauer", run_seconds)],
            timings,
            Some((old, from_evidence)),
            budgets(),
        );
        states.remove("Cauer").unwrap()
    }

    #[test]
    fn a_slow_runner_no_longer_uncertifies_a_model() {
        // CauerLowPassSC on shard 3 of run 46055840822: 42.9 s at k = 2.46.
        let (timings, old) = shard_of(3, 2.46, 40, "Cauer");
        let state = judged(42.9, &timings, &old, false);
        assert!(state.certified, "{:?}", state.stage_seconds);
        assert_eq!(state.stage_seconds.speed, Some(2.46));
        // Its reference stays in the frame: 2.0 s, not 4.92 s.
        let reference = state.stage_seconds.omc_wall_reference.unwrap();
        assert!((reference - 2.0).abs() < 1e-12, "{reference}");
    }

    #[test]
    fn a_genuine_slowdown_on_a_nominal_runner_still_uncertifies() {
        // 2x rumoca slowdown (16 s -> 32 s, over the 22.5 s margin) at k = 1.
        let (timings, old) = shard_of(1, 1.0, 40, "Cauer");
        let state = judged(32.0, &timings, &old, false);
        assert!(!state.certified);
        assert_eq!(state.stage_seconds.timing_inconclusive, None);
        assert!(judged(16.0, &timings, &old, false).certified);
        // A fast runner tightens the margin: 16 s at k = 0.4 reads 40 s.
        let (fast, old) = shard_of(2, 0.4, 40, "Cauer");
        assert!(!judged(16.0, &fast, &old, false).certified);
    }

    #[test]
    fn too_few_fresh_timings_is_inconclusive_not_absolute() {
        let (timings, old) = shard_of(3, 2.46, 29, "Cauer");
        let state = judged(10.0, &timings, &old, false);
        assert!(!state.certified);
        let why = state.stage_seconds.timing_inconclusive.unwrap();
        assert!(why.contains("fewer than 30"), "{why}");
        // A stale timing does not count towards the 30.
        let (mut timings, old) = shard_of(3, 2.46, 30, "Cauer");
        timings.get_mut("Other0").unwrap().fresh = false;
        assert!(
            judged(10.0, &timings, &old, false)
                .stage_seconds
                .timing_inconclusive
                .is_some()
        );
    }

    #[test]
    fn the_first_crossing_fixes_the_frame_and_no_frame_stores_raw_walls() {
        // From evidence on a shard at k = 2: the frame is the median shard
        // speed (2.0 here), so the stored reference is 2.0 / 2.0 * 2.0 s.
        let (timings, old) = shard_of(1, 2.0, 40, "Cauer");
        let state = judged(10.0, &timings, &old, true);
        let reference = state.stage_seconds.omc_wall_reference.unwrap();
        assert!((reference - 4.0).abs() < 1e-12, "{reference}");
        // No old side with references: certification is left as found and the
        // run's own walls become the first references.
        let mut states = ModelStates::from([("Cauer".to_string(), strict_high())]);
        normalize_certification(&mut states, &[], &timings, None, budgets());
        let state = &states["Cauer"];
        assert!(state.certified);
        assert_eq!(state.stage_seconds.omc_wall_reference, Some(4.0));
    }

    #[test]
    fn a_merged_reference_says_each_timings_shard_and_freshness() {
        let reference = serde_json::json!({
            "timing_run": { "run_id": "1" },
            "models": {
                "A": { "omc_wall_seconds": 2.0, "shard": 3, "timing_fresh": true },
                "B": { "omc_wall_seconds": 2.0, "omc_timing_context": { "run": { "run_id": "1" } } },
                "C": { "omc_wall_seconds": 2.0, "omc_timing_context": { "run": { "run_id": "0" } } },
                "D": { "status": "failed" },
            },
        });
        let timings = omc_timings(&reference);
        assert_eq!(timings.len(), 3);
        assert_eq!(timings["A"].shard, 3);
        assert!(timings["A"].fresh && timings["B"].fresh && !timings["C"].fresh);
    }

    #[test]
    fn the_rosters_are_the_states_certification() {
        let (mut timings, old) = shard_of(3, 2.46, 40, "Cauer");
        let mut slow = timings["Cauer"];
        slow.wall_seconds = 1.0;
        timings.insert("Slow".to_string(), slow);
        let mut states = ModelStates::from([
            ("Cauer".to_string(), strict_high()),
            ("Slow".to_string(), strict_high()),
        ]);
        let results = [result("Cauer", 42.9), result("Slow", 80.0)];
        normalize_certification(
            &mut states,
            &results,
            &timings,
            Some((&old, false)),
            budgets(),
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
        assert_eq!(slow.speed, Some(2.46));
        assert!((slow.budget_share - 80.0 / 2.46 / 45.0).abs() < 1e-12);
        assert_eq!(certified.len() + held.len(), 2);
    }
}
