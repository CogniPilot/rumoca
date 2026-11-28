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
use rumoca_msl_quality::speed::{OmcTiming, SpeedGauge, bootstrap_references};
use rumoca_msl_quality::{BandLabel, ModelStates, TraceState};

/// The calibration timings of a (merged) reference file: a merged file's
/// `calibration_by_shard`, or an unsharded run's own section as shard 0.
pub(super) fn calibration_timings(reference: &serde_json::Value) -> Vec<OmcTiming> {
    let own = reference
        .get("calibration_timings")
        .filter(|section| !section.is_null());
    let sections: Vec<(u32, &serde_json::Value, bool)> = match reference
        .get("calibration_by_shard")
        .and_then(serde_json::Value::as_object)
    {
        Some(by_shard) => by_shard
            .iter()
            .filter_map(|(shard, section)| {
                let fresh = section.get("fresh").and_then(serde_json::Value::as_bool);
                Some((shard.parse().ok()?, section, fresh.unwrap_or(false)))
            })
            .collect(),
        None => own
            .map(|section| {
                let fresh = timing_run_of(section)
                    == reference.get("timing_run").cloned().unwrap_or_default();
                vec![(0, section, fresh)]
            })
            .unwrap_or_default(),
    };
    sections
        .into_iter()
        .flat_map(|(shard, section, fresh)| {
            section
                .get("models")
                .and_then(serde_json::Value::as_object)
                .into_iter()
                .flatten()
                .filter(|(_, entry)| {
                    entry.get("status").and_then(serde_json::Value::as_str) == Some("success")
                })
                .filter_map(move |(model, entry)| {
                    Some(OmcTiming {
                        shard,
                        model: model.clone(),
                        wall_seconds: entry.get("omc_wall_seconds")?.as_f64()?,
                        fresh,
                    })
                })
        })
        .collect()
}

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

/// The run provenance a timing context carries (null when none), from an OMC
/// model entry or a calibration section.
pub(in super::super) fn timing_run_of(entry: &serde_json::Value) -> serde_json::Value {
    entry
        .get("omc_timing_context")
        .or_else(|| entry.get("context"))
        .and_then(|context| context.get("run"))
        .cloned()
        .unwrap_or_default()
}

/// The calibration references the baseline stores, if any.
fn stored_references(baseline: &serde_json::Value) -> io::Result<Option<BTreeMap<String, f64>>> {
    baseline
        .get("omc_calibration_references")
        .map(|references| {
            serde_json::from_value(references.clone())
                .map_err(|error| io::Error::other(format!("omc_calibration_references: {error}")))
        })
        .transpose()
}

/// Re-judge `states`' certification under rule B against `references`.
pub(super) fn normalize_certification(
    states: &mut ModelStates,
    results: &[MslModelResult],
    gauge: &SpeedGauge,
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
        let speed = match shards.get(model) {
            Some(&shard) => gauge
                .speed_for(model, shard)
                .map_err(|why| format!("{why}; re-run the shard job")),
            None => Err(format!("{model} has no OMC timing to place it on a shard")),
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
    let read_json = |path: &Path| -> io::Result<serde_json::Value> {
        match fs::read(path) {
            Ok(bytes) => serde_json::from_slice(&bytes)
                .map_err(|error| io::Error::other(format!("{}: {error}", path.display()))),
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(serde_json::Value::Null),
            Err(error) => Err(error),
        }
    };
    let reference = read_json(&omc_simulation_reference_path())?;
    if let Some(windows) = reference.get("shard_timing_windows") {
        for window in windows.as_array().into_iter().flatten() {
            println!(
                "MSL rule B: shard {} rumoca {} .. OMC {}: gap {} s",
                window["shard"], window["rumoca"], window["omc"], window["gap_seconds"]
            );
        }
        root.insert("shard_timing_windows".to_string(), windows.clone());
    }
    let baseline = read_json(&promoted_baseline_file().unwrap_or_else(msl_quality_baseline_path))?;
    let timings = calibration_timings(&reference);
    let references = match stored_references(&baseline)? {
        Some(references) => references,
        None => bootstrap_references(&timings),
    };
    let gauge = SpeedGauge::new(&timings, &references);
    normalize_certification(
        &mut states,
        &summary.model_results,
        &gauge,
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
    root.insert(
        "omc_calibration_references".to_string(),
        to_value(serde_json::to_value(&references))?,
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_msl_quality::speed::CALIBRATION_MODELS;
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
        normalize_certification(
            states,
            results,
            &SpeedGauge::new(timings, references),
            &shards,
            budgets(),
        );
    }

    #[test]
    fn a_slow_runner_no_longer_uncertifies_a_model() {
        // CauerLowPassSC on a runner 2.46x the frame's: 42.9 s reads 17.4 s.
        let references = bootstrap_references(&calibrated(1, 1.0));
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
        let references = bootstrap_references(&calibrated(1, 1.0));
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
        let references = bootstrap_references(&calibrated(1, 1.0));
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
        let references = bootstrap_references(&timings);
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
        let references = bootstrap_references(&calibrated(1, 1.0));
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
}
