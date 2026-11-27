use super::*;
use serde_json::json;
use std::path::PathBuf;

fn boundary(from: i64, to: i64, previous: Option<Value>) -> Value {
    let mut migration = json!({
        "from_quality_gate_version": from,
        "to_quality_gate_version": to,
        "change": format!("reviewed-boundary-v{to}"),
        "exclusions_sha256": format!("digest-v{to}"),
        "evidence_git_commit": format!("commit-v{to}"),
    });
    if let Some(previous) = previous {
        migration["previous"] = previous;
    }
    migration
}

/// Reviewed chain 1 -> 2 -> 3 -> 4; a snapshot at version N declares the
/// boundary that ends at N, with every older boundary as `previous`.
fn boundary_chain(version: i64) -> Value {
    let mut chain = None;
    for to in 2..=version {
        chain = Some(boundary(to - 1, to, chain));
    }
    chain.unwrap_or(Value::Null)
}

fn full_snapshot(version: i64) -> Value {
    let mut snapshot = json!({
        "quality_gate_version": version,
        "run_scope": "full",
        "omc_version": "OpenModelica 1.27.0",
        "simulatable_attempted": 10,
        "sim_target_models": 10,
        "parse_models": 10,
        "flatten_models": 9,
        "dae_models": 8,
        "compiled_models": 8,
        "solve_models": 7,
        "balanced_models": 8,
        "balance_denominator": 8,
        "initial_balanced_models": 8,
        "initial_unbalanced_models": 0,
        "sim_attempted": 7,
        "ic_attempted": 6,
        "ic_ok": 6,
        "sim_ok": 5,
        "partial_models": 1,
        "unbalanced_models": 0,
        "ic_solver_fail": 2,
        "trace_exceptions_sha256": format!("digest-v{version}"),
        "unexcepted_non_high_models": ["Modelica.Open.Defect"],
        "certified_strict_high_models": ["High0", "High1", "High2"],
        "runtime_ratio_stats": {
            "system_ratio_both_success": { "median": 2.0 },
            "wall_ratio_both_success": { "median": 10.0 },
        },
        "trace_accuracy_stats": {
            "models_compared": 5,
            "agreement_high": 3,
            "agreement_minor": 1,
            "agreement_deviation": 1,
            "bad_channels_total": 10,
            "severe_channels_total": 2,
            "models_with_severe_channel": 1,
            "models_with_any_channel_deviation": 2,
            "violation_mass_total": 3.5,
            "initial_condition": {
                "deviation_channels_total": 4,
                "severe_channels_total": 1,
                "violation_mass_total": 2.0,
            },
            "state_selection": {
                "exact_state_set_match_models": 4,
                "total_rumoca_only_states": 3,
                "total_omc_only_states": 2,
            },
        },
    });
    let chain = boundary_chain(version);
    if !chain.is_null() {
        snapshot["reference_boundary_migration"] = chain;
    }
    snapshot
}

fn approved_omc_migration(from: &str, to: &str) -> Value {
    let mut checked_in = full_snapshot(4);
    checked_in["omc_version"] = json!(to);
    checked_in["omc_context_migration"] = json!({
        "from_omc_version": from,
        "to_omc_version": to,
        "sim_target_models": checked_in["sim_target_models"],
    });
    checked_in
}

fn bump(snapshot: &mut Value, path: &[&str], by: i64) {
    let mut value = snapshot;
    for key in path {
        value = &mut value[*key];
    }
    *value = json!(value.as_i64().unwrap() + by);
}

/// Promote `current` over `baseline`, judged by the checked-in baseline of the
/// same commit (the current snapshot's own reviewed schema).
fn decide(current: &Value, baseline: &Value) -> RatchetDecision {
    ratchet_decision(current, baseline, &full_snapshot(4)).unwrap()
}

fn error_of(result: Result<RatchetDecision>) -> String {
    result.expect_err("the ratchet refuses").to_string()
}

#[test]
fn promotion_refuses_dropped_certification_despite_unchanged_high_aggregates() {
    let mut current = full_snapshot(4);
    bump(&mut current, &["flatten_models"], 1);
    current["certified_strict_high_models"] = json!(["High0", "High1"]);
    let decision = decide(&current, &full_snapshot(4));
    assert!(!decision.promote);
    assert!(
        decision
            .reason
            .unwrap()
            .contains("baseline-certified strict-high model High2")
    );
}

#[test]
fn promotion_refuses_equal_count_certification_swaps_into_timing_holdbacks() {
    let mut baseline = full_snapshot(4);
    baseline["timing_margin_models"] = json!({ "Slow": { "phase": "Sim", "budget_share": 0.6 } });
    let mut current = baseline.clone();
    bump(&mut current, &["flatten_models"], 1);
    current["certified_strict_high_models"][0] = json!("Slow");
    current["timing_margin_models"] = json!({ "High0": { "phase": "Sim", "budget_share": 0.6 } });
    let decision = decide(&current, &baseline);
    assert!(!decision.promote);
    assert!(
        decision
            .reason
            .unwrap()
            .contains("baseline-certified strict-high model High0")
    );
}

#[test]
fn reviewed_context_and_schema_transitions_cannot_waive_certification_loss() {
    let old_omc = "OpenModelica 1.26.1";
    let checked_in = approved_omc_migration(old_omc, "OpenModelica 1.27.0");
    let mut baseline = full_snapshot(4);
    baseline["omc_version"] = json!(old_omc);
    let mut current = full_snapshot(4);
    current["certified_strict_high_models"] = json!(["High0", "High1"]);
    assert!(
        !ratchet_decision(&current, &baseline, &checked_in)
            .unwrap()
            .promote
    );
    assert!(!decide(&current, &full_snapshot(2)).promote);
}

#[test]
fn promotion_allows_preserved_certification_and_rejects_absent_or_malformed_rosters() {
    let baseline = full_snapshot(4);
    let mut current = full_snapshot(4);
    bump(&mut current, &["flatten_models"], 1);
    current["certified_strict_high_models"] = json!(["High0", "High1", "High2", "NewHigh"]);
    assert!(decide(&current, &baseline).promote);
    for bad in [
        None,
        Some(Value::Null),
        Some(json!("High0")),
        Some(json!([""])),
        Some(json!(["High0", "High0"])),
    ] {
        for current_side in [true, false] {
            let (mut candidate, mut previous) = (current.clone(), baseline.clone());
            let target = if current_side {
                &mut candidate
            } else {
                &mut previous
            };
            match &bad {
                Some(value) => target["certified_strict_high_models"] = value.clone(),
                None => {
                    target
                        .as_object_mut()
                        .unwrap()
                        .remove("certified_strict_high_models");
                }
            }
            let error = error_of(ratchet_decision(&candidate, &previous, &full_snapshot(4)));
            assert!(error.contains("certified_strict_high_models"), "{error}");
        }
    }
}

#[test]
fn promotable_snapshot_accepts_full_non_partial_artifacts_only() {
    let snapshot = full_snapshot(4);
    ensure_promotable_snapshot(&snapshot, 4, "source snapshot").unwrap();
    let mut not_partial = snapshot.clone();
    not_partial["partial"] = json!(false);
    ensure_promotable_snapshot(&not_partial, 4, "source snapshot").unwrap();

    let mut partial = snapshot.clone();
    partial["partial"] = json!(true);
    let mut scoped = snapshot.clone();
    scoped["run_scope"] = json!("partial");
    for (snapshot, version, message) in [
        (&partial, 4, "partial snapshots cannot be promoted"),
        (&scoped, 4, "only full MSL quality snapshots"),
        (
            &full_snapshot(4),
            5,
            "differs from the reviewed checked-in baseline",
        ),
    ] {
        let error = ensure_promotable_snapshot(snapshot, version, "source snapshot")
            .unwrap_err()
            .to_string();
        assert!(error.contains(message), "{error}");
    }
}

#[test]
fn ratchet_requires_the_reviewed_checked_in_baseline() {
    let error = error_of(ratchet_decision(
        &full_snapshot(4),
        &full_snapshot(4),
        &Value::Null,
    ));
    assert!(error.contains("reviewed checked-in baseline is required"));
}

#[test]
fn ratchet_promotes_non_regressing_improvements_and_skips_equivalent_ones() {
    let mut current = full_snapshot(4);
    current["solve_models"] = json!(8);
    current["trace_accuracy_stats"]["agreement_high"] = json!(4);
    current["trace_accuracy_stats"]["agreement_deviation"] = json!(0);
    current["trace_accuracy_stats"]["bad_channels_total"] = json!(8);
    current["trace_accuracy_stats"]["violation_mass_total"] = json!(2.5);
    let decision = decide(&current, &full_snapshot(4));
    assert!(decision.promote);
    let improvements = decision.improvements.join("\n");
    assert!(improvements.contains("solve models: 7 -> 8"));
    assert!(improvements.contains("high trace agreement"));
    assert!(improvements.contains("trace violation mass: 3.500000e+0 -> 2.500000e+0"));

    let equivalent = decide(&full_snapshot(4), &full_snapshot(4));
    assert!(!equivalent.promote);
    assert!(equivalent.reason.unwrap().contains("equivalent"));
}

#[test]
fn ratchet_skips_when_any_ratchet_metric_regresses() {
    let mut current = full_snapshot(4);
    current["sim_ok"] = json!(6);
    current["trace_accuracy_stats"]["bad_channels_total"] = json!(11);
    let decision = decide(&current, &full_snapshot(4));
    assert!(!decision.promote);
    assert!(
        decision
            .reason
            .unwrap()
            .contains("trace bad channels: 10 -> 11")
    );
}

#[test]
fn ratchet_rejects_a_changed_target_or_a_foreign_schema() {
    let mut current = full_snapshot(4);
    current["sim_target_models"] = json!(11);
    assert!(
        error_of(ratchet_decision(
            &current,
            &full_snapshot(4),
            &full_snapshot(4)
        ))
        .contains("sim_target_models changed from 10 to 11")
    );
    assert!(
        error_of(ratchet_decision(
            &full_snapshot(4),
            &full_snapshot(4),
            &full_snapshot(3)
        ))
        .contains("differs from the reviewed checked-in baseline")
    );
}

#[test]
fn ratchet_crosses_the_reviewed_reference_boundaries_down_to_the_promoted_schema() {
    for version in [1, 2, 3] {
        let decision = decide(&full_snapshot(4), &full_snapshot(version));
        assert!(decision.promote, "from version {version}");
        let boundaries = decision
            .improvements
            .iter()
            .filter(|line| line.starts_with("reference boundary"))
            .cloned()
            .collect::<Vec<_>>();
        let expected = (version..4)
            .map(|from| {
                format!(
                    "reference boundary: {from} -> {} (reviewed-boundary-v{})",
                    from + 1,
                    from + 1
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(boundaries, expected);
        assert!(
            decision
                .improvements
                .join("\n")
                .contains("trace exceptions: reviewed boundary")
        );
    }
}

#[test]
fn ratchet_still_compares_every_metric_across_a_reference_boundary() {
    let lowerings: [fn(&mut Value); 3] = [
        |snapshot| bump(snapshot, &["compiled_models"], -1),
        |snapshot| bump(snapshot, &["trace_accuracy_stats", "agreement_high"], -1),
        |snapshot| {
            snapshot["runtime_ratio_stats"]["wall_ratio_both_success"]["median"] = json!(5.0);
        },
    ];
    for lower in lowerings {
        let mut current = full_snapshot(4);
        lower(&mut current);
        let decision = decide(&current, &full_snapshot(2));
        assert!(!decision.promote);
        assert!(decision.reason.unwrap().contains("regression"));
    }
}

#[test]
fn ratchet_refuses_a_promoted_schema_the_reviewed_chain_does_not_reach() {
    let mut legacy = full_snapshot(4);
    legacy["metric_schema_migration"] =
        json!({ "from_quality_gate_version": 2, "to_quality_gate_version": 3 });
    assert!(decide(&legacy, &full_snapshot(3)).promote);

    let unreached = full_snapshot(0);
    assert!(
        error_of(ratchet_decision(
            &full_snapshot(4),
            &unreached,
            &full_snapshot(4)
        ))
        .contains("reaches quality schema 0")
    );
    assert!(
        error_of(ratchet_decision(
            &full_snapshot(4),
            &full_snapshot(5),
            &full_snapshot(4)
        ))
        .contains("newer than current")
    );
}

#[test]
fn ratchet_refuses_a_forged_or_skipped_boundary_chain() {
    let mut forged_promoted = full_snapshot(2);
    forged_promoted["reference_boundary_migration"]["exclusions_sha256"] = json!("unreviewed");
    assert!(
        error_of(ratchet_decision(
            &full_snapshot(4),
            &forged_promoted,
            &full_snapshot(4)
        ))
        .contains("promoted reference boundary differs from the reviewed chain")
    );

    let mut forged_current = full_snapshot(4);
    forged_current["reference_boundary_migration"]["previous"]["change"] = json!("unreviewed");
    assert!(
        error_of(ratchet_decision(
            &forged_current,
            &full_snapshot(2),
            &full_snapshot(4)
        ))
        .contains("differs from the reviewed checked-in chain")
    );

    let mut skipping = full_snapshot(4);
    skipping["reference_boundary_migration"]["previous"] =
        boundary(1, 2, Some(boundary(1, 1, None)));
    assert!(
        error_of(ratchet_decision(&skipping, &full_snapshot(2), &skipping))
            .contains("does not continue the chain at 3")
    );
}

#[test]
fn a_crossed_boundary_admits_its_reviewed_roster_additions_only() {
    let mut checked_in = full_snapshot(4);
    checked_in["reference_boundary_migration"]["roster_additions"] =
        json!([{ "model_name": "Modelica.Reviewed.Addition" }]);
    let mut current = checked_in.clone();
    current["unexcepted_non_high_models"] =
        json!(["Modelica.Open.Defect", "Modelica.Reviewed.Addition"]);
    let baseline = full_snapshot(3);
    let decision = ratchet_decision(&current, &baseline, &checked_in).unwrap();
    assert!(decision.promote);
    assert!(
        decision
            .improvements
            .join("\n")
            .contains("reviewed addition Modelica.Reviewed.Addition")
    );

    current["unexcepted_non_high_models"]
        .as_array_mut()
        .unwrap()
        .push(json!("Modelica.Unreviewed"));
    let regressed = ratchet_decision(&current, &baseline, &checked_in).unwrap();
    assert!(!regressed.promote);
    assert!(
        regressed
            .reason
            .unwrap()
            .contains("roster gained Modelica.Unreviewed")
    );
}

#[test]
fn a_reference_boundary_cannot_also_change_the_omc_context() {
    let mut current = full_snapshot(4);
    current["omc_version"] = json!("OpenModelica new");
    let mut baseline = full_snapshot(3);
    baseline["omc_version"] = json!("OpenModelica old");
    let checked_in = approved_omc_migration("OpenModelica old", "OpenModelica new");
    assert!(
        error_of(ratchet_decision(&current, &baseline, &checked_in))
            .contains("cannot change the OMC context")
    );
}

#[test]
fn omc_migration_compares_independent_metrics_without_cross_context_trace_rejection() {
    let mut baseline = full_snapshot(4);
    baseline["omc_version"] = json!("OpenModelica old");
    let mut current = full_snapshot(4);
    current["omc_version"] = json!("OpenModelica new");
    current["trace_accuracy_stats"]["agreement_high"] = json!(0);
    current["runtime_ratio_stats"]["system_ratio_both_success"]["median"] = json!(0.01);
    let checked_in = approved_omc_migration("OpenModelica old", "OpenModelica new");

    let decision = ratchet_decision(&current, &baseline, &checked_in).unwrap();
    assert!(decision.promote);
    assert!(decision.improvements.join("\n").contains("OMC context"));

    bump(&mut current, &["compiled_models"], -1);
    let regressed = ratchet_decision(&current, &baseline, &checked_in).unwrap();
    assert!(!regressed.promote);
    assert!(regressed.reason.unwrap().contains("compiled models"));
}

#[test]
fn omc_migration_rejects_missing_or_mismatched_checked_in_approval() {
    let mut baseline = full_snapshot(4);
    baseline["omc_version"] = json!("OpenModelica old");
    let mut current = full_snapshot(4);
    current["omc_version"] = json!("OpenModelica new");

    let mut unapproved = full_snapshot(4);
    unapproved["omc_version"] = json!("OpenModelica new");
    assert!(
        error_of(ratchet_decision(&current, &baseline, &unapproved))
            .contains("reviewed checked-in migration")
    );
    let reversed = approved_omc_migration("OpenModelica new", "OpenModelica old");
    assert!(
        error_of(ratchet_decision(&current, &baseline, &reversed))
            .contains("does not match current snapshot")
    );
}

#[test]
fn ratchet_rejects_a_runtime_speedup_drop_beyond_35_percent() {
    let mut current = full_snapshot(4);
    current["sim_ok"] = json!(6);
    current["runtime_ratio_stats"]["system_ratio_both_success"]["median"] = json!(1.29);
    let decision = decide(&current, &full_snapshot(4));
    assert!(!decision.promote);
    assert_eq!(
        decision.regressions,
        vec!["runtime system speedup median: 2.000000e+0 -> 1.290000e+0".to_string()]
    );
}

/// A scratch directory holding the three snapshot files, removed on drop.
struct Fixtures {
    dir: PathBuf,
}

impl Fixtures {
    fn new(name: &str, baseline: &Value, source: &Value, checked_in: &Value) -> Self {
        let dir = std::env::temp_dir().join(format!("msl-ratchet-{name}-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        for (file, value) in [
            ("baseline.json", baseline),
            ("source.json", source),
            ("checked-in.json", checked_in),
        ] {
            fs::write(dir.join(file), serde_json::to_string_pretty(value).unwrap()).unwrap();
        }
        Self { dir }
    }

    fn promote(&self) -> RatchetDecision {
        promote_baseline_if_improved(
            &self.dir.join("source.json"),
            &self.dir.join("baseline.json"),
            &self.dir.join("checked-in.json"),
            &mut |_| {},
        )
        .unwrap()
    }

    fn baseline_text(&self) -> String {
        fs::read_to_string(self.dir.join("baseline.json")).unwrap()
    }
}

impl Drop for Fixtures {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

#[test]
fn promotion_writes_the_source_bytes_only_when_improved() {
    let mut source = full_snapshot(4);
    source["sim_ok"] = json!(6);
    let improved = Fixtures::new("improved", &full_snapshot(4), &source, &full_snapshot(4));
    assert!(improved.promote().promote);
    assert_eq!(
        improved.baseline_text(),
        serde_json::to_string_pretty(&source).unwrap()
    );

    let crossing = Fixtures::new(
        "crossing",
        &full_snapshot(2),
        &full_snapshot(4),
        &full_snapshot(4),
    );
    assert!(crossing.promote().promote);
    let promoted: Value = serde_json::from_str(&crossing.baseline_text()).unwrap();
    assert_eq!(promoted["quality_gate_version"], json!(4));

    let mut baseline = full_snapshot(4);
    baseline["omc_version"] = json!("OpenModelica old");
    let mut source = full_snapshot(4);
    source["omc_version"] = json!("OpenModelica new");
    let migrated = Fixtures::new(
        "omc",
        &baseline,
        &source,
        &approved_omc_migration("OpenModelica old", "OpenModelica new"),
    );
    assert!(migrated.promote().promote);
    let promoted: Value = serde_json::from_str(&migrated.baseline_text()).unwrap();
    assert_eq!(promoted["omc_version"], json!("OpenModelica new"));

    let equivalent = Fixtures::new(
        "equivalent",
        &full_snapshot(4),
        &full_snapshot(4),
        &full_snapshot(4),
    );
    let before = equivalent.baseline_text();
    assert!(!equivalent.promote().promote);
    assert_eq!(equivalent.baseline_text(), before);
}

/// The tracked baseline is the reviewed schema of this commit: a run equal to it
/// must ratchet over a promoted asset at every version its boundary chain
/// reaches, so a new boundary cannot land while the ratchet refuses it.
#[test]
fn the_tracked_baseline_ratchets_over_every_promoted_schema_its_chain_reaches() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(super::super::MSL_QUALITY_BASELINE_FALLBACK_REL);
    let checked_in: Value = serde_json::from_str(&fs::read_to_string(path).unwrap()).unwrap();
    let current = checked_in.clone();
    assert!(
        !ratchet_decision(&current, &checked_in, &checked_in)
            .unwrap()
            .promote
    );
    let mut link = checked_in.get("reference_boundary_migration");
    let mut reached = 0;
    while let Some(previous) = link.and_then(|link| link.get("previous")) {
        let link_value = link.unwrap();
        let mut promoted = checked_in.clone();
        promoted["quality_gate_version"] = link_value["from_quality_gate_version"].clone();
        promoted["reference_boundary_migration"] = previous.clone();
        promoted["trace_exceptions_sha256"] = previous["exclusions_sha256"].clone();
        let decision = ratchet_decision(&current, &promoted, &checked_in).unwrap();
        assert!(
            decision.promote,
            "from version {}",
            promoted["quality_gate_version"]
        );
        let crossing = format!(
            "reference boundary: {} -> {}",
            link_value["from_quality_gate_version"], link_value["to_quality_gate_version"]
        );
        assert!(decision.improvements.join("\n").contains(&crossing));
        reached += 1;
        link = Some(previous);
    }
    assert!(
        reached > 0,
        "the tracked schema declares no crossable boundary"
    );
}

/// A snapshot from per-model evidence, with the totals the evidence sums to.
fn evidence_snapshot(evidence: &serde_json::Map<String, Value>) -> Value {
    let models = evidence.values().collect::<Vec<_>>();
    let sum = |pick: &dyn Fn(&Value) -> i64| models.iter().map(|entry| pick(entry)).sum::<i64>();
    let count = |key: &str| sum(&|entry| entry[key].as_i64().unwrap());
    let state = |key: &str| sum(&|entry| entry["state_set"][key].as_i64().unwrap());
    let exact = sum(&|entry| i64::from(entry["state_set"]["exact"].as_bool().unwrap()));
    let mass = models
        .iter()
        .map(|entry| entry["ic_violation_mass"].as_f64().unwrap())
        .sum::<f64>();
    let roster = evidence.keys().cloned().collect::<Vec<_>>();
    let mut snapshot = full_snapshot(4);
    snapshot["certified_strict_high_models"] = json!(roster);
    snapshot["trace_model_evidence"] = Value::Object(evidence.clone());
    let stats = &mut snapshot["trace_accuracy_stats"];
    stats["models_compared"] = json!(roster.len());
    stats["agreement_high"] = json!(roster.len());
    stats["agreement_minor"] = json!(0);
    stats["agreement_deviation"] = json!(0);
    stats["models_with_severe_channel"] = json!(0);
    stats["initial_condition"] = json!({
        "models_compared": roster.len(),
        "deviation_channels_total": count("ic_deviation_channels"),
        "severe_channels_total": count("ic_severe_channels"),
        "violation_mass_total": mass,
    });
    stats["state_selection"] = json!({
        "models_compared": roster.len(),
        "exact_state_set_match_models": exact,
        "total_rumoca_only_states": state("rumoca_only"),
        "total_omc_only_states": state("omc_only"),
    });
    snapshot
}

#[test]
fn real_state_set_evidence_refuses_renamed_states_on_certified_models_only() {
    let sets: Value =
        serde_json::from_str(include_str!("../testdata/real-state-sets.json")).unwrap();
    let side = |key: &str| sets[key].as_object().unwrap().clone();
    let reference = evidence_snapshot(&side("reference"));
    let mut candidate = evidence_snapshot(&side("candidate"));
    bump(&mut candidate, &["sim_ok"], 2);
    let decision = decide(&candidate, &reference);
    assert!(!decision.promote);
    assert_eq!(
        decision.regressions,
        vec![
            "state-set rumoca-only states over 10 shared models: 27 -> 53".to_string(),
            "state-set omc-only states over 10 shared models: 27 -> 53".to_string(),
        ]
    );
    // The same models as they were: the newly compared models alone, which the
    // raw totals counted as a regression, promote.
    let mut merged = side("candidate");
    merged.extend(side("reference"));
    let mut unchanged = evidence_snapshot(&merged);
    bump(&mut unchanged, &["sim_ok"], 2);
    let promoted = decide(&unchanged, &reference);
    assert!(promoted.promote, "{:?}", promoted.reason);
    assert!(
        unchanged["trace_accuracy_stats"]["state_selection"]["total_rumoca_only_states"]
            .as_i64()
            .unwrap()
            > reference["trace_accuracy_stats"]["state_selection"]["total_rumoca_only_states"]
                .as_i64()
                .unwrap()
    );
}

#[test]
fn the_run_cohort_not_the_whole_run_carries_the_runtime_medians() {
    let mut reference = full_snapshot(4);
    reference["runtime_ratio_cohort_models"] = json!(["A", "B"]);
    let mut current = full_snapshot(4);
    current["sim_ok"] = json!(6);
    current["runtime_ratio_cohort_models"] = json!(["A", "B", "C"]);
    current["runtime_model_ratios"] = json!({
        "A": { "system": 2.0, "wall": 10.0 },
        "B": { "system": 2.0, "wall": 10.0 },
        "C": { "system": 0.1, "wall": 0.5 },
    });
    assert!(decide(&current, &reference).promote);

    current["runtime_model_ratios"]["A"]["wall"] = json!(1.0);
    current["runtime_model_ratios"]["B"]["wall"] = json!(1.0);
    current["runtime_ratio_stats"]["wall_ratio_both_success"]["median"] = json!(1.0);
    let regressed = decide(&current, &reference);
    assert!(!regressed.promote);
    assert!(
        regressed
            .reason
            .unwrap()
            .contains("runtime wall speedup median over 2 reference cohort models")
    );
}

mod per_model_rule {
    use super::*;
    use rumoca_msl_quality::{
        BandLabel, ModelStage, ModelState, StageSeconds, TraceState, UnscoredCause,
    };
    use sha2::{Digest, Sha256};

    fn state(stage: ModelStage, trace: TraceState, error: Option<f64>) -> Value {
        serde_json::to_value(ModelState {
            stage,
            outcome: None,
            trace,
            compared_channels: 4,
            trace_violation_mass: error.map(|_| 0.0),
            mean_channel_bounded_l1: error,
            certified: false,
            roster: false,
            stage_seconds: StageSeconds::default(),
        })
        .unwrap()
    }

    fn simulated(error: f64) -> Value {
        state(
            ModelStage::Simulated,
            TraceState::Scored(BandLabel::Near),
            Some(error),
        )
    }

    fn with_states(mut snapshot: Value, states: Value) -> Value {
        snapshot["model_states"] = states;
        snapshot
    }

    #[test]
    fn a_model_worse_than_its_own_state_refuses_and_progress_promotes() {
        let baseline = with_states(
            full_snapshot(4),
            json!({ "A": simulated(1.0e-3), "B": state(ModelStage::Compiled, TraceState::Unscored(UnscoredCause::NotSimulated), None) }),
        );
        let dropped = with_states(
            full_snapshot(4),
            json!({ "A": state(ModelStage::Compiled, TraceState::Unscored(UnscoredCause::NotSimulated), None), "B": state(ModelStage::Compiled, TraceState::Unscored(UnscoredCause::NotSimulated), None) }),
        );
        let decision = decide(&dropped, &baseline);
        assert!(!decision.promote);
        assert!(
            decision
                .regressions
                .iter()
                .any(|line| line.starts_with("A: stage dropped")),
            "{:?}",
            decision.regressions
        );

        // B newly simulates: progress, and the aggregate count floors no
        // longer apply, so it promotes on its own.
        let progressed = with_states(
            full_snapshot(4),
            json!({ "A": simulated(1.0e-3), "B": simulated(2.0e-3) }),
        );
        let decision = decide(&progressed, &baseline);
        assert!(decision.promote, "{:?}", decision.reason);
        assert!(decision.improvements.contains(&"B: progressed".to_string()));
    }

    #[test]
    fn a_watchdog_kill_holds_promotion_until_it_is_re_measured() {
        let baseline = with_states(full_snapshot(4), json!({ "A": simulated(1.0e-3) }));
        let mut killed = state(
            ModelStage::Initialized,
            TraceState::Unscored(UnscoredCause::NotSimulated),
            None,
        );
        killed["stage_seconds"]["timeout_phase"] = json!("Sim");
        let current = with_states(full_snapshot(4), json!({ "A": killed }));
        let decision = decide(&current, &baseline);
        assert!(!decision.promote);
        assert!(decision.reason.unwrap().starts_with("re-measure first"));
    }

    #[test]
    fn an_accuracy_rise_must_be_stated_exactly_by_a_crossed_boundary() {
        let before = full_snapshot(3);
        let mut before = with_states(before, json!({ "A": simulated(1.0e-3) }));
        before["reference_boundary_migration"] = boundary_chain(3);
        let rose = json!({ "rose": [{ "model": "A", "before": 1.0e-3, "after": 2.0e-3 }] });
        let current_with = |statement: Option<Value>| {
            let mut current = with_states(full_snapshot(4), json!({ "A": simulated(2.0e-3) }));
            if let Some(statement) = statement {
                current["reference_boundary_migration"]["accuracy_change"] = statement;
            }
            current
        };
        let checked_in_for = |current: &Value| {
            let mut checked_in = full_snapshot(4);
            checked_in["reference_boundary_migration"] =
                current["reference_boundary_migration"].clone();
            checked_in
        };
        let judge =
            |current: &Value| ratchet_decision(current, &before, &checked_in_for(current)).unwrap();
        let unstated = judge(&current_with(None));
        assert!(!unstated.promote);
        assert!(unstated.reason.unwrap().contains("mean channel error rose"));
        assert!(judge(&current_with(Some(rose.clone()))).promote);
        let mut stale = rose;
        stale["rose"][0]["after"] = json!(1.9e-3);
        assert!(!judge(&current_with(Some(stale))).promote);
    }

    #[test]
    fn the_first_crossing_reads_the_evidence_its_boundary_pins() {
        let dir = std::env::temp_dir().join(format!("msl-ratchet-evidence-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        let evidence = json!({
            "schema_version": 1,
            "source_run": "ci-1",
            "source_commit": "abc",
            "caveats": [],
            "model_states": { "A": simulated(1.0e-3) },
        })
        .to_string();
        fs::write(dir.join("evidence.json"), &evidence).unwrap();
        let sha = format!("{:x}", Sha256::digest(evidence.as_bytes()));
        let link = |sha: &str| json!({ "evidence_model_states": { "file": "evidence.json", "sha256": sha } });
        let baseline = full_snapshot(3);
        let current = with_states(full_snapshot(4), json!({ "A": simulated(1.0e-3) }));
        let pinned = link(&sha);
        let found = super::super::per_model::judge_at(&current, &baseline, &[&pinned], false, &dir)
            .unwrap()
            .unwrap();
        assert!(found.regressions.is_empty(), "{:?}", found.regressions);
        let forged = link("0000");
        let error = super::super::per_model::judge_at(&current, &baseline, &[&forged], false, &dir)
            .unwrap_err()
            .to_string();
        assert!(error.contains("the reviewed boundary pins 0000"), "{error}");
        let none = super::super::per_model::judge_at(&current, &baseline, &[], false, &dir)
            .unwrap_err()
            .to_string();
        assert!(error_contains(
            &none,
            "no crossed reference boundary pins evidence"
        ));
        let _ = fs::remove_dir_all(&dir);
    }

    fn error_contains(error: &str, needle: &str) -> bool {
        error.contains(needle)
    }
}
