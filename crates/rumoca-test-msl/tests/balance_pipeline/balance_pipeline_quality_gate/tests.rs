use super::*;

mod gate_hole;
use serde_json::Value;
use serde_json::json;
use std::any::Any;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use tempfile::tempdir;

fn assert_distribution_parsed(input: Value, expected: MslDistributionStats) {
    let stats = parse_distribution_stats(&input).expect("expected distribution stats");
    assert_eq!(stats.sample_count, expected.sample_count);
    assert_eq!(stats.min, expected.min);
    assert_eq!(stats.median, expected.median);
    assert_eq!(stats.mean, expected.mean);
    assert_eq!(stats.max, expected.max);
}

#[test]
fn parse_distribution_stats_accepts_supported_field_sets() {
    let cases = vec![
        (
            json!({
                "sample_count": 3,
                "min": 1.0,
                "median": 2.0,
                "mean": 2.5,
                "max": 4.0
            }),
            MslDistributionStats {
                sample_count: 3,
                min: 1.0,
                median: 2.0,
                mean: 2.5,
                max: 4.0,
            },
        ),
        (
            json!({
                "sample_count": 4,
                "min_ratio": 0.5,
                "median_ratio": 1.2,
                "mean_ratio": 1.4,
                "max_ratio": 2.5
            }),
            MslDistributionStats {
                sample_count: 4,
                min: 0.5,
                median: 1.2,
                mean: 1.4,
                max: 2.5,
            },
        ),
    ];
    for (input, expected) in cases {
        assert_distribution_parsed(input, expected);
    }
}

fn dist(sample_count: usize, min: f64, median: f64, mean: f64, max: f64) -> MslDistributionStats {
    MslDistributionStats {
        sample_count,
        min,
        median,
        mean,
        max,
    }
}

fn runtime_ratio_stats(system_median: f64, wall_median: f64) -> MslRuntimeRatioStatsBaseline {
    MslRuntimeRatioStatsBaseline {
        system_ratio_both_success: MslDistributionStats {
            sample_count: 8,
            min: system_median * 0.5,
            median: system_median,
            mean: system_median * 1.1,
            max: system_median * 1.5,
        },
        wall_ratio_both_success: MslDistributionStats {
            sample_count: 8,
            min: wall_median * 0.5,
            median: wall_median,
            mean: wall_median * 1.1,
            max: wall_median * 1.5,
        },
    }
}

fn panic_message(payload: &Box<dyn Any + Send>) -> String {
    if let Some(message) = payload.downcast_ref::<String>() {
        return message.clone();
    }
    if let Some(message) = payload.downcast_ref::<&'static str>() {
        return (*message).to_string();
    }
    "<non-string panic payload>".to_string()
}

fn baseline_quality_template() -> MslQualityBaseline {
    MslQualityBaseline {
        quality_gate_version: MSL_QUALITY_GATE_VERSION,
        run_scope: MSL_QUALITY_RUN_SCOPE_FULL.to_string(),
        git_commit: "baseline".to_string(),
        msl_version: "v4.1.0".to_string(),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        sim_timeout_seconds: SIM_TIMEOUT_SECS,
        simulatable_attempted: 10,
        parse_models: 10,
        flatten_models: 10,
        dae_models: 10,
        compiled_models: 10,
        solve_models: 8,
        balanced_models: 10,
        unbalanced_models: 0,
        partial_models: 0,
        balance_denominator: 10,
        initial_balanced_models: 10,
        initial_unbalanced_models: 0,
        sim_target_models: 10,
        sim_attempted: 10,
        ic_attempted: 10,
        ic_ok: 8,
        ic_solver_fail: 2,
        sim_ok: 8,
        sim_success_rate: 0.8,
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_ratio_cohort_models: None,
        trace_accuracy_stats: None,
        tensor_preservation: MslTensorPreservationBaseline {
            models_reported: 0,
            family_bodies: 0,
            preserved_family_bodies: 0,
            scalarized_family_rows: 0,
            report_errors: 0,
            preservation_percent: None,
        },
        metric_schema_migration: None,
    }
}

fn gate_input_with_sim_rate(sim_ok: usize, sim_attempted: usize) -> MslQualityGateInput<'static> {
    MslQualityGateInput {
        msl_version: "v4.1.0",
        simulatable_attempted: 10,
        parse_models: 10,
        flatten_models: 10,
        dae_models: 10,
        compiled_models: 10,
        solve_models: sim_ok,
        balanced_models: 10,
        unbalanced_models: 0,
        partial_models: 0,
        balance_denominator: 10,
        initial_balanced_models: 10,
        initial_unbalanced_models: 0,
        sim_target_models: 10,
        sim_attempted,
        ic_attempted: sim_attempted,
        ic_ok: sim_ok,
        ic_solver_fail: sim_attempted.saturating_sub(sim_ok),
        sim_ok,
        tensor_models_reported: 0,
        tensor_family_bodies: 0,
        tensor_preserved_family_bodies: 0,
        tensor_scalarized_family_rows: 0,
        tensor_report_errors: 0,
    }
}

fn valid_summary_template() -> MslSummary {
    let mut summary = super::super::empty_summary(1, 0);
    summary.total_models = 1;
    summary
}

#[test]
fn selected_target_failures_report_non_sim_ok_models() {
    let mut summary = valid_summary_template();
    summary.sim_target_models = vec!["A".to_string(), "B".to_string()];
    let mut ok = phase_error_result("A".to_string(), "Success", None, None);
    ok.sim_status = Some("sim_ok".to_string());
    let mut fail = phase_error_result("B".to_string(), "Success", None, None);
    fail.sim_status = Some("sim_solver_fail".to_string());
    summary.model_results = vec![ok, fail];

    assert_eq!(
        selected_target_failures(&summary),
        vec!["B (sim_solver_fail)".to_string()]
    );
}

#[test]
fn selected_target_failures_report_missing_results_in_target_order() {
    let mut summary = valid_summary_template();
    summary.sim_target_models = vec!["A".to_string(), "B".to_string(), "C".to_string()];
    let mut ok = phase_error_result("A".to_string(), "Success", None, None);
    ok.sim_status = Some("sim_ok".to_string());
    summary.model_results = vec![ok];

    assert_eq!(
        selected_target_failures(&summary),
        vec![
            "B (missing-result)".to_string(),
            "C (missing-result)".to_string()
        ]
    );
}

#[test]
fn selected_target_gate_returns_error_instead_of_asserting() {
    let mut summary = valid_summary_template();
    summary.sim_target_models = vec!["A".to_string()];
    let mut fail = phase_error_result("A".to_string(), "Success", None, None);
    fail.sim_status = Some("sim_solver_fail".to_string());
    summary.model_results = vec![fail];

    let error = enforce_all_selected_targets_succeeded(&summary)
        .expect_err("focused selected-target failure should be returned");
    let message = error.to_string();
    assert!(message.contains("1 of 1 selected simulation target(s) did not succeed"));
    assert!(message.contains("A (sim_solver_fail)"));
}

#[test]
fn full_quality_gate_rejects_zero_simulation_attempts() {
    let message = zero_simulation_attempt_rejection(2, false)
        .expect("a cohort run that simulated nothing must be rejected");
    assert!(message.contains("invalid full run"));
    assert!(message.contains("0 simulations attempted for 2 selected simulation target(s)"));

    assert_eq!(
        zero_simulation_attempt_rejection(2, true),
        None,
        "a focused/partial run may legitimately attempt no simulations"
    );
}

#[test]
fn current_quality_snapshot_marks_only_partial_runs() {
    let summary = valid_summary_template();
    let full = current_msl_quality_snapshot_json(&summary, None, false)
        .expect("full snapshot should serialize");
    assert_eq!(
        full.get("quality_gate_version").and_then(Value::as_u64),
        Some(MSL_QUALITY_GATE_VERSION as u64)
    );
    assert_eq!(
        full.get("run_scope").and_then(Value::as_str),
        Some(MSL_QUALITY_RUN_SCOPE_FULL)
    );
    assert!(
        full.get("partial").is_none(),
        "full baseline snapshots should omit the partial marker"
    );

    let partial = current_msl_quality_snapshot_json(&summary, None, true)
        .expect("partial snapshot should serialize");
    assert_eq!(
        partial.get("run_scope").and_then(Value::as_str),
        Some(MSL_QUALITY_RUN_SCOPE_PARTIAL)
    );
    assert_eq!(partial.get("partial").and_then(Value::as_bool), Some(true));
}

#[test]
fn current_quality_snapshot_records_parity_omc_version() {
    let summary = valid_summary_template();
    let parity = MslParityGateInput {
        total_models: Some(1),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let snapshot = current_msl_quality_snapshot_json(&summary, Some(&parity), false)
        .expect("snapshot should serialize");
    assert_eq!(
        snapshot.get("omc_version").and_then(Value::as_str),
        Some("OpenModelica 1.26.1")
    );
}

#[test]
fn current_quality_snapshot_separates_reviewed_exceptions_from_unclassified_debt() {
    let mut summary = valid_summary_template();
    summary.sim_ok = 10;
    let mut trace = trace_accuracy_baseline();
    trace.policy_excluded_models = 1;
    trace.trace_nonidentifiable_models = 1;
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let snapshot = current_msl_quality_snapshot_json(&summary, Some(&parity), false)
        .expect("snapshot should serialize");
    assert_eq!(
        snapshot
            .pointer("/pipeline_progress/certified_simulations_strict_high")
            .and_then(Value::as_u64),
        Some(8)
    );
    assert_eq!(
        snapshot
            .pointer("/pipeline_progress/reviewed_trace_exceptions")
            .and_then(Value::as_u64),
        Some(2)
    );
    assert_eq!(
        snapshot
            .pointer("/pipeline_progress/unclassified_simulations")
            .and_then(Value::as_u64),
        Some(0)
    );
    assert_eq!(
        snapshot
            .pointer("/pipeline_progress/classified_simulations_percent")
            .and_then(Value::as_f64),
        Some(100.0)
    );
}

#[test]
fn current_quality_snapshot_records_runtime_ratio_stats() {
    let summary = valid_summary_template();
    let parity = MslParityGateInput {
        total_models: Some(1),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: Some(MslParityRuntimeContext {
            workers_used: Some(3),
            omc_threads: Some(1),
        }),
        runtime_ratio_stats: Some(runtime_ratio_stats(5.0, 4.0)),
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let snapshot = current_msl_quality_snapshot_json(&summary, Some(&parity), false)
        .expect("snapshot should serialize");
    assert_eq!(
        snapshot
            .pointer("/runtime_context/workers_used")
            .and_then(Value::as_u64),
        Some(3)
    );
    assert_eq!(
        snapshot
            .pointer("/runtime_ratio_stats/system_ratio_both_success/median")
            .and_then(Value::as_f64),
        Some(5.0)
    );
    assert_eq!(
        snapshot
            .pointer("/runtime_ratio_stats/wall_ratio_both_success/median")
            .and_then(Value::as_f64),
        Some(4.0)
    );
}

#[test]
fn quality_context_reports_omc_version_mismatch_for_pinned_baseline() {
    let gate_input = gate_input_with_sim_rate(8, 10);
    let baseline = baseline_quality_template();
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.27.0".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let reason = msl_quality_context_mismatch_reason(gate_input, &baseline, Some(&parity))
        .expect("pinned OMC version mismatch should be a context mismatch");
    assert!(
        reason.contains("omc_version differs"),
        "unexpected mismatch reason: {reason}"
    );
}

#[test]
fn quality_context_accepts_omc_package_rebuild_suffix_drift() {
    let gate_input = gate_input_with_sim_rate(8, 10);
    let mut baseline = baseline_quality_template();
    baseline.omc_version = Some("OpenModelica 1.26.7~1-g2b913cc".to_string());
    baseline.trace_accuracy_stats = Some(trace_accuracy_baseline());
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.7~2-ge74480f".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    assert_eq!(
        msl_quality_context_mismatch_reason(gate_input, &baseline, Some(&parity)),
        None
    );
}

#[test]
fn current_quality_snapshot_includes_pipeline_progression() {
    let mut summary = valid_summary_template();
    summary.total_models = 12;
    summary.compiled_models = 10;
    summary.balanced_models = 9;
    summary.initial_balanced_models = 8;
    summary.sim_target_models = vec!["A".to_string(), "B".to_string()];
    summary.sim_attempted = 2;
    summary.ic_attempted = 2;
    summary.ic_ok = 1;
    summary.ic_solver_fail = 1;
    summary.sim_ok = 1;
    summary
        .error_code_counts
        .insert("unsupported-feature:events".to_string(), 2);
    summary
        .unsupported_feature_counts
        .insert("events".to_string(), 2);
    summary
        .unsupported_feature_counts_by_backend
        .entry("c-solve".to_string())
        .or_default()
        .insert("events".to_string(), 2);
    summary.failures_by_phase.insert(
        "Flatten".to_string(),
        vec![
            "Modelica.Bad.One".to_string(),
            "Modelica.Bad.Two".to_string(),
        ],
    );
    summary.failures_by_phase.insert(
        "NeedsInner".to_string(),
        vec!["Modelica.NotAStandaloneRoot".to_string()],
    );

    let snapshot = current_msl_quality_snapshot_json(&summary, None, false)
        .expect("snapshot should serialize");
    let pipeline = snapshot
        .get("pipeline_progress")
        .and_then(Value::as_object)
        .expect("snapshot should include pipeline progress");
    assert_eq!(
        pipeline.get("compiled_models").and_then(Value::as_u64),
        Some(10)
    );
    assert_eq!(
        pipeline.get("balanced_models").and_then(Value::as_u64),
        Some(9)
    );
    assert_eq!(
        pipeline
            .get("initial_balanced_models")
            .and_then(Value::as_u64),
        Some(8)
    );
    assert_eq!(pipeline.get("ic_ok").and_then(Value::as_u64), Some(1));
    assert_eq!(pipeline.get("sim_ok").and_then(Value::as_u64), Some(1));
    assert_eq!(
        pipeline
            .get("error_code_counts")
            .and_then(|value| value.get("unsupported-feature:events"))
            .and_then(Value::as_u64),
        Some(2)
    );
    assert_eq!(
        pipeline
            .get("unsupported_feature_counts")
            .and_then(|value| value.get("events"))
            .and_then(Value::as_u64),
        Some(2)
    );
    assert_eq!(
        pipeline
            .get("unsupported_feature_counts_by_backend")
            .and_then(|value| value.get("c-solve"))
            .and_then(|value| value.get("events"))
            .and_then(Value::as_u64),
        Some(2)
    );
    assert_eq!(
        pipeline
            .get("phase_failure_counts")
            .and_then(|value| value.get("Flatten"))
            .and_then(Value::as_u64),
        Some(2)
    );
    assert!(
        pipeline
            .get("phase_failure_counts")
            .and_then(|value| value.get("NeedsInner"))
            .is_none(),
        "NeedsInner is not a blocking phase failure"
    );
    assert_eq!(
        pipeline
            .get("omc_assertion_failure_models")
            .and_then(Value::as_u64),
        Some(0)
    );
}

#[test]
fn current_quality_snapshot_includes_mls_contract_category_coverage() {
    let mut summary = valid_summary_template();
    let mut array_result = phase_error_result(
        "Modelica.Blocks.Examples.MatrixGain".to_string(),
        "Success",
        None,
        None,
    );
    array_result.is_balanced = Some(true);
    array_result.ir_solve_file = Some("MatrixGain.solve.json".to_string());
    array_result.sim_status = Some("sim_ok".to_string());
    let connector_result = phase_error_result(
        "Modelica.Blocks.Examples.BusUsage".to_string(),
        "Flatten",
        Some("connect equation failed".to_string()),
        Some("ECONN001".to_string()),
    );
    summary.model_results = vec![array_result, connector_result];

    let snapshot = current_msl_quality_snapshot_json(&summary, None, false)
        .expect("snapshot should serialize");
    let coverage = snapshot
        .get("mls_contract_coverage")
        .and_then(Value::as_object)
        .expect("snapshot should include MLS category coverage");
    assert_eq!(
        coverage
            .get("ARR")
            .and_then(|category| category.get("sim_ok"))
            .and_then(Value::as_u64),
        Some(1)
    );
    assert_eq!(
        coverage
            .get("CONN_STRM")
            .and_then(|category| category.pointer("/error_code_counts/ECONN001"))
            .and_then(Value::as_u64),
        Some(1)
    );
}

#[test]
fn sim_completion_report_is_quiet_on_equal_cumulative_count() {
    let baseline = MslQualityBaseline {
        sim_ok: 800,
        sim_attempted: 1000,
        sim_target_models: 1000,
        sim_success_rate: 0.8,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(800, 1000);
    gate_input.sim_target_models = 1000;

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "equal cumulative simulation count should pass, got: {reasons:?}"
    );
}

#[test]
fn sim_completion_report_is_quiet_on_one_model_full_run_jitter() {
    let baseline = MslQualityBaseline {
        sim_ok: 800,
        sim_attempted: 1000,
        sim_target_models: 1000,
        sim_success_rate: 0.8,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(799, 1000);
    gate_input.sim_target_models = 1000;

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "one-model full-run simulation jitter should pass, got: {reasons:?}"
    );
}

#[test]
fn sim_completion_report_notes_a_two_model_full_run_drop() {
    let baseline = MslQualityBaseline {
        sim_ok: 800,
        sim_attempted: 1000,
        sim_target_models: 1000,
        sim_success_rate: 0.8,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(798, 1000);
    gate_input.sim_target_models = 1000;

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Sim pass count regressed")),
        "expected simulation-stage regression reason, got: {reasons:?}"
    );
}

#[test]
fn ic_completion_report_is_quiet_on_one_model_full_run_jitter() {
    let baseline = MslQualityBaseline {
        sim_target_models: 1000,
        ic_ok: 800,
        sim_ok: 700,
        sim_success_rate: 0.7,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(700, 1000);
    gate_input.sim_target_models = 1000;
    gate_input.ic_ok = 799;

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "one-model full-run IC jitter should pass, got: {reasons:?}"
    );
}

#[test]
fn ic_completion_report_is_quiet_when_sim_count_is_stable() {
    let baseline = MslQualityBaseline {
        sim_target_models: 1000,
        ic_ok: 800,
        sim_ok: 700,
        sim_success_rate: 0.7,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(700, 1000);
    gate_input.sim_target_models = 1000;
    gate_input.ic_ok = 798;

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "IC-only progress drop should not fail a stable simulation gate, got: {reasons:?}"
    );
}

#[test]
fn ic_completion_note_appears_when_sim_count_also_drops() {
    let baseline = MslQualityBaseline {
        sim_target_models: 1000,
        ic_ok: 800,
        sim_ok: 700,
        sim_success_rate: 0.7,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(698, 1000);
    gate_input.sim_target_models = 1000;
    gate_input.ic_ok = 798;

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("IC pass count regressed")),
        "expected IC-stage context with simulation regression, got: {reasons:?}"
    );
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Sim pass count regressed")),
        "expected simulation-stage regression reason, got: {reasons:?}"
    );
}

#[test]
fn current_sharded_ic_accounting_shape_keeps_the_completion_report_quiet() {
    let baseline = MslQualityBaseline {
        sim_target_models: 566,
        ic_ok: 239,
        sim_ok: 170,
        sim_success_rate: 170.0 / 566.0,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        sim_target_models: 566,
        ic_ok: 227,
        sim_ok: 170,
        ..gate_input_with_sim_rate(170, 413)
    };

    let reasons = sim_completion_report_notes(gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "current CI run preserves simulation successes and should pass, got: {reasons:?}"
    );
}

#[test]
fn cumulative_stage_gate_allows_early_stage_improvement() {
    let baseline = MslQualityBaseline {
        simulatable_attempted: 10,
        parse_models: 10,
        flatten_models: 8,
        dae_models: 6,
        compiled_models: 6,
        solve_models: 4,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        simulatable_attempted: 10,
        parse_models: 10,
        flatten_models: 9,
        dae_models: 6,
        compiled_models: 6,
        solve_models: 4,
        ..gate_input_with_sim_rate(8, 10)
    };

    let mut reasons = Vec::new();
    push_compile_balance_regression_reasons(&mut reasons, gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "early-stage improvements with unchanged later stages should pass, got: {reasons:?}"
    );
}

#[test]
fn tensor_preservation_gate_rejects_family_body_scalarization() {
    let baseline = MslQualityBaseline {
        tensor_preservation: MslTensorPreservationBaseline {
            models_reported: 10,
            family_bodies: 100,
            preserved_family_bodies: 80,
            scalarized_family_rows: 20,
            report_errors: 0,
            preservation_percent: Some(80.0),
        },
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        tensor_models_reported: 10,
        tensor_family_bodies: 100,
        tensor_preserved_family_bodies: 79,
        tensor_scalarized_family_rows: 21,
        ..gate_input_with_sim_rate(8, 10)
    };

    let mut reasons = Vec::new();
    push_tensor_preservation_regression_reasons(&mut reasons, gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("tensor preservation regressed")),
        "one native family body becoming scalar must fail the KPI gate: {reasons:?}"
    );
}

#[test]
fn cumulative_stage_gate_rejects_stage_count_drop() {
    let baseline = MslQualityBaseline {
        simulatable_attempted: 10,
        parse_models: 10,
        flatten_models: 8,
        dae_models: 6,
        compiled_models: 6,
        solve_models: 4,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        simulatable_attempted: 10,
        parse_models: 10,
        flatten_models: 7,
        dae_models: 6,
        compiled_models: 6,
        solve_models: 4,
        ..gate_input_with_sim_rate(8, 10)
    };

    let mut reasons = Vec::new();
    push_compile_balance_regression_reasons(&mut reasons, gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Flatten pass count regressed")),
        "expected flatten-stage regression reason, got: {reasons:?}"
    );
}

#[test]
fn cumulative_stage_gate_rejects_balanced_count_drop() {
    let baseline = MslQualityBaseline {
        simulatable_attempted: 10,
        compiled_models: 6,
        balanced_models: 6,
        balance_denominator: 6,
        initial_balanced_models: 6,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        simulatable_attempted: 10,
        compiled_models: 6,
        balanced_models: 5,
        balance_denominator: 5,
        initial_balanced_models: 5,
        ..gate_input_with_sim_rate(8, 10)
    };

    let mut reasons = Vec::new();
    push_compile_balance_regression_reasons(&mut reasons, gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Balanced pass count regressed")),
        "expected balanced-count regression reason, got: {reasons:?}"
    );
}

#[test]
fn full_run_stage_gate_allows_one_model_solve_and_ic_jitter() {
    let baseline = MslQualityBaseline {
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 560,
        dae_models: 487,
        compiled_models: 487,
        solve_models: 390,
        balanced_models: 390,
        balance_denominator: 487,
        initial_balanced_models: 230,
        sim_target_models: 566,
        ic_ok: 230,
        sim_ok: 156,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 560,
        dae_models: 487,
        compiled_models: 487,
        solve_models: 389,
        balanced_models: 390,
        balance_denominator: 487,
        initial_balanced_models: 230,
        sim_target_models: 566,
        ic_ok: 229,
        sim_ok: 156,
        ..gate_input_with_sim_rate(156, 566)
    };

    let reasons = msl_quality_regression_reasons(gate_input, &baseline, None);
    assert!(
        reasons.is_empty(),
        "observed one-model CI jitter should pass, got: {reasons:?}"
    );
}

#[test]
fn full_run_stage_gate_allows_one_timed_out_compile() {
    let baseline = MslQualityBaseline {
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 545,
        dae_models: 545,
        compiled_models: 545,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 544,
        dae_models: 544,
        compiled_models: 544,
        ..gate_input_with_sim_rate(8, 10)
    };

    let mut reasons = Vec::new();
    push_compile_balance_regression_reasons(&mut reasons, gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "one full-library timeout is allowed host jitter, got: {reasons:?}"
    );
}

#[test]
fn full_run_stage_gate_rejects_two_lost_compiles() {
    let baseline = MslQualityBaseline {
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 545,
        dae_models: 545,
        compiled_models: 545,
        ..baseline_quality_template()
    };
    let gate_input = MslQualityGateInput {
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 543,
        dae_models: 543,
        compiled_models: 543,
        ..gate_input_with_sim_rate(8, 10)
    };

    let mut reasons = Vec::new();
    push_compile_balance_regression_reasons(&mut reasons, gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Compile pass count regressed")),
        "two lost compiles must fail the full-library gate, got: {reasons:?}"
    );
}

#[test]
fn measurability_check_rejects_zero_total_models() {
    let summary = super::super::empty_summary(1, 0);
    let panic = std::panic::catch_unwind(|| assert_msl_run_is_measurable(&summary))
        .expect_err("zero-model summary must panic");
    let message = panic_message(&panic);
    assert!(
        message.contains("total_models == 0"),
        "unexpected panic message: {message}"
    );
}

#[test]
fn measurability_check_rejects_resolve_errors() {
    let mut summary = valid_summary_template();
    summary.resolve_errors = 1;
    let panic = std::panic::catch_unwind(|| assert_msl_run_is_measurable(&summary))
        .expect_err("resolve-error summary must panic");
    let message = panic_message(&panic);
    assert!(
        message.contains("resolve_errors > 0"),
        "unexpected panic message: {message}"
    );
}

/// The structural floor moved to the strict-high band and is now evaluated
/// AFTER the comparator (see `strict_high_hard_floor_reason_for` and
/// `tests::gate_hole`). The pre-comparator check must therefore NOT abort on a
/// simulation collapse: aborting here is what destroyed the `results-wave3`
/// parity measurement before it could be taken.
#[test]
fn the_pre_comparator_check_never_aborts_on_a_simulation_collapse() {
    let mut summary = valid_summary_template();
    summary.total_models = SIM_SET_LIMIT_DEFAULT;
    summary.sim_attempted = SIM_SET_LIMIT_DEFAULT;
    summary.sim_ok = 0;
    summary.sim_target_models = (0..SIM_SET_LIMIT_DEFAULT)
        .map(|idx| format!("Model{idx}"))
        .collect();

    assert_msl_run_is_measurable(&summary);

    // The collapse is still rejected — by the post-comparator floor, which
    // reads the band the comparator produced rather than `sim_ok`.
    let reason = strict_high_hard_floor_reason_for(SIM_SET_LIMIT_DEFAULT, 0)
        .expect("a simulation collapse must still be rejected, just later");
    assert!(
        reason.contains("strict-high agreement below hard floor"),
        "unexpected floor reason: {reason}"
    );
}

#[test]
fn the_strict_high_floor_accepts_the_transitional_architecture_reset_level() {
    let mut summary = valid_summary_template();
    summary.total_models = SIM_SET_LIMIT_DEFAULT;
    summary.sim_attempted = 166;
    summary.sim_ok = 109;
    summary.sim_target_models = (0..SIM_SET_LIMIT_DEFAULT)
        .map(|idx| format!("Model{idx}"))
        .collect();

    assert_msl_run_is_measurable(&summary);
    assert_eq!(
        strict_high_hard_floor_reason_for(SIM_SET_LIMIT_DEFAULT, 109),
        None,
        "109 models in the strict-high band clears the transitional floor"
    );
}

fn trace_accuracy_baseline() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        models_compared: 10,
        missing_trace_models: 0,
        skipped_models: 0,
        policy_excluded_models: 0,
        trace_nonidentifiable_models: 0,
        agreement_high: 8,
        agreement_high_percent: Some(80.0),
        agreement_minor: 1,
        agreement_minor_percent: Some(10.0),
        agreement_deviation: 1,
        agreement_deviation_percent: Some(10.0),
        total_channels_compared: Some(50),
        bad_channels_total: Some(4),
        severe_channels_total: Some(0),
        bad_channels_percent: Some(8.0),
        severe_channels_percent: Some(0.0),
        violation_mass_total: Some(0.4),
        violation_mass_mean_per_model: Some(0.04),
        violation_mass_mean_per_channel: Some(0.008),
        models_with_bad_channel: Some(1),
        models_with_severe_channel: Some(0),
        models_with_any_channel_deviation: Some(1),
        models_with_any_channel_deviation_percent: Some(10.0),
        max_model_channel_deviation_percent: Some(20.0),
        bounded_normalized_l1: Some(dist(10, 0.0, 0.001, 0.01, 0.1)),
        mean_model_mean_channel_bounded_normalized_l1: Some(0.01),
        max_model_max_channel_bounded_normalized_l1: Some(0.1),
        model_mean_channel_bounded_normalized_l1: Some(dist(10, 0.0, 0.002, 0.01, 0.03)),
        model_max_channel_bounded_normalized_l1: Some(dist(10, 0.0, 0.03, 0.05, 0.1)),
        initial_condition: None,
        state_selection: None,
    }
}

fn trace_accuracy_regressed() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        agreement_high: 6,
        agreement_high_percent: Some(60.0),
        agreement_deviation: 3,
        agreement_deviation_percent: Some(30.0),
        bad_channels_total: Some(7),
        severe_channels_total: Some(1),
        bad_channels_percent: Some(14.0),
        severe_channels_percent: Some(2.0),
        violation_mass_total: Some(1.5),
        violation_mass_mean_per_model: Some(0.15),
        violation_mass_mean_per_channel: Some(0.03),
        models_with_bad_channel: Some(2),
        models_with_severe_channel: Some(1),
        models_with_any_channel_deviation: Some(3),
        models_with_any_channel_deviation_percent: Some(30.0),
        max_model_channel_deviation_percent: Some(40.0),
        bounded_normalized_l1: Some(dist(10, 0.0, 0.01, 0.02, 0.2)),
        mean_model_mean_channel_bounded_normalized_l1: Some(0.02),
        max_model_max_channel_bounded_normalized_l1: Some(0.2),
        model_mean_channel_bounded_normalized_l1: Some(dist(10, 0.0, 0.004, 0.02, 0.08)),
        model_max_channel_bounded_normalized_l1: Some(dist(10, 0.0, 0.05, 0.1, 0.2)),
        ..trace_accuracy_baseline()
    }
}

fn trace_accuracy_small_channel_drift() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        bad_channels_total: Some(5),
        severe_channels_total: Some(1),
        bad_channels_percent: Some(8.9),
        severe_channels_percent: Some(0.4),
        ..trace_accuracy_baseline()
    }
}

fn trace_accuracy_near_promoted_to_high() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        agreement_high: 9,
        agreement_high_percent: Some(90.0),
        agreement_minor: 0,
        agreement_minor_percent: Some(0.0),
        agreement_deviation: 1,
        agreement_deviation_percent: Some(10.0),
        ..trace_accuracy_baseline()
    }
}

fn trace_accuracy_acceptable_band_regressed() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        agreement_high: 7,
        agreement_high_percent: Some(70.0),
        agreement_minor: 0,
        agreement_minor_percent: Some(0.0),
        agreement_deviation: 3,
        agreement_deviation_percent: Some(30.0),
        ..trace_accuracy_baseline()
    }
}

fn trace_accuracy_deviation_migrated_to_near() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        agreement_high: 8,
        agreement_high_percent: Some(80.0),
        agreement_minor: 2,
        agreement_minor_percent: Some(20.0),
        agreement_deviation: 0,
        agreement_deviation_percent: Some(0.0),
        bad_channels_total: Some(2),
        severe_channels_total: Some(0),
        bad_channels_percent: Some(4.0),
        severe_channels_percent: Some(0.0),
        models_with_any_channel_deviation: Some(0),
        models_with_any_channel_deviation_percent: Some(0.0),
        mean_model_mean_channel_bounded_normalized_l1: Some(0.005),
        ..trace_accuracy_baseline()
    }
}

#[test]
fn runtime_ratio_regression_reason_triggers_on_large_drop() {
    let baseline = MslQualityBaseline {
        runtime_ratio_stats: Some(runtime_ratio_stats(2.0, 1.5)),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: Some(runtime_ratio_stats(1.0, 0.5)),
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert_eq!(reasons.len(), 2);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime system speedup median"))
    );
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime wall speedup median"))
    );
}

#[test]
fn msl_quality_regression_reasons_include_runtime_ratio_drop() {
    let mut baseline = baseline_quality_template();
    baseline.trace_accuracy_stats = Some(trace_accuracy_baseline());
    baseline.runtime_ratio_stats = Some(runtime_ratio_stats(2.0, 1.5));
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: Some(runtime_ratio_stats(1.0, 1.5)),
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let reasons =
        msl_quality_regression_reasons(gate_input_with_sim_rate(8, 10), &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime system speedup median")),
        "expected runtime regression in reasons: {reasons:#?}"
    );
}

#[test]
fn runtime_ratio_gate_allows_observed_ci_runner_delta() {
    let baseline = MslQualityBaseline {
        runtime_ratio_stats: Some(runtime_ratio_stats(1.287_891, 1.287_891)),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(566),
        omc_version: Some("OpenModelica 1.26.8".to_string()),
        runtime_context: None,
        runtime_ratio_stats: Some(runtime_ratio_stats(0.887_313_1, 0.887_313_1)),
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons.is_empty(),
        "observed hosted-runner runtime delta should pass, got: {reasons:?}"
    );
}

#[test]
fn runtime_ratio_gate_uses_stable_baseline_cohort_when_successes_expand() {
    let mut cohort = IndexSet::new();
    cohort.insert("baseline_a".to_string());
    cohort.insert("baseline_b".to_string());
    let baseline = MslQualityBaseline {
        runtime_ratio_stats: Some(runtime_ratio_stats(10.0, 10.0)),
        runtime_ratio_cohort_models: Some(cohort),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(3),
        omc_version: Some("OpenModelica 1.26.8".to_string()),
        runtime_context: None,
        runtime_ratio_stats: Some(runtime_ratio_stats(1.0, 1.0)),
        runtime_model_ratios: IndexMap::from([
            (
                "baseline_a".to_string(),
                MslRuntimeModelRatio {
                    system: 10.0,
                    wall: 10.0,
                },
            ),
            (
                "baseline_b".to_string(),
                MslRuntimeModelRatio {
                    system: 10.0,
                    wall: 10.0,
                },
            ),
            (
                "new_slow_success".to_string(),
                MslRuntimeModelRatio {
                    system: 0.1,
                    wall: 0.1,
                },
            ),
        ]),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons.is_empty(),
        "new successes outside the committed runtime cohort must not lower its median: {reasons:?}"
    );
}

#[test]
fn runtime_ratio_gate_requires_baseline_cohort_coverage() {
    let cohort = IndexSet::from(["baseline_a".to_string(), "baseline_b".to_string()]);
    let baseline = MslQualityBaseline {
        runtime_ratio_stats: Some(runtime_ratio_stats(10.0, 10.0)),
        runtime_ratio_cohort_models: Some(cohort),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(2),
        omc_version: Some("OpenModelica 1.26.8".to_string()),
        runtime_context: None,
        runtime_ratio_stats: Some(runtime_ratio_stats(10.0, 10.0)),
        runtime_model_ratios: IndexMap::from([(
            "baseline_a".to_string(),
            MslRuntimeModelRatio {
                system: 10.0,
                wall: 10.0,
            },
        )]),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime baseline cohort coverage")),
        "missing baseline models must not silently shrink the runtime cohort: {reasons:?}"
    );
}

#[test]
fn trace_bucket_and_channel_regression_reasons_trigger_when_thresholds_are_exceeded() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_regressed()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Trace classified pass count regressed")),
        "expected classified trace count regression reason, got: {reasons:?}"
    );
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Trace no severe pass count regressed")),
        "expected no-severe trace count regression reason, got: {reasons:?}"
    );
}

#[test]
fn trace_channel_share_tolerances_allow_small_runner_drift() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_small_channel_drift()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .all(|reason| !reason.contains("trace bad channel")),
        "unexpected bad-channel regression reason: {reasons:?}"
    );
    assert!(
        reasons
            .iter()
            .all(|reason| !reason.contains("trace severe channel")),
        "unexpected severe-channel regression reason: {reasons:?}"
    );
}

#[test]
fn trace_near_to_high_promotion_does_not_trigger_regression() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_near_promoted_to_high()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .all(|reason| !reason.contains("Trace classified pass count regressed")),
        "unexpected classified-count regression reason: {reasons:?}"
    );
}

#[test]
fn trace_deviation_to_near_migration_does_not_trigger_regression() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_deviation_migrated_to_near()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons.is_empty(),
        "deviation-to-near migration should be accepted as improvement, got: {reasons:?}"
    );
}

#[test]
fn trace_classified_count_regression_reason_triggers_on_real_drop() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_acceptable_band_regressed()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Trace classified pass count regressed")),
        "expected classified-count regression reason, got: {reasons:?}"
    );
}

#[test]
fn reviewed_pointwise_exclusions_preserve_trace_accounting_ratchets() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let reviewed = MslTraceAccuracyStatsBaseline {
        models_compared: 8,
        skipped_models: 2,
        policy_excluded_models: 2,
        agreement_high: 8,
        agreement_high_percent: Some(100.0),
        agreement_minor: 0,
        agreement_minor_percent: Some(0.0),
        agreement_deviation: 0,
        agreement_deviation_percent: Some(0.0),
        models_with_bad_channel: Some(0),
        models_with_severe_channel: Some(0),
        models_with_any_channel_deviation: Some(0),
        models_with_any_channel_deviation_percent: Some(0.0),
        ..trace_accuracy_baseline()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(reviewed),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons.is_empty(),
        "reviewed oracle boundaries must remain accounted without masquerading as strict-high: {reasons:?}"
    );
}

#[test]
fn trace_fixed_denominator_gate_accepts_current_ci_delta() {
    let baseline_trace = MslTraceAccuracyStatsBaseline {
        models_compared: 115,
        agreement_high: 64,
        agreement_minor: 16,
        agreement_deviation: 35,
        models_with_severe_channel: Some(6),
        bad_channels_percent: Some(36.38),
        mean_model_mean_channel_bounded_normalized_l1: Some(0.069),
        ..trace_accuracy_baseline()
    };
    let current_trace = MslTraceAccuracyStatsBaseline {
        models_compared: 121,
        agreement_high: 64,
        agreement_minor: 17,
        agreement_deviation: 40,
        models_with_severe_channel: Some(8),
        bad_channels_percent: Some(42.80),
        mean_model_mean_channel_bounded_normalized_l1: Some(0.080),
        ..trace_accuracy_baseline()
    };
    let baseline = MslQualityBaseline {
        sim_target_models: 566,
        trace_accuracy_stats: Some(baseline_trace),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(566),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(current_trace),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons.is_empty(),
        "fixed-denominator trace counts improved, got regression reasons: {reasons:?}"
    );
}

#[test]
fn simulation_soundness_rejects_unclassified_non_high_results() {
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    let mut reasons = Vec::new();

    push_trace_soundness_reasons(
        &mut reasons,
        gate_input_with_sim_rate(10, 10),
        Some(&parity),
    );

    assert_eq!(reasons.len(), 1);
    assert!(
        reasons[0].contains(
            "sim_ok=10 strict_high=8 reviewed_exceptions=0 unclassified=2 overclassified=0"
        )
    );
}

#[test]
fn simulation_soundness_accepts_strict_high_and_reviewed_oracle_boundaries() {
    let mut trace = trace_accuracy_baseline();
    trace.policy_excluded_models = 1;
    trace.trace_nonidentifiable_models = 1;
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        runtime_model_ratios: IndexMap::new(),
        trace_accuracy_stats: Some(trace),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    let mut reasons = Vec::new();

    push_trace_soundness_reasons(
        &mut reasons,
        gate_input_with_sim_rate(10, 10),
        Some(&parity),
    );

    assert!(reasons.is_empty());
}

mod parity_cache;

/// `flatten_models` is a *cumulative pass* count: it must include exactly the
/// models that got past flattening, derived from the pipeline phase order.
///
/// Pinning the derivation matters because it moved once already: while the
/// worker attributed every marker-free compile summary to `ToDae`, resolve
/// failures were counted as "flattened". Restating the phase set by hand is
/// what let that mis-attribution inflate a gated metric, so the order is the
/// single source of truth and this test is its contract.
#[test]
fn completed_compile_phase_follows_the_pipeline_order() {
    // Failing later than Flatten (or succeeding) means flattening completed.
    assert!(completed_compile_phase("ToDae", "Flatten"));
    assert!(completed_compile_phase("Success", "Flatten"));
    // Failing in Flatten means flattening did *not* complete.
    assert!(!completed_compile_phase("Flatten", "Flatten"));
    // Everything before Flatten never reached it. `Resolve` is the case the
    // corrected worker attribution produces for `ER0xx` failures that used to
    // fall through to `ToDae`.
    assert!(!completed_compile_phase("Resolve", "Flatten"));
    assert!(!completed_compile_phase("NeedsInner", "Flatten"));
    assert!(!completed_compile_phase("Instantiate", "Flatten"));
    assert!(!completed_compile_phase("Typecheck", "Flatten"));
    assert!(!completed_compile_phase("Parse", "Flatten"));
    // Unknown phase strings (e.g. `NonSim`) never count towards a floor.
    assert!(!completed_compile_phase("NonSim", "Flatten"));
    // `dae_models` uses the same predicate against ToDae.
    assert!(completed_compile_phase("Success", "ToDae"));
    assert!(!completed_compile_phase("ToDae", "ToDae"));
}

#[test]
fn checked_quality_baseline_has_versioned_metric_migration_and_tensor_kpi() {
    let baseline =
        load_msl_quality_baseline(&msl_quality_baseline_path()).expect("load checked baseline");
    assert_eq!(baseline.quality_gate_version, MSL_QUALITY_GATE_VERSION);
    assert_eq!(baseline.flatten_models, 555);
    assert!(baseline.tensor_preservation.models_reported > 0);
    assert!(baseline.tensor_preservation.family_bodies > 0);
    assert_eq!(baseline.tensor_preservation.report_errors, 0);

    let migration = baseline
        .metric_schema_migration
        .expect("checked baseline must document the version-1 to version-2 migration");
    assert_eq!(migration.from_quality_gate_version, 1);
    assert_eq!(migration.to_quality_gate_version, MSL_QUALITY_GATE_VERSION);
    assert_eq!(migration.flatten_models_before, 565);
    assert_eq!(migration.flatten_models_after, 555);
    assert_eq!(migration.reattributed_error_code, "ER002");
    assert_eq!(migration.reattributed_models.len(), 10);
    assert!(!migration.tensor_preservation_source_git_commit.is_empty());
}

#[test]
fn quality_baseline_rejects_missing_tensor_preservation_field() {
    let mut value =
        serde_json::to_value(baseline_quality_template()).expect("serialize baseline fixture");
    value
        .as_object_mut()
        .expect("baseline serializes as object")
        .remove("tensor_preservation");

    let error = serde_json::from_value::<MslQualityBaseline>(value)
        .expect_err("schema-v2 baseline without tensor KPI must fail");
    assert!(error.to_string().contains("tensor_preservation"));
}

#[test]
fn quality_baseline_rejects_missing_schema_version() {
    let mut value =
        serde_json::to_value(baseline_quality_template()).expect("serialize baseline fixture");
    value
        .as_object_mut()
        .expect("baseline serializes as object")
        .remove("quality_gate_version");

    let error = serde_json::from_value::<MslQualityBaseline>(value)
        .expect_err("baseline without schema version must fail");
    assert!(error.to_string().contains("quality_gate_version"));
}

/// The stage counts the gate ratchets must be derived from the row set, not
/// restated: a resolve failure is not a flattened model, and a `ToDae` failure
/// is not a compiled one.
#[test]
fn gate_input_stage_counts_are_derived_from_phase_reached() {
    let mut summary = valid_summary_template();
    summary.model_results = vec![
        phase_error_result("Modelica.A".to_string(), "Success", None, None),
        phase_error_result("Modelica.B".to_string(), "ToDae", None, None),
        phase_error_result("Modelica.C".to_string(), "Flatten", None, None),
        phase_error_result("Modelica.D".to_string(), "Typecheck", None, None),
        // Was mis-attributed to `ToDae` before the worker reported the real
        // failing phase; it never reached flatten.
        phase_error_result("Modelica.E".to_string(), "Resolve", None, None),
    ];

    let gate_input = MslQualityGateInput::from(&summary);
    assert_eq!(gate_input.flatten_models, 2, "Success + ToDae");
    assert_eq!(gate_input.dae_models, 1, "Success only");
}
