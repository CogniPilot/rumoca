use super::*;
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
        sim_timeout_seconds: 10.0,
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
        trace_accuracy_stats: None,
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
    }
}

fn valid_summary_template() -> MslSummary {
    let mut summary = super::super::empty_summary(1, 0);
    summary.total_models = 1;
    summary
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
fn quality_context_reports_omc_version_mismatch_for_pinned_baseline() {
    let gate_input = gate_input_with_sim_rate(8, 10);
    let baseline = baseline_quality_template();
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.27.0".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
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
        .entry("embedded-c".to_string())
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
            .and_then(|value| value.get("embedded-c"))
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
fn sim_stage_gate_allows_equal_cumulative_count() {
    let baseline = MslQualityBaseline {
        sim_ok: 800,
        sim_attempted: 1000,
        sim_target_models: 1000,
        sim_success_rate: 0.8,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(800, 1000);
    gate_input.sim_target_models = 1000;

    let mut reasons = Vec::new();
    push_sim_rate_regression_reason(&mut reasons, gate_input, &baseline);
    assert!(
        reasons.is_empty(),
        "equal cumulative simulation count should pass, got: {reasons:?}"
    );
}

#[test]
fn sim_stage_gate_rejects_one_model_drop() {
    let baseline = MslQualityBaseline {
        sim_ok: 800,
        sim_attempted: 1000,
        sim_target_models: 1000,
        sim_success_rate: 0.8,
        ..baseline_quality_template()
    };
    let mut gate_input = gate_input_with_sim_rate(799, 1000);
    gate_input.sim_target_models = 1000;

    let mut reasons = Vec::new();
    push_sim_rate_regression_reason(&mut reasons, gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Sim pass count regressed")),
        "expected simulation-stage regression reason, got: {reasons:?}"
    );
}

#[test]
fn ic_stage_gate_rejects_one_model_drop() {
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

    let mut reasons = Vec::new();
    push_sim_rate_regression_reason(&mut reasons, gate_input, &baseline);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("IC pass count regressed")),
        "expected IC-stage regression reason, got: {reasons:?}"
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
fn valid_msl_summary_rejects_zero_total_models() {
    let summary = super::super::empty_summary(1, 0);
    let panic = std::panic::catch_unwind(|| assert_valid_msl_summary(&summary))
        .expect_err("zero-model summary must panic");
    let message = panic_message(&panic);
    assert!(
        message.contains("total_models == 0"),
        "unexpected panic message: {message}"
    );
}

#[test]
fn valid_msl_summary_rejects_resolve_errors() {
    let mut summary = valid_summary_template();
    summary.resolve_errors = 1;
    let panic = std::panic::catch_unwind(|| assert_valid_msl_summary(&summary))
        .expect_err("resolve-error summary must panic");
    let message = panic_message(&panic);
    assert!(
        message.contains("resolve_errors > 0"),
        "unexpected panic message: {message}"
    );
}

#[test]
fn valid_msl_summary_rejects_baseline_sim_run_below_hard_floor() {
    let mut summary = valid_summary_template();
    summary.total_models = SIM_SET_LIMIT_DEFAULT;
    summary.sim_attempted = SIM_SET_LIMIT_DEFAULT;
    summary.sim_ok = 0;
    summary.sim_target_models = (0..SIM_SET_LIMIT_DEFAULT)
        .map(|idx| format!("Model{idx}"))
        .collect();

    let panic = std::panic::catch_unwind(|| assert_valid_msl_summary(&summary))
        .expect_err("baseline simulation collapse must panic");
    let message = panic_message(&panic);
    assert!(
        message.contains("sim_ok below hard floor"),
        "unexpected panic message: {message}"
    );
}

#[test]
fn valid_msl_summary_accepts_transitional_architecture_reset_floor() {
    let mut summary = valid_summary_template();
    summary.total_models = SIM_SET_LIMIT_DEFAULT;
    summary.sim_attempted = 166;
    summary.sim_ok = 109;
    summary.sim_target_models = (0..SIM_SET_LIMIT_DEFAULT)
        .map(|idx| format!("Model{idx}"))
        .collect();

    assert_valid_msl_summary(&summary);
}

fn trace_accuracy_baseline() -> MslTraceAccuracyStatsBaseline {
    MslTraceAccuracyStatsBaseline {
        models_compared: 10,
        missing_trace_models: 0,
        skipped_models: 0,
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
        quality_gate_version: MSL_QUALITY_GATE_VERSION,
        run_scope: MSL_QUALITY_RUN_SCOPE_FULL.to_string(),
        git_commit: "baseline".to_string(),
        msl_version: "v4.1.0".to_string(),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        sim_timeout_seconds: 10.0,
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
        runtime_ratio_stats: Some(MslRuntimeRatioStatsBaseline {
            system_ratio_both_success: MslDistributionStats {
                sample_count: 8,
                min: 0.9,
                median: 2.0,
                mean: 2.1,
                max: 3.0,
            },
            wall_ratio_both_success: MslDistributionStats {
                sample_count: 8,
                min: 0.8,
                median: 1.5,
                mean: 1.6,
                max: 2.6,
            },
        }),
        trace_accuracy_stats: None,
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: Some(MslRuntimeRatioStatsBaseline {
            system_ratio_both_success: MslDistributionStats {
                sample_count: 8,
                min: 0.4,
                median: 1.0,
                mean: 1.1,
                max: 1.8,
            },
            wall_ratio_both_success: MslDistributionStats {
                sample_count: 8,
                min: 0.3,
                median: 1.0,
                mean: 1.1,
                max: 1.7,
            },
        }),
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert_eq!(reasons.len(), 1);
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("runtime system speedup median"))
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
        trace_accuracy_stats: Some(trace_accuracy_regressed()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Trace acceptable pass count regressed")),
        "expected acceptable trace count regression reason, got: {reasons:?}"
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
        trace_accuracy_stats: Some(trace_accuracy_near_promoted_to_high()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .all(|reason| !reason.contains("Trace acceptable pass count regressed")),
        "unexpected acceptable-band regression reason: {reasons:?}"
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
fn trace_acceptable_band_regression_reason_triggers_on_real_drop() {
    let baseline = MslQualityBaseline {
        trace_accuracy_stats: Some(trace_accuracy_baseline()),
        ..baseline_quality_template()
    };
    let parity = MslParityGateInput {
        total_models: Some(10),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: Some(trace_accuracy_acceptable_band_regressed()),
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("Trace acceptable pass count regressed")),
        "expected acceptable-band regression reason, got: {reasons:?}"
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

fn valid_simulation_parity_payload() -> Value {
    json!({
        "total_models": 7,
        "runtime_comparison": { "ratio_stats": {
            "system_ratio_both_success": {
                "sample_count": 5,
                "min_ratio": 0.5,
                "median_ratio": 0.9,
                "mean_ratio": 1.0,
                "max_ratio": 1.3
            },
            "wall_ratio_both_success": {
                "sample_count": 5,
                "min_ratio": 0.4,
                "median_ratio": 0.8,
                "mean_ratio": 0.9,
                "max_ratio": 1.4
            }
        }},
        "trace_comparison": {
            "models_compared": 7,
            "missing_trace_models": 0,
            "skipped_models": 0,
            "agreement_high": 5,
            "agreement_minor": 1,
            "agreement_deviation": 1,
            "min_model_bounded_normalized_l1": 0.01,
            "median_model_bounded_normalized_l1": 0.02,
            "mean_model_bounded_normalized_l1": 0.03,
            "max_model_bounded_normalized_l1": 0.08,
            "state_selection": {
                "models_compared": 7,
                "exact_state_set_match_models": 7,
                "state_count_match_models": 7,
                "exact_state_set_match_percent": 100.0,
                "state_count_match_percent": 100.0,
                "total_rumoca_states": 7,
                "total_omc_states": 7,
                "total_matching_states": 7,
                "total_rumoca_only_states": 0,
                "total_omc_only_states": 0,
                "max_model_state_set_difference": 0
            }
        }
    })
}

#[test]
fn simulation_parity_cache_requires_runtime_and_trace_metrics() {
    fn write_payload(path: &Path, payload: &Value) {
        std::fs::write(
            path,
            serde_json::to_vec(payload).expect("serialize payload"),
        )
        .expect("write payload");
    }
    fn assert_cache_metric_check(path: &Path, payload: Value, expected: bool) {
        write_payload(path, &payload);
        let actual = simulation_parity_cache_has_required_metrics(path)
            .expect("check parity metrics payload");
        assert_eq!(actual, expected);
    }

    let dir = tempdir().expect("tempdir");
    let path = dir.path().join("omc_simulation_reference.json");

    let missing = json!({
        "runtime_comparison": { "ratio_stats": {
            "system_ratio_both_success": null,
            "wall_ratio_both_success": null
        }},
        "trace_comparison": { "models_compared": 0 }
    });
    assert_cache_metric_check(&path, missing, false);
    assert_cache_metric_check(&path, valid_simulation_parity_payload(), true);

    let mut assertion_failure = valid_simulation_parity_payload();
    assertion_failure["pipeline_progress"] = json!({ "omc_assertion_failure_models": 1 });
    assertion_failure["omc_assertion_failures"] = json!({
        "model_count": 1,
        "examples": [{
            "model_name": "ModelicaTest.AssertDemo",
            "assertions": ["assert | error | x > 0"]
        }]
    });
    assert_cache_metric_check(&path, assertion_failure, false);
}

#[test]
fn sanitize_simulation_parity_cache_payload_strips_rumoca_metrics() {
    let payload = json!({
        "runtime_comparison": {
            "ratio_stats": {
                "system_ratio_both_success": { "sample_count": 5 },
                "wall_ratio_both_success": { "sample_count": 5 }
            }
        },
        "trace_comparison": {
            "models_compared": 7
        },
        "models": {
            "A": {
                "status": "success",
                "trace_file": "sim_traces/omc/A.json",
                    "rumoca_status": "sim_ok",
                    "rumoca_ic_status": "ic_ok",
                    "rumoca_ic_seconds": 0.01,
                    "rumoca_sim_seconds": 1.0,
                "rumoca_sim_wall_seconds": 1.1,
                "rumoca_trace_file": "sim_traces/rumoca/A.json",
                "rumoca_trace_error": null
            }
        }
    });

    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    assert!(
        sanitized.get("runtime_comparison").is_none(),
        "simulation parity cache should not preserve runtime comparison stats"
    );
    assert!(
        sanitized.get("trace_comparison").is_none(),
        "simulation parity cache should not preserve trace comparison stats"
    );
    let model = sanitized
        .get("models")
        .and_then(Value::as_object)
        .and_then(|models| models.get("A"))
        .and_then(Value::as_object)
        .expect("sanitized cache should preserve OMC model entry");
    assert_eq!(model.get("status").and_then(Value::as_str), Some("success"));
    assert_eq!(
        model.get("trace_file").and_then(Value::as_str),
        Some("sim_traces/omc/A.json")
    );
    assert!(
        model.get("rumoca_status").is_none(),
        "cache should strip Rumoca status"
    );
    assert!(
        model.get("rumoca_ic_status").is_none(),
        "cache should strip Rumoca IC status"
    );
    assert!(
        model.get("rumoca_sim_seconds").is_none(),
        "cache should strip Rumoca runtime"
    );
    assert!(
        model.get("rumoca_sim_wall_seconds").is_none(),
        "cache should strip Rumoca wall runtime"
    );
    assert!(
        model.get("rumoca_trace_file").is_none(),
        "cache should strip Rumoca trace file"
    );
    assert!(
        model.get("rumoca_trace_error").is_none(),
        "cache should strip Rumoca trace error"
    );
}

#[test]
fn materialize_simulation_parity_cache_entry_strips_stale_rumoca_metrics() {
    let temp = tempdir().expect("tempdir");
    let cache_path = temp.path().join("cache.json");
    let active_path = temp.path().join("active.json");
    fs::write(
        &cache_path,
        serde_json::to_vec_pretty(&json!({
            "runtime_comparison": {
                "ratio_stats": {
                    "system_ratio_both_success": { "sample_count": 5 },
                    "wall_ratio_both_success": { "sample_count": 5 }
                }
            },
            "trace_comparison": {
                "models_compared": 7
            },
            "models": {
                "A": {
                    "status": "success",
                    "trace_file": "sim_traces/omc/A.json",
                    "rumoca_status": "sim_ok",
                    "rumoca_ic_status": "ic_ok",
                    "rumoca_trace_file": "sim_traces/rumoca/A.json"
                }
            }
        }))
        .expect("serialize cache payload"),
    )
    .expect("write cache payload");

    materialize_simulation_parity_cache_entry(&cache_path, &active_path)
        .expect("materialize sanitized cache");

    let active: Value = serde_json::from_slice(&fs::read(&active_path).expect("read active"))
        .expect("parse active payload");
    assert!(
        active.get("runtime_comparison").is_none(),
        "active simulation reference should not inherit cached runtime comparison"
    );
    assert!(
        active.get("trace_comparison").is_none(),
        "active simulation reference should not inherit cached trace comparison"
    );
    let model = active
        .get("models")
        .and_then(Value::as_object)
        .and_then(|models| models.get("A"))
        .and_then(Value::as_object)
        .expect("materialized active payload should preserve model entry");
    assert_eq!(model.get("status").and_then(Value::as_str), Some("success"));
    assert!(
        model.get("rumoca_status").is_none(),
        "active simulation reference should drop cached Rumoca status"
    );
    assert!(
        model.get("rumoca_ic_status").is_none(),
        "active simulation reference should drop cached Rumoca IC status"
    );
    assert!(
        model.get("rumoca_trace_file").is_none(),
        "active simulation reference should drop cached Rumoca trace path"
    );
}

#[test]
fn parity_total_models_guard_checks_stale_and_matching_counts() {
    let path = PathBuf::from("/tmp/omc_simulation_reference.json");
    let stale = MslParityGateInput {
        total_models: Some(1),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    let err = validate_parity_total_models(&path, &stale, 180).expect_err("must fail stale count");
    assert!(
        err.to_string().contains("is stale"),
        "unexpected error: {err}"
    );
    let matching = MslParityGateInput {
        total_models: Some(180),
        omc_version: Some("OpenModelica 1.26.1".to_string()),
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: None,
        omc_assertion_failure_models: 0,
        omc_assertion_failure_examples: Vec::new(),
    };
    validate_parity_total_models(&path, &matching, 180).expect("matching count should pass");
}

#[test]
fn parity_target_set_cache_key_is_order_insensitive() {
    let lhs = parity_target_set_cache_key(
        &["B".to_string(), "A".to_string()],
        "v4.1.0",
        "OpenModelica 1.26.1",
    );
    let rhs = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
    );
    assert_eq!(lhs, rhs, "cache key should ignore target order");
}

#[test]
fn parity_target_set_cache_key_changes_with_models_or_versions() {
    let base = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
    );
    let diff_models =
        parity_target_set_cache_key(&["A".to_string()], "4.1.0", "OpenModelica 1.26.1");
    let diff_msl = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.2.0",
        "OpenModelica 1.26.1",
    );
    let diff_omc = parity_target_set_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.27.0",
    );
    assert_ne!(base, diff_models);
    assert_ne!(base, diff_msl);
    assert_ne!(base, diff_omc);
}

#[test]
fn simulation_parity_cache_key_changes_with_policy() {
    let base = simulation_parity_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
        SimulationParityCachePolicy {
            batch_timeout_seconds: 600,
            use_experiment_stop_time: true,
            stop_time_override: None,
        },
    );
    let diff_timeout = simulation_parity_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
        SimulationParityCachePolicy {
            batch_timeout_seconds: 900,
            use_experiment_stop_time: true,
            stop_time_override: None,
        },
    );
    let diff_override = simulation_parity_cache_key(
        &["A".to_string(), "B".to_string()],
        "4.1.0",
        "OpenModelica 1.26.1",
        SimulationParityCachePolicy {
            batch_timeout_seconds: 600,
            use_experiment_stop_time: false,
            stop_time_override: Some(30.0),
        },
    );
    assert_ne!(base, diff_timeout);
    assert_ne!(base, diff_override);
}

#[test]
fn simulation_parity_cache_matches_rejects_mismatched_policy() {
    let temp = tempdir().expect("tempdir");
    let path = temp.path().join("omc_simulation_reference.json");
    fs::write(
        &path,
        serde_json::to_vec_pretty(&json!({
            "msl_version": "4.1.0",
            "omc_version": "OpenModelica 1.26.1",
            "stop_time": 10.0,
            "use_experiment_stop_time": true,
            "timing": {
                "batch_timeout_seconds": 600
            },
            "models": {
                "A": { "status": "success" },
                "B": { "status": "success" }
            }
        }))
        .expect("serialize cache payload"),
    )
    .expect("write cache payload");

    let matching = SimulationParityCachePolicy {
        batch_timeout_seconds: 600,
        use_experiment_stop_time: true,
        stop_time_override: None,
    };
    let mismatched_timeout = SimulationParityCachePolicy {
        batch_timeout_seconds: 900,
        ..matching
    };
    let mismatched_override = SimulationParityCachePolicy {
        batch_timeout_seconds: 600,
        use_experiment_stop_time: false,
        stop_time_override: Some(30.0),
    };
    assert!(
        simulation_parity_cache_matches(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            matching,
        )
        .expect("matching policy should parse"),
        "matching simulation policy should reuse cache entry"
    );
    assert!(
        !simulation_parity_cache_matches(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            mismatched_timeout,
        )
        .expect("mismatched timeout should parse"),
        "batch-timeout drift should invalidate cache entry"
    );
    assert!(
        !simulation_parity_cache_matches(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            mismatched_override,
        )
        .expect("mismatched override should parse"),
        "stop-time policy drift should invalidate cache entry"
    );
}
