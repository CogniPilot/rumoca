use super::*;
use serde_json::Value;
use serde_json::json;
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

fn baseline_quality_template() -> MslQualityBaseline {
    MslQualityBaseline {
        git_commit: "baseline".to_string(),
        msl_version: "v4.1.0".to_string(),
        sim_timeout_seconds: 10.0,
        simulatable_attempted: 10,
        compiled_models: 10,
        balanced_models: 10,
        unbalanced_models: 0,
        partial_models: 0,
        balance_denominator: 10,
        initial_balanced_models: 10,
        initial_unbalanced_models: 0,
        sim_target_models: 10,
        sim_attempted: 10,
        sim_ok: 8,
        sim_success_rate: 0.8,
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: None,
    }
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

#[test]
fn runtime_ratio_regression_reason_triggers_on_large_drop() {
    let baseline = MslQualityBaseline {
        git_commit: "baseline".to_string(),
        msl_version: "v4.1.0".to_string(),
        sim_timeout_seconds: 10.0,
        simulatable_attempted: 10,
        compiled_models: 10,
        balanced_models: 10,
        unbalanced_models: 0,
        partial_models: 0,
        balance_denominator: 10,
        initial_balanced_models: 10,
        initial_unbalanced_models: 0,
        sim_target_models: 10,
        sim_attempted: 10,
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
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: Some(trace_accuracy_regressed()),
    };

    let mut reasons = Vec::new();
    push_trace_regression_reasons(&mut reasons, &baseline, Some(&parity));
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("trace high-agreement model share regressed")),
        "expected high-bucket regression reason, got: {reasons:?}"
    );
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("trace deviation model share regressed")),
        "expected deviation-bucket regression reason, got: {reasons:?}"
    );
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("trace bad channel count regressed")),
        "expected bad-channel regression reason, got: {reasons:?}"
    );
    assert!(
        reasons
            .iter()
            .any(|reason| reason.contains("trace severe channel count regressed")),
        "expected severe-channel regression reason, got: {reasons:?}"
    );
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

    let valid = json!({
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
            "max_model_bounded_normalized_l1": 0.08
        }
    });
    assert_cache_metric_check(&path, valid, true);
}

#[test]
fn parity_total_models_guard_checks_stale_and_matching_counts() {
    let path = PathBuf::from("/tmp/omc_simulation_reference.json");
    let stale = MslParityGateInput {
        total_models: Some(1),
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: None,
    };
    let err = validate_parity_total_models(&path, &stale, 180).expect_err("must fail stale count");
    assert!(
        err.to_string().contains("is stale"),
        "unexpected error: {err}"
    );
    let matching = MslParityGateInput {
        total_models: Some(180),
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: None,
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
