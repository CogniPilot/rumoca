use super::{
    super::VerifyMslParityArgs, BaselineChoice, DistributionMedian, InitialConditionStats,
    MetricSchemaMigration, MslQualityBaselineHeader, OmcContextMigration, RuntimeRatioStats,
    StateSelectionStats, TraceAccuracyStats, choose_baseline, load_baseline_header,
    validate_context_migration, validate_metric_schema_migration,
};
use serde_json::json;
use std::{fs, path::PathBuf};

fn header(omc_version: &str) -> MslQualityBaselineHeader {
    MslQualityBaselineHeader {
        quality_gate_version: 3,
        run_scope: "full".to_string(),
        omc_version: omc_version.to_string(),
        sim_target_models: 566,
        omc_context_migration: None,
        metric_schema_migration: None,
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 565,
        dae_models: 545,
        compiled_models: 545,
        solve_models: 446,
        balanced_models: 532,
        unbalanced_models: 0,
        partial_models: 13,
        balance_denominator: 532,
        initial_balanced_models: 532,
        initial_unbalanced_models: 0,
        sim_attempted: 496,
        ic_attempted: 267,
        ic_ok: 252,
        ic_solver_fail: 15,
        sim_ok: 207,
        runtime_ratio_stats: RuntimeRatioStats {
            system_ratio_both_success: DistributionMedian { median: 1.5 },
            wall_ratio_both_success: DistributionMedian { median: 45.0 },
        },
        trace_accuracy_stats: TraceAccuracyStats {
            models_compared: 202,
            policy_excluded_models: 0,
            agreement_high: 143,
            agreement_minor: 45,
            agreement_deviation: 14,
            bad_channels_total: 784,
            severe_channels_total: 123,
            models_with_severe_channel: 38,
            models_with_any_channel_deviation: 60,
            violation_mass_total: 229.0,
            initial_condition: InitialConditionStats {
                deviation_channels_total: 439,
                severe_channels_total: 50,
                violation_mass_total: 121.0,
            },
            state_selection: StateSelectionStats {
                exact_state_set_match_models: 164,
                total_rumoca_only_states: 108,
                total_omc_only_states: 122,
            },
        },
    }
}

fn migration(from: &str, to: &str) -> OmcContextMigration {
    OmcContextMigration {
        from_omc_version: from.to_string(),
        to_omc_version: to.to_string(),
        sim_target_models: 566,
    }
}

fn schema_migration() -> MetricSchemaMigration {
    MetricSchemaMigration {
        from_quality_gate_version: 2,
        to_quality_gate_version: 3,
        change: "reviewed-pointwise-oracle-boundaries-v1".to_string(),
        strict_high_before: 118,
        strict_high_after: 113,
        policy_excluded_after: 9,
        excluded_strict_high_before: 5,
        excluded_non_high_before: 4,
        exclusions_file: "crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json"
            .to_string(),
        exclusions_sha256: "e064ffb80771c1e231e849afcaa25cc2a08b8b7f9bf449bf8651905e5dcdc4d0"
            .to_string(),
    }
}

#[test]
fn msl_parity_config_forwards_resolved_quality_baseline_path() {
    let args = VerifyMslParityArgs {
        quality_baseline: Some(PathBuf::from(
            "target/msl/baselines/msl_quality_baseline.json",
        )),
        ..VerifyMslParityArgs::default()
    };
    let config = args.to_parity_config_json();

    assert_eq!(
        config
            .get("quality_baseline_file")
            .and_then(serde_json::Value::as_str),
        Some("target/msl/baselines/msl_quality_baseline.json")
    );
}

#[test]
fn default_msl_parity_uses_baseline_relative_quality_gate() {
    assert!(VerifyMslParityArgs::default().uses_baseline_relative_quality_gate());
    let short_run = VerifyMslParityArgs {
        sim_set: Some("short".to_string()),
        ..VerifyMslParityArgs::default()
    };
    assert!(!short_run.uses_baseline_relative_quality_gate());
}

#[test]
fn checked_in_baseline_declares_omc_context_migration() {
    let promoted = header("OpenModelica 1.27.0");
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.omc_context_migration = Some(migration("OpenModelica 1.27.0", "a96aa1a-cmake"));

    assert_eq!(
        choose_baseline(&promoted, &checked_in).expect("declared migration should select"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn same_omc_context_keeps_promoted_baseline() {
    assert_eq!(
        choose_baseline(&header("a96aa1a-cmake"), &header("a96aa1a-cmake"))
            .expect("same context should select"),
        BaselineChoice::Promoted
    );
}

#[test]
fn newer_checked_in_metric_schema_precedes_promoted_baseline() {
    let mut promoted = header("a96aa1a-cmake");
    promoted.quality_gate_version = 2;
    promoted.trace_accuracy_stats.agreement_high = 118;
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.trace_accuracy_stats.agreement_high = 113;
    checked_in.trace_accuracy_stats.policy_excluded_models = 9;
    checked_in.metric_schema_migration = Some(schema_migration());
    assert_eq!(
        choose_baseline(&promoted, &checked_in).expect("declared schema migration"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn metric_schema_migration_requires_exact_accounting_and_artifact() {
    let mut baseline = header("a96aa1a-cmake");
    let mut migration = schema_migration();
    migration.policy_excluded_after = 8;
    baseline.metric_schema_migration = Some(migration);
    assert!(validate_metric_schema_migration(&baseline).is_err());

    let mut migration = schema_migration();
    migration.excluded_strict_high_before = 4;
    baseline.metric_schema_migration = Some(migration);
    assert!(validate_metric_schema_migration(&baseline).is_err());

    let mut migration = schema_migration();
    migration.exclusions_sha256 = "unreviewed".to_string();
    baseline.metric_schema_migration = Some(migration);
    assert!(validate_metric_schema_migration(&baseline).is_err());
}

#[test]
fn metric_schema_migration_rejects_unrelated_cumulative_regression() {
    let mut promoted = header("a96aa1a-cmake");
    promoted.quality_gate_version = 2;
    promoted.trace_accuracy_stats.agreement_high = 118;
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.trace_accuracy_stats.agreement_high = 113;
    checked_in.trace_accuracy_stats.policy_excluded_models = 9;
    checked_in.compiled_models -= 1;
    checked_in.metric_schema_migration = Some(schema_migration());

    let error =
        choose_baseline(&promoted, &checked_in).expect_err("unrelated regression must fail");
    assert!(error.to_string().contains("compiled models"), "{error}");
}

#[test]
fn metric_schema_migration_rejects_unrelated_headline_regression() {
    let mut promoted = header("a96aa1a-cmake");
    promoted.quality_gate_version = 2;
    promoted.trace_accuracy_stats.agreement_high = 118;
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.trace_accuracy_stats.agreement_high = 112;
    checked_in.trace_accuracy_stats.policy_excluded_models = 9;
    checked_in.metric_schema_migration = Some(schema_migration());

    let error = choose_baseline(&promoted, &checked_in).expect_err("headline regression must fail");
    assert!(
        error
            .to_string()
            .contains("trace-classification migration counts"),
        "{error}"
    );
}

#[test]
fn omc_context_migration_compares_only_context_independent_metrics() {
    let promoted = header("old");
    let mut checked_in = header("new");
    checked_in.omc_context_migration = Some(migration("old", "new"));
    checked_in.trace_accuracy_stats.agreement_high = 0;
    checked_in
        .runtime_ratio_stats
        .system_ratio_both_success
        .median = 0.01;
    assert_eq!(
        choose_baseline(&promoted, &checked_in)
            .expect("OMC-dependent metrics are not cross-context comparable"),
        BaselineChoice::CheckedInMigration
    );

    checked_in.compiled_models -= 1;
    let error = choose_baseline(&promoted, &checked_in)
        .expect_err("context-independent regression must still fail");
    assert!(error.to_string().contains("compiled models"), "{error}");
}

#[test]
fn combined_schema_and_omc_migration_accepts_exact_trace_classification() {
    let mut promoted = header("old");
    promoted.quality_gate_version = 2;
    promoted.trace_accuracy_stats.agreement_high = 118;
    let mut checked_in = header("new");
    checked_in.trace_accuracy_stats.agreement_high = 113;
    checked_in.trace_accuracy_stats.policy_excluded_models = 9;
    checked_in.metric_schema_migration = Some(schema_migration());
    checked_in.omc_context_migration = Some(migration("old", "new"));

    assert_eq!(
        choose_baseline(&promoted, &checked_in)
            .expect("reviewed schema correction and OMC context are valid"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn changed_omc_context_requires_exact_migration_declaration() {
    let promoted = header("old");
    let mut checked_in = header("new");
    assert!(choose_baseline(&promoted, &checked_in).is_err());

    checked_in.omc_context_migration = Some(migration("new", "old"));
    assert!(choose_baseline(&promoted, &checked_in).is_err());

    checked_in.omc_context_migration = Some(migration("old", "new"));
    checked_in
        .omc_context_migration
        .as_mut()
        .unwrap()
        .sim_target_models = 565;
    assert!(choose_baseline(&promoted, &checked_in).is_err());
}

#[test]
fn migration_must_be_internally_consistent_without_promoted_baseline() {
    let mut baseline = header("new");
    baseline.omc_context_migration = Some(migration("old", "other"));
    assert!(validate_context_migration(&baseline).is_err());

    baseline.omc_context_migration = Some(migration("new", "new"));
    assert!(validate_context_migration(&baseline).is_err());

    baseline.omc_context_migration = Some(migration("old", "new"));
    baseline
        .omc_context_migration
        .as_mut()
        .unwrap()
        .sim_target_models = 565;
    assert!(validate_context_migration(&baseline).is_err());
}

#[test]
fn baseline_header_rejects_missing_or_invalid_omc_version() {
    let temp = tempfile::tempdir().expect("temporary directory should be available");
    let invalid_versions = [None, Some(json!(null)), Some(json!(7)), Some(json!(" "))];

    for (index, version) in invalid_versions.into_iter().enumerate() {
        let path = temp.path().join(format!("invalid-{index}.json"));
        let mut baseline = json!({
            "quality_gate_version": 3,
            "run_scope": "full",
            "git_commit": "fixture",
            "sim_target_models": 566
        });
        if let Some(version) = version {
            baseline["omc_version"] = version;
        }
        fs::write(&path, baseline.to_string()).expect("fixture should be writable");
        let error = load_baseline_header(&path).expect_err("invalid context must fail");
        assert!(error.to_string().contains("omc_version"), "{error}");
    }
}
