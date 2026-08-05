use super::{
    super::VerifyMslParityArgs, BaselineChoice, CompilerContractMigration,
    CompilerContractStageCounts, DistributionMedian, InitialConditionStats, MetricSchemaMigration,
    MslQualityBaselineHeader, OmcContextMigration, RuntimeRatioStats, StateSelectionStats,
    TraceAccuracyStats, V2_REATTRIBUTED_MODELS, choose_baseline, load_baseline_header,
    reviewed_contract_counts_after, reviewed_contract_counts_before,
    reviewed_error_code_counts_after, reviewed_phase_failure_counts_after,
    validate_context_migration, validate_metric_schema_migration,
};
use serde_json::json;
use std::{fs, path::PathBuf};

fn header(omc_version: &str) -> MslQualityBaselineHeader {
    MslQualityBaselineHeader {
        quality_gate_version: 2,
        run_scope: "full".to_string(),
        git_commit: "current".to_string(),
        omc_version: omc_version.to_string(),
        sim_target_models: 566,
        omc_context_migration: None,
        metric_schema_migration: None,
        compiler_contract_migration: None,
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

fn compiler_contract_migration() -> CompilerContractMigration {
    CompilerContractMigration {
        from_contract: "permissive-dae-v1".to_string(),
        to_contract: "checked-dae-v1".to_string(),
        evidence_git_commit: "3fc9a6cb9c60e1137eb6151f29cb87e9ad35064b".to_string(),
        sim_target_models: 566,
        stage_counts_before: reviewed_contract_counts_before(),
        stage_counts_after: reviewed_contract_counts_after(),
        phase_failure_counts_after: reviewed_phase_failure_counts_after(),
        error_code_counts_after: reviewed_error_code_counts_after(),
    }
}

fn apply_stage_counts(header: &mut MslQualityBaselineHeader, counts: &CompilerContractStageCounts) {
    header.parse_models = counts.parse_models;
    header.flatten_models = counts.flatten_models;
    header.dae_models = counts.dae_models;
    header.compiled_models = counts.compiled_models;
    header.solve_models = counts.solve_models;
    header.balanced_models = counts.balanced_models;
    header.unbalanced_models = counts.unbalanced_models;
    header.partial_models = counts.partial_models;
    header.balance_denominator = counts.balance_denominator;
    header.initial_balanced_models = counts.initial_balanced_models;
    header.initial_unbalanced_models = counts.initial_unbalanced_models;
    header.sim_attempted = counts.sim_attempted;
    header.ic_attempted = counts.ic_attempted;
    header.ic_ok = counts.ic_ok;
    header.ic_solver_fail = counts.ic_solver_fail;
    header.sim_ok = counts.sim_ok;
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
        from_quality_gate_version: 1,
        to_quality_gate_version: 2,
        flatten_models_before: 565,
        flatten_models_after: 555,
        reattributed_error_code: "ER002".to_string(),
        reattributed_models: V2_REATTRIBUTED_MODELS.map(str::to_string).to_vec(),
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
    promoted.quality_gate_version = 1;
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.flatten_models = 555;
    checked_in.metric_schema_migration = Some(schema_migration());
    assert_eq!(
        choose_baseline(&promoted, &checked_in).expect("declared schema migration"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn metric_schema_migration_requires_exact_unique_cohort() {
    let mut baseline = header("a96aa1a-cmake");
    let mut migration = schema_migration();
    migration.reattributed_models.pop();
    baseline.metric_schema_migration = Some(migration);
    assert!(validate_metric_schema_migration(&baseline).is_err());

    let mut migration = schema_migration();
    migration.reattributed_models[9] = migration.reattributed_models[0].clone();
    baseline.metric_schema_migration = Some(migration);
    assert!(validate_metric_schema_migration(&baseline).is_err());

    let mut migration = schema_migration();
    migration.reattributed_models[9] = "Modelica.HandLowered.Substitute".to_string();
    baseline.metric_schema_migration = Some(migration);
    assert!(validate_metric_schema_migration(&baseline).is_err());
}

#[test]
fn metric_schema_migration_rejects_unrelated_cumulative_regression() {
    let mut promoted = header("a96aa1a-cmake");
    promoted.quality_gate_version = 1;
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.flatten_models = 555;
    checked_in.compiled_models -= 1;
    checked_in.metric_schema_migration = Some(schema_migration());

    let error =
        choose_baseline(&promoted, &checked_in).expect_err("unrelated regression must fail");
    assert!(error.to_string().contains("compiled models"), "{error}");
}

#[test]
fn metric_schema_migration_rejects_unrelated_headline_regression() {
    let mut promoted = header("a96aa1a-cmake");
    promoted.quality_gate_version = 1;
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.flatten_models = 555;
    checked_in.trace_accuracy_stats.agreement_high -= 1;
    checked_in.metric_schema_migration = Some(schema_migration());

    let error = choose_baseline(&promoted, &checked_in).expect_err("headline regression must fail");
    assert!(
        error.to_string().contains("high trace agreement"),
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
fn combined_schema_and_omc_migration_accepts_exact_flatten_correction() {
    let mut promoted = header("old");
    promoted.quality_gate_version = 1;
    let mut checked_in = header("new");
    checked_in.flatten_models = 555;
    checked_in.metric_schema_migration = Some(schema_migration());
    checked_in.omc_context_migration = Some(migration("old", "new"));
    checked_in.trace_accuracy_stats.agreement_high = 0;

    assert_eq!(
        choose_baseline(&promoted, &checked_in)
            .expect("reviewed schema correction and OMC context are valid"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn reviewed_checked_dae_contract_cutover_selects_checked_baseline() {
    let mut promoted = header("old");
    promoted.quality_gate_version = 1;
    promoted.flatten_models = 565;
    promoted.solve_models = 381;
    promoted.sim_attempted = 413;
    promoted.ic_attempted = 259;
    promoted.ic_ok = 239;
    promoted.ic_solver_fail = 20;
    promoted.sim_ok = 170;

    let mut checked_in = header("new");
    checked_in.git_commit = "3fc9a6cb9c60e1137eb6151f29cb87e9ad35064b".to_string();
    apply_stage_counts(&mut checked_in, &reviewed_contract_counts_after());
    checked_in.metric_schema_migration = Some(schema_migration());
    checked_in.compiler_contract_migration = Some(compiler_contract_migration());
    checked_in.omc_context_migration = Some(migration("old", "new"));

    assert_eq!(
        choose_baseline(&promoted, &checked_in).expect("reviewed contract cutover"),
        BaselineChoice::CheckedInMigration
    );

    checked_in
        .compiler_contract_migration
        .as_mut()
        .unwrap()
        .stage_counts_after
        .compiled_models += 1;
    let error = choose_baseline(&promoted, &checked_in)
        .expect_err("unreviewed contract count must fail closed");
    assert!(error.to_string().contains("stage counts"), "{error}");
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
            "quality_gate_version": 2,
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
