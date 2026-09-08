use super::{
    super::VerifyMslParityArgs, BaselineChoice, CompilerContractMigrationHeader,
    DistributionMedian, InitialConditionStats, MetricSchemaMigration, MslQualityBaselineHeader,
    OmcContextMigration, PartialClassificationMigration, PromotedBaselineBridge, RuntimeRatioStats,
    StateSelectionStats, TraceAccuracyStats, choose_baseline, load_baseline_header,
    reviewed_affected_partial_models, reviewed_partial_model_names, validate_context_migration,
    validate_metric_schema_migration, validate_partial_classification_migration,
    validate_promoted_baseline_bridge,
};
use serde_json::json;
use std::{fs, path::PathBuf};

fn header(omc_version: &str) -> MslQualityBaselineHeader {
    MslQualityBaselineHeader {
        quality_gate_version: 4,
        document_sha256: String::new(),
        run_scope: "full".to_string(),
        git_commit: "fixture".to_string(),
        omc_version: omc_version.to_string(),
        sim_target_models: 566,
        omc_context_migration: None,
        metric_schema_migration: None,
        partial_classification_migration: Some(partial_classification_migration()),
        compiler_contract_migration: None,
        promoted_baseline_bridge: None,
        simulatable_attempted: 566,
        parse_models: 566,
        flatten_models: 565,
        dae_models: 545,
        compiled_models: 545,
        solve_models: 446,
        balanced_models: 532,
        unbalanced_models: 0,
        partial_models: 13,
        partial_model_names: reviewed_partial_model_names(),
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

fn promoted_bridge() -> PromotedBaselineBridge {
    PromotedBaselineBridge {
        from_quality_gate_version: 1,
        from_git_commit: "08fac54846fd73a3471bafc6609a0d34e74f9fe3".to_string(),
        from_sha256: "2b0a478b922583106342272bbf85411c539d50d9b9e0ca54c4dbb87621f05fe8".to_string(),
        to_quality_gate_version: 4,
        sim_target_models: 566,
        evidence_git_commits: [
            "a499eb8f15bf6af9d28f5a7011e82edfe73803b4",
            "3fc9a6cb9c60e1137eb6151f29cb87e9ad35064b",
            "6d57e9644b4da542a5498ee42510551e7e7ade70",
            "5394156facb1e5ff9f099f21c0e833c4870c506f",
        ]
        .map(str::to_string)
        .to_vec(),
    }
}

fn compiler_contract_migration() -> CompilerContractMigrationHeader {
    CompilerContractMigrationHeader {
        from_contract: "permissive-dae-v1".to_string(),
        to_contract: "checked-dae-v1".to_string(),
        evidence_git_commit: "3fc9a6cb9c60e1137eb6151f29cb87e9ad35064b".to_string(),
        sim_target_models: 566,
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

fn partial_classification_migration() -> PartialClassificationMigration {
    PartialClassificationMigration {
        from_quality_gate_version: 3,
        to_quality_gate_version: 4,
        change: "source-static-partial-cohort-v1".to_string(),
        evidence_git_commit: "5394156facb1e5ff9f099f21c0e833c4870c506f".to_string(),
        sim_target_models: 566,
        partial_models_before: 11,
        partial_models_after: 13,
        affected_diagnostic_cohort: "failed-before-success-with-null-partial-classification"
            .to_string(),
        affected_models: reviewed_affected_partial_models(),
        partial_model_names_after: reviewed_partial_model_names(),
    }
}

fn promoted_v3(omc_version: &str) -> MslQualityBaselineHeader {
    let mut promoted = header(omc_version);
    promoted.quality_gate_version = 3;
    promoted.partial_models = 11;
    promoted.partial_model_names.clear();
    promoted.partial_classification_migration = None;
    promoted
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
fn checked_in_v4_baseline_loads_with_both_reviewed_migrations() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json");
    let baseline = load_baseline_header(&path).expect("checked v4 baseline must validate");

    assert_eq!(baseline.quality_gate_version, 4);
    assert_eq!(baseline.partial_models, 13);
    assert_eq!(baseline.partial_model_names, reviewed_partial_model_names());
    let historical = baseline
        .metric_schema_migration
        .expect("historical v2-to-v3 migration remains recorded");
    assert_eq!(historical.from_quality_gate_version, 2);
    assert_eq!(historical.to_quality_gate_version, 3);
    let partial = baseline
        .partial_classification_migration
        .expect("v3-to-v4 migration is recorded");
    assert_eq!(
        partial.evidence_git_commit,
        partial_classification_migration().evidence_git_commit
    );
}

#[test]
fn newer_checked_in_partial_schema_precedes_promoted_baseline() {
    let promoted = promoted_v3("a96aa1a-cmake");
    let checked_in = header("a96aa1a-cmake");
    assert_eq!(
        choose_baseline(&promoted, &checked_in).expect("declared schema migration"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn exact_promoted_v1_asset_uses_reviewed_migration_lineage() {
    let mut promoted = header("OpenModelica 1.27.0~dev.beta.3");
    promoted.quality_gate_version = 1;
    promoted.partial_classification_migration = None;
    promoted.partial_model_names.clear();
    promoted.git_commit = "08fac54846fd73a3471bafc6609a0d34e74f9fe3".to_string();
    promoted.document_sha256 =
        "2b0a478b922583106342272bbf85411c539d50d9b9e0ca54c4dbb87621f05fe8".to_string();

    let mut checked_in = header("a96aa1a-cmake");
    checked_in.omc_context_migration =
        Some(migration("OpenModelica 1.27.0~dev.beta.3", "a96aa1a-cmake"));
    checked_in.metric_schema_migration = Some(schema_migration());
    checked_in.compiler_contract_migration = Some(compiler_contract_migration());
    checked_in.promoted_baseline_bridge = Some(promoted_bridge());

    assert_eq!(
        choose_baseline(&promoted, &checked_in).expect("reviewed lineage should select"),
        BaselineChoice::CheckedInMigration
    );
}

#[test]
fn promoted_baseline_bridge_rejects_any_other_old_asset() {
    let mut promoted = header("OpenModelica 1.27.0~dev.beta.3");
    promoted.quality_gate_version = 1;
    promoted.partial_classification_migration = None;
    promoted.partial_model_names.clear();
    promoted.git_commit = "08fac54846fd73a3471bafc6609a0d34e74f9fe3".to_string();
    promoted.document_sha256 = "different".to_string();

    let mut checked_in = header("a96aa1a-cmake");
    checked_in.omc_context_migration =
        Some(migration("OpenModelica 1.27.0~dev.beta.3", "a96aa1a-cmake"));
    checked_in.metric_schema_migration = Some(schema_migration());
    checked_in.compiler_contract_migration = Some(compiler_contract_migration());
    checked_in.promoted_baseline_bridge = Some(promoted_bridge());

    let error = choose_baseline(&promoted, &checked_in)
        .expect_err("an unreviewed old baseline must fail closed");
    assert!(error.to_string().contains("digest differs"), "{error}");
}

#[test]
fn promoted_baseline_bridge_requires_the_ordered_evidence_chain() {
    let mut baseline = header("a96aa1a-cmake");
    baseline.metric_schema_migration = Some(schema_migration());
    baseline.compiler_contract_migration = Some(compiler_contract_migration());
    let mut bridge = promoted_bridge();
    bridge.evidence_git_commits.swap(0, 1);
    baseline.promoted_baseline_bridge = Some(bridge);

    let error = validate_promoted_baseline_bridge(&baseline)
        .expect_err("reordered evidence must fail closed");
    assert!(error.to_string().contains("evidence chain"), "{error}");
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
fn partial_classification_migration_requires_exact_cohort_and_roster() {
    let mut baseline = header("a96aa1a-cmake");
    baseline
        .partial_classification_migration
        .as_mut()
        .expect("fixture has migration")
        .affected_models
        .remove("Modelica.Electrical.PowerConverters.Examples.ACAC.ExampleTemplates.Dimmer");
    assert!(validate_partial_classification_migration(&baseline).is_err());

    baseline.partial_classification_migration = Some(partial_classification_migration());
    baseline
        .partial_model_names
        .remove("Modelica.Electrical.PowerConverters.Examples.ACAC.ExampleTemplates.Dimmer");
    assert!(validate_partial_classification_migration(&baseline).is_err());

    baseline.partial_model_names = reviewed_partial_model_names();
    baseline
        .partial_classification_migration
        .as_mut()
        .expect("fixture has migration")
        .evidence_git_commit = "unreviewed".to_string();
    assert!(validate_partial_classification_migration(&baseline).is_err());
}

#[test]
fn partial_schema_migration_rejects_unrelated_cumulative_regression() {
    let promoted = promoted_v3("a96aa1a-cmake");
    let mut checked_in = header("a96aa1a-cmake");
    checked_in.compiled_models -= 1;

    let error =
        choose_baseline(&promoted, &checked_in).expect_err("unrelated regression must fail");
    assert!(error.to_string().contains("compiled models"), "{error}");
}

#[test]
fn partial_schema_migration_rejects_unreviewed_partial_count() {
    let mut promoted = promoted_v3("a96aa1a-cmake");
    promoted.partial_models = 10;
    let checked_in = header("a96aa1a-cmake");

    let error =
        choose_baseline(&promoted, &checked_in).expect_err("unreviewed partial count must fail");
    assert!(
        error
            .to_string()
            .contains("partial-classification migration counts"),
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
fn combined_partial_schema_and_omc_migration_is_accepted() {
    let promoted = promoted_v3("old");
    let mut checked_in = header("new");
    checked_in.omc_context_migration = Some(migration("old", "new"));

    assert_eq!(
        choose_baseline(&promoted, &checked_in)
            .expect("reviewed partial correction and OMC context are valid"),
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
            "quality_gate_version": 4,
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

#[test]
fn baseline_header_rejects_an_omitted_required_provenance_key() {
    let source = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json");
    let data = fs::read(&source).expect("checked-in baseline should exist");
    let mut wire: serde_json::Value = serde_json::from_slice(&data).expect("valid baseline");
    wire.as_object_mut()
        .expect("baseline is an object")
        .remove("git_commit");
    let temp = tempfile::tempdir().expect("temporary directory should be available");
    let path = temp.path().join("missing-git-commit.json");
    fs::write(&path, wire.to_string()).expect("write mutated baseline");
    assert!(
        load_baseline_header(&path).is_err(),
        "missing git_commit must reject the evidence instead of inventing an empty identity"
    );
}
