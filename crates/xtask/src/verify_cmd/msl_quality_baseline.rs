use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Deserializer};
use std::collections::BTreeMap;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

use super::VerifyMslParityArgs;

const MSL_QUALITY_BASELINE_ASSET_URL: &str = "https://github.com/CogniPilot/rumoca/releases/download/msl-quality-baseline/msl_quality_baseline.json";
const MSL_QUALITY_BASELINE_FALLBACK_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json";
const MSL_QUALITY_GATE_VERSION: u64 = 2;
const PREVIOUS_MSL_QUALITY_GATE_VERSION: u64 = 1;
const MSL_QUALITY_RUN_SCOPE: &str = "full";
const V2_FLATTEN_MODELS_BEFORE: usize = 565;
const V2_FLATTEN_MODELS_AFTER: usize = 555;
const V2_REATTRIBUTED_ERROR_CODE: &str = "ER002";
const V2_REATTRIBUTED_MODELS: [&str; 10] = [
    "Modelica.Fluid.Examples.AST_BatchPlant.BatchPlant_StandardWater",
    "Modelica.Fluid.Examples.AST_BatchPlant.Test.OneTank",
    "Modelica.Fluid.Examples.AST_BatchPlant.Test.TankWithEmptyingPipe1",
    "Modelica.Fluid.Examples.AST_BatchPlant.Test.TankWithEmptyingPipe2",
    "Modelica.Fluid.Examples.AST_BatchPlant.Test.TanksWithEmptyingPipe1",
    "Modelica.Fluid.Examples.AST_BatchPlant.Test.TanksWithEmptyingPipe2",
    "Modelica.Fluid.Examples.AST_BatchPlant.Test.TwoTanks",
    "Modelica.Fluid.Examples.Explanatory.MeasuringTemperature",
    "Modelica.Fluid.Examples.Explanatory.MomentumBalanceFittings",
    "Modelica.Fluid.Examples.InverseParameterization",
];
const CHECKED_DAE_CONTRACT_FROM: &str = "permissive-dae-v1";
const CHECKED_DAE_CONTRACT_TO: &str = "checked-dae-v1";
const CHECKED_DAE_EVIDENCE_COMMIT: &str = "3fc9a6cb9c60e1137eb6151f29cb87e9ad35064b";

#[derive(Debug, Clone, Deserialize)]
struct MslQualityBaselineHeader {
    quality_gate_version: u64,
    run_scope: String,
    git_commit: String,
    #[serde(deserialize_with = "deserialize_omc_version")]
    omc_version: String,
    sim_target_models: usize,
    #[serde(default)]
    omc_context_migration: Option<OmcContextMigration>,
    #[serde(default)]
    metric_schema_migration: Option<MetricSchemaMigration>,
    #[serde(default)]
    compiler_contract_migration: Option<CompilerContractMigration>,
    simulatable_attempted: usize,
    parse_models: usize,
    flatten_models: usize,
    dae_models: usize,
    compiled_models: usize,
    solve_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    partial_models: usize,
    balance_denominator: usize,
    initial_balanced_models: usize,
    initial_unbalanced_models: usize,
    sim_attempted: usize,
    ic_attempted: usize,
    ic_ok: usize,
    ic_solver_fail: usize,
    sim_ok: usize,
    runtime_ratio_stats: RuntimeRatioStats,
    trace_accuracy_stats: TraceAccuracyStats,
}

#[derive(Debug, Clone, Deserialize)]
struct OmcContextMigration {
    #[serde(deserialize_with = "deserialize_omc_version")]
    from_omc_version: String,
    #[serde(deserialize_with = "deserialize_omc_version")]
    to_omc_version: String,
    sim_target_models: usize,
}

#[derive(Debug, Clone, Deserialize)]
struct MetricSchemaMigration {
    from_quality_gate_version: u64,
    to_quality_gate_version: u64,
    flatten_models_before: usize,
    flatten_models_after: usize,
    reattributed_error_code: String,
    reattributed_models: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
struct CompilerContractMigration {
    from_contract: String,
    to_contract: String,
    evidence_git_commit: String,
    sim_target_models: usize,
    stage_counts_before: CompilerContractStageCounts,
    stage_counts_after: CompilerContractStageCounts,
    phase_failure_counts_after: BTreeMap<String, usize>,
    error_code_counts_after: BTreeMap<String, usize>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
struct CompilerContractStageCounts {
    parse_models: usize,
    flatten_models: usize,
    dae_models: usize,
    compiled_models: usize,
    solve_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    partial_models: usize,
    balance_denominator: usize,
    initial_balanced_models: usize,
    initial_unbalanced_models: usize,
    sim_attempted: usize,
    ic_attempted: usize,
    ic_ok: usize,
    ic_solver_fail: usize,
    sim_ok: usize,
}

#[derive(Debug, Clone, Deserialize)]
struct RuntimeRatioStats {
    system_ratio_both_success: DistributionMedian,
    wall_ratio_both_success: DistributionMedian,
}

#[derive(Debug, Clone, Deserialize)]
struct DistributionMedian {
    median: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct TraceAccuracyStats {
    models_compared: usize,
    agreement_high: usize,
    agreement_minor: usize,
    agreement_deviation: usize,
    bad_channels_total: usize,
    severe_channels_total: usize,
    models_with_severe_channel: usize,
    models_with_any_channel_deviation: usize,
    violation_mass_total: f64,
    initial_condition: InitialConditionStats,
    state_selection: StateSelectionStats,
}

#[derive(Debug, Clone, Deserialize)]
struct InitialConditionStats {
    deviation_channels_total: usize,
    severe_channels_total: usize,
    violation_mass_total: f64,
}

#[derive(Debug, Clone, Deserialize)]
struct StateSelectionStats {
    exact_state_set_match_models: usize,
    total_rumoca_only_states: usize,
    total_omc_only_states: usize,
}

fn deserialize_omc_version<'de, D>(deserializer: D) -> std::result::Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    let value = serde_json::Value::deserialize(deserializer)?;
    value
        .as_str()
        .map(str::trim)
        .filter(|version| !version.is_empty())
        .map(str::to_owned)
        .ok_or_else(|| serde::de::Error::custom("omc_version must be a non-empty string"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BaselineChoice {
    Promoted,
    CheckedInMigration,
}

pub(super) fn resolve_msl_quality_baseline(
    root: &Path,
    args: &VerifyMslParityArgs,
) -> Result<PathBuf> {
    if let Some(path) = args.quality_baseline.as_ref() {
        let resolved = resolve_workspace_path(root, path);
        ensure!(
            resolved.is_file(),
            "explicit MSL quality baseline not found: {}",
            resolved.display()
        );
        load_baseline_header(&resolved)?;
        println!(
            "MSL quality baseline: using explicit {}",
            resolved.display()
        );
        return Ok(resolved);
    }

    let checked_in = checked_in_msl_quality_baseline_path(root);
    ensure!(
        checked_in.is_file(),
        "checked-in MSL quality baseline not found: {}",
        checked_in.display()
    );
    let checked_in_header = load_baseline_header(&checked_in)?;
    if !args.no_remote_quality_baseline
        && let Some(promoted) = download_msl_quality_baseline_asset(root)?
    {
        let promoted_header = load_promoted_baseline_header(&promoted)?;
        match choose_baseline(&promoted_header, &checked_in_header)? {
            BaselineChoice::Promoted => return Ok(promoted),
            BaselineChoice::CheckedInMigration => {
                println!(
                    "MSL quality baseline: checked-in baseline declares a context/schema migration; using {}",
                    checked_in.display()
                );
                return Ok(checked_in);
            }
        }
    }

    if args.no_remote_quality_baseline {
        println!(
            "MSL quality baseline: using checked-in fallback because --no-remote-quality-baseline was set ({})",
            checked_in.display()
        );
    } else {
        println!(
            "MSL quality baseline: using checked-in fallback {}",
            checked_in.display()
        );
    }
    Ok(checked_in)
}

fn choose_baseline(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<BaselineChoice> {
    validate_context_migration(checked_in)?;
    validate_metric_schema_migration(checked_in)?;
    if promoted.quality_gate_version != checked_in.quality_gate_version {
        let Some(migration) = checked_in.metric_schema_migration.as_ref() else {
            bail!(
                "MSL quality schema differs without an explicit migration (promoted={}, checked-in={})",
                promoted.quality_gate_version,
                checked_in.quality_gate_version
            );
        };
        ensure!(
            migration.from_quality_gate_version == promoted.quality_gate_version
                && migration.to_quality_gate_version == checked_in.quality_gate_version,
            "MSL quality schema migration differs from baseline contexts (declared={} -> {}, actual={} -> {})",
            migration.from_quality_gate_version,
            migration.to_quality_gate_version,
            promoted.quality_gate_version,
            checked_in.quality_gate_version
        );
        ensure!(
            promoted.sim_target_models == checked_in.sim_target_models,
            "MSL quality schema migration target set differs (promoted={}, checked-in={})",
            promoted.sim_target_models,
            checked_in.sim_target_models
        );
        let omc_context_changed = promoted.omc_version != checked_in.omc_version;
        if omc_context_changed {
            let Some(omc_migration) = checked_in.omc_context_migration.as_ref() else {
                bail!(
                    "MSL quality schema and OMC contexts both differ, but no OMC migration is declared"
                );
            };
            ensure!(
                omc_migration.from_omc_version == promoted.omc_version,
                "MSL OMC context migration source differs (declared={}, promoted={})",
                omc_migration.from_omc_version,
                promoted.omc_version
            );
        }
        if checked_in.compiler_contract_migration.is_some() {
            validate_compiler_contract_migration(promoted, checked_in, omc_context_changed)?;
            return Ok(BaselineChoice::CheckedInMigration);
        }
        validate_migration_metric_integrity(promoted, checked_in, true, omc_context_changed)?;
        return Ok(BaselineChoice::CheckedInMigration);
    }
    if promoted.omc_version == checked_in.omc_version {
        return Ok(BaselineChoice::Promoted);
    }

    let Some(migration) = checked_in.omc_context_migration.as_ref() else {
        bail!(
            "MSL quality baseline OMC context differs without an explicit migration (promoted={}, checked-in={})",
            promoted.omc_version,
            checked_in.omc_version
        );
    };
    ensure!(
        migration.from_omc_version == promoted.omc_version,
        "MSL OMC context migration source differs (declared={}, promoted={})",
        migration.from_omc_version,
        promoted.omc_version
    );
    ensure!(
        promoted.sim_target_models == checked_in.sim_target_models,
        "MSL OMC context migration target set differs (promoted={}, checked-in={})",
        promoted.sim_target_models,
        checked_in.sim_target_models
    );
    validate_migration_metric_integrity(promoted, checked_in, false, true)?;
    Ok(BaselineChoice::CheckedInMigration)
}

fn validate_context_migration(baseline: &MslQualityBaselineHeader) -> Result<()> {
    let Some(migration) = baseline.omc_context_migration.as_ref() else {
        return Ok(());
    };
    ensure!(
        migration.from_omc_version != migration.to_omc_version,
        "MSL OMC context migration source and target must differ"
    );
    ensure!(
        migration.to_omc_version == baseline.omc_version,
        "MSL OMC context migration target differs (declared={}, baseline={})",
        migration.to_omc_version,
        baseline.omc_version
    );
    ensure!(
        migration.sim_target_models == baseline.sim_target_models,
        "MSL OMC context migration target set differs (declared={}, baseline={})",
        migration.sim_target_models,
        baseline.sim_target_models
    );
    Ok(())
}

fn validate_metric_schema_migration(baseline: &MslQualityBaselineHeader) -> Result<()> {
    let Some(migration) = baseline.metric_schema_migration.as_ref() else {
        return Ok(());
    };
    ensure!(
        migration.from_quality_gate_version == PREVIOUS_MSL_QUALITY_GATE_VERSION
            && migration.to_quality_gate_version == MSL_QUALITY_GATE_VERSION,
        "MSL metric schema migration must be the reviewed version-1 to version-2 correction"
    );
    ensure!(
        migration.to_quality_gate_version == baseline.quality_gate_version,
        "MSL metric schema migration target differs (declared={}, baseline={})",
        migration.to_quality_gate_version,
        baseline.quality_gate_version
    );
    ensure!(
        migration.flatten_models_before == V2_FLATTEN_MODELS_BEFORE
            && migration.flatten_models_after == V2_FLATTEN_MODELS_AFTER,
        "MSL metric schema migration flatten counts differ from the reviewed correction"
    );
    ensure!(
        migration.flatten_models_before - migration.flatten_models_after
            == migration.reattributed_models.len(),
        "MSL metric schema migration count delta does not match affected model set"
    );
    ensure!(
        migration.reattributed_error_code == V2_REATTRIBUTED_ERROR_CODE,
        "MSL metric schema migration diagnostic cohort differs from the reviewed correction"
    );
    let mut unique_models = migration.reattributed_models.clone();
    unique_models.sort();
    unique_models.dedup();
    ensure!(
        unique_models.len() == migration.reattributed_models.len(),
        "MSL metric schema migration model set contains duplicates"
    );
    let mut expected_models = V2_REATTRIBUTED_MODELS.map(str::to_string).to_vec();
    expected_models.sort();
    ensure!(
        unique_models == expected_models,
        "MSL metric schema migration model set differs from the reviewed correction"
    );
    Ok(())
}

fn validate_compiler_contract_migration(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
    omc_context_changed: bool,
) -> Result<()> {
    let migration = checked_in
        .compiler_contract_migration
        .as_ref()
        .expect("caller established compiler contract migration");
    ensure!(
        migration.from_contract == CHECKED_DAE_CONTRACT_FROM
            && migration.to_contract == CHECKED_DAE_CONTRACT_TO,
        "MSL compiler contract migration is not the reviewed checked-DAE cutover"
    );
    ensure!(
        migration.evidence_git_commit == CHECKED_DAE_EVIDENCE_COMMIT
            && checked_in.git_commit == CHECKED_DAE_EVIDENCE_COMMIT,
        "MSL compiler contract migration evidence commit differs from the reviewed full run"
    );
    ensure!(
        migration.sim_target_models == checked_in.sim_target_models,
        "MSL compiler contract migration target set differs"
    );
    ensure!(
        migration.stage_counts_before == reviewed_contract_counts_before()
            && migration.stage_counts_after == reviewed_contract_counts_after(),
        "MSL compiler contract migration stage counts differ from the reviewed cutover"
    );
    ensure!(
        migration.phase_failure_counts_after == reviewed_phase_failure_counts_after(),
        "MSL compiler contract migration failure census differs from the reviewed cutover"
    );
    ensure!(
        migration.error_code_counts_after == reviewed_error_code_counts_after(),
        "MSL compiler contract migration diagnostic census differs from the reviewed cutover"
    );
    ensure_contract_counts_match_header(&migration.stage_counts_after, checked_in)?;
    ensure_contract_source_did_not_regress(
        promoted,
        &migration.stage_counts_before,
        omc_context_changed,
    )?;
    let failed_models: usize = migration.phase_failure_counts_after.values().sum();
    ensure!(
        failed_models + migration.stage_counts_after.compiled_models == migration.sim_target_models,
        "MSL compiler contract migration failure census does not cover the fixed target set"
    );
    Ok(())
}

fn ensure_contract_counts_match_header(
    counts: &CompilerContractStageCounts,
    header: &MslQualityBaselineHeader,
) -> Result<()> {
    ensure!(
        counts == &contract_counts_from_header(header),
        "MSL compiler contract migration target counts do not match the checked-in baseline"
    );
    Ok(())
}

fn ensure_contract_source_did_not_regress(
    promoted: &MslQualityBaselineHeader,
    before: &CompilerContractStageCounts,
    omc_context_changed: bool,
) -> Result<()> {
    ensure!(
        promoted.parse_models <= before.parse_models
            && V2_FLATTEN_MODELS_AFTER == before.flatten_models
            && promoted.dae_models <= before.dae_models
            && promoted.compiled_models <= before.compiled_models
            && promoted.solve_models <= before.solve_models
            && promoted.balanced_models <= before.balanced_models
            && promoted.balance_denominator <= before.balance_denominator
            && promoted.initial_balanced_models <= before.initial_balanced_models
            && promoted.sim_attempted <= before.sim_attempted
            && promoted.ic_attempted <= before.ic_attempted
            && promoted.ic_ok <= before.ic_ok
            && promoted.sim_ok <= before.sim_ok,
        "MSL checked-DAE cutover source does not dominate the promoted cumulative baseline"
    );
    ensure!(
        promoted.partial_models >= before.partial_models
            && promoted.unbalanced_models >= before.unbalanced_models
            && promoted.initial_unbalanced_models >= before.initial_unbalanced_models
            && promoted.ic_solver_fail >= before.ic_solver_fail,
        "MSL checked-DAE cutover source regresses a lower-is-better baseline metric"
    );
    ensure!(
        omc_context_changed,
        "MSL checked-DAE cutover was reviewed together with its declared OMC context change"
    );
    Ok(())
}

fn contract_counts_from_header(header: &MslQualityBaselineHeader) -> CompilerContractStageCounts {
    CompilerContractStageCounts {
        parse_models: header.parse_models,
        flatten_models: header.flatten_models,
        dae_models: header.dae_models,
        compiled_models: header.compiled_models,
        solve_models: header.solve_models,
        balanced_models: header.balanced_models,
        unbalanced_models: header.unbalanced_models,
        partial_models: header.partial_models,
        balance_denominator: header.balance_denominator,
        initial_balanced_models: header.initial_balanced_models,
        initial_unbalanced_models: header.initial_unbalanced_models,
        sim_attempted: header.sim_attempted,
        ic_attempted: header.ic_attempted,
        ic_ok: header.ic_ok,
        ic_solver_fail: header.ic_solver_fail,
        sim_ok: header.sim_ok,
    }
}

fn reviewed_contract_counts_before() -> CompilerContractStageCounts {
    CompilerContractStageCounts {
        parse_models: 566,
        flatten_models: 555,
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
    }
}

fn reviewed_contract_counts_after() -> CompilerContractStageCounts {
    CompilerContractStageCounts {
        parse_models: 566,
        flatten_models: 444,
        dae_models: 228,
        compiled_models: 228,
        solve_models: 202,
        balanced_models: 217,
        unbalanced_models: 0,
        partial_models: 11,
        balance_denominator: 217,
        initial_balanced_models: 217,
        initial_unbalanced_models: 0,
        sim_attempted: 210,
        ic_attempted: 150,
        ic_ok: 146,
        ic_solver_fail: 4,
        sim_ok: 122,
    }
}

fn reviewed_phase_failure_counts_after() -> BTreeMap<String, usize> {
    [
        ("Flatten", 82),
        ("Instantiate", 9),
        ("Resolve", 25),
        ("ToDae", 216),
        ("Typecheck", 6),
    ]
    .into_iter()
    .map(|(phase, count)| (phase.to_string(), count))
    .collect()
}

fn reviewed_error_code_counts_after() -> BTreeMap<String, usize> {
    [
        ("ED001", 24),
        ("ED008", 7),
        ("ED009", 3),
        ("ED010", 14),
        ("ED013", 22),
        ("ED018", 29),
        ("ED019", 111),
        ("ED020", 1),
        ("ED021", 5),
        ("EF004", 24),
        ("EF005", 11),
        ("EF016", 16),
        ("EF020", 1),
        ("EF024", 16),
        ("EF025", 12),
        ("EI007", 2),
        ("EI012", 6),
        ("EI027", 1),
        ("EL005", 60),
        ("EMSL_TIMEOUT_MODEL_ATTEMPT", 11),
        ("ER066", 23),
        ("ER130", 2),
        ("ET000", 1),
        ("ET004", 4),
        ("EX001", 6),
        ("EX002", 13),
    ]
    .into_iter()
    .map(|(code, count)| (code.to_string(), count))
    .collect()
}

fn validate_migration_metric_integrity(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
    flatten_is_migrated: bool,
    omc_context_changed: bool,
) -> Result<()> {
    ensure!(
        promoted.simulatable_attempted == checked_in.simulatable_attempted,
        "MSL migration denominator changed (promoted={}, checked-in={})",
        promoted.simulatable_attempted,
        checked_in.simulatable_attempted
    );
    if flatten_is_migrated {
        let migration = checked_in
            .metric_schema_migration
            .as_ref()
            .expect("schema migration was established by choose_baseline");
        ensure!(
            promoted.flatten_models == migration.flatten_models_before
                && checked_in.flatten_models == migration.flatten_models_after,
            "MSL metric schema migration flatten counts do not match the compared baselines"
        );
    }

    let higher_is_better = [
        (
            "parse models",
            promoted.parse_models,
            checked_in.parse_models,
        ),
        ("DAE models", promoted.dae_models, checked_in.dae_models),
        (
            "compiled models",
            promoted.compiled_models,
            checked_in.compiled_models,
        ),
        (
            "solve models",
            promoted.solve_models,
            checked_in.solve_models,
        ),
        (
            "balanced models",
            promoted.balanced_models,
            checked_in.balanced_models,
        ),
        (
            "balance denominator",
            promoted.balance_denominator,
            checked_in.balance_denominator,
        ),
        (
            "initial balanced models",
            promoted.initial_balanced_models,
            checked_in.initial_balanced_models,
        ),
        (
            "simulation attempts",
            promoted.sim_attempted,
            checked_in.sim_attempted,
        ),
        (
            "initial-condition attempts",
            promoted.ic_attempted,
            checked_in.ic_attempted,
        ),
        ("initial-condition solves", promoted.ic_ok, checked_in.ic_ok),
        ("successful simulations", promoted.sim_ok, checked_in.sim_ok),
    ];
    for (label, promoted_value, checked_in_value) in higher_is_better {
        ensure_not_lowered(label, promoted_value, checked_in_value)?;
    }
    if !flatten_is_migrated {
        ensure_not_lowered(
            "flatten models",
            promoted.flatten_models,
            checked_in.flatten_models,
        )?;
    }

    for (label, promoted_value, checked_in_value) in [
        (
            "partial models",
            promoted.partial_models,
            checked_in.partial_models,
        ),
        (
            "unbalanced models",
            promoted.unbalanced_models,
            checked_in.unbalanced_models,
        ),
        (
            "initial unbalanced models",
            promoted.initial_unbalanced_models,
            checked_in.initial_unbalanced_models,
        ),
        (
            "initial-condition solver failures",
            promoted.ic_solver_fail,
            checked_in.ic_solver_fail,
        ),
    ] {
        ensure_not_raised(label, promoted_value, checked_in_value)?;
    }

    if !omc_context_changed {
        validate_omc_dependent_metric_integrity(promoted, checked_in)?;
    }
    Ok(())
}

fn validate_omc_dependent_metric_integrity(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<()> {
    let promoted_trace = &promoted.trace_accuracy_stats;
    let checked_trace = &checked_in.trace_accuracy_stats;
    for (label, promoted_value, checked_in_value) in [
        (
            "trace models compared",
            promoted_trace.models_compared,
            checked_trace.models_compared,
        ),
        (
            "high trace agreement",
            promoted_trace.agreement_high,
            checked_trace.agreement_high,
        ),
        (
            "state-set exact matches",
            promoted_trace.state_selection.exact_state_set_match_models,
            checked_trace.state_selection.exact_state_set_match_models,
        ),
    ] {
        ensure_not_lowered(label, promoted_value, checked_in_value)?;
    }
    let promoted_high_minor = promoted_trace
        .agreement_high
        .checked_add(promoted_trace.agreement_minor)
        .context("promoted high+minor trace agreement overflowed")?;
    let checked_high_minor = checked_trace
        .agreement_high
        .checked_add(checked_trace.agreement_minor)
        .context("checked-in high+minor trace agreement overflowed")?;
    ensure_not_lowered(
        "high+minor trace agreement",
        promoted_high_minor,
        checked_high_minor,
    )?;
    ensure_not_lowered(
        "trace models without severe channels",
        promoted_trace
            .models_compared
            .saturating_sub(promoted_trace.models_with_severe_channel),
        checked_trace
            .models_compared
            .saturating_sub(checked_trace.models_with_severe_channel),
    )?;
    validate_omc_error_metric_integrity(promoted_trace, checked_trace)?;
    validate_runtime_metric_integrity(promoted, checked_in)
}

fn validate_omc_error_metric_integrity(
    promoted_trace: &TraceAccuracyStats,
    checked_trace: &TraceAccuracyStats,
) -> Result<()> {
    for (label, promoted_value, checked_in_value) in [
        (
            "trace deviation models",
            promoted_trace.agreement_deviation,
            checked_trace.agreement_deviation,
        ),
        (
            "trace bad channels",
            promoted_trace.bad_channels_total,
            checked_trace.bad_channels_total,
        ),
        (
            "trace severe channels",
            promoted_trace.severe_channels_total,
            checked_trace.severe_channels_total,
        ),
        (
            "trace models with bad channels",
            promoted_trace.models_with_any_channel_deviation,
            checked_trace.models_with_any_channel_deviation,
        ),
        (
            "initial-condition deviation channels",
            promoted_trace.initial_condition.deviation_channels_total,
            checked_trace.initial_condition.deviation_channels_total,
        ),
        (
            "initial-condition severe channels",
            promoted_trace.initial_condition.severe_channels_total,
            checked_trace.initial_condition.severe_channels_total,
        ),
        (
            "state-set rumoca-only states",
            promoted_trace.state_selection.total_rumoca_only_states,
            checked_trace.state_selection.total_rumoca_only_states,
        ),
        (
            "state-set OMC-only states",
            promoted_trace.state_selection.total_omc_only_states,
            checked_trace.state_selection.total_omc_only_states,
        ),
    ] {
        ensure_not_raised(label, promoted_value, checked_in_value)?;
    }
    ensure_float_not_raised(
        "trace violation mass",
        promoted_trace.violation_mass_total,
        checked_trace.violation_mass_total,
    )?;
    ensure_float_not_raised(
        "initial-condition violation mass",
        promoted_trace.initial_condition.violation_mass_total,
        checked_trace.initial_condition.violation_mass_total,
    )
}

fn validate_runtime_metric_integrity(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<()> {
    ensure_runtime_speedup_not_regressed(
        "runtime system speedup median",
        promoted
            .runtime_ratio_stats
            .system_ratio_both_success
            .median,
        checked_in
            .runtime_ratio_stats
            .system_ratio_both_success
            .median,
    )?;
    ensure_runtime_speedup_not_regressed(
        "runtime wall speedup median",
        promoted.runtime_ratio_stats.wall_ratio_both_success.median,
        checked_in
            .runtime_ratio_stats
            .wall_ratio_both_success
            .median,
    )
}

fn ensure_not_lowered(label: &str, promoted: usize, checked_in: usize) -> Result<()> {
    ensure!(
        checked_in >= promoted,
        "MSL migration lowers unrelated {label} (promoted={promoted}, checked-in={checked_in})"
    );
    Ok(())
}

fn ensure_not_raised(label: &str, promoted: usize, checked_in: usize) -> Result<()> {
    ensure!(
        checked_in <= promoted,
        "MSL migration raises unrelated {label} (promoted={promoted}, checked-in={checked_in})"
    );
    Ok(())
}

fn ensure_float_not_raised(label: &str, promoted: f64, checked_in: f64) -> Result<()> {
    ensure!(
        checked_in <= promoted + 1.0e-9,
        "MSL migration raises unrelated {label} (promoted={promoted:.6e}, checked-in={checked_in:.6e})"
    );
    Ok(())
}

fn ensure_runtime_speedup_not_regressed(label: &str, promoted: f64, checked_in: f64) -> Result<()> {
    ensure!(
        checked_in >= promoted * 0.65,
        "MSL migration regresses unrelated {label} by more than 35% (promoted={promoted:.6e}, checked-in={checked_in:.6e})"
    );
    Ok(())
}

fn load_baseline_header(path: &Path) -> Result<MslQualityBaselineHeader> {
    load_baseline_header_with_previous(path, false)
}

fn load_promoted_baseline_header(path: &Path) -> Result<MslQualityBaselineHeader> {
    load_baseline_header_with_previous(path, true)
}

fn load_baseline_header_with_previous(
    path: &Path,
    allow_previous_version: bool,
) -> Result<MslQualityBaselineHeader> {
    let data = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    let baseline: MslQualityBaselineHeader = serde_json::from_slice(&data).map_err(|error| {
        anyhow::anyhow!(
            "invalid MSL quality baseline JSON in {}: {error}",
            path.display()
        )
    })?;
    let version_supported = baseline.quality_gate_version == MSL_QUALITY_GATE_VERSION
        || (allow_previous_version
            && baseline.quality_gate_version == PREVIOUS_MSL_QUALITY_GATE_VERSION);
    ensure!(
        version_supported,
        "unsupported MSL quality_gate_version={} in {}",
        baseline.quality_gate_version,
        path.display()
    );
    ensure!(
        baseline.run_scope == MSL_QUALITY_RUN_SCOPE,
        "MSL quality baseline run_scope must be '{}' in {}",
        MSL_QUALITY_RUN_SCOPE,
        path.display()
    );
    ensure!(
        baseline.sim_target_models > 0,
        "MSL quality baseline sim_target_models must be positive in {}",
        path.display()
    );
    validate_context_migration(&baseline)
        .with_context(|| format!("invalid OMC context migration in {}", path.display()))?;
    validate_metric_schema_migration(&baseline)
        .with_context(|| format!("invalid metric schema migration in {}", path.display()))?;
    Ok(baseline)
}

fn downloaded_msl_quality_baseline_path(root: &Path) -> PathBuf {
    root.join("target/msl/baselines/msl_quality_baseline.json")
}

fn checked_in_msl_quality_baseline_path(root: &Path) -> PathBuf {
    root.join(MSL_QUALITY_BASELINE_FALLBACK_REL)
}

fn resolve_workspace_path(root: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        root.join(path)
    }
}

fn download_msl_quality_baseline_asset(root: &Path) -> Result<Option<PathBuf>> {
    let output_path = downloaded_msl_quality_baseline_path(root);
    println!(
        "MSL quality baseline: downloading latest promoted asset from {}",
        MSL_QUALITY_BASELINE_ASSET_URL
    );
    let response = match ureq::get(MSL_QUALITY_BASELINE_ASSET_URL).call() {
        Ok(response) => response,
        Err(error) => {
            eprintln!(
                "MSL quality baseline: failed to download latest promoted asset ({error}); falling back to checked-in baseline."
            );
            return Ok(None);
        }
    };

    let content_len = response
        .header("content-length")
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(0);
    let mut data = Vec::with_capacity(content_len);
    if let Err(error) = response.into_reader().read_to_end(&mut data) {
        eprintln!(
            "MSL quality baseline: failed to read latest promoted asset ({error}); falling back to checked-in baseline."
        );
        return Ok(None);
    }

    serde_json::from_slice::<serde_json::Value>(&data).with_context(|| {
        format!(
            "downloaded promoted MSL quality baseline from {MSL_QUALITY_BASELINE_ASSET_URL} is not valid JSON"
        )
    })?;

    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&output_path, data)
        .with_context(|| format!("failed to write {}", output_path.display()))?;
    println!(
        "MSL quality baseline: downloaded promoted asset {}",
        output_path.display()
    );
    Ok(Some(output_path))
}

#[cfg(test)]
#[path = "msl_quality_baseline_tests.rs"]
mod tests;
