mod cache;
mod compiler_contract_migration;
mod parity_measurement;
mod reference_stage;
mod runtime_cohort;
mod status;
#[cfg(test)]
mod tests;

use super::*;
use cache::*;
use compiler_contract_migration::*;
use indexmap::{IndexMap, IndexSet};
pub(super) use parity_measurement::*;
pub(super) use reference_stage::*;
use runtime_cohort::*;
use status::*;

// =============================================================================
// MSL quality gate (compile/balance strict + simulation tolerant gate)
// =============================================================================

pub(super) const SIM_RATE_GATE_EPSILON: f64 = 1.0e-12;
/// Transitional structural floor for the default baseline simulation run.
///
/// This is intentionally much looser than the baseline delta gate. Its job is to
/// reject obviously invalid runs before we start comparing finer-grained
/// regressions. It counts models in the OMC comparator's STRICT-HIGH agreement
/// band, not `sim_ok`: `agreement_high <= models_compared <= sim_ok`, so this is
/// strictly tighter than the `sim_ok` floor it replaced, and unlike that floor
/// it cannot be evaluated before the comparator has run.
pub(super) const DEFAULT_STRICT_HIGH_HARD_FLOOR_RATIO: f64 = 0.15;
/// Balance-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const BALANCE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Initial-balance-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const INITIAL_BALANCE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Allowed one-model cumulative-stage jitter for full-library MSL quality runs.
pub(super) const MSL_STAGE_COUNT_ALLOWED_DROP: usize = 1;
/// Solve lowering runs under a per-model wall-clock budget, so its pass count
/// moves with runner load (unlike the deterministic compile-side stages).
/// Allow more jitter before treating a drop as a regression.
pub(super) const MSL_SOLVE_STAGE_COUNT_ALLOWED_DROP: usize = 3;
/// Keep focused/unit gate checks strict; runner jitter allowance is only for
/// full-library scale denominators.
pub(super) const MSL_STAGE_COUNT_ALLOWED_DROP_MIN_DENOMINATOR: usize = 100;
/// Allowed drop in compared trace-model count from baseline.
pub(super) const TRACE_MODELS_COMPARED_ALLOWED_DROP: usize = 2;
/// Allowed relative drop in runtime speedup median (omc/rumoca) before failing.
pub(super) const RUNTIME_RATIO_MEDIAN_REL_TOLERANCE: f64 = 0.35;
/// OMC per-model timeout budget for simulation reference generation. OMC's
/// `simulate()` generates C code and invokes gcc per model, which on shared CI
/// runners far exceeds rumoca's warm per-model budget; giving OMC the same tiny
/// budget kills every model mid-build. The reference generator caps OMC, not
/// rumoca, so a larger budget only lets more OMC models complete (the measured
/// `timeSimulation` it reports is unchanged, so the timing comparison stays fair).
pub(super) const OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS: u64 = 120;
/// Ratio of the rumoca solver budget the OMC reference is given. The default
/// 120s vs 10s ratio covers OMC's C codegen + gcc invocation; keeping it
/// proportional means a long-budget lane that raises the rumoca budget does not
/// silently starve the OMC side and turn real deviations into missing baselines.
const OMC_SIM_REFERENCE_BUDGET_RATIO: u64 = 12;

/// OMC per-model budget, scaled with the (possibly raised) rumoca budget.
pub(super) fn omc_sim_reference_timeout_secs() -> u64 {
    let scaled =
        (sim_timeout_secs().ceil().max(0.0) as u64).saturating_mul(OMC_SIM_REFERENCE_BUDGET_RATIO);
    OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS.max(scaled)
}
/// Force low-impact OpenMP/BLAS threading in OMC child processes.
pub(super) const OMC_PARITY_THREADS_DEFAULT: usize = 1;
/// Version 3 makes reviewed pointwise-oracle boundaries explicit and excludes
/// them from the strict-high denominator while requiring every `sim_ok` model
/// to remain classified.
pub(super) const MSL_QUALITY_GATE_VERSION: u32 = 3;
pub(super) const MSL_QUALITY_RUN_SCOPE_FULL: &str = "full";
pub(super) const MSL_QUALITY_RUN_SCOPE_PARTIAL: &str = "partial";
pub(super) const MSL_QUALITY_BASELINE_FILE_REL: &str = "tests/msl_tests/msl_quality_baseline.json";
pub(super) const MSL_QUALITY_CURRENT_FILE_REL: &str = "msl_quality_current.json";
pub(super) const MSL_SIM_TARGETS_FILE_REL: &str = "msl_simulation_targets.json";
pub(super) const OMC_PARITY_CACHE_DIR_REL: &str = "omc_parity_cache";
pub(super) const OMC_SIM_REFERENCE_FILE_REL: &str = "omc_simulation_reference.json";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslDistributionStats {
    sample_count: usize,
    min: f64,
    median: f64,
    mean: f64,
    max: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslRuntimeRatioStatsBaseline {
    system_ratio_both_success: MslDistributionStats,
    wall_ratio_both_success: MslDistributionStats,
}

#[derive(Debug, Clone)]
pub(super) struct MslRuntimeModelRatio {
    system: f64,
    wall: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslTraceAccuracyStatsBaseline {
    models_compared: usize,
    missing_trace_models: usize,
    skipped_models: usize,
    #[serde(default)]
    policy_excluded_models: usize,
    #[serde(default)]
    trace_nonidentifiable_models: usize,
    agreement_high: usize,
    #[serde(default)]
    agreement_high_percent: Option<f64>,
    #[serde(default, alias = "agreement_near")]
    agreement_minor: usize,
    #[serde(default, alias = "agreement_near_percent")]
    agreement_minor_percent: Option<f64>,
    agreement_deviation: usize,
    #[serde(default)]
    agreement_deviation_percent: Option<f64>,
    #[serde(default)]
    total_channels_compared: Option<usize>,
    #[serde(default)]
    bad_channels_total: Option<usize>,
    #[serde(default)]
    severe_channels_total: Option<usize>,
    #[serde(default)]
    bad_channels_percent: Option<f64>,
    #[serde(default)]
    severe_channels_percent: Option<f64>,
    #[serde(default)]
    violation_mass_total: Option<f64>,
    #[serde(default)]
    violation_mass_mean_per_model: Option<f64>,
    #[serde(default)]
    violation_mass_mean_per_channel: Option<f64>,
    #[serde(default)]
    models_with_bad_channel: Option<usize>,
    #[serde(default)]
    models_with_severe_channel: Option<usize>,
    #[serde(default)]
    models_with_any_channel_deviation: Option<usize>,
    #[serde(default)]
    models_with_any_channel_deviation_percent: Option<f64>,
    #[serde(default)]
    max_model_channel_deviation_percent: Option<f64>,
    #[serde(default)]
    bounded_normalized_l1: Option<MslDistributionStats>,
    #[serde(default)]
    mean_model_mean_channel_bounded_normalized_l1: Option<f64>,
    #[serde(default)]
    max_model_max_channel_bounded_normalized_l1: Option<f64>,
    #[serde(default)]
    model_mean_channel_bounded_normalized_l1: Option<MslDistributionStats>,
    #[serde(default)]
    model_max_channel_bounded_normalized_l1: Option<MslDistributionStats>,
    #[serde(default)]
    initial_condition: Option<MslInitialConditionStatsBaseline>,
    #[serde(default)]
    state_selection: Option<MslStateSelectionStatsBaseline>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslInitialConditionStatsBaseline {
    models_compared: usize,
    models_with_accurate_initial_conditions: usize,
    models_with_initial_condition_deviation: usize,
    models_with_unmeasured_initial_conditions: usize,
    accurate_initial_conditions_percent: Option<f64>,
    models_with_initial_condition_deviation_percent: Option<f64>,
    models_with_unmeasured_initial_conditions_percent: Option<f64>,
    total_channels_compared: usize,
    high_channels_total: usize,
    near_channels_total: usize,
    deviation_channels_total: usize,
    severe_channels_total: usize,
    high_channels_percent: Option<f64>,
    near_channels_percent: Option<f64>,
    deviation_channels_percent: Option<f64>,
    severe_channels_percent: Option<f64>,
    violation_mass_total: Option<f64>,
    violation_mass_mean_per_model: Option<f64>,
    violation_mass_mean_per_channel: Option<f64>,
    mean_channel_bounded_normalized_error: Option<f64>,
    max_channel_bounded_normalized_error: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslStateSelectionStatsBaseline {
    models_compared: usize,
    exact_state_set_match_models: usize,
    state_count_match_models: usize,
    exact_state_set_match_percent: Option<f64>,
    state_count_match_percent: Option<f64>,
    total_rumoca_states: usize,
    total_omc_states: usize,
    total_matching_states: usize,
    total_rumoca_only_states: usize,
    total_omc_only_states: usize,
    max_model_state_set_difference: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslTensorPreservationBaseline {
    models_reported: usize,
    family_bodies: usize,
    preserved_family_bodies: usize,
    scalarized_family_rows: usize,
    report_errors: usize,
    preservation_percent: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslMetricSchemaMigration {
    from_quality_gate_version: u32,
    to_quality_gate_version: u32,
    change: String,
    strict_high_before: usize,
    strict_high_after: usize,
    policy_excluded_after: usize,
    excluded_strict_high_before: usize,
    excluded_non_high_before: usize,
    exclusions_file: String,
    exclusions_sha256: String,
}

fn quality_gate_v3_metric_schema_migration() -> MslMetricSchemaMigration {
    MslMetricSchemaMigration {
        from_quality_gate_version: 2,
        to_quality_gate_version: MSL_QUALITY_GATE_VERSION,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslQualityBaseline {
    quality_gate_version: u32,
    #[serde(default = "default_msl_quality_run_scope")]
    run_scope: String,
    git_commit: String,
    msl_version: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    omc_version: Option<String>,
    sim_timeout_seconds: f64,
    simulatable_attempted: usize,
    #[serde(default)]
    parse_models: usize,
    #[serde(default)]
    flatten_models: usize,
    #[serde(default)]
    dae_models: usize,
    compiled_models: usize,
    #[serde(default)]
    solve_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    partial_models: usize,
    balance_denominator: usize,
    initial_balanced_models: usize,
    initial_unbalanced_models: usize,
    sim_target_models: usize,
    sim_attempted: usize,
    #[serde(default)]
    ic_attempted: usize,
    #[serde(default)]
    ic_ok: usize,
    #[serde(default)]
    ic_solver_fail: usize,
    sim_ok: usize,
    sim_success_rate: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    runtime_context: Option<MslParityRuntimeContext>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    runtime_ratio_stats: Option<MslRuntimeRatioStatsBaseline>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    runtime_ratio_cohort_models: Option<IndexSet<String>>,
    #[serde(default)]
    trace_accuracy_stats: Option<MslTraceAccuracyStatsBaseline>,
    tensor_preservation: MslTensorPreservationBaseline,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    metric_schema_migration: Option<MslMetricSchemaMigration>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    compiler_contract_migration: Option<MslCompilerContractMigration>,
}

fn default_msl_quality_run_scope() -> String {
    MSL_QUALITY_RUN_SCOPE_FULL.to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub(super) struct MslParityRuntimeContext {
    #[serde(default)]
    workers_used: Option<usize>,
    #[serde(default)]
    omc_threads: Option<usize>,
}

#[derive(Debug, Clone)]
pub(super) struct MslParityGateInput {
    total_models: Option<usize>,
    omc_version: Option<String>,
    runtime_context: Option<MslParityRuntimeContext>,
    runtime_ratio_stats: Option<MslRuntimeRatioStatsBaseline>,
    runtime_model_ratios: IndexMap<String, MslRuntimeModelRatio>,
    trace_accuracy_stats: Option<MslTraceAccuracyStatsBaseline>,
    omc_assertion_failure_models: usize,
    omc_assertion_failure_examples: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct MslQualityGateInput<'a> {
    msl_version: &'a str,
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
    sim_target_models: usize,
    sim_attempted: usize,
    ic_attempted: usize,
    ic_ok: usize,
    ic_solver_fail: usize,
    sim_ok: usize,
    tensor_models_reported: usize,
    tensor_family_bodies: usize,
    tensor_preserved_family_bodies: usize,
    tensor_scalarized_family_rows: usize,
    tensor_report_errors: usize,
}

/// Compile phases in pipeline order, with `Success` ranking after all of them.
///
/// `MslModelResult::phase_reached` names the phase that *failed* (or `Success`),
/// so this is the only honest way to answer "did this model get past phase P?".
/// Keep it in step with the dispatch in `summarize_msl_results`.
const COMPILE_PHASE_ORDER: &[&str] = &[
    "Parse",
    "Resolve",
    "NeedsInner",
    "Instantiate",
    "Typecheck",
    "Flatten",
    "ToDae",
    "Success",
];

/// True when `phase_reached` shows the model got *past* `phase`.
///
/// The stage floors (`flatten_models`, `dae_models`) are cumulative pass counts,
/// so a model counts towards the Flatten floor only if it completed flattening —
/// i.e. it failed strictly later, or succeeded outright.
///
/// This used to be spelled as the literal set `{"ToDae", "Success"}`. That set
/// was written when the worker attributed *every* marker-free compile summary to
/// `ToDae`, which silently swept parse/resolve failures into the "flattened"
/// cohort. Now that the worker reports the real failing phase, the set has to be
/// derived from the phase order instead of restated, or a newly-correct
/// attribution reads as a compiler regression. An unknown phase string ranks
/// before everything and therefore never counts towards a floor.
fn completed_compile_phase(phase_reached: &str, phase: &str) -> bool {
    let rank = |name: &str| COMPILE_PHASE_ORDER.iter().position(|entry| *entry == name);
    match (rank(phase_reached), rank(phase)) {
        (Some(reached), Some(target)) => reached > target,
        _ => false,
    }
}

impl<'a> From<&'a MslSummary> for MslQualityGateInput<'a> {
    fn from(summary: &'a MslSummary) -> Self {
        let simulatable_attempted = summary.compiled_models
            + summary.resolve_failed
            + summary.instantiate_failed
            + summary.typecheck_failed
            + summary.flatten_failed
            + summary.todae_failed;
        let balance_denominator = summary
            .compiled_models
            .saturating_sub(summary.partial_models);
        let parse_models = simulatable_attempted;
        let flatten_models = summary
            .model_results
            .iter()
            .filter(|result| completed_compile_phase(&result.phase_reached, "Flatten"))
            .count();
        let dae_models = summary
            .model_results
            .iter()
            .filter(|result| completed_compile_phase(&result.phase_reached, "ToDae"))
            .count();
        let solve_models = summary
            .model_results
            .iter()
            .filter(|result| result.ir_solve_file.is_some() || result.ir_solve_seconds.is_some())
            .count();
        Self {
            msl_version: &summary.msl_version,
            simulatable_attempted,
            parse_models,
            flatten_models,
            dae_models,
            compiled_models: summary.compiled_models,
            solve_models,
            balanced_models: summary.balanced_models,
            unbalanced_models: summary.unbalanced_models,
            partial_models: summary.partial_models,
            balance_denominator,
            initial_balanced_models: summary.initial_balanced_models,
            initial_unbalanced_models: summary.initial_unbalanced_models,
            sim_target_models: summary.sim_target_models.len(),
            sim_attempted: summary.sim_attempted,
            ic_attempted: summary.ic_attempted,
            ic_ok: summary.ic_ok,
            ic_solver_fail: summary.ic_solver_fail,
            sim_ok: summary.sim_ok,
            tensor_models_reported: summary.tensor_models_reported,
            tensor_family_bodies: summary.tensor_family_bodies,
            tensor_preserved_family_bodies: summary.tensor_preserved_family_bodies,
            tensor_scalarized_family_rows: summary.tensor_scalarized_family_rows,
            tensor_report_errors: summary.tensor_report_errors,
        }
    }
}

pub(super) fn sim_success_rate(sim_ok: usize, sim_attempted: usize) -> Option<f64> {
    if sim_attempted == 0 {
        return None;
    }
    Some(sim_ok as f64 / sim_attempted as f64)
}

fn tensor_preservation_percent(preserved: usize, total: usize) -> Option<f64> {
    (total != 0).then(|| 100.0 * preserved as f64 / total as f64)
}

pub(super) fn compile_success_rate(
    compiled_models: usize,
    simulatable_attempted: usize,
) -> Option<f64> {
    if simulatable_attempted == 0 {
        return None;
    }
    Some(compiled_models as f64 / simulatable_attempted as f64)
}

pub(super) fn balance_success_rate(
    balanced_models: usize,
    balance_denominator: usize,
) -> Option<f64> {
    if balance_denominator == 0 {
        return None;
    }
    Some(balanced_models as f64 / balance_denominator as f64)
}

pub(super) fn msl_quality_baseline_path() -> PathBuf {
    if let Some(path) = quality_baseline_file_override() {
        return path;
    }
    Path::new(env!("CARGO_MANIFEST_DIR")).join(MSL_QUALITY_BASELINE_FILE_REL)
}

pub(super) fn msl_quality_current_path() -> PathBuf {
    msl_results_dir().join(MSL_QUALITY_CURRENT_FILE_REL)
}

pub(super) fn msl_simulation_targets_path() -> PathBuf {
    msl_results_dir().join(MSL_SIM_TARGETS_FILE_REL)
}

pub(super) fn omc_parity_cache_dir() -> PathBuf {
    // Results directories are intentionally disposable and often unique per
    // diagnostic run. Keep the content-keyed OMC cache at the relocatable MSL
    // cache root so a fresh results directory can reuse the same verified
    // reference without coupling it to a machine-specific path.
    msl_cache_dir().join(OMC_PARITY_CACHE_DIR_REL)
}

pub(super) fn load_msl_quality_baseline(path: &Path) -> io::Result<MslQualityBaseline> {
    let file = File::open(path)?;
    serde_json::from_reader(file)
        .map_err(|error| io::Error::other(format!("invalid MSL quality baseline JSON: {error}")))
}

pub(super) fn omc_simulation_reference_path() -> PathBuf {
    msl_results_dir().join(OMC_SIM_REFERENCE_FILE_REL)
}

pub(super) fn json_usize_field(root: &serde_json::Value, key: &str) -> Option<usize> {
    root.get(key)?
        .as_u64()
        .and_then(|value| usize::try_from(value).ok())
}

pub(super) fn json_f64_field(root: &serde_json::Value, key: &str) -> Option<f64> {
    root.get(key)?.as_f64()
}

fn json_f64_field_any(root: &serde_json::Value, keys: &[&str]) -> Option<f64> {
    keys.iter().find_map(|key| json_f64_field(root, key))
}

pub(super) fn parse_distribution_stats(root: &serde_json::Value) -> Option<MslDistributionStats> {
    Some(MslDistributionStats {
        sample_count: json_usize_field(root, "sample_count")?,
        min: json_f64_field_any(root, &["min", "min_ratio"])?,
        median: json_f64_field_any(root, &["median", "median_ratio"])?,
        mean: json_f64_field_any(root, &["mean", "mean_ratio"])?,
        max: json_f64_field_any(root, &["max", "max_ratio"])?,
    })
}

fn parse_distribution_stats_with_prefix(
    root: &serde_json::Value,
    prefix: &str,
    sample_count: usize,
) -> Option<MslDistributionStats> {
    Some(MslDistributionStats {
        sample_count,
        min: json_f64_field(root, &format!("min_{prefix}"))?,
        median: json_f64_field(root, &format!("median_{prefix}"))?,
        mean: json_f64_field(root, &format!("mean_{prefix}"))?,
        max: json_f64_field(root, &format!("max_{prefix}"))?,
    })
}

fn parse_total_models(payload: &serde_json::Value) -> Option<usize> {
    json_usize_field(payload, "total_models").or_else(|| {
        payload
            .get("models")
            .and_then(serde_json::Value::as_object)
            .map(serde_json::Map::len)
    })
}

fn parse_runtime_context(payload: &serde_json::Value) -> Option<MslParityRuntimeContext> {
    let timing = payload.get("timing")?;
    let context = MslParityRuntimeContext {
        workers_used: json_usize_field(timing, "workers_used"),
        omc_threads: json_usize_field(timing, "omc_threads"),
    };
    (context.workers_used.is_some() || context.omc_threads.is_some()).then_some(context)
}

fn parse_runtime_ratio_stats(payload: &serde_json::Value) -> Option<MslRuntimeRatioStatsBaseline> {
    let stats = payload.pointer("/runtime_comparison/ratio_stats")?;
    Some(MslRuntimeRatioStatsBaseline {
        system_ratio_both_success: parse_distribution_stats(
            stats.get("system_ratio_both_success")?,
        )?,
        wall_ratio_both_success: parse_distribution_stats(stats.get("wall_ratio_both_success")?)?,
    })
}

fn parse_trace_bounded_normalized_l1(
    trace: &serde_json::Value,
    models_compared: usize,
) -> Option<MslDistributionStats> {
    parse_distribution_stats_with_prefix(
        trace,
        "model_score_bounded_normalized_l1",
        models_compared,
    )
    .or_else(|| {
        Some(MslDistributionStats {
            sample_count: models_compared,
            min: json_f64_field(trace, "min_model_bounded_normalized_l1")?,
            median: json_f64_field(trace, "median_model_bounded_normalized_l1")?,
            mean: json_f64_field(trace, "mean_model_bounded_normalized_l1")?,
            max: json_f64_field(trace, "max_model_bounded_normalized_l1")?,
        })
    })
}

fn parse_initial_condition_stats(
    trace: &serde_json::Value,
) -> Option<MslInitialConditionStatsBaseline> {
    let initial = trace.get("initial_condition")?;
    Some(MslInitialConditionStatsBaseline {
        models_compared: json_usize_field(initial, "models_compared")?,
        models_with_accurate_initial_conditions: json_usize_field(
            initial,
            "models_with_accurate_initial_conditions",
        )?,
        models_with_initial_condition_deviation: json_usize_field(
            initial,
            "models_with_initial_condition_deviation",
        )?,
        models_with_unmeasured_initial_conditions: json_usize_field(
            initial,
            "models_with_unmeasured_initial_conditions",
        )?,
        accurate_initial_conditions_percent: json_f64_field(
            initial,
            "accurate_initial_conditions_percent",
        ),
        models_with_initial_condition_deviation_percent: json_f64_field(
            initial,
            "models_with_initial_condition_deviation_percent",
        ),
        models_with_unmeasured_initial_conditions_percent: json_f64_field(
            initial,
            "models_with_unmeasured_initial_conditions_percent",
        ),
        total_channels_compared: json_usize_field(initial, "total_channels_compared")?,
        high_channels_total: json_usize_field(initial, "high_channels_total")?,
        near_channels_total: json_usize_field(initial, "near_channels_total")?,
        deviation_channels_total: json_usize_field(initial, "deviation_channels_total")?,
        severe_channels_total: json_usize_field(initial, "severe_channels_total")?,
        high_channels_percent: json_f64_field(initial, "high_channels_percent"),
        near_channels_percent: json_f64_field(initial, "near_channels_percent"),
        deviation_channels_percent: json_f64_field(initial, "deviation_channels_percent"),
        severe_channels_percent: json_f64_field(initial, "severe_channels_percent"),
        violation_mass_total: json_f64_field(initial, "violation_mass_total"),
        violation_mass_mean_per_model: json_f64_field(initial, "violation_mass_mean_per_model"),
        violation_mass_mean_per_channel: json_f64_field(initial, "violation_mass_mean_per_channel"),
        mean_channel_bounded_normalized_error: json_f64_field(
            initial,
            "mean_channel_bounded_normalized_error",
        ),
        max_channel_bounded_normalized_error: json_f64_field(
            initial,
            "max_channel_bounded_normalized_error",
        ),
    })
}

fn parse_state_selection_stats(
    trace: &serde_json::Value,
) -> Option<MslStateSelectionStatsBaseline> {
    let state = trace.get("state_selection")?;
    Some(MslStateSelectionStatsBaseline {
        models_compared: json_usize_field(state, "models_compared")?,
        exact_state_set_match_models: json_usize_field(state, "exact_state_set_match_models")?,
        state_count_match_models: json_usize_field(state, "state_count_match_models")?,
        exact_state_set_match_percent: json_f64_field(state, "exact_state_set_match_percent"),
        state_count_match_percent: json_f64_field(state, "state_count_match_percent"),
        total_rumoca_states: json_usize_field(state, "total_rumoca_states")?,
        total_omc_states: json_usize_field(state, "total_omc_states")?,
        total_matching_states: json_usize_field(state, "total_matching_states")?,
        total_rumoca_only_states: json_usize_field(state, "total_rumoca_only_states")?,
        total_omc_only_states: json_usize_field(state, "total_omc_only_states")?,
        max_model_state_set_difference: json_usize_field(state, "max_model_state_set_difference")?,
    })
}

fn parse_trace_accuracy_stats(
    payload: &serde_json::Value,
) -> Option<MslTraceAccuracyStatsBaseline> {
    let trace = payload.pointer("/trace_comparison")?;
    let models_compared = json_usize_field(trace, "models_compared")?;
    let bounded_normalized_l1 = parse_trace_bounded_normalized_l1(trace, models_compared);
    Some(MslTraceAccuracyStatsBaseline {
        models_compared,
        missing_trace_models: json_usize_field(trace, "missing_trace_models")?,
        skipped_models: json_usize_field(trace, "skipped_models")?,
        policy_excluded_models: json_usize_field(trace, "policy_excluded_models").unwrap_or(0),
        trace_nonidentifiable_models: json_usize_field(trace, "trace_nonidentifiable_models")
            .unwrap_or(0),
        agreement_high: json_usize_field(trace, "agreement_high")?,
        agreement_high_percent: json_f64_field(trace, "agreement_high_percent"),
        agreement_minor: json_usize_field(trace, "agreement_near")
            .or_else(|| json_usize_field(trace, "agreement_minor"))?,
        agreement_minor_percent: json_f64_field(trace, "agreement_near_percent")
            .or_else(|| json_f64_field(trace, "agreement_minor_percent")),
        agreement_deviation: json_usize_field(trace, "agreement_deviation")?,
        agreement_deviation_percent: json_f64_field(trace, "agreement_deviation_percent"),
        total_channels_compared: json_usize_field(trace, "total_channels_compared"),
        bad_channels_total: json_usize_field(trace, "bad_channels_total"),
        severe_channels_total: json_usize_field(trace, "severe_channels_total"),
        bad_channels_percent: json_f64_field(trace, "bad_channels_percent"),
        severe_channels_percent: json_f64_field(trace, "severe_channels_percent"),
        violation_mass_total: json_f64_field(trace, "violation_mass_total"),
        violation_mass_mean_per_model: json_f64_field(trace, "violation_mass_mean_per_model"),
        violation_mass_mean_per_channel: json_f64_field(trace, "violation_mass_mean_per_channel"),
        models_with_bad_channel: json_usize_field(trace, "models_with_bad_channel"),
        models_with_severe_channel: json_usize_field(trace, "models_with_severe_channel"),
        models_with_any_channel_deviation: json_usize_field(
            trace,
            "models_with_any_channel_deviation",
        ),
        models_with_any_channel_deviation_percent: json_f64_field(
            trace,
            "models_with_any_channel_deviation_percent",
        ),
        max_model_channel_deviation_percent: json_f64_field(
            trace,
            "max_model_channel_deviation_percent",
        ),
        bounded_normalized_l1,
        mean_model_mean_channel_bounded_normalized_l1: json_f64_field(
            trace,
            "mean_model_mean_channel_bounded_normalized_l1",
        ),
        max_model_max_channel_bounded_normalized_l1: json_f64_field(
            trace,
            "max_model_max_channel_bounded_normalized_l1",
        )
        .or_else(|| json_f64_field(trace, "global_max_channel_bounded_normalized_l1")),
        model_mean_channel_bounded_normalized_l1: parse_distribution_stats_with_prefix(
            trace,
            "model_mean_channel_bounded_normalized_l1",
            models_compared,
        ),
        model_max_channel_bounded_normalized_l1: parse_distribution_stats_with_prefix(
            trace,
            "model_max_channel_bounded_normalized_l1",
            models_compared,
        ),
        initial_condition: parse_initial_condition_stats(trace),
        state_selection: parse_state_selection_stats(trace),
    })
}

fn parse_omc_version(payload: &serde_json::Value) -> Option<String> {
    payload
        .get("omc_version")
        .and_then(serde_json::Value::as_str)
        .map(str::trim)
        .filter(|version| !version.is_empty())
        .map(str::to_string)
}

pub(super) fn load_msl_parity_gate_input(path: &Path) -> io::Result<MslParityGateInput> {
    let file = File::open(path)?;
    let payload: serde_json::Value = serde_json::from_reader(file).map_err(|error| {
        io::Error::other(format!(
            "invalid OMC simulation reference JSON ({}): {error}",
            path.display()
        ))
    })?;

    Ok(MslParityGateInput {
        total_models: parse_total_models(&payload),
        omc_version: parse_omc_version(&payload),
        runtime_context: parse_runtime_context(&payload),
        runtime_ratio_stats: parse_runtime_ratio_stats(&payload),
        runtime_model_ratios: parse_runtime_model_ratios(&payload),
        trace_accuracy_stats: parse_trace_accuracy_stats(&payload),
        omc_assertion_failure_models: parse_omc_assertion_failure_models(&payload),
        omc_assertion_failure_examples: parse_omc_assertion_failure_examples(&payload),
    })
}

fn parse_omc_assertion_failure_models(payload: &serde_json::Value) -> usize {
    payload
        .pointer("/pipeline_progress/omc_assertion_failure_models")
        .and_then(serde_json::Value::as_u64)
        .or_else(|| {
            payload
                .pointer("/omc_assertion_failures/model_count")
                .and_then(serde_json::Value::as_u64)
        })
        .unwrap_or(0) as usize
}

fn parse_omc_assertion_failure_examples(payload: &serde_json::Value) -> Vec<String> {
    let Some(examples) = payload
        .pointer("/omc_assertion_failures/examples")
        .and_then(serde_json::Value::as_array)
    else {
        return Vec::new();
    };
    examples
        .iter()
        .filter_map(|entry| {
            let model = entry.get("model_name")?.as_str()?;
            let assertions = entry
                .get("assertions")
                .and_then(serde_json::Value::as_array)
                .map(|items| {
                    items
                        .iter()
                        .filter_map(serde_json::Value::as_str)
                        .take(3)
                        .collect::<Vec<_>>()
                        .join(" | ")
                })
                .unwrap_or_default();
            if assertions.is_empty() {
                Some(model.to_string())
            } else {
                Some(format!("{model}: {assertions}"))
            }
        })
        .take(10)
        .collect()
}

pub(super) fn load_current_msl_parity_gate_input_required(
    expected_sim_target_models: usize,
) -> io::Result<MslParityGateInput> {
    let path = omc_simulation_reference_path();
    if !path.is_file() {
        return Err(io::Error::other(format!(
            "missing required OMC parity file '{}'",
            path.display()
        )));
    }
    let parity = load_msl_parity_gate_input(&path)?;
    validate_parity_total_models(&path, &parity, expected_sim_target_models)?;
    if parity.omc_version.is_none() {
        return Err(io::Error::other(format!(
            "OMC parity file '{}' is missing omc_version metadata; regenerate OMC simulation reference",
            path.display()
        )));
    }
    if parity.omc_assertion_failure_models > 0 {
        let examples = if parity.omc_assertion_failure_examples.is_empty() {
            "no assertion examples recorded".to_string()
        } else {
            parity.omc_assertion_failure_examples.join("; ")
        };
        return Err(io::Error::other(format!(
            "OMC parity file '{}' contains {} Modelica assertion failure model(s): {}",
            path.display(),
            parity.omc_assertion_failure_models,
            examples
        )));
    }
    let runtime_stats = parity.runtime_ratio_stats.as_ref().ok_or_else(|| {
        io::Error::other(format!(
            "OMC parity file '{}' is missing runtime_ratio_stats",
            path.display()
        ))
    })?;
    if runtime_stats.system_ratio_both_success.sample_count == 0
        || runtime_stats.wall_ratio_both_success.sample_count == 0
    {
        return Err(io::Error::other(format!(
            "OMC parity file '{}' has empty runtime_ratio_stats sample_count (system={}, wall={})",
            path.display(),
            runtime_stats.system_ratio_both_success.sample_count,
            runtime_stats.wall_ratio_both_success.sample_count
        )));
    }
    let trace_stats = parity.trace_accuracy_stats.as_ref().ok_or_else(|| {
        io::Error::other(format!(
            "OMC parity file '{}' is missing trace_accuracy_stats",
            path.display()
        ))
    })?;
    if trace_stats.models_compared == 0 {
        return Err(io::Error::other(format!(
            "OMC parity file '{}' has models_compared=0 (no OMC/Rumoca traces were compared)",
            path.display()
        )));
    }
    if trace_model_bucket_percentages(trace_stats).is_none() {
        return Err(io::Error::other(format!(
            "OMC parity file '{}' is missing trace model bucket percentages",
            path.display()
        )));
    }
    Ok(parity)
}

// `load_current_msl_parity_gate_input_optional` deliberately no longer exists.
// Returning `Ok(None)` for "no reference on disk" was the silent default that
// let a run with no comparator output flow into the gate as an ordinary absent
// value. The replacement is `measure_msl_parity`, which converts every absence
// into a named `MslParityUnmeasuredReason`.

fn validate_parity_total_models(
    path: &Path,
    parity: &MslParityGateInput,
    expected_sim_target_models: usize,
) -> io::Result<()> {
    let parity_total_models = parity.total_models.ok_or_else(|| {
        io::Error::other(format!(
            "OMC parity file '{}' is missing total_models/models metadata",
            path.display()
        ))
    })?;
    if parity_total_models != expected_sim_target_models {
        return Err(io::Error::other(format!(
            "OMC parity file '{}' is stale: total_models={} but current sim_target_models={}; regenerate OMC simulation reference for the active target set",
            path.display(),
            parity_total_models,
            expected_sim_target_models
        )));
    }
    Ok(())
}

pub(super) fn resolve_msl_tools_exe_inner() -> Result<PathBuf, String> {
    for env_key in [
        "CARGO_BIN_EXE_rumoca-msl-tools",
        "CARGO_BIN_EXE_rumoca_msl_tools",
    ] {
        if let Ok(path) = std::env::var(env_key) {
            let candidate = PathBuf::from(path);
            if candidate.is_file() {
                return Ok(candidate);
            }
        }
    }

    let current_exe = std::env::current_exe()
        .map_err(|e| format!("failed to get current test binary path: {e}"))?;
    let deps_dir = current_exe.parent().ok_or_else(|| {
        format!(
            "failed to get parent directory for current test binary: {}",
            current_exe.display()
        )
    })?;
    let profile_dir = deps_dir.parent().ok_or_else(|| {
        format!(
            "failed to get profile directory from test binary path: {}",
            current_exe.display()
        )
    })?;

    let mut candidates = vec![
        profile_dir.join("rumoca-msl-tools"),
        profile_dir.join("rumoca-msl-tools.exe"),
        profile_dir.join("rumoca_msl_tools"),
        profile_dir.join("rumoca_msl_tools.exe"),
    ];

    if let Ok(entries) = fs::read_dir(deps_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let file_name = entry.file_name();
            let file_name = file_name.to_string_lossy();
            if file_name.starts_with("rumoca-msl-tools-")
                || file_name.starts_with("rumoca_msl_tools-")
            {
                candidates.push(path);
            }
        }
    }

    for candidate in candidates {
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    Err(format!(
        "failed to locate rumoca-msl-tools binary; expected a CARGO_BIN_EXE_* env var or a binary near {}",
        current_exe.display()
    ))
}

pub(super) fn resolve_msl_tools_exe() -> io::Result<PathBuf> {
    resolve_msl_tools_exe_inner().map_err(io::Error::other)
}

pub(super) fn normalize_model_names(mut names: Vec<String>) -> Vec<String> {
    names.sort();
    names.dedup();
    names
}

pub(super) fn model_names_from_omc_models_map(payload: &serde_json::Value) -> Option<Vec<String>> {
    let models = payload.get("models")?.as_object()?;
    Some(normalize_model_names(models.keys().cloned().collect()))
}

pub(super) fn current_msl_quality_baseline(
    summary: &MslSummary,
    parity_input: Option<&MslParityGateInput>,
) -> MslQualityBaseline {
    let gate_input = MslQualityGateInput::from(summary);
    MslQualityBaseline {
        quality_gate_version: MSL_QUALITY_GATE_VERSION,
        run_scope: MSL_QUALITY_RUN_SCOPE_FULL.to_string(),
        git_commit: summary.git_commit.clone(),
        msl_version: gate_input.msl_version.to_string(),
        omc_version: parity_input.and_then(|parity| parity.omc_version.clone()),
        sim_timeout_seconds: SIM_TIMEOUT_SECS,
        simulatable_attempted: gate_input.simulatable_attempted,
        parse_models: gate_input.parse_models,
        flatten_models: gate_input.flatten_models,
        dae_models: gate_input.dae_models,
        compiled_models: gate_input.compiled_models,
        solve_models: gate_input.solve_models,
        balanced_models: gate_input.balanced_models,
        unbalanced_models: gate_input.unbalanced_models,
        partial_models: gate_input.partial_models,
        balance_denominator: gate_input.balance_denominator,
        initial_balanced_models: gate_input.initial_balanced_models,
        initial_unbalanced_models: gate_input.initial_unbalanced_models,
        sim_target_models: gate_input.sim_target_models,
        sim_attempted: gate_input.sim_attempted,
        ic_attempted: gate_input.ic_attempted,
        ic_ok: gate_input.ic_ok,
        ic_solver_fail: gate_input.ic_solver_fail,
        sim_ok: gate_input.sim_ok,
        sim_success_rate: sim_success_rate(gate_input.sim_ok, gate_input.sim_target_models)
            .unwrap_or(0.0),
        runtime_context: parity_input.and_then(|parity| parity.runtime_context.clone()),
        runtime_ratio_stats: parity_input.and_then(|parity| parity.runtime_ratio_stats.clone()),
        runtime_ratio_cohort_models: parity_input.and_then(|parity| {
            (!parity.runtime_model_ratios.is_empty())
                .then(|| parity.runtime_model_ratios.keys().cloned().collect())
        }),
        trace_accuracy_stats: parity_input.and_then(|parity| parity.trace_accuracy_stats.clone()),
        tensor_preservation: MslTensorPreservationBaseline {
            models_reported: gate_input.tensor_models_reported,
            family_bodies: gate_input.tensor_family_bodies,
            preserved_family_bodies: gate_input.tensor_preserved_family_bodies,
            scalarized_family_rows: gate_input.tensor_scalarized_family_rows,
            report_errors: gate_input.tensor_report_errors,
            preservation_percent: tensor_preservation_percent(
                gate_input.tensor_preserved_family_bodies,
                gate_input.tensor_family_bodies,
            ),
        },
        metric_schema_migration: Some(quality_gate_v3_metric_schema_migration()),
        compiler_contract_migration: Some(checked_dae_compiler_contract_migration()),
    }
}

fn current_msl_quality_snapshot_json(
    summary: &MslSummary,
    parity_input: Option<&MslParityGateInput>,
    partial: bool,
) -> io::Result<serde_json::Value> {
    let baseline = current_msl_quality_baseline(summary, parity_input);
    let certified_simulations = parity_input
        .and_then(|parity| parity.trace_accuracy_stats.as_ref())
        .map_or(0, |trace| trace.agreement_high);
    let reviewed_trace_exceptions = parity_input
        .and_then(|parity| parity.trace_accuracy_stats.as_ref())
        .map_or(0, |trace| {
            trace.policy_excluded_models + trace.trace_nonidentifiable_models
        });
    let classified_simulations = certified_simulations + reviewed_trace_exceptions;
    let unclassified_simulations = summary.sim_ok.saturating_sub(classified_simulations);
    let classified_simulations_percent = if summary.sim_ok == 0 {
        100.0
    } else {
        100.0 * classified_simulations as f64 / summary.sim_ok as f64
    };
    let mut value = serde_json::to_value(&baseline).map_err(|error| {
        io::Error::other(format!("failed to serialize baseline JSON value: {error}"))
    })?;
    let Some(root) = value.as_object_mut() else {
        return Err(io::Error::other(
            "failed to serialize baseline JSON as an object",
        ));
    };
    root.insert(
        "run_scope".to_string(),
        serde_json::Value::String(
            if partial {
                MSL_QUALITY_RUN_SCOPE_PARTIAL
            } else {
                MSL_QUALITY_RUN_SCOPE_FULL
            }
            .to_string(),
        ),
    );
    root.insert(
        "pipeline_progress".to_string(),
        serde_json::json!({
            "total_mo_files": summary.total_mo_files,
            "parse_errors": summary.parse_errors,
            "total_models": summary.total_models,
            "parse_models": baseline.parse_models,
            "flatten_models": baseline.flatten_models,
            "dae_models": baseline.dae_models,
            "compiled_models": summary.compiled_models,
            "solve_models": baseline.solve_models,
            "balanced_models": summary.balanced_models,
            "initial_balanced_models": summary.initial_balanced_models,
            "state_selection": parity_input
                .and_then(|parity| parity.trace_accuracy_stats.as_ref())
                .and_then(|trace| trace.state_selection.as_ref()),
            "omc_assertion_failure_models": parity_input
                .map(|parity| parity.omc_assertion_failure_models)
                .unwrap_or(0),
            "sim_target_models": summary.sim_target_models.len(),
            "sim_attempted": summary.sim_attempted,
            "ic_attempted": summary.ic_attempted,
            "ic_ok": summary.ic_ok,
            "ic_solver_fail": summary.ic_solver_fail,
            "sim_ok": summary.sim_ok,
            "certified_simulations_strict_high": certified_simulations,
            "reviewed_trace_exceptions": reviewed_trace_exceptions,
            "unclassified_simulations": unclassified_simulations,
            "classified_simulations_percent": classified_simulations_percent,
            "sim_nan": summary.sim_nan,
            "sim_solver_fail": summary.sim_solver_fail,
            "sim_timeout": summary.sim_timeout,
            "tensor_preservation": baseline.tensor_preservation,
            "error_code_counts": &summary.error_code_counts,
            "unsupported_feature_counts": &summary.unsupported_feature_counts,
            "unsupported_feature_counts_by_backend": &summary.unsupported_feature_counts_by_backend,
            "phase_failure_counts": phase_failure_counts(summary),
        }),
    );
    root.insert(
        "mls_contract_coverage".to_string(),
        serde_json::to_value(build_mls_contract_coverage(summary)).map_err(|error| {
            io::Error::other(format!(
                "failed to serialize MLS contract coverage JSON value: {error}"
            ))
        })?,
    );
    if partial {
        root.insert("partial".to_string(), serde_json::Value::Bool(true));
    }
    Ok(value)
}

fn phase_failure_counts(summary: &MslSummary) -> std::collections::BTreeMap<String, usize> {
    summary
        .failures_by_phase
        .iter()
        .filter(|(phase, _)| phase.as_str() != "NeedsInner" && phase.as_str() != "NonSim")
        .map(|(phase, failures)| (phase.clone(), failures.len()))
        .collect()
}

pub(super) fn write_current_msl_quality_snapshot(
    summary: &MslSummary,
    stage: &MslParityStageOutcome,
) -> io::Result<()> {
    if summary.sim_attempted == 0 {
        return Ok(());
    }
    let measurement = measure_msl_parity(stage, summary.sim_target_models.len());
    let snapshot = current_msl_quality_snapshot_json(
        summary,
        measurement.gate_input(),
        should_skip_msl_quality_gate(),
    )?;
    let mut snapshot = snapshot;
    if let Some(root) = snapshot.as_object_mut() {
        // A promoted baseline must be able to say whether its numbers were
        // compared. `parity_measured: false` makes an unmeasured snapshot
        // self-describing instead of silently trace-statistics-free.
        root.insert(
            "parity_measured".to_string(),
            serde_json::Value::Bool(measurement.is_measured()),
        );
        if let Some(reason) = measurement.unmeasured_reason() {
            root.insert(
                "parity_unmeasured_reason".to_string(),
                serde_json::Value::String(reason.detail()),
            );
        }
    }
    let baseline_path = msl_quality_current_path();
    if let Some(parent) = baseline_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let baseline_json = serde_json::to_string_pretty(&snapshot)
        .map_err(|error| io::Error::other(format!("failed to serialize baseline JSON: {error}")))?;
    fs::write(&baseline_path, baseline_json)?;
    println!(
        "MSL current quality snapshot written to {}. Promote with `rum repo msl promote-quality-baseline` after approved full baseline runs; fallback baseline path is {}.",
        baseline_path.display(),
        msl_quality_baseline_path().display()
    );
    Ok(())
}

pub(super) fn sim_rate_gate_override_enabled() -> bool {
    false
}

fn skip_omc_compile_reference_enabled() -> bool {
    false
}

fn msl_quality_env_flag_enabled(name: &str) -> bool {
    std::env::var(name).is_ok_and(|value| {
        matches!(
            value.trim().to_ascii_lowercase().as_str(),
            "1" | "true" | "yes" | "on"
        )
    })
}

pub(super) fn msl_quality_context_mismatch_reason(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) -> Option<String> {
    if baseline.quality_gate_version != MSL_QUALITY_GATE_VERSION {
        return Some(format!(
            "quality_gate_version differs (baseline={}, current={})",
            baseline.quality_gate_version, MSL_QUALITY_GATE_VERSION
        ));
    }
    if baseline.run_scope != MSL_QUALITY_RUN_SCOPE_FULL {
        return Some(format!(
            "baseline run_scope must be '{}', got '{}'",
            MSL_QUALITY_RUN_SCOPE_FULL, baseline.run_scope
        ));
    }
    if baseline.msl_version != gate_input.msl_version {
        return Some(format!(
            "msl_version differs (baseline={}, current={})",
            baseline.msl_version, gate_input.msl_version
        ));
    }
    if let Some(baseline_omc_version) = baseline.omc_version.as_deref()
        && let Some(current_omc_version) =
            parity_input.and_then(|parity| parity.omc_version.as_deref())
        && quality_gate_omc_version(baseline_omc_version)
            != quality_gate_omc_version(current_omc_version)
    {
        return Some(format!(
            "omc_version differs (baseline={}, current={})",
            baseline_omc_version, current_omc_version
        ));
    }
    if (baseline.sim_timeout_seconds - SIM_TIMEOUT_SECS).abs() > SIM_RATE_GATE_EPSILON {
        return Some(format!(
            "sim_timeout_seconds differs (baseline={:.3}, current={:.3})",
            baseline.sim_timeout_seconds, SIM_TIMEOUT_SECS
        ));
    }
    if baseline.trace_accuracy_stats.is_none() {
        return Some("trace_accuracy_stats missing in baseline".to_string());
    }
    if baseline.simulatable_attempted != gate_input.simulatable_attempted {
        return Some(format!(
            "simulatable_attempted differs (baseline={}, current={}); baseline stage percentages require a fixed denominator",
            baseline.simulatable_attempted, gate_input.simulatable_attempted
        ));
    }
    if baseline.sim_target_models != gate_input.sim_target_models {
        return Some(format!(
            "sim_target_models differs (baseline={}, current={}); baseline simulation percentages require a fixed denominator",
            baseline.sim_target_models, gate_input.sim_target_models
        ));
    }

    let simulation_enabled = gate_input.sim_attempted > 0;
    if !simulation_enabled {
        return None;
    }

    None
}

pub(super) fn msl_quality_regression_reasons(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) -> Vec<String> {
    let mut reasons = Vec::new();
    push_compile_balance_regression_reasons(&mut reasons, gate_input, baseline);
    // `sim_ok` is completion, never parity: its movement is reported rather
    // than ratcheted. The separate soundness invariant below gates the only
    // meaningful relationship: every completion must also be strict-high.
    for note in sim_completion_report_notes(gate_input, baseline) {
        println!("MSL simulation completion (reported, not gated): {note}");
    }
    push_tensor_preservation_regression_reasons(&mut reasons, gate_input, baseline);
    push_trace_regression_reasons(&mut reasons, baseline, parity_input);
    push_trace_soundness_reasons(&mut reasons, gate_input, parity_input);
    push_runtime_ratio_regression_reasons(&mut reasons, baseline, parity_input);
    reasons
}

fn push_tensor_preservation_regression_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    if gate_input.tensor_report_errors != 0 {
        reasons.push(format!(
            "tensor-preservation report failures must be zero, got {}",
            gate_input.tensor_report_errors
        ));
    }
    let baseline_tensor = &baseline.tensor_preservation;
    let allowed_drop = solve_stage_count_allowed_drop(gate_input.simulatable_attempted);
    if stage_count_regressed(
        gate_input.tensor_models_reported,
        baseline_tensor.models_reported,
        allowed_drop,
    ) {
        reasons.push(format!(
            "tensor-preservation model coverage regressed: current={} < baseline={} (allowed drop={allowed_drop})",
            gate_input.tensor_models_reported, baseline_tensor.models_reported
        ));
    }
    let current_percent = tensor_preservation_percent(
        gate_input.tensor_preserved_family_bodies,
        gate_input.tensor_family_bodies,
    );
    if let (Some(current), Some(baseline_percent)) =
        (current_percent, baseline_tensor.preservation_percent)
        && current + SIM_RATE_GATE_EPSILON < baseline_percent
    {
        reasons.push(format!(
            "tensor preservation regressed: current={current:.2}% ({}/{}) < baseline={baseline_percent:.2}% ({}/{})",
            gate_input.tensor_preserved_family_bodies,
            gate_input.tensor_family_bodies,
            baseline_tensor.preserved_family_bodies,
            baseline_tensor.family_bodies
        ));
    }
}

pub(super) fn push_compile_balance_regression_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    push_compile_balance_count_regression_reasons(reasons, gate_input, baseline);
    push_compile_balance_rate_regression_reasons(reasons, gate_input, baseline);
}

fn push_compile_balance_count_regression_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    let denominator = gate_input.simulatable_attempted;
    push_stage_count_regression_reason(
        reasons,
        "Parse",
        gate_input.parse_models,
        baseline.parse_models.max(baseline.simulatable_attempted),
        denominator,
    );
    push_stage_count_regression_reason(
        reasons,
        "Flatten",
        gate_input.flatten_models,
        baseline.flatten_models,
        denominator,
    );
    push_stage_count_regression_reason(
        reasons,
        "DAE",
        gate_input.dae_models,
        baseline.dae_models.max(baseline.compiled_models),
        denominator,
    );
    push_stage_count_regression_reason(
        reasons,
        "Compile",
        gate_input.compiled_models,
        baseline.compiled_models,
        denominator,
    );
    push_stage_count_regression_reason_with_drop(
        reasons,
        "IR-Solve",
        gate_input.solve_models,
        baseline.solve_models,
        denominator,
        solve_stage_count_allowed_drop(denominator),
    );
    push_stage_count_regression_reason(
        reasons,
        "Balanced",
        gate_input.balanced_models,
        baseline.balanced_models,
        denominator,
    );
    push_stage_count_regression_reason(
        reasons,
        "Initial balance",
        gate_input.initial_balanced_models,
        baseline.initial_balanced_models,
        denominator,
    );

    if gate_input.partial_models > baseline.partial_models {
        reasons.push(format!(
            "partial_models increased: current={} > baseline={}",
            gate_input.partial_models, baseline.partial_models
        ));
    }
    if gate_input.unbalanced_models > baseline.unbalanced_models {
        reasons.push(format!(
            "unbalanced_models increased: current={} > baseline={}",
            gate_input.unbalanced_models, baseline.unbalanced_models
        ));
    }
}

fn push_compile_balance_rate_regression_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    let current_balance_rate =
        balance_success_rate(gate_input.balanced_models, gate_input.balance_denominator);
    let baseline_balance_rate =
        balance_success_rate(baseline.balanced_models, baseline.balance_denominator);
    if let (Some(current), Some(baseline_rate)) = (current_balance_rate, baseline_balance_rate) {
        let floor = (baseline_rate - BALANCE_RATE_GATE_TOLERANCE).max(0.0);
        if current + SIM_RATE_GATE_EPSILON < floor {
            reasons.push(format!(
                "balance success rate regressed: current={:.2}% ({}/{}) < floor={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                current * 100.0,
                gate_input.balanced_models,
                gate_input.balance_denominator,
                floor * 100.0,
                baseline_rate * 100.0,
                BALANCE_RATE_GATE_TOLERANCE * 100.0
            ));
        }
    }

    let current_initial_balance_rate = balance_success_rate(
        gate_input.initial_balanced_models,
        gate_input.balance_denominator,
    );
    let baseline_initial_balance_rate = balance_success_rate(
        baseline.initial_balanced_models,
        baseline.balance_denominator,
    );
    if let (Some(current), Some(baseline_rate)) =
        (current_initial_balance_rate, baseline_initial_balance_rate)
    {
        let floor = (baseline_rate - INITIAL_BALANCE_RATE_GATE_TOLERANCE).max(0.0);
        if current + SIM_RATE_GATE_EPSILON < floor {
            reasons.push(format!(
                "initial balance success rate regressed: current={:.2}% ({}/{}) < floor={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                current * 100.0,
                gate_input.initial_balanced_models,
                gate_input.balance_denominator,
                floor * 100.0,
                baseline_rate * 100.0,
                INITIAL_BALANCE_RATE_GATE_TOLERANCE * 100.0
            ));
        }
    }
}

/// Completion notes for `sim_ok` / `ic_ok`.
///
/// The predicate is unchanged from when these were gate reasons; only the
/// consumer moved. A note here is printed, recorded, and never fails a run —
/// the gate's ratchet is the strict-high agreement band
/// ([`push_trace_regression_reasons`]), which no model can enter without a
/// comparison against OMC.
pub(super) fn sim_completion_report_notes(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) -> Vec<String> {
    let mut notes = Vec::new();
    let allowed_drop = stage_count_allowed_drop(gate_input.sim_target_models);
    if stage_count_regressed(gate_input.sim_ok, baseline.sim_ok, allowed_drop) {
        push_stage_count_regression_reason_with_drop(
            &mut notes,
            "IC",
            gate_input.ic_ok,
            baseline.ic_ok,
            gate_input.sim_target_models,
            allowed_drop,
        );
    }
    push_stage_count_regression_reason(
        &mut notes,
        "Sim",
        gate_input.sim_ok,
        baseline.sim_ok,
        gate_input.sim_target_models,
    );
    notes
}

fn stage_count_regressed(current: usize, baseline: usize, allowed_drop: usize) -> bool {
    current < baseline.saturating_sub(allowed_drop)
}

fn stage_percent(count: usize, total: usize) -> f64 {
    if total == 0 {
        return 0.0;
    }
    count as f64 * 100.0 / total as f64
}

fn push_stage_count_regression_reason(
    reasons: &mut Vec<String>,
    stage: &str,
    current: usize,
    baseline: usize,
    denominator: usize,
) {
    push_stage_count_regression_reason_with_drop(
        reasons,
        stage,
        current,
        baseline,
        denominator,
        stage_count_allowed_drop(denominator),
    );
}

fn push_stage_count_regression_reason_with_drop(
    reasons: &mut Vec<String>,
    stage: &str,
    current: usize,
    baseline: usize,
    denominator: usize,
    allowed_drop: usize,
) {
    let floor = baseline.saturating_sub(allowed_drop);
    if !stage_count_regressed(current, baseline, allowed_drop) {
        return;
    }
    reasons.push(format!(
        "{stage} pass count regressed: current={} ({:.2}%) < floor={} ({:.2}%) (baseline={}, allowed_drop={}) over {} models",
        current,
        stage_percent(current, denominator),
        floor,
        stage_percent(floor, denominator),
        baseline,
        allowed_drop,
        denominator
    ));
}

fn stage_count_allowed_drop(denominator: usize) -> usize {
    if denominator >= MSL_STAGE_COUNT_ALLOWED_DROP_MIN_DENOMINATOR {
        MSL_STAGE_COUNT_ALLOWED_DROP
    } else {
        0
    }
}

fn solve_stage_count_allowed_drop(denominator: usize) -> usize {
    if denominator >= MSL_STAGE_COUNT_ALLOWED_DROP_MIN_DENOMINATOR {
        MSL_SOLVE_STAGE_COUNT_ALLOWED_DROP
    } else {
        0
    }
}

#[derive(Debug, Clone, Copy)]
struct TraceBucketPercentages {
    high: f64,
    near: f64,
    deviation: f64,
}

fn trace_count_to_percent(count: usize, total: usize) -> Option<f64> {
    if total == 0 {
        return None;
    }
    Some(count as f64 * 100.0 / total as f64)
}

fn trace_model_bucket_percentages(
    stats: &MslTraceAccuracyStatsBaseline,
) -> Option<TraceBucketPercentages> {
    let total = stats.models_compared;
    Some(TraceBucketPercentages {
        high: stats
            .agreement_high_percent
            .or_else(|| trace_count_to_percent(stats.agreement_high, total))?,
        near: stats
            .agreement_minor_percent
            .or_else(|| trace_count_to_percent(stats.agreement_minor, total))?,
        deviation: stats
            .agreement_deviation_percent
            .or_else(|| trace_count_to_percent(stats.agreement_deviation, total))?,
    })
}

fn trace_classified_models(stats: &MslTraceAccuracyStatsBaseline) -> usize {
    stats.agreement_high + stats.policy_excluded_models + stats.trace_nonidentifiable_models
}

fn trace_no_severe_models(stats: &MslTraceAccuracyStatsBaseline) -> Option<usize> {
    let reviewed_boundaries = stats.policy_excluded_models + stats.trace_nonidentifiable_models;
    stats
        .models_with_severe_channel
        .map(|count| {
            stats
                .models_compared
                .saturating_sub(count)
                .saturating_add(reviewed_boundaries)
        })
        .or_else(|| {
            stats
                .severe_channels_total
                .filter(|count| *count == 0)
                .map(|_| stats.models_compared + reviewed_boundaries)
        })
}

fn trace_accounted_models(stats: &MslTraceAccuracyStatsBaseline) -> usize {
    stats.models_compared + stats.policy_excluded_models + stats.trace_nonidentifiable_models
}

fn trace_models_with_any_channel_deviation_percent(
    stats: &MslTraceAccuracyStatsBaseline,
) -> Option<f64> {
    stats.models_with_any_channel_deviation_percent.or_else(|| {
        stats
            .models_with_any_channel_deviation
            .and_then(|count| trace_count_to_percent(count, stats.models_compared))
    })
}

fn trace_bad_channels_total(stats: &MslTraceAccuracyStatsBaseline) -> Option<usize> {
    stats.bad_channels_total
}

fn trace_severe_channels_total(stats: &MslTraceAccuracyStatsBaseline) -> Option<usize> {
    stats.severe_channels_total
}

fn trace_violation_mass_total(stats: &MslTraceAccuracyStatsBaseline) -> Option<f64> {
    stats.violation_mass_total
}

pub(super) fn push_trace_regression_reasons(
    reasons: &mut Vec<String>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    if let (Some(current_trace), Some(baseline_trace)) = (
        parity_input.and_then(|parity| parity.trace_accuracy_stats.as_ref()),
        baseline.trace_accuracy_stats.as_ref(),
    ) {
        // The cohort ratchet: models the comparator placed in the STRICT-HIGH
        // band. `agreement_minor` is a real, reportable band but it is not
        // parity, so it does not hold the ratchet up.
        push_stage_count_regression_reason(
            reasons,
            "Trace strict-high",
            current_trace.agreement_high,
            baseline_trace.agreement_high,
            baseline.sim_target_models,
        );
        let current_classified = trace_classified_models(current_trace);
        let baseline_classified = trace_classified_models(baseline_trace);
        push_stage_count_regression_reason(
            reasons,
            "Trace classified",
            current_classified,
            baseline_classified,
            baseline.sim_target_models,
        );

        if let (Some(current_no_severe), Some(baseline_no_severe)) = (
            trace_no_severe_models(current_trace),
            trace_no_severe_models(baseline_trace),
        ) {
            push_stage_count_regression_reason(
                reasons,
                "Trace no severe",
                current_no_severe,
                baseline_no_severe,
                baseline.sim_target_models,
            );
        }

        let current_accounted = trace_accounted_models(current_trace);
        let baseline_accounted = trace_accounted_models(baseline_trace);
        if current_accounted + TRACE_MODELS_COMPARED_ALLOWED_DROP < baseline_accounted {
            reasons.push(format!(
                "trace model accounting regressed: current={} < baseline={} (allowed_drop={})",
                current_accounted, baseline_accounted, TRACE_MODELS_COMPARED_ALLOWED_DROP
            ));
        }
    }
}

/// Fail closed when a completed simulation has neither strict-high OMC parity
/// nor a reviewed reason that pointwise comparison is non-identifying.
pub(super) fn push_trace_soundness_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    parity_input: Option<&MslParityGateInput>,
) {
    let Some(trace) = parity_input.and_then(|parity| parity.trace_accuracy_stats.as_ref()) else {
        return;
    };
    let reviewed_boundaries = trace.policy_excluded_models + trace.trace_nonidentifiable_models;
    let classified = trace.agreement_high + reviewed_boundaries;
    if gate_input.sim_ok != classified {
        reasons.push(format!(
            "simulation soundness requires every sim_ok model to be strict-high or carry a reviewed pointwise-oracle boundary: sim_ok={} strict_high={} reviewed_exceptions={} unclassified={} overclassified={}",
            gate_input.sim_ok,
            trace.agreement_high,
            reviewed_boundaries,
            gate_input.sim_ok.saturating_sub(classified),
            classified.saturating_sub(gate_input.sim_ok),
        ));
    }
}

pub(super) fn msl_quality_gate_failure_message(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    measurement: &MslParityMeasurement,
) -> Option<String> {
    let parity_input = measurement.gate_input();
    // SPEC 0033: a Tier 2 run whose comparator did not execute reports "parity
    // unmeasured", and unmeasured fails the gate. This reason is checked BEFORE
    // the baseline context comparison: with no reading at all, "the OMC version
    // moved" is a symptom of the missing comparator, not an independent finding,
    // and reporting the symptom would bury the cause.
    if let Some(reason) = measurement.gate_failure_reason() {
        return Some(reason);
    }
    if let Some(reason) = msl_quality_context_mismatch_reason(gate_input, baseline, parity_input) {
        return Some(format!(
            "MSL quality baseline context mismatch: {reason}. Update {} only with explicit review approval",
            MSL_QUALITY_BASELINE_FILE_REL
        ));
    }

    let mut reasons = Vec::new();
    reasons.extend(msl_quality_regression_reasons(
        gate_input,
        baseline,
        parity_input,
    ));
    if reasons.is_empty() {
        return None;
    }
    Some(reasons.join("; "))
}

pub(super) fn enforce_msl_quality_gate(
    summary: &MslSummary,
    stage: &MslParityStageOutcome,
) -> io::Result<()> {
    let measurement = measure_msl_parity(stage, summary.sim_target_models.len());
    // Printed on EVERY path, before any early return, so no run can end without
    // stating whether its numbers were compared against OMC, and — when there
    // is a previous band table — which models entered or left the compared set
    // since. A departure is listed by name: a model that leaves the compared
    // set must never be inferable only from a shrinking total.
    println!(
        "{}",
        measurement.summary_line(summary.sim_target_models.len())
    );
    if let Some(cohort) = measurement.cohort() {
        for line in cohort.departure_lines() {
            println!("{line}");
        }
        // A band is a share of the channels compared, so evidence can collapse
        // under a band that never moves. Printed beside the departures because
        // it is the same class of silent loss.
        for line in cohort.coverage_drop_lines() {
            println!("{line}");
        }
    }

    if require_selected_targets_success() {
        return enforce_all_selected_targets_succeeded(summary);
    }
    if summary.sim_attempted == 0 {
        match zero_simulation_attempt_rejection(
            summary.sim_target_models.len(),
            should_skip_msl_quality_gate(),
        ) {
            Some(message) => return Err(io::Error::other(message)),
            None => {
                println!("MSL quality gate: skipped for compile/balance-only run.");
                return Ok(());
            }
        }
    }
    if should_skip_msl_quality_gate() {
        println!(
            "MSL quality gate: skipped for focused/non-baseline run (committed target scope, explicit target file, subset, or partial sim set)."
        );
        return Ok(());
    }

    assert_msl_run_is_measurable(summary);

    let gate_input = MslQualityGateInput::from(summary);
    let baseline_path = msl_quality_baseline_path();
    let baseline = load_msl_quality_baseline(&baseline_path)?;
    let parity_input = measurement.gate_input();
    let mut gate_failure = msl_quality_gate_failure_message(gate_input, &baseline, &measurement);
    if let Some(floor) = strict_high_hard_floor_reason(summary, &measurement) {
        gate_failure = Some(match gate_failure {
            Some(existing) => format!("{existing}; {floor}"),
            None => floor,
        });
    }
    // A departure out of the strict-high band is invisible to every aggregate:
    // one model leaving and another entering holds `agreement_high` flat. The
    // per-model table is the only artifact that can see it, so the gate reads it
    // — and a cohort run that cannot diff against its predecessor fails rather
    // than passing with that reading silently switched off.
    for reason in [
        measurement.cohort_movement_unverified_reason(),
        measurement.departed_strict_high_reason(),
    ]
    .into_iter()
    .flatten()
    {
        gate_failure = Some(match gate_failure {
            Some(existing) => format!("{existing}; {reason}"),
            None => reason,
        });
    }

    if let Some(message) = gate_failure {
        panic!("MSL quality gate: {message}.");
    }

    print_compile_and_sim_gate_pass(gate_input, &baseline);
    print_trace_gate_status(&baseline, parity_input);
    print_runtime_ratio_status(&baseline, parity_input);
    println!("MSL quality baseline source: {}", baseline_path.display());

    Ok(())
}

/// The "a cohort run that simulated nothing is invalid" rule, as a pure
/// function of the run's shape.
///
/// Extracted so it can be tested without a `target/msl/parity-config.json` on
/// disk: a unit test whose verdict depends on leftover global state is the same
/// class of defect as a parity number with no comparison behind it.
pub(super) fn zero_simulation_attempt_rejection(
    sim_target_models: usize,
    focused_or_partial_run: bool,
) -> Option<String> {
    if focused_or_partial_run {
        return None;
    }
    Some(format!(
        "MSL quality gate: invalid full run (0 simulations attempted for {sim_target_models} selected simulation target(s)); fix the MSL shard/worker setup before accepting this run"
    ))
}

/// Focused-gate enforcement: every selected simulation target must report
/// `sim_ok`. Replaces the baseline-relative gate (which is skipped for explicit
/// target files) so a focused CI run still fails on any simulation failure.
fn enforce_all_selected_targets_succeeded(summary: &MslSummary) -> io::Result<()> {
    let failures = selected_target_failures(summary);
    if !failures.is_empty() {
        return Err(io::Error::other(format!(
            "{} of {} selected simulation target(s) did not succeed: {}.",
            failures.len(),
            summary.sim_target_models.len(),
            failures.join(", ")
        )));
    }
    println!(
        "MSL quality gate: all {} selected simulation target(s) succeeded.",
        summary.sim_target_models.len()
    );
    Ok(())
}

pub(super) fn selected_target_failures(summary: &MslSummary) -> Vec<String> {
    let target_set: IndexSet<&str> = summary
        .sim_target_models
        .iter()
        .map(String::as_str)
        .collect();
    let mut seen_targets = IndexSet::new();
    let mut failures: Vec<String> = summary
        .model_results
        .iter()
        .filter(|result| target_set.contains(result.model_name.as_str()))
        .filter_map(|result| {
            seen_targets.insert(result.model_name.as_str());
            if result.sim_status.as_deref() == Some("sim_ok") {
                return None;
            }
            let status = result.sim_status.as_deref().unwrap_or("not-simulated");
            Some(format!("{} ({status})", result.model_name))
        })
        .collect();
    failures.extend(
        target_set
            .into_iter()
            .filter(|model_name| !seen_targets.contains(*model_name))
            .map(|model_name| format!("{model_name} (missing-result)")),
    );
    failures
}

pub(super) fn should_skip_msl_quality_gate() -> bool {
    msl_target_scope() != MslTargetScope::RootExamples
        || sim_targets_file_override().is_some()
        || !sim_subset_patterns().is_empty()
        || sim_subset_limit().is_some()
        || sim_set_mode() != SimSetMode::Full
        // A shard sees only its stripe of the model set, so the aggregate
        // baseline ratchet + strict-high floor are meaningless here; the fan-in
        // job runs the gate once on the merged results. Also stamps the snapshot
        // partial:true, which promote-quality-baseline correctly refuses.
        || sim_shard().is_some()
}

/// Reject a run whose compile/balance KPIs are not measurable at all.
///
/// This is deliberately the ONLY validity check that runs before the OMC
/// comparator stage, and it reads nothing that depends on simulation outcomes.
/// The structural floor moved to [`strict_high_hard_floor_reason`], which the
/// gate evaluates after the comparator: a floor that aborts the run before the
/// comparator destroys the very measurement it is meant to judge, which is how
/// the `results-wave3` sweep published `sim_ok` with no bands behind it.
pub(super) fn assert_msl_run_is_measurable(summary: &MslSummary) {
    assert_ne!(
        summary.total_models, 0,
        "MSL quality gate: invalid run (total_models == 0). \
         Compile/balance KPIs are not measurable; fix model selection before accepting this run."
    );
    assert_eq!(
        summary.resolve_errors, 0,
        "MSL quality gate: invalid run (resolve_errors > 0). \
         The typed-tree/session build failed before model compilation; fix resolve errors before accepting this run."
    );
}

/// Structural floor on the *measured* cohort.
///
/// `sim_ok` is completion, never parity, so the floor counts models the OMC
/// comparator placed in the strict-high agreement band. Because
/// `agreement_high <= models_compared <= sim_ok`, this is strictly tighter than
/// the `sim_ok` floor it replaces: no run that failed the old floor can pass
/// this one.
pub(super) fn strict_high_hard_floor_reason(
    summary: &MslSummary,
    measurement: &MslParityMeasurement,
) -> Option<String> {
    if should_skip_msl_quality_gate() || summary.sim_attempted == 0 {
        return None;
    }
    strict_high_hard_floor_reason_for(
        summary.sim_target_models.len(),
        measurement.strict_high_models()?,
    )
}

/// The floor itself, independent of run configuration. `None` when the run
/// clears it.
pub(super) fn strict_high_hard_floor_reason_for(
    target_models: usize,
    strict_high: usize,
) -> Option<String> {
    let required = ((target_models as f64) * DEFAULT_STRICT_HIGH_HARD_FLOOR_RATIO).ceil() as usize;
    if strict_high >= required {
        return None;
    }
    Some(format!(
        "strict-high agreement below hard floor: {strict_high}/{target_models} models agree with \
         OMC in the strict-high band; required at least {required} ({:.1}% floor)",
        DEFAULT_STRICT_HIGH_HARD_FLOOR_RATIO * 100.0
    ))
}
