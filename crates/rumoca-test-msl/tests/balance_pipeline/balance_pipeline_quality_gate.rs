use super::*;
#[cfg(test)]
mod tests;
// =============================================================================
// MSL quality gate (compile/balance strict + simulation tolerant gate)
// =============================================================================
pub(super) const SIM_RATE_GATE_OVERRIDE_ENV: &str = "RUMOCA_ALLOW_SIM_RATE_REGRESSION";
pub(super) const FORCE_OMC_PARITY_REFRESH_ENV: &str = "RUMOCA_MSL_FORCE_OMC_PARITY_REFRESH";
pub(super) const OMC_PARITY_WORKERS_ENV: &str = "RUMOCA_MSL_OMC_PARITY_WORKERS";
pub(super) const OMC_COMPILE_REFERENCE_MODEL_TIMEOUT_ENV: &str =
    "RUMOCA_MSL_OMC_COMPILE_REFERENCE_MODEL_TIMEOUT_SECS";
pub(super) const OMC_SIM_REFERENCE_BATCH_TIMEOUT_ENV: &str =
    "RUMOCA_MSL_OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECS";
pub(super) const SIM_RATE_GATE_EPSILON: f64 = 1.0e-12;
/// Allowed simulation-rate drop (absolute ratio, i.e. 0.02 = 2.0 percentage points).
// Temporary relaxation while broader discrete-signal evaluation is being integrated.
// Tighten back after baseline stabilization.
pub(super) const SIM_RATE_GATE_TOLERANCE: f64 = 0.035;
/// Structural floor for the default 180-model baseline simulation run.
///
/// This is intentionally much looser than the baseline delta gate. Its job is to
/// reject obviously invalid runs (for example near-zero or zero successful
/// simulations) before we start comparing finer-grained regressions.
pub(super) const DEFAULT_SIM_OK_HARD_FLOOR_RATIO: f64 = 0.50;
/// Compile-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const COMPILE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Balance-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const BALANCE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Initial-balance-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const INITIAL_BALANCE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Allowed drop in trace high-agreement model percentage (absolute percentage points).
pub(super) const TRACE_HIGH_PERCENT_DROP_TOLERANCE_PP: f64 = 3.0;
/// Allowed drop in total acceptable trace-model share (`high + near`, absolute percentage points).
pub(super) const TRACE_ACCEPTABLE_PERCENT_DROP_TOLERANCE_PP: f64 = 3.0;
/// Allowed increase in trace deviation model percentage (absolute percentage points).
pub(super) const TRACE_DEVIATION_PERCENT_INCREASE_TOLERANCE_PP: f64 = 3.0;
/// Hard guard for models that contain any deviation channel (absolute percentage points).
pub(super) const TRACE_ANY_CHANNEL_DEVIATION_PERCENT_INCREASE_TOLERANCE_PP: f64 = 3.0;
/// Allowed increase in bad-channel share (absolute percentage points).
pub(super) const TRACE_BAD_CHANNEL_PERCENT_INCREASE_TOLERANCE_PP: f64 = 1.0;
/// Allowed increase in severe-channel share (absolute percentage points).
pub(super) const TRACE_SEVERE_CHANNEL_PERCENT_INCREASE_TOLERANCE_PP: f64 = 1.5;
/// Allowed drop in compared trace-model count from baseline.
pub(super) const TRACE_MODELS_COMPARED_ALLOWED_DROP: usize = 2;
/// Allowed relative drop in runtime speedup median (omc/rumoca) before failing.
pub(super) const RUNTIME_RATIO_MEDIAN_REL_TOLERANCE: f64 = 0.20;
/// Default OMC worker cap for parity reference generation.
///
/// OMC is often accessed through a Docker-backed wrapper on macOS. Running one
/// OMC process per local CPU can make otherwise quick Clocked examples hit the
/// per-model timeout and collapse trace coverage. Keep this conservative by
/// default; developers can still override it with `RUMOCA_MSL_OMC_PARITY_WORKERS`.
pub(super) const OMC_PARITY_WORKERS_DEFAULT_MAX: usize = 2;
/// OMC process timeout budget for compile reference generation.
pub(super) const OMC_COMPILE_REFERENCE_MODEL_TIMEOUT_SECONDS: u64 = 60;
/// OMC process timeout budget for simulation reference generation.
pub(super) const OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS: u64 = 60;
/// Force low-impact OpenMP/BLAS threading in OMC child processes.
pub(super) const OMC_PARITY_THREADS_DEFAULT: usize = 1;
pub(super) const MSL_QUALITY_BASELINE_FILE_REL: &str = "tests/msl_tests/msl_quality_baseline.json";
pub(super) const MSL_QUALITY_CURRENT_FILE_REL: &str = "results/msl_quality_current.json";
pub(super) const MSL_COMPILE_TARGETS_FILE_REL: &str = "results/msl_compile_targets.json";
pub(super) const MSL_SIM_TARGETS_FILE_REL: &str = "results/msl_simulation_targets.json";
pub(super) const OMC_PARITY_CACHE_DIR_REL: &str = "results/omc_parity_cache";
pub(super) const OMC_REFERENCE_FILE_REL: &str = "results/omc_reference.json";
pub(super) const OMC_SIM_REFERENCE_FILE_REL: &str = "results/omc_simulation_reference.json";
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslTraceAccuracyStatsBaseline {
    models_compared: usize,
    missing_trace_models: usize,
    skipped_models: usize,
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
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslQualityBaseline {
    git_commit: String,
    msl_version: String,
    sim_timeout_seconds: f64,
    simulatable_attempted: usize,
    compiled_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    partial_models: usize,
    balance_denominator: usize,
    initial_balanced_models: usize,
    initial_unbalanced_models: usize,
    sim_target_models: usize,
    sim_attempted: usize,
    sim_ok: usize,
    sim_success_rate: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    runtime_context: Option<MslParityRuntimeContext>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    runtime_ratio_stats: Option<MslRuntimeRatioStatsBaseline>,
    #[serde(default)]
    trace_accuracy_stats: Option<MslTraceAccuracyStatsBaseline>,
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
    runtime_context: Option<MslParityRuntimeContext>,
    runtime_ratio_stats: Option<MslRuntimeRatioStatsBaseline>,
    trace_accuracy_stats: Option<MslTraceAccuracyStatsBaseline>,
}
#[derive(Debug, Clone, Copy)]
pub(super) struct MslQualityGateInput<'a> {
    msl_version: &'a str,
    simulatable_attempted: usize,
    compiled_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    partial_models: usize,
    balance_denominator: usize,
    initial_balanced_models: usize,
    initial_unbalanced_models: usize,
    sim_target_models: usize,
    sim_attempted: usize,
    sim_ok: usize,
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
        Self {
            msl_version: &summary.msl_version,
            simulatable_attempted,
            compiled_models: summary.compiled_models,
            balanced_models: summary.balanced_models,
            unbalanced_models: summary.unbalanced_models,
            partial_models: summary.partial_models,
            balance_denominator,
            initial_balanced_models: summary.initial_balanced_models,
            initial_unbalanced_models: summary.initial_unbalanced_models,
            sim_target_models: summary.sim_target_models.len(),
            sim_attempted: summary.sim_attempted,
            sim_ok: summary.sim_ok,
        }
    }
}
pub(super) fn sim_success_rate(sim_ok: usize, sim_attempted: usize) -> Option<f64> {
    if sim_attempted == 0 {
        return None;
    }
    Some(sim_ok as f64 / sim_attempted as f64)
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
    Path::new(env!("CARGO_MANIFEST_DIR")).join(MSL_QUALITY_BASELINE_FILE_REL)
}
pub(super) fn msl_quality_current_path() -> PathBuf {
    get_msl_cache_dir().join(MSL_QUALITY_CURRENT_FILE_REL)
}
pub(super) fn msl_compile_targets_path() -> PathBuf {
    get_msl_cache_dir().join(MSL_COMPILE_TARGETS_FILE_REL)
}
pub(super) fn msl_simulation_targets_path() -> PathBuf {
    get_msl_cache_dir().join(MSL_SIM_TARGETS_FILE_REL)
}
pub(super) fn omc_reference_path() -> PathBuf {
    get_msl_cache_dir().join(OMC_REFERENCE_FILE_REL)
}
pub(super) fn omc_parity_cache_dir() -> PathBuf {
    get_msl_cache_dir().join(OMC_PARITY_CACHE_DIR_REL)
}
pub(super) fn load_msl_quality_baseline(path: &Path) -> io::Result<MslQualityBaseline> {
    let file = File::open(path)?;
    serde_json::from_reader(file)
        .map_err(|error| io::Error::other(format!("invalid MSL quality baseline JSON: {error}")))
}
pub(super) fn omc_simulation_reference_path() -> PathBuf {
    get_msl_cache_dir().join(OMC_SIM_REFERENCE_FILE_REL)
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
    })
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
        runtime_context: parse_runtime_context(&payload),
        runtime_ratio_stats: parse_runtime_ratio_stats(&payload),
        trace_accuracy_stats: parse_trace_accuracy_stats(&payload),
    })
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
pub(super) fn load_current_msl_parity_gate_input_optional(
    expected_sim_target_models: usize,
) -> io::Result<Option<MslParityGateInput>> {
    let path = omc_simulation_reference_path();
    if !path.is_file() {
        return Ok(None);
    }
    load_current_msl_parity_gate_input_required(expected_sim_target_models).map(Some)
}
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
    if let Ok(path) = std::env::var("RUMOCA_MSL_TOOLS_EXE") {
        let candidate = PathBuf::from(path);
        if candidate.is_file() {
            return Ok(candidate);
        }
    }
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
        "failed to locate rumoca-msl-tools binary; expected RUMOCA_MSL_TOOLS_EXE or CARGO_BIN_EXE_* env var or binary near {}",
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
fn canonical_msl_version(version: &str) -> &str {
    version.trim().trim_start_matches('v')
}
fn canonical_omc_version(version: &str) -> &str {
    version.trim()
}
fn fnv1a64_update(mut hash: u64, bytes: &[u8]) -> u64 {
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x00000100000001B3;
    if hash == 0 {
        hash = OFFSET;
    }
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}
fn parity_target_set_cache_key(
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
) -> String {
    let normalized_models = normalize_model_names(target_models.to_vec());
    let mut hash = 0_u64;
    hash = fnv1a64_update(hash, canonical_msl_version(msl_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xff]);
    hash = fnv1a64_update(hash, canonical_omc_version(omc_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xfe]);
    hash = fnv1a64_update(hash, normalized_models.len().to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xfd]);
    for model in &normalized_models {
        hash = fnv1a64_update(hash, model.as_bytes());
        hash = fnv1a64_update(hash, &[0x00]);
    }
    format!("{hash:016x}")
}
#[derive(Debug, Clone, Copy, PartialEq)]
struct SimulationParityCachePolicy {
    batch_timeout_seconds: u64,
    workers: usize,
    omc_threads: usize,
    use_experiment_stop_time: bool,
    stop_time_override: Option<f64>,
}
fn positive_usize_env(env_key: &str) -> Option<usize> {
    std::env::var(env_key)
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|value| *value > 0)
}
fn positive_u64_env(env_key: &str) -> Option<u64> {
    std::env::var(env_key)
        .ok()
        .and_then(|raw| raw.trim().parse::<u64>().ok())
        .filter(|value| *value > 0)
}
fn simulation_stop_time_override() -> Option<f64> {
    std::env::var("RUMOCA_MSL_SIM_STOP_TIME_OVERRIDE")
        .ok()
        .and_then(|raw| raw.trim().parse::<f64>().ok())
        .filter(|value| value.is_finite() && *value > 0.0)
}
fn omc_compile_reference_model_timeout_seconds() -> u64 {
    positive_u64_env(OMC_COMPILE_REFERENCE_MODEL_TIMEOUT_ENV)
        .unwrap_or(OMC_COMPILE_REFERENCE_MODEL_TIMEOUT_SECONDS)
}
fn omc_sim_reference_batch_timeout_seconds() -> u64 {
    positive_u64_env(OMC_SIM_REFERENCE_BATCH_TIMEOUT_ENV)
        .unwrap_or(OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS)
}
fn current_simulation_parity_cache_policy(
    context: &ParityStepContext,
) -> SimulationParityCachePolicy {
    let stop_time_override = simulation_stop_time_override();
    SimulationParityCachePolicy {
        batch_timeout_seconds: context.sim_batch_timeout_seconds,
        workers: context.workers,
        omc_threads: context.omc_threads,
        use_experiment_stop_time: stop_time_override.is_none(),
        stop_time_override,
    }
}
fn simulation_parity_cache_key(
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
    policy: SimulationParityCachePolicy,
) -> String {
    let normalized_models = normalize_model_names(target_models.to_vec());
    let mut hash = 0_u64;
    hash = fnv1a64_update(hash, canonical_msl_version(msl_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xff]);
    hash = fnv1a64_update(hash, canonical_omc_version(omc_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xfe]);
    hash = fnv1a64_update(hash, normalized_models.len().to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xfd]);
    for model in &normalized_models {
        hash = fnv1a64_update(hash, model.as_bytes());
        hash = fnv1a64_update(hash, &[0x00]);
    }
    hash = fnv1a64_update(hash, &[0xfc]);
    hash = fnv1a64_update(hash, policy.batch_timeout_seconds.to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xfb]);
    hash = fnv1a64_update(hash, policy.workers.to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xf9]);
    hash = fnv1a64_update(hash, policy.omc_threads.to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xf8]);
    hash = fnv1a64_update(hash, &[u8::from(policy.use_experiment_stop_time)]);
    hash = fnv1a64_update(hash, &[0xfa]);
    if let Some(stop_time_override) = policy.stop_time_override {
        hash = fnv1a64_update(hash, stop_time_override.to_string().as_bytes());
    } else {
        hash = fnv1a64_update(hash, b"none");
    }
    format!("{hash:016x}")
}
fn parity_cache_entry_path(kind: &str, cache_key: &str) -> PathBuf {
    omc_parity_cache_dir()
        .join(kind)
        .join(format!("{cache_key}.json"))
}
fn materialize_parity_cache_entry(
    cache_path: &Path,
    active_path: &Path,
    label: &str,
) -> io::Result<()> {
    if !cache_path.is_file() {
        return Err(io::Error::other(format!(
            "missing {label} parity cache entry '{}'",
            cache_path.display()
        )));
    }
    if let Some(parent) = active_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::copy(cache_path, active_path).map_err(|error| {
        io::Error::other(format!(
            "failed to materialize {label} parity cache '{}' -> '{}': {error}",
            cache_path.display(),
            active_path.display()
        ))
    })?;
    Ok(())
}
fn materialize_simulation_parity_cache_entry(
    cache_path: &Path,
    active_path: &Path,
) -> io::Result<()> {
    if !cache_path.is_file() {
        return Err(io::Error::other(format!(
            "missing simulation parity cache entry '{}'",
            cache_path.display()
        )));
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(cache_path)?).map_err(|error| {
            io::Error::other(format!(
                "failed to parse simulation parity cache '{}' for materialization: {error}",
                cache_path.display()
            ))
        })?;
    if let Some(parent) = active_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    fs::write(
        active_path,
        serde_json::to_vec_pretty(&sanitized).map_err(|error| {
            io::Error::other(format!(
                "failed to serialize sanitized simulation parity cache '{}': {error}",
                active_path.display()
            ))
        })?,
    )
    .map_err(|error| {
        io::Error::other(format!(
            "failed to materialize sanitized simulation parity cache '{}' -> '{}': {error}",
            cache_path.display(),
            active_path.display()
        ))
    })
}
fn sanitize_simulation_parity_cache_payload(mut payload: serde_json::Value) -> serde_json::Value {
    let Some(root) = payload.as_object_mut() else {
        return payload;
    };
    root.remove("runtime_comparison");
    root.remove("trace_comparison");
    let Some(models) = root
        .get_mut("models")
        .and_then(serde_json::Value::as_object_mut)
    else {
        return payload;
    };
    for model in models.values_mut() {
        let Some(model) = model.as_object_mut() else {
            continue;
        };
        model.remove("rumoca_status");
        model.remove("rumoca_sim_seconds");
        model.remove("rumoca_sim_wall_seconds");
        model.remove("rumoca_trace_file");
        model.remove("rumoca_trace_error");
    }
    payload
}
fn persist_simulation_parity_cache_entry(active_path: &Path, cache_path: &Path) -> io::Result<()> {
    if !active_path.is_file() {
        return Ok(());
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(active_path)?).map_err(|error| {
            io::Error::other(format!(
                "failed to parse simulation parity reference '{}' for cache persistence: {error}",
                active_path.display()
            ))
        })?;
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    fs::write(
        cache_path,
        serde_json::to_vec_pretty(&sanitized).map_err(|error| {
            io::Error::other(format!(
                "failed to serialize sanitized simulation parity cache '{}': {error}",
                cache_path.display()
            ))
        })?,
    )
    .map_err(|error| {
        io::Error::other(format!(
            "failed to persist simulation parity cache '{}' -> '{}': {error}",
            active_path.display(),
            cache_path.display()
        ))
    })
}
fn persist_parity_cache_entry(
    active_path: &Path,
    cache_path: &Path,
    label: &str,
) -> io::Result<()> {
    if !active_path.is_file() {
        return Ok(());
    }
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::copy(active_path, cache_path).map_err(|error| {
        io::Error::other(format!(
            "failed to persist {label} parity cache '{}' -> '{}': {error}",
            active_path.display(),
            cache_path.display()
        ))
    })?;
    Ok(())
}
fn current_omc_version() -> io::Result<String> {
    let output = std::process::Command::new("omc")
        .arg("--version")
        .output()?;
    if !output.status.success() {
        return Err(io::Error::other(format!(
            "failed to query OMC version (status={})",
            output.status
        )));
    }
    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let version = if stdout.is_empty() {
        String::from_utf8_lossy(&output.stderr).trim().to_string()
    } else {
        stdout
    };
    if version.is_empty() {
        return Err(io::Error::other("omc --version returned empty output"));
    }
    Ok(version)
}
pub(super) fn parity_cache_matches_targets_and_msl(
    path: &Path,
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
) -> io::Result<bool> {
    if !path.is_file() {
        return Ok(false);
    }
    let file = File::open(path)?;
    let payload: serde_json::Value = serde_json::from_reader(file).map_err(|error| {
        io::Error::other(format!("invalid parity JSON ({}): {error}", path.display()))
    })?;
    let Some(cached_msl_version) = payload
        .get("msl_version")
        .and_then(serde_json::Value::as_str)
    else {
        return Ok(false);
    };
    if canonical_msl_version(cached_msl_version) != canonical_msl_version(msl_version) {
        return Ok(false);
    }
    let Some(cached_omc_version) = payload
        .get("omc_version")
        .and_then(serde_json::Value::as_str)
    else {
        return Ok(false);
    };
    if canonical_omc_version(cached_omc_version) != canonical_omc_version(omc_version) {
        return Ok(false);
    }
    let Some(cached_models) = model_names_from_omc_models_map(&payload) else {
        return Ok(false);
    };
    Ok(cached_models == normalize_model_names(target_models.to_vec()))
}
fn simulation_parity_cache_matches(
    path: &Path,
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
    policy: SimulationParityCachePolicy,
) -> io::Result<bool> {
    if !parity_cache_matches_targets_and_msl(path, target_models, msl_version, omc_version)? {
        return Ok(false);
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(path)?).map_err(|error| {
            io::Error::other(format!(
                "invalid simulation parity JSON ({}): {error}",
                path.display()
            ))
        })?;
    let batch_timeout_seconds = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("batch_timeout_seconds"))
        .and_then(serde_json::Value::as_u64);
    if batch_timeout_seconds != Some(policy.batch_timeout_seconds) {
        return Ok(false);
    }
    let workers_used = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("workers_used"))
        .and_then(serde_json::Value::as_u64);
    if workers_used != Some(policy.workers as u64) {
        return Ok(false);
    }
    let omc_threads = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("omc_threads"))
        .and_then(serde_json::Value::as_u64);
    if omc_threads != Some(policy.omc_threads as u64) {
        return Ok(false);
    }
    let use_experiment_stop_time = payload
        .get("use_experiment_stop_time")
        .and_then(serde_json::Value::as_bool);
    if use_experiment_stop_time != Some(policy.use_experiment_stop_time) {
        return Ok(false);
    }
    let Some(stop_time_override) = policy.stop_time_override else {
        return Ok(true);
    };
    let stop_time = payload.get("stop_time").and_then(serde_json::Value::as_f64);
    Ok(stop_time.is_some_and(|value| {
        (value - stop_time_override).abs() <= f64::EPSILON.max(stop_time_override.abs() * 1e-12)
    }))
}
pub(super) fn run_msl_tool_command<I, S>(exe: &Path, args: I) -> io::Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<std::ffi::OsStr>,
{
    let args_vec: Vec<std::ffi::OsString> = args
        .into_iter()
        .map(|arg| arg.as_ref().to_os_string())
        .collect();
    let mut cmd = Command::new(exe);
    cmd.args(&args_vec);
    cmd.env("RUMOCA_MSL_CACHE_DIR", get_msl_cache_dir());
    cmd.stdout(std::process::Stdio::inherit());
    cmd.stderr(std::process::Stdio::inherit());
    let rendered_args = args_vec
        .iter()
        .map(|arg| arg.to_string_lossy())
        .collect::<Vec<_>>()
        .join(" ");
    println!(
        "Running parity command: {} {}",
        exe.display(),
        rendered_args
    );
    let status = cmd.status()?;
    if status.success() {
        return Ok(());
    }
    Err(io::Error::other(format!(
        "command '{}' failed (status={})",
        exe.display(),
        status
    )))
}
pub(super) fn omc_parity_workers() -> usize {
    positive_usize_env(OMC_PARITY_WORKERS_ENV)
        .unwrap_or_else(|| msl_stage_parallelism().clamp(1, OMC_PARITY_WORKERS_DEFAULT_MAX))
}
pub(super) fn omc_parity_threads() -> usize {
    OMC_PARITY_THREADS_DEFAULT
}
fn force_omc_parity_refresh_enabled() -> bool {
    std::env::var(FORCE_OMC_PARITY_REFRESH_ENV).is_ok_and(|value| {
        value == "1"
            || value.eq_ignore_ascii_case("true")
            || value.eq_ignore_ascii_case("yes")
            || value.eq_ignore_ascii_case("on")
    })
}
fn load_parity_targets() -> io::Result<(PathBuf, Vec<String>, PathBuf, Vec<String>)> {
    let compile_targets_path = msl_compile_targets_path();
    let sim_targets_path = msl_simulation_targets_path();
    let compile_targets = load_target_model_names(&compile_targets_path).map_err(|error| {
        io::Error::other(format!(
            "failed to load compile targets '{}': {}",
            compile_targets_path.display(),
            error
        ))
    })?;
    let sim_targets = load_target_model_names(&sim_targets_path).map_err(|error| {
        io::Error::other(format!(
            "failed to load simulation targets '{}': {}",
            sim_targets_path.display(),
            error
        ))
    })?;
    Ok((
        compile_targets_path,
        compile_targets,
        sim_targets_path,
        sim_targets,
    ))
}
struct ParityStepContext {
    tools_exe: PathBuf,
    omc_version: String,
    workers: usize,
    omc_threads: usize,
    compile_model_timeout_seconds: u64,
    sim_batch_timeout_seconds: u64,
}
fn ensure_compile_parity_reference(
    summary: &MslSummary,
    force_refresh: bool,
    context: &ParityStepContext,
    compile_targets_path: &Path,
    compile_targets: &[String],
) -> io::Result<()> {
    let _compile_ref_watchdog = StageAbortWatchdog::new(
        "parity_compile_reference",
        "RUMOCA_MSL_STAGE_TIMEOUT_PARITY_COMPILE_REF_SECS",
        1800,
    );
    let omc_reference = omc_reference_path();
    let compile_cache_key =
        parity_target_set_cache_key(compile_targets, &summary.msl_version, &context.omc_version);
    let compile_cache_entry = parity_cache_entry_path("compile", &compile_cache_key);
    if !force_refresh
        && parity_cache_matches_targets_and_msl(
            &compile_cache_entry,
            compile_targets,
            &summary.msl_version,
            &context.omc_version,
        )?
    {
        materialize_parity_cache_entry(&compile_cache_entry, &omc_reference, "compile reference")?;
        println!(
            "MSL parity cache hit: reusing {} via keyed cache {}",
            omc_reference.display(),
            compile_cache_entry.display()
        );
        return Ok(());
    }
    let should_regenerate = force_refresh
        || !parity_cache_matches_targets_and_msl(
            &omc_reference,
            compile_targets,
            &summary.msl_version,
            &context.omc_version,
        )?;
    if should_regenerate {
        println!(
            "MSL parity cache miss for compile reference; regenerating {}",
            omc_reference.display()
        );
        let compile_targets_arg = compile_targets_path.to_string_lossy().to_string();
        run_msl_tool_command(
            &context.tools_exe,
            vec![
                "omc-reference".to_string(),
                "--target-models-file".to_string(),
                compile_targets_arg,
                "--workers".to_string(),
                context.workers.to_string(),
                "--omc-threads".to_string(),
                context.omc_threads.to_string(),
                "--model-timeout-seconds".to_string(),
                context.compile_model_timeout_seconds.to_string(),
            ],
        )?;
    } else {
        println!("MSL parity cache hit: reusing {}", omc_reference.display());
    }
    persist_parity_cache_entry(&omc_reference, &compile_cache_entry, "compile reference")?;
    Ok(())
}
fn run_simulation_parity_reference_command(
    context: &ParityStepContext,
    sim_targets_path: &Path,
    resume: bool,
) -> io::Result<()> {
    let sim_targets_arg = sim_targets_path.to_string_lossy().to_string();
    let mut args = vec![
        "omc-simulation-reference".to_string(),
        "--target-models-file".to_string(),
        sim_targets_arg,
        "--use-experiment-stop-time".to_string(),
        "--batch-timeout-seconds".to_string(),
        context.sim_batch_timeout_seconds.to_string(),
        "--workers".to_string(),
        context.workers.to_string(),
        "--omc-threads".to_string(),
        context.omc_threads.to_string(),
    ];
    if resume {
        args.push("--resume".to_string());
    }
    run_msl_tool_command(&context.tools_exe, args)
}
fn ensure_simulation_parity_reference(
    summary: &MslSummary,
    force_refresh: bool,
    context: &ParityStepContext,
    sim_targets_path: &Path,
    sim_targets: &[String],
) -> io::Result<()> {
    let _sim_ref_watchdog = StageAbortWatchdog::new(
        "parity_simulation_reference",
        "RUMOCA_MSL_STAGE_TIMEOUT_PARITY_SIM_REF_SECS",
        1800,
    );
    let sim_policy = current_simulation_parity_cache_policy(context);
    let omc_simulation_reference = omc_simulation_reference_path();
    let sim_cache_key = simulation_parity_cache_key(
        sim_targets,
        &summary.msl_version,
        &context.omc_version,
        sim_policy,
    );
    let sim_cache_entry = parity_cache_entry_path("simulation", &sim_cache_key);
    let keyed_cache_matches = simulation_parity_cache_matches(
        &sim_cache_entry,
        sim_targets,
        &summary.msl_version,
        &context.omc_version,
        sim_policy,
    )?;
    if !force_refresh && keyed_cache_matches {
        materialize_simulation_parity_cache_entry(&sim_cache_entry, &omc_simulation_reference)?;
        println!(
            "MSL parity cache hit: reusing {} via keyed cache {} (refreshing Rumoca trace comparison via --resume)",
            omc_simulation_reference.display(),
            sim_cache_entry.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, true)?;
        persist_simulation_parity_cache_entry(&omc_simulation_reference, &sim_cache_entry)?;
        return Ok(());
    }
    let canonical_cache_matches =
        simulation_parity_cache_matches(
            &omc_simulation_reference,
            sim_targets,
            &summary.msl_version,
            &context.omc_version,
            sim_policy,
        )? && simulation_parity_cache_has_required_metrics(&omc_simulation_reference)?;
    if force_refresh || !canonical_cache_matches {
        println!(
            "MSL parity cache miss/incomplete for simulation reference; regenerating {}",
            omc_simulation_reference.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, false)?;
    } else {
        println!(
            "MSL parity cache hit: reusing {} (refreshing Rumoca trace comparison via --resume)",
            omc_simulation_reference.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, true)?;
    }
    persist_simulation_parity_cache_entry(&omc_simulation_reference, &sim_cache_entry)?;
    Ok(())
}
pub(super) fn ensure_required_msl_parity_references(summary: &MslSummary) -> io::Result<()> {
    if summary.sim_attempted == 0 {
        return Ok(());
    }
    let stage_start = Instant::now();
    let force_refresh = force_omc_parity_refresh_enabled();
    if force_refresh {
        println!(
            "MSL parity cache override active: forcing OMC parity regeneration via {}",
            FORCE_OMC_PARITY_REFRESH_ENV
        );
    }
    let (compile_targets_path, compile_targets, sim_targets_path, sim_targets) =
        load_parity_targets()?;
    let omc_version = match current_omc_version() {
        Ok(version) => version,
        Err(error) => {
            println!(
                "MSL parity stage: OMC unavailable; skipping parity reference generation ({error})"
            );
            return Ok(());
        }
    };
    let context = ParityStepContext {
        tools_exe: resolve_msl_tools_exe()?,
        omc_version,
        workers: omc_parity_workers(),
        omc_threads: omc_parity_threads(),
        compile_model_timeout_seconds: omc_compile_reference_model_timeout_seconds(),
        sim_batch_timeout_seconds: omc_sim_reference_batch_timeout_seconds(),
    };
    println!(
        "MSL parity targets: compile={} simulation={} (workers={}, compile_timeout={}s, sim_timeout={}s)",
        compile_targets.len(),
        sim_targets.len(),
        context.workers,
        context.compile_model_timeout_seconds,
        context.sim_batch_timeout_seconds
    );
    let compile_ref_start = Instant::now();
    ensure_compile_parity_reference(
        summary,
        force_refresh,
        &context,
        &compile_targets_path,
        &compile_targets,
    )?;
    println!(
        "MSL parity compile reference step: {:.2}s",
        compile_ref_start.elapsed().as_secs_f64()
    );
    let sim_ref_start = Instant::now();
    ensure_simulation_parity_reference(
        summary,
        force_refresh,
        &context,
        &sim_targets_path,
        &sim_targets,
    )?;
    println!(
        "MSL parity simulation reference step: {:.2}s",
        sim_ref_start.elapsed().as_secs_f64()
    );
    let _ = load_current_msl_parity_gate_input_required(sim_targets.len())?;
    println!(
        "MSL parity total step time: {:.2}s",
        stage_start.elapsed().as_secs_f64()
    );
    Ok(())
}
fn simulation_parity_cache_has_required_metrics(path: &Path) -> io::Result<bool> {
    if !path.is_file() {
        return Ok(false);
    }
    let parity = load_msl_parity_gate_input(path)?;
    let Some(runtime_stats) = parity.runtime_ratio_stats else {
        return Ok(false);
    };
    let Some(trace_stats) = parity.trace_accuracy_stats else {
        return Ok(false);
    };
    Ok(runtime_stats.system_ratio_both_success.sample_count > 0
        && runtime_stats.wall_ratio_both_success.sample_count > 0
        && trace_stats.models_compared > 0)
}
pub(super) fn current_msl_quality_baseline(
    summary: &MslSummary,
    parity_input: Option<&MslParityGateInput>,
) -> MslQualityBaseline {
    let gate_input = MslQualityGateInput::from(summary);
    MslQualityBaseline {
        git_commit: summary.git_commit.clone(),
        msl_version: gate_input.msl_version.to_string(),
        sim_timeout_seconds: SIM_TIMEOUT_SECS,
        simulatable_attempted: gate_input.simulatable_attempted,
        compiled_models: gate_input.compiled_models,
        balanced_models: gate_input.balanced_models,
        unbalanced_models: gate_input.unbalanced_models,
        partial_models: gate_input.partial_models,
        balance_denominator: gate_input.balance_denominator,
        initial_balanced_models: gate_input.initial_balanced_models,
        initial_unbalanced_models: gate_input.initial_unbalanced_models,
        sim_target_models: gate_input.sim_target_models,
        sim_attempted: gate_input.sim_attempted,
        sim_ok: gate_input.sim_ok,
        sim_success_rate: sim_success_rate(gate_input.sim_ok, gate_input.sim_attempted)
            .unwrap_or(0.0),
        // Runtime speed depends strongly on machine topology/workers; keep it informational
        // in parity output but do not commit it into quality baselines.
        runtime_context: None,
        runtime_ratio_stats: None,
        trace_accuracy_stats: parity_input.and_then(|parity| parity.trace_accuracy_stats.clone()),
    }
}
pub(super) fn write_current_msl_quality_snapshot(summary: &MslSummary) -> io::Result<()> {
    if summary.sim_attempted == 0 {
        return Ok(());
    }
    let parity_input =
        load_current_msl_parity_gate_input_optional(summary.sim_target_models.len())?;
    let baseline = current_msl_quality_baseline(summary, parity_input.as_ref());
    let baseline_path = msl_quality_current_path();
    if let Some(parent) = baseline_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let baseline_json = serde_json::to_string_pretty(&baseline)
        .map_err(|error| io::Error::other(format!("failed to serialize baseline JSON: {error}")))?;
    fs::write(&baseline_path, baseline_json)?;
    println!(
        "MSL current quality snapshot written to {}. Promote by copying to {} after approved runs.",
        baseline_path.display(),
        msl_quality_baseline_path().display()
    );
    Ok(())
}
pub(super) fn sim_rate_gate_override_enabled() -> bool {
    std::env::var(SIM_RATE_GATE_OVERRIDE_ENV).is_ok_and(|value| {
        value == "1"
            || value.eq_ignore_ascii_case("true")
            || value.eq_ignore_ascii_case("yes")
            || value.eq_ignore_ascii_case("on")
    })
}
pub(super) fn msl_quality_context_mismatch_reason(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) -> Option<String> {
    if baseline.msl_version != gate_input.msl_version {
        return Some(format!(
            "msl_version differs (baseline={}, current={})",
            baseline.msl_version, gate_input.msl_version
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
    push_sim_rate_regression_reason(&mut reasons, gate_input, baseline);
    push_trace_regression_reasons(&mut reasons, baseline, parity_input);
    reasons
}
pub(super) fn push_compile_balance_regression_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    let current_compile_rate =
        compile_success_rate(gate_input.compiled_models, gate_input.simulatable_attempted);
    let baseline_compile_rate =
        compile_success_rate(baseline.compiled_models, baseline.simulatable_attempted);
    if let (Some(current), Some(baseline_rate)) = (current_compile_rate, baseline_compile_rate) {
        let floor = (baseline_rate - COMPILE_RATE_GATE_TOLERANCE).max(0.0);
        if current + SIM_RATE_GATE_EPSILON < floor {
            reasons.push(format!(
                "compile success rate regressed: current={:.2}% ({}/{}) < floor={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                current * 100.0,
                gate_input.compiled_models,
                gate_input.simulatable_attempted,
                floor * 100.0,
                baseline_rate * 100.0,
                COMPILE_RATE_GATE_TOLERANCE * 100.0
            ));
        }
    }
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
pub(super) fn push_sim_rate_regression_reason(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    if gate_input.sim_attempted > 0 {
        let current_rate = sim_success_rate(gate_input.sim_ok, gate_input.sim_attempted)
            .expect("sim_attempted > 0 implies Some rate");
        let baseline_rate = baseline.sim_success_rate;
        let tolerated_floor = (baseline_rate - SIM_RATE_GATE_TOLERANCE).max(0.0);
        if current_rate + SIM_RATE_GATE_EPSILON < tolerated_floor {
            reasons.push(format!(
                "simulation success rate regressed beyond tolerance: current={:.2}% ({}/{}) < floor={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                current_rate * 100.0,
                gate_input.sim_ok,
                gate_input.sim_attempted,
                tolerated_floor * 100.0,
                baseline_rate * 100.0,
                SIM_RATE_GATE_TOLERANCE * 100.0
            ));
        }
    }
}
pub(super) fn push_runtime_ratio_regression_reasons(
    reasons: &mut Vec<String>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    let (Some(current_runtime), Some(baseline_runtime)) = (
        parity_input.and_then(|parity| parity.runtime_ratio_stats.as_ref()),
        baseline.runtime_ratio_stats.as_ref(),
    ) else {
        return;
    };
    let allowed_system_median = baseline_runtime.system_ratio_both_success.median
        * (1.0 - RUNTIME_RATIO_MEDIAN_REL_TOLERANCE);
    if current_runtime.system_ratio_both_success.median + SIM_RATE_GATE_EPSILON
        < allowed_system_median
    {
        reasons.push(format!(
            "runtime system speedup median regressed: current={:.6e} < floor={:.6e} (baseline={:.6e}, tolerance={:.1}%)",
            current_runtime.system_ratio_both_success.median,
            allowed_system_median,
            baseline_runtime.system_ratio_both_success.median,
            RUNTIME_RATIO_MEDIAN_REL_TOLERANCE * 100.0
        ));
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
fn trace_bad_channels_percent(stats: &MslTraceAccuracyStatsBaseline) -> Option<f64> {
    stats.bad_channels_percent
}
fn trace_severe_channels_total(stats: &MslTraceAccuracyStatsBaseline) -> Option<usize> {
    stats.severe_channels_total
}
fn trace_severe_channels_percent(stats: &MslTraceAccuracyStatsBaseline) -> Option<f64> {
    stats.severe_channels_percent
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
        if let (Some(current), Some(baseline_pct)) = (
            trace_model_bucket_percentages(current_trace),
            trace_model_bucket_percentages(baseline_trace),
        ) {
            let high_floor = (baseline_pct.high - TRACE_HIGH_PERCENT_DROP_TOLERANCE_PP).max(0.0);
            if current.high + SIM_RATE_GATE_EPSILON < high_floor {
                reasons.push(format!(
                    "trace high-agreement model share regressed: current={:.2}% < floor={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                    current.high,
                    high_floor,
                    baseline_pct.high,
                    TRACE_HIGH_PERCENT_DROP_TOLERANCE_PP
                ));
            }
            // Treat `high + near` as the acceptable band so improvements from
            // near-agreement into high-agreement do not fail the gate.
            let acceptable_floor = ((baseline_pct.high + baseline_pct.near)
                - TRACE_ACCEPTABLE_PERCENT_DROP_TOLERANCE_PP)
                .max(0.0);
            if (current.high + current.near) + SIM_RATE_GATE_EPSILON < acceptable_floor {
                reasons.push(format!(
                    "trace acceptable-agreement model share regressed: current={:.2}% < floor={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                    current.high + current.near,
                    acceptable_floor,
                    baseline_pct.high + baseline_pct.near,
                    TRACE_ACCEPTABLE_PERCENT_DROP_TOLERANCE_PP
                ));
            }
            let deviation_ceiling =
                (baseline_pct.deviation + TRACE_DEVIATION_PERCENT_INCREASE_TOLERANCE_PP).min(100.0);
            if current.deviation > deviation_ceiling + SIM_RATE_GATE_EPSILON {
                reasons.push(format!(
                    "trace deviation model share regressed: current={:.2}% > ceiling={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                    current.deviation,
                    deviation_ceiling,
                    baseline_pct.deviation,
                    TRACE_DEVIATION_PERCENT_INCREASE_TOLERANCE_PP
                ));
            }
        }
        if let (Some(current_any), Some(baseline_any)) = (
            trace_models_with_any_channel_deviation_percent(current_trace),
            trace_models_with_any_channel_deviation_percent(baseline_trace),
        ) {
            let any_ceiling = (baseline_any
                + TRACE_ANY_CHANNEL_DEVIATION_PERCENT_INCREASE_TOLERANCE_PP)
                .min(100.0);
            if current_any > any_ceiling + SIM_RATE_GATE_EPSILON {
                reasons.push(format!(
                    "trace models-with-any-bad-channel share regressed: current={:.2}% > ceiling={:.2}% (baseline={:.2}%, tolerance={:.2}pp)",
                    current_any,
                    any_ceiling,
                    baseline_any,
                    TRACE_ANY_CHANNEL_DEVIATION_PERCENT_INCREASE_TOLERANCE_PP
                ));
            }
        }
        push_trace_channel_regression_reason(
            reasons,
            "bad",
            trace_bad_channels_percent(current_trace),
            trace_bad_channels_percent(baseline_trace),
            TRACE_BAD_CHANNEL_PERCENT_INCREASE_TOLERANCE_PP,
            trace_bad_channels_total(current_trace),
            trace_bad_channels_total(baseline_trace),
        );
        push_trace_channel_regression_reason(
            reasons,
            "severe",
            trace_severe_channels_percent(current_trace),
            trace_severe_channels_percent(baseline_trace),
            TRACE_SEVERE_CHANNEL_PERCENT_INCREASE_TOLERANCE_PP,
            trace_severe_channels_total(current_trace),
            trace_severe_channels_total(baseline_trace),
        );
        if current_trace.models_compared + TRACE_MODELS_COMPARED_ALLOWED_DROP
            < baseline_trace.models_compared
        {
            reasons.push(format!(
                "trace model coverage regressed: current models_compared={} < baseline={} (allowed_drop={})",
                current_trace.models_compared,
                baseline_trace.models_compared,
                TRACE_MODELS_COMPARED_ALLOWED_DROP
            ));
        }
    }
}
fn push_trace_channel_regression_reason(
    reasons: &mut Vec<String>,
    channel_label: &str,
    current_percent: Option<f64>,
    baseline_percent: Option<f64>,
    percent_tolerance_pp: f64,
    current_total: Option<usize>,
    baseline_total: Option<usize>,
) {
    if let (Some(current), Some(baseline)) = (current_percent, baseline_percent) {
        let ceiling = (baseline + percent_tolerance_pp).min(100.0);
        if current > ceiling + SIM_RATE_GATE_EPSILON {
            reasons.push(format!(
                "trace {channel_label} channel share regressed: current={current:.2}% > ceiling={ceiling:.2}% (baseline={baseline:.2}%, tolerance={percent_tolerance_pp:.2}pp)"
            ));
        }
        return;
    }
    if let (Some(current), Some(baseline)) = (current_total, baseline_total)
        && current > baseline
    {
        reasons.push(format!(
            "trace {channel_label} channel count regressed: current={current} > baseline={baseline}"
        ));
    }
}
pub(super) fn msl_quality_gate_failure_message(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) -> Option<String> {
    if let Some(reason) = msl_quality_context_mismatch_reason(gate_input, baseline) {
        return Some(format!(
            "MSL quality baseline context mismatch: {reason}. Update {} only with explicit review approval",
            MSL_QUALITY_BASELINE_FILE_REL
        ));
    }
    let reasons = msl_quality_regression_reasons(gate_input, baseline, parity_input);
    if reasons.is_empty() {
        return None;
    }
    Some(reasons.join("; "))
}
pub(super) fn enforce_msl_quality_gate(summary: &MslSummary) -> io::Result<()> {
    if summary.sim_attempted == 0 {
        println!("MSL quality gate: skipped for compile/balance-only run.");
        return Ok(());
    }
    if should_skip_msl_quality_gate() {
        println!(
            "MSL quality gate: skipped for non-baseline run (focused subset or non-default RUMOCA_MSL_SIM_SET)."
        );
        return Ok(());
    }
    assert_valid_msl_summary(summary);
    let gate_input = MslQualityGateInput::from(summary);
    let baseline_path = msl_quality_baseline_path();
    let baseline = load_msl_quality_baseline(&baseline_path)?;
    let parity_input =
        load_current_msl_parity_gate_input_optional(summary.sim_target_models.len())?;
    let gate_failure =
        msl_quality_gate_failure_message(gate_input, &baseline, parity_input.as_ref());
    if let Some(message) = gate_failure {
        if sim_rate_gate_override_enabled() {
            println!(
                "MSL quality gate: OVERRIDDEN by {} ({}).",
                SIM_RATE_GATE_OVERRIDE_ENV, message
            );
            return Ok(());
        }
        panic!(
            "MSL quality gate: {}. Set {}=1 only for explicitly approved regressions.",
            message, SIM_RATE_GATE_OVERRIDE_ENV
        );
    }
    print_compile_and_sim_gate_pass(gate_input, &baseline);
    print_trace_gate_status(&baseline, parity_input.as_ref());
    print_runtime_ratio_status(&baseline, parity_input.as_ref());
    println!("MSL quality baseline source: {}", baseline_path.display());
    Ok(())
}
pub(super) fn should_skip_msl_quality_gate() -> bool {
    sim_targets_file_override().is_some()
        || !sim_subset_patterns().is_empty()
        || sim_subset_limit().is_some()
        || sim_set_mode() != SimSetMode::Short
}
pub(super) fn assert_valid_msl_summary(summary: &MslSummary) {
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
    if !should_skip_msl_quality_gate()
        && summary.sim_target_models.len() == SIM_SET_LIMIT_DEFAULT
        && summary.sim_attempted > 0
    {
        let required_sim_ok = ((summary.sim_target_models.len() as f64)
            * DEFAULT_SIM_OK_HARD_FLOOR_RATIO)
            .ceil() as usize;
        assert!(
            summary.sim_ok >= required_sim_ok,
            "MSL quality gate: invalid run (sim_ok below hard floor). \
             Default {}-model simulation run produced only {}/{} successful simulations; \
             required at least {} successful simulations ({:.1}% floor).",
            SIM_SET_LIMIT_DEFAULT,
            summary.sim_ok,
            summary.sim_target_models.len(),
            required_sim_ok,
            DEFAULT_SIM_OK_HARD_FLOOR_RATIO * 100.0
        );
    }
}
pub(super) fn print_compile_and_sim_gate_pass(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    let compile_rate =
        compile_success_rate(gate_input.compiled_models, gate_input.simulatable_attempted)
            .unwrap_or(0.0)
            * 100.0;
    let baseline_compile_rate =
        compile_success_rate(baseline.compiled_models, baseline.simulatable_attempted)
            .unwrap_or(0.0)
            * 100.0;
    let balance_rate =
        balance_success_rate(gate_input.balanced_models, gate_input.balance_denominator)
            .unwrap_or(0.0)
            * 100.0;
    let baseline_balance_rate =
        balance_success_rate(baseline.balanced_models, baseline.balance_denominator).unwrap_or(0.0)
            * 100.0;
    let initial_balance_rate = balance_success_rate(
        gate_input.initial_balanced_models,
        gate_input.balance_denominator,
    )
    .unwrap_or(0.0)
        * 100.0;
    let baseline_initial_balance_rate = balance_success_rate(
        baseline.initial_balanced_models,
        baseline.balance_denominator,
    )
    .unwrap_or(0.0)
        * 100.0;
    println!(
        "MSL quality gate: PASS compile={:.2}% (baseline={:.2}%), balance={:.2}% (baseline={:.2}%), initial_balance={:.2}% (baseline={:.2}%).",
        compile_rate,
        baseline_compile_rate,
        balance_rate,
        baseline_balance_rate,
        initial_balance_rate,
        baseline_initial_balance_rate
    );
    if gate_input.sim_attempted > 0 {
        let current_rate = sim_success_rate(gate_input.sim_ok, gate_input.sim_attempted)
            .expect("sim_attempted > 0 implies Some rate");
        println!(
            "MSL simulation gate: PASS current={:.2}% ({}/{}), baseline={:.2}% ({}/{}, commit={}), tolerance={:.2}pp.",
            current_rate * 100.0,
            gate_input.sim_ok,
            gate_input.sim_attempted,
            baseline.sim_success_rate * 100.0,
            baseline.sim_ok,
            baseline.sim_attempted,
            baseline.git_commit,
            SIM_RATE_GATE_TOLERANCE * 100.0
        );
    } else {
        println!("MSL simulation gate: skipped (no simulations attempted in this run).");
    }
}
pub(super) fn print_trace_gate_status(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    if let (Some(current_trace), Some(baseline_trace)) = (
        parity_input.and_then(|parity| parity.trace_accuracy_stats.as_ref()),
        baseline.trace_accuracy_stats.as_ref(),
    ) {
        let current_pct =
            trace_model_bucket_percentages(current_trace).unwrap_or(TraceBucketPercentages {
                high: 0.0,
                near: 0.0,
                deviation: 0.0,
            });
        let baseline_pct =
            trace_model_bucket_percentages(baseline_trace).unwrap_or(TraceBucketPercentages {
                high: 0.0,
                near: 0.0,
                deviation: 0.0,
            });
        let current_any =
            trace_models_with_any_channel_deviation_percent(current_trace).unwrap_or(0.0);
        let baseline_any =
            trace_models_with_any_channel_deviation_percent(baseline_trace).unwrap_or(0.0);
        println!("MSL trace gate: PASS with baseline:");
        println!(
            "  high={:.2}% (baseline={:.2}%), near={:.2}% (baseline={:.2}%), deviation={:.2}% (baseline={:.2}%)",
            current_pct.high,
            baseline_pct.high,
            current_pct.near,
            baseline_pct.near,
            current_pct.deviation,
            baseline_pct.deviation,
        );
        println!(
            "  models_with_any_bad_channel={:.2}% (baseline={:.2}%), models_compared={} (baseline={})",
            current_any,
            baseline_any,
            current_trace.models_compared,
            baseline_trace.models_compared
        );
        if let (Some(current_bad), Some(baseline_bad)) = (
            trace_bad_channels_total(current_trace),
            trace_bad_channels_total(baseline_trace),
        ) {
            let current_severe = trace_severe_channels_total(current_trace).unwrap_or(0);
            let baseline_severe = trace_severe_channels_total(baseline_trace).unwrap_or(0);
            let current_mass = trace_violation_mass_total(current_trace).unwrap_or(0.0);
            let baseline_mass = trace_violation_mass_total(baseline_trace).unwrap_or(0.0);
            println!(
                "  bad_channels={} (baseline={}), severe_channels={} (baseline={}), violation_mass_total={:.6e} (baseline={:.6e})",
                current_bad,
                baseline_bad,
                current_severe,
                baseline_severe,
                current_mass,
                baseline_mass
            );
        }
        return;
    }
    if baseline.trace_accuracy_stats.is_some() {
        println!(
            "MSL trace gate: skipped (missing {}). Run `cargo run -p rumoca-tool-dev --bin rumoca-msl-tools -- omc-simulation-reference ...` to enforce trace baseline.",
            omc_simulation_reference_path().display()
        );
    }
}
fn fmt_opt_usize(value: Option<usize>) -> String {
    value
        .map(|v| v.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}
pub(super) fn print_runtime_ratio_status(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    let Some(current_runtime) = parity_input.and_then(|parity| parity.runtime_ratio_stats.as_ref())
    else {
        return;
    };
    let current_workers = parity_input
        .and_then(|parity| parity.runtime_context.as_ref())
        .and_then(|context| context.workers_used);
    let current_omc_threads = parity_input
        .and_then(|parity| parity.runtime_context.as_ref())
        .and_then(|context| context.omc_threads);
    if let Some(baseline_runtime) = baseline.runtime_ratio_stats.as_ref() {
        println!(
            "MSL speed metrics (informational only, not gated): system_median={:.3e} (baseline={:.3e}), wall_median={:.3e} (baseline={:.3e}), workers={}, omc_threads={}.",
            current_runtime.system_ratio_both_success.median,
            baseline_runtime.system_ratio_both_success.median,
            current_runtime.wall_ratio_both_success.median,
            baseline_runtime.wall_ratio_both_success.median,
            fmt_opt_usize(current_workers),
            fmt_opt_usize(current_omc_threads)
        );
        return;
    }
    println!(
        "MSL speed metrics (informational only, not gated): system_median={:.3e}, wall_median={:.3e}, workers={}, omc_threads={}.",
        current_runtime.system_ratio_both_success.median,
        current_runtime.wall_ratio_both_success.median,
        fmt_opt_usize(current_workers),
        fmt_opt_usize(current_omc_threads)
    );
}
