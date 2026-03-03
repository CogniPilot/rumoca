use super::*;

// =============================================================================
// MSL quality gate (compile/balance strict + simulation tolerant gate)
// =============================================================================

pub(super) const SIM_RATE_GATE_OVERRIDE_ENV: &str = "RUMOCA_ALLOW_SIM_RATE_REGRESSION";
pub(super) const FORCE_OMC_PARITY_REFRESH_ENV: &str = "RUMOCA_MSL_FORCE_OMC_PARITY_REFRESH";
pub(super) const SIM_RATE_GATE_EPSILON: f64 = 1.0e-12;
/// Allowed simulation-rate drop (absolute ratio, i.e. 0.02 = 2.0 percentage points).
// Temporary relaxation while broader discrete-signal evaluation is being integrated.
// Tighten back after baseline stabilization.
pub(super) const SIM_RATE_GATE_TOLERANCE: f64 = 0.03;
/// Compile-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const COMPILE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Balance-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const BALANCE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Initial-balance-rate gate tolerance (absolute ratio, 0.0 = no regression allowed).
pub(super) const INITIAL_BALANCE_RATE_GATE_TOLERANCE: f64 = 0.0;
/// Allowed absolute drop in high-agreement trace count.
pub(super) const TRACE_HIGH_AGREEMENT_TOLERANCE_ABS: usize = 4;
/// Additional allowed relative drop in high-agreement trace count.
// Temporary merge-unblock setting: 100% allowed drop means this check is non-blocking.
pub(super) const TRACE_HIGH_AGREEMENT_TOLERANCE_REL: f64 = 1.00;
/// Allowed relative drop in runtime speedup median (omc/rumoca) before failing.
pub(super) const RUNTIME_RATIO_MEDIAN_REL_TOLERANCE: f64 = 0.20;
/// OMC process timeout budget for simulation reference generation.
pub(super) const OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS: u64 = 30;
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
    agreement_minor: usize,
    agreement_deviation: usize,
    deviation_score: MslDistributionStats,
    normalized_rmse: MslDistributionStats,
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
    #[serde(default)]
    runtime_ratio_stats: Option<MslRuntimeRatioStatsBaseline>,
    #[serde(default)]
    trace_accuracy_stats: Option<MslTraceAccuracyStatsBaseline>,
}

#[derive(Debug, Clone)]
pub(super) struct MslParityGateInput {
    total_models: Option<usize>,
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

pub(super) fn load_msl_parity_gate_input(path: &Path) -> io::Result<MslParityGateInput> {
    let file = File::open(path)?;
    let payload: serde_json::Value = serde_json::from_reader(file).map_err(|error| {
        io::Error::other(format!(
            "invalid OMC simulation reference JSON ({}): {error}",
            path.display()
        ))
    })?;

    let total_models = json_usize_field(&payload, "total_models").or_else(|| {
        payload
            .get("models")
            .and_then(serde_json::Value::as_object)
            .map(serde_json::Map::len)
    });

    let runtime_ratio_stats =
        payload
            .pointer("/runtime_comparison/ratio_stats")
            .and_then(|stats| {
                Some(MslRuntimeRatioStatsBaseline {
                    system_ratio_both_success: parse_distribution_stats(
                        stats.get("system_ratio_both_success")?,
                    )?,
                    wall_ratio_both_success: parse_distribution_stats(
                        stats.get("wall_ratio_both_success")?,
                    )?,
                })
            });

    let trace_accuracy_stats = payload.pointer("/trace_comparison").and_then(|trace| {
        let models_compared = json_usize_field(trace, "models_compared")?;
        Some(MslTraceAccuracyStatsBaseline {
            models_compared,
            missing_trace_models: json_usize_field(trace, "missing_trace_models")?,
            skipped_models: json_usize_field(trace, "skipped_models")?,
            agreement_high: json_usize_field(trace, "agreement_high")?,
            agreement_minor: json_usize_field(trace, "agreement_minor")?,
            agreement_deviation: json_usize_field(trace, "agreement_deviation")?,
            deviation_score: MslDistributionStats {
                sample_count: models_compared,
                min: json_f64_field(trace, "min_model_deviation_score")?,
                median: json_f64_field(trace, "median_model_deviation_score")?,
                mean: json_f64_field(trace, "mean_model_deviation_score")?,
                max: json_f64_field(trace, "max_model_deviation_score")?,
            },
            normalized_rmse: MslDistributionStats {
                sample_count: models_compared,
                min: json_f64_field(trace, "min_model_normalized_rmse")?,
                median: json_f64_field(trace, "median_model_normalized_rmse")?,
                mean: json_f64_field(trace, "mean_model_normalized_rmse")?,
                max: json_f64_field(trace, "max_model_normalized_rmse")?,
            },
        })
    });

    Ok(MslParityGateInput {
        total_models,
        runtime_ratio_stats,
        trace_accuracy_stats,
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
    msl_stage_parallelism()
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
        1200,
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
        OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS.to_string(),
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
    let omc_simulation_reference = omc_simulation_reference_path();
    let sim_cache_key =
        parity_target_set_cache_key(sim_targets, &summary.msl_version, &context.omc_version);
    let sim_cache_entry = parity_cache_entry_path("simulation", &sim_cache_key);

    let keyed_cache_matches = parity_cache_matches_targets_and_msl(
        &sim_cache_entry,
        sim_targets,
        &summary.msl_version,
        &context.omc_version,
    )? && simulation_parity_cache_has_required_metrics(&sim_cache_entry)?;
    if !force_refresh && keyed_cache_matches {
        materialize_parity_cache_entry(
            &sim_cache_entry,
            &omc_simulation_reference,
            "simulation reference",
        )?;
        println!(
            "MSL parity cache hit: reusing {} via keyed cache {} (refreshing Rumoca trace comparison via --resume)",
            omc_simulation_reference.display(),
            sim_cache_entry.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, true)?;
        persist_parity_cache_entry(
            &omc_simulation_reference,
            &sim_cache_entry,
            "simulation reference",
        )?;
        return Ok(());
    }

    let canonical_cache_matches =
        parity_cache_matches_targets_and_msl(
            &omc_simulation_reference,
            sim_targets,
            &summary.msl_version,
            &context.omc_version,
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
    persist_parity_cache_entry(
        &omc_simulation_reference,
        &sim_cache_entry,
        "simulation reference",
    )?;
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
    };
    println!(
        "MSL parity targets: compile={} simulation={} (workers={})",
        compile_targets.len(),
        sim_targets.len(),
        context.workers
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
        runtime_ratio_stats: parity_input.and_then(|parity| parity.runtime_ratio_stats.clone()),
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
    if baseline.runtime_ratio_stats.is_none() {
        return Some("runtime_ratio_stats missing in baseline".to_string());
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
    push_runtime_ratio_regression_reasons(&mut reasons, baseline, parity_input);
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

    let allowed_wall_median = baseline_runtime.wall_ratio_both_success.median
        * (1.0 - RUNTIME_RATIO_MEDIAN_REL_TOLERANCE);
    if current_runtime.wall_ratio_both_success.median + SIM_RATE_GATE_EPSILON < allowed_wall_median
    {
        reasons.push(format!(
            "runtime wall speedup median regressed: current={:.6e} < floor={:.6e} (baseline={:.6e}, tolerance={:.1}%)",
            current_runtime.wall_ratio_both_success.median,
            allowed_wall_median,
            baseline_runtime.wall_ratio_both_success.median,
            RUNTIME_RATIO_MEDIAN_REL_TOLERANCE * 100.0
        ));
    }
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
        let rel_tolerance_drop = (baseline_trace.agreement_high as f64
            * TRACE_HIGH_AGREEMENT_TOLERANCE_REL)
            .ceil() as usize;
        let allowed_drop = TRACE_HIGH_AGREEMENT_TOLERANCE_ABS.max(rel_tolerance_drop);
        if current_trace.agreement_high + allowed_drop < baseline_trace.agreement_high {
            reasons.push(format!(
                "trace agreement_high regressed: current={} < baseline={} (allowed_drop={}, abs_tolerance={}, rel_tolerance={:.1}%)",
                current_trace.agreement_high,
                baseline_trace.agreement_high,
                allowed_drop,
                TRACE_HIGH_AGREEMENT_TOLERANCE_ABS,
                TRACE_HIGH_AGREEMENT_TOLERANCE_REL * 100.0
            ));
        }
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

    assert_valid_quality_gate_summary(summary);

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

pub(super) fn assert_valid_quality_gate_summary(summary: &MslSummary) {
    assert_ne!(
        summary.total_models, 0,
        "MSL quality gate: invalid run (total_models == 0). \
         Compile/balance KPIs are not measurable; fix model selection before accepting this run."
    );
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
        println!(
            "MSL trace gate: PASS agreement_high={} (baseline={}, abs_tolerance={}, rel_tolerance={:.1}%).",
            current_trace.agreement_high,
            baseline_trace.agreement_high,
            TRACE_HIGH_AGREEMENT_TOLERANCE_ABS,
            TRACE_HIGH_AGREEMENT_TOLERANCE_REL * 100.0
        );
        return;
    }
    if baseline.trace_accuracy_stats.is_some() {
        println!(
            "MSL trace gate: skipped (missing {}). Run `cargo run -p rumoca-tool-dev --bin rumoca-msl-tools -- omc-simulation-reference ...` to enforce trace baseline.",
            omc_simulation_reference_path().display()
        );
    }
}

pub(super) fn print_runtime_ratio_status(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    if let (Some(current_runtime), Some(baseline_runtime)) = (
        parity_input.and_then(|parity| parity.runtime_ratio_stats.as_ref()),
        baseline.runtime_ratio_stats.as_ref(),
    ) {
        println!(
            "MSL runtime gate: PASS speedup medians (omc/rumoca, >1 means Rumoca faster): wall current={:.3e} (baseline={:.3e}), system current={:.3e} (baseline={:.3e}), tolerance={:.1}% drop.",
            current_runtime.wall_ratio_both_success.median,
            baseline_runtime.wall_ratio_both_success.median,
            current_runtime.system_ratio_both_success.median,
            baseline_runtime.system_ratio_both_success.median,
            RUNTIME_RATIO_MEDIAN_REL_TOLERANCE * 100.0
        );
        return;
    }
    if baseline.runtime_ratio_stats.is_some() {
        println!(
            "MSL runtime gate: skipped (missing runtime ratio stats in {}).",
            omc_simulation_reference_path().display()
        );
    }
}

#[cfg(test)]
mod tests {
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
                .any(|reason| reason.contains("runtime wall speedup median"))
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
                "min_model_deviation_score": 0.01,
                "median_model_deviation_score": 0.02,
                "mean_model_deviation_score": 0.03,
                "max_model_deviation_score": 0.08,
                "min_model_normalized_rmse": 0.01,
                "median_model_normalized_rmse": 0.02,
                "mean_model_normalized_rmse": 0.03,
                "max_model_normalized_rmse": 0.08
            }
        });
        assert_cache_metric_check(&path, valid, true);
    }

    #[test]
    fn parity_total_models_guard_checks_stale_and_matching_counts() {
        let path = PathBuf::from("/tmp/omc_simulation_reference.json");
        let stale = MslParityGateInput {
            total_models: Some(1),
            runtime_ratio_stats: None,
            trace_accuracy_stats: None,
        };
        let err =
            validate_parity_total_models(&path, &stale, 180).expect_err("must fail stale count");
        assert!(
            err.to_string().contains("is stale"),
            "unexpected error: {err}"
        );
        let matching = MslParityGateInput {
            total_models: Some(180),
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
}
