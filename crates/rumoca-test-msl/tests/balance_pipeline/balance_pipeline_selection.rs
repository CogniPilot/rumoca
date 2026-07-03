use super::*;
#[cfg(target_os = "linux")]
use std::collections::BTreeSet;

// =============================================================================
// Focused simulation target selection and subset controls
// =============================================================================

const MSL_HEADROOM_CORE_THRESHOLD: usize = 16;
const MSL_RESERVED_CORES_ON_LARGE_HOSTS: usize = 2;
/// Default resident-memory estimate per persistent MSL model-worker slot.
pub(super) const MSL_COMPILE_MODEL_MEMORY_MB_DEFAULT: usize = 1536;
const MSL_COMPILE_MODEL_MEMORY_MB_MIN: usize = 128;
/// Default memory left to the OS, desktop, and filesystem cache during MSL gates.
pub(super) const MSL_MEMORY_RESERVED_MB_DEFAULT: usize = 8192;
/// Optional total memory budget for retained compile results (MB).
/// Optional total memory budget for all concurrent sim workers (MB).
/// Default number of models in explicit short/long simulation sets.
pub(super) const SIM_SET_LIMIT_DEFAULT: usize = 180;
pub(super) const DEFAULT_SIM_TARGETS_FILE_REL: &str = "tests/msl_tests/msl_simulation_targets.json";
const MODELICA_TEST_TARGETS_FILE_REL: &str = "tests/msl_tests/modelica_test_targets_ci.json";
const GENERATED_SIM_TARGETS_FILE_REL: &str = "msl_simulation_targets.json";
const PRIOR_RESULTS_FILE_REL: &str = "msl_results.json";
const MIN_PRIOR_COMPLEXITY_RANKED_MODELS: usize = 32;
const MIN_PRIOR_COMPLEXITY_COVERAGE_RATIO: f64 = 0.10;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct CompileMemoryBudget {
    pub(super) budget_mb: usize,
    pub(super) per_model_mb: usize,
    pub(super) model_cap: usize,
    pub(super) source: CompileMemoryBudgetSource,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CompileMemoryBudgetSource {
    HostAvailable,
}

impl CompileMemoryBudgetSource {
    pub(super) fn as_str(self) -> &'static str {
        match self {
            Self::HostAvailable => "/proc/meminfo MemAvailable",
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
struct PriorModelComplexity {
    num_states: usize,
    num_algebraics: usize,
    num_f_x: usize,
    scalar_equations: usize,
    scalar_unknowns: usize,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct PriorModelScheduleMetrics {
    complexity: PriorModelComplexity,
    compile_millis: Option<u64>,
    timed_out: bool,
}

#[derive(Debug, Deserialize)]
struct PriorResultsSummary {
    #[serde(default)]
    model_results: Vec<PriorResultRecord>,
}

#[derive(Debug, Deserialize)]
struct PriorResultRecord {
    model_name: String,
    #[serde(default)]
    num_states: Option<usize>,
    #[serde(default)]
    num_algebraics: Option<usize>,
    #[serde(default)]
    num_f_x: Option<usize>,
    #[serde(default)]
    scalar_equations: Option<usize>,
    #[serde(default)]
    scalar_unknowns: Option<usize>,
    #[serde(default)]
    compile_seconds: Option<f64>,
    #[serde(default)]
    timeout_phase: Option<String>,
}

fn parse_mem_available_mb(meminfo: &str) -> Option<usize> {
    meminfo.lines().find_map(|line| {
        let rest = line.strip_prefix("MemAvailable:")?;
        let kb = rest.split_whitespace().next()?.parse::<usize>().ok()?;
        Some(kb / 1024)
    })
}

fn host_available_memory_mb() -> Option<usize> {
    std::fs::read_to_string("/proc/meminfo")
        .ok()
        .and_then(|contents| parse_mem_available_mb(&contents))
}

pub(super) fn compile_model_memory_mb() -> usize {
    MSL_COMPILE_MODEL_MEMORY_MB_DEFAULT
}

fn prior_model_complexity_score(complexity: PriorModelComplexity) -> usize {
    complexity
        .scalar_equations
        .max(complexity.scalar_unknowns)
        .saturating_add(complexity.num_f_x)
        .saturating_add(complexity.num_algebraics.saturating_mul(2))
        .saturating_add(complexity.num_states.saturating_mul(4))
}

fn prior_compile_millis(compile_seconds: Option<f64>) -> Option<u64> {
    let seconds = compile_seconds?;
    seconds
        .is_finite()
        .then_some(seconds)
        .filter(|seconds| *seconds > 0.0)
        .map(|seconds| (seconds * 1000.0).round() as u64)
}

fn is_compile_timeout_phase(phase: Option<&str>) -> bool {
    matches!(
        phase,
        Some("Compile" | "Instantiate" | "Typecheck" | "Flatten" | "ToDae")
    )
}

fn prior_model_schedule_score(metrics: PriorModelScheduleMetrics) -> u64 {
    metrics
        .compile_millis
        .unwrap_or_else(|| prior_model_complexity_score(metrics.complexity) as u64)
}

pub(super) fn compile_model_memory_mb_from_complexity_score(
    complexity_score: usize,
    fallback_mb: usize,
) -> usize {
    let fallback_mb = fallback_mb.max(1);
    let quarter_mb = (fallback_mb / 4).max(MSL_COMPILE_MODEL_MEMORY_MB_MIN);
    let half_mb = (fallback_mb / 2).max(quarter_mb);
    if complexity_score == 0 {
        fallback_mb
    } else if complexity_score < 100 {
        quarter_mb
    } else if complexity_score < 1_000 {
        half_mb
    } else if complexity_score < 5_000 {
        fallback_mb
    } else {
        fallback_mb.saturating_mul(2)
    }
}

fn compile_model_memory_mb_from_complexity(
    complexity: PriorModelComplexity,
    fallback_mb: usize,
) -> usize {
    compile_model_memory_mb_from_complexity_score(
        prior_model_complexity_score(complexity),
        fallback_mb,
    )
}

pub(super) fn compile_model_memory_costs_for_names(names: &[String]) -> HashMap<String, usize> {
    let fallback_mb = compile_model_memory_mb();
    let complexities = prior_results_file().and_then(|path| load_prior_model_complexities(&path));
    names
        .iter()
        .map(|name| {
            let cost_mb = complexities
                .as_ref()
                .and_then(|entries| entries.get(name).copied())
                .map(|complexity| compile_model_memory_mb_from_complexity(complexity, fallback_mb))
                .unwrap_or(fallback_mb);
            (name.clone(), cost_mb)
        })
        .collect()
}

fn reserved_memory_mb() -> usize {
    MSL_MEMORY_RESERVED_MB_DEFAULT
}

fn compile_memory_budget_mb_from_available(available_mb: usize, reserved_mb: usize) -> usize {
    if available_mb > reserved_mb {
        available_mb - reserved_mb
    } else {
        available_mb / 2
    }
    .max(1)
}

fn compile_memory_budget_mb() -> Option<(usize, CompileMemoryBudgetSource)> {
    host_available_memory_mb().map(|available_mb| {
        (
            compile_memory_budget_mb_from_available(available_mb, reserved_memory_mb()),
            CompileMemoryBudgetSource::HostAvailable,
        )
    })
}

fn compile_model_cap_from_budget(budget_mb: usize, per_model_mb: usize) -> usize {
    (budget_mb / per_model_mb.max(1)).max(1)
}

pub(super) fn compile_memory_budget() -> Option<CompileMemoryBudget> {
    let per_model_mb = compile_model_memory_mb();
    compile_memory_budget_mb().map(|(budget_mb, source)| CompileMemoryBudget {
        budget_mb,
        per_model_mb,
        model_cap: compile_model_cap_from_budget(budget_mb, per_model_mb),
        source,
    })
}

pub(super) fn compile_stage_parallelism() -> usize {
    let requested_threads = msl_stage_parallelism();
    let Some(memory_budget) = compile_memory_budget() else {
        return requested_threads;
    };
    let effective_threads = requested_threads.min(memory_budget.model_cap).max(1);
    if effective_threads < requested_threads {
        println!(
            "MSL compile parallelism capped by memory budget: {effective_threads} workers (requested {requested_threads}, budget {} MB via {}, model estimate {} MB)",
            memory_budget.budget_mb,
            memory_budget.source.as_str(),
            memory_budget.per_model_mb
        );
    }
    effective_threads
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SimSetMode {
    /// Fast development set: first N root examples in selected target order.
    Short,
    /// Complementary stress set: last N root examples in selected target order.
    Long,
    /// Full selected explicit example set.
    Full,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum MslTargetScope {
    /// Committed explicit simulation target list.
    DefaultSimulationTargets,
    /// Baseline scope: root models matching `Modelica.*.Examples.*`.
    RootExamples,
}

impl MslTargetScope {
    fn uses_default_target_file(self) -> bool {
        matches!(self, Self::DefaultSimulationTargets)
    }

    pub(super) fn as_str(self) -> &'static str {
        match self {
            Self::DefaultSimulationTargets => "default-simulation-targets",
            Self::RootExamples => "root-examples",
        }
    }
}

/// MSL target scope (default: root examples). The parity config can select
/// `default-simulation-targets` to run the committed target file instead.
pub(super) fn msl_target_scope() -> MslTargetScope {
    match parity_config().target_scope.as_deref() {
        None | Some("root-examples") => MslTargetScope::RootExamples,
        Some("default-simulation-targets") => MslTargetScope::DefaultSimulationTargets,
        Some(other) => panic!(
            "invalid target_scope '{other}' in MSL parity config (expected 'root-examples' or 'default-simulation-targets')"
        ),
    }
}

impl std::fmt::Display for SimSetMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimSetMode::Short => write!(f, "short"),
            SimSetMode::Long => write!(f, "long"),
            SimSetMode::Full => write!(f, "full"),
        }
    }
}

/// Simulation set to run (default: full). The parity config can select
/// `short`/`long` to run a curated lexical subset.
pub(super) fn sim_set_mode() -> SimSetMode {
    match parity_config().sim_set.as_deref() {
        None | Some("full") => SimSetMode::Full,
        Some("short") => SimSetMode::Short,
        Some("long") => SimSetMode::Long,
        Some(other) => panic!(
            "invalid sim_set '{other}' in MSL parity config (expected 'full', 'short', or 'long')"
        ),
    }
}

pub(super) fn sim_set_limit() -> usize {
    parity_config()
        .sim_set_limit
        .unwrap_or(SIM_SET_LIMIT_DEFAULT)
}

/// Whether every selected simulation target must succeed. Used by the focused
/// CI ModelicaTest gate (which runs an explicit target file and therefore skips
/// the baseline-relative quality gate) to still fail on any simulation failure.
pub(super) fn require_selected_targets_success() -> bool {
    parity_config()
        .require_selected_targets_success
        .unwrap_or(false)
}

/// Explicit simulation-targets file override (None = use the scope's default).
pub(super) fn sim_targets_file_override() -> Option<PathBuf> {
    parity_config()
        .sim_targets_file
        .clone()
        .map(resolve_sim_targets_file_path)
}

fn resolve_sim_targets_file_path(path: PathBuf) -> PathBuf {
    if path.is_absolute() || path.is_file() {
        return path;
    }
    let manifest_path = Path::new(env!("CARGO_MANIFEST_DIR")).join(&path);
    if manifest_path.is_file() {
        return manifest_path;
    }
    let repo_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(&path);
    if repo_path.is_file() {
        return repo_path;
    }
    path
}

fn generated_sim_targets_file() -> Option<PathBuf> {
    let path = msl_results_dir().join(GENERATED_SIM_TARGETS_FILE_REL);
    path.is_file().then_some(path)
}

fn env_var_bool(key: &str) -> bool {
    std::env::var(key)
        .ok()
        .map(|raw| {
            matches!(
                raw.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

fn committed_sim_targets_file() -> Option<PathBuf> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(DEFAULT_SIM_TARGETS_FILE_REL);
    path.is_file().then_some(path)
}

fn select_default_sim_targets_file(
    override_file: Option<PathBuf>,
    committed_file: Option<PathBuf>,
    generated_file: Option<PathBuf>,
    use_generated_targets: bool,
) -> Option<PathBuf> {
    override_file.or_else(|| {
        if use_generated_targets {
            generated_file.or(committed_file)
        } else {
            committed_file.or(generated_file)
        }
    })
}

pub(super) fn default_sim_targets_file() -> Option<PathBuf> {
    committed_sim_targets_file().or_else(generated_sim_targets_file)
}

/// Whether to use the generated simulation-targets file.
fn generated_sim_targets_file_requested() -> bool {
    parity_config().generated_sim_targets_file.unwrap_or(false)
}

fn should_apply_prior_complexity_schedule(subset_requested: bool) -> bool {
    !subset_requested
}

fn prior_results_file() -> Option<PathBuf> {
    let path = msl_results_dir().join(PRIOR_RESULTS_FILE_REL);
    path.is_file().then_some(path)
}

fn dedup_model_names_preserve_order(names: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    names
        .into_iter()
        .filter(|name| seen.insert(name.clone()))
        .collect()
}

pub(super) fn parse_target_model_names(value: &serde_json::Value) -> Result<Vec<String>, String> {
    fn parse_string_array(array: &[serde_json::Value]) -> Result<Vec<String>, String> {
        array
            .iter()
            .map(|entry| {
                entry
                    .as_str()
                    .map(ToOwned::to_owned)
                    .ok_or_else(|| "expected array of strings".to_string())
            })
            .collect()
    }

    if let Some(array) = value.as_array() {
        return parse_string_array(array);
    }
    let Some(object) = value.as_object() else {
        return Err("expected JSON array or object".to_string());
    };
    if let Some(model_names) = object
        .get("model_names")
        .and_then(serde_json::Value::as_array)
    {
        return parse_string_array(model_names);
    }
    if let Some(records) = object.get("records").and_then(serde_json::Value::as_array) {
        let mut names = Vec::new();
        for record in records {
            let Some(name) = record
                .get("model_name")
                .and_then(serde_json::Value::as_str)
                .map(ToOwned::to_owned)
            else {
                return Err("expected records[*].model_name to be a string".to_string());
            };
            names.push(name);
        }
        return Ok(names);
    }
    Err("object must include 'model_names' or 'records'".to_string())
}

pub(super) fn load_target_model_names(path: &Path) -> Result<Vec<String>, String> {
    let raw = fs::read_to_string(path).map_err(|e| format!("read failed: {e}"))?;
    let value: serde_json::Value =
        serde_json::from_str(&raw).map_err(|e| format!("parse failed: {e}"))?;
    Ok(dedup_model_names_preserve_order(parse_target_model_names(
        &value,
    )?))
}

fn retain_selected_model_order(names: &mut Vec<String>, selected: &[String]) -> Vec<String> {
    let available: HashSet<String> = names.iter().cloned().collect();
    *names = selected
        .iter()
        .filter(|name| available.contains(*name))
        .cloned()
        .collect();
    selected
        .iter()
        .filter(|name| !available.contains(*name))
        .cloned()
        .collect()
}

fn load_prior_model_complexities(path: &Path) -> Option<HashMap<String, PriorModelComplexity>> {
    let metrics = load_prior_model_schedule_metrics(path)?;
    Some(
        metrics
            .into_iter()
            .filter_map(|(name, metrics)| {
                (metrics.complexity != PriorModelComplexity::default())
                    .then_some((name, metrics.complexity))
            })
            .collect(),
    )
}

fn load_prior_model_schedule_metrics(
    path: &Path,
) -> Option<HashMap<String, PriorModelScheduleMetrics>> {
    let raw = fs::read_to_string(path).ok()?;
    let summary: PriorResultsSummary = serde_json::from_str(&raw).ok()?;
    let mut metrics_by_model = HashMap::new();
    for result in summary.model_results {
        let complexity = PriorModelComplexity {
            num_states: result.num_states.unwrap_or_default(),
            num_algebraics: result.num_algebraics.unwrap_or_default(),
            num_f_x: result.num_f_x.unwrap_or_default(),
            scalar_equations: result.scalar_equations.unwrap_or_default(),
            scalar_unknowns: result.scalar_unknowns.unwrap_or_default(),
        };
        let metrics = PriorModelScheduleMetrics {
            complexity,
            compile_millis: prior_compile_millis(result.compile_seconds),
            timed_out: is_compile_timeout_phase(result.timeout_phase.as_deref()),
        };
        if metrics != PriorModelScheduleMetrics::default() {
            metrics_by_model.insert(result.model_name, metrics);
        }
    }
    Some(metrics_by_model)
}

fn apply_prior_complexity_schedule(names: &mut [String], label: &str) -> bool {
    let Some(path) = prior_results_file() else {
        return false;
    };
    let Some(metrics_by_model) = load_prior_model_schedule_metrics(&path) else {
        eprintln!(
            "WARNING: failed to read prior MSL results from '{}'; keeping lexical target order",
            path.display()
        );
        return false;
    };
    let ranked_models = names
        .iter()
        .filter(|name| metrics_by_model.contains_key(*name))
        .count();
    if !prior_complexity_coverage_is_usable(ranked_models, names.len()) {
        eprintln!(
            "{label} schedule: ignoring prior compile metrics from {} because coverage is too low ({ranked_models}/{} models)",
            path.display(),
            names.len()
        );
        return false;
    }
    names.sort_by(|left, right| {
        let left_key = metrics_by_model
            .get(left)
            .copied()
            .map(prior_model_schedule_score)
            .unwrap_or_default();
        let right_key = metrics_by_model
            .get(right)
            .copied()
            .map(prior_model_schedule_score)
            .unwrap_or_default();
        right_key.cmp(&left_key).then_with(|| left.cmp(right))
    });
    println!(
        "{label} schedule: ranked {ranked_models}/{} models using prior compile metrics from {}",
        names.len(),
        path.display()
    );
    true
}

fn apply_reachable_class_schedule(
    source_root: &CompiledSourceRoot,
    names: &mut [String],
    label: &str,
) {
    names.sort_by(|left, right| {
        let left_key = source_root.reachable_class_count(left);
        let right_key = source_root.reachable_class_count(right);
        right_key.cmp(&left_key).then_with(|| left.cmp(right))
    });
    println!(
        "{label} schedule: ranked {} models by reachable class count from current source root",
        names.len()
    );
}

fn prior_complexity_coverage_is_usable(ranked_models: usize, target_models: usize) -> bool {
    if target_models == 0 {
        return false;
    }
    ranked_models >= MIN_PRIOR_COMPLEXITY_RANKED_MODELS
        && (ranked_models as f64 / target_models as f64) >= MIN_PRIOR_COMPLEXITY_COVERAGE_RATIO
}

pub(super) fn sim_subset_patterns() -> Vec<String> {
    parity_config().sim_match.clone().unwrap_or_default()
}

pub(super) fn sim_subset_limit() -> Option<usize> {
    parity_config().sim_limit
}

pub(super) fn sim_subset_exact_match_enabled() -> bool {
    parity_config().sim_match_exact.unwrap_or(false)
}

pub(super) fn msl_stage_parallelism() -> usize {
    if let Some(threads) = parity_config().stage_parallelism {
        return threads.max(1);
    }
    let auto_threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    default_msl_stage_parallelism_for(auto_threads, detected_physical_parallelism())
}

fn default_msl_stage_parallelism_for(
    logical_threads: usize,
    physical_cores: Option<usize>,
) -> usize {
    let default_threads = if logical_threads > MSL_HEADROOM_CORE_THRESHOLD {
        physical_cores
            .filter(|physical| *physical > 0 && *physical < logical_threads)
            .unwrap_or(logical_threads)
            .saturating_sub(MSL_RESERVED_CORES_ON_LARGE_HOSTS)
            .max(1)
    } else {
        logical_threads.max(1)
    };
    default_threads.max(1)
}

fn detected_physical_parallelism() -> Option<usize> {
    #[cfg(target_os = "linux")]
    {
        return detect_linux_physical_cores();
    }
    #[allow(unreachable_code)]
    None
}

#[cfg(target_os = "linux")]
fn detect_linux_physical_cores() -> Option<usize> {
    let mut cores = BTreeSet::new();
    for entry in std::fs::read_dir("/sys/devices/system/cpu").ok()? {
        let entry = entry.ok()?;
        let cpu_name = entry.file_name();
        let cpu_name = cpu_name.to_string_lossy();
        let Some(cpu_index) = cpu_name.strip_prefix("cpu") else {
            continue;
        };
        if cpu_index.is_empty() || !cpu_index.bytes().all(|byte| byte.is_ascii_digit()) {
            continue;
        }
        let topology = entry.path().join("topology");
        let package_id = read_trimmed(topology.join("physical_package_id"))?;
        let core_id = read_trimmed(topology.join("core_id"))?;
        cores.insert((package_id, core_id));
    }
    (!cores.is_empty()).then_some(cores.len())
}

#[cfg(target_os = "linux")]
fn read_trimmed(path: impl AsRef<Path>) -> Option<String> {
    Some(std::fs::read_to_string(path).ok()?.trim().to_string())
}

pub(super) fn simulation_parallelism() -> usize {
    let requested_threads = parity_config()
        .sim_parallelism
        .map(|threads| threads.max(1))
        .unwrap_or_else(msl_stage_parallelism);

    let per_worker_memory_mb = sim_worker_memory_limit_mb();
    // Sim total-memory budget caps the worker count when both it and a
    // per-worker estimate are configured; unset by default (no memory cap).
    let total_budget_mb: Option<usize> = parity_config().sim_total_memory_mb;
    let capped_by_memory = match (per_worker_memory_mb, total_budget_mb) {
        (Some(per_worker_mb), Some(total_mb)) => (total_mb / per_worker_mb.max(1)).max(1),
        _ => requested_threads,
    };

    let effective_threads = requested_threads.min(capped_by_memory).max(1);
    if effective_threads < requested_threads {
        println!(
            "Simulation parallelism capped by memory budget: {} workers (requested {}, per-worker cap {} MB, total budget {} MB)",
            effective_threads,
            requested_threads,
            per_worker_memory_mb.unwrap_or(0),
            total_budget_mb.unwrap_or(0)
        );
    }

    effective_threads
}

pub(super) fn model_name_matches_any_pattern(
    name: &str,
    patterns: &[String],
    exact_match: bool,
) -> bool {
    if exact_match {
        patterns.iter().any(|pat| name == pat)
    } else {
        patterns.iter().any(|pat| name.contains(pat))
    }
}

pub(super) fn apply_sim_subset_filters(names: &mut Vec<String>, label: &str) -> bool {
    let baseline_count = names.len();
    let target_file_override = sim_targets_file_override();
    let target_scope = msl_target_scope();
    let committed_file = target_scope
        .uses_default_target_file()
        .then(committed_sim_targets_file)
        .flatten();
    let generated_file = target_scope
        .uses_default_target_file()
        .then(generated_sim_targets_file)
        .flatten();
    let target_file = select_default_sim_targets_file(
        target_file_override.clone(),
        committed_file,
        generated_file,
        generated_sim_targets_file_requested(),
    );
    let patterns = sim_subset_patterns();
    let exact_match = sim_subset_exact_match_enabled();
    let limit = sim_subset_limit();
    let mut subset_requested = false;

    if let Some(path) = target_file.as_ref() {
        let is_override = target_file_override.is_some();
        subset_requested |= is_override;
        let selected = load_target_model_names(path).unwrap_or_else(|err| {
            panic!(
                "invalid simulation targets file '{}': {}",
                path.display(),
                err
            )
        });
        let missing = retain_selected_model_order(names, &selected);
        assert!(
            !is_override || missing.is_empty(),
            "simulation targets file '{}' names model(s) that were not discovered in the loaded Modelica sources: {}",
            path.display(),
            missing.join(", ")
        );
        if is_override {
            println!(
                "{} target file (--sim-targets-file {}) kept {}/{} explicit candidates",
                label,
                path.display(),
                names.len(),
                baseline_count
            );
        } else {
            println!(
                "{} default target file ({}) kept {}/{} root examples",
                label,
                path.display(),
                names.len(),
                baseline_count
            );
        }
    } else if matches!(target_scope, MslTargetScope::RootExamples) {
        println!(
            "{} target scope ({}): using root examples matching {} as the baseline target set",
            label,
            target_scope.as_str(),
            ROOT_MSL_EXAMPLE_SELECTION_PATTERN
        );
    }

    if !patterns.is_empty() {
        subset_requested = true;
        names.retain(|name| model_name_matches_any_pattern(name, &patterns, exact_match));
        println!(
            "{} subset filter (--sim-match{}) kept {}/{} root examples",
            label,
            if exact_match { ", exact" } else { "" },
            names.len(),
            baseline_count
        );
    }

    if let Some(limit) = limit {
        subset_requested = true;
        if names.len() > limit {
            names.truncate(limit);
        }
        println!(
            "{} subset limit (--sim-limit={}) -> {} root examples",
            label,
            limit,
            names.len()
        );
    }

    subset_requested
}

/// Pre-select compile targets for focused simulation runs.
///
/// By default, compile/balance/simulation run on all discovered root
/// `Modelica.*.Examples.*` models. Optional focused controls from the parity
/// config (`--sim-targets-file`, `--sim-match`, `--sim-limit`) can narrow the
/// scope for local debugging.
pub(super) fn select_compile_targets_for_focused_simulation(
    source_root: &CompiledSourceRoot,
    model_names: &[String],
    _run_simulation: bool,
) -> Option<Vec<String>> {
    let include_explicit_non_examples = sim_targets_file_override().is_some();
    let mut names =
        compile_target_candidates(source_root, model_names, include_explicit_non_examples);
    let baseline_count = names.len();
    let subset_requested = apply_sim_subset_filters(&mut names, "Compile");
    if should_apply_prior_complexity_schedule(subset_requested)
        && !apply_prior_complexity_schedule(&mut names, "Compile")
    {
        apply_reachable_class_schedule(source_root, &mut names, "Compile");
    }

    if names.is_empty() {
        None
    } else {
        println!(
            "Compile scope: {}/{} {}",
            names.len(),
            baseline_count,
            if include_explicit_non_examples {
                "explicit target candidates"
            } else {
                "root examples"
            }
        );
        Some(names)
    }
}

fn compile_target_candidates(
    source_root: &CompiledSourceRoot,
    model_names: &[String],
    include_explicit_non_examples: bool,
) -> Vec<String> {
    let mut names = if include_explicit_non_examples {
        model_names.to_vec()
    } else {
        root_msl_example_model_names(source_root.tree(), model_names)
    };
    names.sort();
    names
}

pub(super) fn is_selected_sim_target(name: &str, ctx: &RenderSimContext<'_>) -> bool {
    ctx.sim_target_names.is_none_or(|set| set.contains(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_msl_stage_parallelism_uses_all_cores_through_sixteen() {
        assert_eq!(default_msl_stage_parallelism_for(1, None), 1);
        assert_eq!(default_msl_stage_parallelism_for(2, None), 2);
        assert_eq!(default_msl_stage_parallelism_for(4, Some(2)), 4);
        assert_eq!(default_msl_stage_parallelism_for(16, Some(8)), 16);
    }

    #[test]
    fn default_msl_stage_parallelism_reserves_two_cores_on_large_hosts() {
        assert_eq!(default_msl_stage_parallelism_for(17, None), 15);
        assert_eq!(default_msl_stage_parallelism_for(32, None), 30);
    }

    #[test]
    fn default_msl_stage_parallelism_uses_physical_cores_on_smt_hosts() {
        assert_eq!(default_msl_stage_parallelism_for(24, Some(12)), 10);
        assert_eq!(default_msl_stage_parallelism_for(32, Some(16)), 14);
    }

    #[test]
    fn default_msl_stage_parallelism_ignores_invalid_physical_core_counts() {
        assert_eq!(default_msl_stage_parallelism_for(32, Some(0)), 30);
        assert_eq!(default_msl_stage_parallelism_for(32, Some(32)), 30);
        assert_eq!(default_msl_stage_parallelism_for(32, Some(64)), 30);
    }

    #[test]
    fn mem_available_parser_reads_linux_meminfo() {
        let meminfo = "MemTotal:       64200000 kB\nMemAvailable:   32768000 kB\n";
        assert_eq!(parse_mem_available_mb(meminfo), Some(32000));
    }

    #[test]
    fn compile_memory_budget_reserves_desktop_headroom() {
        assert_eq!(
            compile_memory_budget_mb_from_available(64 * 1024, 8 * 1024),
            56 * 1024
        );
        assert_eq!(
            compile_memory_budget_mb_from_available(4 * 1024, 8 * 1024),
            2 * 1024
        );
    }

    #[test]
    fn compile_model_cap_uses_budget_and_model_estimate() {
        assert_eq!(compile_model_cap_from_budget(56 * 1024, 768), 74);
        assert_eq!(compile_model_cap_from_budget(200, 768), 1);
    }

    #[test]
    fn compile_memory_cost_from_complexity_uses_tiered_tokens() {
        assert_eq!(compile_model_memory_mb_from_complexity_score(0, 768), 768);
        assert_eq!(compile_model_memory_mb_from_complexity_score(42, 768), 192);
        assert_eq!(compile_model_memory_mb_from_complexity_score(500, 768), 384);
        assert_eq!(
            compile_model_memory_mb_from_complexity_score(2_000, 768),
            768
        );
        assert_eq!(
            compile_model_memory_mb_from_complexity_score(8_000, 768),
            1_536
        );
    }

    #[test]
    fn load_target_model_names_preserves_record_order() {
        let temp = tempfile::tempdir().expect("tempdir");
        let path = temp.path().join("targets.json");
        fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "records": [
                    {"model_name": "Modelica.Electrical.Digital.Examples.DFFREGSRL"},
                    {"model_name": "Modelica.Blocks.Examples.BooleanNetwork1"},
                    {"model_name": "Modelica.Electrical.Digital.Examples.DFFREGSRL"},
                    {"model_name": "Modelica.Electrical.Digital.Examples.DFFREG"}
                ]
            }))
            .expect("serialize targets"),
        )
        .expect("write targets");

        assert_eq!(
            load_target_model_names(&path).expect("load target names"),
            vec![
                "Modelica.Electrical.Digital.Examples.DFFREGSRL".to_string(),
                "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
                "Modelica.Electrical.Digital.Examples.DFFREG".to_string(),
            ]
        );
    }

    #[test]
    fn explicit_compile_target_candidates_include_modelica_test_models() {
        fn class(
            name: &str,
            class_type: rumoca_compile::parsing::ClassType,
        ) -> rumoca_compile::parsing::ClassDef {
            rumoca_compile::parsing::ClassDef {
                name: rumoca_compile::parsing::Token {
                    text: std::sync::Arc::from(name),
                    ..Default::default()
                },
                class_type,
                ..Default::default()
            }
        }

        let mut def = rumoca_compile::parsing::StoredDefinition::default();
        let mut modelica = class("Modelica", rumoca_compile::parsing::ClassType::Package);
        let mut blocks = class("Blocks", rumoca_compile::parsing::ClassType::Package);
        let mut examples = class("Examples", rumoca_compile::parsing::ClassType::Package);
        examples.classes.insert(
            "BooleanNetwork1".to_string(),
            class("BooleanNetwork1", rumoca_compile::parsing::ClassType::Model),
        );
        blocks.classes.insert("Examples".to_string(), examples);
        modelica.classes.insert("Blocks".to_string(), blocks);
        def.classes.insert("Modelica".to_string(), modelica);
        let source_root = CompiledSourceRoot::from_stored_definition(def).expect("source root");

        let names = vec![
            "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
            "Modelica.Blocks.Interfaces.SO".to_string(),
            "ModelicaTest.Blocks.Routing.ShowRealPassThrough".to_string(),
        ];

        assert_eq!(
            compile_target_candidates(&source_root, &names, false),
            vec!["Modelica.Blocks.Examples.BooleanNetwork1"]
        );
        assert_eq!(
            compile_target_candidates(&source_root, &names, true),
            vec![
                "Modelica.Blocks.Examples.BooleanNetwork1",
                "Modelica.Blocks.Interfaces.SO",
                "ModelicaTest.Blocks.Routing.ShowRealPassThrough",
            ]
        );
    }

    #[test]
    fn retain_selected_model_order_reports_missing_explicit_targets() {
        let mut names = vec![
            "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
            "ModelicaTest.Blocks.Routing.ShowRealPassThrough".to_string(),
        ];
        let missing = retain_selected_model_order(
            &mut names,
            &[
                "ModelicaTest.Blocks.Routing.ShowRealPassThrough".to_string(),
                "ModelicaTest.DoesNotExist".to_string(),
                "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
            ],
        );

        assert_eq!(
            names,
            vec![
                "ModelicaTest.Blocks.Routing.ShowRealPassThrough",
                "Modelica.Blocks.Examples.BooleanNetwork1",
            ]
        );
        assert_eq!(missing, vec!["ModelicaTest.DoesNotExist"]);
    }

    #[test]
    fn relative_sim_targets_file_can_resolve_from_crate_or_repo_root() {
        let crate_relative = PathBuf::from("tests/msl_tests/modelica_test_targets_ci.json");
        let repo_relative =
            PathBuf::from("crates/rumoca-test-msl/tests/msl_tests/modelica_test_targets_ci.json");

        assert!(resolve_sim_targets_file_path(crate_relative).is_file());
        assert!(resolve_sim_targets_file_path(repo_relative).is_file());
    }

    #[test]
    fn load_prior_model_complexities_reads_state_rankings() {
        let temp = tempfile::tempdir().expect("tempdir");
        let path = temp.path().join("msl_results.json");
        fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "model_results": [
                    {
                        "model_name": "Modelica.Electrical.Digital.Examples.DFFREG",
                        "num_states": 0,
                        "num_algebraics": 42,
                        "num_f_x": 0,
                        "scalar_equations": 84,
                        "scalar_unknowns": 84
                    },
                    {
                        "model_name": "Modelica.Blocks.Examples.BooleanNetwork1",
                        "num_states": 4,
                        "num_algebraics": 8,
                        "num_f_x": 18,
                        "scalar_equations": 18,
                        "scalar_unknowns": 18
                    }
                ]
            }))
            .expect("serialize prior results"),
        )
        .expect("write prior results");

        let complexities = load_prior_model_complexities(&path).expect("prior complexities");
        assert_eq!(
            complexities.get("Modelica.Blocks.Examples.BooleanNetwork1"),
            Some(&PriorModelComplexity {
                num_states: 4,
                num_algebraics: 8,
                num_f_x: 18,
                scalar_equations: 18,
                scalar_unknowns: 18,
            })
        );
        assert_eq!(
            complexities.get("Modelica.Electrical.Digital.Examples.DFFREG"),
            Some(&PriorModelComplexity {
                num_states: 0,
                num_algebraics: 42,
                num_f_x: 0,
                scalar_equations: 84,
                scalar_unknowns: 84,
            })
        );
    }

    #[test]
    fn prior_schedule_metrics_use_timeout_compile_time_as_cost() {
        let temp = tempfile::tempdir().expect("tempdir");
        let path = temp.path().join("msl_results.json");
        fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "model_results": [
                    {
                        "model_name": "Modelica.Blocks.Examples.TimeoutAtFlatten",
                        "phase_reached": "Flatten",
                        "error_code": "EMSL_TIMEOUT_MODEL_ATTEMPT",
                        "timeout_phase": "Flatten",
                        "compile_seconds": 13.542024136
                    },
                    {
                        "model_name": "Modelica.Blocks.Examples.SmallSuccess",
                        "num_states": 1,
                        "num_algebraics": 1,
                        "num_f_x": 1,
                        "scalar_equations": 1,
                        "scalar_unknowns": 1,
                        "compile_seconds": 0.25
                    },
                    {
                        "model_name": "Modelica.Blocks.Examples.SolveTimeout",
                        "timeout_phase": "Solve",
                        "compile_seconds": 13.0
                    }
                ]
            }))
            .expect("serialize prior results"),
        )
        .expect("write prior results");

        let metrics = load_prior_model_schedule_metrics(&path).expect("prior metrics");
        assert_eq!(
            metrics
                .get("Modelica.Blocks.Examples.TimeoutAtFlatten")
                .and_then(|metrics| metrics.compile_millis),
            Some(13_542)
        );
        assert!(
            prior_model_schedule_score(
                *metrics
                    .get("Modelica.Blocks.Examples.TimeoutAtFlatten")
                    .expect("timeout metrics")
            ) > prior_model_schedule_score(
                *metrics
                    .get("Modelica.Blocks.Examples.SmallSuccess")
                    .expect("success metrics")
            )
        );
    }

    #[test]
    fn committed_sim_targets_file_prefers_committed_list() {
        let default_targets = default_sim_targets_file().expect("committed target file");
        assert!(
            default_targets.ends_with(DEFAULT_SIM_TARGETS_FILE_REL),
            "committed target file should be the committed explicit list, got {}",
            default_targets.display()
        );
    }

    #[test]
    fn default_sim_set_mode_runs_full_selected_target_set() {
        assert_eq!(sim_set_mode(), SimSetMode::Full);
    }

    #[test]
    fn root_examples_target_scope_is_default_baseline_scope() {
        assert_eq!(msl_target_scope(), MslTargetScope::RootExamples);
        assert!(MslTargetScope::DefaultSimulationTargets.uses_default_target_file());
        assert!(!MslTargetScope::RootExamples.uses_default_target_file());
    }

    #[test]
    fn committed_target_subset_contains_semantic_representatives() {
        let target_file = committed_sim_targets_file().expect("committed target file");
        let names = load_target_model_names(&target_file).expect("load committed targets");
        let target_set = names.iter().cloned().collect::<HashSet<_>>();

        for required in [
            "Modelica.Fluid.Examples.IncompressibleFluidNetwork",
            "Modelica.Media.Examples.IdealGasH2O",
            "Modelica.Mechanics.MultiBody.Examples.Elementary.Pendulum",
            "Modelica.Mechanics.MultiBody.Examples.Elementary.DoublePendulum",
            "Modelica.Clocked.Examples.Elementary.RealSignals.Sample1",
            "Modelica.StateGraph.Examples.ExecutionPaths",
        ] {
            assert!(
                target_set.contains(required),
                "semantic representative {required} must stay in the committed explicit MSL simulation set"
            );
        }
    }

    #[test]
    fn modelica_test_ci_targets_are_separate_from_committed_msl_subset() {
        let msl_target_file = committed_sim_targets_file().expect("committed target file");
        let msl_names = load_target_model_names(&msl_target_file).expect("load committed targets");
        assert!(
            msl_names
                .iter()
                .all(|name| !name.starts_with("ModelicaTest.")),
            "ModelicaTest targets must remain in the separate CI target file"
        );

        let modelica_test_file =
            Path::new(env!("CARGO_MANIFEST_DIR")).join(MODELICA_TEST_TARGETS_FILE_REL);
        let modelica_test_names =
            load_target_model_names(&modelica_test_file).expect("load ModelicaTest CI targets");
        assert!(!modelica_test_names.is_empty());
        assert!(
            modelica_test_names
                .iter()
                .all(|name| name.starts_with("ModelicaTest.")),
            "ModelicaTest CI target file should contain only ModelicaTest.* models"
        );
    }

    #[test]
    fn select_default_sim_targets_file_prefers_committed_file_without_generated_opt_in() {
        let selected = select_default_sim_targets_file(
            None,
            Some(PathBuf::from("/repo/tests/msl_simulation_targets.json")),
            Some(PathBuf::from(
                "/repo/target/msl/results/msl_simulation_targets.json",
            )),
            false,
        )
        .expect("default target file");
        assert_eq!(
            selected,
            PathBuf::from("/repo/tests/msl_simulation_targets.json")
        );
    }

    #[test]
    fn select_default_sim_targets_file_can_use_generated_targets_with_explicit_opt_in() {
        let selected = select_default_sim_targets_file(
            None,
            Some(PathBuf::from("/repo/tests/msl_simulation_targets.json")),
            Some(PathBuf::from(
                "/repo/target/msl/results/msl_simulation_targets.json",
            )),
            true,
        )
        .expect("generated target file");
        assert_eq!(
            selected,
            PathBuf::from("/repo/target/msl/results/msl_simulation_targets.json")
        );
    }

    #[test]
    fn prior_complexity_schedule_applies_to_full_scope_only() {
        assert!(should_apply_prior_complexity_schedule(false));
        assert!(!should_apply_prior_complexity_schedule(true));
    }

    #[test]
    fn prior_complexity_schedule_requires_enough_full_scope_coverage() {
        assert!(!prior_complexity_coverage_is_usable(0, 566));
        assert!(!prior_complexity_coverage_is_usable(1, 566));
        assert!(!prior_complexity_coverage_is_usable(31, 566));
        assert!(!prior_complexity_coverage_is_usable(32, 566));
        assert!(!prior_complexity_coverage_is_usable(56, 566));
        assert!(prior_complexity_coverage_is_usable(57, 566));
        assert!(!prior_complexity_coverage_is_usable(57, 0));
    }
}
