#![allow(dead_code)]

use super::*;

mod balance_pipeline_config;
mod balance_pipeline_core;
mod balance_pipeline_debug_introspection;
mod balance_pipeline_example_targets;
mod balance_pipeline_perf;
mod balance_pipeline_quality_gate;
mod balance_pipeline_render_sim;
mod balance_pipeline_reporting;
mod balance_pipeline_selection;
mod balance_pipeline_sim_worker;
mod balance_pipeline_stats_report;
mod balance_pipeline_summary;

pub(crate) use balance_pipeline_config::*;
use balance_pipeline_debug_introspection::*;
use balance_pipeline_example_targets::*;
use balance_pipeline_perf::*;
use balance_pipeline_quality_gate::*;
use balance_pipeline_render_sim::*;
use balance_pipeline_reporting::*;
use balance_pipeline_selection::*;
use balance_pipeline_sim_worker::*;
use balance_pipeline_stats_report::*;
use balance_pipeline_summary::*;

/// Per-equation introspection dump. Edit to enable while debugging.
fn msl_introspect_enabled() -> bool {
    false
}

fn msl_introspect_eq_limit() -> usize {
    120
}

fn should_introspect_model(_model_name: &str) -> bool {
    // No model filter; introspection (when enabled above) applies to all.
    true
}

/// Render simulation plots during the MSL run. Edit to enable.
fn msl_render_enabled() -> bool {
    false
}

pub(super) const STAGE_WATCHDOG_LOG_INTERVAL_SECS: u64 = 15;
/// Per-model, per-phase wall budget. Heavy MSL models (MultiBody, Machines, FFT
/// rectifiers) take 10-19s to *lower* to Solve-IR on a shared 4-core CI runner,
/// so a 10s budget timed them out non-deterministically and made the IR-Solve
/// pass count flaky right at the quality-gate threshold. The budget only bounds
/// genuinely stuck models; correct-but-slow lowering must be allowed to finish,
/// so it is sized above the observed worst case with margin (local fast runners
/// never approach it).
pub(super) const MODEL_ATTEMPT_TIMEOUT_SECS: f64 = 45.0;

pub(super) fn model_attempt_timeout_secs() -> f64 {
    MODEL_ATTEMPT_TIMEOUT_SECS
}

pub(super) struct ModelCompileEntry {
    model_name: String,
    compile_outcome: ModelCompileOutcome,
    remaining_budget_secs: Option<f64>,
    compile_seconds: f64,
    compile_perf_profile_file: Option<String>,
}

pub(super) enum ModelCompileOutcome {
    Phase(PhaseResult),
    StrictReport(Box<StrictCompileReport>),
    StrictDaeSuccess(Box<rumoca_compile::compile::DaeCompilationResult>),
    StrictDaeFailure(String),
}

impl ModelCompileOutcome {
    fn is_success(&self) -> bool {
        self.success_result().is_some() || matches!(self, ModelCompileOutcome::StrictDaeSuccess(_))
    }

    fn success_result(&self) -> Option<&rumoca_compile::compile::CompilationResult> {
        match self {
            Self::Phase(PhaseResult::Success(result)) => Some(result.as_ref()),
            Self::StrictReport(report) if report.requested_succeeded() => {
                match report.requested_result.as_ref() {
                    Some(PhaseResult::Success(result)) => Some(result.as_ref()),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

pub(super) struct StageAbortWatchdog {
    done: std::sync::Arc<std::sync::atomic::AtomicBool>,
    worker: Option<std::thread::JoinHandle<()>>,
}

fn run_stage_watchdog_loop(
    done_flag: std::sync::Arc<std::sync::atomic::AtomicBool>,
    stage_label: String,
    timeout_secs: u64,
) {
    let timeout = Duration::from_secs(timeout_secs);
    let start = Instant::now();
    let mut last_log = Instant::now();
    loop {
        if done_flag.load(Ordering::Relaxed) {
            break;
        }
        let elapsed = start.elapsed();
        if elapsed >= timeout {
            eprintln!(
                "ERROR: stage timeout exceeded: '{}' ran for {:.1}s (limit={}s). Aborting to prevent a stuck test run.",
                stage_label,
                elapsed.as_secs_f64(),
                timeout_secs
            );
            std::process::abort();
        }
        if last_log.elapsed().as_secs() >= STAGE_WATCHDOG_LOG_INTERVAL_SECS {
            eprintln!(
                "  stage in-flight: '{}' elapsed {:.1}s / {}s",
                stage_label,
                elapsed.as_secs_f64(),
                timeout_secs
            );
            last_log = Instant::now();
        }
        std::thread::sleep(Duration::from_secs(1));
    }
}

impl StageAbortWatchdog {
    pub(super) fn new(stage_name: impl Into<String>, timeout_secs: u64) -> Self {
        let stage_name = stage_name.into();
        let done = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let done_flag = std::sync::Arc::clone(&done);
        let worker = std::thread::spawn(move || {
            run_stage_watchdog_loop(done_flag, stage_name, timeout_secs);
        });
        Self {
            done,
            worker: Some(worker),
        }
    }
}

impl Drop for StageAbortWatchdog {
    fn drop(&mut self) {
        self.done.store(true, Ordering::Relaxed);
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
    }
}

// =============================================================================
// Balance Pipeline
// =============================================================================

/// Summary of MSL test results (compilation, balance, and simulation).
#[derive(Debug, Clone, Serialize, Deserialize)]
struct MslSummary {
    /// Git commit used to generate this result file.
    #[serde(default)]
    git_commit: String,
    msl_version: String,
    total_mo_files: usize,
    parse_errors: usize,
    /// Global source-root/session resolve failures that invalidate the whole run.
    resolve_errors: usize,
    /// Per-model strict-closure resolve failures.
    #[serde(default)]
    resolve_failed: usize,
    typecheck_errors: usize,
    total_models: usize,
    /// Models with outer components that need inner declarations from enclosing scope.
    /// These are not failures - they're models designed to be used within a system.
    needs_inner: usize,
    instantiate_failed: usize,
    typecheck_failed: usize,
    flatten_failed: usize,
    todae_failed: usize,
    #[serde(default)]
    non_sim_models: usize,
    compiled_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    #[serde(default)]
    initial_balanced_models: usize,
    #[serde(default)]
    initial_unbalanced_models: usize,
    /// Models declared with `partial` keyword (intentionally incomplete).
    /// MLS §4.7: Partial models are excluded from balance checking.
    partial_models: usize,
    /// Class type breakdown (model, connector, function, etc.)
    #[serde(default)]
    class_type_counts: HashMap<String, usize>,
    failures_by_phase: HashMap<String, Vec<String>>,
    unbalanced_list: Vec<String>,
    #[serde(default)]
    initial_unbalanced_list: Vec<String>,
    /// Models that are not standalone-simulatable with default bindings.
    #[serde(default)]
    non_sim_list: Vec<String>,
    /// Flatten error categories with (model_name, error) pairs
    #[serde(default)]
    error_categories: HashMap<String, Vec<(String, String)>>,
    /// Stable compiler diagnostic/error-code counts across all failed models.
    #[serde(default)]
    error_code_counts: HashMap<String, usize>,
    /// Unsupported backend/semantic feature IDs extracted from stable error codes/messages.
    #[serde(default)]
    unsupported_feature_counts: HashMap<String, usize>,
    /// Unsupported feature IDs grouped by manifest target/backend label.
    #[serde(default)]
    unsupported_feature_counts_by_backend: HashMap<String, HashMap<String, usize>>,
    /// Most common undefined variables with counts
    #[serde(default)]
    undefined_vars: HashMap<String, usize>,
    /// Balance value distribution (balance -> count)
    #[serde(default)]
    balance_distribution: HashMap<i64, usize>,
    /// Per-model results with eq/var counts for comparison with OMC reference data
    #[serde(default)]
    model_results: Vec<MslModelResult>,
    /// Timing breakdown for major phases.
    #[serde(default)]
    timings: MslPhaseTimings,
    // --- Simulation stats ---
    /// Number of models that simulated successfully.
    #[serde(default)]
    sim_ok: usize,
    /// Number of models with NaN/Inf in output.
    #[serde(default)]
    sim_nan: usize,
    /// Number of models where the solver failed.
    #[serde(default)]
    sim_solver_fail: usize,
    /// Number of models skipped due to wall-clock timeout.
    #[serde(default)]
    sim_timeout: usize,
    /// Number of models with balance/dimension issues preventing simulation.
    #[serde(default)]
    sim_balance_fail: usize,
    /// Number of models where simulation was attempted.
    #[serde(default)]
    sim_attempted: usize,
    /// Number of simulation-target models whose initialization problem was attempted.
    #[serde(default)]
    ic_attempted: usize,
    /// Number of models whose initialization problem solved before integration.
    #[serde(default)]
    ic_ok: usize,
    /// Number of models whose initialization problem failed before integration.
    #[serde(default)]
    ic_solver_fail: usize,
    /// Total solver/integration seconds (sum of per-model worker-reported runtime).
    #[serde(default)]
    total_sim_seconds: f64,
    /// Total simulator build/setup seconds reported by workers.
    #[serde(default)]
    total_sim_build_seconds: f64,
    /// Total simulator run/integration seconds reported by workers.
    #[serde(default)]
    total_sim_run_seconds: f64,
    /// Total per-model wall/system time including process overhead.
    #[serde(default)]
    total_sim_wall_seconds: f64,
    /// Standalone root MSL example models selected as simulation targets.
    #[serde(default)]
    sim_target_models: Vec<String>,
}

fn current_git_commit() -> String {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output();
    match output {
        Ok(out) if out.status.success() => {
            let commit = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if commit.is_empty() {
                "unknown".to_string()
            } else {
                commit
            }
        }
        _ => "unknown".to_string(),
    }
}

/// Mutable counters for summarizing results.
#[derive(Default)]
struct ResultCounters {
    resolve_failed: usize,
    needs_inner: usize,
    instantiate_failed: usize,
    typecheck_failed: usize,
    flatten_failed: usize,
    todae_failed: usize,
    non_sim_models: usize,
    compiled_models: usize,
    balanced_models: usize,
    unbalanced_models: usize,
    initial_balanced_models: usize,
    initial_unbalanced_models: usize,
    partial_models: usize,
    failures_by_phase: HashMap<String, Vec<String>>,
    unbalanced_list: Vec<String>,
    initial_unbalanced_list: Vec<String>,
    non_sim_list: Vec<String>,
    error_categories: HashMap<String, Vec<(String, String)>>,
    error_code_counts: HashMap<String, usize>,
    unsupported_feature_counts: HashMap<String, usize>,
    unsupported_feature_counts_by_backend: HashMap<String, HashMap<String, usize>>,
    undefined_vars: HashMap<String, usize>,
    balance_distribution: HashMap<i64, usize>,
    // Simulation counters
    sim_ok: usize,
    sim_nan: usize,
    sim_solver_fail: usize,
    sim_timeout: usize,
    sim_balance_fail: usize,
    sim_attempted: usize,
    ic_attempted: usize,
    ic_ok: usize,
    ic_solver_fail: usize,
    total_sim_seconds: f64,
    total_sim_build_seconds: f64,
    total_sim_run_seconds: f64,
    total_sim_wall_seconds: f64,
}

/// Immutable inputs required to build the final MSL summary.
struct MslSummaryInputs {
    total_mo_files: usize,
    parse_errors: usize,
    total_models: usize,
    class_type_counts: HashMap<String, usize>,
}

/// Process a successful compilation result.
fn process_success_result(result: &MslModelResult, counters: &mut ResultCounters) {
    counters.compiled_models += 1;
    if result.is_partial == Some(true) {
        counters.partial_models += 1;
        return;
    }
    let balance = result.balance.unwrap_or(0);
    *counters.balance_distribution.entry(balance).or_insert(0) += 1;
    if result.is_balanced == Some(true) {
        counters.balanced_models += 1;
    } else {
        counters.unbalanced_models += 1;
        counters
            .unbalanced_list
            .push(format!("{} (balance={})", result.model_name, balance));
    }

    if result.initial_balance_ok == Some(true) {
        counters.initial_balanced_models += 1;
    } else {
        counters.initial_unbalanced_models += 1;
        let before = result.initial_balance_deficit_before.unwrap_or_default();
        let after = result.initial_balance_deficit_after.unwrap_or_default();
        counters.initial_unbalanced_list.push(format!(
            "{} (init_deficit_before={}, init_deficit_after={})",
            result.model_name, before, after
        ));
    }
}

fn process_result_error_taxonomy(result: &MslModelResult, counters: &mut ResultCounters) {
    if let Some(code) = result.error_code.as_deref() {
        *counters
            .error_code_counts
            .entry(code.to_string())
            .or_insert(0) += 1;
    }

    let mut features = HashSet::new();
    let mut backend_features = HashSet::new();
    if let Some(code) = result.error_code.as_deref()
        && let Some(feature) = unsupported_feature_id_from_text(code)
    {
        features.insert(feature);
    }
    for text in [
        result.error.as_deref(),
        result.sim_error.as_deref(),
        result.ic_error.as_deref(),
    ]
    .into_iter()
    .flatten()
    {
        if let Some(feature) = unsupported_feature_id_from_text(text) {
            if let Some(target) = unsupported_feature_target_from_text(text) {
                backend_features.insert((target, feature.clone()));
            }
            features.insert(feature);
        }
    }
    for feature in derived_unsupported_feature_ids(result) {
        features.insert(feature);
    }
    for feature in features {
        *counters
            .unsupported_feature_counts
            .entry(feature)
            .or_insert(0) += 1;
    }
    for (backend, feature) in backend_features {
        *counters
            .unsupported_feature_counts_by_backend
            .entry(backend)
            .or_default()
            .entry(feature)
            .or_insert(0) += 1;
    }
}

fn unsupported_feature_id_from_text(text: &str) -> Option<String> {
    let normalized = text.trim();
    if let Some(rest) = normalized.strip_prefix("unsupported-feature:") {
        return stable_feature_id_prefix(rest);
    }
    let marker = "does not support feature '";
    if let Some((_, rest)) = normalized.split_once(marker) {
        let (feature, _) = rest.split_once('\'')?;
        return stable_feature_id(feature);
    }
    None
}

fn unsupported_feature_target_from_text(text: &str) -> Option<String> {
    let marker = "Target '";
    let (_, rest) = text.split_once(marker)?;
    let (target, _) = rest.split_once('\'')?;
    let target = target.trim();
    (!target.is_empty()).then(|| target.to_string())
}

fn stable_feature_id_prefix(text: &str) -> Option<String> {
    let feature = text
        .split(|ch: char| ch == ':' || ch.is_whitespace())
        .next()
        .unwrap_or_default();
    stable_feature_id(feature)
}

fn stable_feature_id(feature: &str) -> Option<String> {
    let feature = feature.trim();
    if feature.is_empty() {
        return None;
    }
    let stable = feature
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' {
                ch.to_ascii_lowercase()
            } else {
                '_'
            }
        })
        .collect::<String>();
    Some(stable)
}

fn derived_phase_unsupported_feature_code(model_name: &str, error: Option<&str>) -> Option<String> {
    let feature = derived_unsupported_feature_from_text(model_name, error?)?;
    Some(format!("unsupported-feature:{feature}"))
}

fn derived_unsupported_feature_ids(result: &MslModelResult) -> Vec<String> {
    [
        result.error.as_deref(),
        result.sim_error.as_deref(),
        result.ic_error.as_deref(),
    ]
    .into_iter()
    .flatten()
    .filter_map(|text| derived_unsupported_feature_from_text(&result.model_name, text))
    .collect()
}

fn derived_unsupported_feature_from_text(model_name: &str, text: &str) -> Option<String> {
    if model_name.starts_with("Modelica.Fluid.")
        && (text.contains("unresolved function call: `Medium`")
            || text.contains("unresolved function call: 'Medium'"))
    {
        return Some("replaceable_media_package_lookup".to_string());
    }
    if model_name.starts_with("Modelica.Mechanics.MultiBody.")
        && text.contains("Modelica.Mechanics.MultiBody.Parts.Body.world")
    {
        return Some("inner_outer_qualified_lookup".to_string());
    }
    if model_name.starts_with("Modelica.Media.") && text.contains("SymbolicSingular") {
        return Some("media_property_initialization_singularity".to_string());
    }
    if model_name.starts_with("Modelica.Media.")
        && text.contains("slice subscript `:` is unsupported")
    {
        return Some("media_property_vector_slice_observation".to_string());
    }
    None
}

/// Process a simple phase failure (NeedsInner, Instantiate, ToDae).
fn process_phase_failure(result: &MslModelResult, phase: &str, counters: &mut ResultCounters) {
    match phase {
        "Resolve" => counters.resolve_failed += 1,
        "NeedsInner" => counters.needs_inner += 1,
        "Instantiate" => counters.instantiate_failed += 1,
        "Typecheck" => counters.typecheck_failed += 1,
        "ToDae" => counters.todae_failed += 1,
        _ => {}
    }
    counters
        .failures_by_phase
        .entry(phase.to_string())
        .or_default()
        .push(result.model_name.clone());
}

fn process_non_sim_result(result: &MslModelResult, counters: &mut ResultCounters) {
    counters.non_sim_models += 1;
    counters.non_sim_list.push(result.model_name.clone());
    counters
        .failures_by_phase
        .entry("NonSim".to_string())
        .or_default()
        .push(result.model_name.clone());
}

/// Process a flatten error result.
fn process_flatten_error(result: &MslModelResult, counters: &mut ResultCounters) {
    counters.flatten_failed += 1;
    counters
        .failures_by_phase
        .entry("Flatten".to_string())
        .or_default()
        .push(result.model_name.clone());
    let Some(error) = &result.error else { return };
    let category = categorize_flatten_error(error);
    counters
        .error_categories
        .entry(category.to_string())
        .or_default()
        .push((result.model_name.clone(), error.clone()));
    if category == "UndefinedVariable"
        && let Some(var) = extract_undefined_var(error)
    {
        *counters.undefined_vars.entry(var).or_insert(0) += 1;
    }
}

/// Create an empty MslSummary with basic file counts.
fn empty_summary(total_mo_files: usize, parse_errors: usize) -> MslSummary {
    MslSummary {
        git_commit: current_git_commit(),
        msl_version: MSL_VERSION.to_string(),
        total_mo_files,
        parse_errors,
        resolve_errors: 0,
        resolve_failed: 0,
        typecheck_errors: 0,
        total_models: 0,
        needs_inner: 0,
        instantiate_failed: 0,
        typecheck_failed: 0,
        flatten_failed: 0,
        todae_failed: 0,
        non_sim_models: 0,
        compiled_models: 0,
        balanced_models: 0,
        unbalanced_models: 0,
        initial_balanced_models: 0,
        initial_unbalanced_models: 0,
        partial_models: 0,
        class_type_counts: HashMap::new(),
        failures_by_phase: HashMap::new(),
        unbalanced_list: Vec::new(),
        initial_unbalanced_list: Vec::new(),
        non_sim_list: Vec::new(),
        error_categories: HashMap::new(),
        error_code_counts: HashMap::new(),
        unsupported_feature_counts: HashMap::new(),
        unsupported_feature_counts_by_backend: HashMap::new(),
        undefined_vars: HashMap::new(),
        balance_distribution: HashMap::new(),
        model_results: Vec::new(),
        timings: MslPhaseTimings::default(),
        sim_ok: 0,
        sim_nan: 0,
        sim_solver_fail: 0,
        sim_timeout: 0,
        sim_balance_fail: 0,
        sim_attempted: 0,
        ic_attempted: 0,
        ic_ok: 0,
        ic_solver_fail: 0,
        total_sim_seconds: 0.0,
        total_sim_build_seconds: 0.0,
        total_sim_run_seconds: 0.0,
        total_sim_wall_seconds: 0.0,
        sim_target_models: Vec::new(),
    }
}

fn phase_error_result(
    name: String,
    phase_reached: &str,
    error: Option<String>,
    error_code: Option<String>,
) -> MslModelResult {
    let error_code =
        error_code.or_else(|| derived_phase_unsupported_feature_code(&name, error.as_deref()));
    MslModelResult {
        model_name: name,
        phase_reached: phase_reached.to_string(),
        error,
        error_code,
        num_states: None,
        num_algebraics: None,
        num_f_x: None,
        balance: None,
        is_balanced: None,
        is_partial: None,
        class_type: None,
        scalar_equations: None,
        scalar_unknowns: None,
        initial_equation_scalars: None,
        initial_algorithm_scalars: None,
        initial_balance_deficit_before: None,
        initial_closure_used: None,
        initial_balance_deficit_after: None,
        initial_balance_ok: None,
        compile_seconds: None,
        instantiate_seconds: None,
        typecheck_seconds: None,
        flatten_seconds: None,
        dae_seconds: None,
        compile_perf_profile_file: None,
        ir_ast_file: None,
        ir_flat_file: None,
        sim_status: None,
        sim_error: None,
        sim_error_span: None,
        ic_status: None,
        ic_error: None,
        ic_error_span: None,
        ic_seconds: None,
        sim_seconds: None,
        sim_build_seconds: None,
        ir_solve_seconds: None,
        ir_solve_structural_dae_seconds: None,
        ir_solve_lower_seconds: None,
        sim_backend_build_seconds: None,
        sim_run_seconds: None,
        sim_wall_seconds: None,
        sim_trace_file: None,
        sim_perf_profile_file: None,
        sim_trace_error: None,
        ir_dae_file: None,
        ir_solve_file: None,
        ir_solve_error: None,
        timeout_phase: None,
        timeout_seconds: None,
    }
}

fn is_non_sim_failure(phase: FailedPhase, error_code: Option<&str>) -> bool {
    match (phase, error_code) {
        (FailedPhase::Typecheck, Some(code)) => code == "ET004" || code.ends_with("ET004"),
        (FailedPhase::Instantiate, Some(code)) => code == "EI012" || code.ends_with("EI012"),
        _ => false,
    }
}

/// Convert PhaseResult to MslModelResult.
pub(super) fn convert_phase_result(name: String, phase_result: PhaseResult) -> MslModelResult {
    match phase_result {
        PhaseResult::Success(result) => summarize_success_result(name, result.as_ref()),
        PhaseResult::NeedsInner { missing_inners, .. } => phase_error_result(
            name,
            "NeedsInner",
            Some(format!("Missing inners: {}", missing_inners.join(", "))),
            None,
        ),
        PhaseResult::Failed {
            phase,
            error,
            error_code,
        } => {
            let mut phase_str = match phase {
                FailedPhase::Instantiate => "Instantiate",
                FailedPhase::Typecheck => "Typecheck",
                FailedPhase::Flatten => "Flatten",
                FailedPhase::ToDae => "ToDae",
            };
            if is_non_sim_failure(phase, error_code.as_deref()) {
                phase_str = "NonSim";
            }
            phase_error_result(name, phase_str, Some(error), error_code)
        }
    }
}

pub(super) fn summarize_success_result(
    name: String,
    result: &rumoca_compile::compile::CompilationResult,
) -> MslModelResult {
    let detail = rumoca_phase_dae::balance::balance_detail(&result.dae)
        .expect("MSL success summary requires valid DAE metadata");
    let discrete_scalars = active_discrete_scalar_count(&result.flat, &result.dae);
    summarize_dae_success_fields(name, &result.dae, &detail, discrete_scalars)
}

pub(super) fn summarize_dae_success_result(
    name: String,
    result: &rumoca_compile::compile::DaeCompilationResult,
) -> MslModelResult {
    summarize_dae_success_fields(
        name,
        result.dae.as_ref(),
        &result.balance_detail,
        result.active_discrete_scalar_count,
    )
}

fn summarize_dae_success_fields(
    name: String,
    dae: &Dae,
    detail: &rumoca_phase_dae::balance::BalanceDetail,
    discrete_scalars: i64,
) -> MslModelResult {
    let (scalar_equations, scalar_unknowns) = detail.equations_unknowns();
    let scalar_equations = scalar_equations as i64;
    let scalar_unknowns = scalar_unknowns as i64;
    let init_check = initialization_balance_check(dae, scalar_unknowns, scalar_equations);
    let scalar_equations_with_init = scalar_equations + init_check.closure_used;

    // OMC checkModel() includes top-level input connector scalars as local
    // unknowns with implicit binding equations, and includes when-only
    // discrete outputs in local counts. It may also use initialization
    // equations to close local deficits. Include these in reported
    // comparison counts while preserving eq-var parity.
    let input_scalars = dae
        .variables
        .inputs
        .values()
        .map(|v| v.size())
        .sum::<usize>() as i64;
    let balanced_discrete_scalars =
        (detail.discrete_real_unknowns + detail.discrete_valued_unknowns) as i64;
    let extra_discrete_report_scalars = (discrete_scalars - balanced_discrete_scalars).max(0);
    let report_offset = input_scalars + extra_discrete_report_scalars;
    let scalar_unknowns_for_report = scalar_unknowns + report_offset;
    let scalar_equations_for_report = scalar_equations_with_init + report_offset;
    let balance_for_report = scalar_equations_for_report - scalar_unknowns_for_report;
    MslModelResult {
        model_name: name,
        phase_reached: "Success".to_string(),
        error: None,
        error_code: None,
        num_states: Some(dae.variables.states.len()),
        num_algebraics: Some(dae.variables.algebraics.len()),
        num_f_x: Some(dae.continuous.equations.len()),
        balance: Some(balance_for_report),
        is_balanced: Some(balance_for_report == 0),
        is_partial: Some(dae.metadata.is_partial),
        class_type: Some(dae.metadata.class_type.as_str().to_string()),
        scalar_equations: usize::try_from(scalar_equations_for_report).ok(),
        scalar_unknowns: usize::try_from(scalar_unknowns_for_report).ok(),
        initial_equation_scalars: usize::try_from(init_check.initial_equation_scalars).ok(),
        initial_algorithm_scalars: usize::try_from(init_check.initial_algorithm_scalars).ok(),
        initial_balance_deficit_before: Some(init_check.deficit_before),
        initial_closure_used: usize::try_from(init_check.closure_used).ok(),
        initial_balance_deficit_after: Some(init_check.deficit_after),
        initial_balance_ok: Some(init_check.is_balanced()),
        compile_seconds: None,
        instantiate_seconds: None,
        typecheck_seconds: None,
        flatten_seconds: None,
        dae_seconds: None,
        compile_perf_profile_file: None,
        ir_ast_file: None,
        ir_flat_file: None,
        sim_status: None,
        sim_error: None,
        sim_error_span: None,
        ic_status: None,
        ic_error: None,
        ic_error_span: None,
        ic_seconds: None,
        sim_seconds: None,
        sim_build_seconds: None,
        ir_solve_seconds: None,
        ir_solve_structural_dae_seconds: None,
        ir_solve_lower_seconds: None,
        sim_backend_build_seconds: None,
        sim_run_seconds: None,
        sim_wall_seconds: None,
        sim_trace_file: None,
        sim_perf_profile_file: None,
        sim_trace_error: None,
        ir_dae_file: None,
        ir_solve_file: None,
        ir_solve_error: None,
        timeout_phase: None,
        timeout_seconds: None,
    }
}

pub(super) fn convert_compile_outcome(
    name: String,
    compile_outcome: ModelCompileOutcome,
) -> MslModelResult {
    match compile_outcome {
        ModelCompileOutcome::Phase(phase_result) => convert_phase_result(name, phase_result),
        ModelCompileOutcome::StrictDaeSuccess(result) => {
            summarize_dae_success_result(name, &result)
        }
        ModelCompileOutcome::StrictDaeFailure(failure_summary) => {
            let phase = strict_dae_failure_phase(&failure_summary);
            phase_error_result(name, phase, Some(failure_summary), None)
        }
        ModelCompileOutcome::StrictReport(report) => {
            let report = *report;
            let failure_summary = report.failure_summary(usize::MAX);
            let error_code = report
                .failures
                .iter()
                .find_map(|failure| failure.error_code.clone());
            match report.requested_result {
                Some(PhaseResult::Success(result)) if report.failures.is_empty() => {
                    convert_phase_result(name, PhaseResult::Success(result))
                }
                Some(phase_result @ PhaseResult::NeedsInner { .. })
                | Some(phase_result @ PhaseResult::Failed { .. }) => {
                    convert_phase_result(name, phase_result)
                }
                Some(PhaseResult::Success(_)) | None => {
                    let phase = strict_dae_failure_phase(&failure_summary);
                    phase_error_result(name, phase, Some(failure_summary), error_code)
                }
            }
        }
    }
}

fn strict_dae_failure_phase(failure_summary: &str) -> &'static str {
    const PHASE_MARKERS: &[(&str, &str)] = &[
        (" failed in Instantiate:", "Instantiate"),
        (" failed in Typecheck:", "Typecheck"),
        (" failed in Flatten:", "Flatten"),
        (" failed in ToDae:", "ToDae"),
    ];
    PHASE_MARKERS
        .iter()
        .find_map(|(marker, phase)| failure_summary.contains(marker).then_some(*phase))
        .unwrap_or("Resolve")
}

fn write_rendered_artifact<E>(
    render_result: Result<String, E>,
    path: std::path::PathBuf,
    rendered: &AtomicUsize,
    render_errors: &AtomicUsize,
) {
    if let Ok(code) = render_result {
        let _ = fs::write(path, code);
        rendered.fetch_add(1, Ordering::Relaxed);
    } else {
        render_errors.fetch_add(1, Ordering::Relaxed);
    }
}

fn pct(part: usize, total: usize) -> f64 {
    if total > 0 {
        (part as f64 / total as f64) * 100.0
    } else {
        0.0
    }
}

fn maybe_log_render_progress(run_simulation: bool, done: usize, total: usize) {
    if !run_simulation && (done.is_multiple_of(50) || done == total) {
        eprintln!("  render progress: {done}/{total}");
    }
}

struct RenderSimContext<'a> {
    run_simulation: bool,
    sim_target_names: Option<&'a HashSet<String>>,
    total_render_targets: usize,
    total_sim_targets: usize,
    dae_dir: &'a Path,
    flat_dir: &'a Path,
    dae_rendered: &'a AtomicUsize,
    flat_rendered: &'a AtomicUsize,
    render_errors: &'a AtomicUsize,
    sim_attempted: &'a AtomicUsize,
    sim_completed: &'a AtomicUsize,
    sim_ok_live: &'a AtomicUsize,
    sim_nan_live: &'a AtomicUsize,
    sim_timeout_live: &'a AtomicUsize,
    sim_solver_fail_live: &'a AtomicUsize,
    sim_balance_fail_live: &'a AtomicUsize,
    render_completed: &'a AtomicUsize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strict_dae_failure_phase_uses_reported_stage_marker() {
        assert_eq!(
            strict_dae_failure_phase("Modelica.A failed in Flatten: unsupported equation form"),
            "Flatten"
        );
        assert_eq!(
            strict_dae_failure_phase("Modelica.A failed in ToDae: unresolved reference"),
            "ToDae"
        );
        assert_eq!(strict_dae_failure_phase("resolve diagnostics"), "Resolve");
    }
}
