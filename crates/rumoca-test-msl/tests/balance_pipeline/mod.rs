use super::*;

mod balance_pipeline_core;
mod balance_pipeline_debug_introspection;
mod balance_pipeline_quality_gate;
mod balance_pipeline_render_sim;
mod balance_pipeline_reporting;
mod balance_pipeline_selection;
mod balance_pipeline_sim_worker;
mod balance_pipeline_stats_report;
mod balance_pipeline_summary;

use balance_pipeline_debug_introspection::*;
use balance_pipeline_quality_gate::*;
use balance_pipeline_render_sim::*;
use balance_pipeline_reporting::*;
use balance_pipeline_selection::*;
use balance_pipeline_sim_worker::*;
use balance_pipeline_stats_report::*;
use balance_pipeline_summary::*;

fn is_explicit_msl_example_model(model_name: &str) -> bool {
    model_name.starts_with("Modelica.") && model_name.contains(".Examples.")
}

/// Package segments that indicate support/helper classes within Examples trees.
const EXAMPLE_SUPPORT_SEGMENTS: &[&str] = &["Utilities", "BaseClasses", "Internal", "Interfaces"];

/// Root MSL examples by name:
/// - under `Modelica.*.Examples.*`
/// - not nested under support/helper package segments
fn is_root_msl_example_model_name(model_name: &str) -> bool {
    if !is_explicit_msl_example_model(model_name) {
        return false;
    }
    let Some((_, suffix)) = model_name.split_once(".Examples.") else {
        return false;
    };
    let mut segments: Vec<&str> = suffix.split('.').collect();
    if segments.len() <= 1 {
        return true;
    }
    // Exclude class name itself; only inspect package path under Examples.
    let _ = segments.pop();
    !segments
        .iter()
        .any(|seg| EXAMPLE_SUPPORT_SEGMENTS.contains(seg))
}

/// Root standalone MSL examples:
/// - root example by package name
/// - non-partial
/// - no top-level input connectors requiring external bindings
/// - no unbound fixed parameters (fixed=true by default for parameters)
fn is_root_standalone_msl_example_model(
    model_name: &str,
    result: &rumoca_session::compile::CompilationResult,
) -> bool {
    is_root_msl_example_model_name(model_name)
        && !result.dae.is_partial
        && result.dae.inputs.is_empty()
        && !result.flat.has_unbound_fixed_parameters()
}

fn msl_introspect_enabled() -> bool {
    std::env::var("RUMOCA_MSL_INTROSPECT").is_ok()
}

fn msl_introspect_eq_limit() -> usize {
    std::env::var("RUMOCA_MSL_INTROSPECT_EQ_LIMIT")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .unwrap_or(120)
}

static MSL_INTROSPECT_MATCH_PATTERNS: std::sync::OnceLock<Vec<String>> = std::sync::OnceLock::new();

fn msl_introspect_match_patterns() -> &'static [String] {
    MSL_INTROSPECT_MATCH_PATTERNS
        .get_or_init(|| {
            std::env::var("RUMOCA_MSL_INTROSPECT_MATCH")
                .ok()
                .map(|raw| {
                    raw.split(',')
                        .map(str::trim)
                        .filter(|s| !s.is_empty())
                        .map(ToOwned::to_owned)
                        .collect()
                })
                .unwrap_or_default()
        })
        .as_slice()
}

fn should_introspect_model(model_name: &str) -> bool {
    let pats = msl_introspect_match_patterns();
    pats.is_empty() || pats.iter().any(|pat| model_name.contains(pat))
}

fn msl_render_enabled() -> bool {
    std::env::var("RUMOCA_MSL_RENDER")
        .ok()
        .map(|raw| {
            matches!(
                raw.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

pub(super) const STAGE_WATCHDOG_LOG_INTERVAL_SECS: u64 = 15;
pub(super) const MODEL_ATTEMPT_TIMEOUT_SECS: f64 = 10.0;

pub(super) fn stage_timeout_seconds(env_key: &str, default_secs: u64) -> u64 {
    std::env::var(env_key)
        .ok()
        .and_then(|raw| raw.trim().parse::<u64>().ok())
        .filter(|secs| *secs > 0)
        .unwrap_or(default_secs)
}

pub(super) fn model_attempt_timeout_secs() -> f64 {
    std::env::var("RUMOCA_MSL_MODEL_ATTEMPT_TIMEOUT_SECS")
        .ok()
        .and_then(|raw| raw.trim().parse::<f64>().ok())
        .filter(|secs| secs.is_finite() && *secs > 0.0)
        .unwrap_or(MODEL_ATTEMPT_TIMEOUT_SECS)
}

pub(super) struct ModelCompileEntry {
    model_name: String,
    compile_outcome: ModelCompileOutcome,
    remaining_budget_secs: Option<f64>,
    compile_seconds: f64,
}

pub(super) enum ModelCompileOutcome {
    Phase(PhaseResult),
    StrictReport(StrictCompileReport),
}

impl ModelCompileOutcome {
    fn is_success(&self) -> bool {
        self.success_result().is_some()
    }

    fn success_result(&self) -> Option<&rumoca_session::compile::CompilationResult> {
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
    pub(super) fn new(stage_name: impl Into<String>, env_key: &str, default_secs: u64) -> Self {
        let stage_name = stage_name.into();
        let timeout_secs = stage_timeout_seconds(env_key, default_secs);
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
    /// Global library/session resolve failures that invalidate the whole run.
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
    /// Total solver/integration seconds (sum of per-model worker-reported runtime).
    #[serde(default)]
    total_sim_seconds: f64,
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
    undefined_vars: HashMap<String, usize>,
    balance_distribution: HashMap<i64, usize>,
    // Simulation counters
    sim_ok: usize,
    sim_nan: usize,
    sim_solver_fail: usize,
    sim_timeout: usize,
    sim_balance_fail: usize,
    sim_attempted: usize,
    total_sim_seconds: f64,
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
        total_sim_seconds: 0.0,
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
        sim_status: None,
        sim_error: None,
        sim_seconds: None,
        sim_wall_seconds: None,
        sim_trace_file: None,
        sim_trace_error: None,
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
        PhaseResult::NeedsInner { missing_inners } => phase_error_result(
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
    result: &rumoca_session::compile::CompilationResult,
) -> MslModelResult {
    let detail = rumoca_eval_dae::analysis::balance_detail(&result.dae);
    // Start from the exact DAE-balance basis (continuous unknowns/equations).
    let scalar_unknowns =
        (detail.state_unknowns + detail.alg_unknowns + detail.output_unknowns) as i64;
    let brk = detail.oc_break_edge_scalar_count as i64;
    let available_oc_interface = detail.overconstrained_interface_count.max(0);
    let base_without_iflow =
        (detail.f_x_scalar + detail.algorithm_outputs + detail.when_eq_scalar) as i64;
    let iflow_needed = (scalar_unknowns - base_without_iflow).max(0);
    let effective_iflow = (detail.interface_flow_count as i64).min(iflow_needed);
    let base_equations = base_without_iflow + effective_iflow;
    let oc_needed = (scalar_unknowns - base_equations).max(0);
    let effective_oc_interface = available_oc_interface.min(oc_needed);
    let raw_equations = base_equations + effective_oc_interface;
    let raw_balance = raw_equations - scalar_unknowns;
    let effective_brk = brk.min(raw_balance.max(0));
    let scalar_equations = raw_equations - effective_brk;
    let init_check = initialization_balance_check(&result.dae, scalar_unknowns, scalar_equations);
    let scalar_equations_with_init = scalar_equations + init_check.closure_used;

    // OMC checkModel() includes top-level input connector scalars as local
    // unknowns with implicit binding equations, and includes when-only
    // discrete outputs in local counts. It may also use initialization
    // equations to close local deficits. Include these in reported
    // comparison counts while preserving eq-var parity.
    let input_scalars = result.dae.inputs.values().map(|v| v.size()).sum::<usize>() as i64;
    let discrete_scalars = active_discrete_scalar_count(&result.flat, &result.dae);
    let report_offset = input_scalars + discrete_scalars;
    let scalar_unknowns_for_report = scalar_unknowns + report_offset;
    let scalar_equations_for_report = scalar_equations_with_init + report_offset;
    let balance_for_report = scalar_equations_for_report - scalar_unknowns_for_report;
    MslModelResult {
        model_name: name,
        phase_reached: "Success".to_string(),
        error: None,
        error_code: None,
        num_states: Some(result.dae.states.len()),
        num_algebraics: Some(result.dae.algebraics.len()),
        num_f_x: Some(result.dae.f_x.len()),
        balance: Some(balance_for_report),
        is_balanced: Some(balance_for_report == 0),
        is_partial: Some(result.dae.is_partial),
        class_type: Some(result.dae.class_type.as_str().to_string()),
        scalar_equations: usize::try_from(scalar_equations_for_report).ok(),
        scalar_unknowns: usize::try_from(scalar_unknowns_for_report).ok(),
        initial_equation_scalars: usize::try_from(init_check.initial_equation_scalars).ok(),
        initial_algorithm_scalars: usize::try_from(init_check.initial_algorithm_scalars).ok(),
        initial_balance_deficit_before: Some(init_check.deficit_before),
        initial_closure_used: usize::try_from(init_check.closure_used).ok(),
        initial_balance_deficit_after: Some(init_check.deficit_after),
        initial_balance_ok: Some(init_check.is_balanced()),
        compile_seconds: None,
        sim_status: None,
        sim_error: None,
        sim_seconds: None,
        sim_wall_seconds: None,
        sim_trace_file: None,
        sim_trace_error: None,
    }
}

pub(super) fn convert_compile_outcome(
    name: String,
    compile_outcome: ModelCompileOutcome,
) -> MslModelResult {
    match compile_outcome {
        ModelCompileOutcome::Phase(phase_result) => convert_phase_result(name, phase_result),
        ModelCompileOutcome::StrictReport(report) => {
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
                    phase_error_result(name, "Resolve", Some(failure_summary), error_code)
                }
            }
        }
    }
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
