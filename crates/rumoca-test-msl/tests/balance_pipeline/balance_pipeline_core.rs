use super::*;
mod streaming_workers;

use rumoca_worker::{
    MODEL_WORKER_PARTIAL_RESULT_FILE, MODEL_WORKER_PROTOCOL_VERSION, MODEL_WORKER_RESULT_FILE,
    ModelWorkerDaemon, ModelWorkerRequest, ModelWorkerRunOutcome, WorkerModelResult,
    WorkerProgressPhase, available_cpu_core_ids, cpu_core_plan, read_model_worker_response_file,
    write_model_worker_request_file,
};
use streaming_workers::*;

/// Run full MSL compile pipeline using Session for parallel compilation.
///
/// Set `run_simulation=false` for compile+balance-only runs.
const COMPILE_CHUNK_PROGRESS_INTERVAL_SECS: u64 = 15;
const COMPILE_CHUNK_PROGRESS_POLL_MILLIS: u64 = 250;
const MODEL_ATTEMPT_TIMEOUT_ERROR_CODE: &str = "EMSL_TIMEOUT_MODEL_ATTEMPT";
const MODEL_WORKER_ERROR_CODE: &str = "EMSL_MODEL_WORKER";
const COMPILE_PIPELINE_STAGE_BUDGETS: f64 = 4.0;
/// Slow-compile logging threshold (None = off). Edit to enable.
const SLOW_COMPILE_LOG_THRESHOLD_SECS: Option<f64> = None;
/// Record perf profiles during MSL compile. Edit to enable profiling.
const COMPILE_PERF_RECORD: bool = false;
/// Keep recorded compile perf profiles only for compiles slower than this.
const COMPILE_PERF_KEEP_THRESHOLD_SECS: f64 = 5.0;
/// `perf record` sampling frequency (Hz) for compile profiling.
const COMPILE_PERF_FREQ: usize = 99;

#[derive(Debug)]
struct ResourceTokenLimiter {
    capacity: usize,
    available: std::sync::Mutex<usize>,
    available_changed: std::sync::Condvar,
}

#[derive(Debug)]
struct ResourceTokenPermit {
    limiter: std::sync::Arc<ResourceTokenLimiter>,
    acquired: usize,
}

impl ResourceTokenLimiter {
    fn new(capacity: usize) -> Self {
        let capacity = capacity.max(1);
        Self {
            capacity,
            available: std::sync::Mutex::new(capacity),
            available_changed: std::sync::Condvar::new(),
        }
    }

    fn acquire(self: &std::sync::Arc<Self>, requested: usize) -> ResourceTokenPermit {
        let acquired = requested.clamp(1, self.capacity);
        let mut available = self
            .available
            .lock()
            .expect("resource token mutex should not be poisoned");
        while *available < acquired {
            available = self
                .available_changed
                .wait(available)
                .expect("resource token mutex should not be poisoned");
        }
        *available -= acquired;
        ResourceTokenPermit {
            limiter: std::sync::Arc::clone(self),
            acquired,
        }
    }

    fn acquire_timed(
        self: &std::sync::Arc<Self>,
        requested: usize,
    ) -> (ResourceTokenPermit, Duration) {
        let started = Instant::now();
        let permit = self.acquire(requested);
        (permit, started.elapsed())
    }

    fn capacity(&self) -> usize {
        self.capacity
    }

    #[cfg(test)]
    fn available_for_tests(&self) -> usize {
        *self
            .available
            .lock()
            .expect("resource token mutex should not be poisoned")
    }
}

impl Drop for ResourceTokenPermit {
    fn drop(&mut self) {
        let mut available = self
            .limiter
            .available
            .lock()
            .expect("resource token mutex should not be poisoned");
        *available = (*available + self.acquired).min(self.limiter.capacity);
        self.limiter.available_changed.notify_one();
    }
}

fn compile_memory_token_limiter() -> Option<std::sync::Arc<ResourceTokenLimiter>> {
    compile_memory_budget()
        .map(|budget| std::sync::Arc::new(ResourceTokenLimiter::new(budget.budget_mb)))
}

fn env_positive_usize(key: &str) -> Option<usize> {
    std::env::var(key)
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|value| *value > 0)
}

fn simulation_memory_token_limiter() -> Option<std::sync::Arc<ResourceTokenLimiter>> {
    // No total-memory limiter by default (sim worker count is the cap).
    None
}

fn pipeline_cpu_token_limiter(worker_threads: usize) -> std::sync::Arc<ResourceTokenLimiter> {
    std::sync::Arc::new(ResourceTokenLimiter::new(worker_threads.max(1)))
}

fn simulation_preparation_memory_cost_mb() -> usize {
    sim_worker_memory_limit_mb().unwrap_or_else(compile_model_memory_mb)
}

trait FocusedClosureCompiler {
    fn strict_compile_for_focused_model(&self, model_name: &str) -> StrictCompileReport;
    fn strict_compile_dae_for_focused_model(
        &self,
        model_name: &str,
    ) -> std::result::Result<Box<rumoca_compile::compile::DaeCompilationResult>, String>;
}

impl FocusedClosureCompiler for CompiledSourceRoot {
    fn strict_compile_for_focused_model(&self, model_name: &str) -> StrictCompileReport {
        self.compile_model_strict_reachable_uncached_with_recovery(model_name)
    }

    fn strict_compile_dae_for_focused_model(
        &self,
        model_name: &str,
    ) -> std::result::Result<Box<rumoca_compile::compile::DaeCompilationResult>, String> {
        self.compile_model_dae_strict_reachable_uncached_with_recovery(model_name)
    }
}

pub(super) fn log_simulation_run_configuration(run_simulation: bool) {
    if !run_simulation {
        return;
    }
    let compile_stage_budget = model_attempt_timeout_secs();
    println!("Per-model compile stage budget: {}s", compile_stage_budget);
    println!(
        "Per-model model worker phase timeout: {}s",
        compile_stage_budget
    );
    println!(
        "Model worker startup timeout: {}s",
        model_worker_startup_timeout_secs(compile_stage_budget)
    );
    println!("Per-model simulation timeout: {}s", sim_timeout_secs());
    if let Some(stop_time_override) = simulation_stop_time_override() {
        println!(
            "Simulation horizon mode: override stopTime={} via RUMOCA_MSL_SIM_STOP_TIME_OVERRIDE",
            stop_time_override
        );
    } else {
        println!("Simulation horizon mode: experiment StopTime when available");
    }
    println!(
        "Simulation experiment settings: applying annotation Tolerance/Interval/StartTime when valid"
    );
    if let Some(timeout_override) = sim_timeout_override_secs() {
        println!(
            "Simulation timeout override: {}s via RUMOCA_MSL_SIM_TIMEOUT_OVERRIDE",
            timeout_override
        );
    }
    if let Some(solver) = simulation_solver_override() {
        println!(
            "Simulation solver override: '{}' (accepts rumoca/OMC/Dymola-style names)",
            solver
        );
    }
}

pub(super) struct CompileSelection {
    compile_scope_count: usize,
    compile_names: Vec<String>,
}

pub(super) fn select_compile_models_for_run(
    source_root: &CompiledSourceRoot,
    model_names: &[String],
    run_simulation: bool,
) -> CompileSelection {
    let compile_scope_names =
        select_compile_targets_for_focused_simulation(source_root, model_names, run_simulation)
            .unwrap_or_else(|| model_names.to_vec());
    let compile_scope_count = compile_scope_names.len();
    CompileSelection {
        compile_scope_count,
        compile_names: compile_scope_names,
    }
}

fn slow_compile_log_threshold_secs_from_override(raw: Option<&str>) -> Option<f64> {
    raw.and_then(|value| value.trim().parse::<f64>().ok())
        .filter(|secs| secs.is_finite() && *secs > 0.0)
}

fn slow_compile_log_threshold_secs() -> Option<f64> {
    SLOW_COMPILE_LOG_THRESHOLD_SECS
}

#[derive(Debug, Clone)]
struct CompilePerfArtifacts {
    profile_path: PathBuf,
    relative_path: String,
}

impl CompilePerfArtifacts {
    fn create(model_name: &str) -> Option<Self> {
        if !COMPILE_PERF_RECORD {
            return None;
        }
        let perf_dir = msl_results_dir().join("perf").join("compile");
        if let Err(error) = fs::create_dir_all(&perf_dir) {
            eprintln!("WARNING: failed to create compile perf profile directory: {error}");
            return None;
        }
        let file_name = format!("{model_name}.perf.data");
        let profile_path = perf_dir.join(&file_name);
        if profile_path.exists() {
            let _ = fs::remove_file(&profile_path);
        }
        let relative_path = Path::new("perf")
            .join("compile")
            .join(file_name)
            .to_string_lossy()
            .to_string();
        Some(Self {
            profile_path,
            relative_path,
        })
    }
}

fn compile_perf_keep_threshold_secs() -> f64 {
    COMPILE_PERF_KEEP_THRESHOLD_SECS
}

fn compile_perf_frequency() -> usize {
    COMPILE_PERF_FREQ
}

fn model_worker_startup_timeout_secs(model_phase_budget_secs: f64) -> f64 {
    // 6x the per-model phase budget (always >= the budget itself).
    model_phase_budget_secs * 6.0
}

fn start_compile_perf_session(artifacts: Option<&CompilePerfArtifacts>) -> Option<PerfSession> {
    let artifacts = artifacts?;
    start_current_thread_perf_record_session(
        &artifacts.profile_path,
        compile_perf_frequency(),
        "model worker",
    )
}

fn retain_compile_perf_profile(
    artifacts: Option<&CompilePerfArtifacts>,
    elapsed_secs: f64,
    compile_outcome: &ModelCompileOutcome,
) -> Option<String> {
    retain_compile_perf_profile_if(artifacts, elapsed_secs, !compile_outcome.is_success())
}

fn retain_compile_timeout_perf_profile(
    artifacts: Option<&CompilePerfArtifacts>,
    elapsed_secs: f64,
) -> Option<String> {
    retain_compile_perf_profile_if(artifacts, elapsed_secs, true)
}

fn retain_compile_perf_profile_if(
    artifacts: Option<&CompilePerfArtifacts>,
    elapsed_secs: f64,
    force_keep: bool,
) -> Option<String> {
    let artifacts = artifacts?;
    if !artifact_file_ready(&artifacts.profile_path) {
        return None;
    }
    let should_keep = force_keep || elapsed_secs >= compile_perf_keep_threshold_secs();
    if should_keep {
        Some(artifacts.relative_path.clone())
    } else {
        let _ = fs::remove_file(&artifacts.profile_path);
        None
    }
}

type SharedPerfSession = std::sync::Arc<std::sync::Mutex<Option<PerfSession>>>;

fn finish_shared_perf_session(session: &SharedPerfSession) {
    let session = session
        .lock()
        .expect("compile perf session mutex should not be poisoned")
        .take();
    if let Some(session) = session {
        session.finish();
    }
}

fn log_compile_batch_limit(run_simulation: bool, compile_count: usize) {
    if let Some(budget) = compile_memory_budget() {
        println!(
            "  Compile memory limiter: {} MB via {}, model token estimate {} MB, cap {} concurrent model tokens",
            budget.budget_mb,
            budget.source.as_str(),
            budget.per_model_mb,
            budget.model_cap.min(compile_count.max(1))
        );
        if !run_simulation {
            println!("  Compile batch limiter: default compile-only batch policy");
        }
        return;
    }
    println!("  Compile batch limiter: default compile-only batch policy");
}

fn delta_compile_timing_stat(
    before: rumoca_compile::compile::CompilePhaseTimingStat,
    after: rumoca_compile::compile::CompilePhaseTimingStat,
) -> rumoca_compile::compile::CompilePhaseTimingStat {
    rumoca_compile::compile::CompilePhaseTimingStat {
        calls: after.calls.saturating_sub(before.calls),
        total_nanos: after.total_nanos.saturating_sub(before.total_nanos),
    }
}

fn delta_compile_phase_timing_snapshot(
    before: rumoca_compile::compile::CompilePhaseTimingSnapshot,
    after: rumoca_compile::compile::CompilePhaseTimingSnapshot,
) -> rumoca_compile::compile::CompilePhaseTimingSnapshot {
    rumoca_compile::compile::CompilePhaseTimingSnapshot {
        instantiate: delta_compile_timing_stat(before.instantiate, after.instantiate),
        typecheck: delta_compile_timing_stat(before.typecheck, after.typecheck),
        flatten: delta_compile_timing_stat(before.flatten, after.flatten),
        todae: delta_compile_timing_stat(before.todae, after.todae),
    }
}

fn delta_flatten_timing_stat(
    before: rumoca_phase_flatten::FlattenPhaseTimingStat,
    after: rumoca_phase_flatten::FlattenPhaseTimingStat,
) -> rumoca_phase_flatten::FlattenPhaseTimingStat {
    rumoca_phase_flatten::FlattenPhaseTimingStat {
        calls: after.calls.saturating_sub(before.calls),
        total_nanos: after.total_nanos.saturating_sub(before.total_nanos),
    }
}

fn delta_flatten_phase_timing_snapshot(
    before: rumoca_phase_flatten::FlattenPhaseTimingSnapshot,
    after: rumoca_phase_flatten::FlattenPhaseTimingSnapshot,
) -> rumoca_phase_flatten::FlattenPhaseTimingSnapshot {
    rumoca_phase_flatten::FlattenPhaseTimingSnapshot {
        connections: delta_flatten_timing_stat(before.connections, after.connections),
        eval_fallback: delta_flatten_timing_stat(before.eval_fallback, after.eval_fallback),
    }
}

fn log_slow_model_compile(
    model_name: &str,
    elapsed_secs: f64,
    compile_delta: rumoca_compile::compile::CompilePhaseTimingSnapshot,
    flatten_delta: rumoca_phase_flatten::FlattenPhaseTimingSnapshot,
) {
    eprintln!(
        "    slow compile: model={model_name} elapsed={elapsed_secs:.2}s | instantiate={:.2}s/{} typecheck={:.2}s/{} flatten={:.2}s/{} todae={:.2}s/{} | flatten.connections={:.2}s/{} eval_fallback={:.2}s/{}",
        compile_delta.instantiate.total_seconds(),
        compile_delta.instantiate.calls,
        compile_delta.typecheck.total_seconds(),
        compile_delta.typecheck.calls,
        compile_delta.flatten.total_seconds(),
        compile_delta.flatten.calls,
        compile_delta.todae.total_seconds(),
        compile_delta.todae.calls,
        flatten_delta.connections.total_seconds(),
        flatten_delta.connections.calls,
        flatten_delta.eval_fallback.total_seconds(),
        flatten_delta.eval_fallback.calls,
    );
}

struct CompileRenderOutput {
    model_results: Vec<MslModelResult>,
    compile_only_seconds: f64,
    render_and_write_seconds: f64,
    batch_size: usize,
    chunk_count: usize,
    worker_threads: usize,
    scheduler: MslSchedulerTimings,
}

struct ParsedMslBatch {
    total_mo_files: usize,
    parse_errors: usize,
    successes: Vec<(String, rumoca_ir_ast::StoredDefinition)>,
}

fn parse_msl_batch(msl_dir: &Path, timings: &mut MslPhaseTimings) -> ParsedMslBatch {
    let mo_files = find_mo_files(msl_dir);
    let total_mo_files = mo_files.len();
    println!("Parsing {} MSL files in parallel...", total_mo_files);
    let parse_start = Instant::now();
    let _parse_watchdog = StageAbortWatchdog::new("parse_msl_batch", 600);
    let parse_threads = msl_stage_parallelism();
    let parse_work = || parse_files_parallel_lenient(&mo_files);
    let (successes, failures) = match rayon::ThreadPoolBuilder::new()
        .num_threads(parse_threads.max(1))
        .build()
    {
        Ok(pool) => pool.install(parse_work),
        Err(err) => {
            eprintln!(
                "WARNING: failed to build parse thread pool ({err}); falling back to sequential parse"
            );
            parse_files_lenient_sequential(&mo_files)
        }
    };
    timings.parse_seconds = parse_start.elapsed().as_secs_f64();
    let parse_errors = failures.len();
    drop(failures);
    drop(mo_files);
    println!(
        "Parsed {} files successfully, {} failures in {:.2}s",
        successes.len(),
        parse_errors,
        timings.parse_seconds
    );
    ParsedMslBatch {
        total_mo_files,
        parse_errors,
        successes,
    }
}

fn parse_files_lenient_sequential(paths: &[PathBuf]) -> LenientParseResult {
    let mut successes = Vec::new();
    let mut failures = Vec::new();
    for path in paths {
        let file_name = path.to_string_lossy().to_string();
        match std::fs::read_to_string(path)
            .map_err(|error| error.to_string())
            .and_then(|source| {
                rumoca_phase_parse::parse_to_ast(&source, &file_name)
                    .map_err(|error| error.to_string())
            }) {
            Ok(definition) => successes.push((file_name, definition)),
            Err(error) => failures.push((file_name, error)),
        }
    }
    (successes, failures)
}

fn compile_timeout_phase_result(
    model_name: &str,
    elapsed_secs: f64,
    phase_timeout_secs: f64,
    active_phase: Option<WorkerProgressPhase>,
) -> PhaseResult {
    let failed_phase = match active_phase {
        Some(WorkerProgressPhase::Typecheck) => FailedPhase::Typecheck,
        Some(WorkerProgressPhase::Flatten) => FailedPhase::Flatten,
        Some(WorkerProgressPhase::Instantiate | WorkerProgressPhase::SourceRootLoad) => {
            FailedPhase::Instantiate
        }
        Some(
            WorkerProgressPhase::Compile
            | WorkerProgressPhase::ToDae
            | WorkerProgressPhase::Solve
            | WorkerProgressPhase::SimBuild
            | WorkerProgressPhase::IC
            | WorkerProgressPhase::Sim
            | WorkerProgressPhase::ArtifactWrite
            | WorkerProgressPhase::Memory,
        )
        | None => FailedPhase::ToDae,
    };
    PhaseResult::Failed {
        phase: failed_phase,
        error: timeout_message(model_name, elapsed_secs, phase_timeout_secs, active_phase),
        error_code: Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE.to_string()),
        diagnostics: Vec::new(),
    }
}

fn model_worker_wall_timeout_secs(stage_budget_secs: f64) -> f64 {
    stage_budget_secs * COMPILE_PIPELINE_STAGE_BUDGETS
}

fn streaming_queue_bound(worker_threads: usize, model_count: usize) -> usize {
    worker_threads_for_model_count(worker_threads, model_count)
}

fn worker_threads_for_model_count(requested_threads: usize, model_count: usize) -> usize {
    requested_threads.max(1).min(model_count.max(1))
}

fn work_queue_waves(model_count: usize, worker_threads: usize) -> u64 {
    let workers = worker_threads_for_model_count(worker_threads, model_count);
    model_count.max(1).div_ceil(workers) as u64
}

fn global_queue_compile_timeout_secs(
    model_count: usize,
    worker_threads: usize,
    model_budget_secs: f64,
) -> u64 {
    let per_wave_secs = model_worker_wall_timeout_secs(model_budget_secs).ceil() as u64;
    work_queue_waves(model_count, worker_threads)
        .saturating_mul(per_wave_secs.max(1))
        .max(300)
}

fn global_queue_sim_timeout_secs(
    model_count: usize,
    worker_threads: usize,
    model_budget_secs: f64,
) -> u64 {
    let compile_secs = model_worker_wall_timeout_secs(model_budget_secs).ceil() as u64;
    let sim_secs = sim_timeout_secs().ceil() as u64;
    let per_wave_secs = compile_secs.saturating_add(sim_secs).saturating_add(5);
    work_queue_waves(model_count, worker_threads)
        .saturating_mul(per_wave_secs.max(1))
        .max(300)
}

fn queue_stage_label(stage: &str, chunk_idx: usize, chunk_count: usize) -> String {
    if chunk_count == 1 {
        format!("{stage} global queue")
    } else {
        format!("{stage} chunk {chunk_idx}/{chunk_count}")
    }
}

fn finalize_compile_entry(
    model_name: &str,
    compile_outcome: ModelCompileOutcome,
    elapsed_secs: f64,
    budget_secs: f64,
    compile_perf_profile_file: Option<String>,
) -> ModelCompileEntry {
    let remaining_budget_secs = if compile_outcome.is_success() {
        // Keep the compile budget and simulation timeout independent. Once
        // compile finishes within budget, the sim worker should still receive
        // the nominal solver timeout rather than "10s minus compile time",
        // otherwise near-threshold models regress due to compile overhead
        // instead of simulation behavior.
        Some(budget_secs)
    } else {
        None
    };
    ModelCompileEntry {
        model_name: model_name.to_string(),
        compile_outcome,
        remaining_budget_secs,
        compile_seconds: elapsed_secs,
        compile_perf_profile_file,
    }
}

fn full_compile_artifacts_required() -> bool {
    msl_render_enabled() || msl_introspect_enabled()
}

fn run_compile_model_attempt<T: FocusedClosureCompiler + Sync + Send>(
    source_root: &std::sync::Arc<T>,
    model_name: &str,
    slow_log_threshold: Option<f64>,
) -> ModelCompileOutcome {
    let compile_timing_before = slow_log_threshold.map(|_| compile_phase_timing_stats());
    let flatten_timing_before = slow_log_threshold.map(|_| flatten_phase_timing_stats());
    let start = Instant::now();
    let compile_outcome = if full_compile_artifacts_required() {
        ModelCompileOutcome::StrictReport(Box::new(
            source_root.strict_compile_for_focused_model(model_name),
        ))
    } else {
        match source_root.strict_compile_dae_for_focused_model(model_name) {
            Ok(result) => ModelCompileOutcome::StrictDaeSuccess(result),
            Err(summary) => ModelCompileOutcome::StrictDaeFailure(summary),
        }
    };
    let elapsed_secs = start.elapsed().as_secs_f64();
    if let (Some(threshold_secs), Some(before_compile), Some(before_flatten)) = (
        slow_log_threshold,
        compile_timing_before,
        flatten_timing_before,
    ) && elapsed_secs >= threshold_secs
    {
        let compile_delta =
            delta_compile_phase_timing_snapshot(before_compile, compile_phase_timing_stats());
        let flatten_delta =
            delta_flatten_phase_timing_snapshot(before_flatten, flatten_phase_timing_stats());
        log_slow_model_compile(model_name, elapsed_secs, compile_delta, flatten_delta);
    }
    compile_outcome
}

fn compile_model_with_budget_timeout<T: FocusedClosureCompiler + Sync + Send + 'static>(
    source_root: &std::sync::Arc<T>,
    model_name: &str,
    budget_secs: f64,
    memory_permit: Option<ResourceTokenPermit>,
    cpu_permit: Option<ResourceTokenPermit>,
) -> ModelCompileEntry {
    let (result_tx, result_rx) = std::sync::mpsc::sync_channel(1);
    let wall_timeout_secs = model_worker_wall_timeout_secs(budget_secs);
    let source_root = std::sync::Arc::clone(source_root);
    let model_name_owned = model_name.to_string();
    let compile_perf_artifacts = CompilePerfArtifacts::create(model_name);
    let compile_perf_session = std::sync::Arc::new(std::sync::Mutex::new(None));
    let worker_perf_artifacts = compile_perf_artifacts.clone();
    let worker_perf_session = std::sync::Arc::clone(&compile_perf_session);
    let spawn_result = std::thread::Builder::new()
        .name(format!("rumoca-msl-compile-{model_name}"))
        .spawn(move || {
            let _memory_permit = memory_permit;
            let _cpu_permit = cpu_permit;
            let perf_session = start_compile_perf_session(worker_perf_artifacts.as_ref());
            if let Some(perf_session) = perf_session {
                *worker_perf_session
                    .lock()
                    .expect("compile perf session mutex should not be poisoned") =
                    Some(perf_session);
            }
            let start = Instant::now();
            let slow_log_threshold = slow_compile_log_threshold_secs();
            let compile_outcome =
                run_compile_model_attempt(&source_root, &model_name_owned, slow_log_threshold);
            let elapsed_secs = start.elapsed().as_secs_f64();
            finish_shared_perf_session(&worker_perf_session);
            let compile_perf_profile_file = retain_compile_perf_profile(
                worker_perf_artifacts.as_ref(),
                elapsed_secs,
                &compile_outcome,
            );
            let _ = result_tx.send((compile_outcome, elapsed_secs, compile_perf_profile_file));
        });
    if let Err(err) = spawn_result {
        return finalize_compile_entry(
            model_name,
            ModelCompileOutcome::Phase(PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: format!("failed to spawn model worker: {err}"),
                error_code: Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE.to_string()),
                diagnostics: Vec::new(),
            }),
            0.0,
            budget_secs,
            None,
        );
    }

    match result_rx.recv_timeout(Duration::from_secs_f64(wall_timeout_secs)) {
        Ok((compile_outcome, elapsed_secs, compile_perf_profile_file)) => finalize_compile_entry(
            model_name,
            compile_outcome,
            elapsed_secs,
            budget_secs,
            compile_perf_profile_file,
        ),
        Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
            finish_shared_perf_session(&compile_perf_session);
            let compile_perf_profile_file = retain_compile_timeout_perf_profile(
                compile_perf_artifacts.as_ref(),
                wall_timeout_secs,
            );
            finalize_compile_entry(
                model_name,
                ModelCompileOutcome::Phase(compile_timeout_phase_result(
                    model_name,
                    wall_timeout_secs,
                    wall_timeout_secs,
                    None,
                )),
                wall_timeout_secs,
                budget_secs,
                compile_perf_profile_file,
            )
        }
        Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => finalize_compile_entry(
            model_name,
            ModelCompileOutcome::Phase(PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: "model worker disconnected before reporting a result".to_string(),
                error_code: Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE.to_string()),
                diagnostics: Vec::new(),
            }),
            0.0,
            budget_secs,
            None,
        ),
    }
}

fn model_worker_failure_entry(
    model_name: &str,
    budget_secs: f64,
    error_code: &str,
    error: impl Into<String>,
) -> ModelCompileEntry {
    finalize_compile_entry(
        model_name,
        ModelCompileOutcome::Phase(PhaseResult::Failed {
            phase: FailedPhase::ToDae,
            error: error.into(),
            error_code: Some(error_code.to_string()),
            diagnostics: Vec::new(),
        }),
        0.0,
        budget_secs,
        None,
    )
}

fn worker_model_result_to_msl(result: WorkerModelResult) -> MslModelResult {
    MslModelResult {
        model_name: result.model_name,
        phase_reached: result.phase_reached,
        error: result.error,
        error_code: result.error_code,
        num_states: result.num_states,
        num_algebraics: result.num_algebraics,
        num_f_x: result.num_f_x,
        balance: result.balance,
        is_balanced: result.is_balanced,
        is_partial: result.is_partial,
        class_type: result.class_type,
        scalar_equations: result.scalar_equations,
        scalar_unknowns: result.scalar_unknowns,
        initial_equation_scalars: result.initial_equation_scalars,
        initial_algorithm_scalars: result.initial_algorithm_scalars,
        initial_balance_deficit_before: result.initial_balance_deficit_before,
        initial_closure_used: result.initial_closure_used,
        initial_balance_deficit_after: result.initial_balance_deficit_after,
        initial_balance_ok: result.initial_balance_ok,
        compile_seconds: result.compile_seconds,
        instantiate_seconds: result.instantiate_seconds,
        typecheck_seconds: result.typecheck_seconds,
        flatten_seconds: result.flatten_seconds,
        dae_seconds: result.dae_seconds,
        compile_perf_profile_file: result.compile_perf_profile_file,
        ir_ast_file: result.ir_ast_file,
        ir_flat_file: result.ir_flat_file,
        sim_status: result.sim_status,
        sim_error: result.sim_error,
        sim_error_span: result.sim_error_span,
        ic_status: result.ic_status,
        ic_error: result.ic_error,
        ic_error_span: result.ic_error_span,
        ic_seconds: result.ic_seconds,
        sim_seconds: result.sim_seconds,
        sim_build_seconds: result.sim_build_seconds,
        ir_solve_seconds: result.ir_solve_seconds,
        ir_solve_structural_dae_seconds: result.ir_solve_structural_dae_seconds,
        ir_solve_lower_seconds: result.ir_solve_lower_seconds,
        sim_backend_build_seconds: result.sim_backend_build_seconds,
        sim_run_seconds: result.sim_run_seconds,
        sim_wall_seconds: result.sim_wall_seconds,
        sim_trace_file: result.sim_trace_file,
        sim_perf_profile_file: result.sim_perf_profile_file,
        sim_trace_error: result.sim_trace_error,
        ir_dae_file: result.ir_dae_file,
        ir_solve_file: result.ir_solve_file,
        ir_solve_error: result.ir_solve_error,
        timeout_phase: result.timeout_phase,
        timeout_seconds: result.timeout_seconds,
    }
}

fn model_worker_failure_result(
    model_name: &str,
    error_code: &str,
    error: impl Into<String>,
) -> MslModelResult {
    worker_model_result_to_msl(WorkerModelResult::phase_failure(
        model_name.to_string(),
        "ToDae",
        error.into(),
        Some(error_code.to_string()),
    ))
}

fn model_worker_artifact_dir_name(model_name: &str) -> String {
    model_name
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || matches!(ch, '.' | '-' | '_') {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

fn model_worker_output_dir(model_name: &str) -> PathBuf {
    msl_results_dir()
        .join("model_worker")
        .join(model_worker_artifact_dir_name(model_name))
}

fn compile_timeout_model_result(
    model_name: &str,
    elapsed_secs: f64,
    phase_timeout_secs: f64,
    active_phase: Option<WorkerProgressPhase>,
) -> MslModelResult {
    let mut result = convert_phase_result(
        model_name.to_string(),
        compile_timeout_phase_result(model_name, elapsed_secs, phase_timeout_secs, active_phase),
    );
    result.compile_seconds = Some(elapsed_secs);
    result.timeout_phase = active_phase;
    result.timeout_seconds = Some(phase_timeout_secs);
    result
}

fn timeout_message(
    model_name: &str,
    elapsed_secs: f64,
    phase_timeout_secs: f64,
    active_phase: Option<WorkerProgressPhase>,
) -> String {
    let active_phase = active_phase
        .map(|phase| phase.to_string())
        .unwrap_or_else(|| "Unknown".to_string());
    format!(
        "model attempt timeout: model worker exceeded {:.3}s phase budget after {:.3}s in phase {active_phase} ({model_name})",
        phase_timeout_secs, elapsed_secs,
    )
}

fn simulation_timeout_model_result(
    model_name: &str,
    output_dir: &Path,
    elapsed_secs: f64,
    phase_timeout_secs: f64,
    active_phase: Option<WorkerProgressPhase>,
) -> Option<MslModelResult> {
    let partial_path = output_dir.join(MODEL_WORKER_PARTIAL_RESULT_FILE);
    let mut result =
        worker_model_result_to_msl(read_model_worker_response_file(&partial_path).ok()?.result);
    if result.phase_reached != "Success" {
        return None;
    }
    let message = timeout_message(model_name, elapsed_secs, phase_timeout_secs, active_phase);
    result.timeout_phase = active_phase;
    result.timeout_seconds = Some(phase_timeout_secs);
    match active_phase {
        Some(WorkerProgressPhase::Solve) => {
            result.ir_solve_error = Some(message.clone());
            result.sim_status = Some("sim_timeout".to_string());
            result.sim_error = Some(message);
            result.sim_seconds = Some(phase_timeout_secs);
            result.sim_wall_seconds = Some(phase_timeout_secs);
        }
        Some(WorkerProgressPhase::SimBuild) => {
            result.sim_status = Some("sim_timeout".to_string());
            result.sim_error = Some(message);
            result.sim_seconds = Some(phase_timeout_secs);
            result.sim_wall_seconds = Some(phase_timeout_secs);
        }
        Some(WorkerProgressPhase::IC) => {
            result.ic_status = Some("ic_timeout".to_string());
            result.ic_error = Some(message.clone());
            result.sim_status = Some("sim_timeout".to_string());
            result.sim_error = Some(message);
            result.ic_seconds = Some(phase_timeout_secs);
            result.sim_seconds = Some(phase_timeout_secs);
            result.sim_wall_seconds = Some(phase_timeout_secs);
        }
        Some(WorkerProgressPhase::Sim) => {
            result.ic_status = Some("ic_ok".to_string());
            result.sim_status = Some("sim_timeout".to_string());
            result.sim_error = Some(message);
            result.sim_seconds = Some(phase_timeout_secs);
            result.sim_wall_seconds = Some(phase_timeout_secs);
        }
        _ => return None,
    }
    Some(result)
}

#[derive(Clone, Copy)]
struct InProcessWorkerRequest<'a> {
    source_root_path: &'a Path,
    cpu_core_id: Option<usize>,
    model_name: &'a str,
    budget_secs: f64,
    startup_timeout_secs: f64,
    run_simulation: bool,
    selected_for_simulation: bool,
    explicit_sim_target: bool,
}

fn run_compile_model_in_process_worker(
    worker: &mut Option<ModelWorkerDaemon>,
    scheduler_stats: &SchedulerStatsCollector,
    plan: InProcessWorkerRequest<'_>,
) -> (MslModelResult, bool) {
    let phase_timeout_secs = plan.budget_secs;
    let compile_perf_artifacts = CompilePerfArtifacts::create(plan.model_name);
    let (output_dir, progress_jsonl, request) = match prepare_model_worker_request(plan) {
        Ok(prepared) => prepared,
        Err(result) => return (*result, false),
    };
    if worker.is_none() {
        match ModelWorkerDaemon::spawn(
            plan.source_root_path,
            plan.startup_timeout_secs,
            plan.cpu_core_id,
        ) {
            Ok(spawned) => {
                scheduler_stats.record_worker_affinity(spawned.cpu_affinity_applied());
                *worker = Some(spawned);
            }
            Err(error) => {
                return (
                    model_worker_failure_result(
                        plan.model_name,
                        MODEL_WORKER_ERROR_CODE,
                        format!("model worker failed to start: {error}"),
                    ),
                    false,
                );
            }
        }
    }
    let worker_ref = worker
        .as_mut()
        .expect("model worker should be available after spawn");
    let perf_session = compile_perf_artifacts.as_ref().and_then(|artifacts| {
        start_perf_record_session(
            PerfRecordTarget::Process(worker_ref.process_id()),
            &artifacts.profile_path,
            compile_perf_frequency(),
            "model worker",
        )
    });
    let outcome = worker_ref.run_request(&request, phase_timeout_secs, &progress_jsonl);
    if let Some(session) = perf_session {
        session.finish();
    }
    match outcome {
        ModelWorkerRunOutcome::Completed(response) => {
            let response = *response;
            let mut result = worker_model_result_to_msl(response.result);
            let force_keep = result.error.is_some()
                || result
                    .sim_status
                    .as_deref()
                    .is_some_and(|status| status != "sim_ok");
            result.compile_perf_profile_file = retain_compile_perf_profile_if(
                compile_perf_artifacts.as_ref(),
                response.elapsed_secs,
                force_keep,
            );
            (result, true)
        }
        ModelWorkerRunOutcome::TimedOut {
            elapsed_secs,
            active_phase,
            phase_timeout_secs,
        } => {
            *worker = None;
            let compile_perf_profile_file =
                retain_compile_timeout_perf_profile(compile_perf_artifacts.as_ref(), elapsed_secs);
            let mut result = simulation_timeout_model_result(
                plan.model_name,
                &output_dir,
                elapsed_secs,
                phase_timeout_secs,
                active_phase,
            )
            .unwrap_or_else(|| {
                compile_timeout_model_result(
                    plan.model_name,
                    elapsed_secs,
                    phase_timeout_secs,
                    active_phase,
                )
            });
            result.compile_perf_profile_file = compile_perf_profile_file;
            (result, false)
        }
        ModelWorkerRunOutcome::Failed(error) => {
            *worker = None;
            (
                model_worker_failure_result(
                    plan.model_name,
                    MODEL_WORKER_ERROR_CODE,
                    format!("model worker failed: {error}"),
                ),
                false,
            )
        }
    }
}

fn prepare_model_worker_request(
    plan: InProcessWorkerRequest<'_>,
) -> Result<(PathBuf, PathBuf, ModelWorkerRequest), Box<MslModelResult>> {
    let output_dir = model_worker_output_dir(plan.model_name);
    if let Err(error) = fs::create_dir_all(&output_dir) {
        return Err(Box::new(model_worker_failure_result(
            plan.model_name,
            MODEL_WORKER_ERROR_CODE,
            format!("failed to create model worker output directory: {error}"),
        )));
    }
    let request_json = output_dir.join("request.json");
    let request = ModelWorkerRequest {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        model_name: plan.model_name.to_string(),
        run_simulation: plan.run_simulation,
        selected_for_simulation: plan.selected_for_simulation,
        explicit_sim_target: plan.explicit_sim_target,
        emit_json: false,
        allow_unbalanced_for_diagnostics: false,
        nan_trace: false,
        emit_modelica: false,
        source_root_path: plan.source_root_path.to_path_buf(),
        output_dir: output_dir.clone(),
    };
    if let Err(error) = write_model_worker_request_file(&request_json, &request) {
        return Err(Box::new(model_worker_failure_result(
            plan.model_name,
            MODEL_WORKER_ERROR_CODE,
            format!("failed to write model worker request: {error}"),
        )));
    }
    let progress_jsonl = output_dir.join("progress.jsonl");
    let _ = fs::remove_file(&progress_jsonl);
    let _ = fs::remove_file(output_dir.join(MODEL_WORKER_RESULT_FILE));
    let _ = fs::remove_file(output_dir.join(MODEL_WORKER_PARTIAL_RESULT_FILE));
    Ok((output_dir, progress_jsonl, request))
}

struct CompileResourcePlan<'a> {
    budget_secs: f64,
    memory_tokens: Option<std::sync::Arc<ResourceTokenLimiter>>,
    memory_costs_mb: &'a HashMap<String, usize>,
}

fn send_compile_chunk_with_model_budgets<T: FocusedClosureCompiler + Sync + Send + 'static>(
    source_root: &std::sync::Arc<T>,
    names_chunk: &[String],
    resources: &CompileResourcePlan<'_>,
    result_tx: std::sync::mpsc::SyncSender<(usize, ModelCompileEntry)>,
) {
    names_chunk
        .par_iter()
        .enumerate()
        .for_each_with(result_tx, |tx, (idx, name)| {
            let memory_cost_mb = resources
                .memory_costs_mb
                .get(name)
                .copied()
                .unwrap_or_else(compile_model_memory_mb);
            let _memory_permit = resources
                .memory_tokens
                .as_ref()
                .map(|tokens| tokens.acquire(memory_cost_mb));
            let entry = compile_model_with_budget_timeout(
                source_root,
                name,
                resources.budget_secs,
                _memory_permit,
                None,
            );
            let _ = tx.send((idx, entry));
        });
}

fn send_compile_chunk_sequential_with_model_budgets<
    T: FocusedClosureCompiler + Sync + Send + 'static,
>(
    source_root: &std::sync::Arc<T>,
    names_chunk: &[String],
    resources: &CompileResourcePlan<'_>,
    result_tx: std::sync::mpsc::SyncSender<(usize, ModelCompileEntry)>,
) {
    for (idx, name) in names_chunk.iter().enumerate() {
        let memory_cost_mb = resources
            .memory_costs_mb
            .get(name)
            .copied()
            .unwrap_or_else(compile_model_memory_mb);
        let _memory_permit = resources
            .memory_tokens
            .as_ref()
            .map(|tokens| tokens.acquire(memory_cost_mb));
        let entry = compile_model_with_budget_timeout(
            source_root,
            name,
            resources.budget_secs,
            _memory_permit,
            None,
        );
        let _ = result_tx.send((idx, entry));
    }
}

fn stream_compile_chunk_with_model_budgets<T, F>(
    source_root: &std::sync::Arc<T>,
    names_chunk: &[String],
    compile_threads: usize,
    budget_secs: f64,
    consume: F,
) -> f64
where
    T: FocusedClosureCompiler + Sync + Send + 'static,
    F: FnMut(usize, ModelCompileEntry) + Send,
{
    let memory_tokens = compile_memory_token_limiter();
    let memory_costs_mb = compile_model_memory_costs_for_names(names_chunk);
    let resources = CompileResourcePlan {
        budget_secs,
        memory_tokens: memory_tokens.clone(),
        memory_costs_mb: &memory_costs_mb,
    };
    let effective_compile_threads =
        worker_threads_for_model_count(compile_threads, names_chunk.len());
    let result_queue_bound = streaming_queue_bound(effective_compile_threads, names_chunk.len());
    let (result_tx, result_rx) =
        std::sync::mpsc::sync_channel::<(usize, ModelCompileEntry)>(result_queue_bound);

    std::thread::scope(|scope| {
        let consumer = scope.spawn(move || {
            let mut consume = consume;
            for (idx, entry) in result_rx {
                consume(idx, entry);
            }
        });

        let compile_start = Instant::now();
        match rayon::ThreadPoolBuilder::new()
            .num_threads(effective_compile_threads)
            .build()
        {
            Ok(pool) => pool.install(|| {
                send_compile_chunk_with_model_budgets(
                    source_root,
                    names_chunk,
                    &resources,
                    result_tx,
                );
            }),
            Err(err) => {
                eprintln!(
                    "WARNING: failed to build compile thread pool ({err}); falling back to sequential compile"
                );
                send_compile_chunk_sequential_with_model_budgets(
                    source_root,
                    names_chunk,
                    &resources,
                    result_tx,
                );
            }
        }
        consumer.join().expect("compile stream consumer panicked");
        compile_start.elapsed().as_secs_f64()
    })
}

struct SourceRootCompileQueue<'a> {
    source_root_path: &'a Path,
    names_chunk: &'a [String],
    compile_threads: usize,
    budget_secs: f64,
    run_simulation: bool,
    sim_target_names: Option<&'a HashSet<String>>,
}

struct SourceRootCompileOutput {
    elapsed_seconds: f64,
    scheduler: MslSchedulerTimings,
}

fn stream_source_root_compile_with_model_budgets<F>(
    _source_root: &std::sync::Arc<CompiledSourceRoot>,
    plan: SourceRootCompileQueue<'_>,
    consume: F,
) -> SourceRootCompileOutput
where
    F: FnMut(usize, MslModelResult) + Send,
{
    let SourceRootCompileQueue {
        source_root_path,
        names_chunk,
        compile_threads,
        budget_secs,
        run_simulation,
        sim_target_names,
    } = plan;
    let memory_tokens = compile_memory_token_limiter();
    let effective_compile_threads =
        worker_threads_for_model_count(compile_threads, names_chunk.len());
    let result_queue_bound = streaming_queue_bound(effective_compile_threads, names_chunk.len());
    let (result_tx, result_rx) =
        std::sync::mpsc::sync_channel::<(usize, MslModelResult)>(result_queue_bound);

    std::thread::scope(|scope| {
        let consumer = scope.spawn(move || {
            let mut consume = consume;
            for (idx, entry) in result_rx {
                consume(idx, entry);
            }
        });
        let compile_start = Instant::now();
        let mut worker_handles = Vec::new();
        let next_model = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let requested_worker_count = effective_compile_threads;
        let available_core_count = available_cpu_core_ids().len();
        let worker_count = if available_core_count == 0 {
            requested_worker_count
        } else {
            requested_worker_count.min(available_core_count)
        };
        let core_plan = cpu_core_plan(worker_count);
        let scheduler_stats = SchedulerStatsCollector::new();
        if worker_count < requested_worker_count {
            println!(
                "  Model worker count capped at {worker_count}/{requested_worker_count} available logical CPU cores"
            );
        }
        let startup_barrier = std::sync::Arc::new(std::sync::Barrier::new(worker_count));
        for cpu_core_id in core_plan.iter().copied().take(worker_count) {
            let result_tx = result_tx.clone();
            let next_model = std::sync::Arc::clone(&next_model);
            let memory_tokens = memory_tokens.clone();
            let startup_barrier = std::sync::Arc::clone(&startup_barrier);
            let scheduler_stats = scheduler_stats.clone();
            worker_handles.push(scope.spawn(move || {
                run_model_worker_queue(ModelWorkerQueue {
                    source_root_path,
                    names_chunk,
                    budget_secs,
                    run_simulation,
                    sim_target_names,
                    explicit_sim_target: sim_targets_file_override().is_some(),
                    cpu_core_id,
                    memory_tokens,
                    startup_barrier,
                    scheduler_stats,
                    next_model: next_model.as_ref(),
                    result_tx,
                });
            }));
        }
        drop(result_tx);
        for handle in worker_handles {
            handle.join().expect("model worker queue thread panicked");
        }
        let compile_seconds = compile_start.elapsed().as_secs_f64();
        consumer.join().expect("compile stream consumer panicked");
        let scheduler = scheduler_stats.snapshot(SchedulerTimingInputs {
            selected_model_count: names_chunk.len(),
            requested_worker_threads: compile_threads,
            effective_worker_threads: effective_compile_threads,
            worker_count,
            compile_memory_token_capacity_mb: memory_tokens
                .as_ref()
                .map(|tokens| tokens.capacity()),
            compile_memory_model_cost_mb: memory_tokens.as_ref().map(|_| compile_model_memory_mb()),
            elapsed_seconds: compile_seconds,
        });
        SourceRootCompileOutput {
            elapsed_seconds: compile_seconds,
            scheduler,
        }
    })
}

fn prepare_compiled_source_root(
    parsed_successes: Vec<(String, rumoca_ir_ast::StoredDefinition)>,
    total_mo_files: usize,
    parse_errors: usize,
    timings: &mut MslPhaseTimings,
    frontend_compile_start: Instant,
    core_start: Instant,
) -> Result<PreparedSourceRoot, Box<MslSummary>> {
    println!("Building tolerant source-root index...");
    let session_start = Instant::now();
    let _session_watchdog = StageAbortWatchdog::new("session_build", 300);
    let source_root = match CompiledSourceRoot::from_parsed_batch_tolerant(parsed_successes) {
        Ok(source_root) => std::sync::Arc::new(source_root),
        Err(error) => {
            println!("Failed to build tolerant source-root index: {error}");
            let mut summary = empty_summary(total_mo_files, parse_errors);
            summary.resolve_errors = 1;
            timings.session_build_seconds = session_start.elapsed().as_secs_f64();
            return Err(Box::new(finalize_early_summary(
                summary,
                timings,
                frontend_compile_start,
                core_start,
            )));
        }
    };
    let model_names = source_root.model_names().to_vec();
    let class_type_counts = source_root.class_type_counts().clone();
    timings.session_build_seconds = session_start.elapsed().as_secs_f64();
    println!(
        "Built tolerant source-root index + model discovery in {:.2}s",
        timings.session_build_seconds
    );
    Ok(PreparedSourceRoot {
        source_root,
        model_names,
        class_type_counts,
    })
}

pub(super) fn run_msl_test(run_simulation: bool) -> MslSummary {
    let core_start = Instant::now();
    let mut timings = MslPhaseTimings::default();
    let frontend_compile_start = Instant::now();
    reset_compile_phase_timing_stats();
    reset_flatten_phase_timing_stats();
    prepare_sim_trace_dirs(run_simulation);

    let msl_dir = ensure_msl_downloaded().expect("Failed to download MSL");
    let parsed = parse_msl_batch(&msl_dir, &mut timings);

    if parsed.successes.is_empty() {
        println!("No files parsed successfully");
        return finalize_early_summary(
            empty_summary(parsed.total_mo_files, parsed.parse_errors),
            &mut timings,
            frontend_compile_start,
            core_start,
        );
    }

    let prepared = match prepare_compiled_source_root(
        parsed.successes,
        parsed.total_mo_files,
        parsed.parse_errors,
        &mut timings,
        frontend_compile_start,
        core_start,
    ) {
        Ok(prepared) => prepared,
        Err(summary) => return *summary,
    };

    let PreparedSourceRoot {
        source_root,
        model_names,
        class_type_counts,
    } = prepared;
    let total_models = model_names.len();
    println!("Found {} simulatable models in MSL", total_models);
    log_simulation_run_configuration(run_simulation);

    let selection = select_compile_models_for_run(&source_root, &model_names, run_simulation);
    drop(model_names);
    let compile_count = selection.compile_names.len();
    log_compile_scope(compile_count);

    let setup = begin_chunked_render_sim_setup(&selection.compile_names, run_simulation);
    let context = setup.context(run_simulation);
    let simulation_threads = simulation_threads_for_run(run_simulation);

    let chunked_output = run_chunked_compile_and_render(
        &source_root,
        &msl_dir,
        &selection.compile_names,
        run_simulation,
        &context,
        simulation_threads,
    );

    timings.compile_seconds = chunked_output.compile_only_seconds;
    timings.render_and_write_seconds = chunked_output.render_and_write_seconds;
    timings.compile_batch_size = chunked_output.batch_size;
    timings.compile_chunk_count = chunked_output.chunk_count;
    timings.worker_threads = chunked_output.worker_threads;
    timings.scheduler = chunked_output.scheduler;
    update_phase_timing_totals(&mut timings);
    timings.frontend_compile_seconds =
        timings.parse_seconds + timings.session_build_seconds + timings.compile_seconds;
    print_compile_timing_summary(compile_count, &timings);
    setup.print_summary(run_simulation);
    println!(
        "Rendered + wrote per-model artifacts in {:.2}s",
        timings.render_and_write_seconds
    );
    // Free the typed-tree/session memory before render+simulation.
    drop(source_root);

    let summary_inputs = MslSummaryInputs {
        total_mo_files: parsed.total_mo_files,
        parse_errors: parsed.parse_errors,
        total_models: selection.compile_scope_count,
        class_type_counts,
    };

    finalize_msl_summary_from_results(
        chunked_output.model_results,
        setup.sim_target_models(),
        summary_inputs,
        timings,
        core_start,
    )
}

#[cfg(test)]
mod tests;

/// Test compilation, balance, and simulation of the default MSL target set.
///
/// This is the main regression test. It compiles the default explicit-example
/// target set, checks structural balance, and simulates those models.
#[test]
#[cfg(feature = "msl-full-test")]
pub(super) fn test_msl_all() {
    check_release_mode();
    let summary = run_msl_test(true);

    print_msl_balance_summary(&summary);

    print_simulation_results(&summary);
    print_timing_breakdown(&summary);
    print_failure_details(&summary);
    print_final_stats(&summary);
}
