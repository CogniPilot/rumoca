use super::*;
use std::io::Write;

// =============================================================================
// Simulation worker orchestration (isolated process)
// =============================================================================
//
// This include groups DAE->worker execution policy and timeout/result mapping
// so the core compile/balance/summarize pipeline remains easier to read.

// =============================================================================
// Simulation support (DAE → diffsol)
// =============================================================================

/// Per-model simulation timeout in seconds.
pub(super) const SIM_TIMEOUT_SECS: f64 = 10.0;
/// Additional parent-process grace window so worker JSON parse/write overhead
/// does not cause false parent-side timeouts when solver budget is respected.
pub(super) const SIM_WORKER_TIMEOUT_GRACE_SECS: f64 = 2.0;
/// Default simulation horizon when model annotations do not specify StopTime.
pub(super) const DEFAULT_SIM_END_TIME_SECS: f64 = 1.0;
/// Output sample count per simulation horizon for stateful models.
pub(super) const SIM_OUTPUT_SAMPLES_DEFAULT: usize = 100;
/// Output sample count for no-state (pure algebraic/discrete) models.
///
/// These models still contain time-driven behavior (relations, sampled tables,
/// delays). Coarse sampling can miss transition times and inflate trace
/// deviation against OMC.
pub(super) const SIM_OUTPUT_SAMPLES_NO_STATES: usize = 500;
/// Emit in-flight simulation progress for models that exceed this wall time.
pub(super) const SIM_PROGRESS_LOG_INTERVAL_SECS: u64 = 15;
/// Poll interval while waiting on isolated simulation worker process.
pub(super) const SIM_WORKER_POLL_MILLIS: u64 = 20;
/// Optional threshold for logging slow simulation-preparation work.
pub(super) const SLOW_SIM_PREP_LOG_THRESHOLD_ENV: &str = "RUMOCA_MSL_SLOW_SIM_PREP_LOG_SECS";
/// Optional per-worker address-space cap (MB), configured via env.
///
/// When unset, no per-worker memory cap is applied and worker fan-out defaults
/// to stage-level CPU parallelism (`n_cpus / 2` by default).
pub(super) const SIM_WORKER_MEMORY_MB_ENV: &str = "RUMOCA_MSL_SIM_WORKER_MEMORY_MB";

static SIM_WORKER_EXE: std::sync::OnceLock<Result<PathBuf, String>> = std::sync::OnceLock::new();
static SIM_WORKER_PRLIMIT_AVAILABLE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
static SIM_WORKER_PRLIMIT_WARNED: AtomicBool = AtomicBool::new(false);
static SIM_WORKER_RUN_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(super) fn sim_timeout_override_secs() -> Option<f64> {
    std::env::var("RUMOCA_MSL_SIM_TIMEOUT_OVERRIDE")
        .ok()
        .and_then(|raw| raw.trim().parse::<f64>().ok())
        .filter(|value| value.is_finite() && *value > 0.0)
}

pub(super) fn sim_timeout_secs() -> f64 {
    sim_timeout_override_secs().unwrap_or(SIM_TIMEOUT_SECS)
}

pub(super) fn sim_worker_wall_timeout_secs(solver_timeout_secs: f64, strict_budget: bool) -> f64 {
    if strict_budget {
        solver_timeout_secs
    } else {
        solver_timeout_secs + SIM_WORKER_TIMEOUT_GRACE_SECS
    }
}

pub(super) fn sim_worker_memory_limit_mb() -> Option<usize> {
    let raw = std::env::var(SIM_WORKER_MEMORY_MB_ENV).ok()?;
    let parsed = raw.trim().parse::<usize>().ok()?;
    if parsed == 0 { None } else { Some(parsed) }
}

fn slow_sim_prep_log_threshold_secs_from_override(raw: Option<&str>) -> Option<f64> {
    raw.and_then(|value| value.trim().parse::<f64>().ok())
        .filter(|secs| secs.is_finite() && *secs > 0.0)
}

fn slow_sim_prep_log_threshold_secs() -> Option<f64> {
    slow_sim_prep_log_threshold_secs_from_override(
        std::env::var(SLOW_SIM_PREP_LOG_THRESHOLD_ENV)
            .ok()
            .as_deref(),
    )
}

fn sim_worker_prlimit_available() -> bool {
    *SIM_WORKER_PRLIMIT_AVAILABLE.get_or_init(|| {
        Command::new("prlimit")
            .arg("--help")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .is_ok()
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum SimStatus {
    Ok,
    Nan,
    Timeout,
    SolverFail,
    BalanceFail,
}

impl std::fmt::Display for SimStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimStatus::Ok => write!(f, "sim_ok"),
            SimStatus::Nan => write!(f, "sim_nan"),
            SimStatus::Timeout => write!(f, "sim_timeout"),
            SimStatus::SolverFail => write!(f, "sim_solver_fail"),
            SimStatus::BalanceFail => write!(f, "sim_balance_fail"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslSimModelResult {
    pub(super) name: String,
    pub(super) status: SimStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) error: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) n_states: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) n_algebraics: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_wall_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_trace_file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_trace_error: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct SimWorkerResult {
    status: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    error: Option<String>,
    sim_seconds: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    trace_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    trace_error: Option<String>,
}

pub(super) fn sim_result(
    model_name: &str,
    status: SimStatus,
    error: Option<String>,
    n_states: usize,
    n_algebraics: usize,
) -> MslSimModelResult {
    MslSimModelResult {
        name: model_name.to_string(),
        status,
        error,
        n_states: Some(n_states),
        n_algebraics: Some(n_algebraics),
        sim_seconds: None,
        sim_wall_seconds: None,
        sim_trace_file: None,
        sim_trace_error: None,
    }
}

pub(super) fn parse_sim_status(status: &str) -> Option<SimStatus> {
    match status {
        "sim_ok" => Some(SimStatus::Ok),
        "sim_nan" => Some(SimStatus::Nan),
        "sim_timeout" => Some(SimStatus::Timeout),
        "sim_solver_fail" => Some(SimStatus::SolverFail),
        "sim_balance_fail" => Some(SimStatus::BalanceFail),
        _ => None,
    }
}

pub(super) fn sim_worker_log_enabled() -> bool {
    std::env::var("RUMOCA_MSL_SIM_WORKER_LOG")
        .ok()
        .is_some_and(|raw| {
            matches!(
                raw.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
}

pub(super) fn resolve_sim_worker_exe_inner() -> Result<PathBuf, String> {
    if let Ok(path) = std::env::var("RUMOCA_SIM_WORKER_EXE") {
        let candidate = PathBuf::from(path);
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    for env_key in [
        "CARGO_BIN_EXE_rumoca-sim-worker",
        "CARGO_BIN_EXE_rumoca_sim_worker",
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
        profile_dir.join("rumoca-sim-worker"),
        profile_dir.join("rumoca-sim-worker.exe"),
        profile_dir.join("rumoca_sim_worker"),
        profile_dir.join("rumoca_sim_worker.exe"),
    ];

    if let Ok(entries) = fs::read_dir(deps_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let name = entry.file_name();
            let file_name = name.to_string_lossy();
            if file_name.starts_with("rumoca-sim-worker-")
                || file_name.starts_with("rumoca_sim_worker-")
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
        "failed to locate rumoca-sim-worker binary; expected RUMOCA_SIM_WORKER_EXE or CARGO_BIN_EXE_* env var or binary near {}",
        current_exe.display()
    ))
}

pub(super) fn resolve_sim_worker_exe() -> Result<&'static Path, String> {
    match SIM_WORKER_EXE.get_or_init(resolve_sim_worker_exe_inner) {
        Ok(path) => Ok(path.as_path()),
        Err(err) => Err(err.clone()),
    }
}

pub(super) fn sim_worker_io_paths(
    run_id: usize,
    model_name: &str,
) -> io::Result<(PathBuf, PathBuf)> {
    let sim_worker_dir = get_msl_cache_dir().join("results").join("sim_worker");
    fs::create_dir_all(&sim_worker_dir)?;
    let sim_trace_dir = get_msl_cache_dir()
        .join("results")
        .join("sim_traces")
        .join("rumoca");
    fs::create_dir_all(&sim_trace_dir)?;
    Ok((
        sim_worker_dir.join(format!("sim_{run_id}.json")),
        sim_trace_dir.join(format!("{model_name}.json")),
    ))
}

pub(super) struct SimRunContext<'a> {
    model_name: &'a str,
    n_states: usize,
    n_algebraics: usize,
}

impl SimRunContext<'_> {
    fn solver_fail(&self, error: impl Into<String>) -> MslSimModelResult {
        sim_result(
            self.model_name,
            SimStatus::SolverFail,
            Some(error.into()),
            self.n_states,
            self.n_algebraics,
        )
    }

    fn timeout(
        &self,
        elapsed_secs: f64,
        solver_timeout_secs: f64,
        process_timeout_secs: f64,
    ) -> MslSimModelResult {
        let mut timed_out = sim_result(
            self.model_name,
            SimStatus::Timeout,
            Some(format!(
                "worker process timeout after {:.3}s (solver limit {:.3}s, process limit {:.3}s)",
                elapsed_secs, solver_timeout_secs, process_timeout_secs,
            )),
            self.n_states,
            self.n_algebraics,
        );
        timed_out.sim_seconds = Some(elapsed_secs);
        timed_out.sim_wall_seconds = Some(elapsed_secs);
        timed_out
    }

    fn finish(
        &self,
        status: SimStatus,
        error: Option<String>,
        sim_seconds: f64,
        sim_wall_seconds: f64,
        sim_trace_file: Option<String>,
        sim_trace_error: Option<String>,
    ) -> MslSimModelResult {
        MslSimModelResult {
            name: self.model_name.to_string(),
            status,
            error,
            n_states: Some(self.n_states),
            n_algebraics: Some(self.n_algebraics),
            sim_seconds: Some(sim_seconds),
            sim_wall_seconds: Some(sim_wall_seconds),
            sim_trace_file,
            sim_trace_error,
        }
    }
}

pub(super) struct SimWorkerArtifacts {
    output_path: PathBuf,
    trace_path: PathBuf,
    trace_relative_path: String,
}

impl SimWorkerArtifacts {
    fn create(model_name: &str) -> Result<Self, String> {
        let run_id = SIM_WORKER_RUN_COUNTER.fetch_add(1, Ordering::Relaxed);
        let (output_path, trace_path) = sim_worker_io_paths(run_id, model_name)
            .map_err(|e| format!("failed to create sim worker artifact paths: {e}"))?;
        if trace_path.exists() {
            let _ = fs::remove_file(&trace_path);
        }
        let trace_relative_path = Path::new("sim_traces")
            .join("rumoca")
            .join(format!("{model_name}.json"))
            .to_string_lossy()
            .to_string();
        Ok(Self {
            output_path,
            trace_path,
            trace_relative_path,
        })
    }
}

impl Drop for SimWorkerArtifacts {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.output_path);
    }
}

fn serialize_worker_input(dae: &Dae) -> Result<Vec<u8>, String> {
    bincode::serialize(dae).map_err(|e| format!("failed to serialize worker DAE input: {e}"))
}

#[derive(Debug, Clone)]
pub(super) struct SimExecutionSettings {
    pub(super) t_start: f64,
    pub(super) t_end: f64,
    pub(super) dt: Option<f64>,
    pub(super) rtol: Option<f64>,
    pub(super) atol: Option<f64>,
    pub(super) solver: String,
    pub(super) timeout_seconds: Option<f64>,
}

pub(super) struct PreparedSimulationRun {
    model_name: String,
    n_states: usize,
    n_algebraics: usize,
    output_samples: usize,
    settings: SimExecutionSettings,
    dae_payload: Vec<u8>,
    artifacts: SimWorkerArtifacts,
}

pub(super) fn spawn_sim_worker_process(
    worker_exe: &Path,
    artifacts: &SimWorkerArtifacts,
    ctx: &SimRunContext<'_>,
    settings: &SimExecutionSettings,
    output_samples: usize,
    solver_timeout_secs: f64,
) -> Result<std::process::Child, String> {
    let mut cmd = match sim_worker_memory_limit_mb() {
        Some(memory_mb) if sim_worker_prlimit_available() => {
            let mut wrapped = Command::new("prlimit");
            let bytes = memory_mb.saturating_mul(1024).saturating_mul(1024);
            wrapped
                .arg(format!("--as={bytes}"))
                .arg("--")
                .arg(worker_exe);
            wrapped
        }
        Some(_) => {
            if !SIM_WORKER_PRLIMIT_WARNED.swap(true, Ordering::Relaxed) {
                eprintln!(
                    "WARNING: sim worker memory cap requested but `prlimit` is unavailable; running uncapped"
                );
            }
            Command::new(worker_exe)
        }
        None => Command::new(worker_exe),
    };
    cmd.arg("--dae-stdin")
        .arg("--result-json")
        .arg(&artifacts.output_path)
        .arg("--model-name")
        .arg(ctx.model_name)
        .arg("--t-start")
        .arg(settings.t_start.to_string())
        .arg("--t-end")
        .arg(settings.t_end.to_string())
        .arg("--output-samples")
        .arg(output_samples.to_string())
        .arg("--solver")
        .arg(&settings.solver)
        .arg("--timeout-seconds")
        .arg(solver_timeout_secs.to_string())
        .arg("--trace-json")
        .arg(&artifacts.trace_path);
    if let Some(dt) = settings.dt {
        cmd.arg("--dt").arg(dt.to_string());
    }
    if let Some(rtol) = settings.rtol {
        cmd.arg("--rtol").arg(rtol.to_string());
    }
    if let Some(atol) = settings.atol {
        cmd.arg("--atol").arg(atol.to_string());
    }

    if sim_worker_log_enabled() {
        cmd.stdout(std::process::Stdio::inherit())
            .stderr(std::process::Stdio::inherit());
    } else {
        cmd.stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null());
    }
    cmd.stdin(std::process::Stdio::piped());

    cmd.spawn()
        .map_err(|e| format!("failed to spawn sim worker: {e}"))
}

fn write_sim_worker_stdin(
    child: &mut std::process::Child,
    payload: &[u8],
    model_name: &str,
) -> Result<(), String> {
    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| format!("sim worker stdin unavailable for {model_name}"))?;
    stdin
        .write_all(payload)
        .map_err(|e| format!("failed to stream worker DAE input for {model_name}: {e}"))?;
    stdin
        .flush()
        .map_err(|e| format!("failed to flush worker DAE input for {model_name}: {e}"))
}

pub(super) enum WorkerWaitOutcome {
    Exited {
        status: std::process::ExitStatus,
        elapsed_secs: f64,
    },
    TimedOut {
        elapsed_secs: f64,
    },
}

pub(super) fn wait_for_sim_worker(
    child: &mut std::process::Child,
    ctx: &SimRunContext<'_>,
    process_timeout_secs: f64,
) -> Result<WorkerWaitOutcome, String> {
    let sim_start = Instant::now();
    let mut last_progress_log = sim_start;
    let deadline = Duration::from_secs_f64(process_timeout_secs);

    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                return Ok(WorkerWaitOutcome::Exited {
                    status,
                    elapsed_secs: sim_start.elapsed().as_secs_f64(),
                });
            }
            Ok(None) => {}
            Err(err) => {
                return Err(format!("failed while waiting for sim worker: {err}"));
            }
        }

        let elapsed = sim_start.elapsed();
        if elapsed >= deadline {
            return Ok(WorkerWaitOutcome::TimedOut {
                elapsed_secs: elapsed.as_secs_f64(),
            });
        }

        if last_progress_log.elapsed() >= Duration::from_secs(SIM_PROGRESS_LOG_INTERVAL_SECS) {
            eprintln!(
                "    sim in-flight: {} elapsed {:.1}s",
                ctx.model_name,
                elapsed.as_secs_f64()
            );
            last_progress_log = Instant::now();
        }

        std::thread::sleep(Duration::from_millis(SIM_WORKER_POLL_MILLIS));
    }
}

pub(super) fn read_sim_worker_result(
    artifacts: &SimWorkerArtifacts,
) -> Result<SimWorkerResult, String> {
    File::open(&artifacts.output_path)
        .map_err(|e| format!("failed to open worker result: {e}"))
        .and_then(|file| {
            serde_json::from_reader::<_, SimWorkerResult>(std::io::BufReader::new(file))
                .map_err(|e| format!("failed to parse worker result JSON: {e}"))
        })
}

pub(super) fn run_simulation_worker(
    dae: &Dae,
    model_name: &str,
    settings: &SimExecutionSettings,
    output_samples: usize,
    n_states: usize,
    n_algebraics: usize,
) -> MslSimModelResult {
    let prepared = match prepare_simulation_run(
        dae,
        model_name,
        settings.clone(),
        output_samples,
        n_states,
        n_algebraics,
    ) {
        Ok(run) => run,
        Err(result) => return *result,
    };
    run_prepared_simulation(prepared)
}

pub(super) fn prepare_simulation_run(
    dae: &Dae,
    model_name: &str,
    settings: SimExecutionSettings,
    output_samples: usize,
    n_states: usize,
    n_algebraics: usize,
) -> Result<PreparedSimulationRun, Box<MslSimModelResult>> {
    let ctx = SimRunContext {
        model_name,
        n_states,
        n_algebraics,
    };

    let prep_started = Instant::now();
    let artifact_create_started = Instant::now();
    let artifacts =
        SimWorkerArtifacts::create(model_name).map_err(|err| Box::new(ctx.solver_fail(err)))?;
    let artifact_create_secs = artifact_create_started.elapsed().as_secs_f64();
    let serialize_started = Instant::now();
    let dae_payload = match serialize_worker_input(dae) {
        Ok(payload) => payload,
        Err(err) => return Err(Box::new(ctx.solver_fail(err))),
    };
    let serialize_secs = serialize_started.elapsed().as_secs_f64();
    let input_bytes = dae_payload.len();
    let prep_secs = prep_started.elapsed().as_secs_f64();
    if slow_sim_prep_log_threshold_secs().is_some_and(|threshold| prep_secs >= threshold) {
        eprintln!(
            "    slow sim prep: model={model_name} total={prep_secs:.2}s create={artifact_create_secs:.2}s serialize_input={serialize_secs:.2}s input_bytes={input_bytes} states={} algebraics={} f_x={} f_z={} f_m={} f_c={} relation={} initial_eqs={}",
            dae.states.len(),
            dae.algebraics.len(),
            dae.f_x.len(),
            dae.f_z.len(),
            dae.f_m.len(),
            dae.f_c.len(),
            dae.relation.len(),
            dae.initial_equations.len(),
        );
    }

    Ok(PreparedSimulationRun {
        model_name: model_name.to_string(),
        n_states,
        n_algebraics,
        output_samples,
        settings,
        dae_payload,
        artifacts,
    })
}

pub(super) fn run_prepared_simulation(run: PreparedSimulationRun) -> MslSimModelResult {
    let PreparedSimulationRun {
        model_name,
        n_states,
        n_algebraics,
        output_samples,
        settings,
        dae_payload,
        artifacts,
    } = run;
    let ctx = SimRunContext {
        model_name: &model_name,
        n_states,
        n_algebraics,
    };

    let worker_exe = match resolve_sim_worker_exe() {
        Ok(path) => path,
        Err(err) => return ctx.solver_fail(err),
    };

    let solver_timeout_secs = settings
        .timeout_seconds
        .filter(|secs| secs.is_finite() && *secs > 0.0)
        .unwrap_or_else(sim_timeout_secs);
    let strict_budget = settings
        .timeout_seconds
        .is_some_and(|secs| secs.is_finite() && secs > 0.0);
    let process_timeout_secs = sim_worker_wall_timeout_secs(solver_timeout_secs, strict_budget);

    let mut child = match spawn_sim_worker_process(
        worker_exe,
        &artifacts,
        &ctx,
        &settings,
        output_samples,
        solver_timeout_secs,
    ) {
        Ok(child) => child,
        Err(err) => return ctx.solver_fail(err),
    };
    if let Err(err) = write_sim_worker_stdin(&mut child, &dae_payload, &model_name) {
        let _ = child.kill();
        let _ = child.wait();
        return ctx.solver_fail(err);
    }

    let wait_outcome = match wait_for_sim_worker(&mut child, &ctx, process_timeout_secs) {
        Ok(outcome) => outcome,
        Err(err) => return ctx.solver_fail(err),
    };

    let elapsed_secs = match wait_outcome {
        WorkerWaitOutcome::TimedOut { elapsed_secs } => {
            let _ = child.kill();
            let _ = child.wait();
            return ctx.timeout(elapsed_secs, solver_timeout_secs, process_timeout_secs);
        }
        WorkerWaitOutcome::Exited {
            status,
            elapsed_secs,
        } => {
            if !status.success() {
                return ctx.solver_fail(format!("sim worker exited unsuccessfully: {status}"));
            }
            elapsed_secs
        }
    };

    let worker_result = match read_sim_worker_result(&artifacts) {
        Ok(result) => result,
        Err(err) => return ctx.solver_fail(err),
    };

    let status = match parse_sim_status(&worker_result.status) {
        Some(status) => status,
        None => {
            return ctx.solver_fail(format!(
                "worker returned unknown status '{}' for {}",
                worker_result.status, ctx.model_name
            ));
        }
    };

    let sim_seconds = if worker_result.sim_seconds.is_finite() && worker_result.sim_seconds >= 0.0 {
        worker_result.sim_seconds
    } else {
        elapsed_secs
    };
    let sim_wall_seconds = if elapsed_secs.is_finite() && elapsed_secs >= 0.0 {
        elapsed_secs
    } else {
        sim_seconds
    };
    let sim_trace_file = if matches!(status, SimStatus::Ok) && artifacts.trace_path.is_file() {
        Some(artifacts.trace_relative_path.clone())
    } else {
        None
    };
    ctx.finish(
        status,
        worker_result.error,
        sim_seconds,
        sim_wall_seconds,
        sim_trace_file,
        worker_result.trace_error,
    )
}

pub(super) fn is_trivial_static_model(dae: &Dae) -> bool {
    let discrete_real_scalars: usize = dae.discrete_reals.values().map(|v| v.size()).sum();
    let discrete_valued_scalars: usize = dae.discrete_valued.values().map(|v| v.size()).sum();
    discrete_real_scalars == 0
        && discrete_valued_scalars == 0
        && dae.f_x.is_empty()
        && dae.f_z.is_empty()
        && dae.f_m.is_empty()
        && dae.f_c.is_empty()
        && dae.relation.is_empty()
        && dae.initial_equations.is_empty()
}

pub(super) fn try_simulate_dae_with_settings(
    dae: &Dae,
    model_name: &str,
    settings: &SimExecutionSettings,
) -> MslSimModelResult {
    let n_states = dae.states.len();
    let n_algebraics = dae.algebraics.len();
    let n_state_scalars: usize = dae.states.values().map(|v| v.size()).sum();

    let total_unknowns: usize = dae.states.values().map(|v| v.size()).sum::<usize>()
        + dae.algebraics.values().map(|v| v.size()).sum::<usize>()
        + dae.outputs.values().map(|v| v.size()).sum::<usize>();
    if total_unknowns == 0 && is_trivial_static_model(dae) {
        return MslSimModelResult {
            name: model_name.to_string(),
            status: SimStatus::Ok,
            error: None,
            n_states: Some(n_states),
            n_algebraics: Some(n_algebraics),
            sim_seconds: Some(0.0),
            sim_wall_seconds: Some(0.0),
            sim_trace_file: None,
            sim_trace_error: None,
        };
    }

    let output_samples = output_samples_for_model(n_state_scalars);
    run_simulation_worker(
        dae,
        model_name,
        settings,
        output_samples,
        n_states,
        n_algebraics,
    )
}

pub(super) fn output_samples_for_model(n_state_scalars: usize) -> usize {
    if n_state_scalars == 0 {
        SIM_OUTPUT_SAMPLES_NO_STATES
    } else {
        SIM_OUTPUT_SAMPLES_DEFAULT
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_session::compile::{Session, SessionConfig};

    #[test]
    fn slow_sim_prep_log_threshold_parses_positive_numbers_only() {
        assert_eq!(slow_sim_prep_log_threshold_secs_from_override(None), None);
        assert_eq!(
            slow_sim_prep_log_threshold_secs_from_override(Some("")),
            None
        );
        assert_eq!(
            slow_sim_prep_log_threshold_secs_from_override(Some("0")),
            None
        );
        assert_eq!(
            slow_sim_prep_log_threshold_secs_from_override(Some("-1")),
            None
        );
        assert_eq!(
            slow_sim_prep_log_threshold_secs_from_override(Some("7.5")),
            Some(7.5)
        );
    }

    #[test]
    fn serialize_worker_input_round_trips_binary_dae_payload() {
        let source =
            "model RoundTrip\n  Real x(start = 1);\nequation\n  der(x) = -x;\nend RoundTrip;\n";
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("round_trip.mo", source)
            .expect("add source file");
        let compiled = session
            .compile_model("RoundTrip")
            .expect("compile round-trip model");

        let payload = serialize_worker_input(&compiled.dae).expect("serialize worker input");
        let round_tripped: Dae =
            bincode::deserialize_from(std::io::Cursor::new(payload)).expect("decode worker input");

        let expected = serde_json::to_value(&compiled.dae).expect("serialize expected DAE");
        let actual = serde_json::to_value(&round_tripped).expect("serialize round-tripped DAE");
        assert_eq!(actual, expected);
    }

    #[test]
    fn sim_worker_io_paths_returns_output_and_trace_paths() {
        let (output_path, trace_path) = sim_worker_io_paths(7, "Demo.Model").expect("worker paths");

        assert!(output_path.ends_with("sim_worker/sim_7.json"));
        assert!(trace_path.ends_with("sim_traces/rumoca/Demo.Model.json"));
    }

    #[test]
    fn run_prepared_simulation_streams_binary_dae_over_stdin() {
        let source =
            "model WorkerPipe\n  Real x(start = 1);\nequation\n  der(x) = -x;\nend WorkerPipe;\n";
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("worker_pipe.mo", source)
            .expect("add source file");
        let compiled = session
            .compile_model("WorkerPipe")
            .expect("compile worker pipe model");

        let settings = SimExecutionSettings {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.01),
            rtol: Some(1e-6),
            atol: Some(1e-6),
            solver: "auto".to_string(),
            timeout_seconds: Some(5.0),
        };
        let prepared = prepare_simulation_run(
            &compiled.dae,
            "WorkerPipe",
            settings,
            10,
            compiled.dae.states.len(),
            compiled.dae.algebraics.len(),
        )
        .expect("prepare simulation");
        let result = run_prepared_simulation(prepared);

        assert!(matches!(result.status, SimStatus::Ok), "{result:?}");
        assert_eq!(result.error, None);
        assert!(result.sim_seconds.is_some());
        assert!(result.sim_wall_seconds.is_some());
    }
}
