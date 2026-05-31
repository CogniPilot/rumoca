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

/// Per-model simulation timeout in seconds (shared with the OMC reference run).
pub(super) const SIM_TIMEOUT_SECS: f64 = rumoca_worker::MSL_SIM_TIMEOUT_SECS;
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
/// Per-model DAE-to-Solve-IR timeout in seconds.
pub(super) const IR_SOLVE_TIMEOUT_SECS: f64 = 10.0;
/// `prlimit --as` caps virtual address space, not resident memory.
///
/// The Rust sim worker needs substantial mmap/headroom above its practical RSS,
/// otherwise modest models abort inside the allocator before the solver timeout
/// path can report a normal `sim_timeout`.
const SIM_WORKER_ADDRESS_SPACE_HEADROOM_FACTOR: usize = 2;

static SIM_WORKER_EXE: std::sync::OnceLock<Result<PathBuf, String>> = std::sync::OnceLock::new();
static SIM_WORKER_PRLIMIT_AVAILABLE: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
static SIM_WORKER_PRLIMIT_WARNED: AtomicBool = AtomicBool::new(false);
static SIM_WORKER_RUN_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(super) fn sim_timeout_override_secs() -> Option<f64> {
    None
}

pub(super) fn sim_timeout_secs() -> f64 {
    SIM_TIMEOUT_SECS
}

pub(super) fn ir_solve_timeout_override_secs() -> Option<f64> {
    None
}

pub(super) fn ir_solve_timeout_secs() -> f64 {
    ir_solve_timeout_override_secs().unwrap_or(IR_SOLVE_TIMEOUT_SECS)
}

pub(super) fn sim_worker_wall_timeout_secs(
    ir_solve_timeout_secs: f64,
    solver_timeout_secs: f64,
) -> f64 {
    ir_solve_timeout_secs + solver_timeout_secs + SIM_WORKER_TIMEOUT_GRACE_SECS
}

pub(super) fn sim_worker_memory_limit_mb() -> Option<usize> {
    // No per-worker address-space cap by default; the parity config can set one.
    parity_config().sim_worker_memory_mb
}

fn sim_worker_address_space_limit_bytes(memory_mb: usize) -> usize {
    memory_mb
        .saturating_mul(SIM_WORKER_ADDRESS_SPACE_HEADROOM_FACTOR)
        .saturating_mul(1024)
        .saturating_mul(1024)
}

fn slow_sim_prep_log_threshold_secs_from_override(raw: Option<&str>) -> Option<f64> {
    raw.and_then(|value| value.trim().parse::<f64>().ok())
        .filter(|secs| secs.is_finite() && *secs > 0.0)
}

fn slow_sim_prep_log_threshold_secs() -> Option<f64> {
    None
}

fn sim_worker_perf_record_enabled() -> bool {
    false
}

fn sim_worker_perf_keep_threshold_secs() -> f64 {
    5.0
}

fn sim_worker_perf_frequency() -> usize {
    99
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
    pub(super) ic_status: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ic_error: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ic_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) n_states: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) n_algebraics: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_build_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ir_solve_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ir_solve_structural_dae_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ir_solve_lower_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_backend_build_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_run_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_wall_seconds: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_trace_file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_perf_profile_file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) sim_trace_error: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ir_dae_file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ir_solve_file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) ir_solve_error: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct SimWorkerResult {
    status: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_status: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    ic_seconds: Option<f64>,
    sim_seconds: f64,
    #[serde(default)]
    sim_build_seconds: f64,
    #[serde(default)]
    ir_solve_seconds: f64,
    #[serde(default)]
    ir_solve_structural_dae_seconds: f64,
    #[serde(default)]
    ir_solve_lower_seconds: f64,
    #[serde(default)]
    sim_backend_build_seconds: f64,
    #[serde(default)]
    sim_run_seconds: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    trace_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    trace_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    solve_ir_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    solve_ir_error: Option<String>,
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
        ic_status: None,
        ic_error: None,
        ic_seconds: None,
        n_states: Some(n_states),
        n_algebraics: Some(n_algebraics),
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
    false
}

pub(super) fn resolve_sim_worker_exe_inner() -> Result<PathBuf, String> {
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
        "failed to locate rumoca-sim-worker binary; expected a CARGO_BIN_EXE_* env var or a binary near {}",
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
    let sim_worker_dir = msl_results_dir().join("sim_worker");
    fs::create_dir_all(&sim_worker_dir)?;
    let sim_trace_dir = msl_results_dir().join("sim_traces").join("rumoca");
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

#[derive(Debug, Clone, Copy)]
struct SimTimingBreakdown {
    sim_seconds: f64,
    sim_build_seconds: f64,
    ir_solve_seconds: f64,
    ir_solve_structural_dae_seconds: f64,
    ir_solve_lower_seconds: f64,
    sim_backend_build_seconds: f64,
    sim_run_seconds: f64,
    sim_wall_seconds: f64,
}

impl SimTimingBreakdown {
    fn timeout(elapsed_secs: f64) -> Self {
        Self {
            sim_seconds: elapsed_secs,
            sim_build_seconds: 0.0,
            ir_solve_seconds: 0.0,
            ir_solve_structural_dae_seconds: 0.0,
            ir_solve_lower_seconds: 0.0,
            sim_backend_build_seconds: 0.0,
            sim_run_seconds: elapsed_secs,
            sim_wall_seconds: elapsed_secs,
        }
    }

    fn from_worker_result(worker_result: &SimWorkerResult, elapsed_secs: f64) -> Self {
        let sim_seconds =
            if worker_result.sim_seconds.is_finite() && worker_result.sim_seconds >= 0.0 {
                worker_result.sim_seconds
            } else {
                elapsed_secs
            };
        let sim_build_seconds = if worker_result.sim_build_seconds.is_finite()
            && worker_result.sim_build_seconds >= 0.0
        {
            worker_result.sim_build_seconds
        } else {
            0.0
        };
        let ir_solve_seconds = finite_nonnegative_or_zero(worker_result.ir_solve_seconds);
        let ir_solve_structural_dae_seconds =
            finite_nonnegative_or_zero(worker_result.ir_solve_structural_dae_seconds);
        let ir_solve_lower_seconds =
            finite_nonnegative_or_zero(worker_result.ir_solve_lower_seconds);
        let sim_backend_build_seconds =
            finite_nonnegative_or_zero(worker_result.sim_backend_build_seconds);
        let sim_run_seconds =
            if worker_result.sim_run_seconds.is_finite() && worker_result.sim_run_seconds >= 0.0 {
                worker_result.sim_run_seconds
            } else {
                sim_seconds
            };
        let sim_wall_seconds = if elapsed_secs.is_finite() && elapsed_secs >= 0.0 {
            elapsed_secs
        } else {
            sim_seconds
        };
        Self {
            sim_seconds,
            sim_build_seconds,
            ir_solve_seconds,
            ir_solve_structural_dae_seconds,
            ir_solve_lower_seconds,
            sim_backend_build_seconds,
            sim_run_seconds,
            sim_wall_seconds,
        }
    }
}

fn finite_nonnegative_or_zero(value: f64) -> f64 {
    if value.is_finite() && value >= 0.0 {
        value
    } else {
        0.0
    }
}

struct SimRunOutcome {
    status: SimStatus,
    error: Option<String>,
    ic_status: Option<String>,
    ic_error: Option<String>,
    ic_seconds: Option<f64>,
    timing: SimTimingBreakdown,
    sim_trace_file: Option<String>,
    sim_perf_profile_file: Option<String>,
    sim_trace_error: Option<String>,
    ir_dae_file: Option<String>,
    ir_solve_file: Option<String>,
    ir_solve_error: Option<String>,
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
        self.finish(SimRunOutcome {
            status: SimStatus::Timeout,
            error: Some(format!(
                "worker process timeout after {:.3}s (solver limit {:.3}s, process limit {:.3}s)",
                elapsed_secs, solver_timeout_secs, process_timeout_secs,
            )),
            ic_status: None,
            ic_error: None,
            ic_seconds: None,
            timing: SimTimingBreakdown::timeout(elapsed_secs),
            sim_trace_file: None,
            sim_perf_profile_file: None,
            sim_trace_error: None,
            ir_dae_file: None,
            ir_solve_file: None,
            ir_solve_error: None,
        })
    }

    fn finish(&self, outcome: SimRunOutcome) -> MslSimModelResult {
        MslSimModelResult {
            name: self.model_name.to_string(),
            status: outcome.status,
            error: outcome.error,
            ic_status: outcome.ic_status,
            ic_error: outcome.ic_error,
            ic_seconds: outcome.ic_seconds,
            n_states: Some(self.n_states),
            n_algebraics: Some(self.n_algebraics),
            sim_seconds: Some(outcome.timing.sim_seconds),
            sim_build_seconds: Some(outcome.timing.sim_build_seconds),
            ir_solve_seconds: Some(outcome.timing.ir_solve_seconds),
            ir_solve_structural_dae_seconds: Some(outcome.timing.ir_solve_structural_dae_seconds),
            ir_solve_lower_seconds: Some(outcome.timing.ir_solve_lower_seconds),
            sim_backend_build_seconds: Some(outcome.timing.sim_backend_build_seconds),
            sim_run_seconds: Some(outcome.timing.sim_run_seconds),
            sim_wall_seconds: Some(outcome.timing.sim_wall_seconds),
            sim_trace_file: outcome.sim_trace_file,
            sim_perf_profile_file: outcome.sim_perf_profile_file,
            sim_trace_error: outcome.sim_trace_error,
            ir_dae_file: outcome.ir_dae_file,
            ir_solve_file: outcome.ir_solve_file,
            ir_solve_error: outcome.ir_solve_error,
        }
    }
}

pub(super) fn artifact_file_ready(path: &Path) -> bool {
    path.metadata().is_ok_and(|metadata| metadata.len() > 0)
}

fn attach_worker_artifacts(
    mut result: MslSimModelResult,
    artifacts: &SimWorkerArtifacts,
) -> MslSimModelResult {
    if result.ir_dae_file.is_none() && artifact_file_ready(&artifacts.dae_path) {
        result.ir_dae_file = Some(artifacts.dae_relative_path.clone());
    }
    if result.ir_solve_file.is_none() && artifact_file_ready(&artifacts.solve_ir_path) {
        result.ir_solve_file = Some(artifacts.solve_ir_relative_path.clone());
    }
    if result.sim_perf_profile_file.is_none()
        && artifacts
            .perf_profile_path
            .as_ref()
            .is_some_and(|path| artifact_file_ready(path))
    {
        result.sim_perf_profile_file = artifacts.perf_profile_relative_path.clone();
    }
    result
}

fn retain_sim_worker_perf_profile(
    artifacts: &SimWorkerArtifacts,
    elapsed_secs: f64,
    status: &SimStatus,
) -> Option<String> {
    let profile_path = artifacts.perf_profile_path.as_ref()?;
    if !artifact_file_ready(profile_path) {
        return None;
    }

    let should_keep =
        elapsed_secs >= sim_worker_perf_keep_threshold_secs() || !matches!(status, SimStatus::Ok);
    if should_keep {
        artifacts.perf_profile_relative_path.clone()
    } else {
        let _ = fs::remove_file(profile_path);
        None
    }
}

pub(super) struct SimWorkerArtifacts {
    output_path: PathBuf,
    dae_path: PathBuf,
    dae_relative_path: String,
    solve_ir_path: PathBuf,
    solve_ir_relative_path: String,
    trace_path: PathBuf,
    trace_relative_path: String,
    perf_profile_path: Option<PathBuf>,
    perf_profile_relative_path: Option<String>,
}

impl SimWorkerArtifacts {
    fn create(model_name: &str) -> Result<Self, String> {
        let run_id = SIM_WORKER_RUN_COUNTER.fetch_add(1, Ordering::Relaxed);
        let (output_path, trace_path) = sim_worker_io_paths(run_id, model_name)
            .map_err(|e| format!("failed to create sim worker artifact paths: {e}"))?;
        let dae_dir = msl_results_dir().join("ir_dae");
        fs::create_dir_all(&dae_dir)
            .map_err(|e| format!("failed to create DAE artifact directory: {e}"))?;
        let solve_ir_dir = msl_results_dir().join("ir_solve");
        fs::create_dir_all(&solve_ir_dir)
            .map_err(|e| format!("failed to create Solve IR artifact directory: {e}"))?;
        let dae_path = dae_dir.join(format!("{model_name}.json"));
        let solve_ir_path = solve_ir_dir.join(format!("{model_name}.json"));
        if dae_path.exists() {
            let _ = fs::remove_file(&dae_path);
        }
        if solve_ir_path.exists() {
            let _ = fs::remove_file(&solve_ir_path);
        }
        if trace_path.exists() {
            let _ = fs::remove_file(&trace_path);
        }
        let (perf_profile_path, perf_profile_relative_path) = if sim_worker_perf_record_enabled() {
            let perf_dir = msl_results_dir().join("perf").join("sim");
            fs::create_dir_all(&perf_dir)
                .map_err(|e| format!("failed to create sim perf profile directory: {e}"))?;
            let path = perf_dir.join(format!("{model_name}.perf.data"));
            if path.exists() {
                let _ = fs::remove_file(&path);
            }
            let relative = Path::new("perf")
                .join("sim")
                .join(format!("{model_name}.perf.data"))
                .to_string_lossy()
                .to_string();
            (Some(path), Some(relative))
        } else {
            (None, None)
        };
        let dae_relative_path = Path::new("ir_dae")
            .join(format!("{model_name}.json"))
            .to_string_lossy()
            .to_string();
        let solve_ir_relative_path = Path::new("ir_solve")
            .join(format!("{model_name}.json"))
            .to_string_lossy()
            .to_string();
        let trace_relative_path = Path::new("sim_traces")
            .join("rumoca")
            .join(format!("{model_name}.json"))
            .to_string_lossy()
            .to_string();
        Ok(Self {
            output_path,
            dae_path,
            dae_relative_path,
            solve_ir_path,
            solve_ir_relative_path,
            trace_path,
            trace_relative_path,
            perf_profile_path,
            perf_profile_relative_path,
        })
    }
}

impl Drop for SimWorkerArtifacts {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.output_path);
    }
}

fn serialize_worker_input(dae: &Dae) -> Result<Vec<u8>, String> {
    serde_json::to_vec(dae).map_err(|e| format!("failed to serialize worker DAE JSON input: {e}"))
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

pub(super) fn gate_simulation_settings_by_compile_budget(
    settings: SimExecutionSettings,
    remaining_budget_secs: Option<f64>,
) -> Result<SimExecutionSettings, f64> {
    match remaining_budget_secs {
        Some(budget_secs) if budget_secs <= 0.0 => Err(budget_secs),
        _ => Ok(settings),
    }
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
    ir_solve_timeout_secs: f64,
    solver_timeout_secs: f64,
) -> Result<std::process::Child, String> {
    let memory_limit_mb = sim_worker_memory_limit_mb();
    let use_prlimit = match memory_limit_mb {
        Some(_) if sim_worker_prlimit_available() => true,
        Some(_) => {
            if !SIM_WORKER_PRLIMIT_WARNED.swap(true, Ordering::Relaxed) {
                eprintln!(
                    "WARNING: sim worker memory cap requested but `prlimit` is unavailable; running uncapped"
                );
            }
            false
        }
        None => false,
    };
    let mut cmd = if use_prlimit {
        let mut wrapped = Command::new("prlimit");
        let bytes = sim_worker_address_space_limit_bytes(
            memory_limit_mb.expect("memory limit exists when prlimit is enabled"),
        );
        wrapped
            .arg(format!("--as={bytes}"))
            .arg("--")
            .arg(worker_exe);
        wrapped
    } else {
        Command::new(worker_exe)
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
        .arg("--solve-timeout-seconds")
        .arg(ir_solve_timeout_secs.to_string())
        .arg("--timeout-seconds")
        .arg(solver_timeout_secs.to_string())
        .arg("--trace-json")
        .arg(&artifacts.trace_path)
        .arg("--solve-ir-json")
        .arg(&artifacts.solve_ir_path);
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

fn start_sim_worker_perf_session(
    worker_pid: u32,
    artifacts: &SimWorkerArtifacts,
) -> Option<PerfSession> {
    let profile_path = artifacts.perf_profile_path.as_ref()?;
    start_perf_record_session(
        PerfRecordTarget::Process(worker_pid),
        profile_path,
        sim_worker_perf_frequency(),
        "sim worker",
    )
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
    if let Err(err) = fs::write(&artifacts.dae_path, &dae_payload) {
        return Err(Box::new(ctx.solver_fail(format!(
            "failed to write DAE IR artifact '{}': {err}",
            artifacts.dae_path.display()
        ))));
    }
    let serialize_secs = serialize_started.elapsed().as_secs_f64();
    let input_bytes = dae_payload.len();
    let prep_secs = prep_started.elapsed().as_secs_f64();
    if slow_sim_prep_log_threshold_secs().is_some_and(|threshold| prep_secs >= threshold) {
        eprintln!(
            "    slow sim prep: model={model_name} total={prep_secs:.2}s create={artifact_create_secs:.2}s serialize_input={serialize_secs:.2}s input_bytes={input_bytes} states={} algebraics={} f_x={} f_z={} f_m={} f_c={} relation={} initial_eqs={}",
            dae.variables.states.len(),
            dae.variables.algebraics.len(),
            dae.continuous.equations.len(),
            dae.discrete.real_updates.len(),
            dae.discrete.valued_updates.len(),
            dae.conditions.equations.len(),
            dae.conditions.relations.len(),
            dae.initialization.equations.len(),
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

fn simulation_timeouts(settings: &SimExecutionSettings) -> (f64, f64, f64) {
    let ir_solve_timeout_secs = ir_solve_timeout_secs();
    let solver_timeout_secs = settings
        .timeout_seconds
        .filter(|secs| secs.is_finite() && *secs > 0.0)
        .unwrap_or_else(sim_timeout_secs);
    let process_timeout_secs =
        sim_worker_wall_timeout_secs(ir_solve_timeout_secs, solver_timeout_secs);
    (
        ir_solve_timeout_secs,
        solver_timeout_secs,
        process_timeout_secs,
    )
}

fn spawn_prepared_sim_worker(
    ctx: &SimRunContext<'_>,
    worker_exe: &Path,
    run: &PreparedSimulationRun,
) -> Result<(std::process::Child, Option<PerfSession>, f64, f64), Box<MslSimModelResult>> {
    let (ir_solve_timeout_secs, solver_timeout_secs, process_timeout_secs) =
        simulation_timeouts(&run.settings);
    let mut child = match spawn_sim_worker_process(
        worker_exe,
        &run.artifacts,
        ctx,
        &run.settings,
        run.output_samples,
        ir_solve_timeout_secs,
        solver_timeout_secs,
    ) {
        Ok(child) => child,
        Err(err) => return Err(Box::new(ctx.solver_fail(err))),
    };
    let perf_session = start_sim_worker_perf_session(child.id(), &run.artifacts);
    if let Err(err) = write_sim_worker_stdin(&mut child, &run.dae_payload, &run.model_name) {
        let _ = child.kill();
        let _ = child.wait();
        if let Some(session) = perf_session {
            session.finish();
        }
        return Err(Box::new(ctx.solver_fail(err)));
    }
    Ok((
        child,
        perf_session,
        solver_timeout_secs,
        process_timeout_secs,
    ))
}

fn wait_for_completed_sim_worker(
    child: &mut std::process::Child,
    perf_session: Option<PerfSession>,
    ctx: &SimRunContext<'_>,
    artifacts: &SimWorkerArtifacts,
    solver_timeout_secs: f64,
    process_timeout_secs: f64,
) -> Result<f64, Box<MslSimModelResult>> {
    let wait_outcome = match wait_for_sim_worker(child, ctx, process_timeout_secs) {
        Ok(outcome) => outcome,
        Err(err) => {
            return Err(Box::new(attach_worker_artifacts(
                ctx.solver_fail(err),
                artifacts,
            )));
        }
    };

    match wait_outcome {
        WorkerWaitOutcome::TimedOut { elapsed_secs } => {
            let _ = child.kill();
            let _ = child.wait();
            if let Some(session) = perf_session {
                session.finish();
            }
            Err(Box::new(attach_worker_artifacts(
                ctx.timeout(elapsed_secs, solver_timeout_secs, process_timeout_secs),
                artifacts,
            )))
        }
        WorkerWaitOutcome::Exited {
            status,
            elapsed_secs,
        } => {
            if let Some(session) = perf_session {
                session.finish();
            }
            if !status.success() {
                Err(Box::new(attach_worker_artifacts(
                    ctx.solver_fail(format!("sim worker exited unsuccessfully: {status}")),
                    artifacts,
                )))
            } else {
                Ok(elapsed_secs)
            }
        }
    }
}

fn finalize_sim_worker_result(
    ctx: &SimRunContext<'_>,
    artifacts: &SimWorkerArtifacts,
    worker_result: SimWorkerResult,
    elapsed_secs: f64,
) -> MslSimModelResult {
    let timing = SimTimingBreakdown::from_worker_result(&worker_result, elapsed_secs);
    let status = match parse_sim_status(&worker_result.status) {
        Some(status) => status,
        None => {
            return ctx.solver_fail(format!(
                "worker returned unknown status '{}' for {}",
                worker_result.status, ctx.model_name
            ));
        }
    };
    let sim_trace_file = if matches!(status, SimStatus::Ok) && artifacts.trace_path.is_file() {
        Some(artifacts.trace_relative_path.clone())
    } else {
        None
    };
    let ir_solve_file = if artifacts.solve_ir_path.is_file() {
        Some(artifacts.solve_ir_relative_path.clone())
    } else {
        worker_result.solve_ir_file
    };
    let sim_perf_profile_file = retain_sim_worker_perf_profile(artifacts, elapsed_secs, &status);
    ctx.finish(SimRunOutcome {
        status,
        error: worker_result.error,
        ic_status: worker_result.ic_status,
        ic_error: worker_result.ic_error,
        ic_seconds: worker_result.ic_seconds,
        timing,
        sim_trace_file,
        sim_perf_profile_file,
        sim_trace_error: worker_result.trace_error,
        ir_dae_file: Some(artifacts.dae_relative_path.clone()),
        ir_solve_file,
        ir_solve_error: worker_result.solve_ir_error,
    })
}

pub(super) fn run_prepared_simulation(run: PreparedSimulationRun) -> MslSimModelResult {
    let model_name = run.model_name.clone();
    let ctx = SimRunContext {
        model_name: &model_name,
        n_states: run.n_states,
        n_algebraics: run.n_algebraics,
    };

    let worker_exe = match resolve_sim_worker_exe() {
        Ok(path) => path,
        Err(err) => return ctx.solver_fail(err),
    };
    let (mut child, perf_session, solver_timeout_secs, process_timeout_secs) =
        match spawn_prepared_sim_worker(&ctx, worker_exe, &run) {
            Ok(spawned) => spawned,
            Err(result) => return *result,
        };
    let elapsed_secs = match wait_for_completed_sim_worker(
        &mut child,
        perf_session,
        &ctx,
        &run.artifacts,
        solver_timeout_secs,
        process_timeout_secs,
    ) {
        Ok(elapsed_secs) => elapsed_secs,
        Err(result) => return *result,
    };

    let worker_result = match read_sim_worker_result(&run.artifacts) {
        Ok(result) => result,
        Err(err) => return ctx.solver_fail(err),
    };
    finalize_sim_worker_result(&ctx, &run.artifacts, worker_result, elapsed_secs)
}

pub(super) fn is_trivial_static_model(dae: &Dae) -> bool {
    let discrete_real_scalars: usize = dae
        .variables
        .discrete_reals
        .values()
        .map(|v| v.size())
        .sum();
    let discrete_valued_scalars: usize = dae
        .variables
        .discrete_valued
        .values()
        .map(|v| v.size())
        .sum();
    discrete_real_scalars == 0
        && discrete_valued_scalars == 0
        && dae.continuous.equations.is_empty()
        && dae.discrete.real_updates.is_empty()
        && dae.discrete.valued_updates.is_empty()
        && dae.conditions.equations.is_empty()
        && dae.conditions.relations.is_empty()
        && dae.initialization.equations.is_empty()
}

pub(super) fn try_simulate_dae_with_settings(
    dae: &Dae,
    model_name: &str,
    settings: &SimExecutionSettings,
) -> MslSimModelResult {
    let n_states = dae.variables.states.len();
    let n_algebraics = dae.variables.algebraics.len();
    let n_state_scalars: usize = dae.variables.states.values().map(|v| v.size()).sum();

    let total_unknowns: usize = dae
        .variables
        .states
        .values()
        .map(|v| v.size())
        .sum::<usize>()
        + dae
            .variables
            .algebraics
            .values()
            .map(|v| v.size())
            .sum::<usize>()
        + dae
            .variables
            .outputs
            .values()
            .map(|v| v.size())
            .sum::<usize>();
    if total_unknowns == 0 && is_trivial_static_model(dae) {
        return MslSimModelResult {
            name: model_name.to_string(),
            status: SimStatus::Ok,
            error: None,
            ic_status: Some("ic_ok".to_string()),
            ic_error: None,
            ic_seconds: Some(0.0),
            n_states: Some(n_states),
            n_algebraics: Some(n_algebraics),
            sim_seconds: Some(0.0),
            sim_build_seconds: Some(0.0),
            ir_solve_seconds: Some(0.0),
            ir_solve_structural_dae_seconds: Some(0.0),
            ir_solve_lower_seconds: Some(0.0),
            sim_backend_build_seconds: Some(0.0),
            sim_run_seconds: Some(0.0),
            sim_wall_seconds: Some(0.0),
            sim_trace_file: None,
            sim_perf_profile_file: None,
            sim_trace_error: None,
            ir_dae_file: None,
            ir_solve_file: None,
            ir_solve_error: None,
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
    use rumoca_compile::compile::{Session, SessionConfig};

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
    fn serialize_worker_input_round_trips_dae_json_payload() {
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
        let round_tripped: Dae = serde_json::from_slice(&payload).expect("decode worker input");

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
    fn sim_worker_wall_timeout_always_includes_parent_grace() {
        assert_eq!(
            sim_worker_wall_timeout_secs(10.0, 10.0),
            20.0 + SIM_WORKER_TIMEOUT_GRACE_SECS
        );
    }

    #[test]
    fn attach_worker_artifacts_preserves_solve_ir_after_worker_failure() {
        let temp = tempfile::tempdir().expect("temp dir");
        let dae_path = temp.path().join("M.dae.json");
        let solve_ir_path = temp.path().join("M.solve.json");
        fs::write(&dae_path, b"{\"schema_version\":1}").expect("write dae artifact");
        fs::write(&solve_ir_path, b"{\"schema_version\":1}").expect("write solve artifact");
        let artifacts = SimWorkerArtifacts {
            output_path: temp.path().join("worker.json"),
            dae_path,
            dae_relative_path: "ir_dae/M.json".to_string(),
            solve_ir_path,
            solve_ir_relative_path: "ir_solve/M.json".to_string(),
            trace_path: temp.path().join("trace.json"),
            trace_relative_path: "sim_traces/rumoca/M.json".to_string(),
            perf_profile_path: None,
            perf_profile_relative_path: None,
        };
        let ctx = SimRunContext {
            model_name: "M",
            n_states: 1,
            n_algebraics: 1,
        };

        let result = attach_worker_artifacts(
            ctx.solver_fail("sim worker exited unsuccessfully: signal: 6 (SIGABRT)"),
            &artifacts,
        );

        assert_eq!(result.ir_dae_file.as_deref(), Some("ir_dae/M.json"));
        assert_eq!(result.ir_solve_file.as_deref(), Some("ir_solve/M.json"));
    }

    #[test]
    fn sim_worker_address_space_limit_adds_virtual_memory_headroom() {
        assert_eq!(
            sim_worker_address_space_limit_bytes(2048),
            4096 * 1024 * 1024
        );
        assert_eq!(
            sim_worker_address_space_limit_bytes(512),
            1024 * 1024 * 1024
        );
    }

    #[test]
    fn gate_simulation_settings_by_compile_budget_preserves_timeout_settings() {
        let settings = SimExecutionSettings {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.01),
            rtol: Some(1e-6),
            atol: Some(1e-6),
            solver: "auto".to_string(),
            timeout_seconds: None,
        };

        let gated = gate_simulation_settings_by_compile_budget(settings, Some(10.0))
            .expect("positive compile budget should allow simulation");

        assert_eq!(gated.timeout_seconds, None);
    }

    #[test]
    fn run_prepared_simulation_streams_dae_json_over_stdin() {
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
            compiled.dae.variables.states.len(),
            compiled.dae.variables.algebraics.len(),
        )
        .expect("prepare simulation");
        let result = run_prepared_simulation(prepared);

        assert!(matches!(result.status, SimStatus::Ok), "{result:?}");
        assert_eq!(result.error, None);
        assert!(result.sim_seconds.is_some());
        assert!(result.sim_wall_seconds.is_some());
    }
}
