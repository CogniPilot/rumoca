use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::{OnceLock, mpsc};
use std::time::{Duration, Instant};

use rumoca_compile::compile::FailedPhase;
use serde::{Deserialize, Serialize, de::DeserializeOwned};

/// Per-model wall timeout (seconds) for one MSL-parity simulation. Shared so the
/// rumoca sim worker and the OMC reference run use the *same* budget (a model is
/// only fairly comparable when both tools are given identical time to simulate).
pub const MSL_SIM_TIMEOUT_SECS: f64 = 10.0;

pub const MODEL_WORKER_PROTOCOL_VERSION: u32 = 1;
pub const MODEL_WORKER_RESULT_FILE: &str = "result.json";
pub const MODEL_WORKER_PARTIAL_RESULT_FILE: &str = "partial_result.json";
const MODEL_WORKER_POLL_MILLIS: u64 = 20;
static MODEL_WORKER_EXE: OnceLock<Result<PathBuf, String>> = OnceLock::new();

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelWorkerRequest {
    pub protocol_version: u32,
    pub model_name: String,
    pub run_simulation: bool,
    pub selected_for_simulation: bool,
    pub explicit_sim_target: bool,
    /// Emit the machine-exact IR JSON (`ir-*.json`) for each stage. Off by
    /// default for interactive debugging (the readable `ir-*.mo` Modelica dumps
    /// are preferred); enable when exact op/index or span detail is needed.
    pub emit_json: bool,
    #[serde(default)]
    pub allow_unbalanced_for_diagnostics: bool,
    /// Enable NaN/non-finite runtime tracing (a first-class debug flag rather
    /// than an env var) — see `rumoca_eval_solve::nan_trace`.
    #[serde(default)]
    pub nan_trace: bool,
    /// Emit human-readable Modelica reconstructions of each IR stage
    /// (`<model>_<stage>.mo`) so transforms can be diffed stage-to-stage.
    #[serde(default)]
    pub emit_modelica: bool,
    pub source_root_path: PathBuf,
    pub output_dir: PathBuf,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
pub enum ModelWorkerCommand {
    Run { request: ModelWorkerRequest },
    Shutdown,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum ModelWorkerControlMessage {
    Ready {
        protocol_version: u32,
        cpu_affinity_applied: Option<bool>,
    },
    Result {
        response: Box<ModelWorkerResponse>,
    },
    Error {
        protocol_version: u32,
        message: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelWorkerResponse {
    pub protocol_version: u32,
    pub elapsed_secs: f64,
    pub result: WorkerModelResult,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkerProgressEvent {
    pub model_name: String,
    pub phase: WorkerProgressPhase,
    pub event: WorkerProgressEventKind,
    pub elapsed_secs: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory: Option<WorkerMemorySnapshot>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkerProgressPhase {
    SourceRootLoad,
    Compile,
    Instantiate,
    Typecheck,
    Flatten,
    ToDae,
    Solve,
    SimBuild,
    IC,
    Sim,
    ArtifactWrite,
    Memory,
}

impl From<FailedPhase> for WorkerProgressPhase {
    fn from(value: FailedPhase) -> Self {
        match value {
            FailedPhase::Instantiate => Self::Instantiate,
            FailedPhase::Typecheck => Self::Typecheck,
            FailedPhase::Flatten => Self::Flatten,
            FailedPhase::ToDae => Self::ToDae,
        }
    }
}

impl std::fmt::Display for WorkerProgressPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SourceRootLoad => write!(f, "SourceRootLoad"),
            Self::Compile => write!(f, "Compile"),
            Self::Instantiate => write!(f, "Instantiate"),
            Self::Typecheck => write!(f, "Typecheck"),
            Self::Flatten => write!(f, "Flatten"),
            Self::ToDae => write!(f, "ToDae"),
            Self::Solve => write!(f, "Solve"),
            Self::SimBuild => write!(f, "SimBuild"),
            Self::IC => write!(f, "IC"),
            Self::Sim => write!(f, "Sim"),
            Self::ArtifactWrite => write!(f, "ArtifactWrite"),
            Self::Memory => write!(f, "Memory"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WorkerProgressEventKind {
    Started,
    Completed,
    Failed,
    Snapshot,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkerMemorySnapshot {
    pub label: String,
    pub rss_kb: Option<u64>,
    pub pss_kb: Option<u64>,
    pub private_clean_kb: Option<u64>,
    pub private_dirty_kb: Option<u64>,
    pub shared_clean_kb: Option<u64>,
    pub shared_dirty_kb: Option<u64>,
    pub anonymous_kb: Option<u64>,
    pub swap_kb: Option<u64>,
}

#[derive(Debug)]
pub struct ModelWorkerPhaseMonitor {
    active_phase: Option<WorkerProgressPhase>,
    active_since: Instant,
    seen_progress: bool,
}

impl ModelWorkerPhaseMonitor {
    pub fn new() -> Self {
        Self {
            active_phase: None,
            active_since: Instant::now(),
            seen_progress: false,
        }
    }

    pub fn update(&mut self, progress_jsonl: &Path) -> Option<WorkerProgressPhase> {
        let state = worker_progress_state(progress_jsonl);
        self.seen_progress = state.seen_progress;
        let active_phase = state.active_phase;
        if active_phase != self.active_phase {
            self.active_phase = active_phase;
            self.active_since = Instant::now();
        }
        self.active_phase
    }

    pub fn phase_elapsed(&self) -> Duration {
        self.active_since.elapsed()
    }

    pub fn has_seen_progress(&self) -> bool {
        self.seen_progress
    }
}

impl Default for ModelWorkerPhaseMonitor {
    fn default() -> Self {
        Self::new()
    }
}

pub enum ModelWorkerRunOutcome {
    Completed(Box<ModelWorkerResponse>),
    TimedOut {
        elapsed_secs: f64,
        active_phase: Option<WorkerProgressPhase>,
        phase_timeout_secs: f64,
    },
    Failed(String),
}

pub struct ModelWorkerDaemon {
    child: Child,
    stdin: ChildStdin,
    messages: mpsc::Receiver<Result<ModelWorkerControlMessage, String>>,
    cpu_affinity_applied: Option<bool>,
}

impl ModelWorkerDaemon {
    pub fn spawn(
        source_root_path: &Path,
        timeout_secs: f64,
        cpu_core_id: Option<usize>,
    ) -> Result<Self, String> {
        let worker_exe = resolve_model_worker_exe()?;
        let mut command = Command::new(worker_exe);
        command.arg("--source-root-path").arg(source_root_path);
        command.arg("--jobs").arg("1");
        if let Some(cpu_core_id) = cpu_core_id {
            command.arg("--cpu-core-id").arg(cpu_core_id.to_string());
        }
        let mut child = command
            .env("RAYON_NUM_THREADS", "1")
            .env("MIMALLOC_ARENA_EAGER_COMMIT", "0")
            .env("MIMALLOC_PURGE_DELAY", "0")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(|error| format!("failed to spawn model worker: {error}"))?;
        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| "failed to open model worker stdin".to_string())?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| "failed to open model worker stdout".to_string())?;
        let messages = spawn_model_worker_message_reader(stdout);
        let mut worker = Self {
            child,
            stdin,
            messages,
            cpu_affinity_applied: None,
        };
        match worker.wait_for_ready(timeout_secs) {
            Ok(()) => Ok(worker),
            Err(error) => {
                worker.kill_and_join();
                Err(error)
            }
        }
    }

    pub fn process_id(&self) -> u32 {
        self.child.id()
    }

    pub fn cpu_affinity_applied(&self) -> Option<bool> {
        self.cpu_affinity_applied
    }

    pub fn run_request(
        &mut self,
        request: &ModelWorkerRequest,
        timeout_secs: f64,
        progress_jsonl: &Path,
    ) -> ModelWorkerRunOutcome {
        if let Err(error) = self.send_command(&ModelWorkerCommand::Run {
            request: request.clone(),
        }) {
            return ModelWorkerRunOutcome::Failed(error);
        }
        let start = Instant::now();
        let mut phase_monitor = ModelWorkerPhaseMonitor::new();
        loop {
            let active_phase = phase_monitor.update(progress_jsonl);
            if let Some(active_phase) = active_phase
                && phase_monitor.phase_elapsed() >= Duration::from_secs_f64(timeout_secs)
            {
                self.kill_and_join();
                return ModelWorkerRunOutcome::TimedOut {
                    elapsed_secs: start.elapsed().as_secs_f64(),
                    active_phase: Some(active_phase),
                    phase_timeout_secs: timeout_secs,
                };
            }
            if active_phase.is_none()
                && !phase_monitor.has_seen_progress()
                && start.elapsed() >= Duration::from_secs_f64(timeout_secs)
            {
                self.kill_and_join();
                return ModelWorkerRunOutcome::TimedOut {
                    elapsed_secs: start.elapsed().as_secs_f64(),
                    active_phase: None,
                    phase_timeout_secs: timeout_secs,
                };
            }
            match self.messages.try_recv() {
                Ok(Ok(ModelWorkerControlMessage::Result { response }))
                    if response.protocol_version == MODEL_WORKER_PROTOCOL_VERSION =>
                {
                    return ModelWorkerRunOutcome::Completed(response);
                }
                Ok(Ok(ModelWorkerControlMessage::Result { response })) => {
                    return ModelWorkerRunOutcome::Failed(format!(
                        "model worker wrote protocol {}, expected {}",
                        response.protocol_version, MODEL_WORKER_PROTOCOL_VERSION
                    ));
                }
                Ok(Ok(ModelWorkerControlMessage::Error { message, .. })) => {
                    return ModelWorkerRunOutcome::Failed(message);
                }
                Ok(Ok(ModelWorkerControlMessage::Ready { .. })) => {}
                Ok(Err(error)) => return ModelWorkerRunOutcome::Failed(error),
                Err(mpsc::TryRecvError::Empty) => {}
                Err(mpsc::TryRecvError::Disconnected) => {
                    return ModelWorkerRunOutcome::Failed(
                        "model worker stdout closed before result".to_string(),
                    );
                }
            }
            match self.child.try_wait() {
                Ok(Some(status)) => {
                    return ModelWorkerRunOutcome::Failed(format!(
                        "model worker exited with status {status}"
                    ));
                }
                Ok(None) => {
                    std::thread::sleep(Duration::from_millis(MODEL_WORKER_POLL_MILLIS));
                }
                Err(error) => {
                    self.kill_and_join();
                    return ModelWorkerRunOutcome::Failed(format!(
                        "failed to poll model worker: {error}"
                    ));
                }
            }
        }
    }

    pub fn shutdown_and_join(&mut self) {
        let _ = self.send_command(&ModelWorkerCommand::Shutdown);
        let _ = self.child.wait();
    }

    fn wait_for_ready(&mut self, timeout_secs: f64) -> Result<(), String> {
        let start = Instant::now();
        loop {
            match self.messages.try_recv() {
                Ok(Ok(ModelWorkerControlMessage::Ready {
                    protocol_version,
                    cpu_affinity_applied,
                })) if protocol_version == MODEL_WORKER_PROTOCOL_VERSION => {
                    self.cpu_affinity_applied = cpu_affinity_applied;
                    return Ok(());
                }
                Ok(Ok(ModelWorkerControlMessage::Ready {
                    protocol_version, ..
                })) => {
                    return Err(format!(
                        "model worker ready protocol {}, expected {}",
                        protocol_version, MODEL_WORKER_PROTOCOL_VERSION
                    ));
                }
                Ok(Ok(ModelWorkerControlMessage::Error { message, .. })) => return Err(message),
                Ok(Ok(ModelWorkerControlMessage::Result { .. })) => {
                    return Err("model worker returned result before ready".to_string());
                }
                Ok(Err(error)) => return Err(error),
                Err(mpsc::TryRecvError::Empty) => {}
                Err(mpsc::TryRecvError::Disconnected) => {
                    return Err("model worker stdout closed before ready".to_string());
                }
            }
            match self.child.try_wait() {
                Ok(Some(status)) => {
                    return Err(format!(
                        "model worker exited before ready with status {status}"
                    ));
                }
                Ok(None) => {}
                Err(error) => return Err(format!("failed to poll model worker startup: {error}")),
            }
            if start.elapsed() >= Duration::from_secs_f64(timeout_secs) {
                return Err(format!(
                    "model worker exceeded {:.3}s startup budget in phase SourceRootLoad",
                    timeout_secs
                ));
            }
            std::thread::sleep(Duration::from_millis(MODEL_WORKER_POLL_MILLIS));
        }
    }

    fn send_command(&mut self, command: &ModelWorkerCommand) -> Result<(), String> {
        serde_json::to_writer(&mut self.stdin, command)
            .map_err(|error| format!("failed to write model worker command: {error}"))?;
        writeln!(self.stdin)
            .map_err(|error| format!("failed to flush model worker command: {error}"))
    }

    fn kill_and_join(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

impl Drop for ModelWorkerDaemon {
    fn drop(&mut self) {
        self.kill_and_join();
    }
}

pub fn available_cpu_core_ids() -> Vec<usize> {
    core_affinity::get_core_ids()
        .unwrap_or_default()
        .into_iter()
        .map(|core| core.id)
        .collect()
}

/// Number of physical CPU cores (ignoring SMT/hyper-threading siblings).
///
/// On Linux this reads `/sys/devices/system/cpu/cpu*/topology` and counts
/// distinct `(physical_package_id, core_id)` pairs. Returns `None` when the
/// topology is unavailable, so callers can fall back to logical parallelism.
/// Used to size warm worker pools so we run one heavyweight worker per real
/// core rather than oversubscribing SMT siblings.
#[cfg(target_os = "linux")]
pub fn physical_cpu_core_count() -> Option<usize> {
    use std::collections::BTreeSet;
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
        let package_id = std::fs::read_to_string(topology.join("physical_package_id")).ok()?;
        let core_id = std::fs::read_to_string(topology.join("core_id")).ok()?;
        cores.insert((package_id.trim().to_string(), core_id.trim().to_string()));
    }
    (!cores.is_empty()).then_some(cores.len())
}

#[cfg(not(target_os = "linux"))]
pub fn physical_cpu_core_count() -> Option<usize> {
    None
}

/// Hosts with more than this many logical CPUs reserve cores for headroom so a
/// long warm-worker run does not make the machine unusable for other work.
pub const WARM_WORKER_HEADROOM_THRESHOLD: usize = 16;
/// Cores left free on large hosts (matches the rumoca MSL stage default).
pub const WARM_WORKER_RESERVED_CORES: usize = 2;

/// Recommended size for a pool of heavyweight warm workers (one OS process per
/// worker), mirroring the rumoca MSL stage policy:
///   * `requested == 0` (auto): use physical cores, minus
///     [`WARM_WORKER_RESERVED_CORES`] on hosts with more than
///     [`WARM_WORKER_HEADROOM_THRESHOLD`] logical CPUs, leaving headroom.
///   * `requested != 0`: honor it, but never exceed the physical core count
///     (one heavyweight worker per real core; do not oversubscribe SMT siblings).
pub fn warm_worker_pool_size(requested: usize) -> usize {
    let logical = std::thread::available_parallelism()
        .map(usize::from)
        .unwrap_or(1);
    let physical = physical_cpu_core_count().unwrap_or(logical).max(1);
    if requested != 0 {
        return requested.min(physical).max(1);
    }
    if logical > WARM_WORKER_HEADROOM_THRESHOLD {
        physical.saturating_sub(WARM_WORKER_RESERVED_CORES).max(1)
    } else {
        physical.max(1)
    }
}

pub fn cpu_core_plan(worker_count: usize) -> Vec<Option<usize>> {
    let mut core_ids = available_cpu_core_ids();
    core_ids.sort_unstable();
    core_ids
        .into_iter()
        .map(Some)
        .chain(std::iter::repeat(None))
        .take(worker_count)
        .collect()
}

pub fn pin_current_thread_to_cpu_core(cpu_core_id: usize) -> Result<(), String> {
    let core = core_affinity::get_core_ids()
        .unwrap_or_default()
        .into_iter()
        .find(|core| core.id == cpu_core_id)
        .ok_or_else(|| format!("requested CPU core {cpu_core_id} is not available"))?;
    if core_affinity::set_for_current(core) {
        Ok(())
    } else {
        Err(format!(
            "failed to pin worker thread to CPU core {cpu_core_id}"
        ))
    }
}

pub fn resolve_model_worker_exe() -> Result<&'static Path, String> {
    match MODEL_WORKER_EXE.get_or_init(resolve_model_worker_exe_inner) {
        Ok(path) => Ok(path.as_path()),
        Err(err) => Err(err.clone()),
    }
}

fn resolve_model_worker_exe_inner() -> Result<PathBuf, String> {
    if let Some(compile_time_path) = option_env!("CARGO_BIN_EXE_rumoca-worker") {
        let candidate = PathBuf::from(compile_time_path);
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    if let Ok(path) = std::env::var("CARGO_BIN_EXE_rumoca-worker") {
        let candidate = PathBuf::from(path);
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    let current_exe = std::env::current_exe()
        .map_err(|error| format!("failed to get current binary path: {error}"))?;
    let deps_dir = current_exe.parent().ok_or_else(|| {
        format!(
            "failed to get parent directory for current binary: {}",
            current_exe.display()
        )
    })?;
    let profile_dir = deps_dir.parent().ok_or_else(|| {
        format!(
            "failed to get profile directory from binary path: {}",
            current_exe.display()
        )
    })?;
    build_model_worker_exe(profile_dir)?;

    let mut candidates = vec![
        profile_dir.join("rumoca-worker"),
        profile_dir.join("rumoca-worker.exe"),
    ];
    if let Ok(entries) = std::fs::read_dir(deps_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_file() {
                continue;
            }
            let file_name = entry.file_name();
            let file_name = file_name.to_string_lossy();
            if file_name.starts_with("rumoca-worker-") {
                candidates.push(path);
            }
        }
    }

    if let Some(candidate) = candidates.into_iter().find(|path| path.is_file()) {
        return Ok(candidate);
    }

    Err(format!(
        "failed to locate rumoca-worker binary; expected CARGO_BIN_EXE_rumoca-worker or a binary near {}",
        current_exe.display()
    ))
}

fn build_model_worker_exe(profile_dir: &Path) -> Result<(), String> {
    let cargo = std::env::var_os("CARGO")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("cargo"));
    let mut command = Command::new(cargo);
    command
        .arg("build")
        .arg("-p")
        .arg("rumoca-worker")
        .arg("--bin")
        .arg("rumoca-worker");
    if profile_dir
        .file_name()
        .is_some_and(|name| name == std::ffi::OsStr::new("release"))
    {
        command.arg("--release");
    }
    let status = command
        .status()
        .map_err(|error| format!("failed to build rumoca-worker: {error}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "failed to build rumoca-worker with status {status}"
        ))
    }
}

fn spawn_model_worker_message_reader(
    stdout: std::process::ChildStdout,
) -> mpsc::Receiver<Result<ModelWorkerControlMessage, String>> {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let reader = std::io::BufReader::new(stdout);
        for line in reader.lines() {
            let line = match line {
                Ok(line) => line,
                Err(error) => {
                    let _ = tx.send(Err(format!("failed to read model worker stdout: {error}")));
                    return;
                }
            };
            if line.trim().is_empty() {
                continue;
            }
            let message = serde_json::from_str::<ModelWorkerControlMessage>(&line)
                .map_err(|error| format!("failed to parse model worker message: {error}"));
            if tx.send(message).is_err() {
                return;
            }
        }
    });
    rx
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkerModelResult {
    pub model_name: String,
    pub phase_reached: String,
    pub error: Option<String>,
    pub error_code: Option<String>,
    pub num_states: Option<usize>,
    pub num_algebraics: Option<usize>,
    pub num_f_x: Option<usize>,
    pub balance: Option<i64>,
    pub is_balanced: Option<bool>,
    pub is_partial: Option<bool>,
    pub class_type: Option<String>,
    pub scalar_equations: Option<usize>,
    pub scalar_unknowns: Option<usize>,
    pub initial_equation_scalars: Option<usize>,
    pub initial_algorithm_scalars: Option<usize>,
    pub initial_balance_deficit_before: Option<i64>,
    pub initial_closure_used: Option<usize>,
    pub initial_balance_deficit_after: Option<i64>,
    pub initial_balance_ok: Option<bool>,
    pub compile_seconds: Option<f64>,
    pub instantiate_seconds: Option<f64>,
    pub typecheck_seconds: Option<f64>,
    pub flatten_seconds: Option<f64>,
    pub dae_seconds: Option<f64>,
    pub compile_perf_profile_file: Option<String>,
    pub ir_ast_file: Option<String>,
    pub ir_flat_file: Option<String>,
    pub sim_status: Option<String>,
    pub sim_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sim_error_span: Option<rumoca_core::Span>,
    pub ic_status: Option<String>,
    pub ic_error: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ic_error_span: Option<rumoca_core::Span>,
    pub ic_seconds: Option<f64>,
    pub sim_seconds: Option<f64>,
    pub sim_build_seconds: Option<f64>,
    pub ir_solve_seconds: Option<f64>,
    pub ir_solve_structural_dae_seconds: Option<f64>,
    pub ir_solve_lower_seconds: Option<f64>,
    pub sim_backend_build_seconds: Option<f64>,
    pub sim_run_seconds: Option<f64>,
    pub sim_wall_seconds: Option<f64>,
    pub sim_trace_file: Option<String>,
    pub sim_perf_profile_file: Option<String>,
    pub sim_trace_error: Option<String>,
    pub ir_dae_file: Option<String>,
    pub ir_solve_file: Option<String>,
    pub ir_solve_error: Option<String>,
    pub timeout_phase: Option<WorkerProgressPhase>,
    pub timeout_seconds: Option<f64>,
}

impl WorkerModelResult {
    pub fn phase_failure(
        model_name: String,
        phase_reached: impl Into<String>,
        error: impl Into<String>,
        error_code: Option<String>,
    ) -> Self {
        Self {
            model_name,
            phase_reached: phase_reached.into(),
            error: Some(error.into()),
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
}

pub fn read_model_worker_request_file(
    path: &std::path::Path,
) -> Result<ModelWorkerRequest, String> {
    read_json_file(path)
}

pub fn write_model_worker_request_file(
    path: &std::path::Path,
    request: &ModelWorkerRequest,
) -> Result<(), String> {
    write_json_file(path, request)
}

pub fn read_model_worker_response_file(
    path: &std::path::Path,
) -> Result<ModelWorkerResponse, String> {
    read_json_file(path)
}

pub fn write_model_worker_response_file(
    path: &std::path::Path,
    response: &ModelWorkerResponse,
) -> Result<(), String> {
    write_json_file(path, response)
}

pub fn last_active_worker_phase(progress_jsonl: &Path) -> Option<WorkerProgressPhase> {
    worker_progress_state(progress_jsonl).active_phase
}

struct WorkerProgressState {
    active_phase: Option<WorkerProgressPhase>,
    seen_progress: bool,
}

fn worker_progress_state(progress_jsonl: &Path) -> WorkerProgressState {
    let Ok(raw) = std::fs::read_to_string(progress_jsonl) else {
        return WorkerProgressState {
            active_phase: None,
            seen_progress: false,
        };
    };
    let mut active = None;
    let mut seen_progress = false;
    for line in raw.lines().filter(|line| !line.trim().is_empty()) {
        let Ok(event) = serde_json::from_str::<WorkerProgressEvent>(line) else {
            continue;
        };
        seen_progress = true;
        match event.event {
            WorkerProgressEventKind::Started => active = Some(event.phase),
            WorkerProgressEventKind::Completed => active = None,
            WorkerProgressEventKind::Failed => active = Some(event.phase),
            WorkerProgressEventKind::Snapshot => {}
        }
    }
    WorkerProgressState {
        active_phase: active,
        seen_progress,
    }
}

fn read_json_file<T: DeserializeOwned>(path: &std::path::Path) -> Result<T, String> {
    let file = std::fs::File::open(path)
        .map_err(|error| format!("failed to open JSON file '{}': {error}", path.display()))?;
    serde_json::from_reader(file)
        .map_err(|error| format!("failed to parse JSON file '{}': {error}", path.display()))
}

fn write_json_file<T: Serialize>(path: &std::path::Path, value: &T) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create JSON parent directory '{}': {error}",
                parent.display()
            )
        })?;
    }
    let file = std::fs::File::create(path)
        .map_err(|error| format!("failed to create JSON file '{}': {error}", path.display()))?;
    serde_json::to_writer_pretty(file, value)
        .map_err(|error| format!("failed to write JSON file '{}': {error}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ready_message_preserves_affinity_result() {
        let ready = ModelWorkerControlMessage::Ready {
            protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
            cpu_affinity_applied: Some(false),
        };
        let encoded = serde_json::to_string(&ready).unwrap();
        let decoded: ModelWorkerControlMessage = serde_json::from_str(&encoded).unwrap();
        assert!(matches!(
            decoded,
            ModelWorkerControlMessage::Ready {
                cpu_affinity_applied: Some(false),
                ..
            }
        ));
    }

    #[test]
    fn ready_message_allows_unrequested_affinity() {
        let ready = ModelWorkerControlMessage::Ready {
            protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
            cpu_affinity_applied: None,
        };
        assert!(
            serde_json::to_string(&ready)
                .unwrap()
                .contains("cpu_affinity_applied")
        );
    }

    #[test]
    fn cpu_core_plan_has_one_entry_per_worker() {
        assert_eq!(cpu_core_plan(0), Vec::<Option<usize>>::new());
        assert_eq!(cpu_core_plan(3).len(), 3);
    }
}
