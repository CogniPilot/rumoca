#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fs::{self, File};
use std::io::{BufRead, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::time::Instant;
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use clap::Parser;
use rumoca_compile::compile::{
    CompilePhaseEvent, DaeCompilationResult, FailedPhase, Session, SessionConfig, SourceRootKind,
    install_compile_phase_observer,
};
use rumoca_sim::{
    BuildSimulationTimings, PreparedSimulation, SimError, SimOptions, SimResult, SimSolverMode,
    build_simulation_with_stage_timing_and_solve_model, check_prepared_initialization,
    run_prepared_simulation, structurally_lowered_dae_for_simulation_artifact,
};
use rumoca_worker::{
    MODEL_WORKER_PARTIAL_RESULT_FILE, MODEL_WORKER_PROTOCOL_VERSION, MODEL_WORKER_RESULT_FILE,
    ModelWorkerCommand, ModelWorkerControlMessage, ModelWorkerRequest, ModelWorkerResponse,
    WorkerMemorySnapshot, WorkerModelResult, WorkerProgressEvent, WorkerProgressEventKind,
    WorkerProgressPhase, pin_current_thread_to_cpu_core, read_model_worker_request_file,
    write_model_worker_response_file,
};

const DEFAULT_SIM_END_TIME_SECS: f64 = 1.0;
const SIM_OUTPUT_SAMPLES_DEFAULT: usize = 100;
const SIM_OUTPUT_SAMPLES_NO_STATES: usize = 500;
const DEFAULT_WORKER_STACK_MB: usize = 64;

#[derive(Debug, Parser)]
#[command(name = "rumoca-worker")]
#[command(about = "Isolated full-model compile/sim worker for rumoca")]
struct Args {
    #[arg(long)]
    source_root_path: Option<PathBuf>,
    #[arg(long)]
    request_json: Option<PathBuf>,
    #[arg(long)]
    cpu_core_id: Option<usize>,
    /// Compiler worker-thread count. The parent daemon passes `--jobs 1` because
    /// it already parallelizes across worker processes.
    #[arg(long)]
    jobs: Option<usize>,
}

#[derive(Debug, Clone)]
struct ProgressLog {
    model_name: String,
    started_at: Instant,
    path: PathBuf,
}

#[derive(Debug, Clone, Copy, Default)]
struct CompilePhaseDurations {
    instantiate_seconds: f64,
    typecheck_seconds: f64,
    flatten_seconds: f64,
    dae_seconds: f64,
}

#[derive(Debug, Default)]
struct CompilePhaseTimer {
    durations: CompilePhaseDurations,
    active: Option<(FailedPhase, Instant)>,
}

impl CompilePhaseTimer {
    fn observe(&mut self, phase: FailedPhase, event: CompilePhaseEvent) {
        match event {
            CompilePhaseEvent::Started => {
                self.active = Some((phase, Instant::now()));
            }
            CompilePhaseEvent::Completed => {
                let Some((active_phase, started_at)) = self.active.take() else {
                    return;
                };
                if active_phase != phase {
                    return;
                }
                self.record(phase, started_at.elapsed().as_secs_f64());
            }
        }
    }

    fn record(&mut self, phase: FailedPhase, seconds: f64) {
        match phase {
            FailedPhase::Instantiate => self.durations.instantiate_seconds += seconds,
            FailedPhase::Typecheck => self.durations.typecheck_seconds += seconds,
            FailedPhase::Flatten => self.durations.flatten_seconds += seconds,
            FailedPhase::ToDae => self.durations.dae_seconds += seconds,
        }
    }

    fn durations(&self) -> CompilePhaseDurations {
        self.durations
    }
}

fn some_positive_duration(seconds: f64) -> Option<f64> {
    (seconds.is_finite() && seconds > 0.0).then_some(seconds)
}

fn apply_compile_phase_durations(row: &mut WorkerModelResult, durations: CompilePhaseDurations) {
    row.instantiate_seconds = some_positive_duration(durations.instantiate_seconds);
    row.typecheck_seconds = some_positive_duration(durations.typecheck_seconds);
    row.flatten_seconds = some_positive_duration(durations.flatten_seconds);
    row.dae_seconds = some_positive_duration(durations.dae_seconds);
}

impl ProgressLog {
    fn new(model_name: &str, path: PathBuf) -> Self {
        if let Some(parent) = path.parent() {
            let _ = fs::create_dir_all(parent);
        }
        let _ = fs::remove_file(&path);
        Self::append(model_name, path)
    }

    fn append(model_name: &str, path: PathBuf) -> Self {
        if let Some(parent) = path.parent() {
            let _ = fs::create_dir_all(parent);
        }
        Self {
            model_name: model_name.to_string(),
            started_at: Instant::now(),
            path,
        }
    }

    fn event(&self, phase: WorkerProgressPhase, event: WorkerProgressEventKind) {
        let payload = WorkerProgressEvent {
            model_name: self.model_name.clone(),
            phase,
            event,
            elapsed_secs: self.started_at.elapsed().as_secs_f64(),
            memory: None,
        };
        self.write_payload(&payload);
    }

    fn compile_phase_event(&self, phase: FailedPhase, event: CompilePhaseEvent) {
        let event = match event {
            CompilePhaseEvent::Started => WorkerProgressEventKind::Started,
            CompilePhaseEvent::Completed => WorkerProgressEventKind::Completed,
        };
        self.event(phase.into(), event);
    }

    fn memory(&self, label: &str) {
        let payload = WorkerProgressEvent {
            model_name: self.model_name.clone(),
            phase: WorkerProgressPhase::Memory,
            event: WorkerProgressEventKind::Snapshot,
            elapsed_secs: self.started_at.elapsed().as_secs_f64(),
            memory: Some(current_memory_snapshot(label)),
        };
        self.write_payload(&payload);
    }

    fn write_payload(&self, payload: &WorkerProgressEvent) {
        let Ok(line) = serde_json::to_string(&payload) else {
            return;
        };
        if let Ok(mut file) = fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)
        {
            let _ = writeln!(file, "{line}");
        }
    }
}

fn current_memory_snapshot(label: &str) -> WorkerMemorySnapshot {
    let mut snapshot = WorkerMemorySnapshot {
        label: label.to_string(),
        rss_kb: None,
        pss_kb: None,
        private_clean_kb: None,
        private_dirty_kb: None,
        shared_clean_kb: None,
        shared_dirty_kb: None,
        anonymous_kb: None,
        swap_kb: None,
    };
    fill_current_memory_snapshot(&mut snapshot);
    snapshot
}

#[cfg(target_os = "linux")]
fn fill_current_memory_snapshot(snapshot: &mut WorkerMemorySnapshot) {
    let Ok(raw) = fs::read_to_string("/proc/self/smaps_rollup") else {
        return;
    };
    for line in raw.lines() {
        let mut fields = line.split_whitespace();
        let Some(key) = fields.next() else {
            continue;
        };
        let Some(value) = fields.next().and_then(|raw| raw.parse::<u64>().ok()) else {
            continue;
        };
        match key.trim_end_matches(':') {
            "Rss" => snapshot.rss_kb = Some(value),
            "Pss" => snapshot.pss_kb = Some(value),
            "Private_Clean" => snapshot.private_clean_kb = Some(value),
            "Private_Dirty" => snapshot.private_dirty_kb = Some(value),
            "Shared_Clean" => snapshot.shared_clean_kb = Some(value),
            "Shared_Dirty" => snapshot.shared_dirty_kb = Some(value),
            "Anonymous" => snapshot.anonymous_kb = Some(value),
            "Swap" => snapshot.swap_kb = Some(value),
            _ => {}
        }
    }
}

#[cfg(not(target_os = "linux"))]
fn fill_current_memory_snapshot(_snapshot: &mut WorkerMemorySnapshot) {
    // OS memory accounting is optional diagnostic data. Non-Linux workers keep
    // the snapshot schema but leave platform-specific counters empty.
}

#[derive(Debug, Clone)]
struct SimSettings {
    t_start: f64,
    t_end: f64,
    dt: Option<f64>,
    rtol: Option<f64>,
    atol: Option<f64>,
    solver: String,
}

struct WorkerRunOk {
    sim_result: SimResult,
    build_timings: BuildSimulationTimings,
    sim_build_seconds: f64,
    sim_run_seconds: f64,
    ic_seconds: f64,
    solve_file: Option<String>,
    solve_error: Option<String>,
}

struct WorkerRunErr {
    err: SimError,
    build_timings: BuildSimulationTimings,
    sim_build_seconds: f64,
    phase: WorkerErrorPhase,
    solve_file: Option<String>,
    solve_error: Option<String>,
}

struct WorkerPreparedSimulation {
    prepared: PreparedSimulation,
    build_timings: BuildSimulationTimings,
    sim_build_seconds: f64,
    solve_file: Option<String>,
    solve_error: Option<String>,
    sim_build_started: bool,
    solve_completed: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SimTraceArtifact {
    model_name: String,
    n_states: usize,
    times: Vec<f64>,
    names: Vec<String>,
    data: Vec<Vec<f64>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    variable_meta: Option<Vec<SimTraceVariableMetaArtifact>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SimTraceVariableMetaArtifact {
    name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    role: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    value_type: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    variability: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    time_domain: Option<String>,
}

fn panic_message(panic_info: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = panic_info.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = panic_info.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic".to_string()
    }
}

#[derive(Debug, Clone, Copy)]
enum WorkerErrorPhase {
    Build,
    SimBuild,
    Initialization { ic_seconds: f64 },
    Simulation { sim_run_seconds: f64 },
}

fn load_source_root(path: &Path) -> Result<Session, String> {
    let mut session = Session::new(SessionConfig::default());
    let report =
        session.load_source_root_tolerant("msl", SourceRootKind::DurableExternal, path, None);
    if report.diagnostics.is_empty() {
        Ok(session)
    } else {
        Err(format!(
            "failed to load source root '{}': {}",
            path.display(),
            report.diagnostics.join("; ")
        ))
    }
}

fn artifact_path(request: &ModelWorkerRequest, file_name: &str) -> PathBuf {
    request.output_dir.join(file_name)
}

fn artifact_relative_path(request: &ModelWorkerRequest, file_name: &str) -> String {
    Path::new("model_worker")
        .join(model_artifact_dir_name(&request.model_name))
        .join(file_name)
        .to_string_lossy()
        .to_string()
}

fn model_artifact_dir_name(model_name: &str) -> String {
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

fn write_artifact_text(
    request: &ModelWorkerRequest,
    file_name: &str,
    content: &str,
) -> Result<String, String> {
    let path = artifact_path(request, file_name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create model worker artifact directory '{}': {error}",
                parent.display()
            )
        })?;
    }
    fs::write(&path, content)
        .map_err(|error| format!("failed to write '{}': {error}", path.display()))?;
    Ok(artifact_relative_path(request, file_name))
}

/// Render a DAE-stage IR back to equivalent Modelica via the `dae-modelica`
/// target template, so transforms (alias elimination, index reduction,
/// dummy-derivative substitution, ...) can be read/diffed stage-to-stage.
fn write_modelica_dae_artifact(
    request: &ModelWorkerRequest,
    file_name: &str,
    dae: &rumoca_compile::compile::Dae,
) -> Result<String, String> {
    let template = rumoca_compile::codegen::templates::builtin_template_source(
        "dae-modelica",
        "dae_modelica.mo.jinja",
    )
    .ok_or_else(|| "missing built-in dae-modelica template".to_string())?;
    let rendered = rumoca_compile::codegen::render_dae_template_with_name(
        dae,
        template,
        rumoca_core::top_level_last_segment(&request.model_name),
    )
    .map_err(|error| format!("render dae-modelica: {error}"))?;
    write_artifact_text(request, file_name, &rendered)
}

fn write_modelica_flat_artifact(
    request: &ModelWorkerRequest,
    file_name: &str,
    flat: &rumoca_compile::compile::FlatModel,
) -> Result<String, String> {
    let template = rumoca_compile::codegen::templates::builtin_template_source(
        "flat-modelica",
        "flat_modelica.mo.jinja",
    )
    .ok_or_else(|| "missing built-in flat-modelica template".to_string())?;
    let rendered = rumoca_compile::codegen::render_flat_template_with_name(
        flat,
        template,
        rumoca_core::top_level_last_segment(&request.model_name),
    )
    .map_err(|error| format!("render flat-modelica: {error}"))?;
    write_artifact_text(request, file_name, &rendered)
}

fn write_artifact_json<T: serde::Serialize>(
    request: &ModelWorkerRequest,
    file_name: &str,
    value: &T,
) -> Result<String, String> {
    let path = artifact_path(request, file_name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create model worker artifact directory '{}': {error}",
                parent.display()
            )
        })?;
    }
    let file = File::create(&path).map_err(|error| {
        format!(
            "failed to create model worker artifact '{}': {error}",
            path.display()
        )
    })?;
    serde_json::to_writer_pretty(file, value).map_err(|error| {
        format!(
            "failed to write model worker artifact '{}': {error}",
            path.display()
        )
    })?;
    Ok(artifact_relative_path(request, file_name))
}

fn write_sim_trace_artifact(
    request: &ModelWorkerRequest,
    result: &SimResult,
) -> Result<String, String> {
    let trace_path = artifact_path(request, "sim-trace.json");
    if let Some(parent) = trace_path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            format!(
                "failed to create simulation trace directory '{}': {error}",
                parent.display()
            )
        })?;
    }
    let trace = SimTraceArtifact {
        model_name: request.model_name.clone(),
        n_states: result.n_states,
        times: result.times.clone(),
        names: result.names.clone(),
        data: result.data.clone(),
        variable_meta: (!result.variable_meta.is_empty()).then(|| {
            result
                .variable_meta
                .iter()
                .map(|meta| SimTraceVariableMetaArtifact {
                    name: meta.name.clone(),
                    role: Some(meta.role.clone()),
                    value_type: meta.value_type.clone(),
                    variability: meta.variability.clone(),
                    time_domain: meta.time_domain.clone(),
                })
                .collect()
        }),
    };
    let file = File::create(&trace_path).map_err(|error| {
        format!(
            "failed to create simulation trace '{}': {error}",
            trace_path.display()
        )
    })?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, &trace).map_err(|error| {
        format!(
            "failed to serialize simulation trace '{}': {error}",
            trace_path.display()
        )
    })?;
    writer.write_all(b"\n").map_err(|error| {
        format!(
            "failed to finalize simulation trace '{}': {error}",
            trace_path.display()
        )
    })?;
    Ok(artifact_relative_path(request, "sim-trace.json"))
}

fn remove_stale_stage_artifacts(request: &ModelWorkerRequest) {
    for file_name in [
        "ir-ast.json",
        "ir-flat.json",
        "ir-dae.json",
        "ir-solve.json",
        "sim-trace.json",
    ] {
        let _ = fs::remove_file(artifact_path(request, file_name));
    }
}

fn write_ast_artifact(
    session: &mut Session,
    request: &ModelWorkerRequest,
    row: &mut WorkerModelResult,
) {
    #[derive(serde::Serialize)]
    struct AstModelArtifact<'a> {
        model_name: &'a str,
        class: &'a rumoca_compile::parsing::ClassDef,
    }

    let tree = match session.tree() {
        Ok(tree) => tree,
        Err(error) => {
            row.ir_solve_error = Some(format!("failed to write ir-ast.json: {error}"));
            return;
        }
    };
    let Some(class) = tree.get_class_by_qualified_name(&request.model_name) else {
        row.ir_solve_error = Some(format!(
            "failed to write ir-ast.json: model '{}' not found in AST",
            request.model_name
        ));
        return;
    };
    let artifact = AstModelArtifact {
        model_name: &request.model_name,
        class,
    };
    match write_artifact_json(request, "ir-ast.json", &artifact) {
        Ok(path) => row.ir_ast_file = Some(path),
        Err(error) => row.ir_solve_error = Some(error),
    }
}

fn write_flat_artifact_after_todae_failure(
    session: &mut Session,
    request: &ModelWorkerRequest,
    row: &mut WorkerModelResult,
) {
    if row.phase_reached != "ToDae" {
        return;
    }
    match session.compile_model_flat_strict_reachable_uncached_with_recovery(&request.model_name) {
        Ok(flat) => match write_artifact_json(request, "ir-flat.json", &flat) {
            Ok(path) => row.ir_flat_file = Some(path),
            Err(error) => row.ir_solve_error = Some(error),
        },
        Err(error) => {
            row.ir_solve_error = Some(format!("failed to write ir-flat.json: {error}"));
        }
    }
}

fn phase_failure(
    model_name: &str,
    phase: &str,
    error: impl Into<String>,
    error_code: Option<String>,
) -> WorkerModelResult {
    WorkerModelResult::phase_failure(model_name.to_string(), phase, error, error_code)
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
        .unwrap_or("ToDae")
}

fn summarize_dae_success(
    model_name: &str,
    result: &rumoca_compile::compile::DaeCompilationResult,
    compile_seconds: f64,
) -> WorkerModelResult {
    let detail = &result.balance_detail;
    let (scalar_equations, scalar_unknowns) = detail.equations_unknowns();
    let closure_detail =
        rumoca_compile::analysis::initial_closure_balance_detail(result.dae.as_ref())
            .expect("successful DAE compilation has valid balance metadata");
    let scalar_equations = scalar_equations as i64;
    let scalar_unknowns = scalar_unknowns as i64;
    let scalar_equations_with_init = scalar_equations + closure_detail.closure_used;
    let input_scalars = result
        .dae
        .variables
        .inputs
        .values()
        .map(|v| v.size())
        .sum::<usize>() as i64;
    let balanced_discrete_scalars =
        (detail.discrete_real_unknowns + detail.discrete_valued_unknowns) as i64;
    let extra_discrete_report_scalars =
        (result.active_discrete_scalar_count - balanced_discrete_scalars).max(0);
    let report_offset = input_scalars + extra_discrete_report_scalars;
    let scalar_unknowns_for_report = scalar_unknowns + report_offset;
    let scalar_equations_for_report = scalar_equations_with_init + report_offset;
    let balance_for_report = scalar_equations_for_report - scalar_unknowns_for_report;

    let mut row = WorkerModelResult::phase_failure(model_name.to_string(), "Success", "", None);
    row.error = None;
    row.num_states = Some(result.dae.variables.states.len());
    row.num_algebraics = Some(result.dae.variables.algebraics.len());
    row.num_f_x = Some(result.dae.continuous.equations.len());
    row.balance = Some(balance_for_report);
    row.is_balanced = Some(balance_for_report == 0);
    row.is_partial = Some(result.dae.metadata.is_partial);
    row.class_type = Some(result.dae.metadata.class_type.as_str().to_string());
    row.scalar_equations = usize::try_from(scalar_equations_for_report).ok();
    row.scalar_unknowns = usize::try_from(scalar_unknowns_for_report).ok();
    row.initial_equation_scalars = usize::try_from(closure_detail.initial_equation_scalars).ok();
    row.initial_algorithm_scalars = usize::try_from(closure_detail.initial_algorithm_scalars).ok();
    row.initial_balance_deficit_before = Some(closure_detail.deficit_before);
    row.initial_closure_used = usize::try_from(closure_detail.closure_used).ok();
    row.initial_balance_deficit_after = Some(closure_detail.deficit_after);
    row.initial_balance_ok = Some(closure_detail.deficit_after == 0);
    row.compile_seconds = Some(compile_seconds);
    row
}

fn sim_timeout_secs() -> f64 {
    rumoca_worker::MSL_SIM_TIMEOUT_SECS
}

fn simulation_settings(result: &rumoca_compile::compile::DaeCompilationResult) -> SimSettings {
    let mut t_start = result
        .experiment_start_time
        .filter(|seconds| seconds.is_finite())
        .unwrap_or(0.0);
    let mut t_end = result
        .experiment_stop_time
        .filter(|seconds| seconds.is_finite() && *seconds > t_start)
        .unwrap_or(t_start + DEFAULT_SIM_END_TIME_SECS);
    if !t_start.is_finite() || !t_end.is_finite() || t_end <= t_start {
        t_start = 0.0;
        t_end = DEFAULT_SIM_END_TIME_SECS;
    }
    let tolerance = result
        .experiment_tolerance
        .filter(|value| value.is_finite() && *value > 0.0);
    let solver = result
        .experiment_solver
        .clone()
        .unwrap_or_else(|| "auto".to_string());
    SimSettings {
        t_start,
        t_end,
        dt: result
            .experiment_interval
            .filter(|value| value.is_finite() && *value > 0.0),
        rtol: tolerance,
        atol: tolerance,
        solver,
    }
}

fn root_standalone_example_name(model_name: &str) -> bool {
    if !model_name.starts_with("Modelica.") || !model_name.contains(".Examples.") {
        return false;
    }
    let Some((_, suffix)) = model_name.split_once(".Examples.") else {
        return false;
    };
    let mut segments = rumoca_compile::compile::core::split_path_with_indices(suffix);
    if segments.len() <= 1 {
        return true;
    }
    let _ = segments.pop();
    !segments.iter().any(|segment| {
        matches!(
            *segment,
            "Utilities" | "BaseClasses" | "Internal" | "Interfaces"
        )
    })
}

fn should_simulate(
    request: &ModelWorkerRequest,
    result: &rumoca_compile::compile::DaeCompilationResult,
) -> bool {
    request.run_simulation
        && request.selected_for_simulation
        && !result.dae.metadata.is_partial
        && (request.explicit_sim_target
            || (root_standalone_example_name(&request.model_name)
                && result.dae.variables.inputs.is_empty()
                && !result.has_unbound_fixed_parameters))
}

fn output_samples_for_model(dae: &rumoca_compile::compile::Dae) -> usize {
    let n_state_scalars: usize = dae.variables.states.values().map(|v| v.size()).sum();
    if n_state_scalars == 0 {
        SIM_OUTPUT_SAMPLES_NO_STATES
    } else {
        SIM_OUTPUT_SAMPLES_DEFAULT
    }
}

fn sim_options(settings: &SimSettings, output_samples: usize) -> SimOptions {
    let span = (settings.t_end - settings.t_start).abs();
    let dt = settings
        .dt
        .filter(|value| value.is_finite() && *value > 0.0)
        .or_else(|| (output_samples > 0).then_some((span / output_samples as f64).max(1e-6)));
    let mut opts = SimOptions {
        t_start: settings.t_start,
        t_end: settings.t_end,
        dt,
        max_wall_seconds: Some(sim_timeout_secs()),
        solver_mode: SimSolverMode::from_external_name(&settings.solver),
        ..SimOptions::default()
    };
    if let Some(rtol) = settings.rtol {
        opts.rtol = rtol;
    }
    if let Some(atol) = settings.atol {
        opts.atol = atol;
    }
    opts
}

fn first_non_finite_sample(result: &SimResult) -> Option<(usize, usize, f64)> {
    result.data.iter().enumerate().find_map(|(col_idx, col)| {
        col.iter().enumerate().find_map(|(time_idx, value)| {
            (!value.is_finite()).then_some((col_idx, time_idx, *value))
        })
    })
}

fn classify_success(
    row: &mut WorkerModelResult,
    result: &SimResult,
    elapsed: f64,
    build_timings: BuildSimulationTimings,
    sim_build_seconds: f64,
    sim_run_seconds: f64,
    ic_seconds: f64,
) {
    if let Some((col_idx, time_idx, value)) = first_non_finite_sample(result) {
        let var_name = result
            .names
            .get(col_idx)
            .cloned()
            .unwrap_or_else(|| format!("col[{col_idx}]"));
        let t = result.times.get(time_idx).copied().unwrap_or(0.0);
        row.sim_status = Some("sim_nan".to_string());
        row.sim_error = Some(format!(
            "NaN/Inf in output at {var_name} (index {col_idx}) t={t} value={value}"
        ));
    } else {
        row.sim_status = Some("sim_ok".to_string());
    }
    row.ic_status = Some("ic_ok".to_string());
    row.ic_seconds = Some(ic_seconds);
    row.sim_seconds = Some(elapsed);
    row.sim_build_seconds = Some(sim_build_seconds);
    row.ir_solve_seconds = Some(build_timings.ir_solve_seconds);
    row.ir_solve_structural_dae_seconds = Some(build_timings.ir_solve_structural_dae_seconds);
    row.ir_solve_lower_seconds = Some(build_timings.ir_solve_lower_seconds);
    row.sim_backend_build_seconds = Some(build_timings.backend_build_seconds);
    row.sim_run_seconds = Some(sim_run_seconds);
    row.sim_wall_seconds = Some(elapsed);
}

fn classify_sim_error(
    row: &mut WorkerModelResult,
    err: SimError,
    elapsed: f64,
    build_timings: BuildSimulationTimings,
    sim_build_seconds: f64,
    phase: WorkerErrorPhase,
) {
    row.sim_status = Some(
        match err {
            SimError::Timeout { .. } => "sim_timeout",
            _ => "sim_solver_fail",
        }
        .to_string(),
    );
    let source_span = None;
    row.sim_error = Some(err.to_string());
    row.sim_error_span = source_span;
    row.sim_seconds = Some(elapsed);
    row.sim_build_seconds = Some(sim_build_seconds);
    row.ir_solve_seconds = Some(build_timings.ir_solve_seconds);
    row.ir_solve_structural_dae_seconds = Some(build_timings.ir_solve_structural_dae_seconds);
    row.ir_solve_lower_seconds = Some(build_timings.ir_solve_lower_seconds);
    row.sim_backend_build_seconds = Some(build_timings.backend_build_seconds);
    row.sim_run_seconds = Some(match phase {
        WorkerErrorPhase::Simulation { sim_run_seconds } => sim_run_seconds,
        WorkerErrorPhase::Build
        | WorkerErrorPhase::SimBuild
        | WorkerErrorPhase::Initialization { .. } => 0.0,
    });
    row.sim_wall_seconds = Some(elapsed);
    match phase {
        WorkerErrorPhase::Build | WorkerErrorPhase::SimBuild => {}
        WorkerErrorPhase::Initialization { ic_seconds } => {
            row.ic_status = Some("ic_solver_fail".to_string());
            row.ic_error = row.sim_error.clone();
            row.ic_error_span = source_span;
            row.ic_seconds = Some(ic_seconds);
        }
        WorkerErrorPhase::Simulation { .. } => {
            row.ic_status = Some("ic_ok".to_string());
        }
    }
}

fn run_simulation_pipeline(
    dae: &rumoca_compile::compile::Dae,
    opts: &SimOptions,
    progress: &ProgressLog,
    request: &ModelWorkerRequest,
) -> Result<WorkerRunOk, Box<WorkerRunErr>> {
    progress.event(WorkerProgressPhase::Solve, WorkerProgressEventKind::Started);
    let build = build_worker_prepared_simulation(dae, opts, progress, request)?;
    if build.sim_build_started {
        progress.event(
            WorkerProgressPhase::SimBuild,
            WorkerProgressEventKind::Completed,
        );
    } else if !build.solve_completed {
        progress.event(
            WorkerProgressPhase::Solve,
            WorkerProgressEventKind::Completed,
        );
    }

    progress.event(WorkerProgressPhase::IC, WorkerProgressEventKind::Started);
    let ic_started = Instant::now();
    check_prepared_initialization(&build.prepared).map_err(|err| {
        Box::new(WorkerRunErr {
            err,
            build_timings: build.build_timings,
            sim_build_seconds: build.sim_build_seconds,
            phase: WorkerErrorPhase::Initialization {
                ic_seconds: ic_started.elapsed().as_secs_f64(),
            },
            solve_file: build.solve_file.clone(),
            solve_error: build.solve_error.clone(),
        })
    })?;
    let ic_seconds = ic_started.elapsed().as_secs_f64();
    progress.event(WorkerProgressPhase::IC, WorkerProgressEventKind::Completed);

    progress.event(WorkerProgressPhase::Sim, WorkerProgressEventKind::Started);
    let run_started = Instant::now();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run_prepared_simulation(&build.prepared)
    }));
    let sim_run_seconds = run_started.elapsed().as_secs_f64();
    match result {
        Ok(Ok(result)) => {
            progress.event(WorkerProgressPhase::Sim, WorkerProgressEventKind::Completed);
            Ok(WorkerRunOk {
                sim_result: result,
                build_timings: build.build_timings,
                sim_build_seconds: build.sim_build_seconds,
                sim_run_seconds,
                ic_seconds,
                solve_file: build.solve_file.clone(),
                solve_error: build.solve_error.clone(),
            })
        }
        Ok(Err(err)) => Err(Box::new(WorkerRunErr {
            err,
            build_timings: build.build_timings,
            sim_build_seconds: build.sim_build_seconds,
            phase: WorkerErrorPhase::Simulation { sim_run_seconds },
            solve_file: build.solve_file,
            solve_error: build.solve_error,
        })),
        Err(panic_info) => Err(Box::new(WorkerRunErr {
            err: SimError::SolverError(format!(
                "panic during in-process simulation: {}",
                panic_message(panic_info)
            )),
            build_timings: build.build_timings,
            sim_build_seconds: build.sim_build_seconds,
            phase: WorkerErrorPhase::Simulation { sim_run_seconds },
            solve_file: build.solve_file,
            solve_error: build.solve_error,
        })),
    }
}

fn build_worker_prepared_simulation(
    dae: &rumoca_compile::compile::Dae,
    opts: &SimOptions,
    progress: &ProgressLog,
    request: &ModelWorkerRequest,
) -> Result<WorkerPreparedSimulation, Box<WorkerRunErr>> {
    let build_started = Instant::now();
    let mut solve_file = None;
    let mut solve_error = initial_structural_dae_artifact_error(dae, opts, request);
    let solve_completed = Cell::new(false);
    let mut sim_build_started = false;
    let prepared = build_simulation_with_stage_timing_and_solve_model(
        dae,
        opts,
        |stage| {
            observe_simulation_build_stage(
                stage,
                progress,
                &solve_completed,
                &mut sim_build_started,
            );
        },
        |solve_model| {
            observe_solve_model_artifact(
                solve_model,
                progress,
                request,
                &solve_completed,
                &mut solve_file,
                &mut solve_error,
            );
        },
    );
    let sim_build_seconds = build_started.elapsed().as_secs_f64();
    let (prepared, build_timings) = prepared.map_err(|err| {
        Box::new(WorkerRunErr {
            err,
            build_timings: BuildSimulationTimings::default(),
            sim_build_seconds,
            phase: if sim_build_started {
                WorkerErrorPhase::SimBuild
            } else {
                WorkerErrorPhase::Build
            },
            solve_file: solve_file.clone(),
            solve_error: solve_error.clone(),
        })
    })?;
    Ok(WorkerPreparedSimulation {
        prepared,
        build_timings,
        sim_build_seconds,
        solve_file,
        solve_error,
        sim_build_started,
        solve_completed: solve_completed.get(),
    })
}

fn initial_structural_dae_artifact_error(
    dae: &rumoca_compile::compile::Dae,
    opts: &SimOptions,
    request: &ModelWorkerRequest,
) -> Option<String> {
    if !request.emit_json && !request.emit_modelica {
        return None;
    }
    match structurally_lowered_dae_for_simulation_artifact(dae, opts) {
        Ok(structural_dae) => {
            let mut error = None;
            if request.emit_modelica {
                error = error.or(write_modelica_dae_artifact(
                    request,
                    "ir-structural-dae.mo",
                    &structural_dae,
                )
                .err());
            }
            if request.emit_json {
                error = error.or(write_artifact_json(
                    request,
                    "ir-structural-dae.json",
                    &structural_dae,
                )
                .err());
            }
            error
        }
        Err(error) => Some(error.to_string()),
    }
}

fn observe_simulation_build_stage(
    stage: &str,
    progress: &ProgressLog,
    solve_completed: &Cell<bool>,
    sim_build_started: &mut bool,
) {
    if stage != "sim_build" {
        progress.event(WorkerProgressPhase::Solve, WorkerProgressEventKind::Started);
        return;
    }
    if !solve_completed.get() {
        progress.event(
            WorkerProgressPhase::Solve,
            WorkerProgressEventKind::Completed,
        );
        solve_completed.set(true);
    }
    progress.event(
        WorkerProgressPhase::SimBuild,
        WorkerProgressEventKind::Started,
    );
    *sim_build_started = true;
}

fn observe_solve_model_artifact<T: serde::Serialize>(
    solve_model: &T,
    progress: &ProgressLog,
    request: &ModelWorkerRequest,
    solve_completed: &Cell<bool>,
    solve_file: &mut Option<String>,
    solve_error: &mut Option<String>,
) {
    if !request.emit_json {
        return;
    }
    if !solve_completed.get() {
        progress.event(
            WorkerProgressPhase::Solve,
            WorkerProgressEventKind::Completed,
        );
        solve_completed.set(true);
    }
    progress.event(
        WorkerProgressPhase::ArtifactWrite,
        WorkerProgressEventKind::Started,
    );
    match write_artifact_json(request, "ir-solve.json", solve_model) {
        Ok(path) => *solve_file = Some(path),
        Err(error) => *solve_error = Some(error),
    }
    progress.event(
        WorkerProgressPhase::ArtifactWrite,
        WorkerProgressEventKind::Completed,
    );
}

fn run_model_request(session: &mut Session, request: &ModelWorkerRequest) -> WorkerModelResult {
    remove_stale_stage_artifacts(request);
    let progress = ProgressLog::append(
        &request.model_name,
        artifact_path(request, "progress.jsonl"),
    );
    progress.memory("before_model");
    progress.event(
        WorkerProgressPhase::Compile,
        WorkerProgressEventKind::Started,
    );
    let compile_start = Instant::now();
    let phase_progress = progress.clone();
    let phase_timer = Rc::new(RefCell::new(CompilePhaseTimer::default()));
    let observer_phase_timer = Rc::clone(&phase_timer);
    let _compile_phase_observer = install_compile_phase_observer(move |phase, event| {
        observer_phase_timer.borrow_mut().observe(phase, event);
        phase_progress.compile_phase_event(phase, event)
    });
    let compile_result = if request.allow_unbalanced_for_diagnostics {
        session
            .compile_model_dae_allow_unbalanced_for_diagnostics(&request.model_name)
            .map_err(|error| error.to_string())
    } else {
        session.compile_model_dae_strict_reachable_uncached_with_recovery(&request.model_name)
    };
    drop(_compile_phase_observer);
    let compile_seconds = compile_start.elapsed().as_secs_f64();
    let result = match compile_result {
        Ok(result) => result,
        Err(summary) => {
            let mut row = phase_failure(
                &request.model_name,
                strict_dae_failure_phase(&summary),
                summary,
                None,
            );
            row.compile_seconds = Some(compile_seconds);
            apply_compile_phase_durations(&mut row, phase_timer.borrow().durations());
            progress.memory("after_compile_failure");
            write_compile_artifacts(session, request, &mut row, None, &progress);
            progress.event(
                WorkerProgressPhase::Compile,
                WorkerProgressEventKind::Failed,
            );
            return row;
        }
    };
    progress.event(
        WorkerProgressPhase::Compile,
        WorkerProgressEventKind::Completed,
    );
    progress.memory("after_compile_success");

    let mut row = summarize_dae_success(&request.model_name, &result, compile_seconds);
    apply_compile_phase_durations(&mut row, phase_timer.borrow().durations());
    write_partial_compile_success(request, &row, compile_seconds);
    write_compile_artifacts(session, request, &mut row, Some(&result), &progress);
    progress.memory("after_artifact_write");
    if !should_simulate(request, &result) {
        return row;
    }

    if is_trivial_static_dae(&result) {
        mark_trivial_static_success(&mut row);
        progress.memory("after_trivial_static");
        return row;
    }

    let settings = simulation_settings(&result);
    let opts = sim_options(&settings, output_samples_for_model(result.dae.as_ref()));
    run_and_classify_simulation(&mut row, request, &result, &opts, &progress);
    row
}

fn is_trivial_static_dae(result: &DaeCompilationResult) -> bool {
    total_dae_unknowns(result) == 0
        && result.dae.continuous.equations.is_empty()
        && result.dae.discrete.real_updates.is_empty()
        && result.dae.discrete.valued_updates.is_empty()
        && result.dae.conditions.equations.is_empty()
        && result.dae.conditions.relations.is_empty()
        && result.dae.initialization.equations.is_empty()
}

fn total_dae_unknowns(result: &DaeCompilationResult) -> usize {
    result
        .dae
        .variables
        .states
        .values()
        .map(|v| v.size())
        .sum::<usize>()
        + result
            .dae
            .variables
            .algebraics
            .values()
            .map(|v| v.size())
            .sum::<usize>()
        + result
            .dae
            .variables
            .outputs
            .values()
            .map(|v| v.size())
            .sum::<usize>()
}

fn mark_trivial_static_success(row: &mut WorkerModelResult) {
    row.sim_status = Some("sim_ok".to_string());
    row.ic_status = Some("ic_ok".to_string());
    row.ic_seconds = Some(0.0);
    row.sim_seconds = Some(0.0);
    row.sim_build_seconds = Some(0.0);
    row.ir_solve_seconds = Some(0.0);
    row.ir_solve_structural_dae_seconds = Some(0.0);
    row.ir_solve_lower_seconds = Some(0.0);
    row.sim_backend_build_seconds = Some(0.0);
    row.sim_run_seconds = Some(0.0);
    row.sim_wall_seconds = Some(0.0);
}

fn run_and_classify_simulation(
    row: &mut WorkerModelResult,
    request: &ModelWorkerRequest,
    result: &DaeCompilationResult,
    opts: &SimOptions,
    progress: &ProgressLog,
) {
    let sim_start = Instant::now();
    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run_simulation_pipeline(result.dae.as_ref(), opts, progress, request)
    }));
    let elapsed = sim_start.elapsed().as_secs_f64();
    match outcome {
        Ok(Ok(run)) => {
            row.ir_solve_file = run.solve_file;
            row.ir_solve_error = run.solve_error;
            classify_success(
                row,
                &run.sim_result,
                elapsed,
                run.build_timings,
                run.sim_build_seconds,
                run.sim_run_seconds,
                run.ic_seconds,
            );
            if row.sim_status.as_deref() == Some("sim_ok") {
                match write_sim_trace_artifact(request, &run.sim_result) {
                    Ok(path) => row.sim_trace_file = Some(path),
                    Err(error) => row.sim_trace_error = Some(error),
                }
            }
        }
        Ok(Err(run_err)) => {
            row.ir_solve_file = run_err.solve_file;
            row.ir_solve_error = run_err.solve_error;
            classify_sim_error(
                row,
                run_err.err,
                elapsed,
                run_err.build_timings,
                run_err.sim_build_seconds,
                run_err.phase,
            );
        }
        Err(panic_info) => {
            row.sim_status = Some("sim_solver_fail".to_string());
            row.sim_error = Some(format!(
                "panic during in-process simulation: {}",
                panic_message(panic_info)
            ));
            row.sim_seconds = Some(elapsed);
            row.sim_wall_seconds = Some(elapsed);
        }
    }
    progress.memory("after_simulation");
}

fn write_partial_compile_success(
    request: &ModelWorkerRequest,
    row: &WorkerModelResult,
    elapsed_secs: f64,
) {
    let response = ModelWorkerResponse {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        elapsed_secs,
        result: row.clone(),
    };
    let _ = write_model_worker_response_file(
        &request.output_dir.join(MODEL_WORKER_PARTIAL_RESULT_FILE),
        &response,
    );
}

fn write_compile_artifacts(
    session: &mut Session,
    request: &ModelWorkerRequest,
    row: &mut WorkerModelResult,
    result: Option<&rumoca_compile::compile::DaeCompilationResult>,
    progress: &ProgressLog,
) {
    if !request.emit_json && !request.emit_modelica {
        return;
    }
    progress.event(
        WorkerProgressPhase::ArtifactWrite,
        WorkerProgressEventKind::Started,
    );
    if request.emit_json {
        write_ast_artifact(session, request, row);
    }
    let Some(result) = result else {
        if request.emit_json {
            write_flat_artifact_after_todae_failure(session, request, row);
        }
        progress.event(
            WorkerProgressPhase::ArtifactWrite,
            WorkerProgressEventKind::Completed,
        );
        return;
    };
    if request.emit_modelica {
        if let Err(error) =
            write_modelica_flat_artifact(request, "ir-flat.mo", result.flat.as_ref())
        {
            row.ir_solve_error = Some(error);
        }
        if let Err(error) = write_modelica_dae_artifact(request, "ir-dae.mo", result.dae.as_ref()) {
            row.ir_solve_error = Some(error);
        }
    }
    if request.emit_json {
        match write_artifact_json(request, "ir-flat.json", result.flat.as_ref()) {
            Ok(path) => row.ir_flat_file = Some(path),
            Err(error) => row.ir_solve_error = Some(error),
        }
        match write_artifact_json(request, "ir-dae.json", result.dae.as_ref()) {
            Ok(path) => row.ir_dae_file = Some(path),
            Err(error) => row.ir_solve_error = Some(error),
        }
    }
    progress.event(
        WorkerProgressPhase::ArtifactWrite,
        WorkerProgressEventKind::Completed,
    );
}

fn compile_request(session: &mut Session, request: ModelWorkerRequest) -> ModelWorkerResponse {
    rumoca_sim::nan_trace::set_nan_trace(request.nan_trace);
    let start = Instant::now();
    let result = run_model_request(session, &request);
    let elapsed_secs = start.elapsed().as_secs_f64();
    ModelWorkerResponse {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        elapsed_secs,
        result,
    }
}

fn worker_stack_size_bytes() -> usize {
    DEFAULT_WORKER_STACK_MB.saturating_mul(1024 * 1024)
}

fn run_worker(args: Args) -> Result<(), String> {
    let request_json = args
        .request_json
        .as_deref()
        .ok_or_else(|| "--request-json is required for one-shot worker mode".to_string())?;
    let request = read_model_worker_request_file(request_json)?;
    if request.protocol_version != MODEL_WORKER_PROTOCOL_VERSION {
        return Err(format!(
            "unsupported model worker protocol {}; expected {}",
            request.protocol_version, MODEL_WORKER_PROTOCOL_VERSION
        ));
    }
    fs::create_dir_all(&request.output_dir).map_err(|error| {
        format!(
            "failed to create model worker output directory '{}': {error}",
            request.output_dir.display()
        )
    })?;
    let _ = fs::remove_file(request.output_dir.join(MODEL_WORKER_RESULT_FILE));
    let _ = fs::remove_file(request.output_dir.join(MODEL_WORKER_PARTIAL_RESULT_FILE));
    let progress = ProgressLog::new(
        &request.model_name,
        artifact_path(&request, "progress.jsonl"),
    );
    progress.event(
        WorkerProgressPhase::SourceRootLoad,
        WorkerProgressEventKind::Started,
    );
    let mut session = load_source_root(&request.source_root_path)?;
    progress.event(
        WorkerProgressPhase::SourceRootLoad,
        WorkerProgressEventKind::Completed,
    );
    let response = compile_request(&mut session, request.clone());
    write_model_worker_response_file(
        &request.output_dir.join(MODEL_WORKER_RESULT_FILE),
        &response,
    )?;
    Ok(())
}

fn write_control_message(message: &ModelWorkerControlMessage) -> Result<(), String> {
    let mut stdout = std::io::stdout().lock();
    serde_json::to_writer(&mut stdout, message)
        .map_err(|error| format!("failed to write model worker control message: {error}"))?;
    writeln!(stdout)
        .map_err(|error| format!("failed to flush model worker control message: {error}"))
}

fn run_worker_daemon(source_root_path: &Path) -> Result<(), String> {
    let mut session = load_source_root(source_root_path)?;
    write_control_message(&ModelWorkerControlMessage::Ready {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
    })?;
    for line in std::io::stdin().lock().lines() {
        let line = line.map_err(|error| format!("failed to read model worker command: {error}"))?;
        if line.trim().is_empty() {
            continue;
        }
        match serde_json::from_str::<ModelWorkerCommand>(&line)
            .map_err(|error| format!("failed to parse model worker command: {error}"))?
        {
            ModelWorkerCommand::Run { request } => {
                let _ = fs::remove_file(artifact_path(&request, "progress.jsonl"));
                let _ = fs::remove_file(request.output_dir.join(MODEL_WORKER_RESULT_FILE));
                let _ = fs::remove_file(request.output_dir.join(MODEL_WORKER_PARTIAL_RESULT_FILE));
                let response = compile_request(&mut session, request.clone());
                write_model_worker_response_file(
                    &request.output_dir.join(MODEL_WORKER_RESULT_FILE),
                    &response,
                )?;
                write_control_message(&ModelWorkerControlMessage::Result {
                    response: Box::new(response),
                })?;
            }
            ModelWorkerCommand::Shutdown => return Ok(()),
        }
    }
    Ok(())
}

fn run_worker_entry(args: Args) -> Result<(), String> {
    if let Some(cpu_core_id) = args.cpu_core_id
        && let Err(error) = pin_current_thread_to_cpu_core(cpu_core_id)
    {
        eprintln!("warning: {error}; continuing without CPU pinning");
    }
    match (args.request_json.as_ref(), args.source_root_path.as_ref()) {
        (Some(_), None) => run_worker(args),
        (None, Some(source_root_path)) => run_worker_daemon(source_root_path),
        (Some(_), Some(_)) => Err("use either --request-json or --source-root-path".to_string()),
        (None, None) => Err("missing --request-json or --source-root-path".to_string()),
    }
}

fn main() {
    let args = Args::parse();
    if let Some(jobs) = args.jobs {
        rumoca_compile::parallelism::set_compiler_parallelism(jobs);
    }
    let result = std::thread::Builder::new()
        .name("rumoca-worker-main".to_string())
        .stack_size(worker_stack_size_bytes())
        .spawn(move || run_worker_entry(args))
        .map_err(|error| format!("failed to spawn worker thread: {error}"))
        .and_then(|handle| match handle.join() {
            Ok(result) => result,
            Err(panic_info) => Err(format!("worker panic: {}", panic_message(panic_info))),
        });
    if let Err(error) = result {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
