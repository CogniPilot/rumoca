#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::time::Instant;

use clap::Parser;
use rumoca_compile::compile::Dae;
use rumoca_sim::{
    BuildSimulationTimings, SimError, build_simulation_with_stage_timing_and_solve_model,
    check_prepared_initialization, run_prepared_simulation,
};
use rumoca_sim::{SimOptions, SimResult, SimSolverMode};

const DEFAULT_WORKER_STACK_MB: usize = 64;

#[derive(Debug, Parser)]
#[command(name = "rumoca-sim-worker")]
#[command(about = "Isolated simulation worker for MSL test harness")]
struct Args {
    #[arg(
        long,
        required_unless_present = "dae_stdin",
        conflicts_with = "dae_stdin"
    )]
    dae_json: Option<PathBuf>,
    #[arg(long)]
    dae_stdin: bool,
    #[arg(long)]
    result_json: PathBuf,
    #[arg(long)]
    model_name: String,
    #[arg(long, default_value_t = 0.0)]
    t_start: f64,
    #[arg(long, default_value_t = 1.0)]
    t_end: f64,
    #[arg(long)]
    dt: Option<f64>,
    #[arg(long)]
    rtol: Option<f64>,
    #[arg(long)]
    atol: Option<f64>,
    /// Solver selection hint. Accepts rumoca (`auto`, `bdf`, `rk`) and
    /// common OpenModelica/Dymola names (`dassl`, `ida`, `rungekutta`, etc).
    #[arg(long, default_value = "auto")]
    solver: String,
    #[arg(long, default_value_t = 100)]
    output_samples: usize,
    #[arg(long, default_value_t = 10.0)]
    timeout_seconds: f64,
    #[arg(long, default_value_t = 10.0)]
    solve_timeout_seconds: f64,
    /// Optional path for a per-model simulation trace JSON artifact.
    #[arg(long)]
    trace_json: Option<PathBuf>,
    #[arg(long)]
    solve_ir_json: Option<PathBuf>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SimWorkerResult {
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

fn sample_series_value(result: &SimResult, series_name: &str, time_idx: usize) -> Option<f64> {
    let col_idx = result.names.iter().position(|name| name == series_name)?;
    result
        .data
        .get(col_idx)
        .and_then(|col| col.get(time_idx))
        .copied()
}

fn push_named_value_detail(
    result: &SimResult,
    details: &mut Vec<String>,
    series_name: &str,
    time_idx: usize,
) {
    if let Some(series_value) = sample_series_value(result, series_name, time_idx) {
        details.push(format!("{series_name}={series_value}"));
    }
}

fn sim_worker_result(
    status: &str,
    error: Option<String>,
    sim_seconds: f64,
    sim_build_seconds: f64,
    sim_run_seconds: f64,
) -> SimWorkerResult {
    SimWorkerResult {
        status: status.to_string(),
        error,
        ic_status: None,
        ic_error: None,
        ic_seconds: None,
        sim_seconds,
        sim_build_seconds,
        ir_solve_seconds: 0.0,
        ir_solve_structural_dae_seconds: 0.0,
        ir_solve_lower_seconds: 0.0,
        sim_backend_build_seconds: 0.0,
        sim_run_seconds,
        trace_file: None,
        trace_error: None,
        solve_ir_file: None,
        solve_ir_error: None,
    }
}

fn with_build_timings(
    mut result: SimWorkerResult,
    build_timings: BuildSimulationTimings,
) -> SimWorkerResult {
    result.ir_solve_seconds = build_timings.ir_solve_seconds;
    result.ir_solve_structural_dae_seconds = build_timings.ir_solve_structural_dae_seconds;
    result.ir_solve_lower_seconds = build_timings.ir_solve_lower_seconds;
    result.sim_backend_build_seconds = build_timings.backend_build_seconds;
    result
}

#[derive(Debug, Clone)]
struct ActiveWorkerStage {
    name: String,
    started_at: Instant,
    timeout_seconds: f64,
}

#[derive(Clone)]
struct WorkerStageWatchdog {
    active_stage: Arc<Mutex<Option<ActiveWorkerStage>>>,
}

struct WorkerStageWatchdogHandle {
    active_stage: Arc<Mutex<Option<ActiveWorkerStage>>>,
    done: Arc<AtomicBool>,
    worker: Option<std::thread::JoinHandle<()>>,
}

impl WorkerStageWatchdogHandle {
    fn start(result_json: PathBuf) -> (Self, WorkerStageWatchdog) {
        let active_stage = Arc::new(Mutex::new(None));
        let done = Arc::new(AtomicBool::new(false));
        let worker_active_stage = Arc::clone(&active_stage);
        let worker_done = Arc::clone(&done);
        let worker = std::thread::spawn(move || {
            run_worker_stage_watchdog_loop(worker_active_stage, worker_done, result_json);
        });
        (
            Self {
                active_stage: Arc::clone(&active_stage),
                done,
                worker: Some(worker),
            },
            WorkerStageWatchdog { active_stage },
        )
    }
}

impl Drop for WorkerStageWatchdogHandle {
    fn drop(&mut self) {
        self.done.store(true, Ordering::Relaxed);
        if let Some(worker) = self.worker.take() {
            let _ = worker.join();
        }
        if let Ok(mut active_stage) = self.active_stage.lock() {
            *active_stage = None;
        }
    }
}

impl WorkerStageWatchdog {
    fn enter(&self, name: impl Into<String>, timeout_seconds: f64) {
        let mut active_stage = self
            .active_stage
            .lock()
            .expect("worker stage watchdog mutex should not be poisoned");
        *active_stage = Some(ActiveWorkerStage {
            name: name.into(),
            started_at: Instant::now(),
            timeout_seconds,
        });
    }

    fn clear(&self) {
        let mut active_stage = self
            .active_stage
            .lock()
            .expect("worker stage watchdog mutex should not be poisoned");
        *active_stage = None;
    }
}

fn run_worker_stage_watchdog_loop(
    active_stage: Arc<Mutex<Option<ActiveWorkerStage>>>,
    done: Arc<AtomicBool>,
    result_json: PathBuf,
) {
    while !done.load(Ordering::Relaxed) {
        let timed_out = {
            let active_stage = active_stage
                .lock()
                .expect("worker stage watchdog mutex should not be poisoned");
            active_stage.as_ref().and_then(|stage| {
                let elapsed = stage.started_at.elapsed().as_secs_f64();
                (elapsed >= stage.timeout_seconds).then(|| (stage.clone(), elapsed))
            })
        };
        if let Some((stage, elapsed)) = timed_out {
            let mut result = sim_worker_result(
                "sim_timeout",
                Some(format!(
                    "{} timeout after {:.3}s (limit {:.3}s)",
                    stage.name, elapsed, stage.timeout_seconds
                )),
                elapsed,
                elapsed,
                0.0,
            );
            if stage.name.starts_with("ir_solve") {
                result.ir_solve_seconds = elapsed;
            } else {
                result.sim_run_seconds = elapsed;
            }
            let _ = write_result(&result_json, &result);
            std::process::exit(0);
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}

fn first_non_finite_sample(result: &SimResult) -> Option<(usize, usize, f64)> {
    result.data.iter().enumerate().find_map(|(col_idx, col)| {
        col.iter()
            .enumerate()
            .find_map(|(time_idx, v)| (!v.is_finite()).then_some((col_idx, time_idx, *v)))
    })
}

fn classify_success(
    result: &SimResult,
    elapsed: f64,
    sim_build_seconds: f64,
    sim_run_seconds: f64,
) -> SimWorkerResult {
    let first_non_finite = first_non_finite_sample(result);
    if let Some((col_idx, time_idx, value)) = first_non_finite {
        let var_name = result
            .names
            .get(col_idx)
            .cloned()
            .unwrap_or_else(|| format!("col[{col_idx}]"));
        let t = result.times.get(time_idx).copied().unwrap_or(0.0);
        let mut details: Vec<String> = Vec::new();

        if let Some(base) = var_name.strip_suffix(".LossPower") {
            let v_name = format!("{base}.v");
            let i_name = format!("{base}.i");
            push_named_value_detail(result, &mut details, &v_name, time_idx);
            push_named_value_detail(result, &mut details, &i_name, time_idx);
        }

        let mut huge_finite: Vec<(String, f64)> = result
            .names
            .iter()
            .enumerate()
            .filter_map(|(idx, name)| {
                let v = result.data.get(idx)?.get(time_idx).copied()?;
                (v.is_finite() && v.abs() > 1.0e100).then_some((name.clone(), v))
            })
            .collect();
        huge_finite.sort_by(|a, b| {
            b.1.abs()
                .partial_cmp(&a.1.abs())
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        if !huge_finite.is_empty() {
            let sample = huge_finite
                .iter()
                .take(3)
                .map(|(name, value)| format!("{name}={value}"))
                .collect::<Vec<_>>()
                .join(", ");
            details.push(format!("huge_finite=[{sample}]"));
        }

        let detail_suffix = if details.is_empty() {
            String::new()
        } else {
            format!(" ({})", details.join("; "))
        };

        return sim_worker_result(
            "sim_nan",
            Some(format!(
                "NaN/Inf in output at {} (index {}) t={} value={}{}",
                var_name, col_idx, t, value, detail_suffix
            )),
            elapsed,
            sim_build_seconds,
            sim_run_seconds,
        );
    }

    sim_worker_result("sim_ok", None, elapsed, sim_build_seconds, sim_run_seconds)
}

fn classify_solver_error(
    err: SimError,
    elapsed: f64,
    sim_build_seconds: f64,
    sim_run_seconds: f64,
) -> SimWorkerResult {
    match err {
        SimError::Timeout { seconds } => sim_worker_result(
            "sim_timeout",
            Some(format!("timeout after {:.3}s", seconds)),
            elapsed,
            sim_build_seconds,
            sim_run_seconds,
        ),
        other => sim_worker_result(
            "sim_solver_fail",
            Some(other.to_string()),
            elapsed,
            sim_build_seconds,
            sim_run_seconds,
        ),
    }
}

fn write_trace_json(trace_path: &Path, model_name: &str, result: &SimResult) -> Result<(), String> {
    if let Some(parent) = trace_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            format!(
                "failed to create trace directory '{}': {e}",
                parent.display()
            )
        })?;
    }

    let trace = SimTraceArtifact {
        model_name: model_name.to_string(),
        n_states: result.n_states,
        times: result.times.clone(),
        names: result.names.clone(),
        data: result.data.clone(),
        variable_meta: if result.variable_meta.is_empty() {
            None
        } else {
            Some(
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
                    .collect(),
            )
        },
    };

    let file = File::create(trace_path).map_err(|e| {
        format!(
            "failed to create trace file '{}': {e}",
            trace_path.display()
        )
    })?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, &trace).map_err(|e| {
        format!(
            "failed to serialize trace JSON '{}': {e}",
            trace_path.display()
        )
    })?;
    writer.write_all(b"\n").map_err(|e| {
        format!(
            "failed to finalize trace JSON '{}': {e}",
            trace_path.display()
        )
    })?;
    Ok(())
}

fn parse_dae_json(mut reader: impl Read, source_label: &str) -> Result<Dae, String> {
    let mut payload = Vec::new();
    reader
        .read_to_end(&mut payload)
        .map_err(|e| format!("failed to read DAE JSON '{source_label}': {e}"))?;

    let source = source_label.to_string();
    let thread_source = source.clone();
    let parser = std::thread::Builder::new()
        .name("rumoca-sim-worker-dae-json-parse".to_string())
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            let mut deserializer = serde_json::Deserializer::from_slice(&payload);
            deserializer.disable_recursion_limit();
            <Dae as serde::Deserialize>::deserialize(&mut deserializer)
                .map_err(|e| format!("failed to parse DAE JSON '{thread_source}': {e}"))
        })
        .map_err(|e| format!("failed to spawn DAE JSON parser thread for '{source}': {e}"))?;

    match parser.join() {
        Ok(result) => result,
        Err(panic_info) => Err(format!(
            "failed to parse DAE JSON '{source}': parser thread panicked: {}",
            panic_message(panic_info)
        )),
    }
}

fn sample_grid_dt(args: &Args) -> Option<f64> {
    if args.output_samples == 0 {
        return None;
    }
    let span = (args.t_end - args.t_start).abs();
    if !span.is_finite() || span <= 0.0 {
        return None;
    }
    Some((span / args.output_samples as f64).max(1e-6))
}

fn effective_output_dt(args: &Args) -> Option<f64> {
    // Honor explicit experiment Interval from the model/test settings.
    // `output_samples` is only a fallback when no interval is provided.
    args.dt
        .filter(|value| value.is_finite() && *value > 0.0)
        .or_else(|| sample_grid_dt(args))
}

fn read_dae_from_args(args: &Args) -> Result<Dae, String> {
    if args.dae_stdin {
        parse_dae_json(BufReader::new(std::io::stdin().lock()), "stdin")
    } else {
        let dae_json = args
            .dae_json
            .as_ref()
            .expect("clap should require dae_json unless dae_stdin is set");
        File::open(dae_json)
            .map_err(|e| format!("failed to open DAE JSON '{}': {e}", dae_json.display()))
            .and_then(|file| parse_dae_json(BufReader::new(file), &dae_json.display().to_string()))
    }
}

fn sim_options_from_args(args: &Args) -> SimOptions {
    let dt = effective_output_dt(args);
    let solver_mode = SimSolverMode::from_external_name(&args.solver);
    let mut opts = SimOptions {
        t_start: args.t_start,
        t_end: args.t_end,
        dt,
        max_wall_seconds: Some(args.timeout_seconds),
        solver_mode,
        ..SimOptions::default()
    };
    if let Some(rtol) = args.rtol.filter(|v| v.is_finite() && *v > 0.0) {
        opts.rtol = rtol;
    }
    if let Some(atol) = args.atol.filter(|v| v.is_finite() && *v > 0.0) {
        opts.atol = atol;
    }
    opts
}

type WorkerRunOk = (SimResult, BuildSimulationTimings, f64, f64, f64);
type WorkerRunErr = (SimError, BuildSimulationTimings, f64, WorkerErrorPhase);

#[derive(Debug, Clone, Copy)]
enum WorkerErrorPhase {
    Build,
    Initialization { ic_seconds: f64 },
    Simulation { sim_run_seconds: f64 },
}

fn write_solve_ir_json(path: &Path, model: &rumoca_ir_solve::SolveModel) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            format!(
                "failed to create Solve IR directory '{}': {e}",
                parent.display()
            )
        })?;
    }
    let file = File::create(path)
        .map_err(|e| format!("failed to create Solve IR JSON '{}': {e}", path.display()))?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, model).map_err(|e| {
        format!(
            "failed to serialize Solve IR JSON '{}': {e}",
            path.display()
        )
    })?;
    writer
        .write_all(b"\n")
        .map_err(|e| format!("failed to finalize Solve IR JSON '{}': {e}", path.display()))
}

fn run_simulation_pipeline(
    dae: &Dae,
    opts: &SimOptions,
    watchdog: &WorkerStageWatchdog,
    solve_timeout_seconds: f64,
    sim_timeout_seconds: f64,
    solve_ir_json: Option<&Path>,
) -> Result<WorkerRunOk, WorkerRunErr> {
    let build_started = Instant::now();
    let mut build_timings = BuildSimulationTimings::default();
    let mut solve_ir_error: Option<String> = None;
    let prepared = build_simulation_with_stage_timing_and_solve_model(
        dae,
        opts,
        |stage| {
            let timeout_seconds = if stage.starts_with("ir_solve") {
                solve_timeout_seconds
            } else {
                sim_timeout_seconds
            };
            watchdog.enter(stage, timeout_seconds);
        },
        |solve_model| {
            if let Some(path) = solve_ir_json
                && let Err(err) = write_solve_ir_json(path, solve_model)
            {
                solve_ir_error = Some(err);
            }
        },
    );
    let sim_build_seconds = build_started.elapsed().as_secs_f64();
    let (prepared, timings) = prepared.map_err(|err| {
        (
            err,
            build_timings,
            sim_build_seconds,
            WorkerErrorPhase::Build,
        )
    })?;
    build_timings = timings;
    if let Some(error) = solve_ir_error {
        return Err((
            SimError::SolveIr(error),
            build_timings,
            sim_build_seconds,
            WorkerErrorPhase::Build,
        ));
    }

    let ic_started = Instant::now();
    watchdog.enter("sim_initialization", sim_timeout_seconds);
    check_prepared_initialization(&prepared).map_err(|err| {
        (
            err,
            build_timings,
            sim_build_seconds,
            WorkerErrorPhase::Initialization {
                ic_seconds: ic_started.elapsed().as_secs_f64(),
            },
        )
    })?;
    let ic_seconds = ic_started.elapsed().as_secs_f64();

    let run_started = Instant::now();
    watchdog.enter("sim", sim_timeout_seconds);
    let result = run_prepared_simulation(&prepared);
    let sim_run_seconds = run_started.elapsed().as_secs_f64();
    watchdog.clear();
    result
        .map(|result| {
            (
                result,
                build_timings,
                sim_build_seconds,
                sim_run_seconds,
                ic_seconds,
            )
        })
        .map_err(|err| {
            (
                err,
                build_timings,
                sim_build_seconds,
                WorkerErrorPhase::Simulation { sim_run_seconds },
            )
        })
}

fn classify_worker_error(
    err: SimError,
    elapsed: f64,
    build_timings: BuildSimulationTimings,
    sim_build_seconds: f64,
    phase: WorkerErrorPhase,
) -> SimWorkerResult {
    let sim_run_seconds = match phase {
        WorkerErrorPhase::Simulation { sim_run_seconds } => sim_run_seconds,
        WorkerErrorPhase::Build | WorkerErrorPhase::Initialization { .. } => 0.0,
    };
    let mut worker_result = with_build_timings(
        classify_solver_error(err, elapsed, sim_build_seconds, sim_run_seconds),
        build_timings,
    );
    match phase {
        WorkerErrorPhase::Build => {}
        WorkerErrorPhase::Initialization { ic_seconds } => {
            worker_result.ic_status = Some("ic_solver_fail".to_string());
            worker_result.ic_error = worker_result.error.clone();
            worker_result.ic_seconds = Some(ic_seconds);
        }
        WorkerErrorPhase::Simulation { .. } => {
            worker_result.ic_status = Some("ic_ok".to_string());
        }
    }
    worker_result
}

fn run(args: &Args) -> SimWorkerResult {
    let (_watchdog_handle, watchdog) = WorkerStageWatchdogHandle::start(args.result_json.clone());
    let dae = match read_dae_from_args(args) {
        Ok(dae) => dae,
        Err(err) => {
            return SimWorkerResult {
                status: "sim_solver_fail".to_string(),
                error: Some(err),
                ic_status: None,
                ic_error: None,
                ic_seconds: None,
                sim_seconds: 0.0,
                sim_build_seconds: 0.0,
                ir_solve_seconds: 0.0,
                ir_solve_structural_dae_seconds: 0.0,
                ir_solve_lower_seconds: 0.0,
                sim_backend_build_seconds: 0.0,
                sim_run_seconds: 0.0,
                trace_file: None,
                trace_error: None,
                solve_ir_file: None,
                solve_ir_error: None,
            };
        }
    };

    let opts = sim_options_from_args(args);

    let sim_start = Instant::now();
    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run_simulation_pipeline(
            &dae,
            &opts,
            &watchdog,
            args.solve_timeout_seconds,
            args.timeout_seconds,
            args.solve_ir_json.as_deref(),
        )
    }));
    watchdog.clear();
    let elapsed = sim_start.elapsed().as_secs_f64();
    match outcome {
        Ok(Ok((result, build_timings, sim_build_seconds, sim_run_seconds, ic_seconds))) => {
            let mut worker_result = with_build_timings(
                classify_success(&result, elapsed, sim_build_seconds, sim_run_seconds),
                build_timings,
            );
            worker_result.ic_status = Some("ic_ok".to_string());
            worker_result.ic_seconds = Some(ic_seconds);
            if worker_result.status == "sim_ok"
                && let Some(trace_path) = args.trace_json.as_deref()
            {
                match write_trace_json(trace_path, &args.model_name, &result) {
                    Ok(()) => {
                        worker_result.trace_file = Some(trace_path.to_string_lossy().to_string());
                    }
                    Err(err) => {
                        worker_result.trace_error = Some(err);
                    }
                }
            }
            if let Some(path) = args.solve_ir_json.as_deref()
                && path.is_file()
            {
                worker_result.solve_ir_file = Some(path.to_string_lossy().to_string());
            }
            worker_result
        }
        Ok(Err((err, build_timings, sim_build_seconds, sim_run_seconds))) => {
            let mut result = classify_worker_error(
                err,
                elapsed,
                build_timings,
                sim_build_seconds,
                sim_run_seconds,
            );
            if let Some(path) = args.solve_ir_json.as_deref()
                && path.is_file()
            {
                result.solve_ir_file = Some(path.to_string_lossy().to_string());
            }
            result
        }
        Err(panic_info) => {
            let mut result = sim_worker_result(
                "sim_solver_fail",
                Some(format!("panic: {}", panic_message(panic_info))),
                elapsed,
                0.0,
                0.0,
            );
            if let Some(path) = args.solve_ir_json.as_deref()
                && path.is_file()
            {
                result.solve_ir_file = Some(path.to_string_lossy().to_string());
            }
            result
        }
    }
}

fn write_result(path: &PathBuf, result: &SimWorkerResult) -> std::io::Result<()> {
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);
    serde_json::to_writer(&mut writer, result).map_err(std::io::Error::other)?;
    writer.write_all(b"\n")?;
    Ok(())
}

fn worker_stack_size_bytes() -> usize {
    DEFAULT_WORKER_STACK_MB.saturating_mul(1024 * 1024)
}

fn run_on_worker_stack(args: Args) -> anyhow::Result<()> {
    let result_json = args.result_json.clone();
    let stack_size = worker_stack_size_bytes();
    let handle = std::thread::Builder::new()
        .name("rumoca-sim-worker-main".to_string())
        .stack_size(stack_size)
        .spawn(move || {
            let result = run(&args);
            write_result(&args.result_json, &result)
        })?;

    match handle.join() {
        Ok(result) => result?,
        Err(panic_info) => {
            let result = sim_worker_result(
                "sim_solver_fail",
                Some(format!("panic: {}", panic_message(panic_info))),
                0.0,
                0.0,
                0.0,
            );
            write_result(&result_json, &result)?;
        }
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    run_on_worker_stack(args)
}

#[cfg(test)]
mod tests {
    use super::{
        Args, WorkerErrorPhase, classify_worker_error, effective_output_dt, parse_dae_json,
        sample_grid_dt,
    };
    use rumoca_compile::compile::{Dae, Session, SessionConfig};
    use rumoca_sim::{BuildSimulationTimings, SimError};
    use serde_json::json;
    use std::path::PathBuf;

    fn deep_add_expr_json(base_add_expr: &serde_json::Value, depth: usize) -> serde_json::Value {
        let mut expr = base_add_expr.clone();
        let add_op = expr
            .get("Binary")
            .and_then(|node| node.get("op"))
            .cloned()
            .expect("base add expression should have Binary.op");
        let rhs_one = base_add_expr
            .get("Binary")
            .and_then(|node| node.get("rhs"))
            .cloned()
            .expect("base add expression should have Binary.rhs");

        for _ in 0..depth {
            expr = json!({
                "Binary": {
                    "op": add_op.clone(),
                    "lhs": expr,
                    "rhs": rhs_one.clone()
                }
            });
        }

        expr
    }

    #[test]
    fn parse_dae_json_handles_deeply_nested_expression_trees() {
        let source = "model DeepBinary\n  Real x(start = 0);\nequation\n  der(x) = 0 + 1;\nend DeepBinary;\n";
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("deep_binary.mo", source)
            .expect("add source file");
        let compiled = session
            .compile_model("DeepBinary")
            .expect("compile deep nested expression model");
        let mut dae_json = serde_json::to_value(&compiled.dae).expect("serialize deep DAE json");
        let base_add_expr = dae_json["f_x"][0]["rhs"]["Binary"]["rhs"].clone();
        dae_json["f_x"][0]["rhs"]["Binary"]["rhs"] = deep_add_expr_json(&base_add_expr, 512);
        let deep_dae: Dae =
            serde_json::from_value(dae_json).expect("deserialize mutated deep DAE value");
        let payload = serde_json::to_vec(&deep_dae).expect("encode deep DAE JSON payload");
        let parsed =
            parse_dae_json(std::io::Cursor::new(payload), "in-memory").expect("parse deep DAE");
        assert_eq!(parsed.continuous.equations.len(), 1);
    }

    fn test_args() -> Args {
        Args {
            dae_json: Some(PathBuf::from("in.json")),
            dae_stdin: false,
            result_json: PathBuf::from("out.json"),
            model_name: "M".to_string(),
            t_start: 0.0,
            t_end: 1.0,
            dt: None,
            rtol: None,
            atol: None,
            solver: "auto".to_string(),
            output_samples: 100,
            timeout_seconds: 10.0,
            solve_timeout_seconds: 10.0,
            trace_json: None,
            solve_ir_json: None,
        }
    }

    #[test]
    fn classify_build_error_does_not_mark_initial_conditions_ok() {
        let result = classify_worker_error(
            SimError::SolveIr("lowering failed".to_string()),
            0.25,
            BuildSimulationTimings::default(),
            0.25,
            WorkerErrorPhase::Build,
        );

        assert_eq!(result.status, "sim_solver_fail");
        assert_eq!(result.ic_status, None);
        assert_eq!(result.ic_seconds, None);
        assert_eq!(result.sim_run_seconds, 0.0);
    }

    #[test]
    fn classify_initialization_error_marks_initial_conditions_failed() {
        let result = classify_worker_error(
            SimError::SolveIr("initial residual failed".to_string()),
            0.5,
            BuildSimulationTimings::default(),
            0.25,
            WorkerErrorPhase::Initialization { ic_seconds: 0.125 },
        );

        assert_eq!(result.status, "sim_solver_fail");
        assert_eq!(result.ic_status.as_deref(), Some("ic_solver_fail"));
        assert_eq!(
            result.ic_error.as_deref(),
            Some("solve-IR evaluation failed: initial residual failed")
        );
        assert_eq!(result.ic_seconds, Some(0.125));
        assert_eq!(result.sim_run_seconds, 0.0);
    }

    #[test]
    fn classify_simulation_error_preserves_initial_conditions_ok() {
        let result = classify_worker_error(
            SimError::SolverError("step failed".to_string()),
            0.75,
            BuildSimulationTimings::default(),
            0.25,
            WorkerErrorPhase::Simulation {
                sim_run_seconds: 0.5,
            },
        );

        assert_eq!(result.status, "sim_solver_fail");
        assert_eq!(result.ic_status.as_deref(), Some("ic_ok"));
        assert_eq!(result.sim_run_seconds, 0.5);
    }

    #[test]
    fn test_sample_grid_dt_uses_span_and_output_samples() {
        let args = test_args();
        let dt = sample_grid_dt(&args).expect("sample dt should be present");
        assert!((dt - 0.01).abs() < 1e-12);
    }

    #[test]
    fn test_effective_output_dt_respects_annotation_interval_even_if_finer_than_sample_grid() {
        let mut args = test_args();
        args.dt = Some(1e-6);
        args.output_samples = 100;
        args.t_start = 0.0;
        args.t_end = 1.0;
        let dt = effective_output_dt(&args).expect("effective dt should be present");
        assert!(
            (dt - 1e-6).abs() < 1e-18,
            "annotation dt should be used directly"
        );
    }

    #[test]
    fn test_effective_output_dt_uses_sample_grid_when_annotation_missing() {
        let args = test_args();
        let dt = effective_output_dt(&args).expect("effective dt should be present");
        assert!((dt - 0.01).abs() < 1e-12);
    }

    #[test]
    fn test_effective_output_dt_keeps_annotation_interval() {
        let mut args = test_args();
        args.dt = Some(0.05);
        args.output_samples = 100;
        args.t_start = 0.0;
        args.t_end = 1.0;
        let dt = effective_output_dt(&args).expect("effective dt should be present");
        assert!((dt - 0.05).abs() < 1e-12);
    }
}
