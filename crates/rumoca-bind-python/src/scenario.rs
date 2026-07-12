//! First-class Python scenario execution for [`crate::session::Session`].
//!
//! This delegates to the same simulation/codegen components used by the CLI.
//! Pacing (`as_fast_as_possible`, `realtime`, `lockstep`), transports, inputs,
//! debug logs, and viewer settings are scenario configuration, not a separate
//! Python API.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use rumoca_compile::compile::Session as CompileSession;
use rumoca_compile::scenario::{ScenarioConfigFile, ScenarioTask, parse_scenario_config_file};
use rumoca_sim::scenario_config::SimulationConfig as ScenarioSimulationConfig;
use rumoca_sim::{
    DiffsolMethod, SimOptions, SimPacingMode, SimResult, SimSolverMode, SimulationRequestSummary,
    SimulationRunMetrics, simulate_with_diagnostics_auto_nan_trace,
};
use serde_json::{Value, json};

use crate::codegen::CodegenResult;
use crate::diagnostics::Diagnostic;
use crate::error::{ApiError, ApiResult};
use crate::result::Result as SimPyResult;
use crate::{
    compile_source_in_session, json_value_to_py, render_target_files, resolve_source_root_paths,
};

#[derive(Default)]
struct ScenarioOverrides {
    t_end: Option<f64>,
    dt: Option<f64>,
    solver: Option<String>,
    mode: Option<SimPacingMode>,
    output: Option<String>,
    output_dir: Option<String>,
    debug_log_path: Option<String>,
}

/// Result returned by `Session.run_scenario`.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct ScenarioResult {
    #[pyo3(get)]
    pub task: String,
    #[pyo3(get)]
    pub status: String,
    #[pyo3(get)]
    pub model: Option<String>,
    #[pyo3(get)]
    pub schedule: Option<String>,
    #[pyo3(get)]
    pub output_paths: Vec<String>,
    #[pyo3(get)]
    pub termination: Option<String>,
    #[pyo3(get)]
    pub diagnostics: Vec<Diagnostic>,
    metrics: Value,
    simulation: Option<SimPyResult>,
    codegen: Option<CodegenResult>,
}

#[pymethods]
impl ScenarioResult {
    #[getter]
    fn metrics(&self, py: Python<'_>) -> PyObject {
        json_value_to_py(py, &self.metrics)
    }

    #[getter]
    fn result(&self) -> Option<SimPyResult> {
        self.simulation.clone()
    }

    #[getter]
    fn codegen(&self) -> Option<CodegenResult> {
        self.codegen.clone()
    }

    fn to_dict(&self, py: Python<'_>) -> PyObject {
        let dict = PyDict::new_bound(py);
        let _ = dict.set_item("task", &self.task);
        let _ = dict.set_item("status", &self.status);
        let _ = dict.set_item("model", &self.model);
        let _ = dict.set_item("schedule", &self.schedule);
        let _ = dict.set_item("output_paths", self.output_paths.clone());
        let _ = dict.set_item("termination", &self.termination);
        let diagnostics = PyList::empty_bound(py);
        for diagnostic in &self.diagnostics {
            if let Ok(diagnostic) = Py::new(py, diagnostic.clone()) {
                let _ = diagnostics.append(diagnostic);
            }
        }
        let _ = dict.set_item("diagnostics", diagnostics);
        let _ = dict.set_item("metrics", json_value_to_py(py, &self.metrics));
        dict.into_py(py)
    }

    fn __repr__(&self) -> String {
        format!(
            "ScenarioResult(task={:?}, status={:?}, model={:?}, outputs={})",
            self.task,
            self.status,
            self.model,
            self.output_paths.len()
        )
    }
}

pub(crate) fn run_scenario_in_session(
    session: &mut CompileSession,
    session_roots: &[String],
    path: &str,
    overrides: Option<&Bound<'_, PyDict>>,
) -> ApiResult<ScenarioResult> {
    let overrides = ScenarioOverrides::from_py(overrides)?;
    let scenario_path = Path::new(path);
    let text = fs::read_to_string(scenario_path)
        .map_err(|e| ApiError::Compile(format!("Failed to read {path}: {e}")))?;
    let config = parse_scenario_config_file(&text)
        .map_err(|e| ApiError::Compile(format!("Scenario config error: {e}")))?;
    match config.rumoca.task {
        ScenarioTask::Codegen => {
            run_codegen_scenario(session, session_roots, scenario_path, config, overrides)
        }
        ScenarioTask::Simulate => {
            run_simulation_scenario(session, session_roots, scenario_path, overrides)
        }
    }
}

pub(crate) fn codegen_file_in_session(
    session: &mut CompileSession,
    session_roots: &[String],
    path: &str,
    model_name: &str,
    target: &str,
    output: &str,
    roots: &[String],
) -> ApiResult<Vec<String>> {
    let source = fs::read_to_string(path)
        .map_err(|e| ApiError::Compile(format!("Failed to read {path}: {e}")))?;
    let roots = merged_code_generation_roots(session_roots, roots);
    let (result, compiled_model) =
        compile_source_in_session(session, &source, Some(model_name), path, &roots)
            .map_err(|e| ApiError::Compile(e.0))?;
    let files = render_target_files(&result, &compiled_model, target)?;
    CodegenResult::new(target.to_string(), files)
        .save_all_to(output)
        .map_err(Into::into)
}

fn run_codegen_scenario(
    session: &mut CompileSession,
    session_roots: &[String],
    scenario_path: &Path,
    config: ScenarioConfigFile,
    overrides: ScenarioOverrides,
) -> ApiResult<ScenarioResult> {
    let base = parent_dir_or_current(scenario_path);
    let model_name = required_string(config.model.name.as_deref(), "[model].name")?;
    let model_file = required_string(config.model.file.as_deref(), "[model].file")?;
    let target = required_string(config.codegen.target.as_deref(), "[codegen].target")?;
    let output_dir = overrides
        .output_dir
        .or(config.codegen.output_dir)
        .unwrap_or_else(|| default_codegen_output_dir(&model_name, &target));
    let model_path = resolve_path(base, &model_file);
    let output_path = resolve_path(base, &output_dir);
    let source = fs::read_to_string(&model_path)
        .map_err(|e| ApiError::Compile(format!("Failed to read {}: {e}", model_path.display())))?;
    let roots = merged_roots(base, session_roots, &config.source_roots);
    let compile_started = Instant::now();
    let (result, compiled_model) = compile_source_in_session(
        session,
        &source,
        Some(&model_name),
        &model_path.to_string_lossy(),
        &roots,
    )
    .map_err(|e| ApiError::Compile(e.0))?;
    let compile_seconds = compile_started.elapsed().as_secs_f64();
    let files = render_target_files(&result, &compiled_model, &target)?;
    let codegen = CodegenResult::new(target.clone(), files);
    let output_dir_string = output_path.to_string_lossy().to_string();
    let written = codegen.save_all_to(&output_dir_string)?;
    Ok(ScenarioResult {
        task: "codegen".to_string(),
        status: "completed".to_string(),
        model: Some(compiled_model),
        schedule: None,
        output_paths: written.clone(),
        termination: None,
        diagnostics: Vec::new(),
        metrics: json!({
            "compile_seconds": compile_seconds,
            "files": written.len(),
            "target": target,
        }),
        simulation: None,
        codegen: Some(codegen),
    })
}

fn run_simulation_scenario(
    session: &mut CompileSession,
    session_roots: &[String],
    scenario_path: &Path,
    overrides: ScenarioOverrides,
) -> ApiResult<ScenarioResult> {
    let base = parent_dir_or_current(scenario_path);
    let mut config = ScenarioSimulationConfig::load(scenario_path)
        .map_err(|e| ApiError::Compile(format!("Load simulation config: {e}")))?;
    apply_simulation_overrides(&mut config, &overrides)?;
    if config.requires_scheduled_loop() {
        return run_scheduled_simulation(config, base);
    }
    run_batch_simulation(session, session_roots, base, config, overrides)
}

fn run_batch_simulation(
    session: &mut CompileSession,
    session_roots: &[String],
    base: &Path,
    config: ScenarioSimulationConfig,
    overrides: ScenarioOverrides,
) -> ApiResult<ScenarioResult> {
    let model_config = config
        .model
        .as_ref()
        .ok_or_else(|| ApiError::Compile("scenario is missing [model]".to_string()))?;
    let model_path = resolve_path(base, &model_config.file);
    let source = fs::read_to_string(&model_path)
        .map_err(|e| ApiError::Compile(format!("Failed to read {}: {e}", model_path.display())))?;
    let roots = merged_roots(base, session_roots, &config.source_roots);
    let compile_started = Instant::now();
    let (result, model_name) = compile_source_in_session(
        session,
        &source,
        Some(&model_config.name),
        &model_path.to_string_lossy(),
        &roots,
    )
    .map_err(|e| ApiError::Compile(e.0))?;
    let compile_seconds = compile_started.elapsed().as_secs_f64();
    let (opts, solver_label) = sim_options_from_config(&config);
    let simulate_started = Instant::now();
    let sim = simulate_with_diagnostics_auto_nan_trace(&result.dae, &opts)
        .map_err(|e| ApiError::Sim(format!("{e}")))?;
    let metrics = SimulationRunMetrics {
        compile_seconds: Some(compile_seconds),
        simulate_seconds: Some(simulate_started.elapsed().as_secs_f64()),
        ..SimulationRunMetrics::default()
    };
    let request = sim_request(&opts, solver_label);
    let output_path =
        simulation_output_path(base, &model_name, config.sim.output.as_deref(), &overrides);
    let written =
        write_simulation_output(&sim, &model_name, &output_path, &request, &metrics, base)?;
    let termination = sim.termination.as_ref().map(|t| t.message.clone());
    Ok(ScenarioResult {
        task: "simulate".to_string(),
        status: "completed".to_string(),
        model: Some(model_name.clone()),
        schedule: Some(opts.pacing_mode.label().to_string()),
        output_paths: written,
        termination,
        diagnostics: Vec::new(),
        metrics: rumoca_sim::build_simulation_metrics_value(&sim, &metrics),
        simulation: Some(SimPyResult::from_sim(
            model_name,
            sim,
            Some(compile_seconds),
            metrics.simulate_seconds,
        )),
        codegen: None,
    })
}

fn run_scheduled_simulation(
    mut config: ScenarioSimulationConfig,
    base: &Path,
) -> ApiResult<ScenarioResult> {
    let model_config = config
        .model
        .as_ref()
        .ok_or_else(|| ApiError::Compile("scenario is missing [model]".to_string()))?;
    let model_name = model_config.name.clone();
    let model_path = resolve_path(base, &model_config.file);
    let model_source = fs::read_to_string(&model_path)
        .map_err(|e| ApiError::Compile(format!("Failed to read {}: {e}", model_path.display())))?;
    let source_roots = config
        .source_roots
        .iter()
        .map(|root| resolve_path(base, root))
        .collect::<Vec<_>>();
    let (scene_script, scene_asset_dir) = resolve_scene_and_asset_dir(&config, base)?;
    let (solver_mode, solver_label) = SimSolverMode::parse_request(config.sim.solver.as_deref());
    let schedule = config.effective_pacing_mode().label().to_string();
    let http_port = config.http_port();
    let ws_port = config.websocket_port();
    let atol = config.sim.atol;
    let rtol = config.sim.rtol;
    let debug_log_path = config.debug_log.as_mut().map(|debug| {
        let path = resolve_path(base, &debug.path);
        debug.path = path.to_string_lossy().to_string();
        path
    });
    rumoca_sim::scheduled_sim::run(rumoca_sim::scheduled_sim::ScheduledSimArgs {
        model_source,
        model_path: Some(model_path),
        model_name: model_name.clone(),
        config,
        solver_mode,
        solver_label,
        atol,
        rtol,
        http_port,
        ws_port,
        scene_script,
        scene_asset_dir,
        source_roots,
        debug: false,
    })
    .map_err(|e| ApiError::Sim(format!("{e}")))?;
    let output_paths = debug_log_path
        .into_iter()
        .map(|path| path.to_string_lossy().to_string())
        .collect::<Vec<_>>();
    Ok(ScenarioResult {
        task: "simulate".to_string(),
        status: "completed".to_string(),
        model: Some(model_name),
        schedule: Some(schedule),
        output_paths,
        termination: None,
        diagnostics: Vec::new(),
        metrics: json!({}),
        simulation: None,
        codegen: None,
    })
}

fn apply_simulation_overrides(
    config: &mut ScenarioSimulationConfig,
    overrides: &ScenarioOverrides,
) -> ApiResult<()> {
    if let Some(t_end) = overrides.t_end {
        config.sim.t_end = t_end;
    }
    if let Some(dt) = overrides.dt {
        config.sim.dt = dt;
    }
    if let Some(solver) = &overrides.solver {
        config.sim.solver = Some(solver.clone());
    }
    if let Some(mode) = overrides.mode {
        config.sim.mode = Some(mode);
    }
    if let Some(output) = &overrides.output {
        config.sim.output = Some(output.clone());
    }
    if let Some(debug_log_path) = &overrides.debug_log_path {
        let Some(debug_log) = config.debug_log.as_mut() else {
            return Err(ApiError::Compile(
                "debug_log_path override requires a [debug_log] section in the scenario"
                    .to_string(),
            ));
        };
        debug_log.path = debug_log_path.clone();
    }
    Ok(())
}

fn sim_options_from_config(config: &ScenarioSimulationConfig) -> (SimOptions, String) {
    let (solver_mode, solver_label) = SimSolverMode::parse_request(config.sim.solver.as_deref());
    let mut opts = SimOptions {
        t_end: config.sim.t_end,
        dt: Some(config.sim.dt),
        solver_mode,
        diffsol_method: DiffsolMethod::from_external_name(&solver_label).unwrap_or_default(),
        pacing_mode: config.effective_pacing_mode(),
        ..SimOptions::default()
    };
    if let Some(atol) = config.sim.atol {
        opts.atol = atol;
    }
    if let Some(rtol) = config.sim.rtol {
        opts.rtol = rtol;
    }
    (opts, solver_label)
}

fn sim_request(opts: &SimOptions, solver_label: String) -> SimulationRequestSummary {
    SimulationRequestSummary {
        solver: solver_label,
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    }
}

fn write_simulation_output(
    sim: &SimResult,
    model_name: &str,
    output_path: &Path,
    request: &SimulationRequestSummary,
    metrics: &SimulationRunMetrics,
    workspace_root: &Path,
) -> ApiResult<Vec<String>> {
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| ApiError::Sim(format!("Failed to create {}: {e}", parent.display())))?;
    }
    match output_path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) if ext.eq_ignore_ascii_case("csv") => {
            rumoca_sim::report::write_csv_results(sim, output_path).map_err(|e| {
                ApiError::Sim(format!("Failed to write {}: {e}", output_path.display()))
            })?;
        }
        Some(ext) if ext.eq_ignore_ascii_case("html") => {
            rumoca_sim::report::write_html_report(
                sim,
                model_name,
                output_path,
                request,
                metrics,
                Some(workspace_root),
            )
            .map_err(|e| {
                ApiError::Sim(format!("Failed to write {}: {e}", output_path.display()))
            })?;
        }
        Some(other) => {
            return Err(ApiError::Sim(format!(
                "unsupported simulation output extension `.{other}` for `{}`: use `.html` or `.csv`",
                output_path.display()
            )));
        }
        None => {
            rumoca_sim::report::write_html_report(
                sim,
                model_name,
                output_path,
                request,
                metrics,
                Some(workspace_root),
            )
            .map_err(|e| {
                ApiError::Sim(format!("Failed to write {}: {e}", output_path.display()))
            })?;
        }
    }
    Ok(vec![output_path.to_string_lossy().to_string()])
}

fn simulation_output_path(
    base: &Path,
    model_name: &str,
    configured_output: Option<&str>,
    overrides: &ScenarioOverrides,
) -> PathBuf {
    if let Some(output) = overrides.output.as_deref().or(configured_output) {
        return resolve_path(base, output);
    }
    if let Some(output_dir) = overrides.output_dir.as_deref() {
        return resolve_path(base, output_dir).join(format!("{model_name}_results.html"));
    }
    base.join(format!("{model_name}_results.html"))
}

fn resolve_scene_and_asset_dir(
    config: &ScenarioSimulationConfig,
    base: &Path,
) -> ApiResult<(Option<String>, Option<PathBuf>)> {
    let http = config.transport.as_ref().and_then(|t| t.http.as_ref());
    let mut scene_asset_dir = None;
    let scene_script = match http.and_then(|http| http.scene.clone()) {
        Some(scene) => {
            let scene_path = resolve_path(base, &scene);
            scene_asset_dir = scene_path.parent().map(Path::to_path_buf);
            Some(fs::read_to_string(&scene_path).map_err(|e| {
                ApiError::Compile(format!("Read scene script {}: {e}", scene_path.display()))
            })?)
        }
        None => None,
    };
    if let Some(asset_dir) = http.and_then(|http| http.asset_dir.as_deref()) {
        scene_asset_dir = Some(resolve_path(base, asset_dir));
    }
    Ok((scene_script, scene_asset_dir))
}

fn merged_roots(base: &Path, session_roots: &[String], scenario_roots: &[String]) -> Vec<String> {
    resolve_source_root_paths(&merged_roots_paths(base, session_roots, scenario_roots))
}

fn merged_code_generation_roots(session_roots: &[String], roots: &[String]) -> Vec<String> {
    let mut merged = session_roots.to_vec();
    merged.extend(roots.iter().cloned());
    resolve_source_root_paths(&merged)
}

fn merged_roots_paths(
    base: &Path,
    session_roots: &[String],
    scenario_roots: &[String],
) -> Vec<String> {
    let mut roots = session_roots.to_vec();
    roots.extend(
        scenario_roots
            .iter()
            .map(|root| resolve_path(base, root).to_string_lossy().to_string()),
    );
    roots
}

fn required_string(value: Option<&str>, field: &str) -> ApiResult<String> {
    value
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .ok_or_else(|| ApiError::Compile(format!("scenario is missing {field}")))
}

fn default_codegen_output_dir(model_name: &str, target: &str) -> String {
    format!(
        "gen/{}_{}",
        model_name.replace(['.', ':', '/', '\\'], "_"),
        target.replace(['.', ':', '/', '\\'], "_")
    )
}

fn resolve_path(base: &Path, rel: &str) -> PathBuf {
    let path = Path::new(rel);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base.join(path)
    }
}

fn parent_dir_or_current(path: &Path) -> &Path {
    path.parent().unwrap_or_else(|| Path::new("."))
}

pub(crate) fn path_string(value: &Bound<'_, PyAny>) -> ApiResult<String> {
    if let Ok(path) = value.extract::<String>() {
        return Ok(path);
    }
    if let Ok(path) = value.call_method0("__fspath__") {
        return path.extract::<String>().map_err(Into::into);
    }
    Err(ApiError::Compile(
        "path must be a string or pathlib-compatible object".to_string(),
    ))
}

impl ScenarioOverrides {
    fn from_py(overrides: Option<&Bound<'_, PyDict>>) -> ApiResult<Self> {
        let Some(overrides) = overrides else {
            return Ok(Self::default());
        };
        let mut parsed = Self::default();
        for (key, value) in overrides.iter() {
            let key: String = key.extract()?;
            match key.as_str() {
                "t_end" => parsed.t_end = Some(value.extract()?),
                "dt" => parsed.dt = Some(value.extract()?),
                "solver" => parsed.solver = Some(value.extract()?),
                "mode" | "schedule" => {
                    parsed.mode = Some(parse_pacing_mode(&value.extract::<String>()?)?)
                }
                "output" | "output_path" => parsed.output = Some(path_string(&value)?),
                "output_dir" => parsed.output_dir = Some(path_string(&value)?),
                "debug_log_path" | "log_path" => parsed.debug_log_path = Some(path_string(&value)?),
                other => return Err(unknown_override(other)),
            }
        }
        Ok(parsed)
    }
}

fn parse_pacing_mode(value: &str) -> ApiResult<SimPacingMode> {
    match value.trim() {
        "as_fast_as_possible" | "fast" => Ok(SimPacingMode::AsFastAsPossible),
        "realtime" | "real_time" => Ok(SimPacingMode::Realtime),
        "lockstep" => Ok(SimPacingMode::Lockstep),
        other => Err(ApiError::Compile(format!(
            "unknown scenario mode {other:?}; expected as_fast_as_possible, realtime, or lockstep"
        ))),
    }
}

fn unknown_override(name: &str) -> ApiError {
    ApiError::Compile(format!(
        "unknown scenario override {name:?}; supported overrides are \
         t_end, dt, solver, mode, output, output_dir, and debug_log_path"
    ))
}
