// PyO3 macro expansion currently emits Rust 2024 `unsafe_op_in_unsafe_fn` patterns.
#![expect(
    unsafe_op_in_unsafe_fn,
    reason = "PyO3 macro expansion emits Rust 2024 unsafe_op_in_unsafe_fn patterns"
)]

//! Python bindings for the Rumoca Modelica compiler.
//!
//! The top-level functions remain convenient one-shot wrappers, while the
//! `ProjectSession` Python class provides a reusable session for workloads that
//! want to retain loaded source roots and compile/query caches across calls.

use ::rumoca::CompilationResult as HighLevelCompilationResult;
use pyo3::prelude::*;
use pyo3::types::PyType;
use pyo3::{PyErr, exceptions::PyRuntimeError};
use rumoca_compile::codegen::targets::{
    TargetBundle, render_dae_target_files, validate_dae_target_capabilities,
};
use rumoca_compile::compile::{FailedPhase, PhaseResult, Session, SessionConfig, SourceRootKind};
use rumoca_compile::parsing::{
    collect_compile_unit_source_files, collect_model_names, validate_source_syntax,
};
use rumoca_compile::scenario::{EffectiveSimulationConfig, ScenarioConfig, ScenarioTask};
use rumoca_compile::source_roots::{
    canonical_path_key, merge_source_root_paths, plan_source_root_loads,
    referenced_unloaded_source_root_paths, source_root_source_set_key,
};
use rumoca_compile::workspace::WorkspaceConfig;
use rumoca_sim::simulate_dae;
use rumoca_sim::{SimOptions, SimSolverMode as RuntimeSimSolverMode};
use rumoca_sim::{
    SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
use rumoca_tool_fmt::FormatOptions;
use rumoca_tool_lint::{LintLevel, LintOptions, lint as lint_source};
use serde_json::{Value, json};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

mod python_result;
#[cfg(test)]
mod tests;

use python_result::{
    CodegenResult, CompileResult, SimulationResult, codegen_result_from_json,
    compile_result_from_compilation, simulation_result_from_json,
};

#[derive(Debug)]
pub(crate) struct PyRuntimeStringError(String);

impl From<PyRuntimeStringError> for PyErr {
    fn from(value: PyRuntimeStringError) -> Self {
        PyRuntimeError::new_err(value.0)
    }
}

/// Result of parsing Modelica source.
#[pyclass]
#[derive(Clone)]
pub struct ParseResult {
    #[pyo3(get)]
    success: bool,
    #[pyo3(get)]
    error: Option<String>,
}

#[pymethods]
impl ParseResult {
    fn __repr__(&self) -> String {
        if self.success {
            "ParseResult(success=True)".to_string()
        } else {
            format!(
                "ParseResult(success=False, error={:?})",
                self.error.as_deref().unwrap_or("unknown")
            )
        }
    }

    fn __bool__(&self) -> bool {
        self.success
    }
}

/// A lint message from the Modelica linter.
#[pyclass]
#[derive(Clone)]
pub struct LintMessage {
    #[pyo3(get)]
    rule: String,
    #[pyo3(get)]
    level: String,
    #[pyo3(get)]
    message: String,
    #[pyo3(get)]
    file: String,
    #[pyo3(get)]
    line: u32,
    #[pyo3(get)]
    column: u32,
    #[pyo3(get)]
    suggestion: Option<String>,
}

#[pymethods]
impl LintMessage {
    fn __repr__(&self) -> String {
        format!(
            "LintMessage(rule='{}', level='{}', line={}, message='{}')",
            self.rule, self.level, self.line, self.message
        )
    }
}

#[pyclass]
#[derive(Clone)]
pub struct SimulationOptions {
    #[pyo3(get)]
    t_start: f64,
    #[pyo3(get)]
    t_end: f64,
    #[pyo3(get)]
    dt: Option<f64>,
    #[pyo3(get)]
    solver: Option<String>,
    #[pyo3(get)]
    rtol: Option<f64>,
    #[pyo3(get)]
    atol: Option<f64>,
    #[pyo3(get)]
    max_wall_seconds: Option<f64>,
}

#[pymethods]
impl SimulationOptions {
    #[new]
    #[pyo3(signature = (t_end=1.0, dt=None, solver=None, t_start=0.0, rtol=None, atol=None, max_wall_seconds=None))]
    fn new(
        t_end: f64,
        dt: Option<f64>,
        solver: Option<String>,
        t_start: f64,
        rtol: Option<f64>,
        atol: Option<f64>,
        max_wall_seconds: Option<f64>,
    ) -> Self {
        Self {
            t_start,
            t_end,
            dt,
            solver,
            rtol,
            atol,
            max_wall_seconds,
        }
    }

    fn __repr__(&self) -> String {
        format!(
            "SimulationOptions(t_start={}, t_end={}, dt={:?}, solver={:?})",
            self.t_start, self.t_end, self.dt, self.solver
        )
    }
}

impl Default for SimulationOptions {
    fn default() -> Self {
        Self {
            t_start: 0.0,
            t_end: 1.0,
            dt: None,
            solver: None,
            rtol: None,
            atol: None,
            max_wall_seconds: None,
        }
    }
}

#[pyclass(unsendable)]
struct ProjectSession {
    session: Session,
    source_root_paths: Vec<String>,
    effective_source_root_paths: Vec<String>,
}

#[pymethods]
impl ProjectSession {
    #[new]
    #[pyo3(signature = (source_roots=None))]
    fn new(source_roots: Option<Vec<String>>) -> Self {
        let source_root_paths = source_roots.unwrap_or_default();
        let effective_source_root_paths = resolve_source_root_paths(&source_root_paths);
        Self {
            session: Session::new(SessionConfig::default()),
            source_root_paths,
            effective_source_root_paths,
        }
    }

    #[classmethod]
    #[pyo3(signature = (workspace_root, focus_path=None, model_name=None, task="simulate", source_roots=None))]
    fn from_project(
        _cls: &Bound<'_, PyType>,
        workspace_root: &str,
        focus_path: Option<&str>,
        model_name: Option<&str>,
        task: &str,
        source_roots: Option<Vec<String>>,
    ) -> Result<Self, PyRuntimeStringError> {
        let source_root_paths = project_configured_source_roots(
            source_roots.unwrap_or_default(),
            Some(workspace_root),
            focus_path,
            model_name,
            task,
        )?;
        Ok(Self::new(Some(source_root_paths)))
    }

    fn __repr__(&self) -> String {
        format!(
            "ProjectSession(source_roots={}, loaded_source_roots={})",
            self.source_root_paths.len(),
            self.session.loaded_source_root_path_keys().len()
        )
    }

    fn clear(&mut self) {
        self.session = Session::new(SessionConfig::default());
    }

    #[pyo3(signature = (source_roots))]
    fn configure_source_roots(
        &mut self,
        source_roots: Vec<String>,
    ) -> Result<(), PyRuntimeStringError> {
        self.source_root_paths = source_roots;
        refresh_effective_source_root_paths(
            &mut self.session,
            &self.source_root_paths,
            &mut self.effective_source_root_paths,
        );
        Ok(())
    }

    fn get_source_roots(&self) -> Vec<String> {
        self.source_root_paths.clone()
    }

    #[pyo3(signature = (workspace_root, focus_path=None, model_name=None, task="simulate", source_roots=None))]
    fn configure_project(
        &mut self,
        workspace_root: &str,
        focus_path: Option<&str>,
        model_name: Option<&str>,
        task: &str,
        source_roots: Option<Vec<String>>,
    ) -> Result<(), PyRuntimeStringError> {
        let source_root_paths = project_configured_source_roots(
            source_roots.unwrap_or_default(),
            Some(workspace_root),
            focus_path,
            model_name,
            task,
        )?;
        self.configure_source_roots(source_root_paths)
    }

    fn load_source_roots(&mut self) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let reports =
            load_source_roots_into_session(&mut self.session, &self.effective_source_root_paths)?;
        source_root_reports_json(&reports)
    }

    fn source_root_statuses(&mut self) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        serde_json::to_string(&self.session.source_root_statuses())
            .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
    }

    #[pyo3(signature = (source, model_name=None, filename=None))]
    fn compile(
        &mut self,
        source: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let filename = filename.unwrap_or("input.mo");
        let (result, _) = compile_source_in_session(
            &mut self.session,
            source,
            model_name,
            filename,
            &self.effective_source_root_paths,
        )?;
        serialize_raw_dae(&result)
    }

    #[pyo3(signature = (source, model_name=None, filename=None))]
    fn compile_to_json(
        &mut self,
        source: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let filename = filename.unwrap_or("input.mo");
        let (result, _) = compile_source_in_session(
            &mut self.session,
            source,
            model_name,
            filename,
            &self.effective_source_root_paths,
        )?;
        result
            .to_json()
            .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
    }

    #[pyo3(signature = (path, model_name=None))]
    fn compile_file(
        &mut self,
        path: &str,
        model_name: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let (result, _) = compile_file_in_session(
            &mut self.session,
            path,
            model_name,
            &self.effective_source_root_paths,
        )?;
        serialize_raw_dae(&result)
    }

    #[pyo3(signature = (path, model_name=None))]
    fn compile_file_to_json(
        &mut self,
        path: &str,
        model_name: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let (result, _) = compile_file_in_session(
            &mut self.session,
            path,
            model_name,
            &self.effective_source_root_paths,
        )?;
        result
            .to_json()
            .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
    }

    #[pyo3(signature = (source, template, model_name=None, filename=None))]
    fn render_model(
        &mut self,
        source: &str,
        template: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let filename = filename.unwrap_or("input.mo");
        let (result, actual_model_name) = compile_source_in_session(
            &mut self.session,
            source,
            model_name,
            filename,
            &self.effective_source_root_paths,
        )?;
        render_compiled_model(&result, &actual_model_name, template)
    }

    #[pyo3(signature = (path, template, model_name=None))]
    fn render_model_file(
        &mut self,
        path: &str,
        template: &str,
        model_name: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let (result, actual_model_name) = compile_file_in_session(
            &mut self.session,
            path,
            model_name,
            &self.effective_source_root_paths,
        )?;
        render_compiled_model(&result, &actual_model_name, template)
    }

    #[pyo3(signature = (source, target, model_name=None, filename=None))]
    fn render_target_model(
        &mut self,
        source: &str,
        target: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let filename = filename.unwrap_or("input.mo");
        let (result, actual_model_name) = compile_source_in_session(
            &mut self.session,
            source,
            model_name,
            filename,
            &self.effective_source_root_paths,
        )?;
        render_compiled_target(&result, &actual_model_name, target)
    }

    #[pyo3(signature = (path, target, model_name=None))]
    fn render_target_file(
        &mut self,
        path: &str,
        target: &str,
        model_name: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let (result, actual_model_name) = compile_file_in_session(
            &mut self.session,
            path,
            model_name,
            &self.effective_source_root_paths,
        )?;
        render_compiled_target(&result, &actual_model_name, target)
    }

    #[pyo3(signature = (source, model_name=None, filename=None, options=None))]
    fn simulate(
        &mut self,
        source: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
        options: Option<PyRef<'_, SimulationOptions>>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let filename = filename.unwrap_or("input.mo");
        simulate_source_in_session(
            &mut self.session,
            source,
            model_name,
            filename,
            &self.effective_source_root_paths,
            sim_request(options.as_deref()),
        )
    }

    #[pyo3(signature = (path, model_name=None, options=None))]
    fn simulate_file(
        &mut self,
        path: &str,
        model_name: Option<&str>,
        options: Option<PyRef<'_, SimulationOptions>>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        simulate_file_in_session(
            &mut self.session,
            path,
            model_name,
            &self.effective_source_root_paths,
            sim_request(options.as_deref()),
        )
    }
}

impl ProjectSession {
    fn sync_source_root_paths(&mut self) -> Result<(), PyRuntimeStringError> {
        refresh_effective_source_root_paths(
            &mut self.session,
            &self.source_root_paths,
            &mut self.effective_source_root_paths,
        );
        Ok(())
    }
}

#[pyclass(unsendable)]
pub struct Project {
    session: ProjectSession,
    workspace_root: String,
    model_name: Option<String>,
    model_file: Option<String>,
    focus_path: Option<String>,
}

#[pymethods]
impl Project {
    #[classmethod]
    #[pyo3(signature = (workspace_root=".", model_name=None, model_file=None, focus_path=None, source_roots=None))]
    fn open(
        _cls: &Bound<'_, PyType>,
        workspace_root: &str,
        model_name: Option<&str>,
        model_file: Option<&str>,
        focus_path: Option<&str>,
        source_roots: Option<Vec<String>>,
    ) -> Result<Self, PyRuntimeStringError> {
        let focus = focus_path.or(model_file);
        let source_root_paths = project_configured_source_roots(
            source_roots.unwrap_or_default(),
            Some(workspace_root),
            focus,
            model_name,
            "simulate",
        )?;
        let session = ProjectSession::new(Some(source_root_paths));
        Ok(Self {
            session,
            workspace_root: workspace_root.to_string(),
            model_name: model_name.map(str::to_string),
            model_file: model_file.map(str::to_string),
            focus_path: focus_path.map(str::to_string),
        })
    }

    fn __repr__(&self) -> String {
        format!(
            "Project(workspace_root='{}', model_name={:?}, model_file={:?})",
            self.workspace_root, self.model_name, self.model_file
        )
    }

    #[getter]
    fn workspace_root(&self) -> String {
        self.workspace_root.clone()
    }

    #[getter]
    fn model_name(&self) -> Option<String> {
        self.model_name.clone()
    }

    #[getter]
    fn model_file(&self) -> Option<String> {
        self.model_file.clone()
    }

    fn source_roots(&self) -> Vec<String> {
        self.session.get_source_roots()
    }

    fn load_source_roots(&mut self) -> Result<String, PyRuntimeStringError> {
        self.session.load_source_roots()
    }

    #[pyo3(signature = (path=None, model_name=None))]
    fn compile_file(
        &mut self,
        path: Option<&str>,
        model_name: Option<&str>,
    ) -> Result<CompileResult, PyRuntimeStringError> {
        let path = self.resolve_model_file(path)?;
        let model_name = self.resolve_model_name(model_name);
        self.session.sync_source_root_paths()?;
        let (result, actual_model_name) = compile_file_in_session(
            &mut self.session.session,
            &path,
            model_name.as_deref(),
            &self.session.effective_source_root_paths,
        )?;
        compile_result_from_compilation(result, actual_model_name)
    }

    #[pyo3(signature = (source, model_name=None, filename=None))]
    fn compile_source(
        &mut self,
        source: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
    ) -> Result<CompileResult, PyRuntimeStringError> {
        self.session.sync_source_root_paths()?;
        let filename = filename
            .map(str::to_string)
            .or_else(|| self.model_file.clone())
            .unwrap_or_else(|| "input.mo".to_string());
        let model_name = self.resolve_model_name(model_name);
        let (result, actual_model_name) = compile_source_in_session(
            &mut self.session.session,
            source,
            model_name.as_deref(),
            &filename,
            &self.session.effective_source_root_paths,
        )?;
        compile_result_from_compilation(result, actual_model_name)
    }

    #[pyo3(signature = (path=None, model_name=None, options=None))]
    fn simulate_file(
        &mut self,
        path: Option<&str>,
        model_name: Option<&str>,
        options: Option<PyRef<'_, SimulationOptions>>,
    ) -> Result<SimulationResult, PyRuntimeStringError> {
        let path = self.resolve_model_file(path)?;
        let model_name = self.resolve_model_name(model_name);
        self.session.sync_source_root_paths()?;
        let json = simulate_file_in_session(
            &mut self.session.session,
            &path,
            model_name.as_deref(),
            &self.session.effective_source_root_paths,
            sim_request(options.as_deref()),
        )?;
        simulation_result_from_json(&json)
    }

    #[pyo3(signature = (source, model_name=None, filename=None, options=None))]
    fn simulate_source(
        &mut self,
        source: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
        options: Option<PyRef<'_, SimulationOptions>>,
    ) -> Result<SimulationResult, PyRuntimeStringError> {
        self.session.sync_source_root_paths()?;
        let filename = filename
            .map(str::to_string)
            .or_else(|| self.model_file.clone())
            .unwrap_or_else(|| "input.mo".to_string());
        let model_name = self.resolve_model_name(model_name);
        let json = simulate_source_in_session(
            &mut self.session.session,
            source,
            model_name.as_deref(),
            &filename,
            &self.session.effective_source_root_paths,
            sim_request(options.as_deref()),
        )?;
        simulation_result_from_json(&json)
    }

    #[pyo3(signature = (path=None, target=None, model_name=None))]
    fn codegen_file(
        &mut self,
        path: Option<&str>,
        target: Option<&str>,
        model_name: Option<&str>,
    ) -> Result<CodegenResult, PyRuntimeStringError> {
        let path = self.resolve_model_file(path)?;
        let model_name = self.resolve_model_name(model_name);
        let target = self.resolve_codegen_target(target, model_name.as_deref())?;
        self.session.sync_source_root_paths()?;
        let (result, actual_model_name) = compile_file_in_session(
            &mut self.session.session,
            &path,
            model_name.as_deref(),
            &self.session.effective_source_root_paths,
        )?;
        let json = render_compiled_target(&result, &actual_model_name, &target)?;
        codegen_result_from_json(&json, actual_model_name, target)
    }
}

impl Project {
    fn resolve_model_name(&self, model_name: Option<&str>) -> Option<String> {
        model_name
            .map(str::to_string)
            .or_else(|| self.model_name.clone())
    }

    fn resolve_model_file(&self, path: Option<&str>) -> Result<String, PyRuntimeStringError> {
        path.map(str::to_string)
            .or_else(|| self.model_file.clone())
            .or_else(|| self.focus_path.clone())
            .ok_or_else(|| {
                PyRuntimeStringError(
                    "model file required: pass path=... or open Project with model_file=..."
                        .to_string(),
                )
            })
    }

    fn resolve_codegen_target(
        &self,
        target: Option<&str>,
        model_name: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        if let Some(target) = target {
            return Ok(target.to_string());
        }
        let Some(model_name) = model_name else {
            return Err(PyRuntimeStringError(
                "codegen target required when project has no model_name".to_string(),
            ));
        };
        let config = ScenarioConfig::load_or_default(Path::new(&self.workspace_root))
            .map_err(|e| PyRuntimeStringError(format!("Scenario config error: {e}")))?;
        config
            .codegen_config_for_model(model_name)
            .target
            .ok_or_else(|| {
                PyRuntimeStringError(
                    "codegen target required: pass target=... or configure [codegen].target"
                        .to_string(),
                )
            })
    }
}

/// Get the Rumoca version.
#[pyfunction]
fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// Parse Modelica source code.
#[pyfunction]
#[pyo3(signature = (source, filename=None))]
fn parse(source: &str, filename: Option<&str>) -> ParseResult {
    let filename = filename.unwrap_or("input.mo");

    match validate_source_syntax(source, filename) {
        Ok(()) => ParseResult {
            success: true,
            error: None,
        },
        Err(e) => ParseResult {
            success: false,
            error: Some(e.to_string()),
        },
    }
}

/// Lint Modelica source code.
#[pyfunction]
#[pyo3(signature = (source, filename=None))]
fn lint(source: &str, filename: Option<&str>) -> Vec<LintMessage> {
    let filename = filename.unwrap_or("input.mo");
    let options = LintOptions::default();
    let messages = lint_source(source, filename, &options);

    messages
        .into_iter()
        .map(|m| LintMessage {
            rule: m.rule.to_string(),
            level: match m.level {
                LintLevel::Error => "error".to_string(),
                LintLevel::Warning => "warning".to_string(),
                LintLevel::Note => "note".to_string(),
                LintLevel::Help => "help".to_string(),
            },
            message: m.message,
            file: m.file,
            line: m.line,
            column: m.column,
            suggestion: m.suggestion,
        })
        .collect()
}

/// Format Modelica source code.
#[pyfunction(name = "format")]
#[pyo3(signature = (source, filename=None))]
fn format_source(source: &str, filename: Option<&str>) -> Result<String, PyRuntimeStringError> {
    let filename = filename.unwrap_or("input.mo");
    let options = FormatOptions::default();
    rumoca_tool_fmt::format_with_source_name(source, &options, filename)
        .map_err(|e| PyRuntimeStringError(format!("Format error: {e}")))
}

/// Format Modelica source code, returning original source on format/syntax error.
#[pyfunction]
#[pyo3(signature = (source, filename=None))]
fn format_or_original(source: &str, filename: Option<&str>) -> String {
    let filename = filename.unwrap_or("input.mo");
    let options = FormatOptions::default();
    rumoca_tool_fmt::format_or_original_with_source_name(source, &options, filename)
}

/// Check Modelica source code for errors and warnings.
#[pyfunction]
#[pyo3(signature = (source, filename=None))]
fn check(source: &str, filename: Option<&str>) -> Vec<LintMessage> {
    let filename = filename.unwrap_or("input.mo");

    if let Err(e) = validate_source_syntax(source, filename) {
        return vec![LintMessage {
            rule: "syntax-error".to_string(),
            level: "error".to_string(),
            message: e.to_string(),
            file: filename.to_string(),
            line: 1,
            column: 1,
            suggestion: None,
        }];
    }

    lint(source, Some(filename))
}

/// Return built-in target.toml codegen targets as JSON.
#[pyfunction]
fn get_builtin_targets() -> Result<String, PyRuntimeStringError> {
    serde_json::to_string(&builtin_targets_json())
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

/// Resolve source roots from MODELICAPATH, workspace config, scenario config, and explicit paths.
#[pyfunction]
#[pyo3(signature = (source_roots=None, workspace_root=None, focus_path=None, model_name=None, task="simulate"))]
fn effective_source_roots(
    source_roots: Option<Vec<String>>,
    workspace_root: Option<&str>,
    focus_path: Option<&str>,
    model_name: Option<&str>,
    task: &str,
) -> Result<String, PyRuntimeStringError> {
    let configured = project_configured_source_roots(
        source_roots.unwrap_or_default(),
        workspace_root,
        focus_path,
        model_name,
        task,
    )?;
    serde_json::to_string(&resolve_source_root_paths(&configured))
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

/// Return effective `rumoca-workspace.toml` source roots for a focused path as JSON.
#[pyfunction]
#[pyo3(signature = (workspace_root, focus_path=None))]
fn workspace_source_roots(
    workspace_root: &str,
    focus_path: Option<&str>,
) -> Result<String, PyRuntimeStringError> {
    let roots = workspace_configured_source_roots(workspace_root, focus_path)?;
    serde_json::to_string(&roots).map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

/// Return the effective simulation config for a model from `rumoca-scenario*.toml`.
#[pyfunction]
#[pyo3(signature = (workspace_root, model_name, solver=None, t_end=None, dt=None, output_dir=None, source_roots=None))]
fn scenario_simulation_config(
    workspace_root: &str,
    model_name: &str,
    solver: Option<&str>,
    t_end: Option<f64>,
    dt: Option<f64>,
    output_dir: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let snapshot = load_scenario_simulation_snapshot(
        workspace_root,
        model_name,
        solver,
        t_end,
        dt,
        output_dir,
        source_roots.unwrap_or_default(),
    )?;
    serde_json::to_string_pretty(&snapshot)
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

/// Return the codegen target config for a model from `rumoca-scenario*.toml`.
#[pyfunction]
#[pyo3(signature = (workspace_root, model_name))]
fn scenario_codegen_config(
    workspace_root: &str,
    model_name: &str,
) -> Result<String, PyRuntimeStringError> {
    let config = ScenarioConfig::load_or_default(Path::new(workspace_root))
        .map_err(|e| PyRuntimeStringError(format!("Scenario config error: {e}")))?;
    let codegen = config.codegen_config_for_model(model_name);
    serde_json::to_string_pretty(&codegen)
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

/// Compile inline Modelica source code to raw DAE JSON.
#[pyfunction]
#[pyo3(signature = (source, model_name=None, filename=None, source_roots=None))]
fn compile(
    source: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.compile(source, model_name, filename)
}

/// Compile inline Modelica source code to canonical CLI-style JSON.
#[pyfunction]
#[pyo3(signature = (source, model_name=None, filename=None, source_roots=None))]
fn compile_to_json(
    source: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.compile_to_json(source, model_name, filename)
}

/// Compile inline Modelica source code to raw DAE JSON.
#[pyfunction(name = "compile_source")]
#[pyo3(signature = (source, model_name=None, filename=None, source_roots=None))]
fn compile_source(
    source: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    compile(source, model_name, filename, source_roots)
}

/// Compile a Modelica file to raw DAE JSON.
#[pyfunction]
#[pyo3(signature = (path, model_name=None, source_roots=None))]
fn compile_file(
    path: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.compile_file(path, model_name)
}

/// Compile a Modelica file to canonical CLI-style JSON.
#[pyfunction]
#[pyo3(signature = (path, model_name=None, source_roots=None))]
fn compile_file_to_json(
    path: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.compile_file_to_json(path, model_name)
}

/// Compile and render a template against one source string.
#[pyfunction]
#[pyo3(signature = (source, template, model_name=None, filename=None, source_roots=None))]
fn render_model(
    source: &str,
    template: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.render_model(source, template, model_name, filename)
}

/// Compile and render a template against one model file.
#[pyfunction]
#[pyo3(signature = (path, template, model_name=None, source_roots=None))]
fn render_model_file(
    path: &str,
    template: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.render_model_file(path, template, model_name)
}

/// Compile and render one target.toml target against inline source.
#[pyfunction]
#[pyo3(signature = (source, target, model_name=None, filename=None, source_roots=None))]
fn render_target_model(
    source: &str,
    target: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.render_target_model(source, target, model_name, filename)
}

/// Compile and render one target.toml target against a model file.
#[pyfunction]
#[pyo3(signature = (path, target, model_name=None, source_roots=None))]
fn render_target_file(
    path: &str,
    target: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.render_target_file(path, target, model_name)
}

/// Compile and simulate inline Modelica source.
#[pyfunction]
#[pyo3(signature = (source, model_name=None, filename=None, source_roots=None, options=None))]
fn simulate(
    source: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
    options: Option<PyRef<'_, SimulationOptions>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.sync_source_root_paths()?;
    let filename = filename.unwrap_or("input.mo");
    simulate_source_in_session(
        &mut session.session,
        source,
        model_name,
        filename,
        &session.effective_source_root_paths,
        sim_request(options.as_deref()),
    )
}

/// Compile and simulate a Modelica file.
#[pyfunction]
#[pyo3(signature = (path, model_name=None, source_roots=None, options=None))]
fn simulate_file(
    path: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
    options: Option<PyRef<'_, SimulationOptions>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.sync_source_root_paths()?;
    simulate_file_in_session(
        &mut session.session,
        path,
        model_name,
        &session.effective_source_root_paths,
        sim_request(options.as_deref()),
    )
}

fn builtin_targets_json() -> Value {
    Value::Array(
        rumoca_compile::codegen::templates::builtin_targets()
            .iter()
            .map(|target| {
                json!({
                    "id": target.name,
                    "manifest": target.manifest,
                    "templates": target.templates.iter().map(|template| template.path).collect::<Vec<_>>(),
                })
            })
            .collect(),
    )
}

fn project_configured_source_roots(
    explicit_source_roots: Vec<String>,
    workspace_root: Option<&str>,
    focus_path: Option<&str>,
    model_name: Option<&str>,
    task: &str,
) -> Result<Vec<String>, PyRuntimeStringError> {
    let mut roots = match workspace_root {
        Some(root) => workspace_configured_source_roots(root, focus_path)?,
        None => Vec::new(),
    };
    if let (Some(root), Some(model)) = (workspace_root, model_name) {
        extend_scenario_source_roots(&mut roots, root, model, task)?;
    }
    roots.extend(explicit_source_roots);
    Ok(dedup_source_roots(roots))
}

fn workspace_configured_source_roots(
    workspace_root: &str,
    focus_path: Option<&str>,
) -> Result<Vec<String>, PyRuntimeStringError> {
    let workspace_root = Path::new(workspace_root);
    let focus_path = focus_path
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root.to_path_buf());
    let config = WorkspaceConfig::load(workspace_root, &focus_path)
        .map_err(|e| PyRuntimeStringError(format!("Workspace config error: {e}")))?;
    Ok(config.effective_source_roots_for(&focus_path))
}

fn extend_scenario_source_roots(
    roots: &mut Vec<String>,
    workspace_root: &str,
    model_name: &str,
    task: &str,
) -> Result<(), PyRuntimeStringError> {
    let task = parse_scenario_task(task)?;
    let config = ScenarioConfig::load_or_default(Path::new(workspace_root))
        .map_err(|e| PyRuntimeStringError(format!("Scenario config error: {e}")))?;
    match task {
        ScenarioTask::Simulate => {
            let fallback = EffectiveSimulationConfig {
                source_root_paths: roots.clone(),
                ..EffectiveSimulationConfig::default()
            };
            *roots = config
                .simulation_snapshot_for_model(model_name, &fallback)
                .effective
                .source_root_paths;
        }
        ScenarioTask::Codegen => roots.extend(config.resolve_all_source_root_paths()),
    }
    Ok(())
}

fn parse_scenario_task(task: &str) -> Result<ScenarioTask, PyRuntimeStringError> {
    match task {
        "simulate" => Ok(ScenarioTask::Simulate),
        "codegen" => Ok(ScenarioTask::Codegen),
        other => Err(PyRuntimeStringError(format!(
            "unknown scenario task '{other}', expected 'simulate' or 'codegen'"
        ))),
    }
}

fn sim_request(options: Option<&SimulationOptions>) -> SimRequest {
    let options = options.cloned().unwrap_or_default();
    SimRequest {
        t_start: options.t_start,
        t_end: options.t_end,
        dt: options.dt,
        solver: options.solver,
        rtol: options.rtol,
        atol: options.atol,
        max_wall_seconds: options.max_wall_seconds,
    }
}

fn load_scenario_simulation_snapshot(
    workspace_root: &str,
    model_name: &str,
    solver: Option<&str>,
    t_end: Option<f64>,
    dt: Option<f64>,
    output_dir: Option<&str>,
    source_roots: Vec<String>,
) -> Result<rumoca_compile::scenario::ScenarioSimulationSnapshot, PyRuntimeStringError> {
    let mut fallback = EffectiveSimulationConfig {
        source_root_paths: source_roots,
        ..EffectiveSimulationConfig::default()
    };
    if let Some(solver) = solver {
        fallback.solver = solver.to_string();
    }
    if let Some(t_end) = t_end {
        fallback.t_end = t_end;
    }
    fallback.dt = dt;
    if let Some(output_dir) = output_dir {
        fallback.output_dir = output_dir.to_string();
    }
    let config = ScenarioConfig::load_or_default(Path::new(workspace_root))
        .map_err(|e| PyRuntimeStringError(format!("Scenario config error: {e}")))?;
    Ok(config.simulation_snapshot_for_model(model_name, &fallback))
}

fn dedup_source_roots(paths: Vec<String>) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    let mut deduped = Vec::new();
    for path in paths {
        let key = canonical_path_key(&path);
        if seen.insert(key) {
            deduped.push(path);
        }
    }
    deduped
}

fn split_env_modelicapath() -> Vec<String> {
    let Some(raw) = env::var_os("MODELICAPATH") else {
        return Vec::new();
    };
    env::split_paths(&raw)
        .filter(|entry| !entry.as_os_str().is_empty())
        .map(|entry| entry.to_string_lossy().to_string())
        .collect()
}

fn resolve_source_root_paths(configured_paths: &[String]) -> Vec<String> {
    let env_paths = split_env_modelicapath();
    merge_source_root_paths(&env_paths, configured_paths)
}

fn refresh_effective_source_root_paths(
    session: &mut Session,
    configured_paths: &[String],
    effective_paths: &mut Vec<String>,
) {
    let next_paths = resolve_source_root_paths(configured_paths);
    let next_keys = next_paths
        .iter()
        .map(|path| canonical_path_key(path))
        .collect::<std::collections::HashSet<_>>();
    for removed_path in effective_paths.iter() {
        if next_keys.contains(&canonical_path_key(removed_path)) {
            continue;
        }
        let source_set_key = source_root_source_set_key(removed_path);
        let _ = session.replace_parsed_source_set(
            &source_set_key,
            SourceRootKind::External,
            Vec::new(),
            None,
        );
    }
    *effective_paths = next_paths;
}

fn source_root_reports_json(
    reports: &[rumoca_compile::compile::SourceRootLoadReport],
) -> Result<String, PyRuntimeStringError> {
    let payload = json!({
        "count": reports.len(),
        "reports": reports.iter().map(|report| {
            json!({
                "sourceSetId": report.source_set_id,
                "sourceRootPath": report.source_root_path,
                "parsedFileCount": report.parsed_file_count,
                "insertedFileCount": report.inserted_file_count,
                "cacheStatus": report.cache_status.map(|status| format!("{status:?}")),
                "cacheKey": report.cache_key,
                "cacheFile": report.cache_file.as_ref().map(|path| path.to_string_lossy().to_string()),
                "diagnostics": report.diagnostics,
            })
        }).collect::<Vec<_>>(),
    });
    serde_json::to_string(&payload).map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

fn load_source_roots_into_session(
    session: &mut Session,
    source_root_paths: &[String],
) -> Result<Vec<rumoca_compile::compile::SourceRootLoadReport>, PyRuntimeStringError> {
    let loaded = session.loaded_source_root_path_keys();
    let plan = plan_source_root_loads(source_root_paths, &loaded);
    let mut reports = Vec::with_capacity(plan.load_paths.len());
    for source_root_path in plan.load_paths {
        let source_root_key = source_root_source_set_key(&source_root_path);
        let report = session.load_source_root_tolerant(
            &source_root_key,
            SourceRootKind::External,
            Path::new(&source_root_path),
            None,
        );
        if !report.diagnostics.is_empty() {
            return Err(PyRuntimeStringError(report.diagnostics.join("\n")));
        }
        reports.push(report);
    }
    Ok(reports)
}

fn ensure_required_source_roots_loaded(
    session: &mut Session,
    source: &str,
    source_root_paths: &[String],
) -> Result<(), PyRuntimeStringError> {
    let loaded = session.loaded_source_root_path_keys();
    let referenced = referenced_unloaded_source_root_paths(source, source_root_paths, &loaded);
    let plan = plan_source_root_loads(&referenced, &loaded);
    for source_root_path in plan.load_paths {
        let source_root_key = source_root_source_set_key(&source_root_path);
        let report = session.load_source_root_tolerant(
            &source_root_key,
            SourceRootKind::External,
            Path::new(&source_root_path),
            None,
        );
        if !report.diagnostics.is_empty() {
            return Err(PyRuntimeStringError(report.diagnostics.join("\n")));
        }
    }
    Ok(())
}

fn load_local_compile_unit(
    session: &mut Session,
    source: &str,
    file_name: &str,
) -> Result<(), PyRuntimeStringError> {
    let path = Path::new(file_name);
    if !path.is_file() {
        let _ = session.update_document(file_name, source);
        return Ok(());
    }

    let files = collect_compile_unit_source_files(path)
        .map_err(|e| PyRuntimeStringError(format!("Compile-unit error: {e}")))?;
    for sibling in files {
        if sibling == path {
            continue;
        }
        let sibling_path = sibling.to_string_lossy().to_string();
        let sibling_source = fs::read_to_string(&sibling)
            .map_err(|e| PyRuntimeStringError(format!("Failed to read {sibling_path}: {e}")))?;
        let _ = session.update_document(&sibling_path, &sibling_source);
    }

    let _ = session.update_document(file_name, source);
    Ok(())
}

fn infer_model_name_from_session(
    session: &mut Session,
    uri: &str,
) -> Result<String, PyRuntimeStringError> {
    let definition = session
        .get_document(uri)
        .map(|doc| doc.best_effort().clone())
        .ok_or_else(|| PyRuntimeStringError(format!("failed to load document '{uri}'")))?;

    let top_level_names = definition
        .classes
        .iter()
        .filter_map(|(name, class)| {
            let class_kind = class.class_type.as_str();
            if class_kind == "model" || class_kind == "block" || class_kind == "class" {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut candidates = collect_model_names(&definition);
    candidates.sort();
    candidates.dedup();
    if candidates.is_empty() {
        if let Some((diagnostics, source_map)) =
            session.document_parse_diagnostics_with_source_map(uri)
        {
            let source_summary = diagnostics
                .into_iter()
                .map(|diagnostic| match diagnostic.code {
                    Some(code) => format!("{code}: {}", diagnostic.message),
                    None => diagnostic.message,
                })
                .collect::<Vec<_>>()
                .join("\n");
            return Err(PyRuntimeStringError(format!(
                "failed to infer model from '{uri}': {source_summary}\nsource_map={source_map:?}"
            )));
        }
        return Err(PyRuntimeStringError(format!(
            "No compilable model/block/class candidates found in '{uri}'."
        )));
    }

    if top_level_names.len() == 1
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, &top_level_names[0])
    {
        return Ok(model);
    }

    if candidates.len() == 1 {
        return Ok(candidates[0].clone());
    }

    let file_stem = Path::new(uri)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or_default();
    if !file_stem.is_empty()
        && let Some(model) = choose_single_candidate_by_suffix(&candidates, file_stem)
    {
        return Ok(model);
    }

    let preview = candidates
        .iter()
        .take(15)
        .cloned()
        .collect::<Vec<_>>()
        .join(", ");
    Err(PyRuntimeStringError(format!(
        "Unable to infer model from '{uri}'. Candidates: {}{}.",
        preview,
        if candidates.len() > 15 { ", ..." } else { "" }
    )))
}

fn choose_single_candidate_by_suffix(candidates: &[String], suffix: &str) -> Option<String> {
    let mut matches = candidates
        .iter()
        .filter(|candidate| last_segment(candidate) == suffix || candidate.as_str() == suffix)
        .collect::<Vec<_>>();
    if matches.len() == 1 {
        return Some(matches[0].clone());
    }
    if matches.is_empty() {
        return None;
    }

    matches.sort_by_key(|candidate| candidate.matches('.').count());
    let min_depth = matches[0].matches('.').count();
    let min_matches = matches
        .into_iter()
        .filter(|candidate| candidate.matches('.').count() == min_depth)
        .collect::<Vec<_>>();
    if min_matches.len() == 1 {
        Some(min_matches[0].clone())
    } else {
        None
    }
}

fn last_segment(qualified_name: &str) -> &str {
    rumoca_core::top_level_last_segment(qualified_name)
}

fn compile_requested_model(
    session: &mut Session,
    model_name: &str,
) -> Result<HighLevelCompilationResult, PyRuntimeStringError> {
    let mut report = session.compile_model_strict_reachable_with_recovery(model_name);
    let failure_summary = report.failure_summary(usize::MAX);
    let result = match report.requested_result.take() {
        Some(PhaseResult::Success(result)) => {
            if !report.failures.is_empty() {
                return Err(PyRuntimeStringError(failure_summary));
            }
            *result
        }
        Some(PhaseResult::NeedsInner { .. }) => {
            return Err(PyRuntimeStringError(failure_summary));
        }
        Some(PhaseResult::Failed { phase, .. }) => {
            let phase_name = match phase {
                FailedPhase::Instantiate => "instantiate",
                FailedPhase::Typecheck => "typecheck",
                FailedPhase::Flatten => "flatten",
                FailedPhase::ToDae => "todae",
            };
            return Err(PyRuntimeStringError(format!(
                "{phase_name} failed: {failure_summary}"
            )));
        }
        None => return Err(PyRuntimeStringError(failure_summary)),
    };
    let resolved = session.resolved_cached().ok_or_else(|| {
        PyRuntimeStringError("strict compile produced no cached resolved tree".to_string())
    })?;
    Ok(HighLevelCompilationResult::new(
        result.dae,
        result.balance_detail,
        result.flat,
        resolved,
    ))
}

fn compile_source_in_session(
    session: &mut Session,
    source: &str,
    model_name: Option<&str>,
    file_name: &str,
    source_root_paths: &[String],
) -> Result<(HighLevelCompilationResult, String), PyRuntimeStringError> {
    ensure_required_source_roots_loaded(session, source, source_root_paths)?;
    load_local_compile_unit(session, source, file_name)?;
    let model_name = match model_name {
        Some(name) => name.to_string(),
        None => infer_model_name_from_session(session, file_name)?,
    };
    let result = compile_requested_model(session, &model_name)?;
    Ok((result, model_name))
}

fn compile_file_in_session(
    session: &mut Session,
    path: &str,
    model_name: Option<&str>,
    source_root_paths: &[String],
) -> Result<(HighLevelCompilationResult, String), PyRuntimeStringError> {
    let source = fs::read_to_string(path)
        .map_err(|e| PyRuntimeStringError(format!("Failed to read {path}: {e}")))?;
    compile_source_in_session(session, &source, model_name, path, source_root_paths)
}

fn serialize_raw_dae(result: &HighLevelCompilationResult) -> Result<String, PyRuntimeStringError> {
    serde_json::to_string_pretty(&result.dae)
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

fn render_compiled_model(
    result: &HighLevelCompilationResult,
    model_name: &str,
    template: &str,
) -> Result<String, PyRuntimeStringError> {
    result
        .render_template_str_with_name(template, model_name)
        .map_err(|e| PyRuntimeStringError(format!("Template error: {e}")))
}

fn render_compiled_target(
    result: &HighLevelCompilationResult,
    model_name: &str,
    target: &str,
) -> Result<String, PyRuntimeStringError> {
    let bundle = TargetBundle::load(target)
        .map_err(|e| PyRuntimeStringError(format!("Target error: {e}")))?;
    let manifest = bundle
        .parse_manifest()
        .map_err(|e| PyRuntimeStringError(format!("Target manifest error: {e}")))?;
    if let Some(capabilities) = manifest.capabilities.as_ref() {
        validate_dae_target_capabilities(&result.dae, &manifest, capabilities)
            .map_err(|e| PyRuntimeStringError(format!("Target capability error: {e}")))?;
    }
    let files = render_dae_target_files(&bundle, &manifest, &result.dae, model_name)
        .map_err(|e| PyRuntimeStringError(format!("Target rendering error: {e}")))?;
    serde_json::to_string_pretty(&files)
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

fn seconds_since(started: Instant) -> f64 {
    started.elapsed().as_secs_f64()
}

#[derive(Clone)]
struct SimRequest {
    t_start: f64,
    t_end: f64,
    dt: Option<f64>,
    solver: Option<String>,
    rtol: Option<f64>,
    atol: Option<f64>,
    max_wall_seconds: Option<f64>,
}

fn simulate_compiled_model(
    result: &HighLevelCompilationResult,
    model_name: &str,
    compile_seconds: f64,
    request: SimRequest,
) -> Result<String, PyRuntimeStringError> {
    let (solver_mode, solver_label) =
        RuntimeSimSolverMode::parse_request(request.solver.as_deref());
    let mut opts = SimOptions {
        t_start: request.t_start,
        t_end: request.t_end,
        dt: request.dt,
        solver_mode,
        ..SimOptions::default()
    };
    if let Some(rtol) = request.rtol {
        opts.rtol = rtol;
    }
    if let Some(atol) = request.atol {
        opts.atol = atol;
    }
    opts.max_wall_seconds = request.max_wall_seconds;
    let sim_started = Instant::now();
    let sim = simulate_dae(&result.dae, &opts)
        .map_err(|e| PyRuntimeStringError(format!("Simulation error: {e}")))?;
    let metrics = SimulationRunMetrics {
        compile_seconds: Some(compile_seconds),
        simulate_seconds: Some(seconds_since(sim_started)),
        ..SimulationRunMetrics::default()
    };
    let request = SimulationRequestSummary {
        solver: solver_label,
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };
    let output = json!({
        "model": model_name,
        "payload": build_simulation_payload(&sim, &request, &metrics),
        "metrics": build_simulation_metrics_value(&sim, &metrics),
    });
    serde_json::to_string(&output).map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

fn simulate_source_in_session(
    session: &mut Session,
    source: &str,
    model_name: Option<&str>,
    file_name: &str,
    source_root_paths: &[String],
    request: SimRequest,
) -> Result<String, PyRuntimeStringError> {
    let compile_started = Instant::now();
    let (result, actual_model_name) =
        compile_source_in_session(session, source, model_name, file_name, source_root_paths)?;
    simulate_compiled_model(
        &result,
        &actual_model_name,
        seconds_since(compile_started),
        request,
    )
}

fn simulate_file_in_session(
    session: &mut Session,
    path: &str,
    model_name: Option<&str>,
    source_root_paths: &[String],
    request: SimRequest,
) -> Result<String, PyRuntimeStringError> {
    let compile_started = Instant::now();
    let (result, actual_model_name) =
        compile_file_in_session(session, path, model_name, source_root_paths)?;
    simulate_compiled_model(
        &result,
        &actual_model_name,
        seconds_since(compile_started),
        request,
    )
}

/// Rumoca Python module.
#[pymodule]
fn rumoca(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(version, m)?)?;
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    m.add_function(wrap_pyfunction!(format_source, m)?)?;
    m.add_function(wrap_pyfunction!(format_or_original, m)?)?;
    m.add_function(wrap_pyfunction!(lint, m)?)?;
    m.add_function(wrap_pyfunction!(check, m)?)?;
    m.add_function(wrap_pyfunction!(compile, m)?)?;
    m.add_function(wrap_pyfunction!(compile_to_json, m)?)?;
    m.add_function(wrap_pyfunction!(compile_file, m)?)?;
    m.add_function(wrap_pyfunction!(compile_file_to_json, m)?)?;
    m.add_function(wrap_pyfunction!(compile_source, m)?)?;
    m.add_function(wrap_pyfunction!(render_model, m)?)?;
    m.add_function(wrap_pyfunction!(render_model_file, m)?)?;
    m.add_function(wrap_pyfunction!(render_target_model, m)?)?;
    m.add_function(wrap_pyfunction!(render_target_file, m)?)?;
    m.add_function(wrap_pyfunction!(get_builtin_targets, m)?)?;
    m.add_function(wrap_pyfunction!(effective_source_roots, m)?)?;
    m.add_function(wrap_pyfunction!(workspace_source_roots, m)?)?;
    m.add_function(wrap_pyfunction!(scenario_simulation_config, m)?)?;
    m.add_function(wrap_pyfunction!(scenario_codegen_config, m)?)?;
    m.add_function(wrap_pyfunction!(simulate, m)?)?;
    m.add_function(wrap_pyfunction!(simulate_file, m)?)?;
    m.add_class::<ParseResult>()?;
    m.add_class::<LintMessage>()?;
    m.add_class::<SimulationOptions>()?;
    m.add_class::<CompileResult>()?;
    m.add_class::<SimulationResult>()?;
    m.add_class::<CodegenResult>()?;
    m.add_class::<ProjectSession>()?;
    m.add_class::<Project>()?;
    Ok(())
}
