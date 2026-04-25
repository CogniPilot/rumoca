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
use pyo3::{PyErr, exceptions::PyRuntimeError};
use rumoca_compile::codegen::render_dae_template_with_json;
use rumoca_compile::compile::{FailedPhase, PhaseResult, Session, SessionConfig, SourceRootKind};
use rumoca_compile::parsing::{
    collect_compile_unit_source_files, collect_model_names, validate_source_syntax,
};
use rumoca_compile::source_roots::{
    canonical_path_key, merge_source_root_paths, plan_source_root_loads,
    referenced_unloaded_source_root_paths, source_root_source_set_key,
};
use rumoca_sim::{SimOptions, SimSolverMode as RuntimeSimSolverMode};
use rumoca_sim::{
    SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
use rumoca_solver_diffsol::simulate_dae;
use rumoca_tool_fmt::FormatOptions;
use rumoca_tool_lint::{LintLevel, LintOptions, lint as lint_source};
use serde_json::{Value, json};
use std::env;
use std::fs;
use std::path::Path;
use std::time::Instant;

#[derive(Debug)]
struct PyRuntimeStringError(String);

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

    #[pyo3(signature = (source, template_id, model_name=None, filename=None))]
    fn render_builtin_model(
        &mut self,
        source: &str,
        template_id: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        let template = builtin_template_source(template_id).ok_or_else(|| {
            PyRuntimeStringError(format!("Unknown built-in template id: {template_id}"))
        })?;
        self.render_model(source, template, model_name, filename)
    }

    #[pyo3(signature = (path, template_id, model_name=None))]
    fn render_builtin_file(
        &mut self,
        path: &str,
        template_id: &str,
        model_name: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        let template = builtin_template_source(template_id).ok_or_else(|| {
            PyRuntimeStringError(format!("Unknown built-in template id: {template_id}"))
        })?;
        self.render_model_file(path, template, model_name)
    }

    #[pyo3(signature = (source, model_name=None, filename=None, t_end=1.0, dt=None, solver=None))]
    fn simulate(
        &mut self,
        source: &str,
        model_name: Option<&str>,
        filename: Option<&str>,
        t_end: f64,
        dt: Option<f64>,
        solver: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        let filename = filename.unwrap_or("input.mo");
        simulate_source_in_session(
            &mut self.session,
            source,
            model_name,
            filename,
            &self.effective_source_root_paths,
            SimRequest { t_end, dt, solver },
        )
    }

    #[pyo3(signature = (path, model_name=None, t_end=1.0, dt=None, solver=None))]
    fn simulate_file(
        &mut self,
        path: &str,
        model_name: Option<&str>,
        t_end: f64,
        dt: Option<f64>,
        solver: Option<&str>,
    ) -> Result<String, PyRuntimeStringError> {
        self.sync_source_root_paths()?;
        simulate_file_in_session(
            &mut self.session,
            path,
            model_name,
            &self.effective_source_root_paths,
            SimRequest { t_end, dt, solver },
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

/// Render code from DAE JSON using an explicit template string.
#[pyfunction]
fn render_template(dae_json: &str, template: &str) -> Result<String, PyRuntimeStringError> {
    let dae_json: Value = serde_json::from_str(dae_json)
        .map_err(|e| PyRuntimeStringError(format!("Invalid DAE JSON: {e}")))?;
    render_dae_template_with_json(&dae_json, template)
        .map_err(|e| PyRuntimeStringError(format!("Template rendering error: {e}")))
}

/// Return the built-in codegen templates as JSON.
#[pyfunction]
fn get_builtin_templates() -> Result<String, PyRuntimeStringError> {
    serde_json::to_string(&builtin_templates_json())
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

/// Compile and render one built-in template against inline source.
#[pyfunction]
#[pyo3(signature = (source, template_id, model_name=None, filename=None, source_roots=None))]
fn render_builtin_model(
    source: &str,
    template_id: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.render_builtin_model(source, template_id, model_name, filename)
}

/// Compile and render one built-in template against a model file.
#[pyfunction]
#[pyo3(signature = (path, template_id, model_name=None, source_roots=None))]
fn render_builtin_file(
    path: &str,
    template_id: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.render_builtin_file(path, template_id, model_name)
}

/// Compile and simulate inline Modelica source.
#[pyfunction]
#[pyo3(signature = (source, model_name=None, filename=None, source_roots=None, t_end=1.0, dt=None, solver=None))]
fn simulate(
    source: &str,
    model_name: Option<&str>,
    filename: Option<&str>,
    source_roots: Option<Vec<String>>,
    t_end: f64,
    dt: Option<f64>,
    solver: Option<&str>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.simulate(source, model_name, filename, t_end, dt, solver)
}

/// Compile and simulate a Modelica file.
#[pyfunction]
#[pyo3(signature = (path, model_name=None, source_roots=None, t_end=1.0, dt=None, solver=None))]
fn simulate_file(
    path: &str,
    model_name: Option<&str>,
    source_roots: Option<Vec<String>>,
    t_end: f64,
    dt: Option<f64>,
    solver: Option<&str>,
) -> Result<String, PyRuntimeStringError> {
    let mut session = ProjectSession::new(source_roots);
    session.simulate_file(path, model_name, t_end, dt, solver)
}

fn builtin_templates_json() -> Value {
    json!([
            {
                "id": "sympy.py.jinja",
                "label": "SymPy (Python)",
                "language": "python",
                "source": rumoca_compile::codegen::templates::SYMPY,
            },
            {
                "id": "jax.py.jinja",
                "label": "JAX / Diffrax (Python)",
                "language": "python",
                "source": rumoca_compile::codegen::templates::JAX,
            },
            {
                "id": "onnx.py.jinja",
                "label": "ONNX (Python)",
                "language": "python",
                "source": rumoca_compile::codegen::templates::ONNX,
            },
            {
                "id": "julia_mtk.jl.jinja",
                "label": "Julia MTK",
                "language": "julia",
                "source": rumoca_compile::codegen::templates::JULIA_MTK,
            },
            {
                "id": "casadi_sx.py.jinja",
                "label": "CasADi SX (Python)",
                "language": "python",
                "source": rumoca_compile::codegen::templates::CASADI_SX,
            },
            {
                "id": "casadi_mx.py.jinja",
                "label": "CasADi MX (Python)",
                "language": "python",
                "source": rumoca_compile::codegen::templates::CASADI_MX,
            },
    {
                "id": "embedded_c/model.h.jinja",
                "label": "Embedded C Header",
                "language": "c",
                "source": rumoca_compile::codegen::templates::EMBEDDED_C_H,
            },
            {
                "id": "embedded_c/model.c.jinja",
                "label": "Embedded C Implementation",
                "language": "c",
                "source": rumoca_compile::codegen::templates::EMBEDDED_C_IMPL,
            },
            {
                "id": "dae_modelica.mo.jinja",
                "label": "DAE Modelica",
                "language": "modelica",
                "source": rumoca_compile::codegen::templates::DAE_MODELICA,
            },
            {
                "id": "flat_modelica.mo.jinja",
                "label": "Flat Modelica",
                "language": "modelica",
                "source": rumoca_compile::codegen::templates::FLAT_MODELICA,
            },
            {
                "id": "fmi2/modelDescription.xml.jinja",
                "label": "FMI 2.0 modelDescription.xml",
                "language": "xml",
                "source": rumoca_compile::codegen::templates::FMI2_MODEL_DESCRIPTION,
            },
            {
                "id": "fmi2/model.c.jinja",
                "label": "FMI 2.0 model.c",
                "language": "c",
                "source": rumoca_compile::codegen::templates::FMI2_MODEL,
            },
            {
                "id": "fmi2/test_driver.c.jinja",
                "label": "FMI 2.0 test driver",
                "language": "c",
                "source": rumoca_compile::codegen::templates::FMI2_TEST_DRIVER,
            },
            {
                "id": "fmi3/modelDescription.xml.jinja",
                "label": "FMI 3.0 modelDescription.xml",
                "language": "xml",
                "source": rumoca_compile::codegen::templates::FMI3_MODEL_DESCRIPTION,
            },
            {
                "id": "fmi3/model.c.jinja",
                "label": "FMI 3.0 model.c",
                "language": "c",
                "source": rumoca_compile::codegen::templates::FMI3_MODEL,
            },
            {
                "id": "fmi3/test_driver.c.jinja",
                "label": "FMI 3.0 test driver",
                "language": "c",
                "source": rumoca_compile::codegen::templates::FMI3_TEST_DRIVER,
            },
        ])
}

fn builtin_template_source(template_id: &str) -> Option<&'static str> {
    match template_id {
        "sympy.py.jinja" => Some(rumoca_compile::codegen::templates::SYMPY),
        "jax.py.jinja" => Some(rumoca_compile::codegen::templates::JAX),
        "onnx.py.jinja" => Some(rumoca_compile::codegen::templates::ONNX),
        "julia_mtk.jl.jinja" => Some(rumoca_compile::codegen::templates::JULIA_MTK),
        "casadi_sx.py.jinja" => Some(rumoca_compile::codegen::templates::CASADI_SX),
        "casadi_mx.py.jinja" => Some(rumoca_compile::codegen::templates::CASADI_MX),
        "embedded_c/model.h.jinja" => Some(rumoca_compile::codegen::templates::EMBEDDED_C_H),
        "embedded_c/model.c.jinja" => Some(rumoca_compile::codegen::templates::EMBEDDED_C_IMPL),
        "dae_modelica.mo.jinja" => Some(rumoca_compile::codegen::templates::DAE_MODELICA),
        "flat_modelica.mo.jinja" => Some(rumoca_compile::codegen::templates::FLAT_MODELICA),
        "fmi2/modelDescription.xml.jinja" => {
            Some(rumoca_compile::codegen::templates::FMI2_MODEL_DESCRIPTION)
        }
        "fmi2/model.c.jinja" => Some(rumoca_compile::codegen::templates::FMI2_MODEL),
        "fmi2/test_driver.c.jinja" => Some(rumoca_compile::codegen::templates::FMI2_TEST_DRIVER),
        "fmi3/modelDescription.xml.jinja" => {
            Some(rumoca_compile::codegen::templates::FMI3_MODEL_DESCRIPTION)
        }
        "fmi3/model.c.jinja" => Some(rumoca_compile::codegen::templates::FMI3_MODEL),
        "fmi3/test_driver.c.jinja" => Some(rumoca_compile::codegen::templates::FMI3_TEST_DRIVER),
        _ => None,
    }
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
    qualified_name.rsplit('.').next().unwrap_or(qualified_name)
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
    Ok(HighLevelCompilationResult {
        dae: result.dae,
        flat: result.flat,
        resolved,
    })
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

fn seconds_since(started: Instant) -> f64 {
    started.elapsed().as_secs_f64()
}

#[derive(Clone, Copy)]
struct SimRequest<'a> {
    t_end: f64,
    dt: Option<f64>,
    solver: Option<&'a str>,
}

fn simulate_compiled_model(
    result: &HighLevelCompilationResult,
    model_name: &str,
    compile_seconds: f64,
    request: SimRequest<'_>,
) -> Result<String, PyRuntimeStringError> {
    let (solver_mode, solver_label) = RuntimeSimSolverMode::parse_request(request.solver);
    let opts = SimOptions {
        t_end: request.t_end,
        dt: request.dt,
        solver_mode,
        ..SimOptions::default()
    };
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
    request: SimRequest<'_>,
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
    request: SimRequest<'_>,
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
    m.add_function(wrap_pyfunction!(render_template, m)?)?;
    m.add_function(wrap_pyfunction!(render_model, m)?)?;
    m.add_function(wrap_pyfunction!(render_model_file, m)?)?;
    m.add_function(wrap_pyfunction!(render_builtin_model, m)?)?;
    m.add_function(wrap_pyfunction!(render_builtin_file, m)?)?;
    m.add_function(wrap_pyfunction!(get_builtin_templates, m)?)?;
    m.add_function(wrap_pyfunction!(simulate, m)?)?;
    m.add_function(wrap_pyfunction!(simulate_file, m)?)?;
    m.add_class::<ParseResult>()?;
    m.add_class::<LintMessage>()?;
    m.add_class::<ProjectSession>()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn test_version() {
        let v = version();
        assert!(!v.is_empty());
    }

    #[test]
    fn test_parse_valid() {
        let result = parse("model M Real x; end M;", None);
        assert!(result.success);
        assert!(result.error.is_none());
    }

    #[test]
    fn test_parse_invalid() {
        let result = parse("model M Real x end M;", None);
        assert!(!result.success);
        assert!(result.error.is_some());
    }

    #[test]
    fn test_lint() {
        let messages = lint("model m Real x; end m;", None);
        assert!(!messages.is_empty());
    }

    #[test]
    fn test_format_valid() {
        let formatted = format_source("model M Real x; end M;", None).expect("format");
        assert!(formatted.contains("model M"));
        assert!(formatted.ends_with('\n'));
    }

    #[test]
    fn test_format_or_original_invalid() {
        let source = "model M Real x end M;";
        let out = format_or_original(source, None);
        assert_eq!(out, source);
    }

    #[test]
    fn test_get_builtin_templates_includes_sympy() {
        let templates = get_builtin_templates().expect("templates");
        let parsed: Value = serde_json::from_str(&templates).expect("json");
        assert!(parsed.as_array().is_some_and(|items| {
            items
                .iter()
                .any(|item| item.get("id").and_then(Value::as_str) == Some("sympy.py.jinja"))
        }));
    }

    #[test]
    fn test_compile_inline_source() {
        let dae_json = compile(
            "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
            Some("Ball"),
            Some("Ball.mo"),
            None,
        )
        .expect("compile inline");
        let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
        assert_eq!(
            dae.get("class_type").and_then(|v| v.as_str()),
            Some("Model")
        );
        assert!(
            dae.get("x")
                .and_then(|v| v.as_object())
                .is_some_and(|state_map| state_map.contains_key("x"))
        );
    }

    #[test]
    fn test_compile_to_json_returns_canonical_payload() {
        let dae_json = compile_to_json(
            "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
            Some("Ball"),
            Some("Ball.mo"),
            None,
        )
        .expect("compile to json");
        let dae: Value = serde_json::from_str(&dae_json).expect("valid JSON");
        assert!(dae.get("x").is_some(), "canonical payload should contain x");
        assert!(
            dae.get("f_x").is_some(),
            "canonical payload should contain f_x"
        );
    }

    #[test]
    fn test_compile_file_reads_source_from_path() {
        let temp_file = unique_temp_model_path("rumoca_bind_python_ball");
        fs::write(
            &temp_file,
            "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
        )
        .expect("write temp model");

        let dae_json = compile_file(temp_file.to_string_lossy().as_ref(), Some("Ball"), None)
            .expect("compile from file");
        let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
        assert_eq!(
            dae.get("class_type").and_then(|v| v.as_str()),
            Some("Model")
        );

        let _ = fs::remove_file(temp_file);
    }

    #[test]
    fn test_compile_file_fixture_loads_source_root_and_infers_model() {
        let fixture = fixture_path("UsesLib.mo");
        let source_root = fixture_path("Lib");

        let dae_json = compile_file(
            fixture.to_string_lossy().as_ref(),
            None,
            Some(vec![source_root.to_string_lossy().to_string()]),
        )
        .expect("compile from fixture");
        let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
        assert_eq!(
            dae.get("class_type").and_then(|v| v.as_str()),
            Some("Model")
        );
        assert!(
            dae.get("x")
                .and_then(|v| v.as_object())
                .is_some_and(|state_map| state_map.contains_key("x"))
        );
    }

    #[test]
    fn test_project_session_reuses_fixture_source_root_for_simulation() {
        let fixture = fixture_path("UsesLib.mo");
        let source_root = fixture_path("Lib");
        let mut session =
            ProjectSession::new(Some(vec![source_root.to_string_lossy().to_string()]));

        let dae_json = session
            .compile_file(fixture.to_string_lossy().as_ref(), None)
            .expect("compile from project session");
        let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
        assert_eq!(
            dae.get("class_type").and_then(|v| v.as_str()),
            Some("Model")
        );

        let statuses = session.source_root_statuses().expect("statuses");
        let parsed: Value = serde_json::from_str(&statuses).expect("status json");
        assert!(
            parsed.as_array().is_some_and(|items| !items.is_empty()),
            "expected loaded source root status after compile"
        );

        let sim_json = session
            .simulate_file(
                fixture.to_string_lossy().as_ref(),
                None,
                0.2,
                Some(0.1),
                Some("auto"),
            )
            .expect("simulate fixture");
        let sim: Value = serde_json::from_str(&sim_json).expect("valid sim JSON");
        assert!(
            sim.get("metrics")
                .and_then(|value| value.get("points"))
                .and_then(Value::as_u64)
                .is_some_and(|points| points >= 2)
        );
    }

    #[test]
    fn test_render_builtin_file_uses_native_dae_template() {
        let fixture = fixture_path("UsesLib.mo");
        let source_root = fixture_path("Lib");
        let rendered = render_builtin_file(
            fixture.to_string_lossy().as_ref(),
            "dae_modelica.mo.jinja",
            None,
            Some(vec![source_root.to_string_lossy().to_string()]),
        )
        .expect("render built-in template");
        assert!(rendered.contains("class UsesLib"));
    }

    #[test]
    fn test_compile_source_alias_matches_compile() {
        let dae_json = compile_source(
            "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
            Some("Ball"),
            Some("Ball.mo"),
            None,
        )
        .expect("compile via alias");
        let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
        assert_eq!(
            dae.get("class_type").and_then(|v| v.as_str()),
            Some("Model")
        );
    }

    #[test]
    fn test_compile_rejects_empty_source() {
        let err =
            compile("", Some("Ball"), Some("Ball.mo"), None).expect_err("empty source should fail");
        assert!(!err.0.is_empty());
    }

    #[test]
    fn test_compile_file_rejects_missing_file() {
        let err =
            compile_file("missing.mo", Some("Ball"), None).expect_err("missing file should fail");
        assert!(err.0.contains("Failed to read"));
    }

    #[test]
    fn test_load_source_roots_reports_fixture_load() {
        let source_root = fixture_path("Lib");
        let mut session =
            ProjectSession::new(Some(vec![source_root.to_string_lossy().to_string()]));
        let summary = session.load_source_roots().expect("load roots");
        let parsed: Value = serde_json::from_str(&summary).expect("summary json");
        assert!(
            parsed
                .get("reports")
                .and_then(Value::as_array)
                .is_some_and(|reports| !reports.is_empty())
        );
    }

    fn fixture_path(relative: &str) -> std::path::PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("fixtures")
            .join(relative)
    }

    fn unique_temp_model_path(stem: &str) -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time")
            .as_nanos();
        std::env::temp_dir().join(format!("{stem}_{nanos}.mo"))
    }
}
