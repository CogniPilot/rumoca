//! The reusable [`Session`] and the module-level convenience functions that
//! delegate to a default session (`rumoca.load`, `rumoca.loads`, ...).

use pyo3::prelude::*;
use pyo3::types::PyType;
use rumoca_compile::compile::{Session as CompileSession, SessionConfig};
use std::path::Path;

use crate::diagnostics::Diagnostic;
use crate::error::{ApiError, ApiResult};
use crate::model::{Model, RecompileContext, SimConfig};
use crate::{
    PyRuntimeStringError, compile_source_in_session, project_configured_source_roots,
    refresh_effective_source_root_paths, resolve_source_root_paths,
};

/// A reusable compiler session that retains loaded source roots and caches
/// across calls. Free functions (`rumoca.load`, ...) delegate to a default one.
#[pyclass(module = "rumoca", unsendable)]
pub struct Session {
    session: CompileSession,
    source_root_paths: Vec<String>,
    effective_source_root_paths: Vec<String>,
}

impl Session {
    fn build(source_root_paths: Vec<String>) -> Self {
        let effective_source_root_paths = resolve_source_root_paths(&source_root_paths);
        Self {
            session: CompileSession::new(SessionConfig::default()),
            source_root_paths,
            effective_source_root_paths,
        }
    }

    fn sync(&mut self) {
        refresh_effective_source_root_paths(
            &mut self.session,
            &self.source_root_paths,
            &mut self.effective_source_root_paths,
        );
    }
}

#[pymethods]
impl Session {
    #[new]
    #[pyo3(signature = (roots=None, *, workspace=None))]
    fn new(
        roots: Option<Vec<String>>,
        workspace: Option<&str>,
    ) -> std::result::Result<Self, PyRuntimeStringError> {
        let explicit = roots.unwrap_or_default();
        let resolved =
            project_configured_source_roots(explicit, workspace, None, None, "simulate")?;
        Ok(Self::build(resolved))
    }

    /// Build a session, compile the model, and read the solver config from a
    /// `rumoca-scenario*.toml` file in one call, returning `(session, model,
    /// config)`. The model's `.mo` and source roots are resolved relative to the
    /// scenario file; `config` carries its solver/dt — pass it to
    /// `model.simulate(..., config=config)`. (The scenario's `t_end` is the sim
    /// span, supplied to `simulate(t=...)`, so it is not part of `config`.)
    #[classmethod]
    #[pyo3(signature = (path))]
    fn from_scenario(
        _cls: &Bound<'_, PyType>,
        path: &str,
    ) -> ApiResult<(Session, Model, SimConfig)> {
        let scenario_path = Path::new(path);
        let text = std::fs::read_to_string(scenario_path)
            .map_err(|e| ApiError::Compile(format!("Failed to read {path}: {e}")))?;
        let config = rumoca_compile::scenario::parse_scenario_config_file(&text)
            .map_err(|e| ApiError::Compile(e.to_string()))?;
        let model_name = config
            .model
            .name
            .ok_or_else(|| ApiError::Compile(format!("scenario {path:?} has no [model] name")))?;
        let model_file = config
            .model
            .file
            .ok_or_else(|| ApiError::Compile(format!("scenario {path:?} has no [model] file")))?;

        // Resolve the .mo and source roots relative to the scenario file's dir.
        let base = scenario_path.parent().unwrap_or_else(|| Path::new("."));
        let model_path = base.join(&model_file).to_string_lossy().into_owned();
        let roots: Vec<String> = config
            .source_roots
            .iter()
            .map(|root| base.join(root).to_string_lossy().into_owned())
            .collect();

        let mut session = Self::build(roots);
        let model = session.load(&model_path, Some(model_name.as_str()))?;
        let config = SimConfig {
            solver: config.sim.solver,
            rtol: None,
            atol: None,
            dt: config.sim.dt,
            max_wall_seconds: None,
        };
        Ok((session, model, config))
    }

    /// The configured source roots.
    #[getter]
    fn roots(&self) -> Vec<String> {
        self.source_root_paths.clone()
    }

    /// Compile a model from a file and return a [`Model`].
    #[pyo3(signature = (path, *, model=None))]
    fn load(&mut self, path: &str, model: Option<&str>) -> ApiResult<Model> {
        self.sync();
        // Read the source ourselves (rather than `compile_file_in_session`) so we
        // can retain it on the Model for structural recompiles.
        let source = std::fs::read_to_string(path)
            .map_err(|e| ApiError::Compile(format!("Failed to read {path}: {e}")))?;
        let (result, name) = compile_source_in_session(
            &mut self.session,
            &source,
            model,
            path,
            &self.effective_source_root_paths,
        )
        .map_err(compile_err)?;
        Ok(Model::new(
            name,
            result,
            RecompileContext {
                source,
                filename: path.to_string(),
                roots: self.effective_source_root_paths.clone(),
            },
        ))
    }

    /// Compile a model from inline source and return a [`Model`].
    #[pyo3(signature = (source, *, model=None, filename=None))]
    fn loads(
        &mut self,
        source: &str,
        model: Option<&str>,
        filename: Option<&str>,
    ) -> ApiResult<Model> {
        self.sync();
        let filename = filename.unwrap_or("input.mo");
        let (result, name) = compile_source_in_session(
            &mut self.session,
            source,
            model,
            filename,
            &self.effective_source_root_paths,
        )
        .map_err(compile_err)?;
        Ok(Model::new(
            name,
            result,
            RecompileContext {
                source: source.to_string(),
                filename: filename.to_string(),
                roots: self.effective_source_root_paths.clone(),
            },
        ))
    }

    fn clear(&mut self) {
        self.session = CompileSession::new(SessionConfig::default());
    }

    fn __repr__(&self) -> String {
        format!("Session(roots={})", self.source_root_paths.len())
    }
}

/// Map an internal compile error to a typed [`ApiError::Compile`].
fn compile_err(err: PyRuntimeStringError) -> ApiError {
    ApiError::Compile(err.0)
}

// ── module-level functions (delegate to a fresh default session) ────────────

/// Load and compile a model from a file.
#[pyfunction]
#[pyo3(signature = (path, *, model=None, roots=None))]
pub fn load(path: &str, model: Option<&str>, roots: Option<Vec<String>>) -> ApiResult<Model> {
    let mut session = Session::build(resolve_roots(roots));
    session.load(path, model)
}

/// Load and compile a model from inline source.
#[pyfunction]
#[pyo3(signature = (source, *, model=None, filename=None, roots=None))]
pub fn loads(
    source: &str,
    model: Option<&str>,
    filename: Option<&str>,
    roots: Option<Vec<String>>,
) -> ApiResult<Model> {
    let mut session = Session::build(resolve_roots(roots));
    session.loads(source, model, filename)
}

fn resolve_roots(roots: Option<Vec<String>>) -> Vec<String> {
    roots.unwrap_or_default()
}

/// Validate a model file, returning all diagnostics. Never raises on model
/// errors (only on I/O failure reading the file).
#[pyfunction]
#[pyo3(signature = (path, *, model=None))]
pub fn validate(
    path: &str,
    model: Option<&str>,
) -> std::result::Result<Vec<Diagnostic>, PyRuntimeStringError> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| PyRuntimeStringError(format!("Failed to read {path}: {e}")))?;
    let _ = model;
    Ok(crate::diagnostics_for_source(&source, path))
}

/// Validate inline source, returning all diagnostics. Never raises on model
/// errors.
#[pyfunction]
#[pyo3(signature = (source, *, model=None, filename=None))]
pub fn validate_source(
    source: &str,
    model: Option<&str>,
    filename: Option<&str>,
) -> Vec<Diagnostic> {
    let _ = model;
    crate::diagnostics_for_source(source, filename.unwrap_or("input.mo"))
}
