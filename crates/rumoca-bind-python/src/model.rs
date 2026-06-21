//! The Python `Model` hub — a presentation convenience over the compiled
//! artifact. Introspection, IR access, structure, codegen, and simulation all
//! hang off it, backed directly by the Rust `CompilationResult` (no JSON).

use ::rumoca::CompilationResult as HighLevelCompilationResult;
use pyo3::prelude::*;
use pyo3::types::{PyBool, PyDict, PyFloat, PyInt};
use rumoca_compile::compile::{Session as CompileSession, SessionConfig, StructuralOverride};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;

use rumoca_sim::{SimOptions, SimSolverMode, simulate_with_diagnostics};

/// Solver controls, bundled so `Model.simulate` stays ergonomic without a long
/// argument list. Mirrors the roadmap `SimConfig`. The common `dt`/`solver`
/// remain direct `simulate` kwargs (and override the config when both are set).
#[pyclass(module = "rumoca")]
#[derive(Clone, Default)]
pub struct SimConfig {
    #[pyo3(get, set)]
    pub solver: Option<String>,
    #[pyo3(get, set)]
    pub rtol: Option<f64>,
    #[pyo3(get, set)]
    pub atol: Option<f64>,
    #[pyo3(get, set)]
    pub dt: Option<f64>,
    #[pyo3(get, set)]
    pub max_wall_seconds: Option<f64>,
}

#[pymethods]
impl SimConfig {
    #[new]
    #[pyo3(signature = (*, solver=None, rtol=None, atol=None, dt=None, max_wall_seconds=None))]
    fn new(
        solver: Option<String>,
        rtol: Option<f64>,
        atol: Option<f64>,
        dt: Option<f64>,
        max_wall_seconds: Option<f64>,
    ) -> Self {
        Self {
            solver,
            rtol,
            atol,
            dt,
            max_wall_seconds,
        }
    }

    fn __repr__(&self) -> String {
        format!(
            "SimConfig(solver={:?}, rtol={:?}, atol={:?}, dt={:?}, max_wall_seconds={:?})",
            self.solver, self.rtol, self.atol, self.dt, self.max_wall_seconds
        )
    }
}

use crate::codegen::CodegenResult;
use crate::error::{ApiError, ApiResult};
use crate::metadata::{ParamView, ParameterInfo, VarView, VariableInfo};
use crate::result::Result as SimPyResult;
use crate::{PyRuntimeStringError, render_target_files};

/// Structural summary of a compiled model: variable/equation counts plus the
/// BLT (block lower-triangular) decomposition — how many blocks the solve splits
/// into and whether any are coupled algebraic loops.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct StructuralInfo {
    #[pyo3(get)]
    pub n_states: usize,
    #[pyo3(get)]
    pub n_algebraic: usize,
    #[pyo3(get)]
    pub n_outputs: usize,
    #[pyo3(get)]
    pub n_equations: usize,
    #[pyo3(get)]
    pub n_unknowns: usize,
    #[pyo3(get)]
    pub is_balanced: bool,
    /// Whether the system is structurally non-singular (a full equation↔unknown
    /// matching exists). `False` means the BLT could not be built — the system is
    /// structurally singular, and the BLT fields below stay at zero.
    #[pyo3(get)]
    pub is_matched: bool,
    /// Number of BLT blocks the causalized solve splits into (`0` if unmatched).
    #[pyo3(get)]
    pub n_blocks: usize,
    /// Number of coupled blocks (algebraic loops solved simultaneously).
    #[pyo3(get)]
    pub n_algebraic_loops: usize,
    /// Size of the largest algebraic loop (`0` if there are none).
    #[pyo3(get)]
    pub largest_algebraic_loop: usize,
}

#[pymethods]
impl StructuralInfo {
    fn __repr__(&self) -> String {
        format!(
            "StructuralInfo(states={}, algebraic={}, outputs={}, equations={}, unknowns={}, \
             balanced={}, matched={}, blocks={}, algebraic_loops={})",
            self.n_states,
            self.n_algebraic,
            self.n_outputs,
            self.n_equations,
            self.n_unknowns,
            self.is_balanced,
            self.is_matched,
            self.n_blocks,
            self.n_algebraic_loops,
        )
    }
}

/// The inputs needed to *recompile* a model — retained so `with_params(
/// recompile=True)` can re-instantiate with structural overrides. Shared via
/// `Rc` so tunable handles don't copy the source.
pub(crate) struct RecompileContext {
    pub(crate) source: String,
    pub(crate) filename: String,
    pub(crate) roots: Vec<String>,
}

#[pyclass(module = "rumoca", unsendable)]
pub struct Model {
    name: String,
    // Shared so a tunable `with_params`/`with_start` handle reuses the compiled
    // structural artifact (a 100-point tunable sweep compiles once).
    result: Rc<HighLevelCompilationResult>,
    /// Source/roots to recompile from (for structural `with_params(recompile=)`).
    recompile: Rc<RecompileContext>,
    /// Pending tunable parameter overrides (from `with_params`), name→value.
    param_overrides: Vec<(String, f64)>,
    /// Pending state-start overrides (from `with_start`), name→value.
    start_overrides: Vec<(String, f64)>,
}

impl Model {
    pub(crate) fn new(
        name: String,
        result: HighLevelCompilationResult,
        recompile: RecompileContext,
    ) -> Self {
        Self::from_parts(name, result, Rc::new(recompile))
    }

    fn from_parts(
        name: String,
        result: HighLevelCompilationResult,
        recompile: Rc<RecompileContext>,
    ) -> Self {
        Self {
            name,
            result: Rc::new(result),
            recompile,
            param_overrides: Vec::new(),
            start_overrides: Vec::new(),
        }
    }

    /// A new handle sharing the same compiled artifact, with overrides merged in.
    fn with_overrides(
        &self,
        extra_params: &[(String, f64)],
        extra_start: &[(String, f64)],
    ) -> Self {
        Self {
            name: self.name.clone(),
            result: Rc::clone(&self.result),
            recompile: Rc::clone(&self.recompile),
            param_overrides: merge_overrides(&self.param_overrides, extra_params),
            start_overrides: merge_overrides(&self.start_overrides, extra_start),
        }
    }
}

/// Convert an optional `{name: value}` mapping to override pairs.
fn to_pairs(map: Option<HashMap<String, f64>>) -> Vec<(String, f64)> {
    map.map(|m| m.into_iter().collect()).unwrap_or_default()
}

/// Convert an optional `**kwargs` dict to override pairs (each value as `f64`).
fn dict_to_pairs(dict: Option<&Bound<'_, PyDict>>) -> ApiResult<Vec<(String, f64)>> {
    match dict {
        Some(dict) => {
            let map: HashMap<String, f64> = dict.extract()?;
            Ok(map.into_iter().collect())
        }
        None => Ok(Vec::new()),
    }
}

/// Convert a `**kwargs` dict to typed structural overrides, preserving the
/// Python value kind (bool → Boolean, int → Integer, float → Real) so the
/// re-instantiated literal matches the parameter's declared type — e.g. an
/// Integer dimension stays an integer, a Boolean gate stays a boolean.
fn dict_to_structural(
    dict: Option<&Bound<'_, PyDict>>,
) -> ApiResult<Vec<(String, StructuralOverride)>> {
    let Some(dict) = dict else {
        return Ok(Vec::new());
    };
    let mut out = Vec::with_capacity(dict.len());
    for (key, value) in dict.iter() {
        let name: String = key.extract()?;
        // bool is a subclass of int in Python, so check it first.
        let value = if value.is_instance_of::<PyBool>() {
            StructuralOverride::Bool(value.extract()?)
        } else if value.is_instance_of::<PyInt>() {
            StructuralOverride::Int(value.extract()?)
        } else if value.is_instance_of::<PyFloat>() {
            StructuralOverride::Real(value.extract()?)
        } else {
            return Err(ApiError::StructuralParam(format!(
                "structural override {name:?} must be a bool, int, or float"
            )));
        };
        out.push((name, value));
    }
    Ok(out)
}

/// Merge override lists, last value winning per name, preserving first-seen order.
fn merge_overrides(base: &[(String, f64)], extra: &[(String, f64)]) -> Vec<(String, f64)> {
    let mut out: Vec<(String, f64)> = base.to_vec();
    for (name, value) in extra {
        if let Some(slot) = out.iter_mut().find(|(n, _)| n == name) {
            slot.1 = *value;
        } else {
            out.push((name.clone(), *value));
        }
    }
    out
}

/// Translate the `t` argument (float end, `(start, end)` tuple) into a
/// `(t_start, t_end)` pair.
fn parse_time_span(t: &Bound<'_, PyAny>) -> ApiResult<(f64, f64)> {
    if let Ok(end) = t.extract::<f64>() {
        return Ok((0.0, end));
    }
    if let Ok((start, end)) = t.extract::<(f64, f64)>() {
        return Ok((start, end));
    }
    Err(ApiError::Sim(
        "t must be a float end-time or a (start, end) tuple".to_string(),
    ))
}

#[pymethods]
impl Model {
    #[getter]
    fn name(&self) -> String {
        self.name.clone()
    }

    #[getter]
    fn states(&self) -> VarView {
        VarView::new(
            self.result
                .dae
                .variables
                .states
                .iter()
                .map(|(k, v)| VariableInfo::from_variable(k.as_str(), v))
                .collect(),
        )
    }

    #[getter]
    fn algebraics(&self) -> VarView {
        VarView::new(
            self.result
                .dae
                .variables
                .algebraics
                .iter()
                .map(|(k, v)| VariableInfo::from_variable(k.as_str(), v))
                .collect(),
        )
    }

    #[getter]
    fn inputs(&self) -> VarView {
        VarView::new(
            self.result
                .dae
                .variables
                .inputs
                .iter()
                .map(|(k, v)| VariableInfo::from_variable(k.as_str(), v))
                .collect(),
        )
    }

    #[getter]
    fn outputs(&self) -> VarView {
        VarView::new(
            self.result
                .dae
                .variables
                .outputs
                .iter()
                .map(|(k, v)| VariableInfo::from_variable(k.as_str(), v))
                .collect(),
        )
    }

    #[getter]
    fn parameters(&self) -> ParamView {
        ParamView::new(
            self.result
                .dae
                .variables
                .parameters
                .iter()
                .map(|(k, v)| ParameterInfo::from_variable(k.as_str(), v))
                .collect(),
        )
    }

    fn summary(&self) -> String {
        let v = &self.result.dae.variables;
        format!(
            "{} — {} states, {} algebraic, {} inputs, {} outputs, {} parameters",
            self.name,
            v.states.len(),
            v.algebraics.len(),
            v.inputs.len(),
            v.outputs.len(),
            v.parameters.len(),
        )
    }

    fn structure(&self) -> StructuralInfo {
        let b = &self.result.balance_detail;
        let (n_eq, n_unk) = b.equations_unknowns();
        let mut info = StructuralInfo {
            n_states: self.result.dae.variables.states.len(),
            n_algebraic: self.result.dae.variables.algebraics.len(),
            n_outputs: self.result.dae.variables.outputs.len(),
            n_equations: n_eq,
            n_unknowns: n_unk,
            is_balanced: b.is_balanced(),
            is_matched: false,
            n_blocks: 0,
            n_algebraic_loops: 0,
            largest_algebraic_loop: 0,
        };
        // BLT decomposition from the same scalarized analysis `--inspect
        // structure` uses. A structurally singular system has no full matching,
        // so the report errors — leave the BLT fields at their unmatched defaults.
        if let Ok(report) =
            rumoca_sim::structural_report_for_dae(&self.result.dae, &SimOptions::default())
        {
            info.is_matched = true;
            // Source the scalar counts from the same (prepared) system the BLT
            // comes from, so equations/unknowns and blocks stay consistent.
            info.n_equations = report.n_equations;
            info.n_unknowns = report.n_unknowns;
            info.n_blocks = report.blocks.len();
            info.n_algebraic_loops = report.coupled_block_count();
            let largest = report.largest_coupled_block();
            // `largest_coupled_block` returns 1 for a fully-sequential system;
            // report 0 when there are no loops so the field reads honestly.
            info.largest_algebraic_loop = if info.n_algebraic_loops == 0 {
                0
            } else {
                largest
            };
        }
        info
    }

    #[pyo3(signature = (stage="dae"))]
    fn to_dict(
        &self,
        py: Python<'_>,
        stage: &str,
    ) -> std::result::Result<PyObject, PyRuntimeStringError> {
        let value = self.stage_value(stage)?;
        Ok(crate::json_value_to_py(py, &value))
    }

    #[pyo3(signature = (stage="dae", pretty=true))]
    fn to_json(
        &self,
        stage: &str,
        pretty: bool,
    ) -> std::result::Result<String, PyRuntimeStringError> {
        let value = self.stage_value(stage)?;
        if pretty {
            serde_json::to_string_pretty(&value)
        } else {
            serde_json::to_string(&value)
        }
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
    }

    #[pyo3(signature = (path, stage="dae"))]
    fn save_json(&self, path: &str, stage: &str) -> std::result::Result<(), PyRuntimeStringError> {
        let text = self.to_json(stage, true)?;
        std::fs::write(path, text)
            .map_err(|e| PyRuntimeStringError(format!("Failed to write {path}: {e}")))
    }

    /// Render a codegen target and return its single concatenated content string.
    /// Use [`Model::codegen`] for the per-file result.
    fn render(&self, target: &str) -> std::result::Result<String, PyRuntimeStringError> {
        let files = render_target_files(&self.result, &self.name, target)?;
        Ok(CodegenResult::new(target.to_string(), files).joined_content())
    }

    fn codegen(&self, target: &str) -> std::result::Result<CodegenResult, PyRuntimeStringError> {
        let files = render_target_files(&self.result, &self.name, target)?;
        Ok(CodegenResult::new(target.to_string(), files))
    }

    #[pyo3(signature = (
        t=None, *, dt=None, config=None, params=None, start=None, inputs=None
    ))]
    fn simulate(
        &self,
        t: Option<&Bound<'_, PyAny>>,
        dt: Option<f64>,
        config: Option<SimConfig>,
        params: Option<HashMap<String, f64>>,
        start: Option<HashMap<String, f64>>,
        inputs: Option<&Bound<'_, PyAny>>,
    ) -> ApiResult<SimPyResult> {
        if inputs.is_some() {
            return Err(ApiError::Sim(
                "time-varying inputs= are a fast-follower (roadmap §8); not yet available"
                    .to_string(),
            ));
        }

        // Merge with_params/with_start handles with call-time overrides
        // (call-time wins), then validate before building options.
        let param_overrides = merge_overrides(&self.param_overrides, &to_pairs(params));
        let start_overrides = merge_overrides(&self.start_overrides, &to_pairs(start));
        self.validate_param_overrides(&param_overrides)?;
        self.validate_start_overrides(&start_overrides)?;

        let (t_start, t_end) = match t {
            Some(t) => parse_time_span(t)?,
            None => (0.0, 1.0),
        };
        let mut opts = self.sim_options(t_start, t_end, dt, config.unwrap_or_default());
        opts.param_overrides = param_overrides;
        opts.start_overrides = start_overrides;

        // Release the GIL for the solve; `with_gil` is cheap when already held.
        // Bind `&Dae` before the closure so it captures a `Send` reference, not
        // the `!Send` `Rc`. The solver-neutral dispatcher honors `solver_mode`
        // (auto/bdf via diffsol, rk-like via rk45), applying overrides identically.
        let dae = &self.result.dae;
        let started = Instant::now();
        let sim = Python::with_gil(|py| py.allow_threads(|| simulate_with_diagnostics(dae, &opts)))
            .map_err(|e| ApiError::Sim(format!("{e}")))?;
        let simulate_seconds = started.elapsed().as_secs_f64();
        Ok(SimPyResult::from_sim(
            self.name.clone(),
            sim,
            None,
            Some(simulate_seconds),
        ))
    }

    // ── live symbolic exports — render the existing target in memory, then
    //    exec+wrap it (roadmap §5.3). Shares the same tested templates as
    //    `codegen()`, so the live path cannot drift from the file path.
    #[pyo3(signature = (form="dae", *, mode="mx"))]
    fn to_casadi(&self, form: &str, mode: &str) -> ApiResult<PyObject> {
        let target = match (form, mode) {
            ("dae", "mx") => "casadi-mx",
            ("dae", "sx") => "casadi-sx",
            ("solve", "mx" | "sx") => "casadi-solve",
            _ => {
                return Err(ApiError::NotImplemented(format!(
                    "unsupported casadi form/mode: form={form:?}, mode={mode:?} \
                     (form is \"dae\"|\"solve\", mode is \"mx\"|\"sx\")"
                )));
            }
        };
        self.live_export("build_casadi", target, form)
    }

    /// Live JAX export. `form="dae"` (default) yields a richly-typed `JaxModel`
    /// with an explicit `ode_fn` + diffrax `simulate`; `form="solve"` taps the
    /// lower-level `jax-solve` target (returned as a generic module wrapper).
    #[pyo3(signature = (form="dae"))]
    fn to_jax(&self, form: &str) -> ApiResult<PyObject> {
        let target = match form {
            "dae" => "jax",
            "solve" => "jax-solve",
            _ => {
                return Err(ApiError::NotImplemented(format!(
                    "unsupported jax form: {form:?} (expected \"dae\" or \"solve\")"
                )));
            }
        };
        self.live_export("build_jax", target, form)
    }

    fn to_sympy(&self) -> ApiResult<PyObject> {
        self.live_export("build_sympy", "sympy", "dae")
    }

    /// A new `Model` handle with tunable parameter overrides applied at the next
    /// `simulate` — sharing this model's compiled artifact, so a tunable sweep
    /// compiles once. `recompile=True` (structural re-instantiation) is the
    /// compile-API path and not yet available.
    #[pyo3(signature = (*, recompile=false, **overrides))]
    fn with_params(
        &self,
        recompile: bool,
        overrides: Option<Bound<'_, PyDict>>,
    ) -> ApiResult<Model> {
        if recompile {
            // Structural override: re-instantiate with the values injected at the
            // root (re-evaluates dimensions / conditional components).
            let structural = dict_to_structural(overrides.as_ref())?;
            return self.recompile_with_structural(&structural);
        }
        let pairs = dict_to_pairs(overrides.as_ref())?;
        // Fail fast: a tunable handle must only carry validly-tunable overrides.
        self.validate_param_overrides(&pairs)?;
        Ok(self.with_overrides(&pairs, &[]))
    }

    /// A new `Model` handle with state start-value overrides (initial-guess
    /// seeds for the init solve) applied at the next `simulate`.
    #[pyo3(signature = (**overrides))]
    fn with_start(&self, overrides: Option<Bound<'_, PyDict>>) -> ApiResult<Model> {
        let pairs = dict_to_pairs(overrides.as_ref())?;
        // Fail fast, mirroring `with_params`: reject unknown state names now.
        self.validate_start_overrides(&pairs)?;
        Ok(self.with_overrides(&[], &pairs))
    }

    fn __repr__(&self) -> String {
        format!("Model({})", self.summary())
    }

    fn _repr_html_(&self) -> String {
        let v = &self.result.dae.variables;
        format!(
            "<b>Model</b> <code>{}</code><ul>\
             <li>{} states</li><li>{} algebraic</li><li>{} inputs</li>\
             <li>{} outputs</li><li>{} parameters</li></ul>",
            self.name,
            v.states.len(),
            v.algebraics.len(),
            v.inputs.len(),
            v.outputs.len(),
            v.parameters.len(),
        )
    }
}

impl Model {
    /// Validate tunable parameter overrides against the compiled model, raising a
    /// precise typed error: unknown name (`KeyError`) or structural parameter
    /// (`StructuralParamError`, needs recompile). Dependent parameters (e.g.
    /// `b = 2*a`) are *not* rejected here — the sim layer propagates the override
    /// to them, and rejects only if a dependent genuinely can't be re-evaluated.
    fn validate_param_overrides(&self, overrides: &[(String, f64)]) -> ApiResult<()> {
        if overrides.is_empty() {
            return Ok(());
        }
        let params = &self.result.dae.variables.parameters;
        for (name, _) in overrides {
            let is_tunable = params
                .iter()
                .find(|(key, _)| key.as_str() == name)
                .map(|(_, var)| var.is_tunable);
            match is_tunable {
                None => {
                    let names: Vec<String> =
                        params.keys().map(|k| k.as_str().to_string()).collect();
                    return Err(ApiError::Py(pyo3::exceptions::PyKeyError::new_err(
                        crate::unknown_name_message("parameter", name, &names),
                    )));
                }
                Some(false) => {
                    return Err(ApiError::StructuralParam(format!(
                        "{name:?} is a structural parameter (it affects sizing/instantiation); \
                         change it by recompiling, not by a simulation override"
                    )));
                }
                Some(true) => {}
            }
        }
        Ok(())
    }

    /// Validate state start-value overrides: each name must be a declared state
    /// (a subscripted spelling like `v[1]` resolves to its base state `v`). An
    /// unknown name raises `KeyError`, matching the parameter-override path.
    fn validate_start_overrides(&self, overrides: &[(String, f64)]) -> ApiResult<()> {
        if overrides.is_empty() {
            return Ok(());
        }
        let states = &self.result.dae.variables.states;
        for (name, _) in overrides {
            let base = name.split('[').next().unwrap_or(name);
            let known = states
                .keys()
                .any(|key| key.as_str() == name || key.as_str() == base);
            if !known {
                let names: Vec<String> = states.keys().map(|k| k.as_str().to_string()).collect();
                return Err(ApiError::Py(pyo3::exceptions::PyKeyError::new_err(
                    crate::unknown_name_message("state", name, &names),
                )));
            }
        }
        Ok(())
    }

    /// Re-instantiate this model with structural parameter overrides injected at
    /// the root, returning a fresh `Model`. Uses the compile-API
    /// `set_structural_overrides` (synthetic root modification) so dimensions and
    /// conditional components re-evaluate — never source edits or IR patches.
    fn recompile_with_structural(
        &self,
        overrides: &[(String, StructuralOverride)],
    ) -> ApiResult<Model> {
        let ctx = &self.recompile;
        let mut session = CompileSession::new(SessionConfig::default());
        session.set_structural_overrides(overrides);
        let (result, name) = crate::compile_source_in_session(
            &mut session,
            &ctx.source,
            Some(&self.name),
            &ctx.filename,
            &ctx.roots,
        )
        .map_err(|e| ApiError::Compile(e.0))?;
        // The recompiled model has identical source/filename/roots — share the
        // recompile context rather than cloning the source again.
        Ok(Model::from_parts(name, result, Rc::clone(&self.recompile)))
    }

    /// Render a python codegen target in memory and hand its source to the
    /// `rumoca._export` builder, which exec+wraps it into a live typed object.
    fn live_export(&self, builder: &str, target: &str, form: &str) -> ApiResult<PyObject> {
        let files = render_target_files(&self.result, &self.name, target)?;
        let content = CodegenResult::new(target.to_string(), files).joined_content();
        Python::with_gil(|py| {
            let module = py.import_bound("rumoca._export")?;
            let obj = module.call_method1(builder, (content, self.name.clone(), form))?;
            Ok(obj.into_py(py))
        })
    }

    /// Resolve solver controls: the explicit `dt` kwarg wins over `config.dt`,
    /// which wins over `SimOptions` defaults. The solver is selected from
    /// `config.solver` (defaulting to auto).
    fn sim_options(
        &self,
        t_start: f64,
        t_end: f64,
        dt: Option<f64>,
        config: SimConfig,
    ) -> SimOptions {
        let (solver_mode, _label) = SimSolverMode::parse_request(config.solver.as_deref());
        let mut opts = SimOptions {
            t_start,
            t_end,
            dt: dt.or(config.dt),
            solver_mode,
            ..SimOptions::default()
        };
        if let Some(rtol) = config.rtol {
            opts.rtol = rtol;
        }
        if let Some(atol) = config.atol {
            opts.atol = atol;
        }
        opts.max_wall_seconds = config.max_wall_seconds;
        opts
    }

    fn stage_value(
        &self,
        stage: &str,
    ) -> std::result::Result<serde_json::Value, PyRuntimeStringError> {
        match stage {
            "dae" => serde_json::to_value(&self.result.dae),
            "flat" => serde_json::to_value(&self.result.flat),
            other => {
                return Err(PyRuntimeStringError(format!(
                    "stage {other:?} is not yet available via to_dict/to_json (have: \"dae\", \"flat\"); \
                     use codegen()/render() for solve-stage targets"
                )));
            }
        }
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
    }
}
