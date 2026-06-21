//! Typed variable / parameter metadata and the Pythonic views over them.
//!
//! These are first-class PyO3 classes backed directly by the compiled DAE
//! variables — no JSON round-trip. `Model.states["body.v[1]"]` resolves a
//! flattened/subscripted name; `Model.parameters` autocompletes typed
//! [`ParameterInfo`] objects with `.value` and `.kind`.

use pyo3::exceptions::{PyIndexError, PyKeyError};
use pyo3::prelude::*;
use rumoca_core::{Expression, Literal, OpUnary};
use rumoca_ir_dae::Variable;

/// Reduce a constant expression (the common parameter/start/min/max/nominal
/// case) to an `f64`. Returns `None` for non-constant or non-numeric bindings
/// rather than guessing — callers surface that as "unknown", never a silent 0.
fn literal_f64(expr: &Expression) -> Option<f64> {
    match expr {
        Expression::Literal { value, .. } => match value {
            Literal::Real(v) => Some(*v),
            Literal::Integer(v) => Some(*v as f64),
            Literal::Boolean(v) => Some(if *v { 1.0 } else { 0.0 }),
            Literal::String(_) => None,
        },
        Expression::Unary { op, rhs, .. } => {
            let inner = literal_f64(rhs)?;
            match op {
                OpUnary::Minus | OpUnary::DotMinus => Some(-inner),
                OpUnary::Plus | OpUnary::DotPlus => Some(inner),
                OpUnary::Not | OpUnary::Empty => None,
            }
        }
        _ => None,
    }
}

fn opt_literal(expr: &Option<Expression>) -> Option<f64> {
    expr.as_ref().and_then(literal_f64)
}

/// Classification of a parameter's effect on the compiled artifact.
///
/// * `tunable` — value can change without recompilation (sim-API override).
/// * `structural` — value participates in instantiation/sizing (e.g.
///   `Evaluate=true`, or an Integer/Boolean used for sizing), so changing it
///   requires re-instantiation (compile-API override).
///
/// Derived from the DAE variable's `is_tunable` flag, which the compiler already
/// computes per MLS §18.3 / FMI tunability.
const PARAM_KIND_TUNABLE: &str = "tunable";
const PARAM_KIND_STRUCTURAL: &str = "structural";

fn param_kind(is_tunable: bool) -> String {
    if is_tunable {
        PARAM_KIND_TUNABLE
    } else {
        PARAM_KIND_STRUCTURAL
    }
    .to_string()
}

/// Typed metadata for a single model variable (state/algebraic/input/output).
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct VariableInfo {
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub unit: Option<String>,
    #[pyo3(get)]
    pub quantity: Option<String>,
    #[pyo3(get)]
    pub min: Option<f64>,
    #[pyo3(get)]
    pub max: Option<f64>,
    #[pyo3(get)]
    pub nominal: Option<f64>,
    #[pyo3(get)]
    pub fixed: bool,
    #[pyo3(get)]
    pub description: Option<String>,
    /// Array dimensions (empty for scalars).
    #[pyo3(get)]
    pub dims: Vec<i64>,
}

impl VariableInfo {
    pub(crate) fn from_variable(name: &str, var: &Variable) -> Self {
        Self {
            name: name.to_string(),
            unit: var.unit.clone(),
            // quantity is not yet carried to the DAE boundary (roadmap §5.1).
            quantity: None,
            min: opt_literal(&var.min),
            max: opt_literal(&var.max),
            nominal: opt_literal(&var.nominal),
            fixed: var.fixed.unwrap_or(false),
            description: var.description.clone(),
            dims: var.dims.clone(),
        }
    }
}

#[pymethods]
impl VariableInfo {
    fn __repr__(&self) -> String {
        let mut parts = vec![format!("name={:?}", self.name)];
        if let Some(unit) = &self.unit {
            parts.push(format!("unit={unit:?}"));
        }
        if !self.dims.is_empty() {
            parts.push(format!("dims={:?}", self.dims));
        }
        format!("VariableInfo({})", parts.join(", "))
    }
}

/// Typed metadata for a single parameter — a [`VariableInfo`] plus its value and
/// tunable/structural classification.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct ParameterInfo {
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub unit: Option<String>,
    #[pyo3(get)]
    pub quantity: Option<String>,
    #[pyo3(get)]
    pub min: Option<f64>,
    #[pyo3(get)]
    pub max: Option<f64>,
    #[pyo3(get)]
    pub nominal: Option<f64>,
    #[pyo3(get)]
    pub fixed: bool,
    #[pyo3(get)]
    pub description: Option<String>,
    #[pyo3(get)]
    pub dims: Vec<i64>,
    /// The parameter's bound value (`None` if non-constant / not yet evaluated).
    #[pyo3(get)]
    pub value: Option<f64>,
    /// `"tunable"` or `"structural"` (see module docs).
    #[pyo3(get)]
    pub kind: String,
}

impl ParameterInfo {
    pub(crate) fn from_variable(name: &str, var: &Variable) -> Self {
        Self {
            name: name.to_string(),
            unit: var.unit.clone(),
            quantity: None,
            min: opt_literal(&var.min),
            max: opt_literal(&var.max),
            nominal: opt_literal(&var.nominal),
            fixed: var.fixed.unwrap_or(false),
            description: var.description.clone(),
            dims: var.dims.clone(),
            value: opt_literal(&var.start),
            kind: param_kind(var.is_tunable),
        }
    }
}

#[pymethods]
impl ParameterInfo {
    fn __repr__(&self) -> String {
        let value = self
            .value
            .map_or_else(|| "?".to_string(), |v| v.to_string());
        format!(
            "ParameterInfo(name={:?}, value={value}, kind={:?})",
            self.name, self.kind
        )
    }
}

/// A Pythonic, autocompleting view over a model's variables.
///
/// Behaves like a read-only `Sequence`: `len(view)`, `view[i]`, `view["name"]`,
/// `for v in view`, and `view.names`.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct VarView {
    items: Vec<VariableInfo>,
}

impl VarView {
    pub(crate) fn new(items: Vec<VariableInfo>) -> Self {
        Self { items }
    }
}

/// Resolve a flattened/subscripted name against a set of base names.
///
/// An exact match wins. Otherwise a subscripted spelling like `v[1]` resolves to
/// its base `v` (the scalar columns share one declared [`VariableInfo`]); the
/// subscript itself is validated by the caller's data, not invented here.
fn resolve_named<'a, T>(items: &'a [T], names: &[String], key: &str) -> Option<&'a T> {
    if let Some(idx) = names.iter().position(|n| n == key) {
        return Some(&items[idx]);
    }
    let base = key.split('[').next().unwrap_or(key);
    if base != key
        && let Some(idx) = names.iter().position(|n| n == base)
    {
        return Some(&items[idx]);
    }
    None
}

#[pymethods]
impl VarView {
    #[getter]
    fn names(&self) -> Vec<String> {
        self.items.iter().map(|v| v.name.clone()).collect()
    }

    fn __len__(&self) -> usize {
        self.items.len()
    }

    fn __getitem__(&self, key: &Bound<'_, PyAny>) -> PyResult<VariableInfo> {
        if let Ok(idx) = key.extract::<isize>() {
            let len = self.items.len() as isize;
            let resolved = if idx < 0 { idx + len } else { idx };
            return self
                .items
                .get(
                    usize::try_from(resolved).map_err(|_| {
                        PyIndexError::new_err("VarView index out of range".to_string())
                    })?,
                )
                .cloned()
                .ok_or_else(|| PyIndexError::new_err("VarView index out of range".to_string()));
        }
        let name: String = key.extract()?;
        let names = self.names();
        resolve_named(&self.items, &names, &name)
            .cloned()
            .ok_or_else(|| {
                PyKeyError::new_err(crate::unknown_name_message("variable", &name, &names))
            })
    }

    fn __iter__(slf: PyRef<'_, Self>, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let objs: Vec<PyObject> = slf.items.iter().map(|v| v.clone().into_py(py)).collect();
        let py_list = pyo3::types::PyList::new_bound(py, objs);
        Ok(py_list.as_any().iter()?.into_py(py))
    }

    fn __contains__(&self, key: &str) -> bool {
        let names = self.names();
        resolve_named(&self.items, &names, key).is_some()
    }

    fn __repr__(&self) -> String {
        format!("VarView({} vars: {:?})", self.items.len(), self.names())
    }
}

/// A Pythonic, autocompleting view over a model's parameters.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct ParamView {
    items: Vec<ParameterInfo>,
}

impl ParamView {
    pub(crate) fn new(items: Vec<ParameterInfo>) -> Self {
        Self { items }
    }
}

#[pymethods]
impl ParamView {
    #[getter]
    fn names(&self) -> Vec<String> {
        self.items.iter().map(|v| v.name.clone()).collect()
    }

    fn __len__(&self) -> usize {
        self.items.len()
    }

    fn __getitem__(&self, key: &Bound<'_, PyAny>) -> PyResult<ParameterInfo> {
        if let Ok(idx) = key.extract::<isize>() {
            let len = self.items.len() as isize;
            let resolved = if idx < 0 { idx + len } else { idx };
            return self
                .items
                .get(usize::try_from(resolved).map_err(|_| {
                    PyIndexError::new_err("ParamView index out of range".to_string())
                })?)
                .cloned()
                .ok_or_else(|| PyIndexError::new_err("ParamView index out of range".to_string()));
        }
        let name: String = key.extract()?;
        let names = self.names();
        resolve_named(&self.items, &names, &name)
            .cloned()
            .ok_or_else(|| {
                PyKeyError::new_err(crate::unknown_name_message("parameter", &name, &names))
            })
    }

    fn __iter__(slf: PyRef<'_, Self>, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let objs: Vec<PyObject> = slf.items.iter().map(|v| v.clone().into_py(py)).collect();
        let py_list = pyo3::types::PyList::new_bound(py, objs);
        Ok(py_list.as_any().iter()?.into_py(py))
    }

    fn __contains__(&self, key: &str) -> bool {
        let names = self.names();
        resolve_named(&self.items, &names, key).is_some()
    }

    fn __repr__(&self) -> String {
        format!("ParamView({} params: {:?})", self.items.len(), self.names())
    }
}
