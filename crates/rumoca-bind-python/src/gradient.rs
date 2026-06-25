//! Typed steady-state objective-gradient result for `Model.objective_gradient`.
//!
//! No JSON in the object graph: the gradient is keyed by parameter name
//! (`grad["k"]`) and projects to a numpy vector on request. Mirrors `Result`'s
//! lazy-numpy convention so numpy stays an optional `data` extra — `to_numpy`
//! falls back to a plain `list[float]` when numpy is absent.

use pyo3::exceptions::{PyImportError, PyKeyError};
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};

use crate::error::{ApiError, ApiResult};
use rumoca_sim::ObjectiveGradientProbe;

/// Steady-state gradient `d(objective)/dp`, one entry per model parameter,
/// addressed by name. Returned by [`crate::model::Model::objective_gradient`].
#[pyclass(module = "rumoca")]
pub struct GradientResult {
    model_name: String,
    objective: String,
    mode: String,
    t: f64,
    /// Parameter names, aligned with `values` (P-slot order from the probe).
    param_names: Vec<String>,
    /// `d(objective)/d(param_names[i])`.
    values: Vec<f64>,
}

impl GradientResult {
    /// Build from a successful probe. The caller must have already surfaced
    /// `probe.report.error`, so this consumes a known-good report.
    pub(crate) fn from_probe(
        model_name: String,
        mode: &str,
        probe: ObjectiveGradientProbe,
    ) -> Self {
        let report = probe.report;
        Self {
            model_name,
            objective: report.objective,
            mode: mode.to_string(),
            t: report.t,
            param_names: report.param_labels,
            values: report.gradient,
        }
    }

    fn value_of(&self, name: &str) -> Option<f64> {
        self.param_names
            .iter()
            .position(|n| n == name)
            .and_then(|i| self.values.get(i).copied())
    }
}

#[pymethods]
impl GradientResult {
    #[getter]
    fn model(&self) -> String {
        self.model_name.clone()
    }

    #[getter]
    fn objective(&self) -> String {
        self.objective.clone()
    }

    /// Gradient mode used: `"forward"` (implicit-function sensitivity) or
    /// `"adjoint"` (reverse-mode, matrix-free).
    #[getter]
    fn mode(&self) -> String {
        self.mode.clone()
    }

    /// Linearization time the gradient was evaluated at.
    #[getter]
    fn t(&self) -> f64 {
        self.t
    }

    /// Parameter names, in the order the gradient vector is stored.
    #[getter]
    fn names(&self) -> Vec<String> {
        self.param_names.clone()
    }

    fn keys(&self) -> Vec<String> {
        self.param_names.clone()
    }

    fn __len__(&self) -> usize {
        self.param_names.len()
    }

    fn __getitem__(&self, name: &str) -> PyResult<f64> {
        self.value_of(name).ok_or_else(|| {
            PyKeyError::new_err(crate::unknown_name_message(
                "parameter",
                name,
                &self.param_names,
            ))
        })
    }

    fn __contains__(&self, name: &str) -> bool {
        self.param_names.iter().any(|n| n == name)
    }

    /// The gradient as a `{parameter: d(objective)/d(parameter)}` dict.
    fn to_dict(&self, py: Python<'_>) -> PyObject {
        let dict = PyDict::new_bound(py);
        for (name, value) in self.param_names.iter().zip(&self.values) {
            let _ = dict.set_item(name, *value);
        }
        dict.into_py(py)
    }

    /// The gradient as a 1-D numpy array in `names` order. Falls back to a plain
    /// `list[float]` when numpy (`pip install rumoca[data]`) is absent, so the
    /// call never fails on a missing optional dependency.
    fn to_numpy(&self, py: Python<'_>) -> PyObject {
        if let Ok(np) = py.import_bound("numpy")
            && let Ok(arr) = np.call_method1("asarray", (self.values.clone(),))
        {
            return arr.into_py(py);
        }
        PyList::new_bound(py, self.values.iter().copied()).into_py(py)
    }

    /// Stack `(names, gradient)` into a pandas `Series` indexed by parameter name.
    /// Requires pandas (`pip install rumoca[data]`).
    fn to_series(&self, py: Python<'_>) -> ApiResult<PyObject> {
        let pd = py.import_bound("pandas").map_err(|_| {
            ApiError::Py(PyImportError::new_err(
                "to_series requires pandas: pip install rumoca[data]",
            ))
        })?;
        let kwargs = PyDict::new_bound(py);
        kwargs.set_item("index", self.param_names.clone())?;
        kwargs.set_item("name", &self.objective)?;
        let series = pd.call_method("Series", (self.values.clone(),), Some(&kwargs))?;
        Ok(series.into_py(py))
    }

    fn __repr__(&self) -> String {
        format!(
            "GradientResult(model={:?}, objective={:?}, mode={:?}, t={}, parameters={})",
            self.model_name,
            self.objective,
            self.mode,
            self.t,
            self.param_names.len()
        )
    }

    fn _repr_html_(&self) -> String {
        let rows: String = self
            .param_names
            .iter()
            .zip(&self.values)
            .map(|(name, value)| {
                format!("<tr><td><code>{name}</code></td><td>{value:.6e}</td></tr>")
            })
            .collect();
        format!(
            "<b>GradientResult</b> d(<code>{}</code>)/dp via {} at t={}\
             <table><tr><th>parameter</th><th>∂/∂p</th></tr>{rows}</table>",
            self.objective, self.mode, self.t
        )
    }
}
