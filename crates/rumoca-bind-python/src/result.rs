//! Typed simulation result, backed directly by `rumoca_solver::SimResult`.
//!
//! No JSON in the object graph: `result["x"]` and `result.time` build numpy
//! arrays straight from the result columns (numpy/pandas/matplotlib are imported
//! lazily, so they stay optional `data`/`plot` extras). `.to_json()`/`.to_dict()`
//! are the only places JSON appears, and only when explicitly requested.

use pyo3::exceptions::{PyImportError, PyKeyError};
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use rumoca_sim::SimResult;
use serde_json::{Value, json};

use crate::PyRuntimeStringError;
use crate::error::{ApiError, ApiResult};

/// Build a Python sequence from an `f64` column: a numpy `ndarray` when numpy is
/// installed (the `data` extra), otherwise a plain `list[float]` so the call
/// never fails just because an optional dependency is absent.
fn to_py_array(py: Python<'_>, values: &[f64]) -> PyObject {
    if let Ok(np) = py.import_bound("numpy")
        && let Ok(arr) = np.call_method1("asarray", (values.to_vec(),))
    {
        return arr.into_py(py);
    }
    PyList::new_bound(py, values.iter().copied()).into_py(py)
}

#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct Result {
    model_name: String,
    times: Vec<f64>,
    names: Vec<String>,
    /// `data[var_idx][time_idx]`, aligned with `names`.
    data: Vec<Vec<f64>>,
    compile_seconds: Option<f64>,
    simulate_seconds: Option<f64>,
    termination: Option<String>,
}

impl Result {
    pub(crate) fn from_sim(
        model_name: String,
        sim: SimResult,
        compile_seconds: Option<f64>,
        simulate_seconds: Option<f64>,
    ) -> Self {
        let termination = sim.termination.as_ref().map(|t| t.message.clone());
        Self {
            model_name,
            times: sim.times,
            names: sim.names,
            data: sim.data,
            compile_seconds,
            simulate_seconds,
            termination,
        }
    }

    fn column(&self, name: &str) -> Option<&[f64]> {
        let idx = self.names.iter().position(|n| n == name)?;
        self.data.get(idx).map(Vec::as_slice)
    }
}

#[pymethods]
impl Result {
    #[getter]
    fn model(&self) -> String {
        self.model_name.clone()
    }

    #[getter]
    fn names(&self) -> Vec<String> {
        self.names.clone()
    }

    #[getter]
    fn time(&self, py: Python<'_>) -> PyObject {
        to_py_array(py, &self.times)
    }

    #[getter]
    fn termination(&self) -> Option<String> {
        self.termination.clone()
    }

    #[getter]
    fn metrics(&self, py: Python<'_>) -> PyObject {
        let dict = PyDict::new_bound(py);
        let _ = dict.set_item("points", self.times.len());
        let _ = dict.set_item("variables", self.names.len());
        let _ = dict.set_item("compile_seconds", self.compile_seconds);
        let _ = dict.set_item("simulate_seconds", self.simulate_seconds);
        if let Some(t) = &self.termination {
            let _ = dict.set_item("termination", t);
        }
        dict.into_py(py)
    }

    fn __len__(&self) -> usize {
        self.times.len()
    }

    fn __getitem__(&self, py: Python<'_>, name: &str) -> PyResult<PyObject> {
        if name == "time" || name == "t" {
            return Ok(to_py_array(py, &self.times));
        }
        match self.column(name) {
            Some(col) => Ok(to_py_array(py, col)),
            None => Err(PyKeyError::new_err(crate::unknown_name_message(
                "variable",
                name,
                &self.names,
            ))),
        }
    }

    fn __contains__(&self, name: &str) -> bool {
        name == "time" || self.names.iter().any(|n| n == name)
    }

    /// Stack the requested columns (or all variables) into a 2-D numpy array of
    /// shape `(n_vars, n_times)`. Requires numpy (`pip install rumoca[data]`).
    #[pyo3(signature = (names=None))]
    fn to_numpy(&self, py: Python<'_>, names: Option<Vec<String>>) -> ApiResult<PyObject> {
        let np = py.import_bound("numpy").map_err(|_| {
            ApiError::Py(PyImportError::new_err(
                "to_numpy requires numpy: pip install rumoca[data]",
            ))
        })?;
        let selected = names.unwrap_or_else(|| self.names.clone());
        let mut rows: Vec<Vec<f64>> = Vec::with_capacity(selected.len());
        for name in &selected {
            let col = self.column(name).ok_or_else(|| {
                ApiError::Py(PyKeyError::new_err(crate::unknown_name_message(
                    "variable",
                    name,
                    &self.names,
                )))
            })?;
            rows.push(col.to_vec());
        }
        Ok(np.call_method1("asarray", (rows,))?.into_py(py))
    }

    /// Build a pandas `DataFrame` indexed by time. Requires pandas
    /// (`pip install rumoca[data]`).
    fn to_dataframe(&self, py: Python<'_>) -> ApiResult<PyObject> {
        let pd = py.import_bound("pandas").map_err(|_| {
            ApiError::Py(PyImportError::new_err(
                "to_dataframe requires pandas: pip install rumoca[data]",
            ))
        })?;
        let columns = PyDict::new_bound(py);
        for (name, col) in self.names.iter().zip(self.data.iter()) {
            columns.set_item(name, to_py_array(py, col))?;
        }
        let kwargs = PyDict::new_bound(py);
        kwargs.set_item("index", to_py_array(py, &self.times))?;
        let df = pd.call_method("DataFrame", (columns,), Some(&kwargs))?;
        Ok(df.call_method1("rename_axis", ("time",))?.into_py(py))
    }

    /// Plot the named variables (all states by default) against time. Requires
    /// matplotlib (`pip install rumoca[plot]`).
    #[pyo3(signature = (*names, ax=None))]
    fn plot(
        &self,
        py: Python<'_>,
        names: Vec<String>,
        ax: Option<PyObject>,
    ) -> ApiResult<PyObject> {
        let plt = py.import_bound("matplotlib.pyplot").map_err(|_| {
            ApiError::Py(PyImportError::new_err(
                "plot requires matplotlib: pip install rumoca[plot]",
            ))
        })?;
        let axes = match ax {
            Some(ax) => ax.into_bound(py),
            None => {
                let fig_ax = plt.call_method0("subplots")?;
                fig_ax.get_item(1)?
            }
        };
        let selected = if names.is_empty() {
            self.names.clone()
        } else {
            names
        };
        for name in &selected {
            let Some(col) = self.column(name) else {
                return Err(ApiError::Py(PyKeyError::new_err(
                    crate::unknown_name_message("variable", name, &self.names),
                )));
            };
            let kwargs = PyDict::new_bound(py);
            kwargs.set_item("label", name)?;
            axes.call_method(
                "plot",
                (to_py_array(py, &self.times), to_py_array(py, col)),
                Some(&kwargs),
            )?;
        }
        axes.call_method1("set_xlabel", ("time",))?;
        axes.call_method0("legend")?;
        Ok(axes.into_py(py))
    }

    fn to_dict(&self, py: Python<'_>) -> PyObject {
        let dict = PyDict::new_bound(py);
        let _ = dict.set_item("model", &self.model_name);
        let _ = dict.set_item("time", to_py_array(py, &self.times));
        let _ = dict.set_item("names", self.names.clone());
        let data = PyDict::new_bound(py);
        for (name, col) in self.names.iter().zip(self.data.iter()) {
            let _ = data.set_item(name, to_py_array(py, col));
        }
        let _ = dict.set_item("data", data);
        let _ = dict.set_item("metrics", self.metrics(py));
        dict.into_py(py)
    }

    #[pyo3(signature = (pretty=true))]
    fn to_json(&self, pretty: bool) -> std::result::Result<String, PyRuntimeStringError> {
        let mut data = serde_json::Map::new();
        for (name, col) in self.names.iter().zip(self.data.iter()) {
            data.insert(name.clone(), json!(col));
        }
        let value = json!({
            "model": self.model_name,
            "time": self.times,
            "names": self.names,
            "data": Value::Object(data),
        });
        if pretty {
            serde_json::to_string_pretty(&value)
        } else {
            serde_json::to_string(&value)
        }
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
    }

    fn __repr__(&self) -> String {
        format!(
            "Result(model={:?}, points={}, variables={})",
            self.model_name,
            self.times.len(),
            self.names.len()
        )
    }
}
