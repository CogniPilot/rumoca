use ::rumoca::CompilationResult as HighLevelCompilationResult;
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use pyo3::{PyObject, Python, exceptions::PyKeyError};
use serde_json::Value;
use std::fs;
use std::path::Path;

use crate::PyRuntimeStringError;

#[pyclass]
#[derive(Clone)]
pub(crate) struct CompileResult {
    #[pyo3(get)]
    model_name: String,
    payload: Value,
}

#[pymethods]
impl CompileResult {
    fn __repr__(&self) -> String {
        format!("CompileResult(model_name='{}')", self.model_name)
    }

    fn __getitem__(&self, py: Python<'_>, key: &str) -> PyResult<PyObject> {
        json_object_item(py, &self.payload, key)
    }

    fn to_dict(&self, py: Python<'_>) -> PyObject {
        json_value_to_py(py, &self.payload)
    }

    #[pyo3(signature = (pretty=true))]
    fn to_json(&self, pretty: bool) -> Result<String, PyRuntimeStringError> {
        json_value_to_string(&self.payload, pretty)
    }

    #[pyo3(signature = (path, pretty=true))]
    fn save_json(&self, path: &str, pretty: bool) -> Result<(), PyRuntimeStringError> {
        save_json_value(path, &self.payload, pretty)
    }
}

#[pyclass]
#[derive(Clone)]
pub(crate) struct SimulationResult {
    #[pyo3(get)]
    model_name: String,
    payload: Value,
}

#[pymethods]
impl SimulationResult {
    fn __repr__(&self) -> String {
        format!("SimulationResult(model_name='{}')", self.model_name)
    }

    fn __getitem__(&self, py: Python<'_>, key: &str) -> PyResult<PyObject> {
        json_object_item(py, &self.payload, key)
    }

    #[getter]
    fn metrics(&self, py: Python<'_>) -> PyObject {
        json_nested_object_item(py, &self.payload, "metrics")
    }

    #[getter]
    fn data(&self, py: Python<'_>) -> PyObject {
        json_nested_object_item(py, &self.payload, "payload")
    }

    fn to_dict(&self, py: Python<'_>) -> PyObject {
        json_value_to_py(py, &self.payload)
    }

    #[pyo3(signature = (pretty=true))]
    fn to_json(&self, pretty: bool) -> Result<String, PyRuntimeStringError> {
        json_value_to_string(&self.payload, pretty)
    }

    #[pyo3(signature = (path, pretty=true))]
    fn save_json(&self, path: &str, pretty: bool) -> Result<(), PyRuntimeStringError> {
        save_json_value(path, &self.payload, pretty)
    }
}

#[pyclass]
#[derive(Clone)]
pub(crate) struct CodegenResult {
    #[pyo3(get)]
    model_name: String,
    #[pyo3(get)]
    target: String,
    files: Value,
}

#[pymethods]
impl CodegenResult {
    fn __repr__(&self) -> String {
        let count = self.files.as_array().map_or(0, std::vec::Vec::len);
        format!(
            "CodegenResult(model_name='{}', target='{}', files={})",
            self.model_name, self.target, count
        )
    }

    fn to_dict(&self, py: Python<'_>) -> PyObject {
        json_value_to_py(py, &self.files)
    }

    #[getter]
    fn paths(&self) -> Vec<String> {
        rendered_file_entries(&self.files)
            .into_iter()
            .map(|(path, _)| path)
            .collect()
    }

    #[pyo3(signature = (pretty=true))]
    fn to_json(&self, pretty: bool) -> Result<String, PyRuntimeStringError> {
        json_value_to_string(&self.files, pretty)
    }

    #[pyo3(signature = (path, pretty=true))]
    fn save_json(&self, path: &str, pretty: bool) -> Result<(), PyRuntimeStringError> {
        save_json_value(path, &self.files, pretty)
    }

    fn save_all(&self, output_dir: &str) -> Result<Vec<String>, PyRuntimeStringError> {
        let output_dir = Path::new(output_dir);
        let mut written = Vec::new();
        for (relative_path, content) in rendered_file_entries(&self.files) {
            written.push(write_rendered_file(output_dir, &relative_path, &content)?);
        }
        Ok(written)
    }
}

pub(crate) fn compile_result_from_compilation(
    result: HighLevelCompilationResult,
    model_name: String,
) -> Result<CompileResult, PyRuntimeStringError> {
    let payload = serde_json::to_value(result.dae)
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))?;
    Ok(CompileResult {
        model_name,
        payload,
    })
}

pub(crate) fn simulation_result_from_json(
    json: &str,
) -> Result<SimulationResult, PyRuntimeStringError> {
    let payload: Value =
        serde_json::from_str(json).map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))?;
    let model_name = payload
        .get("model")
        .and_then(Value::as_str)
        .unwrap_or("")
        .to_string();
    Ok(SimulationResult {
        model_name,
        payload,
    })
}

pub(crate) fn codegen_result_from_json(
    json: &str,
    model_name: String,
    target: String,
) -> Result<CodegenResult, PyRuntimeStringError> {
    let files: Value =
        serde_json::from_str(json).map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))?;
    Ok(CodegenResult {
        model_name,
        target,
        files,
    })
}

fn json_value_to_py(py: Python<'_>, value: &Value) -> PyObject {
    match value {
        Value::Null => py.None(),
        Value::Bool(value) => value.into_py(py),
        Value::Number(value) => json_number_to_py(py, value),
        Value::String(value) => value.into_py(py),
        Value::Array(values) => {
            let items = values
                .iter()
                .map(|value| json_value_to_py(py, value))
                .collect::<Vec<_>>();
            PyList::new_bound(py, items).into_py(py)
        }
        Value::Object(values) => {
            let dict = PyDict::new_bound(py);
            for (key, value) in values {
                dict.set_item(key, json_value_to_py(py, value))
                    .expect("serde_json object keys and values convert to Python");
            }
            dict.into_py(py)
        }
    }
}

fn json_number_to_py(py: Python<'_>, value: &serde_json::Number) -> PyObject {
    if let Some(value) = value.as_i64() {
        return value.into_py(py);
    }
    if let Some(value) = value.as_u64() {
        return value.into_py(py);
    }
    if let Some(value) = value.as_f64() {
        return value.into_py(py);
    }
    py.None()
}

fn json_object_item(py: Python<'_>, value: &Value, key: &str) -> PyResult<PyObject> {
    match value.get(key) {
        Some(value) => Ok(json_value_to_py(py, value)),
        None => Err(PyKeyError::new_err(key.to_string())),
    }
}

fn json_nested_object_item(py: Python<'_>, value: &Value, key: &str) -> PyObject {
    value.get(key).map_or_else(
        || PyDict::new_bound(py).into_py(py),
        |value| json_value_to_py(py, value),
    )
}

fn json_value_to_string(value: &Value, pretty: bool) -> Result<String, PyRuntimeStringError> {
    if pretty {
        serde_json::to_string_pretty(value)
    } else {
        serde_json::to_string(value)
    }
    .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
}

fn save_json_value(path: &str, value: &Value, pretty: bool) -> Result<(), PyRuntimeStringError> {
    let text = json_value_to_string(value, pretty)?;
    fs::write(path, text).map_err(|e| PyRuntimeStringError(format!("Failed to write {path}: {e}")))
}

fn rendered_file_entries(files: &Value) -> Vec<(String, String)> {
    files
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|file| {
            let path = file.get("path")?.as_str()?;
            let content = file.get("content")?.as_str()?;
            Some((path.to_string(), content.to_string()))
        })
        .collect()
}

fn write_rendered_file(
    output_dir: &Path,
    relative_path: &str,
    content: &str,
) -> Result<String, PyRuntimeStringError> {
    let path = output_dir.join(relative_path);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            PyRuntimeStringError(format!("Failed to create {}: {e}", parent.display()))
        })?;
    }
    fs::write(&path, content)
        .map_err(|e| PyRuntimeStringError(format!("Failed to write {}: {e}", path.display())))?;
    Ok(path.to_string_lossy().to_string())
}
