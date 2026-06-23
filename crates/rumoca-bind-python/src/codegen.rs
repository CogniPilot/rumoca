//! Typed codegen output: generated files with paths and content.

use pyo3::prelude::*;
use pyo3::types::PyList;
use rumoca_compile::codegen::targets::RenderedTargetFile;
use std::fs;
use std::path::{Path, PathBuf};

use crate::PyRuntimeStringError;

/// A single generated file (path relative to the output root + its content).
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct GeneratedFile {
    #[pyo3(get)]
    pub path: String,
    #[pyo3(get)]
    pub content: String,
}

#[pymethods]
impl GeneratedFile {
    fn __repr__(&self) -> String {
        format!(
            "GeneratedFile(path={:?}, {} bytes)",
            self.path,
            self.content.len()
        )
    }
}

/// The result of rendering a codegen target — a list of [`GeneratedFile`]s.
///
/// Iterating yields `(path, content)` tuples; `save_all(out)` writes them.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct CodegenResult {
    #[pyo3(get)]
    pub target: String,
    files: Vec<GeneratedFile>,
}

impl CodegenResult {
    pub(crate) fn new(target: String, files: Vec<RenderedTargetFile>) -> Self {
        Self {
            target,
            files: files
                .into_iter()
                .map(|f| GeneratedFile {
                    path: f.path,
                    content: f.content,
                })
                .collect(),
        }
    }

    /// Concatenated content (used by `Model.render` for the single-string view).
    pub(crate) fn joined_content(&self) -> String {
        self.files
            .iter()
            .map(|f| f.content.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[pymethods]
impl CodegenResult {
    #[getter]
    fn files(&self) -> Vec<GeneratedFile> {
        self.files.clone()
    }

    #[getter]
    fn paths(&self) -> Vec<String> {
        self.files.iter().map(|f| f.path.clone()).collect()
    }

    fn __len__(&self) -> usize {
        self.files.len()
    }

    fn __iter__(slf: PyRef<'_, Self>, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let tuples: Vec<PyObject> = slf
            .files
            .iter()
            .map(|f| (f.path.clone(), f.content.clone()).into_py(py))
            .collect();
        let list = PyList::new_bound(py, tuples);
        Ok(list.as_any().iter()?.into_py(py))
    }

    /// Write every file under `out`, creating parent directories. Returns the
    /// list of written paths.
    fn save_all(&self, out: &str) -> std::result::Result<Vec<String>, PyRuntimeStringError> {
        let root = Path::new(out);
        self.files
            .iter()
            .map(|file| write_one(root, file))
            .collect()
    }

    fn __repr__(&self) -> String {
        format!(
            "CodegenResult(target={:?}, files={})",
            self.target,
            self.files.len()
        )
    }
}

/// Write one generated file under `root`, creating parent directories. Returns
/// the written path.
fn write_one(
    root: &Path,
    file: &GeneratedFile,
) -> std::result::Result<String, PyRuntimeStringError> {
    let path: PathBuf = root.join(&file.path);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            PyRuntimeStringError(format!("Failed to create {}: {e}", parent.display()))
        })?;
    }
    fs::write(&path, &file.content)
        .map_err(|e| PyRuntimeStringError(format!("Failed to write {}: {e}", path.display())))?;
    Ok(path.to_string_lossy().to_string())
}
