//! A small typed error domain that maps cleanly onto the public Python
//! exception hierarchy.
//!
//! Functions return `Result<T, ApiError>` rather than `PyResult<T>` so the
//! pyo3-generated error conversion is a real `ApiError -> PyErr` mapping (which
//! also picks the *typed* exception class), not an identity `PyErr -> PyErr`.

use pyo3::PyErr;
use pyo3::exceptions::PyNotImplementedError;

use crate::PyRuntimeStringError;
use crate::diagnostics::{CompileError, SimulationError, StructuralParamError};

/// Error raised by the typed API; converts to the matching Python exception.
pub enum ApiError {
    Compile(String),
    Sim(String),
    StructuralParam(String),
    NotImplemented(String),
    /// An already-constructed Python error (e.g. from a pyo3 call via `?`).
    Py(PyErr),
}

impl std::fmt::Display for ApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ApiError::Compile(msg)
            | ApiError::Sim(msg)
            | ApiError::StructuralParam(msg)
            | ApiError::NotImplemented(msg) => f.write_str(msg),
            ApiError::Py(err) => write!(f, "{err}"),
        }
    }
}

impl From<PyErr> for ApiError {
    fn from(err: PyErr) -> Self {
        Self::Py(err)
    }
}

impl From<PyRuntimeStringError> for ApiError {
    fn from(err: PyRuntimeStringError) -> Self {
        Self::Py(err.into())
    }
}

impl From<ApiError> for PyErr {
    fn from(err: ApiError) -> Self {
        match err {
            ApiError::Compile(msg) => CompileError::new_err(msg),
            ApiError::Sim(msg) => SimulationError::new_err(msg),
            ApiError::StructuralParam(msg) => StructuralParamError::new_err(msg),
            ApiError::NotImplemented(msg) => PyNotImplementedError::new_err(msg),
            ApiError::Py(err) => err,
        }
    }
}

/// Convenience alias for the typed-API result.
pub type ApiResult<T> = Result<T, ApiError>;
