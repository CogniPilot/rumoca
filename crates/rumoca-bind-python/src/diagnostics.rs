//! Typed diagnostics and the public exception hierarchy.
//!
//! `validate`/`validate_source` return `list[Diagnostic]` and never raise on
//! model errors. `load`/`loads`/`simulate` raise the typed exceptions below so
//! `except rumoca.CompileError` works and the message teaches.

use pyo3::create_exception;
use pyo3::exceptions::PyException;
use pyo3::prelude::*;

create_exception!(
    rumoca,
    RumocaError,
    PyException,
    "Base class for all Rumoca errors; carries a `.diagnostics` list."
);
create_exception!(rumoca, ParseError, RumocaError, "Modelica syntax error.");
create_exception!(rumoca, CompileError, RumocaError, "Compilation error.");
create_exception!(rumoca, SimulationError, RumocaError, "Simulation error.");
create_exception!(
    rumoca,
    StructuralParamError,
    RumocaError,
    "Attempt to tune a structural parameter without recompiling."
);

/// One diagnostic message (syntax/semantic/lint), addressed by source location.
#[pyclass(module = "rumoca")]
#[derive(Clone, Debug)]
pub struct Diagnostic {
    #[pyo3(get)]
    pub rule: Option<String>,
    /// One of `"error" | "warning" | "note" | "help"`.
    #[pyo3(get)]
    pub level: String,
    #[pyo3(get)]
    pub message: String,
    #[pyo3(get)]
    pub file: Option<String>,
    #[pyo3(get)]
    pub line: Option<u32>,
    #[pyo3(get)]
    pub column: Option<u32>,
    #[pyo3(get)]
    pub suggestion: Option<String>,
}

#[pymethods]
impl Diagnostic {
    fn __repr__(&self) -> String {
        let loc = match (&self.file, self.line, self.column) {
            (Some(f), Some(l), Some(c)) => format!("{f}:{l}:{c}: "),
            (Some(f), Some(l), None) => format!("{f}:{l}: "),
            _ => String::new(),
        };
        let rule = self
            .rule
            .as_ref()
            .map_or_else(String::new, |r| format!(" [{r}]"));
        format!("{loc}{}: {}{rule}", self.level, self.message)
    }

    fn __str__(&self) -> String {
        self.__repr__()
    }
}

impl Diagnostic {
    pub(crate) fn syntax(message: String, file: String) -> Self {
        Self {
            rule: Some("syntax-error".to_string()),
            level: "error".to_string(),
            message,
            file: Some(file),
            line: Some(1),
            column: Some(1),
            suggestion: None,
        }
    }
}
