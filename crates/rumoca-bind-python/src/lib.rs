// PyO3 macro expansion currently emits Rust 2024 `unsafe_op_in_unsafe_fn` patterns.
#![expect(
    unsafe_op_in_unsafe_fn,
    reason = "PyO3 macro expansion emits Rust 2024 unsafe_op_in_unsafe_fn patterns"
)]

//! Python bindings for the Rumoca Modelica compiler.
//!
//! This module provides Python bindings using PyO3, allowing the Rumoca
//! compiler to be used directly from Python.
//!
//! ## Usage from Python
//! ```python
//! import rumoca
//!
//! # Check if code is valid
//! result = rumoca.parse("model M Real x; end M;")
//! print(result.success)  # True
//!
//! # Get lint messages
//! messages = rumoca.lint("model m Real x; end m;")
//! for msg in messages:
//!     print(f"{msg.level}: {msg.message}")
//!
//! # Generate code from DAE JSON
//! code = rumoca.generate_code(dae_json, "casadi")
//! ```

use pyo3::prelude::*;
use pyo3::{PyErr, exceptions::PyRuntimeError};

use rumoca_session::parsing::validate_source_syntax;
use rumoca_session::runtime::render_dae_template_for_target;
use rumoca_tool_fmt::FormatOptions;
use rumoca_tool_lint::{LintLevel, LintOptions, lint as lint_source};

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

/// Get the Rumoca version.
#[pyfunction]
fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// Parse Modelica source code.
///
/// Args:
///     source: Modelica source code as a string
///     filename: Optional filename for error messages (default: "input.mo")
///
/// Returns:
///     ParseResult with success flag and optional error message
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
///
/// Args:
///     source: Modelica source code as a string
///     filename: Optional filename for error messages (default: "input.mo")
///
/// Returns:
///     List of LintMessage objects
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
///
/// Args:
///     source: Modelica source code as a string
///     filename: Optional filename for diagnostics (default: "input.mo")
///
/// Returns:
///     Formatted Modelica source code
///
/// Raises:
///     RuntimeError: If source has syntax errors
#[pyfunction(name = "format")]
#[pyo3(signature = (source, filename=None))]
fn format_source(source: &str, filename: Option<&str>) -> Result<String, PyRuntimeStringError> {
    let filename = filename.unwrap_or("input.mo");
    let options = FormatOptions::default();
    rumoca_tool_fmt::format_with_source_name(source, &options, filename)
        .map_err(|e| PyRuntimeStringError(format!("Format error: {e}")))
}

/// Format Modelica source code, returning original source on format/syntax error.
///
/// Args:
///     source: Modelica source code as a string
///     filename: Optional filename for diagnostics (default: "input.mo")
///
/// Returns:
///     Formatted source, or original source if formatting fails
#[pyfunction]
#[pyo3(signature = (source, filename=None))]
fn format_or_original(source: &str, filename: Option<&str>) -> String {
    let filename = filename.unwrap_or("input.mo");
    let options = FormatOptions::default();
    rumoca_tool_fmt::format_or_original_with_source_name(source, &options, filename)
}

/// Check Modelica source code for errors and warnings.
///
/// This combines parsing and linting, returning all issues found.
///
/// Args:
///     source: Modelica source code as a string
///     filename: Optional filename for error messages (default: "input.mo")
///
/// Returns:
///     List of LintMessage objects (including syntax errors)
#[pyfunction]
#[pyo3(signature = (source, filename=None))]
fn check(source: &str, filename: Option<&str>) -> Vec<LintMessage> {
    let filename = filename.unwrap_or("input.mo");

    // First check for syntax errors
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

    // Then run linter
    lint(source, Some(filename))
}

/// Generate code from a DAE JSON representation.
///
/// Args:
///     dae_json: DAE as a JSON string
///     target: Target name (e.g., "casadi", "cyecca", "julia", "c", "jax", "onnx")
///
/// Returns:
///     Generated code as a string
///
/// Raises:
///     RuntimeError: If JSON parsing or code generation fails
#[pyfunction]
fn generate_code(dae_json: &str, target: &str) -> Result<String, PyRuntimeStringError> {
    let dae_json: serde_json::Value = serde_json::from_str(dae_json)
        .map_err(|e| PyRuntimeStringError(format!("Invalid DAE JSON: {e}")))?;

    render_dae_template_for_target(&dae_json, target)
        .map_err(|e| PyRuntimeStringError(format!("Code generation error: {e}")))
}

/// Compile Modelica source code to DAE JSON.
///
/// Args:
///     source: Modelica source code as a string
///     model_name: Name of the model to compile
///     filename: Optional filename for error messages (default: "input.mo")
///
/// Returns:
///     DAE as a JSON string
///
/// Raises:
///     RuntimeError: If compilation fails
#[pyfunction]
#[pyo3(signature = (source, model_name, filename=None))]
fn compile(
    source: &str,
    model_name: &str,
    filename: Option<&str>,
) -> Result<String, PyRuntimeStringError> {
    use rumoca_session::compile::PhaseResult;
    use rumoca_session::{Session, SessionConfig};

    let filename = filename.unwrap_or("input.mo");
    let mut session = Session::new(SessionConfig::default());

    session
        .add_document(filename, source)
        .map_err(|e| PyRuntimeStringError(format!("Parse error: {e}")))?;

    let mut report = session.compile_model_strict_reachable_with_recovery(model_name);
    let result = match report.requested_result.take() {
        Some(PhaseResult::Success(result)) => *result,
        _ => return Err(PyRuntimeStringError(report.failure_summary(10))),
    };

    serde_json::to_string_pretty(&result.dae)
        .map_err(|e| PyRuntimeStringError(format!("JSON error: {e}")))
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
    m.add_function(wrap_pyfunction!(generate_code, m)?)?;
    m.add_class::<ParseResult>()?;
    m.add_class::<LintMessage>()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
        // Should have naming convention warning
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
}
