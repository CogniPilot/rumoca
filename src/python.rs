//! Python bindings for the Rumoca Modelica compiler.
//!
//! This module provides Python bindings using PyO3, allowing the Rumoca
//! compiler to be used directly from Python without subprocess calls.

use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;

use crate::compiler::Compiler;
use crate::ir::transform::multi_file::{discover_modelica_files, get_modelica_path};

/// Python wrapper for compilation results.
#[pyclass]
#[derive(Clone)]
pub struct PyCompilationResult {
    json_cache: Option<String>,
    model_name: String,
    parse_time_ms: f64,
    flatten_time_ms: f64,
    dae_time_ms: f64,
    is_balanced: bool,
    balance_status: String,
}

#[pymethods]
impl PyCompilationResult {
    /// Get the DAE as a JSON string (Base Modelica format).
    #[getter]
    fn json(&self) -> PyResult<String> {
        self.json_cache
            .clone()
            .ok_or_else(|| PyRuntimeError::new_err("JSON not available"))
    }

    /// Get the model name.
    #[getter]
    fn model_name(&self) -> &str {
        &self.model_name
    }

    /// Get parse time in milliseconds.
    #[getter]
    fn parse_time_ms(&self) -> f64 {
        self.parse_time_ms
    }

    /// Get flatten time in milliseconds.
    #[getter]
    fn flatten_time_ms(&self) -> f64 {
        self.flatten_time_ms
    }

    /// Get DAE creation time in milliseconds.
    #[getter]
    fn dae_time_ms(&self) -> f64 {
        self.dae_time_ms
    }

    /// Get total compilation time in milliseconds.
    #[getter]
    fn total_time_ms(&self) -> f64 {
        self.parse_time_ms + self.flatten_time_ms + self.dae_time_ms
    }

    /// Check if the model is balanced (equations == unknowns).
    #[getter]
    fn is_balanced(&self) -> bool {
        self.is_balanced
    }

    /// Get a human-readable balance status string.
    #[getter]
    fn balance_status(&self) -> &str {
        &self.balance_status
    }

    fn __repr__(&self) -> String {
        format!(
            "CompilationResult(model='{}', balanced={}, time={:.2}ms)",
            self.model_name,
            self.is_balanced,
            self.total_time_ms()
        )
    }
}

/// Compile a Modelica source string.
///
/// Args:
///     source: Modelica source code as a string
///     model_name: Name of the model to compile
///     filename: Optional filename for error messages (default: "<string>")
///     library_paths: Optional list of library paths to include (e.g., ["/path/to/MSL"])
///     use_modelica_path: If True, also search MODELICAPATH env var for libraries (default: True)
///     threads: Number of threads for parallel parsing (default: num_cpus - 1)
///
/// Returns:
///     PyCompilationResult containing the compiled model
///
/// Raises:
///     RuntimeError: If compilation fails
#[pyfunction]
#[pyo3(signature = (source, model_name, filename = "<string>", library_paths = None, use_modelica_path = true, threads = None))]
fn compile_str(
    source: &str,
    model_name: &str,
    filename: &str,
    library_paths: Option<Vec<String>>,
    use_modelica_path: bool,
    threads: Option<usize>,
) -> PyResult<PyCompilationResult> {
    let mut compiler = Compiler::new().model(model_name);

    // Set thread count if specified
    if let Some(t) = threads {
        compiler = compiler.threads(t);
    }

    // Add library paths
    compiler = add_library_paths(compiler, library_paths, use_modelica_path)?;

    let result = compiler
        .compile_str(source, filename)
        .map_err(|e| PyRuntimeError::new_err(format!("Compilation failed: {}", e)))?;

    let json = result
        .to_dae_ir_json()
        .map_err(|e| PyRuntimeError::new_err(format!("JSON serialization failed: {}", e)))?;

    Ok(PyCompilationResult {
        json_cache: Some(json),
        model_name: model_name.to_string(),
        parse_time_ms: result.parse_time.as_secs_f64() * 1000.0,
        flatten_time_ms: result.flatten_time.as_secs_f64() * 1000.0,
        dae_time_ms: result.dae_time.as_secs_f64() * 1000.0,
        is_balanced: result.is_balanced(),
        balance_status: result.balance_status(),
    })
}

/// Helper function to add library paths to a compiler
fn add_library_paths(
    mut compiler: Compiler,
    library_paths: Option<Vec<String>>,
    use_modelica_path: bool,
) -> PyResult<Compiler> {
    // Collect all paths to search
    let mut all_paths: Vec<std::path::PathBuf> = Vec::new();

    // Add explicit library paths
    if let Some(paths) = library_paths {
        for path in paths {
            all_paths.push(std::path::PathBuf::from(path));
        }
    }

    // Add MODELICAPATH paths if enabled
    if use_modelica_path {
        all_paths.extend(get_modelica_path());
    }

    // Discover and include files from all library paths
    // The Compiler handles deduplication internally
    for path in all_paths {
        if path.exists() {
            let files = discover_modelica_files(&path).map_err(|e| {
                PyRuntimeError::new_err(format!("Failed to discover files in {:?}: {}", path, e))
            })?;
            for file in files {
                compiler = compiler.include(&file.to_string_lossy());
            }
        }
    }

    Ok(compiler)
}

/// Compile a Modelica file.
///
/// Args:
///     path: Path to the Modelica file
///     model_name: Name of the model to compile
///     library_paths: Optional list of library paths to include (e.g., ["/path/to/MSL"])
///     use_modelica_path: If True, also search MODELICAPATH env var for libraries (default: True)
///     threads: Number of threads for parallel parsing (default: num_cpus - 1)
///
/// Returns:
///     PyCompilationResult containing the compiled model
///
/// Raises:
///     RuntimeError: If compilation fails
#[pyfunction]
#[pyo3(signature = (path, model_name, library_paths = None, use_modelica_path = true, threads = None))]
fn compile_file(
    path: &str,
    model_name: &str,
    library_paths: Option<Vec<String>>,
    use_modelica_path: bool,
    threads: Option<usize>,
) -> PyResult<PyCompilationResult> {
    let mut compiler = Compiler::new().model(model_name);

    // Set thread count if specified
    if let Some(t) = threads {
        compiler = compiler.threads(t);
    }

    // Add library paths
    compiler = add_library_paths(compiler, library_paths, use_modelica_path)?;

    let result = compiler
        .compile_file(path)
        .map_err(|e| PyRuntimeError::new_err(format!("Compilation failed: {}", e)))?;

    let json = result
        .to_dae_ir_json()
        .map_err(|e| PyRuntimeError::new_err(format!("JSON serialization failed: {}", e)))?;

    Ok(PyCompilationResult {
        json_cache: Some(json),
        model_name: model_name.to_string(),
        parse_time_ms: result.parse_time.as_secs_f64() * 1000.0,
        flatten_time_ms: result.flatten_time.as_secs_f64() * 1000.0,
        dae_time_ms: result.dae_time.as_secs_f64() * 1000.0,
        is_balanced: result.is_balanced(),
        balance_status: result.balance_status(),
    })
}

/// Get the version of the Rumoca compiler.
#[pyfunction]
fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

/// Native Rust bindings for the Rumoca Modelica compiler.
///
/// This module provides Python bindings for the Rumoca Modelica compiler,
/// enabling direct compilation of Modelica models without subprocess calls.
///
/// Example:
///     >>> from rumoca._native import compile_str
///     >>> result = compile_str('''
///     ...     model Test
///     ...         Real x;
///     ...     equation
///     ...         der(x) = 1;
///     ...     end Test;
///     ... ''', "Test")
///     >>> print(result.json)
#[pymodule]
fn _native(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(compile_str, m)?)?;
    m.add_function(wrap_pyfunction!(compile_file, m)?)?;
    m.add_function(wrap_pyfunction!(version, m)?)?;
    m.add_class::<PyCompilationResult>()?;
    Ok(())
}
