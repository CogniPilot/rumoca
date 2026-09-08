//! Typed, runtime-discovered codegen targets and solver listing.

use pyo3::prelude::*;
use pyo3::types::PyDict;
use rumoca_compile::codegen::targets::{TargetCapabilities, builtin_target_descriptors};

use crate::error::ApiResult;

/// A codegen target as declared by its `target.toml`.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct Target {
    #[pyo3(get)]
    pub id: String,
    /// The single checked product derived from the target's file contexts.
    #[pyo3(get)]
    pub required_product: String,
    /// Ordered `(output_path_template, IR_crate_context, checked_view)` plans.
    #[pyo3(get)]
    pub file_plans: Vec<(String, String, String)>,
    #[pyo3(get)]
    pub description: Option<String>,
    capabilities: Option<TargetCapabilities>,
}

#[pymethods]
impl Target {
    /// Declared capability flags (events, AD, initialization, ...) as a dict.
    #[getter]
    fn capabilities(&self, py: Python<'_>) -> ApiResult<PyObject> {
        capabilities_dict(py, self.capabilities.as_ref())
    }

    fn __repr__(&self) -> String {
        format!(
            "Target(id={:?}, required_product={:?})",
            self.id, self.required_product
        )
    }
}

fn capabilities_dict(py: Python<'_>, caps: Option<&TargetCapabilities>) -> ApiResult<PyObject> {
    let dict = PyDict::new_bound(py);
    if let Some(c) = caps {
        let pairs: [(&str, Option<bool>); 8] = [
            ("events", c.events),
            ("runtime_events", c.runtime_events),
            ("initialization", c.initialization),
            ("forward_ad", c.forward_ad),
            ("reverse_ad", c.reverse_ad),
            ("continuous_states", c.continuous_states),
            ("residual_equations", c.residual_equations),
            ("exact_algebraic_assignments", c.exact_algebraic_assignments),
        ];
        for (key, value) in pairs {
            if let Some(v) = value {
                dict.set_item(key, v)?;
            }
        }
    }
    Ok(dict.into_py(py))
}

/// One solver available in THIS build (feature-gated).
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct SolverInfo {
    #[pyo3(get)]
    pub id: String,
    /// `"explicit"` or `"implicit"`.
    #[pyo3(get)]
    pub family: String,
    #[pyo3(get)]
    pub available: bool,
}

#[pymethods]
impl SolverInfo {
    fn __repr__(&self) -> String {
        format!(
            "SolverInfo(id={:?}, family={:?}, available={})",
            self.id, self.family, self.available
        )
    }
}

/// Runtime-discovered codegen targets, sorted by id.
pub(crate) fn list_targets() -> Result<Vec<Target>, crate::PyRuntimeStringError> {
    let descriptors = builtin_target_descriptors().map_err(|error| {
        crate::PyRuntimeStringError(format!(
            "built-in target discovery failed checked construction: {error:#}"
        ))
    })?;
    let mut targets = Vec::with_capacity(descriptors.len());
    for descriptor in descriptors {
        targets.push(Target {
            id: descriptor.id,
            required_product: descriptor.required_product.as_str().to_string(),
            file_plans: descriptor
                .file_plans
                .into_iter()
                .map(|file| {
                    (
                        file.path,
                        file.semantic_context.as_str().to_string(),
                        file.semantic_view.as_str().to_string(),
                    )
                })
                .collect(),
            description: descriptor.description,
            capabilities: descriptor.capabilities,
        });
    }
    targets.sort_by(|a, b| a.id.cmp(&b.id));
    Ok(targets)
}

/// Solvers available in this build. The Python extension always compiles
/// `rumoca-sim` with the `solver-diffsol` feature (see `Cargo.toml`), so the
/// implicit (diffsol) family is present alongside the always-available explicit
/// `rk-like` solver.
pub(crate) fn list_solvers() -> Vec<SolverInfo> {
    let implicit = |id: &str| SolverInfo {
        id: id.to_string(),
        family: "implicit".to_string(),
        available: true,
    };
    vec![
        implicit("auto"),
        SolverInfo {
            id: "rk-like".to_string(),
            family: "explicit".to_string(),
            available: true,
        },
        implicit("bdf"),
    ]
}
