//! Typed, runtime-discovered codegen targets and solver listing.

use pyo3::prelude::*;
use pyo3::types::PyDict;
use rumoca_compile::codegen::targets::{
    TargetCapabilities, TargetTemplateIr, parse_target_manifest,
};
use rumoca_compile::codegen::templates::builtin_targets;

/// A codegen target as declared by its `target.toml`.
#[pyclass(module = "rumoca")]
#[derive(Clone)]
pub struct Target {
    #[pyo3(get)]
    pub id: String,
    /// IR stage this target consumes: `"ast" | "flat" | "dae" | "solve"`.
    #[pyo3(get)]
    pub ir: String,
    #[pyo3(get)]
    pub description: Option<String>,
}

#[pymethods]
impl Target {
    /// Declared capability flags (events, AD, initialization, ...) as a dict.
    #[getter]
    fn capabilities(&self, py: Python<'_>) -> PyObject {
        // Re-parse lazily so the hot listing path stays cheap; targets are few.
        let caps = builtin_targets()
            .iter()
            .find(|t| t.name == self.id)
            .and_then(|t| parse_target_manifest(t.manifest).ok())
            .and_then(|m| m.capabilities);
        capabilities_dict(py, caps.as_ref())
    }

    fn __repr__(&self) -> String {
        format!("Target(id={:?}, ir={:?})", self.id, self.ir)
    }
}

fn ir_str(ir: TargetTemplateIr) -> &'static str {
    match ir {
        TargetTemplateIr::Ast => "ast",
        TargetTemplateIr::Flat => "flat",
        TargetTemplateIr::Dae => "dae",
        TargetTemplateIr::Solve => "solve",
    }
}

fn capabilities_dict(py: Python<'_>, caps: Option<&TargetCapabilities>) -> PyObject {
    let dict = PyDict::new_bound(py);
    if let Some(c) = caps {
        let pairs: [(&str, Option<bool>); 7] = [
            ("events", c.events),
            ("runtime_events", c.runtime_events),
            ("initialization", c.initialization),
            ("forward_ad", c.forward_ad),
            ("reverse_ad", c.reverse_ad),
            ("continuous_states", c.continuous_states),
            ("residual_equations", c.residual_equations),
        ];
        for (key, value) in pairs {
            if let Some(v) = value {
                let _ = dict.set_item(key, v);
            }
        }
    }
    dict.into_py(py)
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
pub(crate) fn list_targets() -> Vec<Target> {
    let mut targets: Vec<Target> = builtin_targets()
        .iter()
        .filter_map(|t| {
            let manifest = parse_target_manifest(t.manifest).ok()?;
            Some(Target {
                id: t.name.to_string(),
                ir: ir_str(manifest.ir).to_string(),
                description: manifest.description.clone(),
            })
        })
        .collect();
    targets.sort_by(|a, b| a.id.cmp(&b.id));
    targets
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
        implicit("esdirk34"),
        implicit("trbdf2"),
    ]
}
