use rumoca_core::Span;
use serde::{Deserialize, Serialize};

/// Return a component-path base name with all bracketed subscripts removed.
pub fn component_base_name(name: &str) -> Option<String> {
    rumoca_core::component_path_base_name(name)
}

/// One expanded iteration inside a preserved model for-equation.
///
/// DAE equation buckets still contain scalar residual equations for solver
/// compatibility. This metadata preserves the source loop grouping so codegen
/// backends can choose whether to emit compact loops or unrolled scalar code.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForEquationIteration {
    /// Concrete index values for this iteration, in declaration order.
    pub index_values: Vec<i64>,
    /// Number of DAE equations produced for this iteration.
    pub equation_count: usize,
}

/// Preserved source grouping for an expanded model for-equation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForEquation {
    /// Index variable names in declaration order.
    pub index_names: Vec<String>,
    /// First equation index in the corresponding DAE equation vector.
    #[serde(default)]
    pub first_equation_index: usize,
    /// Per-iteration equation counts.
    pub iterations: Vec<ForEquationIteration>,
    /// Source span for diagnostics.
    pub span: Span,
    /// Human-readable origin description for traceability.
    pub origin: String,
}
