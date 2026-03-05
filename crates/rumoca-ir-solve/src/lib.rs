//! Symbolic prepared solve IR for Rumoca.
//!
//! This crate is intentionally data-only. It defines the serializable schema
//! emitted by solve-preparation phases and consumed by runtime/codegen backends.

use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use serde::{Deserialize, Serialize};

/// Solve-IR schema version.
pub const SOLVE_IR_VERSION: &str = "solve-0.1.0";

fn default_ir_version() -> String {
    SOLVE_IR_VERSION.to_string()
}

/// Canonical symbolic prepared-system IR for solve backends.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SolveIr {
    /// Solve-IR schema version for compatibility checks.
    #[serde(default = "default_ir_version")]
    pub ir_version: String,
    /// Ordered symbolic variable table for deterministic traversal/serialization.
    pub symbols: IndexMap<dae::VarName, SolveVariable>,
    /// Required baseline solve form: implicit residual equations.
    pub implicit_residual: ImplicitResidualForm,
    /// Optional symbolic mass-matrix form, built only when requested.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mass_matrix: Option<MassMatrixForm>,
    /// Optional symbolic causal form, built only when requested.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub causal: Option<CausalForm>,
}

impl SolveIr {
    /// Build an empty solve IR with the current schema version.
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for SolveIr {
    fn default() -> Self {
        Self {
            ir_version: default_ir_version(),
            symbols: IndexMap::new(),
            implicit_residual: ImplicitResidualForm::default(),
            mass_matrix: None,
            causal: None,
        }
    }
}

/// Symbolic variable kind in solve-IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum SolveVariableKind {
    State,
    #[default]
    Algebraic,
    Parameter,
    Input,
    Output,
    DiscreteReal,
    DiscreteValued,
}

/// Symbolic solve variable metadata.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SolveVariable {
    pub kind: SolveVariableKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub start: Option<dae::Expression>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub nominal: Option<dae::Expression>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub unit: Option<String>,
}

/// Symbolic residual row.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResidualEquation {
    pub residual: dae::Expression,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub origin: String,
}

/// Symbolic propagation assignment row.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PropagationAssignment {
    pub target: dae::VarName,
    pub expr: dae::Expression,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub origin: String,
}

/// Implicit residual form (baseline solve contract).
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ImplicitResidualForm {
    #[serde(default)]
    pub residuals: Vec<ResidualEquation>,
    #[serde(default)]
    pub initial_residuals: Vec<ResidualEquation>,
    #[serde(default)]
    pub event_relations: Vec<dae::Expression>,
    #[serde(default)]
    pub propagation: Vec<PropagationAssignment>,
}

/// Symbolic mass-matrix form.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct MassMatrixForm {
    /// Ordered state vector corresponding to rows/columns of `mass_matrix`.
    #[serde(default)]
    pub state_order: Vec<dae::VarName>,
    /// Symbolic mass matrix entries.
    #[serde(default)]
    pub mass_matrix: Vec<Vec<dae::Expression>>,
    /// Symbolic RHS in `M(x,t,p) * xdot = rhs(x,t,p,...)`.
    #[serde(default)]
    pub rhs: Vec<dae::Expression>,
}

/// Symbolic causal form.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CausalForm {
    /// Ordered state vector corresponding to `derivatives`.
    #[serde(default)]
    pub state_order: Vec<dae::VarName>,
    /// Symbolic derivatives (`x_dot = f(...)`).
    #[serde(default)]
    pub derivatives: Vec<dae::Expression>,
    /// Ordered symbolic assignments for remaining channels.
    #[serde(default)]
    pub algebraic_assignments: Vec<PropagationAssignment>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lit(v: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(v))
    }

    #[test]
    fn solve_ir_roundtrip_preserves_forms_and_version() {
        let mut ir = SolveIr::new();
        ir.symbols.insert(
            dae::VarName::new("x"),
            SolveVariable {
                kind: SolveVariableKind::State,
                start: Some(lit(0.0)),
                nominal: Some(lit(1.0)),
                unit: Some("m".to_string()),
            },
        );
        ir.implicit_residual.residuals.push(ResidualEquation {
            residual: lit(1.0),
            origin: "unit-test".to_string(),
        });
        ir.mass_matrix = Some(MassMatrixForm {
            state_order: vec![dae::VarName::new("x")],
            mass_matrix: vec![vec![lit(1.0)]],
            rhs: vec![lit(2.0)],
        });

        let encoded = serde_json::to_string_pretty(&ir).expect("serialize solve-ir");
        let before: serde_json::Value =
            serde_json::from_str(&encoded).expect("decode pre-roundtrip json");
        let decoded: SolveIr = serde_json::from_str(&encoded).expect("deserialize solve-ir");
        let after_encoded = serde_json::to_string_pretty(&decoded).expect("re-serialize solve-ir");
        let after: serde_json::Value =
            serde_json::from_str(&after_encoded).expect("decode post-roundtrip json");
        assert_eq!(decoded.ir_version, SOLVE_IR_VERSION);
        assert_eq!(before, after);
    }

    #[test]
    fn solve_ir_symbol_order_is_deterministic() {
        let mut ir = SolveIr::new();
        ir.symbols.insert(
            dae::VarName::new("b"),
            SolveVariable {
                kind: SolveVariableKind::Algebraic,
                ..SolveVariable::default()
            },
        );
        ir.symbols.insert(
            dae::VarName::new("a"),
            SolveVariable {
                kind: SolveVariableKind::State,
                ..SolveVariable::default()
            },
        );

        let json = serde_json::to_string(&ir).expect("serialize deterministic solve-ir");
        let b_ix = json.find("\"b\"").expect("find b symbol key");
        let a_ix = json.find("\"a\"").expect("find a symbol key");
        assert!(
            b_ix < a_ix,
            "expected insertion order in serialized symbols map: {json}"
        );
    }
}
