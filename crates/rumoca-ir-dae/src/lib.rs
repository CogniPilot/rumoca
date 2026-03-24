//! Hybrid DAE representation for the Rumoca compiler (MLS Appendix B).
//!
//! This crate defines the canonical DAE form per MLS Appendix B (B.1):
//!
//! ```text
//! 0 = f_x(v, c)                    (B.1a) — implicit continuous equations
//! z = f_z(v, c)                    (B.1b) — discrete Real updates (at events)
//! m := f_m(v, c)                   (B.1c) — discrete-valued updates (Boolean, Integer)
//! c := f_c(relation(v))            (B.1d) — conditions
//! ```
//!
//! Where: `v := [p; t; ẋ; x; y; z; m; pre(z); pre(m)]`
//!
//! Variables are classified by role:
//! - Parameters (p), States (x), Algebraics (y)
//! - Discrete Reals (z), Discrete-valued (m: Boolean, Integer, enum)
//! - Inputs (u), Outputs (w), Constants
//!
//! Continuous equations are ONE implicit set (f_x). Equation classification
//! (which equation solves for which variable) is structural analysis work.

use indexmap::{IndexMap, IndexSet};
use rumoca_core::Span;
use serde::{Deserialize, Serialize};

mod fold_start_values;
mod types;
pub mod visitor;
pub use fold_start_values::{
    fold_start_values_to_literals, sort_algebraics_by_equation_deps, sort_parameters_by_start_deps,
};
pub use types::{
    BuiltinFunction, ComponentRefPart, ComponentReference, ComprehensionIndex,
    DerivativeAnnotation, Expression, ExternalFunction, ForIndex, Function, FunctionParam, Literal,
    Statement, StatementBlock, Subscript, VarName, component_base_name, extract_algorithm_outputs,
};
pub use visitor::{
    AlgorithmOutputCollector, ContainsDerChecker, ContainsDerOfStateChecker, ExpressionVisitor,
    ImplicitSampleChecker, StateVariableCollector, StatementVisitor, VarRefCollector,
    VarRefWithSubscriptsCollector,
};

/// Detailed breakdown of balance calculation components.
#[derive(Debug, Clone)]
pub struct BalanceDetail {
    pub state_unknowns: usize,
    pub alg_unknowns: usize,
    pub output_unknowns: usize,
    pub f_x_scalar: usize,
    pub algorithm_outputs: usize,
    pub when_eq_scalar: usize,
    pub interface_flow_count: usize,
    pub overconstrained_interface_count: i64,
    pub oc_break_edge_scalar_count: usize,
}

impl std::fmt::Display for BalanceDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let unknowns = (self.state_unknowns + self.alg_unknowns + self.output_unknowns) as i64;
        let brk = self.oc_break_edge_scalar_count as i64;
        let available_oc_interface = self.overconstrained_interface_count.max(0);
        let base_without_iflow =
            (self.f_x_scalar + self.algorithm_outputs + self.when_eq_scalar) as i64;
        // Interface-flow contribution only closes a remaining deficit.
        let iflow_needed = (unknowns - base_without_iflow).max(0);
        let effective_iflow = (self.interface_flow_count as i64).min(iflow_needed);
        let base_equations = base_without_iflow + effective_iflow;
        let oc_needed = (unknowns - base_equations).max(0);
        let effective_oc_interface = available_oc_interface.min(oc_needed);
        let raw_equations = base_equations + effective_oc_interface;
        let raw_balance = raw_equations - unknowns;
        let effective_brk = brk.min(raw_balance.max(0));
        let balance = raw_balance - effective_brk;
        writeln!(
            f,
            "  Unknowns: {} = states({}) + alg({}) + out({})",
            unknowns, self.state_unknowns, self.alg_unknowns, self.output_unknowns
        )?;
        writeln!(
            f,
            "  Equations: {} = f_x({}) + algo({}) + when({}) + iflow({}) + oc({}) - brk({})",
            raw_equations - effective_brk,
            self.f_x_scalar,
            self.algorithm_outputs,
            self.when_eq_scalar,
            effective_iflow,
            effective_oc_interface,
            effective_brk
        )?;
        write!(f, "  Balance: {}", balance)
    }
}

/// Scalar counts for canonical runtime variable partitions.
///
/// MLS Appendix B notation:
/// - `p`: parameters + constants
/// - `t`: independent time variable (always 1)
/// - `x`: continuous states
/// - `y`: continuous algebraics (including outputs)
/// - `z`: discrete Real variables
/// - `m`: discrete-valued variables (Boolean/Integer/enum)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RuntimePartitionScalarCounts {
    pub p: usize,
    pub t: usize,
    pub x: usize,
    pub y: usize,
    pub z: usize,
    pub m: usize,
}

/// Solver-agnostic periodic clock schedule descriptor.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClockSchedule {
    /// Positive tick period in seconds.
    pub period_seconds: f64,
    /// Tick phase offset in seconds.
    pub phase_seconds: f64,
}

/// Hybrid DAE representation (MLS Appendix B).
///
/// Equations follow the MLS B.1 canonical form:
/// - `f_x`: 0 = f_x(v, c) — implicit continuous equations (B.1a)
/// - `f_z`: z = f_z(v, c) — discrete Real updates at events (B.1b)
/// - `f_m`: m := f_m(v, c) — discrete-valued updates (B.1c)
/// - `f_c`: c := f_c(relation(v)) — conditions (B.1d)
///
/// Runtime contract:
/// - Variable partitions are explicit and disjoint (`p`, `x`, `y`, `z`, `m`),
///   with `t` represented implicitly as the simulation independent variable.
/// - Runtime/equation partitions are explicit vectors and always present in
///   the schema: `f_x`, `f_z`, `f_m`, `f_c`, `relation`,
///   `synthetic_root_conditions`, `initial_equations`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Dae {
    // ── Variables ──────────────────────────────────────────────────────
    /// State variables (x) - continuous variables with derivatives.
    #[serde(rename = "x")]
    pub states: IndexMap<VarName, Variable>,
    /// Algebraic variables (y) - variables without derivatives.
    #[serde(rename = "y")]
    pub algebraics: IndexMap<VarName, Variable>,
    /// Input variables (u) - known externally provided values.
    #[serde(rename = "u")]
    pub inputs: IndexMap<VarName, Variable>,
    /// Output variables (w) - computed from states/algebraics.
    #[serde(rename = "w")]
    pub outputs: IndexMap<VarName, Variable>,
    /// Parameters (p) - fixed values during simulation.
    /// MLS Appendix B groups parameters/constants under p; constants remain
    /// separately tracked in `constants` for compiler/runtime behavior.
    #[serde(rename = "p")]
    pub parameters: IndexMap<VarName, Variable>,
    /// Constants - fixed values at compile time.
    #[serde(rename = "constants")]
    pub constants: IndexMap<VarName, Variable>,
    /// Discrete Real variables (z) - change only at events (MLS B.1b).
    #[serde(rename = "z")]
    pub discrete_reals: IndexMap<VarName, Variable>,
    /// Discrete-valued variables (m) - Boolean, Integer, enum (MLS B.1c).
    #[serde(rename = "m")]
    pub discrete_valued: IndexMap<VarName, Variable>,
    /// Derivative alias variables - defined by ODE equations but not states.
    /// e.g., `omega` in `omega = der(gamma)` is a derivative alias.
    /// These are not counted as algebraic unknowns because they're defined by ODEs.
    #[serde(rename = "x_dot_alias")]
    pub derivative_aliases: IndexMap<VarName, Variable>,

    // ── Equations (MLS B.1) ───────────────────────────────────────────
    /// Continuous implicit equations: 0 = f_x(v, c) (MLS B.1a).
    /// All continuous equations in one unified set — no ODE/algebraic/output split.
    #[serde(rename = "f_x")]
    pub f_x: Vec<Equation>,
    /// Discrete Real update equations: z = f_z(v, c) (MLS B.1b).
    /// Extracted from when-clauses that assign to Real variables.
    #[serde(rename = "f_z")]
    pub f_z: Vec<Equation>,
    /// Discrete-valued update equations: m := f_m(v, c) (MLS B.1c).
    /// Extracted from when-clauses that assign to Boolean/Integer/enum variables.
    #[serde(rename = "f_m")]
    pub f_m: Vec<Equation>,
    /// Condition equations: c := f_c(relation(v)) (MLS B.1d).
    /// Canonically populated during ToDAE from if/when conditions.
    #[serde(rename = "f_c")]
    pub f_c: Vec<Equation>,
    /// Relation expressions used by `f_c(relation(v))` (MLS B.1d).
    #[serde(default)]
    pub relation: Vec<Expression>,
    /// Extra root conditions synthesized from equation expressions beyond canonical `relation`.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub synthetic_root_conditions: Vec<Expression>,
    /// Scheduled discontinuity instants derived at compile time.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub scheduled_time_events: Vec<f64>,
    /// Clock constructor expressions extracted from discrete update equations.
    /// These are evaluated against simulation parameters to build tick schedules.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub clock_constructor_exprs: Vec<Expression>,
    /// Lowered periodic clock schedules.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub clock_schedules: Vec<ClockSchedule>,
    /// Triggered clock conditions — boolean expressions from non-static Clock()
    /// constructors that cannot be resolved to periodic schedules at compile time.
    /// These conditions must be evaluated at runtime to determine clock ticks.
    #[serde(default)]
    pub triggered_clock_conditions: Vec<Expression>,
    /// Per-variable effective clock interval (seconds) for clocked variables.
    ///
    /// Keys use canonical flattened variable names (e.g. `pulse.simTime`).
    /// This enables spec-compliant evaluation of `interval(v)` where `v` is a
    /// clocked variable and the source clock is implicit.
    #[serde(default)]
    pub clock_intervals: IndexMap<String, f64>,

    /// Initial equations.
    pub initial_equations: Vec<Equation>,

    /// True if the model is declared with the `partial` keyword.
    /// MLS §4.7: Partial models are incomplete and shouldn't be balance-checked.
    pub is_partial: bool,
    /// The class type of the root model.
    #[serde(default)]
    pub class_type: rumoca_ir_core::ClassType,

    /// User-defined functions used by this model (MLS §12).
    pub functions: IndexMap<VarName, Function>,
    /// Enumeration literal ordinal map (MLS §4.9.5, 1-based ordinals).
    ///
    /// Keys are canonical literal paths (e.g.
    /// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`), values are
    /// integer ordinals used by runtime numeric evaluation.
    #[serde(default)]
    pub enum_literal_ordinals: IndexMap<String, i64>,

    /// Count of interface flow variables (MLS §4.7).
    /// Per MLS §4.7, flow variables in top-level public connectors count toward
    /// the local equation size, not as unknowns. This is because they will receive
    /// their defining equations from external connections when the component is used.
    /// Interface connectors are identified by containing flow variables but not
    /// contributing any behavioral equations (only connectors, not model components).
    #[serde(default)]
    pub interface_flow_count: usize,

    /// Overconstrained interface balance correction (MLS §4.8, §9.4).
    ///
    /// For overconstrained connector types (e.g., QuasiStatic Reference), this is:
    ///   N_vars - N_linking_equations - N_explicit_roots
    /// where N_vars is the number of overconstrained unknowns, N_linking_equations
    /// is the number of equations already linking them (connection equalities +
    /// component equations), and N_explicit_roots is the number of explicit root
    /// equations (from Connections.root/isRoot).
    ///
    /// This can be negative when the connection graph has cycles (redundant
    /// equations), which is common in polyphase models with adapter components.
    #[serde(default)]
    pub overconstrained_interface_count: i64,

    /// Scalar count of excess equations from VCG break edges (MLS §9.4).
    /// Break edges in the overconstrained connection graph generate equality equations
    /// that should be replaced by `equalityConstraint()` calls. This correction
    /// tracks the number of excess equation scalars.
    #[serde(default)]
    pub oc_break_edge_scalar_count: usize,

    /// Optional description string from the root class declaration.
    #[serde(default)]
    pub model_description: Option<String>,
}

impl Dae {
    /// Create a new empty DAE.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get total number of variables.
    pub fn num_variables(&self) -> usize {
        self.states.len()
            + self.algebraics.len()
            + self.inputs.len()
            + self.outputs.len()
            + self.parameters.len()
            + self.constants.len()
            + self.discrete_reals.len()
            + self.discrete_valued.len()
    }

    /// Compute scalar sizes for runtime partitions `(p, t, x, y, z, m)`.
    pub fn runtime_partition_scalar_counts(&self) -> RuntimePartitionScalarCounts {
        RuntimePartitionScalarCounts {
            p: self.parameters.values().map(|v| v.size()).sum::<usize>()
                + self.constants.values().map(|v| v.size()).sum::<usize>(),
            t: 1,
            x: self.states.values().map(|v| v.size()).sum(),
            y: self
                .algebraics
                .values()
                .chain(self.outputs.values())
                .map(|v| v.size())
                .sum(),
            z: self.discrete_reals.values().map(|v| v.size()).sum(),
            m: self.discrete_valued.values().map(|v| v.size()).sum(),
        }
    }

    /// Get total number of continuous equations (f_x).
    pub fn num_equations(&self) -> usize {
        self.f_x.len()
    }
}

/// A variable in the DAE system.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Variable {
    /// Variable name.
    pub name: VarName,
    /// Array dimensions (empty for scalars).
    pub dims: Vec<i64>,
    /// Start value.
    pub start: Option<Expression>,
    /// Fixed attribute (for initial conditions).
    pub fixed: Option<bool>,
    /// Minimum value.
    pub min: Option<Expression>,
    /// Maximum value.
    pub max: Option<Expression>,
    /// Nominal value (for scaling).
    pub nominal: Option<Expression>,
    /// Physical unit.
    pub unit: Option<String>,
    /// State selection hint.
    pub state_select: rumoca_ir_core::StateSelect,
    /// Description string.
    pub description: Option<String>,
    /// True if this parameter is tunable at runtime (FMI 3.0 ConfigurationMode).
    /// Structural parameters (evaluate=true, Integer/Boolean used for sizing)
    /// remain fixed; all other parameters are tunable.
    #[serde(default)]
    pub is_tunable: bool,
}

impl Variable {
    /// Create a new variable with the given name.
    pub fn new(name: VarName) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    /// Check if this is a scalar (0-dimensional).
    pub fn is_scalar(&self) -> bool {
        self.dims.is_empty()
    }

    /// Get the total size (product of dimensions, 1 for scalars).
    pub fn size(&self) -> usize {
        if self.dims.is_empty() {
            1
        } else {
            self.dims.iter().map(|&d| d.max(0) as usize).product()
        }
    }
}

/// An equation in the DAE system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Equation {
    /// Left-hand side (variable being defined, if any).
    pub lhs: Option<VarName>,
    /// Right-hand side expression.
    pub rhs: Expression,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
    /// Number of scalar equations this represents (MLS §8.4).
    /// For array equations like `x[n] = expr`, this is n.
    /// For scalar equations, this is 1.
    #[serde(default = "default_scalar_count")]
    pub scalar_count: usize,
}

/// Default scalar count for equations (1 for serde deserialization).
fn default_scalar_count() -> usize {
    1
}

#[cfg(test)]
mod tests {
    use super::{Algorithm, Dae, Equation, VarName, Variable};
    use rumoca_core::Span;

    mod flat {
        pub(crate) use super::super::*;
    }

    fn assignment_stmt(name: &str) -> flat::Statement {
        flat::Statement::Assignment {
            comp: flat::ComponentReference {
                local: false,
                parts: vec![flat::ComponentRefPart {
                    ident: name.to_string(),
                    subs: Vec::new(),
                }],
                def_id: None,
            },
            value: flat::Expression::Literal(flat::Literal::Integer(1)),
        }
    }

    #[test]
    fn test_dae_json_uses_mls_symbol_keys() {
        let dae = Dae::default();
        let value = serde_json::to_value(&dae).expect("DAE should serialize");
        let obj = value
            .as_object()
            .expect("serialized DAE should be a JSON object");

        for key in [
            "x", "y", "u", "w", "p", "z", "m", "f_x", "f_z", "f_m", "f_c",
        ] {
            assert!(obj.contains_key(key), "expected MLS key `{key}`");
        }

        for legacy in [
            "states",
            "algebraics",
            "inputs",
            "outputs",
            "parameters",
            "discrete_reals",
            "discrete_valued",
            "derivative_aliases",
        ] {
            assert!(
                !obj.contains_key(legacy),
                "legacy key `{legacy}` must not be serialized"
            );
        }
    }

    #[test]
    fn test_runtime_partition_scalar_counts() {
        let mut dae = Dae::default();
        dae.parameters.insert(
            VarName::new("p"),
            Variable {
                name: VarName::new("p"),
                ..Default::default()
            },
        );
        dae.constants.insert(
            VarName::new("c"),
            Variable {
                name: VarName::new("c"),
                ..Default::default()
            },
        );
        dae.states.insert(
            VarName::new("x"),
            Variable {
                name: VarName::new("x"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae.algebraics.insert(
            VarName::new("y"),
            Variable {
                name: VarName::new("y"),
                ..Default::default()
            },
        );
        dae.outputs.insert(
            VarName::new("w"),
            Variable {
                name: VarName::new("w"),
                ..Default::default()
            },
        );
        dae.discrete_reals.insert(
            VarName::new("z"),
            Variable {
                name: VarName::new("z"),
                dims: vec![3],
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            VarName::new("m"),
            Variable {
                name: VarName::new("m"),
                ..Default::default()
            },
        );

        let counts = dae.runtime_partition_scalar_counts();
        assert_eq!(counts.p, 2);
        assert_eq!(counts.t, 1);
        assert_eq!(counts.x, 2);
        assert_eq!(counts.y, 2);
        assert_eq!(counts.z, 3);
        assert_eq!(counts.m, 1);
    }

    #[test]
    fn test_dae_algorithm_from_flat_fills_outputs_from_statements() {
        let flat_alg =
            flat::Algorithm::new(vec![assignment_stmt("y")], Span::DUMMY, "algorithm section");

        let dae_alg = Algorithm::new(flat_alg.statements.clone(), flat_alg.span, &flat_alg.origin);
        assert_eq!(dae_alg.outputs, vec![VarName::new("y")]);
    }

    #[test]
    fn test_explicit_with_scalar_count_clamps_zero_to_one() {
        let eq = Equation::explicit_with_scalar_count(
            VarName::new("x"),
            flat::Expression::Literal(flat::Literal::Integer(1)),
            Span::DUMMY,
            "test",
            0,
        );
        assert_eq!(eq.scalar_count, 1);
    }

    #[test]
    fn test_explicit_with_scalar_count_preserves_nonzero_count() {
        let eq = Equation::explicit_with_scalar_count(
            VarName::new("x"),
            flat::Expression::Literal(flat::Literal::Integer(1)),
            Span::DUMMY,
            "test",
            3,
        );
        assert_eq!(eq.scalar_count, 3);
    }
}

impl Equation {
    /// Create a new equation in residual form (0 = rhs).
    pub fn residual(rhs: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self {
            lhs: None,
            rhs,
            span,
            origin: origin.into(),
            scalar_count: 1,
        }
    }

    /// Create a new equation in residual form with explicit scalar count.
    pub fn residual_array(
        rhs: Expression,
        span: Span,
        origin: impl Into<String>,
        scalar_count: usize,
    ) -> Self {
        Self {
            lhs: None,
            rhs,
            span,
            origin: origin.into(),
            scalar_count,
        }
    }

    /// Create a new equation in explicit form (lhs = rhs).
    pub fn explicit(lhs: VarName, rhs: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self::explicit_with_scalar_count(lhs, rhs, span, origin, 1)
    }

    /// Create a new explicit equation with a scalarized equation count.
    ///
    /// The scalar count is clamped to at least 1 so callers cannot accidentally
    /// construct an explicit equation that contributes zero scalars to balance.
    pub fn explicit_with_scalar_count(
        lhs: VarName,
        rhs: Expression,
        span: Span,
        origin: impl Into<String>,
        scalar_count: usize,
    ) -> Self {
        Self {
            lhs: Some(lhs),
            rhs,
            span,
            origin: origin.into(),
            scalar_count: scalar_count.max(1),
        }
    }
}

/// A when clause for discrete event handling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenClause {
    /// The trigger condition.
    pub condition: Expression,
    /// Equations active when triggered.
    pub equations: Vec<Equation>,
    /// Source span for error reporting.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl WhenClause {
    /// Create a new when clause with span information.
    pub fn new(condition: Expression, span: Span, origin: impl Into<String>) -> Self {
        Self {
            condition,
            equations: Vec::new(),
            span,
            origin: origin.into(),
        }
    }
}

/// An algorithm section in the DAE system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Algorithm {
    /// Statements in this algorithm.
    pub statements: Vec<Statement>,
    /// Output variables (left-hand sides of assignments).
    /// Used for balance checking per SPEC_0020.
    pub outputs: Vec<VarName>,
    /// Source span for error reporting.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl Algorithm {
    /// Create a new algorithm section and derive output variables from statements.
    pub fn new(statements: Vec<Statement>, span: Span, origin: impl Into<String>) -> Self {
        let mut outputs = IndexSet::new();
        outputs.extend(extract_algorithm_outputs(&statements));
        Self {
            statements,
            outputs: outputs.into_iter().collect(),
            span,
            origin: origin.into(),
        }
    }

    /// Get the number of outputs (equations contributed).
    pub fn num_outputs(&self) -> usize {
        self.outputs.len()
    }
}

/// Classification of a variable for DAE analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VariableKind {
    /// State variable (has derivative).
    State,
    /// Derivative of a state variable.
    Derivative,
    /// Algebraic variable (no derivative).
    Algebraic,
    /// Input variable (known externally).
    Input,
    /// Output variable (computed result).
    Output,
    /// Parameter (fixed during simulation).
    Parameter,
    /// Constant (fixed at compile time).
    Constant,
    /// Discrete variable (changes at events).
    Discrete,
}
