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

use indexmap::IndexMap;
use rumoca_core::{
    ComponentReference, DefId, Expression, Function, FunctionShapeContractError, Reference, Span,
    Statement, VarName, component_reference_from_flat_name, extract_algorithm_outputs,
};
use serde::ser::{SerializeStruct, SerializeTuple};
use serde::{Deserialize, Serialize};

pub const DAE_SCHEMA_VERSION: u16 = 3;

mod event_threshold;
mod expr_query;
mod types;
pub mod visitor;
pub use event_threshold::{is_event_constant_threshold, is_event_constant_time_threshold_relation};
pub use expr_query::{
    DerivativeNameMatcher, complex_base_alias_match, embedded_subscripts_all_one,
    expr_contains_der_of, expr_contains_der_of_any, expr_contains_var, expr_refers_to_var,
    parse_embedded_subscripts, split_complex_field_suffix, subscripts_all_one,
    subscripts_match_indices, var_ref_matches_unknown,
};
pub use types::{ForEquation, ForEquationIteration, component_base_name};
pub use visitor::{
    AlgorithmOutputCollector, ContainsDerChecker, ContainsDerOfStateChecker, DaeExpressionRewriter,
    DaeVariableMutVisitor, DaeVisitor, ImplicitSampleChecker, StateVariableCollector,
    StatementScope, StatementVisitor, VarRefCollector, VarRefWithSubscriptsCollector,
};

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
    /// Source span for the clock expression that produced this schedule.
    pub source_span: Span,
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
#[derive(Debug, Clone)]
pub struct Dae {
    pub schema_version: u16,
    pub variables: DaeVariables,
    pub continuous: DaeContinuousPartition,
    pub initialization: DaeInitializationPartition,
    pub discrete: DaeDiscretePartition,
    pub conditions: DaeConditionPartition,
    pub events: DaeEventPartition,
    pub clocks: DaeClockPartition,
    pub symbols: DaeSymbolTable,
    pub metadata: DaeMetadata,
}

#[derive(Deserialize)]
struct DaeWire {
    schema_version: u16,
    #[serde(rename = "x")]
    states: IndexMap<VarName, Variable>,
    #[serde(rename = "y")]
    algebraics: IndexMap<VarName, Variable>,
    #[serde(rename = "u")]
    inputs: IndexMap<VarName, Variable>,
    #[serde(rename = "w")]
    outputs: IndexMap<VarName, Variable>,
    #[serde(rename = "p")]
    parameters: IndexMap<VarName, Variable>,
    constants: IndexMap<VarName, Variable>,
    #[serde(rename = "z")]
    discrete_reals: IndexMap<VarName, Variable>,
    #[serde(rename = "m")]
    discrete_valued: IndexMap<VarName, Variable>,
    #[serde(rename = "f_x")]
    continuous_equations: Vec<Equation>,
    #[serde(default)]
    for_equations: Vec<ForEquation>,
    #[serde(rename = "initial_equations")]
    initial_equations: Vec<Equation>,
    #[serde(default, rename = "initial_for_equations")]
    initial_for_equations: Vec<ForEquation>,
    #[serde(rename = "f_z")]
    real_updates: Vec<Equation>,
    #[serde(rename = "f_m")]
    valued_updates: Vec<Equation>,
    #[serde(rename = "f_c")]
    condition_equations: Vec<Equation>,
    #[serde(default, rename = "relation")]
    relations: Vec<Expression>,
    synthetic_root_conditions: Vec<Expression>,
    scheduled_time_events: Vec<f64>,
    event_actions: Vec<DaeEventAction>,
    constructor_exprs: Vec<Expression>,
    schedules: Vec<ClockSchedule>,
    #[serde(default)]
    triggered_conditions: Vec<Expression>,
    #[serde(default)]
    intervals: IndexMap<String, f64>,
    #[serde(default)]
    timings: IndexMap<String, ClockSchedule>,
    functions: IndexMap<VarName, Function>,
    #[serde(default)]
    enum_literal_ordinals: IndexMap<String, i64>,
    metadata: DaeMetadata,
}

impl Default for Dae {
    fn default() -> Self {
        Self {
            schema_version: DAE_SCHEMA_VERSION,
            variables: DaeVariables::default(),
            continuous: DaeContinuousPartition::default(),
            initialization: DaeInitializationPartition::default(),
            discrete: DaeDiscretePartition::default(),
            conditions: DaeConditionPartition::default(),
            events: DaeEventPartition::default(),
            clocks: DaeClockPartition::default(),
            symbols: DaeSymbolTable::default(),
            metadata: DaeMetadata::default(),
        }
    }
}

impl Serialize for Dae {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if !serializer.is_human_readable() {
            let mut tuple = serializer.serialize_tuple(28)?;
            tuple.serialize_element(&self.schema_version)?;
            tuple.serialize_element(&self.variables.states)?;
            tuple.serialize_element(&self.variables.algebraics)?;
            tuple.serialize_element(&self.variables.inputs)?;
            tuple.serialize_element(&self.variables.outputs)?;
            tuple.serialize_element(&self.variables.parameters)?;
            tuple.serialize_element(&self.variables.constants)?;
            tuple.serialize_element(&self.variables.discrete_reals)?;
            tuple.serialize_element(&self.variables.discrete_valued)?;
            tuple.serialize_element(&self.continuous.equations)?;
            tuple.serialize_element(&self.continuous.for_equations)?;
            tuple.serialize_element(&self.initialization.equations)?;
            tuple.serialize_element(&self.initialization.for_equations)?;
            tuple.serialize_element(&self.discrete.real_updates)?;
            tuple.serialize_element(&self.discrete.valued_updates)?;
            tuple.serialize_element(&self.conditions.equations)?;
            tuple.serialize_element(&self.conditions.relations)?;
            tuple.serialize_element(&self.events.synthetic_root_conditions)?;
            tuple.serialize_element(&self.events.scheduled_time_events)?;
            tuple.serialize_element(&self.events.event_actions)?;
            tuple.serialize_element(&self.clocks.constructor_exprs)?;
            tuple.serialize_element(&self.clocks.schedules)?;
            tuple.serialize_element(&self.clocks.triggered_conditions)?;
            tuple.serialize_element(&self.clocks.intervals)?;
            tuple.serialize_element(&self.clocks.timings)?;
            tuple.serialize_element(&self.symbols.functions)?;
            tuple.serialize_element(&self.symbols.enum_literal_ordinals)?;
            tuple.serialize_element(&self.metadata)?;
            return tuple.end();
        }

        let mut state = serializer.serialize_struct("Dae", 28)?;
        state.serialize_field("schema_version", &self.schema_version)?;
        state.serialize_field("x", &self.variables.states)?;
        state.serialize_field("y", &self.variables.algebraics)?;
        state.serialize_field("u", &self.variables.inputs)?;
        state.serialize_field("w", &self.variables.outputs)?;
        state.serialize_field("p", &self.variables.parameters)?;
        state.serialize_field("constants", &self.variables.constants)?;
        state.serialize_field("z", &self.variables.discrete_reals)?;
        state.serialize_field("m", &self.variables.discrete_valued)?;
        state.serialize_field("f_x", &self.continuous.equations)?;
        state.serialize_field("for_equations", &self.continuous.for_equations)?;
        state.serialize_field("initial_equations", &self.initialization.equations)?;
        state.serialize_field("initial_for_equations", &self.initialization.for_equations)?;
        state.serialize_field("f_z", &self.discrete.real_updates)?;
        state.serialize_field("f_m", &self.discrete.valued_updates)?;
        state.serialize_field("f_c", &self.conditions.equations)?;
        state.serialize_field("relation", &self.conditions.relations)?;
        state.serialize_field(
            "synthetic_root_conditions",
            &self.events.synthetic_root_conditions,
        )?;
        state.serialize_field("scheduled_time_events", &self.events.scheduled_time_events)?;
        state.serialize_field("event_actions", &self.events.event_actions)?;
        state.serialize_field("constructor_exprs", &self.clocks.constructor_exprs)?;
        state.serialize_field("schedules", &self.clocks.schedules)?;
        state.serialize_field("triggered_conditions", &self.clocks.triggered_conditions)?;
        state.serialize_field("intervals", &self.clocks.intervals)?;
        state.serialize_field("timings", &self.clocks.timings)?;
        state.serialize_field("functions", &self.symbols.functions)?;
        state.serialize_field("enum_literal_ordinals", &self.symbols.enum_literal_ordinals)?;
        state.serialize_field("metadata", &self.metadata)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for Dae {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = DaeWire::deserialize(deserializer)?;
        if wire.schema_version != DAE_SCHEMA_VERSION {
            return Err(serde::de::Error::custom(format!(
                "unsupported DAE schema_version {}; expected {}",
                wire.schema_version, DAE_SCHEMA_VERSION
            )));
        }

        Ok(Self {
            schema_version: wire.schema_version,
            variables: DaeVariables {
                states: wire.states,
                algebraics: wire.algebraics,
                inputs: wire.inputs,
                outputs: wire.outputs,
                parameters: wire.parameters,
                constants: wire.constants,
                discrete_reals: wire.discrete_reals,
                discrete_valued: wire.discrete_valued,
            },
            continuous: DaeContinuousPartition {
                equations: wire.continuous_equations,
                for_equations: wire.for_equations,
            },
            initialization: DaeInitializationPartition {
                equations: wire.initial_equations,
                for_equations: wire.initial_for_equations,
            },
            discrete: DaeDiscretePartition {
                real_updates: wire.real_updates,
                valued_updates: wire.valued_updates,
            },
            conditions: DaeConditionPartition {
                equations: wire.condition_equations,
                relations: wire.relations,
            },
            events: DaeEventPartition {
                synthetic_root_conditions: wire.synthetic_root_conditions,
                scheduled_time_events: wire.scheduled_time_events,
                event_actions: wire.event_actions,
            },
            clocks: DaeClockPartition {
                constructor_exprs: wire.constructor_exprs,
                schedules: wire.schedules,
                triggered_conditions: wire.triggered_conditions,
                intervals: wire.intervals,
                timings: wire.timings,
            },
            symbols: DaeSymbolTable {
                functions: wire.functions,
                enum_literal_ordinals: wire.enum_literal_ordinals,
            },
            metadata: wire.metadata,
        })
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeVariables {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DaeVariablePartition {
    State,
    Algebraic,
    Input,
    Output,
    Parameter,
    Constant,
    DiscreteReal,
    DiscreteValued,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DaeShapeContractError {
    Variable(VariableShapeContractError),
    Function(FunctionShapeContractError),
    VariableKeyNameMismatch {
        partition: DaeVariablePartition,
        key: VarName,
        name: VarName,
        span: Span,
    },
    FunctionKeyNameMismatch {
        key: VarName,
        name: VarName,
        span: Span,
    },
}

impl DaeShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::Variable(error) => error.span(),
            Self::Function(error) => error.span(),
            Self::VariableKeyNameMismatch { span, .. }
            | Self::FunctionKeyNameMismatch { span, .. } => *span,
        }
    }
}

impl DaeVariables {
    pub fn validate_shape_contract(&self) -> Result<(), DaeShapeContractError> {
        validate_partition_shape_contract(DaeVariablePartition::State, &self.states)?;
        validate_partition_shape_contract(DaeVariablePartition::Algebraic, &self.algebraics)?;
        validate_partition_shape_contract(DaeVariablePartition::Input, &self.inputs)?;
        validate_partition_shape_contract(DaeVariablePartition::Output, &self.outputs)?;
        validate_partition_shape_contract(DaeVariablePartition::Parameter, &self.parameters)?;
        validate_partition_shape_contract(DaeVariablePartition::Constant, &self.constants)?;
        validate_partition_shape_contract(
            DaeVariablePartition::DiscreteReal,
            &self.discrete_reals,
        )?;
        validate_partition_shape_contract(
            DaeVariablePartition::DiscreteValued,
            &self.discrete_valued,
        )?;
        Ok(())
    }
}

fn validate_partition_shape_contract(
    partition: DaeVariablePartition,
    variables: &IndexMap<VarName, Variable>,
) -> Result<(), DaeShapeContractError> {
    for (key, variable) in variables {
        if key != &variable.name {
            return Err(DaeShapeContractError::VariableKeyNameMismatch {
                partition,
                key: key.clone(),
                name: variable.name.clone(),
                span: variable.source_span,
            });
        }
        variable
            .try_size()
            .map_err(DaeShapeContractError::Variable)?;
    }
    Ok(())
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeContinuousPartition {
    /// Continuous implicit equations: 0 = f_x(v, c) (MLS B.1a).
    /// All continuous equations in one unified set — no ODE/algebraic/output split.
    #[serde(rename = "f_x")]
    pub equations: Vec<Equation>,
    /// Preserved source grouping metadata for for-equations whose expanded
    /// continuous equations are present in `f_x`.
    ///
    /// This lets code generators decide whether to emit the grouped loop or
    /// scalarize it for a target that cannot handle structured loops.
    #[serde(default)]
    pub for_equations: Vec<ForEquation>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeInitializationPartition {
    /// Initial equations.
    #[serde(rename = "initial_equations")]
    pub equations: Vec<Equation>,
    /// Preserved source grouping metadata for initial for-equations whose
    /// expanded equations are present in `initial_equations`.
    #[serde(default, rename = "initial_for_equations")]
    pub for_equations: Vec<ForEquation>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeDiscretePartition {
    /// Discrete Real update equations: z = f_z(v, c) (MLS B.1b).
    /// Extracted from when-clauses that assign to Real variables.
    #[serde(rename = "f_z")]
    pub real_updates: Vec<Equation>,
    /// Discrete-valued update equations: m := f_m(v, c) (MLS B.1c).
    /// Extracted from when-clauses that assign to Boolean/Integer/enum variables.
    #[serde(rename = "f_m")]
    pub valued_updates: Vec<Equation>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeConditionPartition {
    /// Condition equations: c := f_c(relation(v)) (MLS B.1d).
    /// Canonically populated during ToDAE from if/when conditions.
    #[serde(rename = "f_c")]
    pub equations: Vec<Equation>,
    /// Relation expressions used by `f_c(relation(v))` (MLS B.1d).
    #[serde(default, rename = "relation")]
    pub relations: Vec<Expression>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeEventPartition {
    /// Extra root conditions synthesized from equation expressions beyond canonical `relation`.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub synthetic_root_conditions: Vec<Expression>,
    /// Scheduled discontinuity instants derived at compile time.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub scheduled_time_events: Vec<f64>,
    /// Runtime actions evaluated at event instants.
    ///
    /// `assert` and `terminate` are integration-flow constructs, not numeric
    /// residual expressions. `reinit` is lowered earlier into guarded discrete
    /// state-update equations and must not appear here.
    pub event_actions: Vec<DaeEventAction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DaeEventAction {
    pub condition: Expression,
    pub kind: DaeEventActionKind,
    pub span: Span,
    pub origin: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DaeEventActionKind {
    Assert { message: Expression },
    Terminate { message: Expression },
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeClockPartition {
    /// Clock constructor expressions extracted from discrete update equations.
    /// These are evaluated against simulation parameters to build tick schedules.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub constructor_exprs: Vec<Expression>,
    /// Lowered periodic clock schedules.
    /// This is canonical runtime metadata (always present in DAE schema).
    pub schedules: Vec<ClockSchedule>,
    /// Triggered clock conditions — boolean expressions from non-static Clock()
    /// constructors that cannot be resolved to periodic schedules at compile time.
    /// These conditions must be evaluated at runtime to determine clock ticks.
    #[serde(default)]
    pub triggered_conditions: Vec<Expression>,
    /// Per-variable effective clock interval (seconds) for clocked variables.
    ///
    /// Keys use canonical flattened variable names (e.g. `pulse.simTime`).
    /// This enables spec-compliant evaluation of `interval(v)` where `v` is a
    /// clocked variable and the source clock is implicit.
    #[serde(default)]
    pub intervals: IndexMap<String, f64>,
    /// Per-variable effective periodic clock timing for clocked variables.
    ///
    /// Keys use canonical flattened variable names. This is the phase-bearing
    /// companion to `intervals`: `interval(v)` only needs the period, but
    /// value-form sample-rate conversion must preserve the variable clock
    /// phase when deriving the converted target tick.
    #[serde(default)]
    pub timings: IndexMap<String, ClockSchedule>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeSymbolTable {
    /// User-defined functions used by this model (MLS §12).
    pub functions: IndexMap<VarName, Function>,
    /// Enumeration literal ordinal map (MLS §4.9.5, 1-based ordinals).
    ///
    /// Keys are canonical literal paths (e.g.
    /// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`), values are
    /// integer ordinals used by runtime numeric evaluation.
    #[serde(default)]
    pub enum_literal_ordinals: IndexMap<String, i64>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DaeMetadata {
    /// True if the model is declared with the `partial` keyword.
    /// MLS §4.7: Partial models are incomplete and shouldn't be balance-checked.
    pub is_partial: bool,
    /// The class type of the root model.
    #[serde(default)]
    pub class_type: rumoca_core::ClassType,
    /// Start expressions for source variables, including variables that may be
    /// eliminated from runtime partitions by aliasing.
    ///
    /// Runtime lowering uses this as semantic metadata for operations such as
    /// synchronous back-sampling, where the value before the first source-clock
    /// tick is the source variable's `start` attribute.
    #[serde(default)]
    pub variable_starts: IndexMap<String, Expression>,
    /// Discrete-valued variables whose source causality is input.
    ///
    /// These are carried in `m` when event/discrete lowering needs them in the
    /// runtime discrete partition, but local balance still treats their values
    /// as externally supplied inputs.
    #[serde(skip)]
    pub discrete_input_names: Vec<String>,
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

    /// DefId ancestry for resolved source symbols, ordered from outermost owner
    /// to the symbol itself. Used for structured balance and lowering queries.
    #[serde(default)]
    pub symbol_ancestry: IndexMap<DefId, Vec<DefId>>,
}

impl Dae {
    /// Create a new empty DAE.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get total number of variables.
    pub fn num_variables(&self) -> usize {
        self.variables.states.len()
            + self.variables.algebraics.len()
            + self.variables.inputs.len()
            + self.variables.outputs.len()
            + self.variables.parameters.len()
            + self.variables.constants.len()
            + self.variables.discrete_reals.len()
            + self.variables.discrete_valued.len()
    }

    /// Compute scalar sizes for runtime partitions `(p, t, x, y, z, m)`.
    pub fn runtime_partition_scalar_counts(&self) -> RuntimePartitionScalarCounts {
        RuntimePartitionScalarCounts {
            p: self
                .variables
                .parameters
                .values()
                .map(|v| v.size())
                .sum::<usize>()
                + self
                    .variables
                    .constants
                    .values()
                    .map(|v| v.size())
                    .sum::<usize>(),
            t: 1,
            x: self.variables.states.values().map(|v| v.size()).sum(),
            y: self
                .variables
                .algebraics
                .values()
                .chain(self.variables.outputs.values())
                .map(|v| v.size())
                .sum(),
            z: self
                .variables
                .discrete_reals
                .values()
                .map(|v| v.size())
                .sum(),
            m: self
                .variables
                .discrete_valued
                .values()
                .map(|v| v.size())
                .sum(),
        }
    }

    /// Get total number of continuous equations (f_x).
    pub fn num_equations(&self) -> usize {
        self.continuous.equations.len()
    }

    pub fn validate_shape_contract(&self) -> Result<(), DaeShapeContractError> {
        self.variables.validate_shape_contract()?;
        for (key, function) in &self.symbols.functions {
            if key != &function.name {
                return Err(DaeShapeContractError::FunctionKeyNameMismatch {
                    key: key.clone(),
                    name: function.name.clone(),
                    span: function.span,
                });
            }
            function
                .validate_shape_contract()
                .map_err(DaeShapeContractError::Function)?;
        }
        Ok(())
    }
}

/// A variable in the DAE system.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct Variable {
    /// Variable name.
    pub name: VarName,
    /// Structured component reference that produced this DAE variable, when it
    /// corresponds directly to a Modelica component.
    ///
    /// Generated variables and backend helper slots leave this empty. Compiler
    /// logic that needs path structure or resolved identity should use this
    /// reference instead of reparsing `name`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub component_ref: Option<ComponentReference>,
    /// Source span for the variable declaration.
    #[serde(default)]
    pub source_span: Span,
    /// Array dimensions (empty for scalars).
    pub dims: Vec<i64>,
    /// Start value.
    pub start: Option<Expression>,
    /// Source span for the start attribute expression.
    pub start_span: Option<Span>,
    /// Fixed attribute (for initial conditions).
    pub fixed: Option<bool>,
    /// Minimum value.
    pub min: Option<Expression>,
    /// Source span for the minimum value attribute expression.
    pub min_span: Option<Span>,
    /// Maximum value.
    pub max: Option<Expression>,
    /// Source span for the maximum value attribute expression.
    pub max_span: Option<Span>,
    /// Nominal value (for scaling).
    pub nominal: Option<Expression>,
    /// Source span for the nominal value attribute expression.
    pub nominal_span: Option<Span>,
    /// Physical unit.
    pub unit: Option<String>,
    /// State selection hint.
    pub state_select: rumoca_core::StateSelect,
    /// Description string.
    pub description: Option<String>,
    /// FMI-style causality metadata for downstream JSON consumers.
    #[serde(default)]
    pub causality: VariableCausality,
    /// True if this parameter is tunable at runtime (FMI 3.0 ConfigurationMode).
    /// Structural parameters (evaluate=true, Integer/Boolean used for sizing)
    /// remain fixed; all other parameters are tunable.
    #[serde(default)]
    pub is_tunable: bool,
    /// Whether this variable corresponds to a source Modelica component or a
    /// compiler-generated Appendix B/backend slot.
    #[serde(default)]
    pub origin: VariableOrigin,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum VariableCausality {
    Input,
    Output,
    #[default]
    Local,
    Parameter,
    CalculatedParameter,
    Independent,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum VariableOrigin {
    #[default]
    Source,
    Generated,
}

impl Serialize for Variable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let include_component_ref = !serializer.is_human_readable() || self.component_ref.is_some();
        let field_count = if include_component_ref { 20 } else { 19 };
        let mut state = serializer.serialize_struct("Variable", field_count)?;
        state.serialize_field("name", &self.name)?;
        if include_component_ref {
            state.serialize_field("component_ref", &self.component_ref)?;
        }
        state.serialize_field("source_span", &self.source_span)?;
        state.serialize_field("dims", &self.dims)?;
        state.serialize_field("start", &self.start)?;
        state.serialize_field("start_span", &self.start_span)?;
        state.serialize_field("fixed", &self.fixed)?;
        state.serialize_field("min", &self.min)?;
        state.serialize_field("min_span", &self.min_span)?;
        state.serialize_field("max", &self.max)?;
        state.serialize_field("max_span", &self.max_span)?;
        state.serialize_field("nominal", &self.nominal)?;
        state.serialize_field("nominal_span", &self.nominal_span)?;
        state.serialize_field("unit", &self.unit)?;
        state.serialize_field("state_select", &self.state_select)?;
        state.serialize_field("description", &self.description)?;
        state.serialize_field("causality", &self.causality)?;
        state.serialize_field("is_tunable", &self.is_tunable)?;
        state.serialize_field("origin", &self.origin)?;
        state.end()
    }
}

impl Variable {
    /// Create a new variable with the given name.
    pub fn new(name: VarName) -> Self {
        let component_ref = component_reference_from_flat_name(&name, Span::DUMMY);
        Self {
            name,
            component_ref,
            ..Default::default()
        }
    }

    /// Check if this is a scalar (0-dimensional).
    pub fn is_scalar(&self) -> bool {
        self.dims.is_empty()
    }

    /// Get the total size (product of dimensions, 1 for scalars).
    pub fn size(&self) -> usize {
        self.try_size()
            .expect("DAE variable dimensions must be non-negative")
    }

    pub fn try_size(&self) -> Result<usize, VariableShapeContractError> {
        shape_size(&self.name, self.source_span, &self.dims)
    }

    pub fn start_attribute_span(&self) -> Option<Span> {
        self.start_span
    }

    pub fn min_attribute_span(&self) -> Option<Span> {
        self.min_span
    }

    pub fn max_attribute_span(&self) -> Option<Span> {
        self.max_span
    }

    pub fn nominal_attribute_span(&self) -> Option<Span> {
        self.nominal_span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableShapeContractError {
    NegativeDimension {
        variable: VarName,
        dimension: i64,
        span: Span,
    },
}

impl VariableShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::NegativeDimension { span, .. } => *span,
        }
    }
}

fn shape_size(
    name: &VarName,
    span: Span,
    dims: &[i64],
) -> Result<usize, VariableShapeContractError> {
    if dims.is_empty() {
        return Ok(1);
    }
    let mut size = 1usize;
    for &dimension in dims {
        let dim = usize::try_from(dimension).map_err(|_| {
            VariableShapeContractError::NegativeDimension {
                variable: name.clone(),
                dimension,
                span,
            }
        })?;
        size = size.saturating_mul(dim);
    }
    Ok(size)
}

/// Convert a zero-based flattened array offset into one-based Modelica
/// subscripts using the variable's DAE shape metadata.
///
/// MLS §10.1: array dimensions are part of the variable's type. Solver/runtime
/// scalar names must therefore be derived from `Variable::dims`, not by parsing
/// or inventing flattened string suffixes.
pub fn flat_index_to_subscripts(dims: &[i64], flat_index: usize) -> Option<Vec<usize>> {
    if dims.is_empty() {
        // Scalar variable: caller should use the name directly, no subscripts.
        return None;
    }

    let mut dims_usize = Vec::with_capacity(dims.len());
    for &dim in dims {
        // None here: a dim value is negative or overflows usize (malformed IR).
        let dim = usize::try_from(dim).ok()?;
        if dim == 0 {
            return None;
        }
        dims_usize.push(dim);
    }

    let mut remainder = flat_index;
    let mut subscripts = Vec::with_capacity(dims_usize.len());
    for dim in dims_usize.iter().rev().copied() {
        subscripts.push((remainder % dim) + 1);
        remainder /= dim;
    }
    if remainder != 0 {
        // flat_index is out-of-bounds for the given shape.
        return None;
    }
    subscripts.reverse();
    Some(subscripts)
}

pub fn format_subscript_key(name: &str, subscripts: &[usize]) -> String {
    let index_text = subscripts
        .iter()
        .map(usize::to_string)
        .collect::<Vec<_>>()
        .join(",");
    format!("{name}[{index_text}]")
}

pub fn scalar_name_for_flat_index(name: &VarName, dims: &[i64], flat_index: usize) -> VarName {
    VarName::new(scalar_name_text_for_flat_index(
        name.as_str(),
        dims,
        flat_index,
    ))
}

pub fn scalar_name_text_for_flat_index(name: &str, dims: &[i64], flat_index: usize) -> String {
    flat_index_to_subscripts(dims, flat_index)
        .filter(|subscripts| subscripts.len() > 1)
        .map(|subscripts| format_subscript_key(name, &subscripts))
        .unwrap_or_else(|| format!("{name}[{}]", flat_index + 1))
}

#[cfg(test)]
mod variable_shape_contract_tests {
    use super::*;

    #[test]
    fn dae_variable_size_preserves_zero_sized_arrays() {
        let variable = Variable {
            name: VarName::new("x"),
            dims: vec![0, 3],
            ..Default::default()
        };

        assert_eq!(variable.try_size(), Ok(0));
    }

    #[test]
    fn dae_variable_shape_contract_rejects_negative_dims() {
        let variable = Variable {
            name: VarName::new("x"),
            dims: vec![2, -1],
            ..Default::default()
        };

        assert_eq!(
            variable.try_size(),
            Err(VariableShapeContractError::NegativeDimension {
                variable: VarName::new("x"),
                dimension: -1,
                span: Span::DUMMY,
            })
        );
    }

    #[test]
    fn dae_model_shape_contract_rejects_key_name_mismatch() {
        let mut dae = Dae::new();
        dae.variables.states.insert(
            VarName::new("key"),
            Variable {
                name: VarName::new("stored"),
                ..Default::default()
            },
        );

        assert_eq!(
            dae.validate_shape_contract(),
            Err(DaeShapeContractError::VariableKeyNameMismatch {
                partition: DaeVariablePartition::State,
                key: VarName::new("key"),
                name: VarName::new("stored"),
                span: Span::DUMMY,
            })
        );
    }

    #[test]
    fn dae_model_shape_contract_rejects_function_param_negative_dims() {
        let mut dae = Dae::new();
        let mut function = Function::new("Pkg.f", Span::DUMMY);
        function.add_output(rumoca_core::FunctionParam::new("y", "Real").with_dims(vec![-1]));
        dae.symbols
            .functions
            .insert(VarName::new("Pkg.f"), function);

        assert!(matches!(
            dae.validate_shape_contract(),
            Err(DaeShapeContractError::Function(
                rumoca_core::FunctionShapeContractError::Param { .. }
            ))
        ));
    }
}

/// An equation in the DAE system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Equation {
    /// Left-hand side reference (variable being defined, if any).
    ///
    /// Explicit equations preserve the structured component reference that
    /// produced the lhs. Semantic phases must use this instead of recovering
    /// path structure from the rendered variable name.
    pub lhs: Option<Reference>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum WhenEquationInactiveRhs {
    #[default]
    Pre,
    Current,
}

#[cfg(test)]
mod tests {
    use super::{
        Algorithm, ClockSchedule, DAE_SCHEMA_VERSION, Dae, Equation, VarName, Variable,
        VariableCausality, flat_index_to_subscripts, scalar_name_text_for_flat_index,
    };
    use rumoca_core::{Expression, Literal, OpBinary, OpUnary, Reference, Span};

    const LOGICAL_NETWORK1_DAE_GOLDEN: &str =
        include_str!("../tests/golden/modelica_blocks_examples_logical_network1.dae.json");

    mod flat {
        pub(crate) use super::super::*;
    }

    fn fixture_span() -> Span {
        Span::from_offsets(rumoca_core::SourceId(1), 10, 20)
    }

    fn lit_real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: fixture_span(),
        }
    }

    fn lit_bool(value: bool) -> Expression {
        Expression::Literal {
            value: Literal::Boolean(value),
            span: fixture_span(),
        }
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::from(name),
            subscripts: Vec::new(),
            span: fixture_span(),
        }
    }

    fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: fixture_span(),
        }
    }

    fn logical_network1_dae_fixture() -> Dae {
        let mut dae = Dae::default();
        dae.variables.parameters.insert(
            VarName::new("samplePeriod"),
            Variable {
                name: VarName::new("samplePeriod"),
                start: Some(lit_real(0.1)),
                fixed: Some(true),
                unit: Some("s".to_string()),
                description: Some("Periodic Boolean sample interval".to_string()),
                causality: VariableCausality::CalculatedParameter,
                ..Default::default()
            },
        );
        dae.variables.discrete_reals.insert(
            VarName::new("hold.y"),
            Variable {
                name: VarName::new("hold.y"),
                start: Some(lit_real(0.0)),
                description: Some("Latched numeric visualization of logical state".to_string()),
                ..Default::default()
            },
        );
        dae.variables.discrete_valued.insert(
            VarName::new("and1.y"),
            Variable {
                name: VarName::new("and1.y"),
                start: Some(lit_bool(false)),
                description: Some("Logical block output".to_string()),
                ..Default::default()
            },
        );
        dae.variables.discrete_valued.insert(
            VarName::new("not1.y"),
            Variable {
                name: VarName::new("not1.y"),
                start: Some(lit_bool(true)),
                description: Some("Logical inversion output".to_string()),
                ..Default::default()
            },
        );

        let relation = binary(OpBinary::Ge, var_ref("time"), var_ref("samplePeriod"));
        dae.conditions.relations.push(relation.clone());
        dae.conditions.equations.push(Equation::explicit(
            VarName::new("c[1]"),
            relation,
            Span::DUMMY,
            "LogicalNetwork1 sample trigger",
        ));
        dae.discrete.valued_updates.push(Equation::explicit(
            VarName::new("and1.y"),
            binary(OpBinary::And, var_ref("greater.y"), var_ref("pulse.y")),
            Span::DUMMY,
            "when sample trigger then and1.y",
        ));
        dae.discrete.valued_updates.push(Equation::explicit(
            VarName::new("not1.y"),
            Expression::Unary {
                op: OpUnary::Not,
                rhs: Box::new(var_ref("and1.y")),
                span: fixture_span(),
            },
            Span::DUMMY,
            "when sample trigger then not1.y",
        ));
        dae.discrete.real_updates.push(Equation::explicit(
            VarName::new("hold.y"),
            Expression::If {
                branches: vec![(var_ref("and1.y"), lit_real(1.0))],
                else_branch: Box::new(lit_real(0.0)),
                span: fixture_span(),
            },
            Span::DUMMY,
            "when sample trigger then hold.y",
        ));
        dae.events.scheduled_time_events.push(0.1);
        dae.clocks.schedules.push(ClockSchedule {
            period_seconds: 0.1,
            phase_seconds: 0.0,
            source_span: Span::DUMMY,
        });
        dae.metadata.model_description = Some(
            "Schema golden derived from Modelica.Blocks.Examples.LogicalNetwork1 surface: Boolean network with timed trigger"
                .to_string(),
        );
        dae
    }

    fn assert_same_json_shape<T: serde::Serialize>(actual: &T, expected: &T) {
        assert_eq!(
            serde_json::to_value(actual).expect("serialize actual"),
            serde_json::to_value(expected).expect("serialize expected")
        );
    }

    fn assignment_stmt(name: &str) -> rumoca_core::Statement {
        rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference {
                local: false,
                span: rumoca_core::Span::DUMMY,
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: name.to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                }],
                def_id: None,
            },
            value: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_dae_json_uses_mls_symbol_keys() {
        let dae = Dae::default();
        let value = serde_json::to_value(&dae).expect("DAE should serialize");
        let obj = value
            .as_object()
            .expect("serialized DAE should be a JSON object");

        assert_eq!(
            obj.get("schema_version")
                .and_then(serde_json::Value::as_u64),
            Some(u64::from(DAE_SCHEMA_VERSION))
        );

        for key in [
            "x", "y", "u", "w", "p", "z", "m", "f_x", "f_z", "f_m", "f_c",
        ] {
            assert!(obj.contains_key(key), "expected MLS key `{key}`");
        }
    }

    #[test]
    fn variable_json_surfaces_causality() {
        let variable = Variable {
            name: VarName::new("u"),
            causality: VariableCausality::Input,
            ..Default::default()
        };
        let value = serde_json::to_value(variable).expect("variable should serialize");

        assert_eq!(value["causality"], serde_json::json!("input"));
    }

    #[test]
    fn test_dae_json_requires_supported_schema_version() {
        let value = serde_json::to_value(Dae::default()).expect("DAE should serialize");

        let mut missing = value.clone();
        missing
            .as_object_mut()
            .expect("DAE JSON should be object")
            .remove("schema_version");
        assert!(
            serde_json::from_value::<Dae>(missing).is_err(),
            "DAE JSON must carry an explicit schema_version"
        );

        let mut unsupported = value;
        unsupported["schema_version"] = serde_json::json!(DAE_SCHEMA_VERSION + 1);
        let err = serde_json::from_value::<Dae>(unsupported)
            .expect_err("unsupported DAE schema version must fail");
        assert!(err.to_string().contains("unsupported DAE schema_version"));
    }

    #[test]
    fn logical_network1_dae_json_matches_committed_golden() {
        let expected: serde_json::Value =
            serde_json::from_str(LOGICAL_NETWORK1_DAE_GOLDEN).expect("valid DAE golden JSON");
        let actual =
            serde_json::to_value(logical_network1_dae_fixture()).expect("fixture serializes");
        assert_eq!(actual, expected);

        let decoded: Dae =
            serde_json::from_value(expected).expect("golden uses supported DAE schema");
        assert_eq!(
            decoded.metadata.model_description.as_deref(),
            Some(
                "Schema golden derived from Modelica.Blocks.Examples.LogicalNetwork1 surface: Boolean network with timed trigger",
            )
        );
    }

    #[test]
    fn representative_dae_json_roundtrip_preserves_schema_shape() {
        let dae = logical_network1_dae_fixture();
        let json = serde_json::to_string_pretty(&dae).expect("serialize representative DAE");
        let decoded: Dae = serde_json::from_str(&json).expect("deserialize representative DAE");
        assert_same_json_shape(&decoded, &dae);
    }

    #[test]
    fn representative_dae_bincode_roundtrip_preserves_schema_shape() {
        let dae = logical_network1_dae_fixture();
        let bytes = bincode::serialize(&dae).expect("serialize representative DAE as bincode");
        let decoded: Dae =
            bincode::deserialize(&bytes).expect("deserialize representative DAE from bincode");
        assert_same_json_shape(&decoded, &dae);
    }

    #[test]
    fn test_runtime_partition_scalar_counts() {
        let mut dae = Dae::default();
        dae.variables.parameters.insert(
            VarName::new("p"),
            Variable {
                name: VarName::new("p"),
                ..Default::default()
            },
        );
        dae.variables.constants.insert(
            VarName::new("c"),
            Variable {
                name: VarName::new("c"),
                ..Default::default()
            },
        );
        dae.variables.states.insert(
            VarName::new("x"),
            Variable {
                name: VarName::new("x"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae.variables.algebraics.insert(
            VarName::new("y"),
            Variable {
                name: VarName::new("y"),
                ..Default::default()
            },
        );
        dae.variables.outputs.insert(
            VarName::new("w"),
            Variable {
                name: VarName::new("w"),
                ..Default::default()
            },
        );
        dae.variables.discrete_reals.insert(
            VarName::new("z"),
            Variable {
                name: VarName::new("z"),
                dims: vec![3],
                ..Default::default()
            },
        );
        dae.variables.discrete_valued.insert(
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
    fn scalar_name_for_flat_index_uses_shape_metadata() {
        assert_eq!(scalar_name_text_for_flat_index("v", &[3], 1), "v[2]");
        assert_eq!(scalar_name_text_for_flat_index("M", &[3, 4], 4), "M[2,1]");
        assert_eq!(
            flat_index_to_subscripts(&[2, 3, 4], 23),
            Some(vec![2, 3, 4])
        );
    }

    #[test]
    fn test_dae_algorithm_from_flat_fills_outputs_from_statements() {
        let flat_alg =
            flat::Algorithm::new(vec![assignment_stmt("y")], Span::DUMMY, "algorithm section");

        let dae_alg = Algorithm::new(flat_alg.statements.clone(), flat_alg.span, &flat_alg.origin);
        assert_eq!(dae_alg.outputs.len(), 1);
        assert_eq!(dae_alg.outputs[0].as_str(), "y");
        assert!(dae_alg.outputs[0].has_structure());
    }

    #[test]
    fn test_explicit_with_scalar_count_clamps_zero_to_one() {
        let eq = Equation::explicit_with_scalar_count(
            VarName::new("x"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            },
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
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            },
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
    pub fn explicit(
        lhs: impl Into<Reference>,
        rhs: Expression,
        span: Span,
        origin: impl Into<String>,
    ) -> Self {
        Self::explicit_with_scalar_count(lhs, rhs, span, origin, 1)
    }

    /// Create a new explicit equation with a scalarized equation count.
    ///
    /// The scalar count is clamped to at least 1 so callers cannot accidentally
    /// construct an explicit equation that contributes zero scalars to balance.
    pub fn explicit_with_scalar_count(
        lhs: impl Into<Reference>,
        rhs: Expression,
        span: Span,
        origin: impl Into<String>,
        scalar_count: usize,
    ) -> Self {
        Self {
            lhs: Some(structured_lhs_reference(lhs.into(), span)),
            rhs,
            span,
            origin: origin.into(),
            scalar_count: scalar_count.max(1),
        }
    }
}

/// Normalize an explicit-equation target at construction: a bare rendered
/// name gains its structured component reference here, once, so every
/// producer emits structured targets and DAE resolution never parses names.
/// Targets whose rendered subscripts are not static integers stay
/// unstructured and fail resolution loudly.
fn structured_lhs_reference(lhs: Reference, span: Span) -> Reference {
    if lhs.has_structure() || lhs.is_generated() {
        return lhs;
    }
    match rumoca_core::component_reference_from_flat_name(lhs.var_name(), span) {
        Some(component_ref) => Reference::with_component_reference(lhs.as_str(), component_ref),
        None => lhs,
    }
}

/// A when clause for discrete event handling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenClause {
    /// The trigger condition.
    pub condition: Expression,
    /// Equations active when triggered.
    pub equations: Vec<Equation>,
    /// Inactive-guard value for each equation in `equations`.
    pub equation_inactive_rhs: Vec<WhenEquationInactiveRhs>,
    /// Runtime actions active when triggered.
    pub actions: Vec<DaeEventAction>,
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
            equation_inactive_rhs: Vec::new(),
            actions: Vec::new(),
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
    pub outputs: Vec<Reference>,
    /// Source span for error reporting.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl Algorithm {
    /// Create a new algorithm section and derive output variables from statements.
    pub fn new(statements: Vec<Statement>, span: Span, origin: impl Into<String>) -> Self {
        let mut outputs = Vec::new();
        for output in extract_algorithm_outputs(&statements) {
            if !outputs.contains(&output) {
                outputs.push(output);
            }
        }
        Self {
            statements,
            outputs,
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
