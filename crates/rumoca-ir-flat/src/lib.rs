//! Flat Model IR for the Rumoca compiler.
//!
//! This crate defines the Flat Model (MLS §5.6), which represents
//! the flat equation system with globally unique variable names.
//!
//! The Flat Model is produced by the flatten phase from the Instance Tree.

pub mod clocks;
#[cfg(test)]
mod component_ref_helpers;
pub mod connections;
#[cfg(test)]
mod convert_from_ast;
pub mod name_utils;
#[cfg(test)]
mod subscripts;
pub mod visitor;
mod when_equations;

#[cfg(test)]
mod tests;

#[cfg(test)]
use convert_from_ast::{
    component_reference_from_ast_with_def_map, expression_from_ast,
    expression_from_ast_with_def_map, expression_from_component_ref,
};
use indexmap::{IndexMap, IndexSet};
use rumoca_core::{
    BuiltinFunction, Causality, ClassType, ComponentReference, ComprehensionTemplate, DefId,
    Expression, ForIndex, Function, FunctionInstanceId, FunctionShapeContractError, Reference,
    RegularForFamily, Span, StateSelect, Statement, StatementBlock, StructuredIndexDomain,
    Subscript, SymbolAncestry, TypeId, VarName, Variability,
};
#[cfg(test)]
use rumoca_core::{ComprehensionIndex, Literal};
#[cfg(test)]
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};

pub type VarNameIndexMap<V> = IndexMap<VarName, V, rustc_hash::FxBuildHasher>;
pub type SymbolAncestryMap = IndexMap<DefId, SymbolAncestry, rustc_hash::FxBuildHasher>;

#[cfg(test)]
use component_ref_helpers::from_component_ref_with_def_map_impl;
#[cfg(test)]
use subscripts::subscript_from_ast;

// Re-export connection types
pub use connections::{
    ConnectedVariable, ConnectionGraph, ConnectionSet, ConnectionSets, EqualityConstraint,
    GraphEdge, GraphNode, RootStatus, SpanningTree, SpanningTreeEdge,
};

// Re-export clock types
pub use clocks::{
    BaseClock, BaseClockPartition, ClockKind, ClockPartitions, SubClock, SubClockPartition,
};
pub use name_utils::component_base_name;

// Re-export visitor types
pub use visitor::{
    AlgorithmOutputCollector, ContainsDerChecker, FallibleStatementVisitor, FunctionCallCollector,
    StateVariableCollector, StatementScope, StatementVisitor, VarRefCollector,
};

pub use when_equations::{WhenClause, WhenEquation};

/// MLS §5.6: "flat equation system with globally unique variable names"
///
/// The Flat Model is the result of flattening, containing all variables
/// with globally unique names and all equations ready for analysis.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Model {
    /// All variables with globally unique names.
    pub variables: VarNameIndexMap<Variable>,
    /// Resolved record containers retained for record-equation lowering.
    ///
    /// Record fields remain the authoritative Flat variables. This table only
    /// preserves the compact type identity discarded when containers expand.
    #[serde(default)]
    pub record_instances: VarNameIndexMap<RecordInstance>,
    /// Resolved field layout for each record declaration used by an instance.
    ///
    /// This is type metadata, not callable-function reachability. Record
    /// equations use it to preserve one tensor equation per declared field.
    pub record_types: IndexMap<DefId, RecordType, rustc_hash::FxBuildHasher>,
    /// Declared flat-output type name for each variable (e.g., Boolean, Integer, MyEnum).
    ///
    /// Keys match `variables` and values preserve resolved type identity for rendering.
    #[serde(default)]
    pub variable_type_names: VarNameIndexMap<String>,
    /// Flat-output `final` qualifier flags keyed by variable name (MLS §7.2.6).
    ///
    /// When present and true, codegen should emit `final` before the declaration prefix.
    #[serde(default)]
    pub variable_final_flags: VarNameIndexMap<bool>,
    /// Regular equations (0 = residual form).
    pub equations: Vec<Equation>,
    /// Structured source equation families for regular equations.
    #[serde(default)]
    pub structured_equations: Vec<StructuredEquationFamily>,
    /// Runtime assertion equations from regular equation sections (MLS §8.3.7).
    ///
    /// Assertions are preserved for flat output but do not contribute to DAE
    /// equation balance/unknown counts.
    #[serde(default)]
    pub assert_equations: Vec<AssertEquation>,
    /// Initial equations (0 = residual form).
    pub initial_equations: Vec<Equation>,
    /// Structured source equation families for initial equations.
    #[serde(default)]
    pub initial_structured_equations: Vec<StructuredEquationFamily>,
    /// Runtime assertion equations from initial equation sections (MLS §8.6, §8.3.7).
    #[serde(default)]
    pub initial_assert_equations: Vec<AssertEquation>,
    /// Algorithm sections.
    pub algorithms: Vec<Algorithm>,
    /// Initial algorithm sections.
    pub initial_algorithms: Vec<Algorithm>,
    /// When clauses.
    pub when_clauses: Vec<WhenClause>,
    /// User-defined functions used by this model (MLS §12).
    pub functions: VarNameIndexMap<Function>,
    /// True if the model is declared with the `partial` keyword.
    /// MLS §4.7: Partial models are incomplete and shouldn't be balance-checked.
    pub is_partial: bool,
    /// The class type of the root model (model, connector, record, etc.)
    #[serde(default)]
    pub class_type: ClassType,
    /// Optional description string from the root class declaration.
    pub model_description: Option<String>,
    /// Connectors with Connections.root() declarations (MLS §9.4.1).
    /// These are definite roots for overconstrained connectors, providing
    /// implicit equations that don't need to come from external connections.
    /// Stores the full path to the overconstrained record (e.g., "pin_p.reference").
    #[serde(default)]
    pub definite_roots: IndexSet<String>,
    /// Branches from Connections.branch(a, b) calls (MLS §9.4).
    /// Required edges in the virtual connection graph.
    #[serde(default)]
    pub branches: Vec<(String, String)>,
    /// Optional edges derived from connect() statements for overconstrained nodes (MLS §9.4).
    /// These are used together with `branches` when building the virtual connection graph.
    #[serde(default)]
    pub optional_edges: Vec<(String, String)>,
    /// Potential roots from Connections.potentialRoot(a, priority) calls (MLS §9.4).
    #[serde(default)]
    pub potential_roots: Vec<(String, i64)>,
    /// Names of top-level components whose class type is `connector` (MLS §4.7).
    /// Per MLS §4.7, only flow variables in top-level public connector components
    /// count toward the local equation size for balance checking. Components of
    /// type `model` or `block` (like Delta in transformers) are NOT interface
    /// connectors even if they contain connectors internally.
    #[serde(default)]
    pub top_level_connectors: IndexSet<String>,
    /// Names of top-level components declared with `input` causality.
    /// Fields of these components (e.g., `state.phase` from `input Record state`)
    /// are external inputs and should NOT be promoted to algebraic unknowns,
    /// unlike sub-component inputs from type interfaces (MLS §4.4.2.2).
    #[serde(default)]
    pub top_level_input_components: IndexSet<String>,
    /// Scalar count of excess equations from VCG break edges (MLS §9.4).
    /// Break edges in the overconstrained connection graph generate equality equations
    /// that should be replaced by `equalityConstraint()` calls. Until that's implemented,
    /// this correction tracks how many excess equation scalars exist.
    #[serde(default)]
    pub oc_break_edge_scalar_count: usize,
    /// Enumeration literal ordinal map (MLS §4.9.5, 1-based ordinals).
    ///
    /// Keys are canonical literal paths (e.g.
    /// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`), values are
    /// integer ordinals used by runtime numeric evaluation.
    #[serde(default)]
    pub enum_literal_ordinals: IndexMap<String, i64>,
    /// DefId ancestry for resolved source symbols, ordered from outermost owner
    /// to the symbol itself. Used by downstream phases for child/descendant
    /// checks without rendered-name prefix matching.
    #[serde(default)]
    pub symbol_ancestry: SymbolAncestryMap,
}

/// Compact resolved identity for a record container expanded into Flat fields.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordInstance {
    pub component_ref: ComponentReference,
    pub source_span: Span,
    pub canonical_type_id: TypeId,
    pub type_name: String,
    pub type_def_id: DefId,
    pub dims: Vec<i64>,
}

/// Resolved field layout of one record declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordType {
    pub name: String,
    pub fields: Vec<RecordField>,
}

/// One declared record field retained for exact Flat-to-DAE expansion.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordField {
    pub name: String,
    pub def_id: DefId,
    pub dims: Vec<i64>,
}

impl Model {
    /// Create a new empty flat model.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a variable to the model.
    pub fn add_variable(&mut self, name: VarName, var: Variable) {
        self.variables.insert(name, var);
    }

    /// Add an equation to the model.
    pub fn add_equation(&mut self, eq: Equation) {
        self.equations.push(eq);
    }

    /// Add a structured source equation family for regular equations.
    pub fn add_structured_equation(&mut self, family: StructuredEquationFamily) {
        self.structured_equations.push(family);
    }

    /// Add an initial equation to the model.
    pub fn add_initial_equation(&mut self, eq: Equation) {
        self.initial_equations.push(eq);
    }

    /// Add a structured source equation family for initial equations.
    pub fn add_initial_structured_equation(&mut self, family: StructuredEquationFamily) {
        self.initial_structured_equations.push(family);
    }

    /// Add a function definition to the model.
    pub fn add_function(&mut self, mut func: Function) {
        let instance_id = self
            .functions
            .get(&func.name)
            .and_then(|existing| existing.instance_id)
            .unwrap_or_else(|| next_function_instance_id(&self.functions));
        func.instance_id = Some(instance_id);
        self.functions.insert(func.name.clone(), func);
    }

    /// Get a function definition by name.
    pub fn get_function(&self, name: &VarName) -> Option<&Function> {
        self.functions.get(name)
    }

    /// Get the number of variables.
    pub fn num_variables(&self) -> usize {
        self.variables.len()
    }

    /// Get the number of equations.
    pub fn num_equations(&self) -> usize {
        self.equations.len()
    }

    /// Get the number of functions.
    pub fn num_functions(&self) -> usize {
        self.functions.len()
    }

    /// Return parameter names that are fixed at initialization but have no binding equation.
    ///
    /// Per MLS §8.6, parameters default to `fixed=true`. For standalone simulation,
    /// fixed parameters should have explicit bindings.
    pub fn unbound_fixed_parameters(&self) -> Vec<VarName> {
        self.variables
            .iter()
            .filter_map(|(name, var)| {
                if matches!(var.variability, Variability::Parameter(_))
                    && var.fixed.unwrap_or(true)
                    && var.binding.is_none()
                {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// True if any fixed parameter has no binding equation.
    pub fn has_unbound_fixed_parameters(&self) -> bool {
        self.variables.values().any(|var| {
            matches!(var.variability, Variability::Parameter(_))
                && var.fixed.unwrap_or(true)
                && var.binding.is_none()
        })
    }

    pub fn validate_shape_contract(&self) -> Result<(), ModelShapeContractError> {
        for (key, variable) in &self.variables {
            if key != &variable.name {
                return Err(ModelShapeContractError::VariableKeyNameMismatch {
                    key: key.clone(),
                    name: variable.name.clone(),
                    span: variable.source_span,
                });
            }
            variable
                .validate_shape_contract()
                .map_err(ModelShapeContractError::Variable)?;
        }
        let mut function_instances = IndexMap::new();
        for (key, function) in &self.functions {
            if key != &function.name {
                return Err(ModelShapeContractError::FunctionKeyNameMismatch {
                    key: key.clone(),
                    name: function.name.clone(),
                    span: function.span,
                });
            }
            function
                .validate_shape_contract()
                .map_err(ModelShapeContractError::Function)?;
            let instance_id = function.instance_id.ok_or_else(|| {
                ModelShapeContractError::MissingFunctionInstanceId {
                    function: function.name.clone(),
                    span: function.span,
                }
            })?;
            if let Some(first) = function_instances.insert(instance_id, function.name.clone()) {
                return Err(ModelShapeContractError::DuplicateFunctionInstanceId {
                    instance_id,
                    first,
                    second: function.name.clone(),
                    span: function.span,
                });
            }
        }
        Ok(())
    }
}

fn next_function_instance_id(functions: &VarNameIndexMap<Function>) -> FunctionInstanceId {
    let Some(last) = functions
        .values()
        .filter_map(|function| function.instance_id)
        .map(FunctionInstanceId::index)
        .max()
    else {
        return FunctionInstanceId::new(0);
    };
    FunctionInstanceId::new(
        last.checked_add(1)
            .expect("Flat function instance identity space exhausted"),
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModelShapeContractError {
    Variable(VariableShapeContractError),
    Function(FunctionShapeContractError),
    VariableKeyNameMismatch {
        key: VarName,
        name: VarName,
        span: Span,
    },
    FunctionKeyNameMismatch {
        key: VarName,
        name: VarName,
        span: Span,
    },
    MissingFunctionInstanceId {
        function: VarName,
        span: Span,
    },
    DuplicateFunctionInstanceId {
        instance_id: FunctionInstanceId,
        first: VarName,
        second: VarName,
        span: Span,
    },
}

impl ModelShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::Variable(error) => error.span(),
            Self::Function(error) => error.span(),
            Self::VariableKeyNameMismatch { span, .. }
            | Self::FunctionKeyNameMismatch { span, .. }
            | Self::MissingFunctionInstanceId { span, .. }
            | Self::DuplicateFunctionInstanceId { span, .. } => *span,
        }
    }
}

/// Flat variable with globally unique name.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variable {
    /// Globally unique flat name.
    pub name: VarName,
    /// Structured component reference that produced this flattened variable.
    ///
    /// The rendered flat name is display/protocol data. Compiler logic that
    /// needs path structure or resolved identity should use this reference
    /// instead of reparsing `name`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub component_ref: Option<ComponentReference>,
    /// Source span for the component declaration that produced this flat variable.
    pub source_span: Span,
    /// Reference to the type in the TypeTable.
    pub type_id: TypeId,
    /// Variability (constant, parameter, discrete, continuous).
    pub variability: Variability,
    /// Causality (input, output, or empty).
    pub causality: Causality,
    /// Flow prefix.
    pub flow: bool,
    /// Stream prefix.
    pub stream: bool,
    /// Resolved array dimensions (preserved per SPEC_0019).
    pub dims: Vec<i64>,
    /// True if this variable is used in connection equations.
    pub connected: bool,

    // Resolved attributes
    /// Start value attribute.
    pub start: Option<Expression>,
    /// Fixed attribute.
    pub fixed: Option<bool>,
    /// Minimum value attribute.
    pub min: Option<Expression>,
    /// Maximum value attribute.
    pub max: Option<Expression>,
    /// Nominal value attribute.
    pub nominal: Option<Expression>,
    /// Quantity string attribute.
    pub quantity: Option<String>,
    /// Unit string attribute.
    pub unit: Option<String>,
    /// Display-unit string attribute.
    pub display_unit: Option<String>,
    /// Optional declaration description string (`"..."` after declaration).
    pub description: Option<String>,
    /// State selection hint.
    pub state_select: StateSelect,

    /// Binding equation value.
    pub binding: Option<Expression>,
    /// True if binding came from a modification rather than declaration.
    pub binding_from_modification: bool,
    /// True if this parameter has annotation(Evaluate=true) or is declared final.
    /// Structural parameters can be evaluated at compile time for if-equation
    /// branch selection (MLS §18.3).
    pub evaluate: bool,

    /// True if this variable's base type is Integer or Boolean (MLS §4.5).
    /// Such variables are discrete by default even without explicit `discrete` prefix.
    /// This is used during variable classification to correctly identify discrete
    /// variables for the DAE balance calculation.
    #[serde(default)]
    pub is_discrete_type: bool,

    /// True if this variable is a primitive type (Real, Integer, Boolean, String).
    /// Record-typed variables (like Complex with .re and .im fields) are not primitive.
    /// Non-primitive variables should not be counted as unknowns since their fields
    /// are counted separately. MLS §4.8: Balance checking uses expanded scalar counts.
    #[serde(default)]
    pub is_primitive: bool,

    /// True if this variable comes from an expandable connector (MLS §9.1.3).
    /// Unconnected expandable connector members without bindings are unused and
    /// shouldn't count as unknowns in the DAE balance calculation.
    #[serde(default)]
    pub from_expandable_connector: bool,

    /// True if this variable belongs to an overconstrained connector (MLS §9.4).
    /// A connector is overconstrained if its type defines an `equalityConstraint` function.
    #[serde(default)]
    pub is_overconstrained: bool,

    /// True if this component is declared in a protected section (MLS §4.7).
    /// Protected components are not part of the public interface and their flow
    /// variables should not count as interface flows for balance checking.
    #[serde(default)]
    pub is_protected: bool,

    /// The path of the enclosing overconstrained record (MLS §9.4).
    /// E.g., "frame_a.R" for variables frame_a.R.T and frame_a.R.w.
    /// Used to group OC variables into VCG nodes for balance correction.
    #[serde(default)]
    pub oc_record_path: Option<String>,

    /// The output size of the enclosing record's equalityConstraint function.
    /// E.g., 3 for Orientation (returns `Real[3]`).
    #[serde(default)]
    pub oc_eq_constraint_size: Option<usize>,
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

impl Variable {
    pub fn shape_size(&self) -> Result<usize, VariableShapeContractError> {
        shape_size(&self.name, self.source_span, &self.dims)
    }

    pub fn validate_shape_contract(&self) -> Result<(), VariableShapeContractError> {
        self.shape_size().map(|_| ())
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

impl Variable {
    pub fn empty_with_span(source_span: Span) -> Self {
        Self {
            name: VarName::default(),
            component_ref: None,
            source_span,
            type_id: TypeId::default(),
            variability: Variability::Empty,
            causality: Causality::Empty,
            flow: false,
            stream: false,
            dims: Vec::new(),
            connected: false,
            start: None,
            fixed: None,
            min: None,
            max: None,
            nominal: None,
            quantity: None,
            unit: None,
            display_unit: None,
            description: None,
            state_select: StateSelect::default(),
            binding: None,
            binding_from_modification: false,
            evaluate: false,
            is_discrete_type: false,
            is_primitive: false,
            from_expandable_connector: false,
            is_overconstrained: false,
            is_protected: false,
            oc_record_path: None,
            oc_eq_constraint_size: None,
        }
    }
}

#[cfg(test)]
mod variable_shape_contract_tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("flat_variable_test.mo"),
            1,
            2,
        )
    }

    #[test]
    fn flat_variable_shape_size_preserves_zero_sized_arrays() {
        let variable = Variable {
            name: VarName::new("x"),
            dims: vec![0, 3],
            ..Variable::empty_with_span(test_span())
        };

        assert_eq!(variable.shape_size(), Ok(0));
    }

    #[test]
    fn flat_variable_shape_contract_rejects_negative_dims() {
        let variable = Variable {
            name: VarName::new("x"),
            dims: vec![2, -1],
            ..Variable::empty_with_span(test_span())
        };

        assert_eq!(
            variable.validate_shape_contract(),
            Err(VariableShapeContractError::NegativeDimension {
                variable: VarName::new("x"),
                dimension: -1,
                span: test_span(),
            })
        );
    }

    #[test]
    fn flat_model_shape_contract_rejects_key_name_mismatch() {
        let mut model = Model::new();
        model.add_variable(
            VarName::new("key"),
            Variable {
                name: VarName::new("stored"),
                ..Variable::empty_with_span(test_span())
            },
        );

        assert_eq!(
            model.validate_shape_contract(),
            Err(ModelShapeContractError::VariableKeyNameMismatch {
                key: VarName::new("key"),
                name: VarName::new("stored"),
                span: test_span(),
            })
        );
    }

    #[test]
    fn add_function_assigns_stable_distinct_exposure_identities() {
        let mut model = Model::new();
        let mut a = Function::new("Pkg.A.f", test_span());
        a.instance_id = Some(FunctionInstanceId::new(99));
        let mut b = Function::new("Pkg.B.f", test_span());
        b.instance_id = Some(FunctionInstanceId::new(99));
        model.add_function(a);
        model.add_function(b);

        let a_id = model.functions[&VarName::new("Pkg.A.f")]
            .instance_id
            .expect("first exposure identity");
        let b_id = model.functions[&VarName::new("Pkg.B.f")]
            .instance_id
            .expect("second exposure identity");
        assert_ne!(a_id, b_id);

        model.add_function(Function::new("Pkg.A.f", test_span()));
        assert_eq!(
            model.functions[&VarName::new("Pkg.A.f")].instance_id,
            Some(a_id),
            "replacing an exposed definition must preserve its identity"
        );
    }

    #[test]
    fn flat_model_shape_contract_rejects_missing_function_instance_identity() {
        let mut model = Model::new();
        let function = Function::new("Pkg.f", test_span());
        model.functions.insert(function.name.clone(), function);

        assert_eq!(
            model.validate_shape_contract(),
            Err(ModelShapeContractError::MissingFunctionInstanceId {
                function: VarName::new("Pkg.f"),
                span: test_span(),
            })
        );
    }

    #[test]
    fn flat_model_shape_contract_rejects_duplicate_function_instance_identity() {
        let mut model = Model::new();
        for name in ["Pkg.A.f", "Pkg.B.f"] {
            let mut function = Function::new(name, test_span());
            function.instance_id = Some(FunctionInstanceId::new(7));
            model.functions.insert(function.name.clone(), function);
        }

        assert_eq!(
            model.validate_shape_contract(),
            Err(ModelShapeContractError::DuplicateFunctionInstanceId {
                instance_id: FunctionInstanceId::new(7),
                first: VarName::new("Pkg.A.f"),
                second: VarName::new("Pkg.B.f"),
                span: test_span(),
            })
        );
    }

    #[test]
    fn flat_model_shape_contract_rejects_function_param_negative_dims() {
        let mut model = Model::new();
        let mut function = Function::new("Pkg.f", Span::DUMMY);
        function.add_output(
            rumoca_core::FunctionParam::new("y", "Real", test_span()).with_dims(vec![-1]),
        );
        model.add_function(function);

        assert!(matches!(
            model.validate_shape_contract(),
            Err(ModelShapeContractError::Function(
                rumoca_core::FunctionShapeContractError::Param { .. }
            ))
        ));
    }
}

/// Typed origin for equations, replacing free-form string classification.
///
/// Each variant represents a specific equation source, enabling
/// pattern matching instead of `starts_with()` string checks.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EquationOrigin {
    /// Equation from a component instance (e.g., `equation from resistor[1]`).
    ComponentEquation { component: String },
    /// Connection equality equation: `lhs = rhs` (MLS §9.2).
    Connection { lhs: String, rhs: String },
    /// Flow sum equation: `sum of signed flows = 0` (MLS §9.2).
    FlowSum { description: String },
    /// Unconnected flow variable set to zero (MLS §9.2).
    UnconnectedFlow { variable: String },
    /// Algorithm section from a component.
    Algorithm { component: String },
    /// Reinit equation (MLS §8.3.5).
    Reinit { state: String },
    /// When-clause assignment.
    WhenAssignment { target: String },
    /// Binding equation from variable declaration (MLS §4.4.1).
    Binding { variable: String },
}

impl std::fmt::Display for EquationOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EquationOrigin::ComponentEquation { component } => {
                // Top-level model equations have no instance prefix; rendering
                // `equation from ` then reads as truncated, so name the source
                // plainly instead.
                if component.is_empty() {
                    write!(f, "top-level model equation")
                } else {
                    write!(f, "equation from {}", component)
                }
            }
            EquationOrigin::Connection { lhs, rhs } => {
                write!(f, "connection equation: {} = {}", lhs, rhs)
            }
            EquationOrigin::FlowSum { description } => {
                write!(f, "flow sum equation: {}", description)
            }
            EquationOrigin::UnconnectedFlow { variable } => {
                write!(f, "unconnected flow: {} = 0", variable)
            }
            EquationOrigin::Algorithm { component } => {
                write!(f, "algorithm from {}", component)
            }
            EquationOrigin::Reinit { state } => {
                write!(f, "reinit equation for {}", state)
            }
            EquationOrigin::WhenAssignment { target } => {
                write!(f, "when assignment for {}", target)
            }
            EquationOrigin::Binding { variable } => {
                write!(f, "binding equation for {}", variable)
            }
        }
    }
}

impl EquationOrigin {
    /// Check if this origin represents a connection equation.
    pub fn is_connection(&self) -> bool {
        matches!(self, EquationOrigin::Connection { .. })
    }

    /// Check if this origin represents a component equation.
    pub fn is_component_equation(&self) -> bool {
        matches!(self, EquationOrigin::ComponentEquation { .. })
    }

    /// Get the component name if this is a component equation origin.
    pub fn component_name(&self) -> Option<&str> {
        match self {
            EquationOrigin::ComponentEquation { component } => Some(component),
            _ => None,
        }
    }

    /// Get the variable name if this is a binding equation origin.
    pub fn binding_variable(&self) -> Option<&str> {
        match self {
            EquationOrigin::Binding { variable } => Some(variable),
            _ => None,
        }
    }
}

fn default_equation_origin() -> EquationOrigin {
    EquationOrigin::ComponentEquation {
        component: String::new(),
    }
}

/// Equation in residual form: 0 = residual
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Equation {
    /// The residual expression (equation is: 0 = residual).
    pub residual: Expression,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
    /// Typed origin indicating where this equation came from.
    pub origin: EquationOrigin,
    /// Number of scalar equations this represents (MLS §8.4).
    /// For array equations like `x[n] = expr`, this is n.
    /// For scalar equations, this is 1.
    /// Used for balance checking per MLS §4.7.
    #[serde(default = "default_scalar_count")]
    pub scalar_count: usize,
}

/// Structured source equation family over an index domain.
///
/// Current Flat lowering still materializes deterministic scalar views in
/// `Model::equations` or `Model::initial_equations`; this family is the
/// authoritative source grouping for later structured lowering.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructuredEquationFamily {
    /// Compact index domain in source binder declaration order.
    pub domain: StructuredIndexDomain,
    /// First equation index in the corresponding flat equation vector.
    #[serde(default)]
    pub first_equation_index: usize,
    /// Scalar-view equation count for each domain point in deterministic order.
    pub equation_counts: Vec<usize>,
    /// Source span for diagnostics.
    pub span: Span,
    /// Typed origin for traceability.
    pub origin: EquationOrigin,
    /// Family-native classification: present when this family is a regular
    /// elementwise stencil whose array accesses are all affine in the binders,
    /// carrying the per-access stride table the Solve-IR lowering needs to build
    /// a compact `AffineStencil` without materializing one row per index tuple.
    /// `None` means the family is materialized the historical (scalar) way.
    #[serde(default)]
    pub regular: Option<RegularForFamily>,
    /// The family's canonical comprehension body, captured by flatten before the
    /// loop is expanded. When present, downstream phases read the template directly
    /// (printing it as a `for`-comprehension, deriving strides, building a compact
    /// kernel) instead of reconstructing it from materialized corner cells. `None`
    /// for families flattened before this representation existed. See
    /// [`rumoca_core::ComprehensionTemplate`].
    #[serde(default)]
    pub template: Option<ComprehensionTemplate>,
    /// Whether the interior cells' scalar bodies are materialized in the equation
    /// vector. `true` (the default) is the historical behavior: every cell carries
    /// a full body. When a regular family is lowered with
    /// `FlattenOptions::materialize_structured_families = false`, only the corner
    /// cells (base + one neighbor per binder) carry real bodies and this is `false`,
    /// signaling downstream phases to reconstruct interior incidence/strides from
    /// the corners instead of reading the (placeholder) interior bodies.
    #[serde(default = "default_true")]
    pub interiors_materialized: bool,
}

/// Default scalar count for equations (1 for serde deserialization).
fn default_scalar_count() -> usize {
    1
}

/// Serde default for `interiors_materialized` (historical behavior: all cells
/// carry full bodies).
fn default_true() -> bool {
    true
}

impl Equation {
    /// Create a new flat equation with span information.
    pub fn new(residual: Expression, span: Span, origin: EquationOrigin) -> Self {
        Self {
            residual,
            span,
            origin,
            scalar_count: 1,
        }
    }

    /// Create a new flat equation with explicit scalar count for array equations.
    pub fn new_array(
        residual: Expression,
        span: Span,
        origin: EquationOrigin,
        scalar_count: usize,
    ) -> Self {
        Self {
            residual,
            span,
            origin,
            scalar_count,
        }
    }

    /// Create a flat equation with a dummy span (for testing only).
    #[cfg(test)]
    pub fn new_without_span(residual: Expression, origin: EquationOrigin) -> Self {
        Self {
            residual,
            span: Span::DUMMY,
            origin,
            scalar_count: 1,
        }
    }
}

/// Runtime assertion equation preserved from `equation` / `initial equation` sections.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssertEquation {
    /// Assertion condition expression.
    pub condition: Expression,
    /// Assertion message expression.
    pub message: Expression,
    /// Optional assertion level expression.
    pub level: Option<Expression>,
    /// Source span for diagnostics and traceability.
    pub span: Span,
    /// Typed origin for scoped parameter/constant substitution.
    #[serde(default = "default_equation_origin")]
    pub origin: EquationOrigin,
}

impl AssertEquation {
    /// Create a new flat assertion equation.
    pub fn new(
        condition: Expression,
        message: Expression,
        level: Option<Expression>,
        span: Span,
        origin: EquationOrigin,
    ) -> Self {
        Self {
            condition,
            message,
            level,
            span,
            origin,
        }
    }
}

/// Algorithm section with preserved structure (SPEC_0020).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Algorithm {
    /// The statements in this algorithm section.
    pub statements: Vec<Statement>,
    /// Output variables (left-hand side variables).
    pub outputs: Vec<Reference>,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
    /// Human-readable origin description (for debugging).
    pub origin: String,
}

impl Algorithm {
    /// Create a new flat algorithm with span information.
    pub fn new(statements: Vec<Statement>, span: Span, origin: impl Into<String>) -> Self {
        Self {
            statements,
            outputs: Vec::new(),
            span,
            origin: origin.into(),
        }
    }
}
