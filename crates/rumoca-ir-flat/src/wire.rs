//! Checked replay of the current Flat wire shape.
//!
//! The input records are deliberately distinct from invariant-bearing Flat
//! values. Deserialization can therefore produce only private wire data; a
//! public [`Model`] is returned only after the root-owned type, provenance,
//! call-identity, occurrence, and equation-family checks below succeed.
//!
//! Private semantic and provenance checks live in [`semantics`] so the replay
//! constructor and its semantic checker remain independently reviewable.

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{
    Causality, ClassType, ComponentReference, ComprehensionTemplate, DefId, EffectiveType,
    Expression, ExternalFunction, FallibleExpressionVisitor, Function, FunctionCallKind,
    FunctionInstanceId, FunctionParam, InstanceId, Literal, Reference, RegularForFamily, Span,
    StateSelect, Statement, StructuredIndexBinderId, Subscript, TypeId, VarName, Variability,
    extract_algorithm_outputs,
};
use serde::{Deserialize, Serialize};

mod semantics;
use semantics::*;

use crate::visitor::FallibleStatementVisitor;
use crate::{
    Algorithm, AssertEquation, ConnectedDomain, Equation, EquationOrigin, InstanceKind,
    InstanceRelation, Model, ModelShapeContractError, PredefinedTypeIds, RecordField,
    RecordInstance, RecordType, StructuredEquationFamily, StructuredEquationOwnerError,
    TypeIdentityMap, VarNameIndexMap, Variable, WhenChain, WhenEquation,
};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ModelWire {
    predefined_string_declaration: Option<DefId>,
    predefined_types: PredefinedTypeIds,
    effective_types: IndexMap<TypeId, EffectiveType, rustc_hash::FxBuildHasher>,
    type_roots: crate::TypeRootMap,
    enumeration_types: IndexSet<TypeId>,
    type_ids_by_def_id: TypeIdentityMap,
    enumeration_type_roots: IndexSet<TypeId>,
    variables: IndexMap<VarName, VariableWire, rustc_hash::FxBuildHasher>,
    record_instances: IndexMap<VarName, RecordInstanceWire, rustc_hash::FxBuildHasher>,
    record_types: IndexMap<DefId, RecordTypeWire, rustc_hash::FxBuildHasher>,
    variable_type_names: VarNameIndexMap<String>,
    variable_final_flags: VarNameIndexMap<bool>,
    equations: Vec<EquationWire>,
    structured_equations: Vec<StructuredEquationFamilyWire>,
    assert_equations: Vec<AssertEquationWire>,
    initial_equations: Vec<EquationWire>,
    initial_structured_equations: Vec<StructuredEquationFamilyWire>,
    initial_assert_equations: Vec<AssertEquationWire>,
    algorithms: Vec<AlgorithmWire>,
    initial_algorithms: Vec<AlgorithmWire>,
    when_chains: Vec<WhenChain>,
    functions: VarNameIndexMap<Function>,
    is_partial: bool,
    class_type: ClassType,
    model_description: Option<String>,
    definite_roots: IndexSet<String>,
    branches: Vec<(String, String)>,
    optional_edges: Vec<(String, String)>,
    potential_roots: Vec<(String, i64)>,
    top_level_connectors: IndexSet<String>,
    top_level_input_components: IndexSet<String>,
    oc_break_edge_scalar_count: usize,
    enum_literal_ordinals: IndexMap<String, i64>,
    instance_relations: IndexMap<InstanceId, InstanceRelationWire, rustc_hash::FxBuildHasher>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct InstanceRelationWire {
    owner: Option<InstanceId>,
    declaration: Option<DefId>,
    indices: Box<[i64]>,
    kind: InstanceKind,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecordInstanceWire {
    instance_id: InstanceId,
    component_ref: ComponentReference,
    source_span: Span,
    effective_type_id: TypeId,
    type_name: String,
    type_def_id: DefId,
    dims: Vec<i64>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecordTypeWire {
    name: String,
    fields: Vec<RecordFieldWire>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecordFieldWire {
    name: String,
    def_id: DefId,
    type_def_id: DefId,
    effective_type: rumoca_core::EffectiveType,
    dims: Vec<i64>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct VariableWire {
    instance_id: InstanceId,
    name: VarName,
    component_ref: Option<ComponentReference>,
    source_span: Span,
    type_id: TypeId,
    variability: Variability,
    causality: Causality,
    flow: bool,
    stream: bool,
    dims: Vec<i64>,
    connected: ConnectedDomain,
    start: Option<Expression>,
    fixed: Option<bool>,
    min: Option<Expression>,
    max: Option<Expression>,
    nominal: Option<Expression>,
    quantity: Option<String>,
    unit: Option<String>,
    display_unit: Option<String>,
    description: Option<String>,
    state_select: StateSelect,
    binding: Option<Expression>,
    binding_from_modification: bool,
    evaluate: bool,
    is_discrete_type: bool,
    is_primitive: bool,
    from_expandable_connector: bool,
    is_protected: bool,
}

#[derive(Serialize, Deserialize)]
enum WireEquationOrigin {
    ComponentEquation {
        component: String,
    },
    Connection {
        lhs: String,
        rhs: String,
    },
    OutsideStream {
        variable: String,
    },
    EqualityConstraint {
        lhs_record: InstanceId,
        rhs_record: InstanceId,
        function: FunctionInstanceId,
    },
    FlowSum {
        description: String,
    },
    UnconnectedFlow {
        variable: String,
    },
    Algorithm {
        component: String,
    },
    Reinit {
        state: String,
    },
    WhenAssignment {
        target: String,
    },
    Binding {
        variable: String,
    },
}

pub(crate) fn serialize_equation_origin<S>(
    origin: &EquationOrigin,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    WireEquationOrigin::from(origin).serialize(serializer)
}

/// Operation-shaped equation record shared by Model and checked clock wires.
///
/// In particular, `scalar_count` is required. The former fieldwise
/// `Equation` decoder defaulted an omitted count to one, which could change an
/// array equation into a scalar equation without evidence.
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct EquationWire {
    residual: Expression,
    span: Span,
    origin: WireEquationOrigin,
    scalar_count: usize,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StructuredEquationFamilyWire {
    domain: rumoca_core::StructuredIndexDomain,
    first_equation_index: usize,
    equations_per_point: usize,
    span: Span,
    origin: WireEquationOrigin,
    regular: Option<RegularForFamily>,
    template: Option<ComprehensionTemplate>,
    interiors_materialized: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct AssertEquationWire {
    condition: Expression,
    message: Expression,
    level: Option<Expression>,
    span: Span,
    origin: WireEquationOrigin,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct AlgorithmWire {
    statements: Vec<Statement>,
    outputs: Vec<Reference>,
    span: Span,
    origin: String,
}

impl<'de> Deserialize<'de> for Model {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        ModelWireConstruction::replay(ModelWire::deserialize(deserializer)?)
            .and_then(ModelWireConstruction::finish)
            .map_err(serde::de::Error::custom)
    }
}

/// Private ownership guard for one Flat root replay.
///
/// The candidate root cannot escape until `finish` has closed the exact
/// occurrence and callable catalogs and replayed every semantic expression
/// against them.
struct ModelWireConstruction {
    candidate: Model,
}

impl ModelWireConstruction {
    fn replay(wire: ModelWire) -> Result<Self, FlatWireError> {
        let equation_wires = wire.equations;
        let structured_equation_wires = wire.structured_equations;
        let assert_equation_wires = wire.assert_equations;
        let initial_equation_wires = wire.initial_equations;
        let initial_structured_equation_wires = wire.initial_structured_equations;
        let initial_assert_equation_wires = wire.initial_assert_equations;
        let predefined_string_declaration = wire.predefined_string_declaration;
        let predefined_types = wire.predefined_types;
        let effective_types = wire.effective_types;
        let type_roots = wire.type_roots;
        let enumeration_types = wire.enumeration_types;
        let type_ids_by_def_id = wire.type_ids_by_def_id;
        let enumeration_type_roots = wire.enumeration_type_roots;
        let variables = wire
            .variables
            .into_iter()
            .map(|(name, variable)| (name, variable.into()))
            .collect();
        let record_instances = wire
            .record_instances
            .into_iter()
            .map(|(name, record)| (name, record.into()))
            .collect();
        let record_types = wire
            .record_types
            .into_iter()
            .map(|(id, record)| (id, record.into()))
            .collect();
        let functions = wire.functions;
        let equations = equation_wires
            .into_iter()
            .map(|wire| wire.reconstruct("regular"))
            .collect::<Result<_, _>>()?;
        let structured_equations = structured_equation_wires
            .into_iter()
            .map(StructuredEquationFamilyWire::replay)
            .collect::<Result<_, _>>()?;
        let assert_equations = assert_equation_wires
            .into_iter()
            .map(AssertEquationWire::replay)
            .collect::<Result<_, _>>()?;
        let initial_equations = initial_equation_wires
            .into_iter()
            .map(|wire| wire.reconstruct("initial"))
            .collect::<Result<_, _>>()?;
        let initial_structured_equations = initial_structured_equation_wires
            .into_iter()
            .map(StructuredEquationFamilyWire::replay)
            .collect::<Result<_, _>>()?;
        let initial_assert_equations = initial_assert_equation_wires
            .into_iter()
            .map(AssertEquationWire::replay)
            .collect::<Result<_, _>>()?;
        Ok(Self {
            candidate: Model {
                predefined_string_declaration,
                predefined_types,
                effective_types,
                type_roots,
                enumeration_types,
                type_ids_by_def_id,
                enumeration_type_roots,
                variables,
                record_instances,
                record_types,
                variable_type_names: wire.variable_type_names,
                variable_final_flags: wire.variable_final_flags,
                equations,
                structured_equations,
                assert_equations,
                initial_equations,
                initial_structured_equations,
                initial_assert_equations,
                algorithms: wire.algorithms.into_iter().map(Into::into).collect(),
                initial_algorithms: wire
                    .initial_algorithms
                    .into_iter()
                    .map(Into::into)
                    .collect(),
                when_chains: wire.when_chains,
                functions,
                is_partial: wire.is_partial,
                class_type: wire.class_type,
                model_description: wire.model_description,
                definite_roots: wire.definite_roots,
                branches: wire.branches,
                optional_edges: wire.optional_edges,
                potential_roots: wire.potential_roots,
                top_level_connectors: wire.top_level_connectors,
                top_level_input_components: wire.top_level_input_components,
                oc_break_edge_scalar_count: wire.oc_break_edge_scalar_count,
                enum_literal_ordinals: wire.enum_literal_ordinals,
                instance_relations: wire
                    .instance_relations
                    .into_iter()
                    .map(|(id, relation)| (id, relation.into()))
                    .collect(),
            },
        })
    }

    fn finish(self) -> Result<Model, FlatWireError> {
        {
            let model = &self.candidate;
            model.validate().map_err(FlatWireError::Shape)?;
            model.validate_wire_type_catalog()?;
            let catalogs = model.close_wire_catalogs()?;
            model.validate_wire_record_types(&catalogs.targets)?;
            model
                .structured_equation_owners()
                .map_err(FlatWireError::from)?;
            model.validate_wire_provenance_and_calls(&catalogs)?;
        }
        Ok(self.candidate)
    }
}

impl From<InstanceRelationWire> for InstanceRelation {
    fn from(wire: InstanceRelationWire) -> Self {
        Self {
            owner: wire.owner,
            declaration: wire.declaration,
            indices: wire.indices,
            kind: wire.kind,
        }
    }
}

impl From<RecordInstanceWire> for RecordInstance {
    fn from(wire: RecordInstanceWire) -> Self {
        Self {
            instance_id: wire.instance_id,
            component_ref: wire.component_ref,
            source_span: wire.source_span,
            effective_type_id: wire.effective_type_id,
            type_name: wire.type_name,
            type_def_id: wire.type_def_id,
            dims: wire.dims,
        }
    }
}

impl From<RecordTypeWire> for RecordType {
    fn from(wire: RecordTypeWire) -> Self {
        Self {
            name: wire.name,
            fields: wire.fields.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<RecordFieldWire> for RecordField {
    fn from(wire: RecordFieldWire) -> Self {
        Self {
            name: wire.name,
            def_id: wire.def_id,
            type_def_id: wire.type_def_id,
            effective_type: wire.effective_type,
            dims: wire.dims,
        }
    }
}

impl From<VariableWire> for Variable {
    fn from(wire: VariableWire) -> Self {
        Self {
            instance_id: wire.instance_id,
            name: wire.name,
            component_ref: wire.component_ref,
            source_span: wire.source_span,
            type_id: wire.type_id,
            variability: wire.variability,
            causality: wire.causality,
            flow: wire.flow,
            stream: wire.stream,
            dims: wire.dims,
            connected: wire.connected,
            start: wire.start,
            fixed: wire.fixed,
            min: wire.min,
            max: wire.max,
            nominal: wire.nominal,
            quantity: wire.quantity,
            unit: wire.unit,
            display_unit: wire.display_unit,
            description: wire.description,
            state_select: wire.state_select,
            binding: wire.binding,
            binding_from_modification: wire.binding_from_modification,
            evaluate: wire.evaluate,
            is_discrete_type: wire.is_discrete_type,
            is_primitive: wire.is_primitive,
            from_expandable_connector: wire.from_expandable_connector,
            is_protected: wire.is_protected,
        }
    }
}

impl From<&EquationOrigin> for WireEquationOrigin {
    fn from(origin: &EquationOrigin) -> Self {
        match origin {
            EquationOrigin::ComponentEquation { component } => Self::ComponentEquation {
                component: component.clone(),
            },
            EquationOrigin::Connection { lhs, rhs } => Self::Connection {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            },
            EquationOrigin::OutsideStream { variable } => Self::OutsideStream {
                variable: variable.clone(),
            },
            EquationOrigin::EqualityConstraint {
                lhs_record,
                rhs_record,
                function,
            } => Self::EqualityConstraint {
                lhs_record: *lhs_record,
                rhs_record: *rhs_record,
                function: *function,
            },
            EquationOrigin::FlowSum { description } => Self::FlowSum {
                description: description.clone(),
            },
            EquationOrigin::UnconnectedFlow { variable } => Self::UnconnectedFlow {
                variable: variable.clone(),
            },
            EquationOrigin::Algorithm { component } => Self::Algorithm {
                component: component.clone(),
            },
            EquationOrigin::Reinit { state } => Self::Reinit {
                state: state.clone(),
            },
            EquationOrigin::WhenAssignment { target } => Self::WhenAssignment {
                target: target.clone(),
            },
            EquationOrigin::Binding { variable } => Self::Binding {
                variable: variable.clone(),
            },
        }
    }
}

impl WireEquationOrigin {
    fn replay(self) -> Result<EquationOrigin, FlatWireError> {
        match self {
            Self::ComponentEquation { component } => {
                Ok(EquationOrigin::ComponentEquation { component })
            }
            Self::Connection { .. }
            | Self::OutsideStream { .. }
            | Self::EqualityConstraint { .. }
            | Self::FlowSum { .. }
            | Self::UnconnectedFlow { .. } => Err(FlatWireError::InvalidConnectionEvidence {
                reason: "connection-derived rows cannot replay without canonical Instance source groups and an atomic connection transaction",
            }),
            Self::Algorithm { component } => Ok(EquationOrigin::Algorithm { component }),
            Self::Reinit { state } => Ok(EquationOrigin::Reinit { state }),
            Self::WhenAssignment { target } => Ok(EquationOrigin::WhenAssignment { target }),
            Self::Binding { variable } => Ok(EquationOrigin::Binding { variable }),
        }
    }
}

impl From<&Equation> for EquationWire {
    fn from(equation: &Equation) -> Self {
        Self {
            residual: equation.residual.clone(),
            span: equation.span,
            origin: (&equation.origin).into(),
            scalar_count: equation.scalar_count,
        }
    }
}

impl EquationWire {
    pub(crate) fn reconstruct(self, partition: &'static str) -> Result<Equation, FlatWireError> {
        let equation = Equation {
            residual: self.residual,
            span: self.span,
            origin: self.origin.replay()?,
            scalar_count: self.scalar_count,
        };
        if equation.scalar_count == 0 {
            return Err(FlatWireError::InvalidEquationShape {
                partition,
                index: 0,
                reason: "scalar_count must be positive",
            });
        }
        Ok(equation)
    }

    pub(crate) fn reconstruct_standalone(
        self,
        partition: &'static str,
    ) -> Result<Equation, FlatWireError> {
        let equation = self.reconstruct(partition)?;
        validate_standalone_equation_expression(&equation.residual)?;
        Ok(equation)
    }
}

impl StructuredEquationFamilyWire {
    fn replay(self) -> Result<StructuredEquationFamily, FlatWireError> {
        Ok(StructuredEquationFamily {
            domain: self.domain,
            first_equation_index: self.first_equation_index,
            equations_per_point: self.equations_per_point,
            span: self.span,
            origin: self.origin.replay()?,
            regular: self.regular,
            template: self.template,
            interiors_materialized: self.interiors_materialized,
        })
    }
}

impl AssertEquationWire {
    fn replay(self) -> Result<AssertEquation, FlatWireError> {
        Ok(AssertEquation {
            condition: self.condition,
            message: self.message,
            level: self.level,
            span: self.span,
            origin: self.origin.replay()?,
        })
    }
}

impl From<AlgorithmWire> for Algorithm {
    fn from(wire: AlgorithmWire) -> Self {
        Self {
            statements: wire.statements,
            outputs: wire.outputs,
            span: wire.span,
            origin: wire.origin,
        }
    }
}

/// Typed reason the checked current Flat wire refused a payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatWireError {
    Shape(ModelShapeContractError),
    MissingPredefinedStringDeclaration,
    IncompletePredefinedTypes,
    InvalidTypeCatalog {
        reason: &'static str,
    },
    MissingProvenance {
        context: &'static str,
    },
    InvalidRecordFieldShape {
        record: DefId,
        field: String,
    },
    InvalidRecordCatalog {
        record: DefId,
        reason: &'static str,
    },
    InvalidInstanceRelation {
        instance_id: InstanceId,
        reason: &'static str,
    },
    InvalidOccurrenceTarget {
        name: VarName,
        instance_id: InstanceId,
        reason: &'static str,
    },
    InvalidReferenceTarget {
        name: VarName,
        instance_id: InstanceId,
        reason: &'static str,
    },
    InvalidWriteTarget {
        name: VarName,
        reason: &'static str,
    },
    InvalidAlgorithm {
        reason: &'static str,
    },
    InvalidConnectionEvidence {
        reason: &'static str,
    },
    InvalidEquationOrigin {
        reason: &'static str,
    },
    ContradictoryConnectedState {
        variable: VarName,
        claimed: bool,
        equation_evidence: bool,
    },
    InvalidFunctionCall {
        function: VarName,
        reason: &'static str,
    },
    InvalidSubscript {
        reason: &'static str,
    },
    RecoveryNode {
        context: &'static str,
    },
    InvalidEquationShape {
        partition: &'static str,
        index: usize,
        reason: &'static str,
    },
    InvalidStructuredEquationShape {
        partition: &'static str,
        index: usize,
        reason: &'static str,
    },
}

impl std::fmt::Display for FlatWireError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shape(_)
            | Self::MissingPredefinedStringDeclaration
            | Self::IncompletePredefinedTypes
            | Self::InvalidTypeCatalog { .. }
            | Self::MissingProvenance { .. }
            | Self::InvalidRecordFieldShape { .. }
            | Self::InvalidRecordCatalog { .. } => self.fmt_catalog(formatter),
            Self::InvalidInstanceRelation { .. }
            | Self::InvalidOccurrenceTarget { .. }
            | Self::InvalidReferenceTarget { .. }
            | Self::InvalidWriteTarget { .. } => self.fmt_occurrence(formatter),
            Self::InvalidAlgorithm { .. }
            | Self::InvalidConnectionEvidence { .. }
            | Self::InvalidEquationOrigin { .. }
            | Self::ContradictoryConnectedState { .. }
            | Self::InvalidFunctionCall { .. }
            | Self::InvalidSubscript { .. }
            | Self::RecoveryNode { .. }
            | Self::InvalidEquationShape { .. }
            | Self::InvalidStructuredEquationShape { .. } => self.fmt_semantic(formatter),
        }
    }
}

impl FlatWireError {
    fn fmt_catalog(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shape(error) => write!(formatter, "Flat shape contract failed: {error:?}"),
            Self::MissingPredefinedStringDeclaration => {
                formatter.write_str("Flat wire is missing the predefined String declaration")
            }
            Self::IncompletePredefinedTypes => {
                formatter.write_str("Flat wire has incomplete or contradictory predefined types")
            }
            Self::InvalidTypeCatalog { reason } => {
                write!(formatter, "Flat wire type catalog is invalid: {reason}")
            }
            Self::MissingProvenance { context } => {
                write!(
                    formatter,
                    "Flat wire is missing source provenance for {context}"
                )
            }
            Self::InvalidRecordFieldShape { record, field } => write!(
                formatter,
                "Flat wire record {record} field `{field}` has an invalid shape"
            ),
            Self::InvalidRecordCatalog { record, reason } => {
                write!(
                    formatter,
                    "Flat wire record catalog {record} is invalid: {reason}"
                )
            }
            _ => Err(std::fmt::Error),
        }
    }

    fn fmt_occurrence(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidInstanceRelation {
                instance_id,
                reason,
            } => write!(
                formatter,
                "Flat wire occurrence {} is invalid: {reason}",
                instance_id.index()
            ),
            Self::InvalidOccurrenceTarget {
                name,
                instance_id,
                reason,
            } => write!(
                formatter,
                "Flat wire target `{name}` for occurrence {} is invalid: {reason}",
                instance_id.index()
            ),
            Self::InvalidReferenceTarget {
                name,
                instance_id,
                reason,
            } => write!(
                formatter,
                "Flat wire reference `{name}` to occurrence {} is invalid: {reason}",
                instance_id.index()
            ),
            Self::InvalidWriteTarget { name, reason } => {
                write!(
                    formatter,
                    "Flat wire write target `{name}` is invalid: {reason}"
                )
            }
            _ => Err(std::fmt::Error),
        }
    }

    fn fmt_semantic(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidAlgorithm { reason } => {
                write!(formatter, "Flat wire algorithm is invalid: {reason}")
            }
            Self::InvalidConnectionEvidence { reason } => {
                write!(
                    formatter,
                    "Flat wire connection evidence is invalid: {reason}"
                )
            }
            Self::InvalidEquationOrigin { reason } => {
                write!(formatter, "Flat wire equation origin is invalid: {reason}")
            }
            Self::ContradictoryConnectedState {
                variable,
                claimed,
                equation_evidence,
            } => write!(
                formatter,
                "Flat wire variable `{variable}` claims connected={claimed}, but its connection-equation evidence is {equation_evidence}"
            ),
            Self::InvalidFunctionCall { function, reason } => {
                write!(
                    formatter,
                    "Flat wire call to `{function}` is invalid: {reason}"
                )
            }
            Self::InvalidSubscript { reason } => {
                write!(formatter, "Flat wire subscript is invalid: {reason}")
            }
            Self::RecoveryNode { context } => {
                write!(formatter, "Flat wire contains a recovery-only {context}")
            }
            Self::InvalidEquationShape {
                partition,
                index,
                reason,
            } => write!(
                formatter,
                "Flat wire {partition} equation {index} has invalid shape: {reason}"
            ),
            Self::InvalidStructuredEquationShape {
                partition,
                index,
                reason,
            } => write!(
                formatter,
                "Flat wire {partition} structured family {index} is invalid: {reason}"
            ),
            _ => Err(std::fmt::Error),
        }
    }
}

impl std::error::Error for FlatWireError {}

impl From<StructuredEquationOwnerError> for FlatWireError {
    fn from(error: StructuredEquationOwnerError) -> Self {
        let partition = error.partition().wire_name();
        let reason = error.wire_reason();
        match error.location() {
            crate::structured_equation_owners::StructuredOwnerErrorLocation::Row(index) => {
                Self::InvalidEquationShape {
                    partition,
                    index,
                    reason,
                }
            }
            crate::structured_equation_owners::StructuredOwnerErrorLocation::Family(index) => {
                Self::InvalidStructuredEquationShape {
                    partition,
                    index,
                    reason,
                }
            }
        }
    }
}

impl Model {
    fn validate_wire_type_catalog(&self) -> Result<(), FlatWireError> {
        let (_, predefined) = self.validate_predefined_type_catalog()?;
        self.validate_type_root_authority(predefined)?;
        self.validate_effective_type_catalog()?;
        self.validate_declaration_type_catalog()?;
        self.validate_enumeration_type_catalog(predefined)?;
        self.validate_type_presentation_catalog()
    }

    fn validate_predefined_type_catalog(&self) -> Result<(DefId, [TypeId; 5]), FlatWireError> {
        let Some(string_declaration) = self.predefined_string_declaration else {
            return Err(FlatWireError::MissingPredefinedStringDeclaration);
        };
        if string_declaration.index() == 0 {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "the predefined String declaration cannot use DefId(0)",
            });
        }
        if !self.predefined_types.is_complete() {
            return Err(FlatWireError::IncompletePredefinedTypes);
        }
        let predefined = [
            self.predefined_types.real,
            self.predefined_types.integer,
            self.predefined_types.boolean,
            self.predefined_types.string,
            self.predefined_types.clock,
        ];
        if predefined.iter().copied().collect::<IndexSet<_>>().len() != predefined.len() {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "predefined scalar kinds must own distinct exact type identities",
            });
        }
        for type_id in &predefined {
            if self.type_roots.get(type_id) != Some(type_id) {
                return Err(FlatWireError::InvalidTypeCatalog {
                    reason: "a predefined type does not own an exact self-rooted identity",
                });
            }
        }
        if self.type_ids_by_def_id.get(&string_declaration) != Some(&self.predefined_types.string) {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "the predefined String declaration contradicts its canonical type identity",
            });
        }
        Ok((string_declaration, predefined))
    }

    fn validate_type_root_authority(&self, predefined: [TypeId; 5]) -> Result<(), FlatWireError> {
        if self.type_roots.iter().any(|(type_id, root)| {
            type_id.is_unknown()
                || root.is_unknown()
                || !self.type_roots.contains_key(root)
                || self.type_roots.get(root) != Some(root)
        }) {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "the issued type-root catalog contains an unknown, cyclic, or foreign identity",
            });
        }
        let mut issued_type_ids = predefined.into_iter().collect::<IndexSet<_>>();
        issued_type_ids.extend(self.type_ids_by_def_id.values().copied());
        issued_type_ids.extend(self.enumeration_type_roots.iter().copied());
        for (type_id, effective) in &self.effective_types {
            issued_type_ids.insert(*type_id);
            issued_type_ids.insert(effective.nominal_type());
            issued_type_ids.insert(effective.canonical_type());
        }
        if self
            .type_roots
            .keys()
            .any(|type_id| !issued_type_ids.contains(type_id))
        {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "the type-root catalog contains an unissued foreign identity",
            });
        }
        Ok(())
    }

    fn validate_effective_type_catalog(&self) -> Result<(), FlatWireError> {
        for (type_id, effective) in &self.effective_types {
            if type_id.is_unknown()
                || invalid_dimensions(effective.dimensions())
                || self.type_roots.get(type_id) != Some(&effective.canonical_type())
                || self.type_roots.get(&effective.nominal_type())
                    != Some(&effective.canonical_type())
                || self.type_roots.get(&effective.canonical_type())
                    != Some(&effective.canonical_type())
            {
                return Err(FlatWireError::InvalidTypeCatalog {
                    reason: "an effective type has unresolved identity, shape, or issued roots",
                });
            }
        }
        Ok(())
    }

    fn validate_declaration_type_catalog(&self) -> Result<(), FlatWireError> {
        for (declaration, type_id) in &self.type_ids_by_def_id {
            if declaration.index() == 0
                || type_id.is_unknown()
                || !self.type_roots.contains_key(type_id)
            {
                return Err(FlatWireError::InvalidTypeCatalog {
                    reason: "a declaration-to-type entry is unresolved or absent from the effective catalog",
                });
            }
        }
        Ok(())
    }

    fn validate_enumeration_type_catalog(
        &self,
        predefined: [TypeId; 5],
    ) -> Result<(), FlatWireError> {
        for type_id in &self.enumeration_types {
            let Some(effective) = self.effective_types.get(type_id) else {
                return Err(FlatWireError::InvalidTypeCatalog {
                    reason: "an enumeration occurrence is absent from the effective-type catalog",
                });
            };
            if !self
                .enumeration_type_roots
                .contains(&effective.canonical_type())
            {
                return Err(FlatWireError::InvalidTypeCatalog {
                    reason: "an enumeration occurrence contradicts its canonical enumeration root",
                });
            }
        }
        if self.enumeration_type_roots.iter().any(|root| {
            root.is_unknown()
                || predefined.contains(root)
                || self.type_roots.get(root) != Some(root)
        }) {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "an enumeration root lacks a distinct canonical scalar catalog entry",
            });
        }
        if self.effective_types.iter().any(|(type_id, effective)| {
            self.enumeration_type_roots
                .contains(&effective.canonical_type())
                != self.enumeration_types.contains(type_id)
        }) {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "the enumeration occurrence set contradicts the effective-type catalog",
            });
        }
        Ok(())
    }

    fn validate_type_presentation_catalog(&self) -> Result<(), FlatWireError> {
        if self
            .enum_literal_ordinals
            .values()
            .any(|ordinal| *ordinal <= 0)
        {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "enumeration literal ordinals are one-based positive values",
            });
        }
        if self
            .variable_type_names
            .keys()
            .any(|name| !self.variables.contains_key(name))
            || self
                .variable_final_flags
                .iter()
                .any(|(name, final_flag)| !*final_flag || !self.variables.contains_key(name))
        {
            return Err(FlatWireError::InvalidTypeCatalog {
                reason: "variable presentation metadata is detached from the exact variable catalog",
            });
        }
        Ok(())
    }

    fn close_wire_catalogs(&self) -> Result<ClosedWireCatalogs<'_>, FlatWireError> {
        let topology = validate_occurrence_graph(&self.instance_relations)?;
        Ok(ClosedWireCatalogs {
            targets: WireTargetIndex::build(self, &topology)?,
        })
    }

    fn validate_wire_record_types(
        &self,
        targets: &WireTargetIndex<'_>,
    ) -> Result<(), FlatWireError> {
        for (record_id, record) in &self.record_types {
            validate_record_type_shape(
                *record_id,
                record,
                &self.type_ids_by_def_id,
                &self.type_roots,
            )?;
        }
        let membership = WireRecordMembershipIndex::build(&self.instance_relations)?;
        let mut consumed = rustc_hash::FxHashSet::default();
        for record in self.record_instances.values() {
            let layout = self.validate_record_instance_identity(record)?;
            self.validate_record_instance_membership(
                record,
                layout,
                targets,
                &membership,
                &mut consumed,
            )?;
        }
        Ok(())
    }

    fn validate_record_instance_identity<'model>(
        &'model self,
        record: &RecordInstance,
    ) -> Result<&'model RecordType, FlatWireError> {
        let invalid = |reason| FlatWireError::InvalidRecordCatalog {
            record: record.type_def_id,
            reason,
        };
        if record.type_def_id.index() == 0 {
            return Err(invalid(
                "a record instance cannot use the global-scope declaration sentinel",
            ));
        }
        let layout = self
            .record_types
            .get(&record.type_def_id)
            .ok_or_else(|| invalid("a record instance has no exact record layout"))?;
        if layout.name != record.type_name {
            return Err(invalid(
                "a record instance's display type contradicts its exact layout",
            ));
        }
        let declared = self
            .type_ids_by_def_id
            .get(&record.type_def_id)
            .ok_or_else(|| {
                invalid("a record instance declaration is absent from the type-identity catalog")
            })?;
        let effective = self
            .effective_types
            .get(&record.effective_type_id)
            .ok_or_else(|| invalid("a record instance has no exact effective identity"))?;
        if declared != &effective.nominal_type()
            || self.type_roots.get(declared) != Some(&effective.canonical_type())
        {
            return Err(invalid(
                "a record instance's exact nominal or canonical type contradicts its layout declaration",
            ));
        }
        Ok(layout)
    }

    fn validate_record_instance_membership(
        &self,
        record: &RecordInstance,
        layout: &RecordType,
        targets: &WireTargetIndex<'_>,
        membership: &WireRecordMembershipIndex,
        consumed: &mut rustc_hash::FxHashSet<InstanceId>,
    ) -> Result<(), FlatWireError> {
        let class = membership.only_child(
            record.instance_id,
            record.type_def_id,
            InstanceKind::Class,
            record.type_def_id,
        )?;
        consume_record_child(consumed, class, record.type_def_id)?;
        membership.require_child_count(class, layout.fields.len(), record.type_def_id)?;
        for field in &layout.fields {
            let child = membership.child(class, field.def_id, record.type_def_id)?;
            consume_record_child(consumed, child, record.type_def_id)?;
            self.validate_record_member(record, field, child, targets)?;
        }
        Ok(())
    }

    fn validate_record_member(
        &self,
        record: &RecordInstance,
        field: &RecordField,
        child: InstanceId,
        targets: &WireTargetIndex<'_>,
    ) -> Result<(), FlatWireError> {
        let target =
            targets
                .by_occurrence
                .get(&child)
                .ok_or(FlatWireError::InvalidRecordCatalog {
                    record: record.type_def_id,
                    reason: "an exact record field occurrence has no materialized reverse target",
                })?;
        let (dimensions, effective_type_id, nested_record_type) = match target.kind {
            WireTargetKind::Variable => {
                let variable = self.variables.get(target.name).ok_or(
                    FlatWireError::InvalidRecordCatalog {
                        record: record.type_def_id,
                        reason: "a record leaf reverse target is absent from the variable catalog",
                    },
                )?;
                (&variable.dims, variable.type_id, None)
            }
            WireTargetKind::Record => {
                let nested = self.record_instances.get(target.name).ok_or(
                    FlatWireError::InvalidRecordCatalog {
                        record: record.type_def_id,
                        reason: "a nested record reverse target is absent from the record catalog",
                    },
                )?;
                (
                    &nested.dims,
                    nested.effective_type_id,
                    Some(nested.type_def_id),
                )
            }
        };
        let mut expected_dimensions = record.dims.clone();
        expected_dimensions.extend_from_slice(&field.dims);
        let member_effective = self.effective_types.get(&effective_type_id);
        let exact = dimensions == &expected_dimensions
            && member_effective.is_some_and(|effective| {
                effective.nominal_type() == field.effective_type.nominal_type()
                    && effective.canonical_type() == field.effective_type.canonical_type()
                    && effective.dimensions() == expected_dimensions
            })
            && match nested_record_type {
                Some(nested) => nested == field.type_def_id,
                None => !self.record_types.contains_key(&field.type_def_id),
            };
        if exact {
            Ok(())
        } else {
            Err(FlatWireError::InvalidRecordCatalog {
                record: record.type_def_id,
                reason: "an immediate record member's exact declared or effective type contradicts its layout",
            })
        }
    }

    fn validate_wire_provenance_and_calls(
        &self,
        catalogs: &ClosedWireCatalogs<'_>,
    ) -> Result<(), FlatWireError> {
        let mut checker = WireSemanticChecker::from_closed_root(self, catalogs);
        for variable in self.variables.values() {
            require_span(variable.source_span, "variable declaration")?;
            for expression in [
                variable.start.as_ref(),
                variable.min.as_ref(),
                variable.max.as_ref(),
                variable.nominal.as_ref(),
                variable.binding.as_ref(),
            ]
            .into_iter()
            .flatten()
            {
                checker.visit_expression(expression)?;
            }
        }
        for record in self.record_instances.values() {
            require_span(record.source_span, "record instance")?;
        }
        for equation in self.equations.iter().chain(&self.initial_equations) {
            require_span(equation.span, "equation owner")?;
            checker.visit_owned_expression(
                &equation.residual,
                &equation.origin,
                ConnectionExpressionKind::Residual,
            )?;
        }
        for family in self
            .structured_equations
            .iter()
            .chain(&self.initial_structured_equations)
        {
            validate_family_provenance(&mut checker, family)?;
        }
        for assertion in self
            .assert_equations
            .iter()
            .chain(&self.initial_assert_equations)
        {
            require_span(assertion.span, "assert equation owner")?;
            checker.visit_owned_expression(
                &assertion.condition,
                &assertion.origin,
                ConnectionExpressionKind::Assertion,
            )?;
            checker.visit_expression(&assertion.message)?;
            if let Some(level) = &assertion.level {
                checker.visit_expression(level)?;
            }
        }
        for algorithm in self.algorithms.iter().chain(&self.initial_algorithms) {
            validate_algorithm(&mut checker, algorithm)?;
        }
        for chain in &self.when_chains {
            validate_when_chain(&mut checker, chain)?;
        }
        for function in self.functions.values() {
            validate_function_provenance(&mut checker, function)?;
        }
        checker.validate_connected_state()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OccurrenceVisitState {
    Unseen,
    Active,
    Complete,
}

struct OccurrenceTopology {
    preceding_component: rustc_hash::FxHashMap<InstanceId, Option<InstanceId>>,
}

fn validate_occurrence_graph(
    relations: &IndexMap<InstanceId, InstanceRelation, rustc_hash::FxBuildHasher>,
) -> Result<OccurrenceTopology, FlatWireError> {
    let mut states = IndexMap::with_capacity_and_hasher(relations.len(), rustc_hash::FxBuildHasher);
    let mut child_counts = rustc_hash::FxHashMap::<InstanceId, usize>::default();
    let mut class_body_counts = rustc_hash::FxHashMap::<InstanceId, usize>::default();
    let mut sibling_coordinates = rustc_hash::FxHashSet::default();
    for (&instance_id, relation) in relations {
        validate_occurrence_edge(instance_id, relation, relations)?;
        if let Some(owner) = relation.owner {
            *child_counts.entry(owner).or_default() += 1;
            if relation.kind == InstanceKind::Class {
                *class_body_counts.entry(owner).or_default() += 1;
            }
            let Some(declaration) = relation.declaration else {
                return Err(FlatWireError::InvalidInstanceRelation {
                    instance_id,
                    reason: "a child occurrence lacks its exact source declaration",
                });
            };
            if !sibling_coordinates.insert((owner, declaration, relation.indices.to_vec())) {
                return Err(FlatWireError::InvalidInstanceRelation {
                    instance_id,
                    reason: "two sibling occurrences claim one declaration coordinate",
                });
            }
        }
        states.insert(instance_id, OccurrenceVisitState::Unseen);
    }

    for (&instance_id, relation) in relations {
        match relation.kind {
            InstanceKind::Materialized if child_counts.contains_key(&instance_id) => {
                return Err(FlatWireError::InvalidInstanceRelation {
                    instance_id,
                    reason: "a materialized occurrence cannot own child occurrences",
                });
            }
            InstanceKind::Aggregate
                if class_body_counts.get(&instance_id).copied().unwrap_or(0) != 1
                    || child_counts.get(&instance_id).copied().unwrap_or(0) != 1 =>
            {
                return Err(FlatWireError::InvalidInstanceRelation {
                    instance_id,
                    reason: "an aggregate occurrence must own exactly one class-body occurrence",
                });
            }
            InstanceKind::Class | InstanceKind::Aggregate | InstanceKind::Materialized => {}
        }
    }

    for &start in relations.keys() {
        if states[&start] == OccurrenceVisitState::Complete {
            continue;
        }
        let mut path = Vec::new();
        let mut cursor = Some(start);
        while let Some(instance_id) = cursor {
            match states[&instance_id] {
                OccurrenceVisitState::Complete => break,
                OccurrenceVisitState::Active => {
                    return Err(FlatWireError::InvalidInstanceRelation {
                        instance_id,
                        reason: "the occurrence ownership graph contains a cycle",
                    });
                }
                OccurrenceVisitState::Unseen => {
                    states.insert(instance_id, OccurrenceVisitState::Active);
                    path.push(instance_id);
                    cursor = relations[&instance_id].owner;
                }
            }
        }
        for instance_id in path {
            states.insert(instance_id, OccurrenceVisitState::Complete);
        }
    }
    build_occurrence_topology(relations)
}

fn build_occurrence_topology(
    relations: &IndexMap<InstanceId, InstanceRelation, rustc_hash::FxBuildHasher>,
) -> Result<OccurrenceTopology, FlatWireError> {
    let mut preceding_component = rustc_hash::FxHashMap::default();
    for &start in relations.keys() {
        if preceding_component.contains_key(&start) {
            continue;
        }
        let mut path = Vec::new();
        let mut cursor = Some(start);
        while let Some(instance_id) = cursor {
            if preceding_component.contains_key(&instance_id) {
                break;
            }
            path.push(instance_id);
            cursor = relations[&instance_id].owner;
        }
        for instance_id in path.into_iter().rev() {
            let preceding = match relations[&instance_id].owner {
                None => None,
                Some(owner) => match relations[&owner].kind {
                    InstanceKind::Aggregate => Some(owner),
                    InstanceKind::Class => preceding_component[&owner],
                    InstanceKind::Materialized => {
                        return Err(FlatWireError::InvalidInstanceRelation {
                            instance_id,
                            reason: "an occurrence path crosses a materialized owner",
                        });
                    }
                },
            };
            preceding_component.insert(instance_id, preceding);
        }
    }
    Ok(OccurrenceTopology {
        preceding_component,
    })
}

fn validate_occurrence_edge(
    instance_id: InstanceId,
    relation: &InstanceRelation,
    relations: &IndexMap<InstanceId, InstanceRelation, rustc_hash::FxBuildHasher>,
) -> Result<(), FlatWireError> {
    if instance_id.is_unset() {
        return Err(FlatWireError::InvalidInstanceRelation {
            instance_id,
            reason: "the reserved unset identity cannot own an occurrence",
        });
    }
    if relation.declaration.is_none() {
        return Err(FlatWireError::InvalidInstanceRelation {
            instance_id,
            reason: "an occurrence requires its exact source declaration",
        });
    }
    if relation
        .declaration
        .is_some_and(|declaration| declaration.index() == 0)
    {
        return Err(FlatWireError::InvalidInstanceRelation {
            instance_id,
            reason: "an occurrence declaration cannot use the reserved global-scope DefId(0)",
        });
    }
    if relation.indices.iter().any(|index| *index <= 0) {
        return Err(FlatWireError::InvalidInstanceRelation {
            instance_id,
            reason: "array occurrence coordinates are one-based positive ordinals",
        });
    }
    if relation.owner == Some(instance_id) {
        return Err(FlatWireError::InvalidInstanceRelation {
            instance_id,
            reason: "an occurrence cannot own itself",
        });
    }
    if relation
        .owner
        .is_some_and(|owner| !relations.contains_key(&owner))
    {
        return Err(FlatWireError::InvalidInstanceRelation {
            instance_id,
            reason: "its owner is absent from the occurrence graph",
        });
    }
    match relation.owner.and_then(|owner| relations.get(&owner)) {
        None if relation.owner.is_none() && relation.kind != InstanceKind::Class => {
            return Err(FlatWireError::InvalidInstanceRelation {
                instance_id,
                reason: "only a class-body occurrence may be an ownership root",
            });
        }
        Some(owner) if owner.kind == InstanceKind::Materialized => {
            return Err(FlatWireError::InvalidInstanceRelation {
                instance_id,
                reason: "a materialized occurrence cannot own another occurrence",
            });
        }
        Some(owner)
            if matches!(
                relation.kind,
                InstanceKind::Aggregate | InstanceKind::Materialized
            ) && owner.kind != InstanceKind::Class =>
        {
            return Err(FlatWireError::InvalidInstanceRelation {
                instance_id,
                reason: "a component occurrence must be owned by a class-body occurrence",
            });
        }
        Some(owner)
            if relation.kind == InstanceKind::Class
                && !matches!(owner.kind, InstanceKind::Class | InstanceKind::Aggregate) =>
        {
            return Err(FlatWireError::InvalidInstanceRelation {
                instance_id,
                reason: "a class body must belong to an aggregate or inherited class scope",
            });
        }
        None | Some(_) => {}
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WireTargetKind {
    Variable,
    Record,
}

#[derive(Debug, Clone, Copy)]
struct WireTarget<'model> {
    name: &'model VarName,
    component_ref: &'model ComponentReference,
    kind: WireTargetKind,
}

#[derive(Debug, Clone, Copy)]
struct WireFunctionTarget<'model> {
    name: &'model VarName,
    function: &'model Function,
}

struct WireTargetIndex<'model> {
    by_occurrence: rustc_hash::FxHashMap<InstanceId, WireTarget<'model>>,
    by_function_instance: rustc_hash::FxHashMap<FunctionInstanceId, WireFunctionTarget<'model>>,
}

/// Exact root inventories that must be closed before semantic replay starts.
struct ClosedWireCatalogs<'model> {
    targets: WireTargetIndex<'model>,
}

struct WireRecordMembershipIndex {
    children_by_owner: rustc_hash::FxHashMap<InstanceId, Vec<InstanceId>>,
    child_by_coordinate: rustc_hash::FxHashMap<(InstanceId, DefId, Box<[i64]>), InstanceId>,
    kinds: rustc_hash::FxHashMap<InstanceId, InstanceKind>,
}

impl WireRecordMembershipIndex {
    fn build(
        relations: &IndexMap<InstanceId, InstanceRelation, rustc_hash::FxBuildHasher>,
    ) -> Result<Self, FlatWireError> {
        let mut children_by_owner = rustc_hash::FxHashMap::default();
        let mut child_by_coordinate = rustc_hash::FxHashMap::default();
        let mut kinds = rustc_hash::FxHashMap::default();
        for (&instance, relation) in relations {
            kinds.insert(instance, relation.kind);
            let Some(owner) = relation.owner else {
                continue;
            };
            children_by_owner
                .entry(owner)
                .or_insert_with(Vec::new)
                .push(instance);
            if let Some(declaration) = relation.declaration
                && child_by_coordinate
                    .insert((owner, declaration, relation.indices.clone()), instance)
                    .is_some()
            {
                return Err(FlatWireError::InvalidInstanceRelation {
                    instance_id: instance,
                    reason: "one occurrence owner has two children for the same exact declaration coordinate",
                });
            }
        }
        Ok(Self {
            children_by_owner,
            child_by_coordinate,
            kinds,
        })
    }

    fn child(
        &self,
        owner: InstanceId,
        declaration: DefId,
        record: DefId,
    ) -> Result<InstanceId, FlatWireError> {
        self.child_by_coordinate
            .get(&(owner, declaration, Box::default()))
            .copied()
            .ok_or(FlatWireError::InvalidRecordCatalog {
                record,
                reason: "a record instance is missing an exact declared layout field",
            })
    }

    fn only_child(
        &self,
        owner: InstanceId,
        declaration: DefId,
        kind: InstanceKind,
        record: DefId,
    ) -> Result<InstanceId, FlatWireError> {
        self.require_child_count(owner, 1, record)?;
        let child = self.child(owner, declaration, record)?;
        if self.kinds.get(&child) == Some(&kind) {
            Ok(child)
        } else {
            Err(FlatWireError::InvalidRecordCatalog {
                record,
                reason: "a record occurrence does not own one exact class-body child",
            })
        }
    }

    fn require_child_count(
        &self,
        owner: InstanceId,
        expected: usize,
        record: DefId,
    ) -> Result<(), FlatWireError> {
        let actual = self.children_by_owner.get(&owner).map_or(0, Vec::len);
        if actual == expected {
            Ok(())
        } else {
            Err(FlatWireError::InvalidRecordCatalog {
                record,
                reason: "a record occurrence child inventory contradicts its exact layout",
            })
        }
    }
}

fn consume_record_child(
    consumed: &mut rustc_hash::FxHashSet<InstanceId>,
    child: InstanceId,
    record: DefId,
) -> Result<(), FlatWireError> {
    if consumed.insert(child) {
        Ok(())
    } else {
        Err(FlatWireError::InvalidRecordCatalog {
            record,
            reason: "two record layouts consume one exact occurrence child",
        })
    }
}

impl<'model> WireTargetIndex<'model> {
    fn build(model: &'model Model, topology: &OccurrenceTopology) -> Result<Self, FlatWireError> {
        let mut by_occurrence = rustc_hash::FxHashMap::default();
        for (name, variable) in &model.variables {
            let component_ref = variable.component_ref.as_ref().ok_or_else(|| {
                FlatWireError::InvalidOccurrenceTarget {
                    name: name.clone(),
                    instance_id: variable.instance_id,
                    reason: "a Flat variable requires its exact component reference",
                }
            })?;
            validate_occurrence_target(
                name,
                variable.instance_id,
                component_ref,
                InstanceKind::Materialized,
                &model.instance_relations,
                topology,
            )?;
            insert_wire_target(
                &mut by_occurrence,
                variable.instance_id,
                WireTarget {
                    name,
                    component_ref,
                    kind: WireTargetKind::Variable,
                },
            )?;
        }
        for (name, record) in &model.record_instances {
            validate_occurrence_target(
                name,
                record.instance_id,
                &record.component_ref,
                InstanceKind::Aggregate,
                &model.instance_relations,
                topology,
            )?;
            insert_wire_target(
                &mut by_occurrence,
                record.instance_id,
                WireTarget {
                    name,
                    component_ref: &record.component_ref,
                    kind: WireTargetKind::Record,
                },
            )?;
        }
        for (&instance_id, relation) in &model.instance_relations {
            if relation.kind == InstanceKind::Materialized
                && !by_occurrence
                    .get(&instance_id)
                    .is_some_and(|target| target.kind == WireTargetKind::Variable)
            {
                return Err(FlatWireError::InvalidInstanceRelation {
                    instance_id,
                    reason: "a materialized occurrence has no reverse Flat variable membership",
                });
            }
        }
        let mut by_function_instance = rustc_hash::FxHashMap::default();
        for (name, function) in &model.functions {
            let Some(instance_id) = function.instance_id else {
                continue;
            };
            if by_function_instance
                .insert(instance_id, WireFunctionTarget { name, function })
                .is_some()
            {
                return Err(FlatWireError::InvalidFunctionCall {
                    function: name.clone(),
                    reason: "two functions claim one exact callable-instance identity",
                });
            }
        }
        Ok(Self {
            by_occurrence,
            by_function_instance,
        })
    }
}

mod occurrence_paths;
use occurrence_paths::*;
