use std::marker::PhantomData;

use rumoca_core::{
    ComponentReference, SourceMap, Span, StateSelect, StructuredIndexDomain, TypeId, VarName,
};
use serde::{Deserialize, Serialize};

use crate::clocks::{
    ClockEntry, ClockOperation, ClockOwnershipEntry, ClockOwnershipView, ClockView,
    ClockedVariableKind, ClockedVariableRole, Clocks,
};
use crate::conditions::{
    ConditionEntry, ConditionOperation, ConditionView, Conditions, RelationEntry, RelationView,
    RootEntry, RootView,
};
use crate::equations::{
    ContinuousEquations, DiscreteAssignmentEntry, DiscreteAssignmentView, DiscreteEquations,
    EquationOwnerEntry, InitializationEquations, ResidualEquationEntry, StructuredFamilyEntry,
};
use crate::events::{
    EventActionEntry, EventActionKind, EventActionOperation, EventActionView, Events,
    TimeEventEntry, TimeEventView,
};
use crate::expression::{
    BinaryOperator, Coordinate, CoordinateInput, ExprNode, ExpressionArenaStorage,
    ExpressionVariability, Expressions, FrozenExpressionArenaStorage, PackedSubscriptKind,
    ValueType, source_text,
};
use crate::temporal::{
    DelayEntry, DelayView, PositiveParameterEntry, PositiveParameterView, PreviousEntry,
    PreviousView, Temporal, TerminalEntry, TerminalView,
};
use crate::{
    AlgebraicId, ClockId, ClockOwnershipId, ConditionId, ContinuousEquationId, ContinuousFamilyId,
    DaeConstructionError, DaeGeneration, DaeLiteral, DaeProvenance, DelayId, DiscreteAssignmentId,
    DiscreteRealId, DiscreteValueId, DomainBinderId, DomainId, EventActionId, ExprId,
    FunctionFoldId, FunctionId, FunctionParameterId, FunctionValueId, InitializationEquationId,
    InitializationFamilyId, InputId, ParameterId, PreviousId, RelationId, RootId, ScalarType,
    StateId, TerminalId, TimeEventId, ValueTypeId, VariableId,
};

pub const DAE_SCHEMA_VERSION: u16 = 11;

mod domains;
mod function_checks;
mod storage;
mod value_types;
mod view;
mod wire;
pub use domains::Domains;
pub(crate) use domains::insert_domain;
use function_checks::*;
pub use value_types::ValueTypes;

pub use view::{
    ContinuousOwnerView, CoordinateView, DaeView, DomainView, ExpressionKind, ExpressionOperands,
    ExpressionOperation, ExpressionView, FunctionFoldView, FunctionParameterView,
    FunctionStatementView, FunctionStatements, FunctionValueView, FunctionView,
    InitializationOwnerView, ResidualEquationView, StructuredFamilyView, SubscriptView,
    SubscriptsView, ValueTypeOperands, VariableIdentity, VariableView,
};

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct VariableEntry {
    pub(crate) name: VarName,
    pub(crate) role: VariableRole,
    variability: ExpressionVariability,
    pub(crate) value_type: u32,
    declaration: DaeProvenance,
    attributes: Option<VariableAttributesWire>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct VariableAttributesWire {
    component_ref: Option<ComponentReference>,
    binding: Option<u32>,
    start: Option<u32>,
    fixed: Option<bool>,
    min: Option<u32>,
    max: Option<u32>,
    nominal: Option<u32>,
    unit: Option<String>,
    state_select: StateSelect,
    description: Option<String>,
    causality: VariableCausality,
    is_tunable: bool,
    is_held: bool,
    origin: VariableOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableRole {
    Parameter,
    Constant,
    Input,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputVariability {
    Discrete,
    Continuous,
}

impl InputVariability {
    const fn expression_variability(self) -> ExpressionVariability {
        match self {
            Self::Discrete => ExpressionVariability::Discrete,
            Self::Continuous => ExpressionVariability::Continuous,
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableCausality {
    Input,
    Output,
    Parameter,
    CalculatedParameter,
    Independent,
    #[default]
    Local,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableOrigin {
    #[default]
    Source,
    Generated,
}

#[derive(Debug, Clone, Default)]
pub struct VariableAttributes<'dae> {
    pub component_ref: Option<ComponentReference>,
    pub binding: Option<ExprId<'dae>>,
    pub start: Option<ExprId<'dae>>,
    pub fixed: Option<bool>,
    pub min: Option<ExprId<'dae>>,
    pub max: Option<ExprId<'dae>>,
    pub nominal: Option<ExprId<'dae>>,
    pub unit: Option<String>,
    pub state_select: StateSelect,
    pub description: Option<String>,
    pub causality: VariableCausality,
    pub is_tunable: bool,
    pub is_held: bool,
    pub origin: VariableOrigin,
}

/// Linear authority to attach forward-referencing variable attributes.
///
/// The token is branded, non-cloneable, and consumed by [`Variables::define`].
pub struct VariableReservation<'dae> {
    variable: VariableId<'dae>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FunctionEntry {
    name: VarName,
    parameters: Vec<u32>,
    results: Vec<u32>,
    parameter_values: Vec<FunctionParameterEntry>,
    pub(crate) values: Vec<FunctionValueEntry>,
    output_values: Vec<u32>,
    pub(crate) folds: Vec<u32>,
    declaration: DaeProvenance,
    definition: Option<FunctionDefinitionWire>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct FunctionParameterEntry {
    name: VarName,
    value_type: u32,
    declaration: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FunctionValueRole {
    Output,
    Local,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FunctionValueEntry {
    name: VarName,
    pub(crate) value_type: u32,
    role: FunctionValueRole,
    declaration: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum FunctionStatementWire {
    Assignment {
        target: u32,
        value: u32,
        provenance: DaeProvenance,
    },
    For {
        fold: u32,
        statements: Vec<FunctionStatementWire>,
        provenance: DaeProvenance,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct FunctionDefinitionWire {
    statements: Vec<FunctionStatementWire>,
    results: Vec<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FunctionFoldEntry {
    pub(crate) function: u32,
    pub(crate) ordinal: u32,
    pub(crate) domain: u32,
    pub(crate) targets: Vec<u32>,
    pub(crate) parameter_values: Vec<u32>,
    pub(crate) initial_values: Vec<u32>,
    pub(crate) update_values: Vec<u32>,
    pub(crate) output_values: Vec<u32>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct DomainEntry {
    parent: Option<u32>,
    domain: StructuredIndexDomain,
    extents: Box<[u32]>,
    scalar_count: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize, Default)]
#[serde(deny_unknown_fields)]
pub(crate) struct Storage {
    pub(crate) value_types: Vec<ValueType>,
    flat_type_ids: Vec<Option<TypeId>>,
    value_type_provenance: Vec<DaeProvenance>,
    variables: Vec<VariableEntry>,
    pub(crate) functions: Vec<FunctionEntry>,
    pub(crate) function_folds: Vec<FunctionFoldEntry>,
    domains: Vec<DomainEntry>,
    pub(crate) expressions: ExpressionArenaStorage,
    pub(crate) continuous_equations: Vec<ResidualEquationEntry>,
    pub(crate) initialization_equations: Vec<ResidualEquationEntry>,
    pub(crate) discrete_real_equations: Vec<ResidualEquationEntry>,
    pub(crate) discrete_assignments: Vec<DiscreteAssignmentEntry>,
    pub(crate) continuous_families: Vec<StructuredFamilyEntry>,
    pub(crate) initialization_families: Vec<StructuredFamilyEntry>,
    pub(crate) continuous_equation_owners: Vec<EquationOwnerEntry>,
    pub(crate) initialization_equation_owners: Vec<EquationOwnerEntry>,
    pub(crate) equation_family_bodies: Vec<u32>,
    pub(crate) relations: Vec<RelationEntry>,
    pub(crate) conditions: Vec<ConditionEntry>,
    pub(crate) roots: Vec<RootEntry>,
    pub(crate) time_events: Vec<TimeEventEntry>,
    pub(crate) event_actions: Vec<EventActionEntry>,
    pub(crate) clocks: Vec<ClockEntry>,
    pub(crate) clock_ownerships: Vec<ClockOwnershipEntry>,
    pub(crate) previous_values: Vec<PreviousEntry>,
    pub(crate) terminals: Vec<TerminalEntry>,
    pub(crate) delays: Vec<DelayEntry>,
    #[serde(skip)]
    pub(crate) delay_coordinate_uses: Vec<u8>,
    #[serde(skip)]
    unfilled_variables: usize,
    #[serde(skip)]
    unfilled_functions: usize,
    #[serde(skip)]
    unfilled_function_folds: usize,
    #[serde(skip)]
    pub(crate) unfilled_conditions: usize,
    #[serde(skip)]
    pub(crate) unconsumed_delays: usize,
    #[serde(skip)]
    pub(crate) unassigned_discrete_values: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct FrozenStorage {
    value_types: Box<[ValueType]>,
    flat_type_ids: Box<[Option<TypeId>]>,
    value_type_provenance: Box<[DaeProvenance]>,
    variables: Box<[VariableEntry]>,
    functions: Box<[FunctionEntry]>,
    function_folds: Box<[FunctionFoldEntry]>,
    domains: Box<[DomainEntry]>,
    expressions: FrozenExpressionArenaStorage,
    continuous_equations: Box<[ResidualEquationEntry]>,
    initialization_equations: Box<[ResidualEquationEntry]>,
    discrete_real_equations: Box<[ResidualEquationEntry]>,
    discrete_assignments: Box<[DiscreteAssignmentEntry]>,
    continuous_families: Box<[StructuredFamilyEntry]>,
    initialization_families: Box<[StructuredFamilyEntry]>,
    continuous_equation_owners: Box<[EquationOwnerEntry]>,
    initialization_equation_owners: Box<[EquationOwnerEntry]>,
    equation_family_bodies: Box<[u32]>,
    relations: Box<[RelationEntry]>,
    conditions: Box<[ConditionEntry]>,
    roots: Box<[RootEntry]>,
    time_events: Box<[TimeEventEntry]>,
    event_actions: Box<[EventActionEntry]>,
    clocks: Box<[ClockEntry]>,
    clock_ownerships: Box<[ClockOwnershipEntry]>,
    previous_values: Box<[PreviousEntry]>,
    terminals: Box<[TerminalEntry]>,
    delays: Box<[DelayEntry]>,
}

/// Immutable, valid-by-construction schema-v11 DAE.
#[derive(Debug, Clone, Serialize)]
pub struct Dae {
    schema_version: u16,
    source_map: SourceMap,
    storage: FrozenStorage,
}

impl Dae {
    /// Construct a DAE through a fresh, generative ownership brand.
    ///
    /// The higher-ranked closure prevents any arena ID from escaping. Semantic
    /// owner closures borrow this one aggregate sequentially.
    pub fn construct<F>(source_map: SourceMap, build: F) -> Result<Self, DaeConstructionError>
    where
        F: for<'dae> FnOnce(&mut DaeConstruction<'dae>) -> Result<(), DaeConstructionError>,
    {
        let mut storage = Storage::default();
        {
            let mut construction = DaeConstruction {
                source_map: &source_map,
                storage: &mut storage,
                marker: PhantomData,
            };
            build(&mut construction)?;
        }
        storage.finish_construction()?;
        Ok(Self {
            schema_version: DAE_SCHEMA_VERSION,
            source_map,
            storage: storage.freeze(),
        })
    }

    pub const fn schema_version(&self) -> u16 {
        self.schema_version
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub fn source_text(&self, provenance: DaeProvenance) -> Option<&str> {
        source_text(&self.source_map, provenance)
    }

    /// Inspect the finalized DAE through a fresh brand.
    pub fn inspect<R>(&self, inspect: impl for<'dae> FnOnce(DaeView<'dae>) -> R) -> R {
        inspect(DaeView {
            dae: self,
            marker: PhantomData,
        })
    }
}

/// The single mutable aggregate lent to semantic owner closures.
pub struct DaeConstruction<'dae> {
    source_map: &'dae SourceMap,
    storage: &'dae mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DaeConstruction<'dae> {
    pub fn types<R>(
        &mut self,
        build: impl FnOnce(&mut ValueTypes<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut ValueTypes {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn variables<R>(
        &mut self,
        build: impl FnOnce(&mut Variables<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Variables {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn functions<R>(
        &mut self,
        build: impl FnOnce(&mut Functions<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Functions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn domains<R>(
        &mut self,
        build: impl FnOnce(&mut Domains<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Domains {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn expressions<R>(
        &mut self,
        build: impl FnOnce(&mut Expressions<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Expressions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn continuous<R>(
        &mut self,
        build: impl FnOnce(&mut ContinuousEquations<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut ContinuousEquations {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn initialization<R>(
        &mut self,
        build: impl FnOnce(&mut InitializationEquations<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut InitializationEquations {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn discrete<R>(
        &mut self,
        build: impl FnOnce(&mut DiscreteEquations<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut DiscreteEquations {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn conditions<R>(
        &mut self,
        build: impl FnOnce(&mut Conditions<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Conditions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn events<R>(
        &mut self,
        build: impl FnOnce(&mut Events<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Events {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn clocks<R>(
        &mut self,
        build: impl FnOnce(&mut Clocks<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Clocks {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn temporal<R>(
        &mut self,
        build: impl FnOnce(&mut Temporal<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Temporal {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }
}

pub struct Variables<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Variables<'_, 'dae> {
    pub fn parameter(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<ParameterId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Parameter,
            ExpressionVariability::Parameter,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| ParameterId::from_raw(id.index()))
    }

    pub fn constant(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<ParameterId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Constant,
            ExpressionVariability::Constant,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| ParameterId::from_raw(id.index()))
    }

    pub fn input(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        variability: InputVariability,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<InputId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Input,
            variability.expression_variability(),
            value_type,
            declaration,
            attributes,
        )
        .map(|id| InputId::from_raw(id.index()))
    }

    pub fn state(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<StateId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::State,
            ExpressionVariability::Continuous,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| StateId::from_raw(id.index()))
    }

    pub fn algebraic(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<AlgebraicId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Algebraic,
            ExpressionVariability::Continuous,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| AlgebraicId::from_raw(id.index()))
    }

    pub fn output(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<AlgebraicId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Output,
            ExpressionVariability::Continuous,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| AlgebraicId::from_raw(id.index()))
    }

    pub fn discrete_real(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<DiscreteRealId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::DiscreteReal,
            ExpressionVariability::Discrete,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| DiscreteRealId::from_raw(id.index()))
    }

    pub fn discrete_value(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<DiscreteValueId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::DiscreteValue,
            ExpressionVariability::Discrete,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| DiscreteValueId::from_raw(id.index()))
    }

    pub fn reserve_algebraic(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(AlgebraicId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::Algebraic,
            ExpressionVariability::Continuous,
            value_type,
            declaration,
        )?;
        Ok((
            AlgebraicId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_constant(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(ParameterId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::Constant,
            ExpressionVariability::Constant,
            value_type,
            declaration,
        )?;
        Ok((
            ParameterId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_input(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        variability: InputVariability,
        declaration: DaeProvenance,
    ) -> Result<(InputId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::Input,
            variability.expression_variability(),
            value_type,
            declaration,
        )?;
        Ok((
            InputId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_state(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(StateId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::State,
            ExpressionVariability::Continuous,
            value_type,
            declaration,
        )?;
        Ok((
            StateId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_output(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(AlgebraicId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::Output,
            ExpressionVariability::Continuous,
            value_type,
            declaration,
        )?;
        Ok((
            AlgebraicId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_discrete_real(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(DiscreteRealId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::DiscreteReal,
            ExpressionVariability::Discrete,
            value_type,
            declaration,
        )?;
        Ok((
            DiscreteRealId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_discrete_value(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(DiscreteValueId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::DiscreteValue,
            ExpressionVariability::Discrete,
            value_type,
            declaration,
        )?;
        Ok((
            DiscreteValueId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_parameter(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(ParameterId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(
            name,
            VariableRole::Parameter,
            ExpressionVariability::Parameter,
            value_type,
            declaration,
        )?;
        Ok((
            ParameterId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn define(
        &mut self,
        reservation: VariableReservation<'dae>,
        attributes: VariableAttributes<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let variable = reservation.variable;
        self.validate_attributes(variable, &attributes, provenance)?;
        let role = self
            .storage
            .variables
            .get(variable.index() as usize)
            .map(|entry| entry.role)
            .ok_or_else(|| unknown("variable", variable.index(), provenance))?;
        if role == VariableRole::DiscreteValue && !attributes.is_held {
            if attributes.causality == VariableCausality::Input {
                return Err(DaeConstructionError::InvalidVariableRole {
                    name: self.storage.variables[variable.index() as usize]
                        .name
                        .clone(),
                    span: provenance.span(),
                });
            }
            self.storage.unassigned_discrete_values += 1;
        }
        let Some(entry) = self.storage.variables.get_mut(variable.index() as usize) else {
            return Err(unknown("variable", variable.index(), provenance));
        };
        if entry.attributes.is_some() {
            return Err(duplicate("variable", variable.index(), provenance));
        }
        entry.attributes = Some(erase_variable_attributes(attributes));
        self.storage.unfilled_variables -= 1;
        Ok(())
    }

    fn add_complete(
        &mut self,
        name: VarName,
        role: VariableRole,
        variability: ExpressionVariability,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        let id = self.reserve_forward(name, role, variability, value_type, declaration)?;
        self.validate_attributes(id, &attributes, declaration)?;
        if role == VariableRole::DiscreteValue && !attributes.is_held {
            if attributes.causality == VariableCausality::Input {
                return Err(DaeConstructionError::InvalidVariableRole {
                    name: self.storage.variables[id.index() as usize].name.clone(),
                    span: declaration.span(),
                });
            }
            self.storage.unassigned_discrete_values += 1;
        }
        self.storage.variables[id.index() as usize].attributes =
            Some(erase_variable_attributes(attributes));
        self.storage.unfilled_variables -= 1;
        Ok(id)
    }

    fn reserve_forward(
        &mut self,
        name: VarName,
        role: VariableRole,
        variability: ExpressionVariability,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, declaration)?;
        self.storage
            .value_type_at(value_type.index(), declaration)?;
        if self
            .storage
            .variables
            .iter()
            .any(|entry| entry.name == name)
        {
            return Err(DaeConstructionError::DuplicateKey {
                kind: "variable",
                key: name.to_string(),
                span: declaration.span(),
            });
        }
        let raw = checked_u32(self.storage.variables.len(), "variable arena", declaration)?;
        self.storage.variables.push(VariableEntry {
            name,
            role,
            variability,
            value_type: value_type.index(),
            declaration,
            attributes: None,
        });
        self.storage.unfilled_variables += 1;
        Ok(VariableId::from_raw(raw))
    }

    fn validate_attributes(
        &self,
        variable: VariableId<'dae>,
        attributes: &VariableAttributes<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let expected = self
            .storage
            .variables
            .get(variable.index() as usize)
            .map(|entry| entry.value_type)
            .ok_or_else(|| unknown("variable", variable.index(), provenance))?;
        if attributes.is_held {
            let entry = &self.storage.variables[variable.index() as usize];
            if !matches!(
                entry.role,
                VariableRole::DiscreteReal | VariableRole::DiscreteValue
            ) || attributes.start.is_none()
            {
                return Err(DaeConstructionError::InvalidVariableRole {
                    name: entry.name.clone(),
                    span: provenance.span(),
                });
            }
        }
        if let Some(binding) = attributes.binding {
            self.storage.expect_closed_expression(binding, provenance)?;
            let found = self
                .storage
                .expressions
                .value_types
                .get(binding.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", binding.index(), provenance))?;
            self.storage
                .expect_value_type_compatible(expected, found, provenance)?;
        }
        for expression in [
            attributes.start,
            attributes.min,
            attributes.max,
            attributes.nominal,
        ]
        .into_iter()
        .flatten()
        {
            self.storage
                .expect_closed_expression(expression, provenance)?;
            let found = self
                .storage
                .expressions
                .value_types
                .get(expression.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", expression.index(), provenance))?;
            self.storage
                .expect_attribute_type_compatible(expected, found, provenance)?;
        }
        Ok(())
    }
}

fn erase_variable_attributes(attributes: VariableAttributes<'_>) -> VariableAttributesWire {
    VariableAttributesWire {
        component_ref: attributes.component_ref,
        binding: attributes.binding.map(ExprId::index),
        start: attributes.start.map(ExprId::index),
        fixed: attributes.fixed,
        min: attributes.min.map(ExprId::index),
        max: attributes.max.map(ExprId::index),
        nominal: attributes.nominal.map(ExprId::index),
        unit: attributes.unit,
        state_select: attributes.state_select,
        description: attributes.description,
        causality: attributes.causality,
        is_tunable: attributes.is_tunable,
        is_held: attributes.is_held,
        origin: attributes.origin,
    }
}

pub struct Functions<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

/// Linear authority to define one forward-reserved recursive function.
pub struct FunctionReservation<'dae> {
    function: FunctionId<'dae>,
}

impl<'dae> FunctionReservation<'dae> {
    pub const fn function(&self) -> FunctionId<'dae> {
        self.function
    }
}

/// In-progress body owned by exactly one forward-reserved function.
///
/// The body records source-order statements and the constructor-proven
/// denotation of every currently assigned output or local.
pub struct FunctionBody<'dae> {
    function: FunctionId<'dae>,
    domain: Option<DomainId<'dae>>,
    current_values: Vec<Option<u32>>,
    statements: Vec<FunctionStatementWire>,
}

/// Linear authority for one compact function-loop transition.
pub struct FunctionLoop<'dae> {
    fold: FunctionFoldId<'dae>,
    parent_values: Vec<Option<u32>>,
    body: FunctionBody<'dae>,
}

impl<'dae> Functions<'_, 'dae> {
    pub fn value_type(
        &self,
        value: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage.function_value_facts(value, provenance)
    }

    pub fn reserve_recursive(
        &mut self,
        name: VarName,
        parameters: impl IntoIterator<Item = ValueTypeId<'dae>>,
        results: impl IntoIterator<Item = ValueTypeId<'dae>>,
        declaration: DaeProvenance,
    ) -> Result<(FunctionId<'dae>, FunctionReservation<'dae>), DaeConstructionError> {
        check_provenance(self.source_map, declaration)?;
        let parameters = parameters
            .into_iter()
            .map(ValueTypeId::index)
            .collect::<Vec<_>>();
        let results = results
            .into_iter()
            .map(ValueTypeId::index)
            .collect::<Vec<_>>();
        for &ty in parameters.iter().chain(&results) {
            self.storage.value_type_at(ty, declaration)?;
        }
        let raw = checked_u32(self.storage.functions.len(), "function arena", declaration)?;
        self.storage.functions.push(FunctionEntry {
            name,
            parameters,
            results,
            parameter_values: Vec::new(),
            values: Vec::new(),
            output_values: Vec::new(),
            folds: Vec::new(),
            declaration,
            definition: None,
        });
        self.storage.unfilled_functions += 1;
        let function = FunctionId::from_raw(raw);
        Ok((function, FunctionReservation { function }))
    }

    pub fn parameter(
        &mut self,
        reservation: &FunctionReservation<'dae>,
        name: VarName,
        ordinal: usize,
        provenance: DaeProvenance,
    ) -> Result<FunctionParameterId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let entry = self
            .storage
            .functions
            .get_mut(reservation.function.index() as usize)
            .ok_or_else(|| unknown("function", reservation.function.index(), provenance))?;
        if ordinal != entry.parameter_values.len() || ordinal >= entry.parameters.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: entry.parameter_values.len(),
                found: ordinal,
                span: provenance.span(),
            });
        }
        ensure_unique_function_name(entry, &name, provenance)?;
        let ordinal = checked_u32(ordinal, "function parameter", provenance)?;
        entry.parameter_values.push(FunctionParameterEntry {
            name,
            value_type: entry.parameters[ordinal as usize],
            declaration: provenance,
        });
        Ok(FunctionParameterId::from_raw(
            reservation.function.index(),
            ordinal,
        ))
    }

    pub fn output(
        &mut self,
        reservation: &FunctionReservation<'dae>,
        name: VarName,
        ordinal: usize,
        provenance: DaeProvenance,
    ) -> Result<FunctionValueId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let entry = self
            .storage
            .functions
            .get_mut(reservation.function.index() as usize)
            .ok_or_else(|| unknown("function", reservation.function.index(), provenance))?;
        if entry.parameter_values.len() != entry.parameters.len() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function parameter declaration",
                index: checked_u32(
                    entry.parameter_values.len(),
                    "function parameter ordinal",
                    provenance,
                )?,
                span: provenance.span(),
            });
        }
        if ordinal != entry.output_values.len() || ordinal >= entry.results.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: entry.output_values.len(),
                found: ordinal,
                span: provenance.span(),
            });
        }
        ensure_unique_function_name(entry, &name, provenance)?;
        let value = checked_u32(entry.values.len(), "function value arena", provenance)?;
        entry.values.push(FunctionValueEntry {
            name,
            value_type: entry.results[ordinal],
            role: FunctionValueRole::Output,
            declaration: provenance,
        });
        entry.output_values.push(value);
        Ok(FunctionValueId::from_raw(
            reservation.function.index(),
            value,
        ))
    }

    pub fn local(
        &mut self,
        reservation: &FunctionReservation<'dae>,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionValueId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage.value_type_at(value_type.index(), provenance)?;
        let entry = self
            .storage
            .functions
            .get_mut(reservation.function.index() as usize)
            .ok_or_else(|| unknown("function", reservation.function.index(), provenance))?;
        if entry.output_values.len() != entry.results.len() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function output declaration",
                index: checked_u32(
                    entry.output_values.len(),
                    "function output ordinal",
                    provenance,
                )?,
                span: provenance.span(),
            });
        }
        ensure_unique_function_name(entry, &name, provenance)?;
        let value = checked_u32(entry.values.len(), "function value arena", provenance)?;
        entry.values.push(FunctionValueEntry {
            name,
            value_type: value_type.index(),
            role: FunctionValueRole::Local,
            declaration: provenance,
        });
        Ok(FunctionValueId::from_raw(
            reservation.function.index(),
            value,
        ))
    }

    pub fn begin(
        &self,
        reservation: FunctionReservation<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionBody<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let entry = self
            .storage
            .functions
            .get(reservation.function.index() as usize)
            .ok_or_else(|| unknown("function", reservation.function.index(), provenance))?;
        if entry.parameter_values.len() != entry.parameters.len() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function parameter declaration",
                index: checked_u32(
                    entry.parameter_values.len(),
                    "function parameter ordinal",
                    provenance,
                )?,
                span: provenance.span(),
            });
        }
        if entry.output_values.len() != entry.results.len() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function output declaration",
                index: checked_u32(
                    entry.output_values.len(),
                    "function output ordinal",
                    provenance,
                )?,
                span: provenance.span(),
            });
        }
        Ok(FunctionBody {
            function: reservation.function,
            domain: None,
            current_values: vec![None; entry.values.len()],
            statements: Vec::new(),
        })
    }

    pub fn read(
        &mut self,
        body: &FunctionBody<'dae>,
        value: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        check_function_value_owner(body.function, value, provenance)?;
        let definition = body
            .current_values
            .get(value.ordinal() as usize)
            .copied()
            .flatten()
            .ok_or(DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index: value.ordinal(),
                span: provenance.span(),
            })?;
        crate::expression::insert_function_value_use(
            self.source_map,
            self.storage,
            value,
            ExprId::from_raw(definition),
            body.domain,
            provenance,
        )
    }

    pub(crate) fn reconstruct_read(
        &mut self,
        value: FunctionValueId<'dae>,
        definition: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        crate::expression::insert_function_value_use(
            self.source_map,
            self.storage,
            value,
            definition,
            self.storage
                .expr_binder_domain(definition, provenance)?
                .map(DomainId::from_raw),
            provenance,
        )
    }

    pub fn assign(
        &mut self,
        body: &mut FunctionBody<'dae>,
        target: FunctionValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(body.function, target, provenance)?;
        let entry = function_value_entry(self.storage, target, provenance)?;
        expect_function_body_expression(self.storage, body, value, provenance)?;
        validate_function_value_reads(self.storage, body, value, provenance)?;
        let found = self
            .storage
            .expressions
            .value_types
            .get(value.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", value.index(), provenance))?;
        self.storage
            .expect_value_type_compatible(entry.value_type, found, provenance)?;
        body.current_values[target.ordinal() as usize] = Some(value.index());
        body.statements.push(FunctionStatementWire::Assignment {
            target: target.ordinal(),
            value: value.index(),
            provenance,
        });
        Ok(())
    }

    pub fn begin_loop(
        &mut self,
        parent: &FunctionBody<'dae>,
        domain: DomainId<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .domains
            .get(domain.index() as usize)
            .ok_or_else(|| unknown("domain", domain.index(), provenance))?;
        if parent.domain.is_some() {
            return Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: parent.domain.map(DomainId::index),
                found_domain: domain.index(),
                span: provenance.span(),
            });
        }
        let targets = targets.into_iter().collect::<Vec<_>>();
        if targets.is_empty() {
            return Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found: 0,
                span: provenance.span(),
            });
        }
        let mut raw_targets = Vec::with_capacity(targets.len());
        let mut initial_values = Vec::with_capacity(targets.len());
        for target in &targets {
            check_function_value_owner(parent.function, *target, provenance)?;
            if raw_targets.contains(&target.ordinal()) {
                return Err(DaeConstructionError::DuplicateDefinition {
                    kind: "function loop target",
                    index: target.ordinal(),
                    span: provenance.span(),
                });
            }
            raw_targets.push(target.ordinal());
            initial_values.push(parent.current_values[target.ordinal() as usize].ok_or(
                DaeConstructionError::IncompleteDefinition {
                    kind: "function loop initial value",
                    index: target.ordinal(),
                    span: provenance.span(),
                },
            )?);
        }
        let fold = reserve_function_fold(
            self.storage,
            parent.function,
            domain,
            raw_targets,
            initial_values,
            provenance,
        )?;
        let raw = function_fold_raw(self.storage, fold, provenance)?;
        let generated =
            DaeProvenance::generated(DaeGeneration::FunctionLoopLowering, provenance.span())?;
        let mut body = FunctionBody {
            function: parent.function,
            domain: Some(domain),
            current_values: parent.current_values.clone(),
            statements: Vec::new(),
        };
        for (carried, target) in targets.iter().enumerate() {
            let parameter = crate::expression::insert_function_fold_parameter(
                self.source_map,
                self.storage,
                fold,
                carried,
                generated,
            )?;
            self.storage.function_folds[raw as usize]
                .parameter_values
                .push(parameter.index());
            body.current_values[target.ordinal() as usize] = Some(parameter.index());
        }
        Ok(FunctionLoop {
            fold,
            parent_values: parent.current_values.clone(),
            body,
        })
    }

    pub(crate) fn reconstruct_reserve_loop(
        &mut self,
        function: FunctionId<'dae>,
        domain: DomainId<'dae>,
        targets: Vec<FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<FunctionFoldId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .domains
            .get(domain.index() as usize)
            .ok_or_else(|| unknown("domain", domain.index(), provenance))?;
        let mut raw_targets = Vec::with_capacity(targets.len());
        for target in targets {
            check_function_value_owner(function, target, provenance)?;
            if raw_targets.contains(&target.ordinal()) {
                return Err(DaeConstructionError::DuplicateDefinition {
                    kind: "function loop target",
                    index: target.ordinal(),
                    span: provenance.span(),
                });
            }
            raw_targets.push(target.ordinal());
        }
        if raw_targets.is_empty() {
            return Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found: 0,
                span: provenance.span(),
            });
        }
        reserve_function_fold(
            self.storage,
            function,
            domain,
            raw_targets,
            Vec::new(),
            provenance,
        )
    }

    pub(crate) fn reconstruct_loop_parameter(
        &mut self,
        fold: FunctionFoldId<'dae>,
        carried: usize,
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let raw = function_fold_raw(self.storage, fold, provenance)?;
        let entry = &self.storage.function_folds[raw as usize];
        expect_function_loop_generation(entry, provenance)?;
        if carried != entry.parameter_values.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: entry.parameter_values.len(),
                found: carried,
                span: provenance.span(),
            });
        }
        let parameter = crate::expression::insert_function_fold_parameter(
            self.source_map,
            self.storage,
            fold,
            carried,
            provenance,
        )?;
        self.storage.function_folds[raw as usize]
            .parameter_values
            .push(parameter.index());
        Ok(parameter)
    }

    pub(crate) fn reconstruct_define_loop(
        &mut self,
        fold: FunctionFoldId<'dae>,
        initial_values: Vec<ExprId<'dae>>,
        update_values: Vec<ExprId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = function_fold_raw(self.storage, fold, provenance)?;
        let (function, domain, targets, parameter_count, already_defined) = {
            let entry = &self.storage.function_folds[raw as usize];
            (
                fold.function(),
                DomainId::from_raw(entry.domain),
                entry.targets.clone(),
                entry.parameter_values.len(),
                !entry.update_values.is_empty(),
            )
        };
        if already_defined {
            return Err(duplicate("function fold", raw, provenance));
        }
        if initial_values.len() != targets.len() || update_values.len() != targets.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: targets.len(),
                found: initial_values.len().max(update_values.len()),
                span: provenance.span(),
            });
        }
        if parameter_count != targets.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: targets.len(),
                found: parameter_count,
                span: provenance.span(),
            });
        }
        for ((target, initial), update) in targets.iter().zip(&initial_values).zip(&update_values) {
            let expected = self.storage.functions[fold.function().index() as usize].values
                [*target as usize]
                .value_type;
            let initial_type = self
                .storage
                .expressions
                .value_types
                .get(initial.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", initial.index(), provenance))?;
            let update_type = self
                .storage
                .expressions
                .value_types
                .get(update.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", update.index(), provenance))?;
            self.storage
                .expect_value_type_compatible(expected, initial_type, provenance)?;
            self.storage
                .expect_value_type_compatible(expected, update_type, provenance)?;
            self.storage
                .expect_function_expression(*initial, function, provenance)?;
            self.storage
                .expect_domain_expression(*update, domain, provenance)?;
            expect_expression_function_scope(self.storage, *update, function, provenance)?;
        }
        let entry = &mut self.storage.function_folds[raw as usize];
        entry.initial_values = initial_values.into_iter().map(ExprId::index).collect();
        entry.update_values = update_values.into_iter().map(ExprId::index).collect();
        self.storage.unfilled_function_folds -= 1;
        Ok(())
    }

    pub(crate) fn reconstruct_loop_output(
        &mut self,
        fold: FunctionFoldId<'dae>,
        carried: usize,
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let raw = function_fold_raw(self.storage, fold, provenance)?;
        let entry = &self.storage.function_folds[raw as usize];
        expect_function_loop_generation(entry, provenance)?;
        if carried != entry.output_values.len() || entry.update_values.is_empty() {
            return Err(DaeConstructionError::InvalidArity {
                expected: entry.output_values.len(),
                found: carried,
                span: provenance.span(),
            });
        }
        let output = crate::expression::insert_function_fold_output(
            self.source_map,
            self.storage,
            fold,
            carried,
            provenance,
        )?;
        self.storage.function_folds[raw as usize]
            .output_values
            .push(output.index());
        Ok(output)
    }

    pub(crate) fn reconstruct_begin_defined_loop(
        &self,
        parent: &FunctionBody<'dae>,
        fold: FunctionFoldId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionLoop<'dae>, DaeConstructionError> {
        let raw = function_fold_raw(self.storage, fold, provenance)?;
        let entry = &self.storage.function_folds[raw as usize];
        if entry.function != parent.function.index()
            || entry.targets.len() != entry.parameter_values.len()
            || entry.targets.len() != entry.initial_values.len()
            || entry.targets.len() != entry.update_values.len()
            || entry.targets.len() != entry.output_values.len()
        {
            return Err(DaeConstructionError::MalformedWire {
                column: "function_folds",
            });
        }
        for (target, initial) in entry.targets.iter().zip(&entry.initial_values) {
            if parent.current_values[*target as usize] != Some(*initial) {
                return Err(DaeConstructionError::InvalidFunctionValueRead {
                    value: *target,
                    expected_definition: parent.current_values[*target as usize],
                    found_definition: *initial,
                    span: provenance.span(),
                });
            }
        }
        let mut body = FunctionBody {
            function: parent.function,
            domain: Some(DomainId::from_raw(entry.domain)),
            current_values: parent.current_values.clone(),
            statements: Vec::new(),
        };
        for (target, parameter) in entry.targets.iter().zip(&entry.parameter_values) {
            body.current_values[*target as usize] = Some(*parameter);
        }
        Ok(FunctionLoop {
            fold,
            parent_values: parent.current_values.clone(),
            body,
        })
    }

    pub(crate) fn reconstruct_finish_defined_loop(
        &self,
        parent: &mut FunctionBody<'dae>,
        loop_body: FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let raw = function_fold_raw(self.storage, loop_body.fold, provenance)?;
        let entry = &self.storage.function_folds[raw as usize];
        for (target, update) in entry.targets.iter().zip(&entry.update_values) {
            if loop_body.body.current_values[*target as usize] != Some(*update) {
                return Err(DaeConstructionError::InvalidFunctionValueRead {
                    value: *target,
                    expected_definition: loop_body.body.current_values[*target as usize],
                    found_definition: *update,
                    span: provenance.span(),
                });
            }
        }
        for (target, output) in entry.targets.iter().zip(&entry.output_values) {
            parent.current_values[*target as usize] = Some(*output);
        }
        parent.statements.push(FunctionStatementWire::For {
            fold: loop_body.fold.ordinal(),
            statements: loop_body.body.statements,
            provenance,
        });
        Ok(())
    }

    pub fn assign_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        target: FunctionValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.assign(&mut loop_body.body, target, value, provenance)
    }

    pub fn finish_loop(
        &mut self,
        parent: &mut FunctionBody<'dae>,
        loop_body: FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        if parent.function != loop_body.body.function
            || parent.current_values != loop_body.parent_values
        {
            return Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(parent.function.index()),
                found_function: loop_body.body.function.index(),
                span: provenance.span(),
            });
        }
        let raw = function_fold_raw(self.storage, loop_body.fold, provenance)?;
        let (domain, targets) = {
            let entry = &self.storage.function_folds[raw as usize];
            if !entry.update_values.is_empty() {
                return Err(duplicate("function fold", raw, provenance));
            }
            (DomainId::from_raw(entry.domain), entry.targets.clone())
        };
        let updates = targets
            .iter()
            .map(|target| {
                loop_body.body.current_values[*target as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function loop update",
                        index: *target,
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        for update in &updates {
            let update = ExprId::from_raw(*update);
            self.storage
                .expect_domain_expression(update, domain, provenance)?;
            match self.storage.expr_function_scope(update, provenance)? {
                None => {}
                Some(function) if function == parent.function.index() => {}
                Some(function) => {
                    return Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(parent.function.index()),
                        found_function: function,
                        span: provenance.span(),
                    });
                }
            }
        }
        self.storage.function_folds[raw as usize].update_values = updates;
        self.storage.unfilled_function_folds -= 1;
        let generated =
            DaeProvenance::generated(DaeGeneration::FunctionLoopLowering, provenance.span())?;
        for (carried, target) in targets.iter().enumerate() {
            let output = crate::expression::insert_function_fold_output(
                self.source_map,
                self.storage,
                loop_body.fold,
                carried,
                generated,
            )?;
            self.storage.function_folds[raw as usize]
                .output_values
                .push(output.index());
            parent.current_values[*target as usize] = Some(output.index());
        }
        parent.statements.push(FunctionStatementWire::For {
            fold: loop_body.fold.ordinal(),
            statements: loop_body.body.statements,
            provenance,
        });
        Ok(())
    }

    pub fn define(
        &mut self,
        body: FunctionBody<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let function = body.function;
        let expected = self
            .storage
            .functions
            .get(function.index() as usize)
            .map(|entry| entry.results.clone())
            .ok_or_else(|| unknown("function", function.index(), provenance))?;
        let output_values = self.storage.functions[function.index() as usize]
            .output_values
            .clone();
        let results = output_values
            .iter()
            .map(|value| {
                body.current_values[*value as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function output",
                        index: *value,
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        validate_function_results(self.storage, function, &expected, &results, provenance)?;
        let Some(entry) = self.storage.functions.get_mut(function.index() as usize) else {
            return Err(unknown("function", function.index(), provenance));
        };
        if entry.definition.is_some() {
            return Err(duplicate("function", function.index(), provenance));
        }
        entry.definition = Some(FunctionDefinitionWire {
            statements: body.statements,
            results,
        });
        self.storage.unfilled_functions -= 1;
        Ok(())
    }
}

fn validate_function_results(
    storage: &Storage,
    function: FunctionId<'_>,
    expected: &[u32],
    results: &[u32],
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if expected.len() != results.len() {
        return Err(DaeConstructionError::InvalidArity {
            expected: expected.len(),
            found: results.len(),
            span: at.span(),
        });
    }
    for (&result, &expected_type) in results.iter().zip(expected) {
        storage.expression_at(result, at)?;
        storage.expect_function_expression(ExprId::from_raw(result), function, at)?;
        storage.expect_value_type_compatible(
            expected_type,
            storage.expressions.value_types[result as usize],
            at,
        )?;
    }
    Ok(())
}

pub(crate) fn check_provenance(
    source_map: &SourceMap,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let span = provenance.span();
    let Some((_, source)) = source_map.get_source(span.source) else {
        return Err(DaeConstructionError::UnknownSource { span });
    };
    let range = span.start.0..span.end.0;
    if range.start > range.end
        || range.end > source.len()
        || !source.is_char_boundary(range.start)
        || !source.is_char_boundary(range.end)
    {
        return Err(DaeConstructionError::InvalidSourceRange {
            span,
            source_len: source.len(),
        });
    }
    Ok(())
}

fn check_type_capacity(ty: &ValueType, at: DaeProvenance) -> Result<(), DaeConstructionError> {
    if ty.is_record() || ty.scalar_count().is_some() {
        return Ok(());
    }
    Err(DaeConstructionError::CapacityExceeded {
        arena: "value type scalar layout",
        attempted_index: usize::MAX,
        span: at.span(),
    })
}

pub(crate) fn checked_u32(
    value: usize,
    arena: &'static str,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    u32::try_from(value).map_err(|_| DaeConstructionError::CapacityExceeded {
        arena,
        attempted_index: value,
        span: at.span(),
    })
}

pub(crate) fn unknown(kind: &'static str, index: u32, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::UnknownId {
        kind,
        index,
        span: at.span(),
    }
}

pub(crate) fn duplicate(kind: &'static str, index: u32, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::DuplicateDefinition {
        kind,
        index,
        span: at.span(),
    }
}

fn incomplete(kind: &'static str, index: usize, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::IncompleteDefinition {
        kind,
        index: u32::try_from(index)
            .expect("a decoded DAE arena cannot exceed addressable u32 capacity"),
        span: at.span(),
    }
}

trait DeclaredEntry {
    fn declaration(&self) -> DaeProvenance;
    fn is_complete(&self) -> bool;
}

impl DeclaredEntry for VariableEntry {
    fn declaration(&self) -> DaeProvenance {
        self.declaration
    }

    fn is_complete(&self) -> bool {
        self.attributes.is_some()
    }
}

impl DeclaredEntry for FunctionEntry {
    fn declaration(&self) -> DaeProvenance {
        self.declaration
    }

    fn is_complete(&self) -> bool {
        self.definition.is_some()
    }
}

impl DeclaredEntry for ConditionEntry {
    fn declaration(&self) -> DaeProvenance {
        self.provenance
    }

    fn is_complete(&self) -> bool {
        self.node.is_some()
    }
}
