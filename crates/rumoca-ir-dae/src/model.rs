mod domains;
mod external_functions;
mod function_checks;
mod function_reads;
mod storage;
mod value_types;
mod variable_types;
mod view;
mod wire;

use std::marker::PhantomData;

use rumoca_core::{
    ComponentReference, SourceMap, Span, StateSelect, StructuredIndexDomain, TypeId, VarName,
};
use serde::{Deserialize, Serialize};

use crate::clocks::{
    ClockEntry, ClockOperation, ClockOwnershipEntry, ClockOwnershipView, ClockView,
    ClockedVariableKind, Clocks,
};
use crate::conditions::{
    ConditionEntry, ConditionOperation, ConditionView, Conditions, RelationEntry, RelationView,
    RootEntry, RootView,
};
use crate::discrete_values::{
    DiscreteBranchActivationEntry, DiscreteValueBranchEntry, DiscreteValueOwnerEntry,
    DiscreteValueOwnerView, DiscreteValueTopology, build_discrete_value_topology,
};
use crate::equations::{
    ContinuousEquations, DiscreteEquations, DiscreteRealActivation, DiscreteRealEquationEntry,
    DiscreteRealEquationView, EquationOwnerEntry, InitialDiscreteValueEntry,
    InitialDiscreteValueView, InitializationEquations, ResidualEquationEntry, ResidualShape,
    StructuredFamilyEntry,
};
use crate::events::{
    EventActionEntry, EventActionKind, EventActionOperation, EventActionView, Events,
    TimeEventEntry, TimeEventView,
};
use crate::expression::{
    BinaryOperator, Coordinate, CoordinateInput, ExprNode, ExpressionArenaStorage,
    ExpressionVariability, Expressions, FrozenExpressionArenaStorage, ValueType, source_text,
};
use crate::temporal::{
    DelayEntry, DelayKind, DelayOperation, DelayView, PositiveParameterEntry,
    PositiveParameterView, PreviousEntry, PreviousView, Temporal, TerminalEntry, TerminalView,
};
use crate::{
    AlgebraicId, ClockId, ClockOwnershipId, ConditionId, ContinuousEquationId, ContinuousFamilyId,
    DaeConstructionError, DaeGeneration, DaeLiteral, DaeProvenance, DelayId, DiscreteRealId,
    DiscreteValueId, DiscreteValueOwnerId, DomainBinderId, DomainId, EventActionId, ExprId,
    FunctionDefinitionId, FunctionFoldId, FunctionId, FunctionParameterId, FunctionValueId,
    InitializationEquationId, InitializationFamilyId, InputId, ParameterId, PreviousId, RelationId,
    RootId, ScalarType, StateId, TerminalId, TimeEventId, ValueTypeId, VariableId,
};

/// The one supported DAE wire version.
///
/// Wire records name their own columns, but ordinal-tagged encodings identify
/// enum variants positionally, so adding, removing, or reordering a wire
/// variant changes the decodable shape. Every such change bumps this constant,
/// and decode then rejects the superseded number instead of reading it.
///
/// 14 inserts `ConditionNodeWire::Always` at ordinal 1 and appends
/// `ConditionNodeWire::AnyRise`. The insertion shifts every later condition
/// ordinal, so a v13 payload decodes to the wrong variant rather than failing —
/// which is exactly the case this constant exists to reject.
///
/// 15 appends `CoordinateWire::PreState` and `CoordinateWire::PreAlgebraic`.
/// They sit at the tail deliberately: decode reads the whole payload before it
/// checks this number, so appended ordinals keep every earlier variant stable
/// and an old blob fails cleanly on the version instead of mis-decoding
/// mid-stream.
pub const DAE_SCHEMA_VERSION: u16 = 15;

pub use domains::Domains;
pub(crate) use domains::insert_domain;
use external_functions::build_external_body;
pub use external_functions::{
    ExternalArgument, ExternalFunctionBody, ExternalLanguage, ExternalLinkage, FunctionPurity,
};
pub(crate) use external_functions::{ExternalArgumentEntry, ExternalBodyEntry};
use function_checks::*;
pub(crate) use function_reads::{
    FunctionReadFact, FunctionReadMergeError, FunctionReadSet, FunctionReadSets,
};
pub use value_types::ValueTypes;
use variable_types::VariableTypeCapability;

pub use view::{
    ContinuousOwnerView, CoordinateView, DaeView, DomainView, ExpressionKind, ExpressionOperands,
    ExpressionOperation, ExpressionView, ExternalArgumentView, ExternalFunctionView,
    FunctionDefinitionValues, FunctionDefinitionView, FunctionFoldView, FunctionParameterView,
    FunctionStatementView, FunctionStatements, FunctionValueView, FunctionView,
    InitializationOwnerView, RangeBoundView, RangeView, ResidualEquationView,
    StringConversionFormatView, StructuredFamilyView, SubscriptView, SubscriptsView,
    ValueTypeOperands, VariableIdentity, VariableView,
};

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct VariableEntry {
    pub(crate) name: VarName,
    pub(crate) role: VariableRole,
    variability: ExpressionVariability,
    pub(crate) value_type: u32,
    pub(crate) declaration: DaeProvenance,
    pub(crate) attributes: Option<VariableAttributesWire>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct VariableAttributesWire {
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
    pub(crate) causality: VariableCausality,
    is_tunable: bool,
    is_held: bool,
    origin: VariableOrigin,
}

impl VariableEntry {
    pub(crate) const fn declaration(&self) -> DaeProvenance {
        self.declaration
    }

    pub(crate) const fn attributes_missing(&self) -> bool {
        self.attributes.is_none()
    }

    pub(crate) fn is_discrete_value_input(&self) -> bool {
        self.role == VariableRole::DiscreteValue
            && self
                .attributes
                .as_ref()
                .is_some_and(|attributes| attributes.causality == VariableCausality::Input)
    }

    pub(crate) fn requires_discrete_value_owner(&self) -> bool {
        self.role == VariableRole::DiscreteValue
            && self.attributes.is_some()
            && !self.is_discrete_value_input()
    }
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionEntry {
    name: VarName,
    parameters: Vec<u32>,
    results: Vec<u32>,
    parameter_values: Vec<FunctionParameterEntry>,
    pub(crate) values: Vec<FunctionValueEntry>,
    output_values: Vec<u32>,
    pub(crate) definitions: Vec<FunctionDefinitionEntry>,
    pub(crate) folds: Vec<u32>,
    declaration: DaeProvenance,
    definition: Option<FunctionBodyEntry>,
    build: Option<FunctionBuildState>,
}

/// The one checked body a finalized function owns.
///
/// A function is either a Modelica algorithm body proven by assignment and
/// fold transitions, or an MLS §12.9 external interface. There is no third,
/// bodyless state a finalized `Dae` can hold.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FunctionBodyEntry {
    Modelica(FunctionDefinitionWire),
    External(ExternalBodyEntry),
}

impl FunctionBodyEntry {
    fn modelica(&self) -> Option<&FunctionDefinitionWire> {
        match self {
            Self::Modelica(definition) => Some(definition),
            Self::External(_) => None,
        }
    }

    pub(crate) fn external(&self) -> Option<&ExternalBodyEntry> {
        match self {
            Self::Modelica(_) => None,
            Self::External(external) => Some(external),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionBuildState {
    current_values: Vec<Option<u32>>,
    statements: Vec<FunctionStatementWire>,
    carried_targets: rustc_hash::FxHashSet<u32>,
    parent_statements: Vec<FunctionStatementWire>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionValueEntry {
    name: VarName,
    pub(crate) value_type: u32,
    role: FunctionValueRole,
    declaration: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionDefinitionEntry {
    target: u32,
    rhs: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq)]
enum FunctionStatementWire {
    Assignment {
        definition: u32,
    },
    For {
        fold: u32,
        statements: Vec<FunctionStatementWire>,
        provenance: DaeProvenance,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionDefinitionWire {
    statements: Vec<FunctionStatementWire>,
    results: Vec<u32>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionFoldEntry {
    pub(crate) function: u32,
    pub(crate) ordinal: u32,
    pub(crate) domain: u32,
    pub(crate) targets: Vec<u32>,
    pub(crate) parameter_definitions: Vec<u32>,
    pub(crate) initial_definitions: Vec<u32>,
    pub(crate) update_definitions: Vec<u32>,
    pub(crate) output_definitions: Vec<u32>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct DomainEntry {
    parent: Option<u32>,
    domain: StructuredIndexDomain,
    #[serde(skip_serializing)]
    extents: Box<[u32]>,
    #[serde(skip_serializing)]
    scalar_count: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Default)]
pub(crate) struct Storage {
    pub(crate) predefined_string_declaration: Option<rumoca_core::DefId>,
    pub(crate) value_types: Vec<ValueType>,
    flat_type_ids: Vec<Option<TypeId>>,
    value_type_provenance: Vec<DaeProvenance>,
    flat_type_lookup: rustc_hash::FxHashMap<TypeId, u32>,
    structural_type_lookup: rustc_hash::FxHashMap<ValueType, u32>,
    pub(crate) variables: Vec<VariableEntry>,
    pub(crate) functions: Vec<FunctionEntry>,
    pub(crate) function_folds: Vec<FunctionFoldEntry>,
    domains: Vec<DomainEntry>,
    pub(crate) expressions: ExpressionArenaStorage,
    pub(crate) continuous_equations: Vec<ResidualEquationEntry>,
    pub(crate) initialization_equations: Vec<ResidualEquationEntry>,
    pub(crate) initial_discrete_values: Vec<InitialDiscreteValueEntry>,
    pub(crate) initial_discrete_value_by_variable: rustc_hash::FxHashMap<u32, u32>,
    pub(crate) discrete_real_equations: Vec<DiscreteRealEquationEntry>,
    pub(crate) discrete_value_owners: Vec<DiscreteValueOwnerEntry>,
    pub(crate) discrete_value_targets: Vec<u32>,
    pub(crate) discrete_value_branches: Vec<DiscreteValueBranchEntry>,
    pub(crate) discrete_value_branch_values: Vec<u32>,
    pub(crate) discrete_value_branch_value_provenance: Vec<DaeProvenance>,
    pub(crate) continuous_families: Vec<StructuredFamilyEntry>,
    pub(crate) initialization_families: Vec<StructuredFamilyEntry>,
    pub(crate) continuous_equation_owners: Vec<EquationOwnerEntry>,
    pub(crate) initialization_equation_owners: Vec<EquationOwnerEntry>,
    pub(crate) equation_family_bodies: Vec<u32>,
    pub(crate) relations: Vec<RelationEntry>,
    pub(crate) conditions: Vec<ConditionEntry>,
    pub(crate) condition_owner_clocks: Vec<Option<u32>>,
    pub(crate) roots: Vec<RootEntry>,
    pub(crate) time_events: Vec<TimeEventEntry>,
    pub(crate) event_actions: Vec<EventActionEntry>,
    pub(crate) clocks: Vec<ClockEntry>,
    pub(crate) clock_ownerships: Vec<ClockOwnershipEntry>,
    pub(crate) clock_ownership_by_variable: rustc_hash::FxHashMap<u32, u32>,
    pub(crate) previous_values: Vec<PreviousEntry>,
    pub(crate) previous_by_variable: rustc_hash::FxHashMap<u32, u32>,
    pub(crate) terminals: Vec<TerminalEntry>,
    pub(crate) delays: Vec<DelayEntry>,
    pub(crate) function_read_sets: FunctionReadSets,
    unfilled_variables: usize,
    unfilled_functions: usize,
    unfilled_function_folds: usize,
    pub(crate) unfilled_conditions: usize,
    pub(crate) required_discrete_value_count: usize,
    pub(crate) first_required_discrete_value: Option<(u32, DaeProvenance)>,
    pub(crate) discrete_value_topology_complete: bool,
}

#[derive(Debug, PartialEq)]
struct FrozenStorage {
    predefined_string_declaration: Option<rumoca_core::DefId>,
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
    initial_discrete_values: Box<[InitialDiscreteValueEntry]>,
    discrete_real_equations: Box<[DiscreteRealEquationEntry]>,
    discrete_value_owners: Box<[DiscreteValueOwnerEntry]>,
    discrete_value_targets: Box<[u32]>,
    discrete_value_branches: Box<[DiscreteValueBranchEntry]>,
    discrete_value_branch_values: Box<[u32]>,
    discrete_value_branch_value_provenance: Box<[DaeProvenance]>,
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

/// Immutable, valid-by-construction schema-v13 DAE.
#[derive(Debug, Serialize)]
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

/// Complete function header supplied to one scoped construction operation.
pub struct FunctionSignature<'dae> {
    name: VarName,
    parameters: Vec<ValueTypeId<'dae>>,
    results: Vec<ValueTypeId<'dae>>,
    declaration: DaeProvenance,
}

impl<'dae> FunctionSignature<'dae> {
    pub fn new(
        name: VarName,
        parameters: impl IntoIterator<Item = ValueTypeId<'dae>>,
        results: impl IntoIterator<Item = ValueTypeId<'dae>>,
        declaration: DaeProvenance,
    ) -> Self {
        Self {
            name,
            parameters: parameters.into_iter().collect(),
            results: results.into_iter().collect(),
            declaration,
        }
    }
}

macro_rules! construction_owner_scopes {
    ($($method:ident => $owner:ident),+ $(,)?) => {
        $(pub fn $method<R>(
            &mut self,
            build: impl FnOnce(&mut $owner<'_, 'dae>) -> Result<R, DaeConstructionError>,
        ) -> Result<R, DaeConstructionError> {
            build(&mut $owner {
                source_map: self.source_map,
                storage: self.storage,
                marker: PhantomData,
            })
        })+
    };
}

impl<'dae> DaeConstruction<'dae> {
    construction_owner_scopes! {
        types => ValueTypes,
        variables => Variables,
        functions => Functions,
        domains => Domains,
        expressions => Expressions,
        continuous => ContinuousEquations,
        initialization => InitializationEquations,
        discrete => DiscreteEquations,
        conditions => Conditions,
        events => Events,
        clocks => Clocks,
        temporal => Temporal,
    }

    /// Register the Resolve-proven predefined `String` declaration before any
    /// conversion expression is constructed.
    pub fn register_predefined_string(
        &mut self,
        declaration: rumoca_core::DefId,
    ) -> Result<(), DaeConstructionError> {
        match self.storage.predefined_string_declaration {
            Some(expected) if expected != declaration => Err(
                DaeConstructionError::ConflictingPredefinedStringRegistration {
                    expected,
                    found: declaration,
                },
            ),
            Some(_) => Ok(()),
            None => {
                self.storage.predefined_string_declaration = Some(declaration);
                Ok(())
            }
        }
    }

    /// Construct one acyclic function after all of its callees.
    pub fn function<R>(
        &mut self,
        signature: FunctionSignature<'dae>,
        build: impl for<'function> FnOnce(
            &mut DaeConstruction<'dae>,
            FunctionReservation<'function, 'dae>,
        ) -> Result<R, DaeConstructionError>,
    ) -> Result<(FunctionId<'dae>, R), DaeConstructionError> {
        let declaration = signature.declaration;
        let function = self.functions(|functions| functions.insert_header(signature))?;
        let result = build(
            self,
            FunctionReservation::new(function, FunctionConstruction::Acyclic),
        )?;
        expect_complete_function(self.storage, function, declaration)?;
        Ok((function, result))
    }

    /// Construct one proven recursive SCC through a single lexical capability.
    pub fn recursive_functions<R>(
        &mut self,
        first: FunctionSignature<'dae>,
        additional: impl IntoIterator<Item = FunctionSignature<'dae>>,
        build: impl for<'group> FnOnce(
            &mut DaeConstruction<'dae>,
            Vec<FunctionReservation<'group, 'dae>>,
        ) -> Result<R, DaeConstructionError>,
    ) -> Result<(Vec<FunctionId<'dae>>, R), DaeConstructionError> {
        let owner = first.declaration;
        let signatures = std::iter::once(first).chain(additional).collect::<Vec<_>>();
        let start = checked_u32(self.storage.functions.len(), "function arena", owner)?;
        let end = start
            .checked_add(u32::try_from(signatures.len()).map_err(|_| {
                DaeConstructionError::CapacityExceeded {
                    arena: "function arena",
                    attempted_index: self.storage.functions.len() + signatures.len(),
                    span: owner.span(),
                }
            })?)
            .ok_or(DaeConstructionError::CapacityExceeded {
                arena: "function arena",
                attempted_index: self.storage.functions.len() + signatures.len(),
                span: owner.span(),
            })?;
        let construction = FunctionConstruction::Recursive { start, end };
        let mut functions = Vec::with_capacity(signatures.len());
        for signature in signatures {
            let function = self.functions(|owner| owner.insert_header(signature))?;
            functions.push(function);
        }
        let reservations = functions
            .iter()
            .copied()
            .map(|function| FunctionReservation::new(function, construction))
            .collect();
        let result = build(self, reservations)?;
        for &function in &functions {
            expect_complete_function(self.storage, function, owner)?;
        }
        validate_recursive_function_group(self.storage, &functions, owner)?;
        Ok((functions, result))
    }

    pub fn b1c(
        &mut self,
        plan: impl IntoIterator<Item = DiscreteValueId<'dae>>,
        build: impl FnOnce(&mut DiscreteValueTopology<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<(), DaeConstructionError> {
        build_discrete_value_topology(self.source_map, self.storage, plan, build)
    }
}

pub struct Variables<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

macro_rules! variable_role_constructors {
    ($(
        $complete:ident / $reserve:ident
        ($($argument:ident: $argument_type:ty),*)
        => $id:ident, $role:path, $variability:expr
    );+ $(;)?) => {
        $(pub fn $complete(
            &mut self,
            name: VarName,
            value_type: ValueTypeId<'dae>,
            $($argument: $argument_type,)*
            declaration: DaeProvenance,
            attributes: VariableAttributes<'dae>,
        ) -> Result<$id<'dae>, DaeConstructionError> {
            self.add_complete(
                name,
                $role,
                $variability,
                value_type,
                declaration,
                attributes,
            )
            .map(|id| $id::from_raw(id.index()))
        }

        pub fn $reserve(
            &mut self,
            name: VarName,
            value_type: ValueTypeId<'dae>,
            $($argument: $argument_type,)*
            declaration: DaeProvenance,
        ) -> Result<($id<'dae>, VariableReservation<'dae>), DaeConstructionError> {
            let id = self.reserve_forward(
                name,
                $role,
                $variability,
                value_type,
                declaration,
            )?;
            Ok((
                $id::from_raw(id.index()),
                VariableReservation { variable: id },
            ))
        })+
    };
}

impl<'dae> Variables<'_, 'dae> {
    variable_role_constructors! {
        parameter / reserve_parameter ()
            => ParameterId, VariableRole::Parameter, ExpressionVariability::Parameter;
        constant / reserve_constant ()
            => ParameterId, VariableRole::Constant, ExpressionVariability::Constant;
        input / reserve_input (variability: InputVariability)
            => InputId, VariableRole::Input, variability.expression_variability();
        state / reserve_state ()
            => StateId, VariableRole::State, ExpressionVariability::Continuous;
        algebraic / reserve_algebraic ()
            => AlgebraicId, VariableRole::Algebraic, ExpressionVariability::Continuous;
        output / reserve_output ()
            => AlgebraicId, VariableRole::Output, ExpressionVariability::Continuous;
        discrete_real / reserve_discrete_real ()
            => DiscreteRealId, VariableRole::DiscreteReal, ExpressionVariability::Discrete;
        discrete_value / reserve_discrete_value ()
            => DiscreteValueId, VariableRole::DiscreteValue, ExpressionVariability::Discrete;
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
        let requires_discrete_value_owner =
            role == VariableRole::DiscreteValue && attributes.causality != VariableCausality::Input;
        if requires_discrete_value_owner {
            self.storage
                .expect_discrete_value_topology_open(provenance)?;
        }
        let Some(entry) = self.storage.variables.get_mut(variable.index() as usize) else {
            return Err(unknown("variable", variable.index(), provenance));
        };
        if entry.attributes.is_some() {
            return Err(duplicate("variable", variable.index(), provenance));
        }
        entry.attributes = Some(erase_variable_attributes(attributes));
        if requires_discrete_value_owner {
            self.storage
                .register_required_discrete_value(variable.index(), provenance);
        }
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
        let requires_discrete_value_owner =
            role == VariableRole::DiscreteValue && attributes.causality != VariableCausality::Input;
        if requires_discrete_value_owner {
            self.storage
                .expect_discrete_value_topology_open(declaration)?;
        }
        let id = self.reserve_forward(name, role, variability, value_type, declaration)?;
        self.validate_attributes(id, &attributes, declaration)?;
        self.storage.variables[id.index() as usize].attributes =
            Some(erase_variable_attributes(attributes));
        if requires_discrete_value_owner {
            self.storage
                .register_required_discrete_value(id.index(), declaration);
        }
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
        let capability = self.storage.variable_type_capability(
            &name,
            role,
            variability,
            value_type,
            declaration,
        )?;
        self.insert_reservation(name, capability, declaration)
    }

    fn insert_reservation(
        &mut self,
        name: VarName,
        capability: VariableTypeCapability<'dae>,
        declaration: DaeProvenance,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
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
            role: capability.role(),
            variability: capability.variability(),
            value_type: capability.value_type().index(),
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

#[derive(Clone, Copy)]
enum FunctionConstruction {
    Acyclic,
    Recursive { start: u32, end: u32 },
}

/// Linear lexical authority to define one function.
pub struct FunctionReservation<'function, 'dae> {
    function: FunctionId<'dae>,
    construction: FunctionConstruction,
    marker: PhantomData<&'function mut &'function ()>,
}

impl<'function, 'dae> FunctionReservation<'function, 'dae> {
    const fn new(function: FunctionId<'dae>, construction: FunctionConstruction) -> Self {
        Self {
            function,
            construction,
            marker: PhantomData,
        }
    }

    pub const fn function(&self) -> FunctionId<'dae> {
        self.function
    }
}

/// Non-owning linear capability for one aggregate-owned function body.
pub struct FunctionBody<'dae> {
    function: FunctionId<'dae>,
    construction: FunctionConstruction,
    domain: Option<DomainId<'dae>>,
}

/// Non-owning linear capability for one compact function-loop transition.
pub struct FunctionLoop<'dae> {
    fold: FunctionFoldId<'dae>,
    body: FunctionBody<'dae>,
}

impl<'dae> FunctionLoop<'dae> {
    pub const fn body(&self) -> &FunctionBody<'dae> {
        &self.body
    }

    pub const fn fold(&self) -> FunctionFoldId<'dae> {
        self.fold
    }
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

    fn insert_header(
        &mut self,
        signature: FunctionSignature<'dae>,
    ) -> Result<FunctionId<'dae>, DaeConstructionError> {
        let FunctionSignature {
            name,
            parameters,
            results,
            declaration,
        } = signature;
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
            definitions: Vec::new(),
            folds: Vec::new(),
            declaration,
            definition: None,
            build: None,
        });
        self.storage.unfilled_functions += 1;
        Ok(FunctionId::from_raw(raw))
    }

    pub fn parameter(
        &mut self,
        reservation: &FunctionReservation<'_, 'dae>,
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
            return Err(invalid_arity(
                entry.parameter_values.len(),
                ordinal,
                provenance,
            ));
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
        reservation: &FunctionReservation<'_, 'dae>,
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
            return Err(invalid_arity(
                entry.output_values.len(),
                ordinal,
                provenance,
            ));
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
        reservation: &FunctionReservation<'_, 'dae>,
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
        &mut self,
        reservation: FunctionReservation<'_, 'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionBody<'dae>, DaeConstructionError> {
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
        entry.build = Some(FunctionBuildState {
            current_values: vec![None; entry.values.len()],
            statements: Vec::new(),
            carried_targets: rustc_hash::FxHashSet::default(),
            parent_statements: Vec::new(),
        });
        Ok(FunctionBody {
            function: reservation.function,
            construction: reservation.construction,
            domain: None,
        })
    }

    pub fn read(
        &mut self,
        body: &FunctionBody<'dae>,
        value: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let definition = self.current_definition_id(body, value, provenance)?;
        crate::expression::insert_function_value_use(
            self.source_map,
            self.storage,
            value,
            definition,
            body.domain,
            provenance,
        )
    }

    /// Return the current checked denotation of a function value.
    ///
    /// The body capability proves the function and lexical-domain owner. This
    /// query lets aggregate-preserving transforms correlate constructor-created
    /// loop parameters and outputs without exposing mutable function storage.
    pub fn current_definition(
        &self,
        body: &FunctionBody<'dae>,
        value: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let definition = self.current_definition_id(body, value, provenance)?;
        let rhs = function_definition_rhs(self.storage, value, definition, provenance)?;
        expect_function_body_expression(self.storage, body, rhs, provenance)?;
        Ok(rhs)
    }

    pub fn current_definition_id(
        &self,
        body: &FunctionBody<'dae>,
        value: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(body.function, value, provenance)?;
        function_value_entry(self.storage, value, provenance)?;
        let definition = function_build_state(self.storage, body)
            .current_values
            .get(value.ordinal() as usize)
            .copied()
            .flatten()
            .ok_or(DaeConstructionError::IncompleteDefinition {
                kind: "function value",
                index: value.ordinal(),
                span: provenance.span(),
            })?;
        let definition = FunctionDefinitionId::from_raw(body.function.index(), definition);
        function_definition_rhs(self.storage, value, definition, provenance)?;
        Ok(definition)
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
        self.assign_after_owner_checks(body, target, value, provenance)
    }

    fn assign_after_owner_checks(
        &mut self,
        body: &mut FunctionBody<'dae>,
        target: FunctionValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
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
        let definition = insert_function_definition(self.storage, target, value, provenance)?;
        let build = function_build_state_mut(self.storage, body);
        build.current_values[target.ordinal() as usize] = Some(definition.ordinal());
        build.statements.push(FunctionStatementWire::Assignment {
            definition: definition.ordinal(),
        });
        Ok(())
    }

    pub fn begin_loop(
        &mut self,
        mut parent: FunctionBody<'dae>,
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
            return Err(invalid_arity(1, 0, provenance));
        }
        let mut seen = rustc_hash::FxHashSet::default();
        seen.reserve(targets.len());
        let mut raw_targets = Vec::with_capacity(targets.len());
        for target in &targets {
            check_function_value_owner(parent.function, *target, provenance)?;
            if !seen.insert(target.ordinal()) {
                return Err(DaeConstructionError::DuplicateDefinition {
                    kind: "function loop target",
                    index: target.ordinal(),
                    span: provenance.span(),
                });
            }
            raw_targets.push(target.ordinal());
        }
        let build = function_build_state(self.storage, &parent);
        let initial_values = targets
            .iter()
            .map(|target| {
                build.current_values[target.ordinal() as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function loop initial value",
                        index: target.ordinal(),
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
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
        let build = function_build_state_mut(self.storage, &parent);
        build.parent_statements = std::mem::take(&mut build.statements);
        build.carried_targets = seen;
        parent.domain = Some(domain);
        for (carried, target) in targets.iter().enumerate() {
            let definition = next_function_definition_id(self.storage, parent.function, generated)?;
            let parameter = crate::expression::insert_function_fold_parameter(
                self.source_map,
                self.storage,
                fold,
                carried,
                definition,
                generated,
            )?;
            let inserted = insert_function_definition(self.storage, *target, parameter, generated)?;
            if inserted != definition {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: generated.span(),
                });
            }
            self.storage.function_folds[raw as usize]
                .parameter_definitions
                .push(definition.ordinal());
            function_build_state_mut(self.storage, &parent).current_values
                [target.ordinal() as usize] = Some(definition.ordinal());
        }
        Ok(FunctionLoop { fold, body: parent })
    }

    pub fn assign_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        target: FunctionValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(loop_body.body.function, target, provenance)?;
        let carries_target = function_build_state(self.storage, &loop_body.body)
            .carried_targets
            .contains(&target.ordinal());
        if !carries_target {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function loop target",
                index: target.ordinal(),
                span: provenance.span(),
            });
        }
        self.assign_after_owner_checks(&mut loop_body.body, target, value, provenance)
    }

    pub fn finish_loop(
        &mut self,
        mut loop_body: FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionBody<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = function_fold_raw(self.storage, loop_body.fold, provenance)?;
        let (domain, targets) = {
            let entry = &self.storage.function_folds[raw as usize];
            if !entry.update_definitions.is_empty() {
                return Err(duplicate("function fold", raw, provenance));
            }
            (DomainId::from_raw(entry.domain), entry.targets.clone())
        };
        let Some(found_domain) = loop_body.body.domain else {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function loop domain",
                index: loop_body.fold.ordinal(),
                span: provenance.span(),
            });
        };
        if found_domain != domain {
            return Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: Some(domain.index()),
                found_domain: found_domain.index(),
                span: provenance.span(),
            });
        }
        let build = function_build_state(self.storage, &loop_body.body);
        let updates = targets
            .iter()
            .map(|target| {
                build.current_values[*target as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function loop update",
                        index: *target,
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        for update in &updates {
            let definition =
                FunctionDefinitionId::from_raw(loop_body.body.function.index(), *update);
            let update = ExprId::from_raw(
                function_definition_entry(self.storage, definition, provenance)?.rhs,
            );
            self.storage
                .expect_domain_expression(update, domain, provenance)?;
            match self.storage.expr_function_scope(update, provenance)? {
                None => {}
                Some(function) if function == loop_body.body.function.index() => {}
                Some(function) => {
                    return Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(loop_body.body.function.index()),
                        found_function: function,
                        span: provenance.span(),
                    });
                }
            }
        }
        self.storage.function_folds[raw as usize].update_definitions = updates;
        let generated =
            DaeProvenance::generated(DaeGeneration::FunctionLoopLowering, provenance.span())?;
        for (carried, target) in targets.iter().enumerate() {
            let definition =
                next_function_definition_id(self.storage, loop_body.body.function, generated)?;
            let output = crate::expression::insert_function_fold_output(
                self.source_map,
                self.storage,
                loop_body.fold,
                carried,
                definition,
                generated,
            )?;
            let inserted = insert_function_definition(
                self.storage,
                FunctionValueId::from_raw(loop_body.body.function.index(), *target),
                output,
                generated,
            )?;
            if inserted != definition {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: generated.span(),
                });
            }
            self.storage.function_folds[raw as usize]
                .output_definitions
                .push(definition.ordinal());
            function_build_state_mut(self.storage, &loop_body.body).current_values
                [*target as usize] = Some(definition.ordinal());
        }
        let build = function_build_state_mut(self.storage, &loop_body.body);
        let loop_statements = std::mem::take(&mut build.statements);
        build.statements = std::mem::take(&mut build.parent_statements);
        build.carried_targets.clear();
        build.statements.push(FunctionStatementWire::For {
            fold: loop_body.fold.ordinal(),
            statements: loop_statements,
            provenance,
        });
        loop_body.body.domain = None;
        self.storage.unfilled_function_folds -= 1;
        Ok(loop_body.body)
    }

    pub fn define(
        &mut self,
        body: FunctionBody<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        validate_function_dependencies(self.storage, &body, provenance)?;
        let function = body.function;
        let output_values = self.storage.functions[function.index() as usize]
            .output_values
            .clone();
        let build = function_build_state(self.storage, &body);
        // Checked assignment and loop transitions are the only writers of
        // current values; finalization only has to prove every output is set.
        let results = output_values
            .iter()
            .map(|value| {
                build.current_values[*value as usize].ok_or(
                    DaeConstructionError::IncompleteDefinition {
                        kind: "function output",
                        index: *value,
                        span: provenance.span(),
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let Some(entry) = self.storage.functions.get_mut(function.index() as usize) else {
            return Err(unknown("function", function.index(), provenance));
        };
        if entry.definition.is_some() {
            return Err(duplicate("function", function.index(), provenance));
        }
        let build = entry
            .build
            .take()
            .expect("checked function body remains aggregate-owned until definition");
        entry.definition = Some(FunctionBodyEntry::Modelica(FunctionDefinitionWire {
            statements: build.statements,
            results,
        }));
        self.storage.unfilled_functions -= 1;
        Ok(())
    }

    /// Define one reserved function through its MLS §12.9 external interface.
    ///
    /// The reservation is consumed exactly like a Modelica body definition, so
    /// a function receives one body and never both. Purity, language, symbol,
    /// ordered ABI arguments, produced outputs, and link facts are all checked
    /// here; nothing about the interface is inferred from a rendered name.
    pub fn define_external(
        &mut self,
        reservation: FunctionReservation<'_, 'dae>,
        external: ExternalFunctionBody<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let function = reservation.function;
        let entry = build_external_body(
            self.storage,
            function,
            reservation.construction,
            external,
            provenance,
        )?;
        let Some(stored) = self.storage.functions.get_mut(function.index() as usize) else {
            return Err(unknown("function", function.index(), provenance));
        };
        stored.definition = Some(FunctionBodyEntry::External(entry));
        self.storage.unfilled_functions -= 1;
        Ok(())
    }
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

pub(crate) fn function_definition_rhs<'dae>(
    storage: &Storage,
    value: FunctionValueId<'dae>,
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    if definition.function() != value.function() {
        return Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(value.function().index()),
            found_function: definition.function().index(),
            span: provenance.span(),
        });
    }
    let entry = function_definition_entry(storage, definition, provenance)?;
    if entry.target != value.ordinal() {
        return Err(DaeConstructionError::InvalidFunctionValueRead {
            value: value.ordinal(),
            expected_definition: None,
            found_definition: definition.ordinal(),
            span: provenance.span(),
        });
    }
    Ok(ExprId::from_raw(entry.rhs))
}

pub(crate) fn unknown(kind: &'static str, index: u32, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::UnknownId {
        kind,
        index,
        span: at.span(),
    }
}

pub(crate) fn invalid_arity(
    expected: usize,
    found: usize,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::InvalidArity {
        expected,
        found,
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
