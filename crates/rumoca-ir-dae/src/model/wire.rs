mod equation_systems;
mod expression_wire;
mod function_graph;
mod function_replay;
mod helpers;

use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

use super::*;
use crate::expression::Subscript;
use crate::{
    ClockId, ConditionInput, DaeProvenanceOrigin, DelayId, ExpressionAt, PeriodicClockId,
    PreviousId, PureBuiltin, RelationId, TerminalId, UnaryOperator,
};

use equation_systems::reconstruct_equation_systems;
use expression_wire::*;
use helpers::{
    expect_ordinal, map_expression_operands, map_many, mapped, take_packed, wire_operands,
};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ProvenanceInput {
    origin: DaeProvenanceOrigin,
    span: rumoca_core::Span,
}

fn deserialize_provenance<'de, D>(deserializer: D) -> Result<DaeProvenance, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let input = ProvenanceInput::deserialize(deserializer)?;
    DaeProvenance::try_new(input.origin, input.span).map_err(serde::de::Error::custom)
}

fn deserialize_provenance_vec<'de, D>(deserializer: D) -> Result<Vec<DaeProvenance>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let inputs = Vec::<ProvenanceInput>::deserialize(deserializer)?;
    inputs
        .into_iter()
        .map(|input| {
            DaeProvenance::try_new(input.origin, input.span).map_err(serde::de::Error::custom)
        })
        .collect()
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DaeWire {
    schema_version: u16,
    source_map: SourceMap,
    storage: StorageWire,
}

impl Serialize for FrozenStorage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("DaeStorage", 23)?;
        state.serialize_field(
            "predefined_string_declaration",
            &self.predefined_string_declaration,
        )?;
        state.serialize_field("value_types", &self.value_types)?;
        state.serialize_field("flat_type_ids", &self.flat_type_ids)?;
        state.serialize_field("value_type_provenance", &self.value_type_provenance)?;
        state.serialize_field("variables", &self.variables)?;
        state.serialize_field(
            "functions",
            &function_replay::FunctionArenaOutput::new(&self.functions, &self.function_folds),
        )?;
        state.serialize_field("domains", &self.domains)?;
        state.serialize_field("expressions", &self.expressions)?;
        state.serialize_field(
            "continuous_equation_operations",
            &equation_systems::EquationOperationsOutput::new(
                &self.continuous_equation_owners,
                &self.continuous_equations,
                &self.continuous_families,
                &self.equation_family_bodies,
            ),
        )?;
        state.serialize_field(
            "initialization_equation_operations",
            &equation_systems::EquationOperationsOutput::new(
                &self.initialization_equation_owners,
                &self.initialization_equations,
                &self.initialization_families,
                &self.equation_family_bodies,
            ),
        )?;
        state.serialize_field("initial_discrete_values", &self.initial_discrete_values)?;
        state.serialize_field("discrete_real_equations", &self.discrete_real_equations)?;
        state.serialize_field("discrete_value_owners", &discrete_value_owner_output(self))?;
        state.serialize_field("relations", &self.relations)?;
        state.serialize_field("conditions", &self.conditions)?;
        state.serialize_field("roots", &self.roots)?;
        state.serialize_field("time_events", &self.time_events)?;
        state.serialize_field("event_actions", &self.event_actions)?;
        state.serialize_field("clocks", &self.clocks)?;
        state.serialize_field("clock_ownerships", &self.clock_ownerships)?;
        state.serialize_field("previous_values", &self.previous_values)?;
        state.serialize_field("terminals", &self.terminals)?;
        state.serialize_field("delays", &self.delays)?;
        state.end()
    }
}

/// Private schema-v13 input records.
///
/// These mirror the serialized column names, but they are deliberately
/// distinct from every invariant-bearing arena entry. Deserialization can
/// therefore produce only wire data; the records below enter the IR solely
/// through the same checked operations used by production construction.
///
/// The columns are operation-shaped: each record names the semantic owners,
/// exact provenance, explicit operands, and references one construction
/// operation needs. Facts the operation itself produces — derived types,
/// variabilities, domains, scopes, and generated fold results — are absent,
/// because replay re-issues them through the same checked operation.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StorageWire {
    #[serde(deserialize_with = "deserialize_required_def_id_option")]
    predefined_string_declaration: Option<rumoca_core::DefId>,
    value_types: Vec<ValueTypeWire>,
    flat_type_ids: Vec<Option<rumoca_core::TypeId>>,
    #[serde(deserialize_with = "deserialize_provenance_vec")]
    value_type_provenance: Vec<DaeProvenance>,
    variables: Vec<VariableEntryWire>,
    functions: Vec<FunctionEntryWire>,
    domains: Vec<DomainEntryWire>,
    expressions: ExpressionArenaWire,
    continuous_equation_operations: Vec<EquationOperationInput>,
    initialization_equation_operations: Vec<EquationOperationInput>,
    initial_discrete_values: Vec<InitialDiscreteValueWire>,
    discrete_real_equations: Vec<DiscreteRealEquationWire>,
    discrete_value_owners: Vec<DiscreteValueOwnerWire>,
    relations: Vec<RelationEntryWire>,
    conditions: Vec<ConditionEntryWire>,
    roots: Vec<RootEntryWire>,
    time_events: Vec<TimeEventEntryWire>,
    event_actions: Vec<EventActionEntryWire>,
    clocks: Vec<ClockEntryWire>,
    clock_ownerships: Vec<ClockOwnershipEntryWire>,
    previous_values: Vec<PreviousEntryWire>,
    terminals: Vec<TerminalEntryWire>,
    delays: Vec<DelayEntryWire>,
}

fn deserialize_required_def_id_option<'de, D>(
    deserializer: D,
) -> Result<Option<rumoca_core::DefId>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Option::<rumoca_core::DefId>::deserialize(deserializer)
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ValueTypeWire {
    scalar: ScalarType,
    dimensions: Box<[u32]>,
    record_name: Option<rumoca_core::VarName>,
    record_fields: Box<[RecordFieldTypeWire]>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecordFieldTypeWire {
    name: rumoca_core::VarName,
    value_type: u32,
}

impl ValueTypeWire {
    fn as_primitive_value_type(&self) -> Result<ValueType, DaeConstructionError> {
        if self.scalar == ScalarType::Record
            || self.record_name.is_some()
            || !self.record_fields.is_empty()
        {
            return Err(DaeConstructionError::MalformedWire {
                column: "value_types",
            });
        }
        Ok(ValueType::array(self.scalar, self.dimensions.clone()))
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct VariableEntryWire {
    name: rumoca_core::VarName,
    role: VariableRole,
    variability: ExpressionVariability,
    value_type: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    declaration: DaeProvenance,
    attributes: Option<VariableAttributesInput>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct VariableAttributesInput {
    component_ref: Option<rumoca_core::ComponentReference>,
    binding: Option<u32>,
    start: Option<u32>,
    fixed: Option<bool>,
    min: Option<u32>,
    max: Option<u32>,
    nominal: Option<u32>,
    unit: Option<String>,
    state_select: rumoca_core::StateSelect,
    description: Option<String>,
    causality: VariableCausality,
    is_tunable: bool,
    is_held: bool,
    origin: VariableOrigin,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionEntryWire<Name = rumoca_core::VarName> {
    name: Name,
    parameters: Vec<FunctionNamedValueWire<Name>>,
    outputs: Vec<FunctionNamedValueWire<Name>>,
    locals: Vec<FunctionNamedValueWire<Name>>,
    statements: Vec<FunctionStatementInput>,
    /// MLS §12.9 external interface; mutually exclusive with `statements`.
    external: Option<ExternalBodyInput<Name>>,
    #[serde(deserialize_with = "deserialize_provenance")]
    declaration: DaeProvenance,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ExternalBodyInput<Name = rumoca_core::VarName> {
    purity: FunctionPurity,
    language: ExternalLanguage,
    symbol: Name,
    arguments: Vec<ExternalArgumentEntry>,
    result: Option<u32>,
    linkage: ExternalLinkage,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionNamedValueWire<Name = rumoca_core::VarName> {
    name: Name,
    value_type: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    declaration: DaeProvenance,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum FunctionStatementInput {
    Assignment {
        target: u32,
        rhs: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    For {
        domain: u32,
        targets: Vec<u32>,
        statements: Vec<FunctionStatementInput>,
        #[serde(deserialize_with = "deserialize_provenance")]
        begin_provenance: DaeProvenance,
        #[serde(deserialize_with = "deserialize_provenance")]
        finish_provenance: DaeProvenance,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DomainEntryWire {
    parent: Option<u32>,
    domain: rumoca_core::StructuredIndexDomain,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DiscreteRealEquationWire {
    residual: u32,
    activation: DiscreteRealActivationWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct InitialDiscreteValueWire {
    target: u32,
    value: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum DiscreteRealActivationWire {
    Always,
    When { trigger: u32, guard: u32 },
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum EquationOperationInput {
    Residual {
        residual: u32,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
    Structured {
        domain: u32,
        scalar_view: rumoca_core::ComprehensionScalarView,
        bodies: Vec<u32>,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct DiscreteValueOwnerWire<Targets = Vec<u32>> {
    targets: Targets,
    branches: Vec<DiscreteValueBranchWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct DiscreteValueBranchWire {
    activation: DiscreteBranchActivationWire,
    values: Vec<DiscreteValueActionWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Serialize, Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum DiscreteBranchActivationWire {
    Always,
    When { trigger: u32, guard: u32 },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct DiscreteValueActionWire {
    value: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

fn discrete_value_owner_output(storage: &FrozenStorage) -> Vec<DiscreteValueOwnerWire<&[u32]>> {
    storage
        .discrete_value_owners
        .iter()
        .map(|owner| {
            let targets = &storage.discrete_value_targets[owner.targets.indices()];
            let branches = storage.discrete_value_branches[owner.branches.indices()]
                .iter()
                .map(|branch| {
                    let values = branch
                        .values
                        .indices()
                        .map(|index| DiscreteValueActionWire {
                            value: storage.discrete_value_branch_values[index],
                            provenance: storage.discrete_value_branch_value_provenance[index],
                        })
                        .collect();
                    let activation = match branch.activation {
                        DiscreteBranchActivationEntry::Always => {
                            DiscreteBranchActivationWire::Always
                        }
                        DiscreteBranchActivationEntry::When { trigger, guard } => {
                            DiscreteBranchActivationWire::When { trigger, guard }
                        }
                    };
                    DiscreteValueBranchWire {
                        activation,
                        values,
                        provenance: branch.provenance,
                    }
                })
                .collect();
            DiscreteValueOwnerWire {
                targets,
                branches,
                provenance: owner.provenance,
            }
        })
        .collect()
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum ConditionNodeWire {
    Initial,
    Relation(u32),
    Discrete(u32),
    Clock(u32),
    Not(u32),
    And { lhs: u32, rhs: u32 },
    Or { lhs: u32, rhs: u32 },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RelationEntryWire {
    expression: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ConditionEntryWire {
    node: Option<ConditionNodeWire>,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RootEntryWire {
    relation: u32,
    activation: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TimeEventEntryWire {
    instant: ClockRationalWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum EventActionKindWire {
    Assert { message: u32, level: Option<u32> },
    Terminate { message: u32 },
    Reinitialize { state: u32, value: u32 },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EventActionEntryWire {
    trigger: u32,
    guard: u32,
    kind: EventActionKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum ClockKindWire {
    Periodic(ClockLatticeWire),
    Triggered(u32),
}

#[derive(Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
struct ClockRationalWire {
    num: i128,
    den: i128,
}

impl ClockRationalWire {
    fn checked(
        self,
        at: DaeProvenance,
    ) -> Result<rumoca_core::ClockRational, DaeConstructionError> {
        rumoca_core::ClockRational::new(self.num, self.den).map_err(|source| {
            DaeConstructionError::InvalidClockLattice {
                source,
                span: at.span(),
            }
        })
    }
}

#[derive(Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
struct ClockLatticeWire {
    period: ClockRationalWire,
    phase: ClockRationalWire,
}

impl ClockLatticeWire {
    fn checked(self, at: DaeProvenance) -> Result<rumoca_core::ClockLattice, DaeConstructionError> {
        rumoca_core::ClockLattice::new(self.period.checked(at)?, self.phase.checked(at)?).map_err(
            |source| DaeConstructionError::InvalidClockLattice {
                source,
                span: at.span(),
            },
        )
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ClockEntryWire {
    kind: ClockKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ClockOwnershipEntryWire {
    variable: u32,
    clock: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PreviousEntryWire {
    variable: u32,
    clock: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TerminalEntryWire {
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PositiveParameterWire {
    expression: u32,
    value: f64,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum DelayKindWire {
    ParameterDelay {
        delay_time: PositiveParameterWire,
    },
    BoundedDelay {
        delay_time: u32,
        delay_max: PositiveParameterWire,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DelayEntryWire {
    source: u32,
    kind: DelayKindWire,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

impl<'de> Deserialize<'de> for Dae {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = DaeWire::deserialize(deserializer)?;
        if wire.schema_version != DAE_SCHEMA_VERSION {
            return Err(serde::de::Error::custom(
                DaeConstructionError::InvalidSchemaVersion {
                    expected: DAE_SCHEMA_VERSION,
                    found: wire.schema_version,
                },
            ));
        }
        Dae::construct(wire.source_map, |dae| reconstruct(&wire.storage, dae))
            .map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Copy)]
enum WireClockId<'dae> {
    Periodic(PeriodicClockId<'dae>),
    Triggered(ClockId<'dae>),
}

impl<'dae> WireClockId<'dae> {
    fn clock_id(self) -> ClockId<'dae> {
        match self {
            Self::Periodic(clock) => clock.into(),
            Self::Triggered(clock) => clock,
        }
    }

    fn periodic(self, at: DaeProvenance) -> Result<PeriodicClockId<'dae>, DaeConstructionError> {
        match self {
            Self::Periodic(clock) => Ok(clock),
            Self::Triggered(clock) => Err(unknown("periodic clock", clock.index(), at)),
        }
    }
}

struct WireIds<'dae> {
    types: Vec<ValueTypeId<'dae>>,
    variables: Vec<VariableId<'dae>>,
    functions: Vec<FunctionId<'dae>>,
    domains: Vec<DomainId<'dae>>,
    conditions: Vec<ConditionId<'dae>>,
    relations: Vec<RelationId<'dae>>,
    clocks: Vec<WireClockId<'dae>>,
    previous_values: Vec<PreviousId<'dae>>,
    terminals: Vec<TerminalId<'dae>>,
    delays: Vec<DelayId<'dae>>,
    expressions: Vec<ExprId<'dae>>,
    next_operand: usize,
    next_subscript: usize,
}

fn mapped_expression<'dae>(
    ids: &WireIds<'dae>,
    raw: u32,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    mapped(&ids.expressions, raw, "expression", provenance)
}

fn reconstruct<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
) -> Result<(), DaeConstructionError> {
    if let Some(declaration) = wire.predefined_string_declaration {
        dae.register_predefined_string(declaration)?;
    }
    let types = reconstruct_types(wire, dae)?;
    let (variables, variable_reservations) = reconstruct_variables(wire, dae, &types)?;
    let domains = reconstruct_domains(wire, dae)?;
    let conditions = reconstruct_conditions(wire, dae)?;
    let mut ids = WireIds {
        types,
        variables,
        functions: Vec::with_capacity(wire.functions.len()),
        domains,
        conditions,
        relations: Vec::with_capacity(wire.relations.len()),
        clocks: Vec::with_capacity(wire.clocks.len()),
        previous_values: Vec::with_capacity(wire.previous_values.len()),
        terminals: Vec::with_capacity(wire.terminals.len()),
        delays: Vec::with_capacity(wire.delays.len()),
        expressions: Vec::with_capacity(wire.expressions.nodes.len()),
        next_operand: 0,
        next_subscript: 0,
    };
    reconstruct_clocks(wire, dae, &mut ids)?;
    reconstruct_temporal(wire, dae, &mut ids)?;
    function_replay::reconstruct(wire, dae, &mut ids)?;
    reconstruct_relations(wire, dae, &mut ids)?;
    define_variables(wire, dae, &ids, variable_reservations)?;
    define_conditions(wire, dae, &ids)?;
    reconstruct_roots(wire, dae, &ids)?;
    reconstruct_events(wire, dae, &ids)?;
    reconstruct_equation_systems(wire, dae, &ids)?;
    reconstruct_initial_discrete_values(wire, dae, &ids)?;
    reconstruct_discrete_value_owners(wire, dae, &ids)
}

/// Replay every MLS §8.6 discrete initial-value definition through the same
/// checked initialization owner production uses.
fn reconstruct_initial_discrete_values<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, definition) in wire.initial_discrete_values.iter().enumerate() {
        let at = definition.provenance;
        let variable = mapped(&ids.variables, definition.target, "variable", at)?;
        let value = mapped_expression(ids, definition.value, at)?;
        let role = dae.storage.variables[variable.index() as usize].role;
        let id = dae.initialization(|initialization| match role {
            VariableRole::DiscreteReal => initialization.discrete_real_initial_value(
                DiscreteRealId::from_raw(variable.index()),
                value,
                at,
            ),
            VariableRole::DiscreteValue => initialization.discrete_value_initial_value(
                DiscreteValueId::from_raw(variable.index()),
                value,
                at,
            ),
            _ => Err(DaeConstructionError::InvalidVariableRole {
                name: ownership_variable_name(wire, definition.target, at)?,
                span: at.span(),
            }),
        })?;
        expect_ordinal("discrete initial value", index, id.index(), at)?;
    }
    Ok(())
}

fn reconstruct_types<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
) -> Result<Vec<ValueTypeId<'dae>>, DaeConstructionError> {
    if wire.flat_type_ids.len() != wire.value_types.len()
        || wire.value_type_provenance.len() != wire.value_types.len()
    {
        return Err(malformed("value_types"));
    }
    let mut ids = Vec::with_capacity(wire.value_types.len());
    for (index, ((flat_type, ty), provenance)) in wire
        .flat_type_ids
        .iter()
        .zip(&wire.value_types)
        .zip(&wire.value_type_provenance)
        .enumerate()
    {
        let id = if ty.scalar == ScalarType::Record {
            if flat_type.is_some() || !ty.dimensions.is_empty() {
                return Err(DaeConstructionError::MalformedWire {
                    column: "value_types",
                });
            }
            let name = ty
                .record_name
                .clone()
                .ok_or(DaeConstructionError::MalformedWire {
                    column: "value_types.record_name",
                })?;
            let fields = ty
                .record_fields
                .iter()
                .map(|field| {
                    Ok((
                        field.name.clone(),
                        *ids.get(field.value_type as usize).ok_or(
                            DaeConstructionError::MalformedWire {
                                column: "value_types.record_fields",
                            },
                        )?,
                    ))
                })
                .collect::<Result<Vec<_>, DaeConstructionError>>()?;
            dae.types(|types| types.record(name, fields, *provenance))?
        } else {
            let value_type = ty.as_primitive_value_type()?;
            dae.types(|types| match flat_type {
                Some(flat_type) => types.intern(*flat_type, value_type, *provenance),
                None => types.derived(value_type, *provenance),
            })?
        };
        expect_ordinal("value type", index, id.index(), *provenance)?;
        ids.push(id);
    }
    Ok(ids)
}

fn reconstruct_variables<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
) -> Result<(Vec<VariableId<'dae>>, Vec<VariableReservation<'dae>>), DaeConstructionError> {
    let mut ids = Vec::with_capacity(wire.variables.len());
    let mut reservations = Vec::with_capacity(wire.variables.len());
    for variable in &wire.variables {
        let ty = mapped(
            types,
            variable.value_type,
            "value type",
            variable.declaration,
        )?;
        let (id, reservation) =
            dae.variables(|variables| reserve_wire_variable(variables, variable, ty))?;
        if dae.storage.variables[id.index() as usize].variability != variable.variability {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: variable.declaration.span(),
            });
        }
        ids.push(id);
        reservations.push(reservation);
    }
    Ok((ids, reservations))
}

fn reserve_wire_variable<'dae>(
    variables: &mut Variables<'_, 'dae>,
    variable: &VariableEntryWire,
    ty: ValueTypeId<'dae>,
) -> Result<(VariableId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
    let name = variable.name.clone();
    let declaration = variable.declaration;
    let pair = match variable.role {
        VariableRole::Parameter => {
            let (id, reservation) = variables.reserve_parameter(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
        VariableRole::Constant => {
            let (id, reservation) = variables.reserve_constant(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
        VariableRole::Input => reserve_wire_input(variables, variable, ty)?,
        VariableRole::State => {
            let (id, reservation) = variables.reserve_state(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
        VariableRole::Algebraic => {
            let (id, reservation) = variables.reserve_algebraic(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
        VariableRole::Output => {
            let (id, reservation) = variables.reserve_output(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
        VariableRole::DiscreteReal => {
            let (id, reservation) = variables.reserve_discrete_real(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
        VariableRole::DiscreteValue => {
            let (id, reservation) = variables.reserve_discrete_value(name, ty, declaration)?;
            (VariableId::from_raw(id.index()), reservation)
        }
    };
    Ok(pair)
}

fn reserve_wire_input<'dae>(
    variables: &mut Variables<'_, 'dae>,
    variable: &VariableEntryWire,
    ty: ValueTypeId<'dae>,
) -> Result<(VariableId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
    let variability = match variable.variability {
        ExpressionVariability::Discrete => InputVariability::Discrete,
        ExpressionVariability::Continuous => InputVariability::Continuous,
        ExpressionVariability::Constant | ExpressionVariability::Parameter => {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: variable.declaration.span(),
            });
        }
    };
    let (id, reservation) =
        variables.reserve_input(variable.name.clone(), ty, variability, variable.declaration)?;
    Ok((VariableId::from_raw(id.index()), reservation))
}

fn map_function_value_types<'dae>(
    types: &[ValueTypeId<'dae>],
    values: &[FunctionNamedValueWire],
) -> Result<Vec<ValueTypeId<'dae>>, DaeConstructionError> {
    values
        .iter()
        .map(|value| mapped(types, value.value_type, "value type", value.declaration))
        .collect()
}

fn reconstruct_function_values<'dae>(
    function: &FunctionEntryWire,
    dae: &mut DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
    reservation: &FunctionReservation<'_, 'dae>,
) -> Result<(), DaeConstructionError> {
    for (ordinal, parameter) in function.parameters.iter().enumerate() {
        let rebuilt = dae.functions(|functions| {
            functions.parameter(
                reservation,
                parameter.name.clone(),
                ordinal,
                parameter.declaration,
            )
        })?;
        expect_ordinal(
            "function parameter",
            ordinal,
            rebuilt.ordinal(),
            parameter.declaration,
        )?;
    }
    for (ordinal, value) in function.outputs.iter().enumerate() {
        let rebuilt = dae.functions(|functions| {
            functions.output(reservation, value.name.clone(), ordinal, value.declaration)
        })?;
        expect_ordinal(
            "function value",
            ordinal,
            rebuilt.ordinal(),
            value.declaration,
        )?;
    }
    let output_count = function.outputs.len();
    for (local, value) in function.locals.iter().enumerate() {
        let value_type = mapped(types, value.value_type, "value type", value.declaration)?;
        let rebuilt = dae.functions(|functions| {
            functions.local(
                reservation,
                value.name.clone(),
                value_type,
                value.declaration,
            )
        })?;
        expect_ordinal(
            "function value",
            output_count + local,
            rebuilt.ordinal(),
            value.declaration,
        )?;
    }
    Ok(())
}

fn reconstruct_domains<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
) -> Result<Vec<DomainId<'dae>>, DaeConstructionError> {
    let mut ids = Vec::with_capacity(wire.domains.len());
    for (index, domain) in wire.domains.iter().enumerate() {
        let parent = domain
            .parent
            .map(|parent| mapped(&ids, parent, "domain", domain.provenance))
            .transpose()?;
        let id = dae.domains(|domains| match parent {
            Some(parent) => domains.nested(parent, domain.domain.clone(), domain.provenance),
            None => domains.structured(domain.domain.clone(), domain.provenance),
        })?;
        expect_ordinal("domain", index, id.index(), domain.provenance)?;
        ids.push(id);
    }
    Ok(ids)
}

fn reconstruct_conditions<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
) -> Result<Vec<ConditionId<'dae>>, DaeConstructionError> {
    wire.conditions
        .iter()
        .map(|condition| dae.conditions(|conditions| conditions.reserve(condition.provenance)))
        .collect()
}

fn reconstruct_next_expression<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<bool, DaeConstructionError> {
    let index = ids.expressions.len();
    let raw = index as u32;
    let expression = wire_expression(wire, index)?;
    let provenance = expression.provenance;
    let node = expression.node;
    let id = match node {
        ExprNodeWire::Coordinate(CoordinateWire::Delay(delay)) => {
            reconstruct_delay_coordinate(wire, dae, ids, *delay, provenance)?
        }
        ExprNodeWire::FunctionValue { .. }
        | ExprNodeWire::FunctionFoldParameter { .. }
        | ExprNodeWire::FunctionFoldOutput { .. } => {
            return Ok(false);
        }
        _ => dae.expressions(|expressions| {
            rebuild_node(wire, ids, expressions.at(provenance), node, provenance)
        })?,
    };
    if id.index() != raw {
        return Err(DaeConstructionError::MalformedWire {
            column: "expressions.nodes",
        });
    }
    ids.expressions.push(id);
    Ok(true)
}

fn expect_expression_arena_consumed(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
    ids: &WireIds<'_>,
) -> Result<(), DaeConstructionError> {
    let count = wire.expressions.nodes.len();
    if wire.expressions.provenance.len() != count
        || ids.next_operand != wire.expressions.operands.len()
        || ids.next_subscript != wire.expressions.subscripts.len()
        || ids.delays.len() != wire.delays.len()
        || dae.storage.expressions.nodes.len() != count
        || dae.storage.expressions.operands.len() != wire.expressions.operands.len()
        || dae.storage.expressions.subscripts.len() != wire.expressions.subscripts.len()
    {
        return Err(malformed("expressions"));
    }
    Ok(())
}

fn reconstruct_delay_coordinate<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    target: u32,
    coordinate_provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let index = ids.delays.len();
    if target as usize != index {
        return Err(DaeConstructionError::MalformedWire {
            column: "delay coordinate order",
        });
    }
    let delay = wire
        .delays
        .get(index)
        .ok_or_else(|| unknown("delay", target, coordinate_provenance))?;
    let source = mapped(
        &ids.expressions,
        delay.source,
        "expression",
        delay.provenance,
    )?;
    let coordinate = match &delay.kind {
        DelayKindWire::ParameterDelay {
            delay_time: evidence,
        } => {
            let expression = mapped(
                &ids.expressions,
                evidence.expression,
                "expression",
                evidence.provenance,
            )?;
            let positive = dae.temporal(|temporal| {
                temporal.positive_parameter(expression, evidence.value, evidence.provenance)
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(coordinate_provenance)
                    .delay(source, positive, delay.provenance)
            })?
        }
        DelayKindWire::BoundedDelay {
            delay_time,
            delay_max: maximum,
        } => {
            let delay_time = mapped(
                &ids.expressions,
                *delay_time,
                "expression",
                delay.provenance,
            )?;
            let maximum_expression = mapped(
                &ids.expressions,
                maximum.expression,
                "expression",
                maximum.provenance,
            )?;
            let maximum = dae.temporal(|temporal| {
                temporal.positive_parameter(maximum_expression, maximum.value, maximum.provenance)
            })?;
            dae.expressions(|expressions| {
                expressions.at(coordinate_provenance).bounded_delay(
                    source,
                    delay_time,
                    maximum,
                    delay.provenance,
                )
            })?
        }
    };
    expect_ordinal("delay", index, coordinate.id().index(), delay.provenance)?;
    ids.delays.push(coordinate.id());
    Ok(coordinate.expression())
}

fn rebuild_node<'dae>(
    wire: &StorageWire,
    ids: &mut WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    node: &ExprNodeWire,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    match node {
        ExprNodeWire::Literal(DaeLiteralWire::Enumeration(ordinal)) => {
            at.enumeration_literal(*ordinal)
        }
        ExprNodeWire::Literal(value) => at.literal(value.as_literal()),
        ExprNodeWire::Coordinate(CoordinateWire::Binder { domain, ordinal }) => {
            let domain = mapped(&ids.domains, *domain, "domain", provenance)?;
            at.binder(DomainBinderId::from_raw(domain.index(), *ordinal))
        }
        ExprNodeWire::Coordinate(coordinate) => {
            at.coordinate(rebuild_coordinate(ids, coordinate, provenance)?)
        }
        ExprNodeWire::Unary { operator, operand } => at.unary(
            *operator,
            mapped(&ids.expressions, *operand, "expression", provenance)?,
        ),
        ExprNodeWire::Binary { operator, lhs, rhs } => at.binary(
            *operator,
            mapped(&ids.expressions, *lhs, "expression", provenance)?,
            mapped(&ids.expressions, *rhs, "expression", provenance)?,
        ),
        ExprNodeWire::Conditional { operand_count } => {
            rebuild_conditional(wire, ids, at, *operand_count, provenance)
        }
        ExprNodeWire::Array {
            operand_count,
            value_type,
        } => rebuild_array(wire, ids, at, *operand_count, *value_type, provenance),
        ExprNodeWire::Record {
            operand_count,
            value_type,
        } => at.record(
            mapped(&ids.types, *value_type, "value type", provenance)?,
            map_expression_operands(wire, ids, *operand_count, provenance)?,
        ),
        ExprNodeWire::Field { base, field } => at.field(
            mapped(&ids.expressions, *base, "expression", provenance)?,
            *field as usize,
        ),
        ExprNodeWire::Range {
            start_expression,
            explicit_step_expression,
            stop_expression,
        } => rebuild_range(
            ids,
            at,
            *start_expression,
            *explicit_step_expression,
            *stop_expression,
            provenance,
        ),
        ExprNodeWire::Comprehension { domain, body } => at.comprehension(
            mapped(&ids.domains, *domain, "domain", provenance)?,
            mapped(&ids.expressions, *body, "expression", provenance)?,
        ),
        ExprNodeWire::Index {
            base,
            subscript_count,
        } => rebuild_index(wire, ids, at, *base, *subscript_count, provenance),
        ExprNodeWire::ArrayUpdate {
            base,
            value,
            subscript_count,
        } => at.array_update(
            mapped(&ids.expressions, *base, "expression", provenance)?,
            mapped(&ids.expressions, *value, "expression", provenance)?,
            rebuild_subscripts(wire, ids, *subscript_count, provenance)?,
        ),
        ExprNodeWire::Builtin {
            builtin,
            operand_count,
        } => at.builtin(
            *builtin,
            map_expression_operands(wire, ids, *operand_count, provenance)?,
        ),
        ExprNodeWire::Call {
            function,
            output,
            operand_count,
        } => at.call(
            mapped(&ids.functions, *function, "function", provenance)?,
            *output as usize,
            map_expression_operands(wire, ids, *operand_count, provenance)?,
        ),
        node @ ExprNodeWire::StringConversion { .. } => {
            rebuild_string_conversion(ids, at, WireStringConversion::from_node(node), provenance)
        }
        ExprNodeWire::FunctionValue { .. } => Err(malformed("expressions.nodes.function_value")),
        ExprNodeWire::FunctionFoldParameter { .. } | ExprNodeWire::FunctionFoldOutput { .. } => {
            Err(malformed("expressions.nodes.function_fold"))
        }
    }
}

fn rebuild_range<'dae>(
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    start: u32,
    step: Option<u32>,
    stop: u32,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    at.range(
        mapped(&ids.expressions, start, "expression", provenance)?,
        step.map(|step| mapped(&ids.expressions, step, "expression", provenance))
            .transpose()?,
        mapped(&ids.expressions, stop, "expression", provenance)?,
    )
}

/// Rebuild one array node, requiring the element type exactly when it is an
/// operand: an empty array carries the type it was constructed with, and a
/// populated array infers it from those operands and cannot restate it.
fn rebuild_array<'dae>(
    wire: &StorageWire,
    ids: &mut WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    operands: u32,
    value_type: Option<u32>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let operands = map_expression_operands(wire, ids, operands, provenance)?;
    match (operands.is_empty(), value_type) {
        (true, Some(value_type)) => {
            at.empty_array(mapped(&ids.types, value_type, "value type", provenance)?)
        }
        (false, None) => at.array(operands),
        _ => Err(malformed("expressions.nodes.array.value_type")),
    }
}

#[derive(Clone, Copy)]
struct WireStringConversion {
    declaration: rumoca_core::DefId,
    value: u32,
    minimum_length: Option<u32>,
    left_justified: Option<u32>,
    significant_digits: Option<u32>,
    format: Option<u32>,
}

impl WireStringConversion {
    fn from_node(node: &ExprNodeWire) -> Self {
        let ExprNodeWire::StringConversion {
            declaration,
            value,
            minimum_length,
            left_justified,
            significant_digits,
            format,
        } = node
        else {
            unreachable!("caller selected a String conversion wire node")
        };
        Self {
            declaration: *declaration,
            value: *value,
            minimum_length: *minimum_length,
            left_justified: *left_justified,
            significant_digits: *significant_digits,
            format: *format,
        }
    }
}

fn rebuild_string_conversion<'dae>(
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    wire: WireStringConversion,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let value = mapped(&ids.expressions, wire.value, "expression", provenance)?;
    let format = match wire.format {
        Some(format) => crate::StringConversionFormatInput::Format {
            value: mapped(&ids.expressions, format, "expression", provenance)?,
        },
        None => crate::StringConversionFormatInput::Options {
            minimum_length: map_optional_expression(ids, wire.minimum_length, provenance)?,
            left_justified: map_optional_expression(ids, wire.left_justified, provenance)?,
            significant_digits: map_optional_expression(ids, wire.significant_digits, provenance)?,
        },
    };
    at.string_conversion(wire.declaration, value, format)
}

fn map_optional_expression<'dae>(
    ids: &WireIds<'dae>,
    expression: Option<u32>,
    provenance: DaeProvenance,
) -> Result<Option<ExprId<'dae>>, DaeConstructionError> {
    expression
        .map(|value| mapped(&ids.expressions, value, "expression", provenance))
        .transpose()
}

fn rebuild_coordinate<'dae>(
    ids: &WireIds<'dae>,
    coordinate: &CoordinateWire,
    at: DaeProvenance,
) -> Result<CoordinateInput<'dae>, DaeConstructionError> {
    Ok(match coordinate {
        CoordinateWire::Parameter(variable) => CoordinateInput::Parameter(ParameterId::from_raw(
            mapped(&ids.variables, *variable, "variable", at)?.index(),
        )),
        CoordinateWire::Input(variable) => CoordinateInput::Input(InputId::from_raw(
            mapped(&ids.variables, *variable, "variable", at)?.index(),
        )),
        CoordinateWire::State(variable) => CoordinateInput::State(StateId::from_raw(
            mapped(&ids.variables, *variable, "variable", at)?.index(),
        )),
        CoordinateWire::Derivative(variable) => CoordinateInput::Derivative(StateId::from_raw(
            mapped(&ids.variables, *variable, "variable", at)?.index(),
        )),
        CoordinateWire::Algebraic(variable) => CoordinateInput::Algebraic(AlgebraicId::from_raw(
            mapped(&ids.variables, *variable, "variable", at)?.index(),
        )),
        CoordinateWire::DiscreteReal(variable) => CoordinateInput::DiscreteReal(
            DiscreteRealId::from_raw(mapped(&ids.variables, *variable, "variable", at)?.index()),
        ),
        CoordinateWire::DiscreteValue(variable) => CoordinateInput::DiscreteValue(
            DiscreteValueId::from_raw(mapped(&ids.variables, *variable, "variable", at)?.index()),
        ),
        CoordinateWire::PreDiscreteReal(variable) => CoordinateInput::PreDiscreteReal(
            DiscreteRealId::from_raw(mapped(&ids.variables, *variable, "variable", at)?.index()),
        ),
        CoordinateWire::PreDiscreteValue(variable) => CoordinateInput::PreDiscreteValue(
            DiscreteValueId::from_raw(mapped(&ids.variables, *variable, "variable", at)?.index()),
        ),
        CoordinateWire::Time => CoordinateInput::Time,
        CoordinateWire::ClockInterval(clock) => {
            CoordinateInput::ClockInterval(mapped(&ids.clocks, *clock, "clock", at)?.periodic(at)?)
        }
        CoordinateWire::Condition(condition) => {
            CoordinateInput::Condition(mapped(&ids.conditions, *condition, "condition", at)?)
        }
        CoordinateWire::Delay(_) => {
            return Err(malformed("expressions.nodes.delay"));
        }
        CoordinateWire::Previous(previous) => CoordinateInput::Previous(mapped(
            &ids.previous_values,
            *previous,
            "previous value",
            at,
        )?),
        CoordinateWire::Terminal(terminal) => CoordinateInput::Terminal(mapped(
            &ids.terminals,
            *terminal,
            "terminal coordinate",
            at,
        )?),
        CoordinateWire::Binder { .. } => {
            return Err(malformed("expressions.nodes.binder"));
        }
        CoordinateWire::FunctionParameter { function, ordinal } => {
            let function = mapped(&ids.functions, *function, "function", at)?;
            CoordinateInput::FunctionParameter(FunctionParameterId::from_raw(
                function.index(),
                *ordinal,
            ))
        }
    })
}

fn rebuild_conditional<'dae>(
    wire: &StorageWire,
    ids: &mut WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    count: u32,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let operands = wire_operands(wire, &mut ids.next_operand, count)?;
    let Some((&fallback, branch_operands)) = operands.split_last() else {
        return Err(invalid_arity(1, 0, provenance));
    };
    if branch_operands.len() % 2 != 0 {
        return Err(invalid_arity(
            branch_operands.len() + 1,
            operands.len(),
            provenance,
        ));
    }
    let branches = branch_operands
        .chunks_exact(2)
        .map(|pair| {
            Ok((
                mapped(&ids.expressions, pair[0], "expression", provenance)?,
                mapped(&ids.expressions, pair[1], "expression", provenance)?,
            ))
        })
        .collect::<Result<Vec<_>, DaeConstructionError>>()?;
    at.conditional(
        branches,
        mapped(&ids.expressions, fallback, "expression", provenance)?,
    )
}

fn rebuild_index<'dae>(
    wire: &StorageWire,
    ids: &mut WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    base: u32,
    count: u32,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let subscripts = rebuild_subscripts(wire, ids, count, provenance)?;
    at.index(
        mapped(&ids.expressions, base, "expression", provenance)?,
        subscripts,
    )
}

fn rebuild_subscripts<'dae>(
    wire: &StorageWire,
    ids: &mut WireIds<'dae>,
    count: u32,
    provenance: DaeProvenance,
) -> Result<Vec<Subscript<'dae>>, DaeConstructionError> {
    let packed = take_packed(
        &wire.expressions.subscripts,
        &mut ids.next_subscript,
        count,
        "expressions.subscripts",
    )?;
    packed
        .iter()
        .map(|subscript| rebuild_subscript(ids, subscript, provenance))
        .collect()
}

fn rebuild_subscript<'dae>(
    ids: &WireIds<'dae>,
    subscript: &PackedSubscriptWire,
    at: DaeProvenance,
) -> Result<Subscript<'dae>, DaeConstructionError> {
    Ok(match subscript.kind {
        PackedSubscriptKindWire::Index(expression) => Subscript::Index {
            expression: mapped(&ids.expressions, expression, "expression", at)?,
            provenance: subscript.provenance,
        },
        PackedSubscriptKindWire::Whole => Subscript::Whole {
            provenance: subscript.provenance,
        },
        PackedSubscriptKindWire::Slice(expression) => Subscript::Slice {
            expression: mapped(&ids.expressions, expression, "expression", at)?,
            provenance: subscript.provenance,
        },
    })
}

fn define_variables<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    reservations: Vec<VariableReservation<'dae>>,
) -> Result<(), DaeConstructionError> {
    for (index, (variable, reservation)) in wire.variables.iter().zip(reservations).enumerate() {
        let Some(attributes) = &variable.attributes else {
            return Err(incomplete("variable", index, variable.declaration));
        };
        let mapped_expression =
            |raw| mapped(&ids.expressions, raw, "expression", variable.declaration);
        let attributes = VariableAttributes {
            component_ref: attributes.component_ref.clone(),
            binding: attributes.binding.map(mapped_expression).transpose()?,
            start: attributes.start.map(mapped_expression).transpose()?,
            fixed: attributes.fixed,
            min: attributes.min.map(mapped_expression).transpose()?,
            max: attributes.max.map(mapped_expression).transpose()?,
            nominal: attributes.nominal.map(mapped_expression).transpose()?,
            unit: attributes.unit.clone(),
            state_select: attributes.state_select,
            description: attributes.description.clone(),
            causality: attributes.causality,
            is_tunable: attributes.is_tunable,
            is_held: attributes.is_held,
            origin: attributes.origin,
        };
        dae.variables(|variables| variables.define(reservation, attributes, variable.declaration))?;
    }
    Ok(())
}

fn define_conditions<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, condition) in wire.conditions.iter().enumerate() {
        let Some(node) = condition.node else {
            return Err(incomplete("condition", index, condition.provenance));
        };
        let input = rebuild_condition_input(ids, node, condition.provenance)?;
        dae.conditions(|conditions| {
            conditions.define(ids.conditions[index], input, condition.provenance)
        })?;
    }
    Ok(())
}

fn reconstruct_relations<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, relation) in wire.relations.iter().enumerate() {
        let expression = mapped(
            &ids.expressions,
            relation.expression,
            "expression",
            relation.provenance,
        )?;
        let id =
            dae.conditions(|conditions| conditions.relation(expression, relation.provenance))?;
        expect_ordinal("relation", index, id.index(), relation.provenance)?;
        ids.relations.push(id);
    }
    Ok(())
}

fn rebuild_condition_input<'dae>(
    ids: &WireIds<'dae>,
    node: ConditionNodeWire,
    at: DaeProvenance,
) -> Result<ConditionInput<'dae>, DaeConstructionError> {
    Ok(match node {
        ConditionNodeWire::Initial => ConditionInput::Initial,
        ConditionNodeWire::Relation(raw) => {
            ConditionInput::Relation(mapped(&ids.relations, raw, "relation", at)?)
        }
        ConditionNodeWire::Discrete(raw) => {
            ConditionInput::Discrete(mapped(&ids.expressions, raw, "expression", at)?)
        }
        ConditionNodeWire::Clock(raw) => {
            ConditionInput::Clock(mapped(&ids.clocks, raw, "clock", at)?.clock_id())
        }
        ConditionNodeWire::Not(raw) => {
            ConditionInput::Not(mapped(&ids.conditions, raw, "condition", at)?)
        }
        ConditionNodeWire::And { lhs, rhs } => ConditionInput::And(
            mapped(&ids.conditions, lhs, "condition", at)?,
            mapped(&ids.conditions, rhs, "condition", at)?,
        ),
        ConditionNodeWire::Or { lhs, rhs } => ConditionInput::Or(
            mapped(&ids.conditions, lhs, "condition", at)?,
            mapped(&ids.conditions, rhs, "condition", at)?,
        ),
    })
}

fn reconstruct_roots<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, root) in wire.roots.iter().enumerate() {
        let relation = mapped(&ids.relations, root.relation, "relation", root.provenance)?;
        let activation = mapped(
            &ids.conditions,
            root.activation,
            "condition",
            root.provenance,
        )?;
        let id =
            dae.conditions(|conditions| conditions.root(relation, activation, root.provenance))?;
        expect_ordinal("root", index, id.index(), root.provenance)?;
    }
    Ok(())
}

fn reconstruct_events<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, event) in wire.time_events.iter().enumerate() {
        let instant = event.instant.checked(event.provenance)?;
        let id = dae.events(|events| events.time_event(instant, event.provenance))?;
        expect_ordinal("time event", index, id.index(), event.provenance)?;
    }
    for (index, action) in wire.event_actions.iter().enumerate() {
        let guard = mapped(
            &ids.conditions,
            action.guard,
            "condition",
            action.provenance,
        )?;
        let trigger = mapped(
            &ids.conditions,
            action.trigger,
            "condition",
            action.provenance,
        )?;
        let id = dae.events(|events| match action.kind {
            EventActionKindWire::Assert { message, level } => events.assert_with_level(
                trigger,
                guard,
                mapped(&ids.expressions, message, "expression", action.provenance)?,
                level
                    .map(|level| mapped(&ids.expressions, level, "expression", action.provenance))
                    .transpose()?,
                action.provenance,
            ),
            EventActionKindWire::Terminate { message } => events.terminate(
                trigger,
                guard,
                mapped(&ids.expressions, message, "expression", action.provenance)?,
                action.provenance,
            ),
            EventActionKindWire::Reinitialize { state, value } => {
                let state = mapped(&ids.variables, state, "variable", action.provenance)?;
                events.reinitialize(
                    trigger,
                    guard,
                    StateId::from_raw(state.index()),
                    mapped(&ids.expressions, value, "expression", action.provenance)?,
                    action.provenance,
                )
            }
        })?;
        expect_ordinal("event action", index, id.index(), action.provenance)?;
    }
    Ok(())
}

fn reconstruct_discrete_value_owners<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    let plan = wire
        .discrete_value_owners
        .iter()
        .flat_map(|owner| {
            owner
                .targets
                .iter()
                .copied()
                .map(move |target| (target, owner.provenance))
        })
        .map(|(target, provenance)| {
            let variable = mapped(&ids.variables, target, "variable", provenance)?;
            Ok(DiscreteValueId::from_raw(variable.index()))
        })
        .collect::<Result<Vec<_>, DaeConstructionError>>()?;
    dae.b1c(plan, |topology| {
        for (ordinal, owner) in wire.discrete_value_owners.iter().enumerate() {
            let rebuilt = replay_discrete_value_owner(owner, topology, ids)?;
            expect_ordinal("B.1c owner", ordinal, rebuilt.index(), owner.provenance)?;
        }
        Ok(())
    })
}

fn replay_discrete_value_owner<'dae>(
    owner: &DiscreteValueOwnerWire,
    topology: &mut crate::DiscreteValueTopology<'_, 'dae>,
    ids: &WireIds<'dae>,
) -> Result<crate::DiscreteValueOwnerId<'dae>, DaeConstructionError> {
    let targets = owner
        .targets
        .iter()
        .map(|&target| {
            let variable = mapped(&ids.variables, target, "variable", owner.provenance)?;
            Ok(DiscreteValueId::from_raw(variable.index()))
        })
        .collect::<Result<Vec<_>, DaeConstructionError>>()?;
    topology.owner(owner.provenance, targets, |staged| {
        for branch in &owner.branches {
            replay_discrete_value_branch(branch, staged, ids)?;
        }
        Ok(())
    })
}

fn replay_discrete_value_branch<'dae>(
    branch: &DiscreteValueBranchWire,
    staged: &mut crate::DiscreteValueOwner<'_, 'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    let values = branch
        .values
        .iter()
        .map(|action| {
            Ok((
                mapped(
                    &ids.expressions,
                    action.value,
                    "expression",
                    action.provenance,
                )?,
                action.provenance,
            ))
        })
        .collect::<Result<Vec<_>, DaeConstructionError>>()?;
    match branch.activation {
        DiscreteBranchActivationWire::Always => staged.always(branch.provenance, values),
        DiscreteBranchActivationWire::When { trigger, guard } => staged.when(
            mapped(&ids.conditions, trigger, "condition", branch.provenance)?,
            mapped(&ids.conditions, guard, "condition", branch.provenance)?,
            branch.provenance,
            values,
        ),
    }
}

fn reconstruct_clocks<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, clock) in wire.clocks.iter().enumerate() {
        let id = dae.clocks(|clocks| match clock.kind {
            ClockKindWire::Periodic(lattice) => clocks
                .periodic(lattice.checked(clock.provenance)?, clock.provenance)
                .map(WireClockId::Periodic),
            ClockKindWire::Triggered(condition) => clocks
                .triggered(
                    mapped(&ids.conditions, condition, "condition", clock.provenance)?,
                    clock.provenance,
                )
                .map(WireClockId::Triggered),
        })?;
        expect_ordinal("clock", index, id.clock_id().index(), clock.provenance)?;
        ids.clocks.push(id);
    }
    for (index, ownership) in wire.clock_ownerships.iter().enumerate() {
        let clock = mapped(&ids.clocks, ownership.clock, "clock", ownership.provenance)?.clock_id();
        let variable = mapped(
            &ids.variables,
            ownership.variable,
            "variable",
            ownership.provenance,
        )?;
        let role = dae.storage.variables[variable.index() as usize].role;
        let id = dae.clocks(|clocks| match role {
            VariableRole::DiscreteReal => clocks.own_discrete_real(
                clock,
                DiscreteRealId::from_raw(variable.index()),
                ownership.provenance,
            ),
            VariableRole::DiscreteValue => clocks.own_discrete_value(
                clock,
                DiscreteValueId::from_raw(variable.index()),
                ownership.provenance,
            ),
            _ => Err(DaeConstructionError::InvalidVariableRole {
                name: ownership_variable_name(wire, ownership.variable, ownership.provenance)?,
                span: ownership.provenance.span(),
            }),
        })?;
        expect_ordinal("clock ownership", index, id.index(), ownership.provenance)?;
    }
    Ok(())
}

fn reconstruct_temporal<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, previous) in wire.previous_values.iter().enumerate() {
        let clock = mapped(&ids.clocks, previous.clock, "clock", previous.provenance)?.clock_id();
        let variable = mapped(
            &ids.variables,
            previous.variable,
            "variable",
            previous.provenance,
        )?;
        let role = dae.storage.variables[variable.index() as usize].role;
        let id = dae.temporal(|temporal| match role {
            VariableRole::DiscreteReal => temporal.previous_discrete_real(
                clock,
                DiscreteRealId::from_raw(variable.index()),
                previous.provenance,
            ),
            VariableRole::DiscreteValue => temporal.previous_discrete_value(
                clock,
                DiscreteValueId::from_raw(variable.index()),
                previous.provenance,
            ),
            _ => Err(DaeConstructionError::InvalidVariableRole {
                name: ownership_variable_name(wire, previous.variable, previous.provenance)?,
                span: previous.provenance.span(),
            }),
        })?;
        expect_ordinal("previous value", index, id.index(), previous.provenance)?;
        ids.previous_values.push(id);
    }
    for (index, terminal) in wire.terminals.iter().enumerate() {
        let id = dae.temporal(|temporal| temporal.terminal(terminal.provenance))?;
        expect_ordinal(
            "terminal coordinate",
            index,
            id.index(),
            terminal.provenance,
        )?;
        ids.terminals.push(id);
    }
    Ok(())
}

fn ownership_variable_name(
    wire: &StorageWire,
    raw: u32,
    provenance: DaeProvenance,
) -> Result<rumoca_core::VarName, DaeConstructionError> {
    wire.variables
        .get(raw as usize)
        .map(|variable| variable.name.clone())
        .ok_or_else(|| unknown("variable", raw, provenance))
}

const fn malformed(column: &'static str) -> DaeConstructionError {
    DaeConstructionError::MalformedWire { column }
}
