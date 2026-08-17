mod equation_systems;
mod expression_wire;
mod function_graph;
mod function_replay;
mod helpers;
mod quotient_projection;
mod records;

use serde::ser::{SerializeStruct, SerializeStructVariant};
use serde::{Deserialize, Serialize};

use crate::expression::OperandRange;

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
use quotient_projection::*;
use records::*;

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
        let projection = verify_owner_projection(self).map_err(serde::ser::Error::custom)?;
        let mut state = serializer.serialize_struct("DaeStorage", 25)?;
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
        state.serialize_field(
            "expressions",
            &ExpressionArenaOutput {
                arena: &self.expressions,
                projection: &projection,
            },
        )?;
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
        state.serialize_field("model_event_transactions", &self.model_event_transactions)?;
        state.serialize_field(
            "relations",
            &RelationsOutput {
                relations: &self.relations,
                markers: &projection.relation_markers,
            },
        )?;
        state.serialize_field(
            "conditions",
            &ConditionsOutput {
                conditions: &self.conditions,
                markers: &projection.activation_markers,
            },
        )?;
        state.serialize_field(
            "roots",
            &RootsOutput {
                roots: &self.roots,
                markers: &projection.root_markers,
            },
        )?;
        state.serialize_field("structured_roots", &self.structured_roots)?;
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

/// Private current-schema input records.
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
    model_event_transactions: Vec<ModelEventTransactionWire>,
    relations: Vec<RelationEntryWire>,
    conditions: Vec<ConditionEntryWire>,
    roots: Vec<RootEntryWire>,
    structured_roots: Vec<StructuredRootEntryWire>,
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
    /// Emitted-record cursor over `wire.expressions.nodes`. It trails
    /// `expressions.len()` (the source-ordinal cursor) by six for every
    /// replayed model owner.
    next_wire_expression: usize,
    /// In-flight owner replays, in owner-record (= quotient) order. Each
    /// entry's token is consumed by its three stream markers and finished
    /// after root reconstruction.
    quotient_replays: Vec<PendingQuotientReplay<'dae>>,
    /// Extra target expression nodes produced by owner replays (six per
    /// model owner).
    owner_expression_extra: usize,
    /// Extra target packed operands produced by owner replays (three per
    /// model owner, two per function owner).
    owner_operand_extra: usize,
}

struct PendingQuotientReplay<'dae> {
    token: Option<QuotientReplayToken<'dae>>,
    activation: u32,
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
        next_wire_expression: 0,
        quotient_replays: Vec::new(),
        owner_expression_extra: 0,
        owner_operand_extra: 0,
    };
    reconstruct_clocks(wire, dae, &mut ids)?;
    reconstruct_temporal(wire, dae, &mut ids)?;
    function_replay::reconstruct(wire, dae, &mut ids)?;
    reconstruct_relations(wire, dae, &mut ids)?;
    define_variables(wire, dae, &ids, variable_reservations)?;
    define_conditions(wire, dae, &mut ids)?;
    reconstruct_roots(wire, dae, &mut ids)?;
    finish_quotient_replays(dae, &mut ids)?;
    reconstruct_structured_roots(wire, dae, &ids)?;
    reconstruct_events(wire, dae, &ids)?;
    reconstruct_equation_systems(wire, dae, &ids)?;
    reconstruct_initial_discrete_values(wire, dae, &ids)?;
    reconstruct_discrete_value_owners(wire, dae, &ids)?;
    reconstruct_model_event_transactions(wire, dae, &ids)
}

fn reconstruct_model_event_transactions<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (ordinal, transaction) in wire.model_event_transactions.iter().enumerate() {
        let targets = transaction
            .targets
            .iter()
            .copied()
            .map(|target| map_model_event_target(target, ids, transaction.provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let steps = transaction
            .steps
            .iter()
            .map(|step| {
                let definitions = step
                    .definitions
                    .iter()
                    .map(|definition| {
                        Ok(crate::ModelEventDefinition::new(
                            map_model_event_target(definition.target, ids, definition.provenance)?,
                            mapped_expression(ids, definition.value, definition.provenance)?,
                            definition.provenance,
                        ))
                    })
                    .collect::<Result<Vec<_>, DaeConstructionError>>()?;
                Ok(crate::ModelEventStep::new(
                    mapped(&ids.conditions, step.trigger, "condition", step.provenance)?,
                    mapped(&ids.conditions, step.guard, "condition", step.provenance)?,
                    step.clock
                        .map(|clock| mapped(&ids.clocks, clock, "clock", step.provenance))
                        .transpose()?
                        .map(WireClockId::clock_id),
                    definitions,
                    step.provenance,
                ))
            })
            .collect::<Result<Vec<_>, DaeConstructionError>>()?;
        let rebuilt =
            dae.model_events(|events| events.transaction(targets, steps, transaction.provenance))?;
        expect_ordinal(
            "model-event transaction",
            ordinal,
            rebuilt.index(),
            transaction.provenance,
        )?;
    }
    Ok(())
}

fn map_model_event_target<'dae>(
    target: ModelEventTargetWire,
    ids: &WireIds<'dae>,
    provenance: DaeProvenance,
) -> Result<crate::ModelEventTarget<'dae>, DaeConstructionError> {
    let raw = match target {
        ModelEventTargetWire::DiscreteReal(raw) | ModelEventTargetWire::DiscreteValue(raw) => raw,
    };
    let variable = mapped(&ids.variables, raw, "variable", provenance)?;
    Ok(match target {
        ModelEventTargetWire::DiscreteReal(_) => {
            crate::ModelEventTarget::DiscreteReal(DiscreteRealId::from_raw(variable.index()))
        }
        ModelEventTargetWire::DiscreteValue(_) => {
            crate::ModelEventTarget::DiscreteValue(DiscreteValueId::from_raw(variable.index()))
        }
    })
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
            if flat_type.is_some() {
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
            dae.types(|types| types.record_array(name, fields, ty.dimensions.clone(), *provenance))?
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
    let raw = ids.expressions.len() as u32;
    let expression = wire_expression(wire, ids.next_wire_expression)?;
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
        ExprNodeWire::RuntimeQuotientOwner {
            kind: QuotientOwnerKindWire::Model { activation },
            builtin,
            lhs,
            rhs,
        } => {
            return replay_model_quotient_owner(
                dae,
                ids,
                (*builtin, *lhs, *rhs, *activation),
                provenance,
            )
            .map(|()| true);
        }
        ExprNodeWire::RuntimeQuotientOwner {
            kind: QuotientOwnerKindWire::Function { .. },
            ..
        } => {
            // A function-owned quotient replays only inside its open body;
            // reaching the generic path means the wire placed it outside
            // every function component.
            return Err(malformed("expressions.nodes.function_quotient_owner"));
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
    ids.next_wire_expression += 1;
    Ok(true)
}

/// Replay one model quotient owner record: regenerate the seven-node batch
/// atomically, fill all seven source-ordinal mappings, and stage the token
/// for its three stream markers.
fn replay_model_quotient_owner<'dae>(
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
    record: (PureBuiltin, u32, u32, u32),
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let (builtin, lhs, rhs, activation) = record;
    let lhs = mapped(&ids.expressions, lhs, "expression", provenance)?;
    let rhs = mapped(&ids.expressions, rhs, "expression", provenance)?;
    // The reservation ordinal is semantic input; its definition is consumed
    // later by this owner's activation marker.
    if activation as usize >= ids.conditions.len() {
        return Err(malformed("expressions.nodes.quotient_owner_activation"));
    }
    let token = dae.begin_quotient_replay(builtin, [lhs, rhs], provenance)?;
    for id in std::iter::once(token.quotient()).chain(token.generated()) {
        if id.index() as usize != ids.expressions.len() {
            return Err(malformed("expressions.nodes.quotient_owner_batch"));
        }
        ids.expressions.push(id);
    }
    ids.owner_expression_extra += 6;
    ids.owner_operand_extra += 3;
    ids.quotient_replays.push(PendingQuotientReplay {
        token: Some(token),
        activation,
    });
    ids.next_wire_expression += 1;
    Ok(())
}

fn pending_quotient_replay<'ids, 'dae>(
    ids: &'ids mut WireIds<'dae>,
    owner: u32,
) -> Result<&'ids mut QuotientReplayToken<'dae>, DaeConstructionError> {
    ids.quotient_replays
        .get_mut(owner as usize)
        .and_then(|pending| pending.token.as_mut())
        .ok_or_else(|| malformed("quotient_owner marker"))
}

/// After root reconstruction every staged token must be fully consumed:
/// exactly one relation, activation, and root marker each. Finishing records
/// the regenerated owner; an unconsumed stage is the typed replay rejection.
fn finish_quotient_replays<'dae>(
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for pending in &mut ids.quotient_replays {
        let token = pending
            .token
            .take()
            .expect("stream markers only borrow staged tokens");
        dae.finish_quotient_replay(token)?;
    }
    Ok(())
}

fn expect_expression_arena_consumed(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
    ids: &WireIds<'_>,
) -> Result<(), DaeConstructionError> {
    let count = wire.expressions.nodes.len();
    // Owner replay widens the target arena past the emitted records by the
    // exact owner widths: six extra nodes/provenance and three packed
    // operands per model owner, two packed operands per function owner.
    if wire.expressions.provenance.len() != count
        || ids.next_wire_expression != count
        || ids.next_operand != wire.expressions.operands.len()
        || ids.next_subscript != wire.expressions.subscripts.len()
        || ids.delays.len() != wire.delays.len()
        || ids.expressions.len() != count + ids.owner_expression_extra
        || dae.storage.expressions.nodes.len() != count + ids.owner_expression_extra
        || dae.storage.expressions.operands.len()
            != wire.expressions.operands.len() + ids.owner_operand_extra
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
            owner,
            function,
            output,
            operand_count,
        } => {
            let function = mapped(&ids.functions, *function, "function", provenance)?;
            let arguments = map_expression_operands(wire, ids, *operand_count, provenance)?;
            rebuild_call(ids, at, *owner, function, *output, arguments, provenance)
        }
        node @ ExprNodeWire::StringConversion { .. } => {
            rebuild_string_conversion(ids, at, WireStringConversion::from_node(node), provenance)
        }
        ExprNodeWire::FunctionValue { .. } => Err(malformed("expressions.nodes.function_value")),
        ExprNodeWire::FunctionFoldParameter { .. } | ExprNodeWire::FunctionFoldOutput { .. } => {
            Err(malformed("expressions.nodes.function_fold"))
        }
        node @ ExprNodeWire::ClockTransfer { .. } => {
            rebuild_clock_transfer(ids, at, node, provenance)
        }
        // Owner records replay through the staged token machinery before the
        // generic rebuild; one reaching here is out of place.
        ExprNodeWire::RuntimeQuotientOwner { .. } => {
            Err(malformed("expressions.nodes.quotient_owner"))
        }
    }
}

/// Rebuild one call node from its already-mapped function and arguments.
///
/// A node that owns its own call replays those arguments; any other owner is a
/// projection of one output out of that earlier call node and therefore carries
/// no arguments of its own, which `arguments` being empty restates exactly
/// because the wire operand count is what produced it.
fn rebuild_call<'dae>(
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    owner: u32,
    function: FunctionId<'dae>,
    output: u32,
    arguments: Vec<ExprId<'dae>>,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    if owner as usize == ids.expressions.len() {
        return at.call(function, output as usize, arguments);
    }
    if !arguments.is_empty() {
        return Err(malformed("expressions.nodes.call.operand_count"));
    }
    at.replay_call_projection(
        mapped(&ids.expressions, owner, "function call owner", provenance)?,
        function,
        output as usize,
    )
}

fn rebuild_clock_transfer<'dae>(
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    node: &ExprNodeWire,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let ExprNodeWire::ClockTransfer {
        kind,
        source,
        source_clock,
        target_clock,
    } = node
    else {
        return Err(malformed("expressions.nodes.clock_transfer"));
    };
    at.clock_transfer(
        *kind,
        mapped(&ids.expressions, *source, "expression", provenance)?,
        mapped(&ids.clocks, *source_clock, "clock", provenance)?.clock_id(),
        mapped(&ids.clocks, *target_clock, "clock", provenance)?.clock_id(),
    )
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
        CoordinateWire::PreState(variable) => CoordinateInput::PreState(StateId::from_raw(
            mapped(&ids.variables, *variable, "variable", at)?.index(),
        )),
        CoordinateWire::PreAlgebraic(variable) => CoordinateInput::PreAlgebraic(
            AlgebraicId::from_raw(mapped(&ids.variables, *variable, "variable", at)?.index()),
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
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for index in 0..wire.conditions.len() {
        let condition = &wire.conditions[index];
        let Some(node) = condition.node else {
            return Err(incomplete("condition", index, condition.provenance));
        };
        if let ConditionNodeWire::QuotientOwner(owner) = node {
            // The marker must sit at exactly the reservation ordinal the
            // owner record named as its semantic input.
            let activation = ids.conditions[index];
            let owner_activation = ids
                .quotient_replays
                .get(owner as usize)
                .map(|pending| pending.activation);
            if owner_activation != Some(index as u32) {
                return Err(malformed("conditions.quotient_owner"));
            }
            let token = pending_quotient_replay(ids, owner)?;
            dae.replay_quotient_activation(token, activation)?;
            continue;
        }
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
    for index in 0..wire.relations.len() {
        let id = match &wire.relations[index] {
            RelationEntryWire::Relation {
                expression,
                provenance,
            } => {
                let expression = mapped(&ids.expressions, *expression, "expression", *provenance)?;
                let id =
                    dae.conditions(|conditions| conditions.relation(expression, *provenance))?;
                expect_ordinal("relation", index, id.index(), *provenance)?;
                id
            }
            RelationEntryWire::QuotientOwner { owner } => {
                let token = pending_quotient_replay(ids, *owner)?;
                let at = token.provenance();
                let id = dae.replay_quotient_relation(token)?;
                expect_ordinal("relation", index, id.index(), at)?;
                id
            }
        };
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
        ConditionNodeWire::Always => ConditionInput::Always,
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
        ConditionNodeWire::AnyRise { lhs, rhs } => ConditionInput::AnyRise(
            mapped(&ids.conditions, lhs, "condition", at)?,
            mapped(&ids.conditions, rhs, "condition", at)?,
        ),
        // Owner markers are consumed by `define_conditions` before any
        // generic rebuild; one reaching here is out of place.
        ConditionNodeWire::QuotientOwner(_) => {
            return Err(malformed("conditions.quotient_owner"));
        }
    })
}

fn reconstruct_roots<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for index in 0..wire.roots.len() {
        match &wire.roots[index] {
            RootEntryWire::Root {
                relation,
                activation,
                provenance,
            } => {
                let relation = mapped(&ids.relations, *relation, "relation", *provenance)?;
                let activation = mapped(&ids.conditions, *activation, "condition", *provenance)?;
                let id = dae
                    .conditions(|conditions| conditions.root(relation, activation, *provenance))?;
                expect_ordinal("root", index, id.index(), *provenance)?;
            }
            RootEntryWire::QuotientOwner { owner } => {
                let token = pending_quotient_replay(ids, *owner)?;
                let at = token.provenance();
                let id = dae.replay_quotient_root(token)?;
                expect_ordinal("root", index, id.index(), at)?;
            }
        }
    }
    Ok(())
}

fn reconstruct_structured_roots<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, root) in wire.structured_roots.iter().enumerate() {
        let domain = mapped(&ids.domains, root.domain, "domain", root.provenance)?;
        let expression = mapped(
            &ids.expressions,
            root.expression,
            "expression",
            root.provenance,
        )?;
        let id = dae.conditions(|conditions| {
            conditions.structured_root(domain, expression, root.provenance)
        })?;
        expect_ordinal("structured root", index, id.index(), root.provenance)?;
    }
    Ok(())
}

fn reconstruct_events<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, event) in wire.time_events.iter().enumerate() {
        let id = match (event.instant, event.deadline) {
            (Some(instant), None) => {
                let instant = instant.checked(event.provenance)?;
                dae.events(|events| events.time_event(instant, event.provenance))?
            }
            (None, Some(deadline)) => {
                let deadline = mapped(
                    &ids.expressions,
                    deadline,
                    "dynamic time-event deadline",
                    event.provenance,
                )?;
                dae.events(|events| events.dynamic_time_event(deadline, event.provenance))?
            }
            _ => {
                return Err(DaeConstructionError::InvalidDynamicTimeEventDeadline {
                    span: event.provenance.span(),
                });
            }
        };
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
    let replay = |staged: &mut crate::DiscreteValueOwner<'_, 'dae>| {
        for branch in &owner.branches {
            replay_discrete_value_branch(branch, staged, ids)?;
        }
        Ok(())
    };
    match owner.structure {
        Some(structure) => topology.structured_owner(
            owner.provenance,
            mapped(&ids.domains, structure.domain, "domain", owner.provenance)?,
            structure.scalar_view,
            targets,
            replay,
        ),
        None => topology.owner(owner.provenance, targets, replay),
    }
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
            ClockKindWire::Periodic(schedule) => clocks
                .scheduled(schedule.checked(clock.provenance)?, clock.provenance)
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
        let id = dae.clocks(|clocks| match (role, ownership.sampled) {
            (VariableRole::DiscreteReal, false) => clocks.own_discrete_real(
                clock,
                DiscreteRealId::from_raw(variable.index()),
                ownership.provenance,
            ),
            (VariableRole::DiscreteValue, false) => clocks.own_discrete_value(
                clock,
                DiscreteValueId::from_raw(variable.index()),
                ownership.provenance,
            ),
            (VariableRole::DiscreteReal, true) => clocks.own_sampled_discrete_real(
                clock,
                DiscreteRealId::from_raw(variable.index()),
                ownership.provenance,
            ),
            (VariableRole::DiscreteValue, true) => clocks.own_sampled_discrete_value(
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
