mod equation_systems;
mod expression_wire;
mod function_replay;
mod helpers;

use serde::Deserialize;

use super::*;
use crate::expression::Subscript;
use crate::{
    ClockId, ConditionInput, DaeProvenanceOrigin, DelayId, ExpressionAt, PreviousId, PureBuiltin,
    RelationId, TerminalId, UnaryOperator,
};

use equation_systems::reconstruct_equation_systems;
use expression_wire::*;
use helpers::{expect_ordinal, map_expression_operands, map_many, mapped, wire_operands};

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

/// Private schema-v11 input records.
///
/// These mirror the serialized column names, but they are deliberately
/// distinct from every invariant-bearing arena entry. Deserialization can
/// therefore produce only wire data; the records below enter the IR solely
/// through the same checked operations used by production construction.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StorageWire {
    value_types: Vec<ValueTypeWire>,
    flat_type_ids: Vec<Option<rumoca_core::TypeId>>,
    #[serde(deserialize_with = "deserialize_provenance_vec")]
    value_type_provenance: Vec<DaeProvenance>,
    variables: Vec<VariableEntryWire>,
    functions: Vec<FunctionEntryWire>,
    function_folds: Vec<FunctionFoldEntryWire>,
    domains: Vec<DomainEntryWire>,
    expressions: ExpressionArenaWire,
    continuous_equations: Vec<ResidualEquationWire>,
    initialization_equations: Vec<ResidualEquationWire>,
    discrete_real_equations: Vec<ResidualEquationWire>,
    discrete_assignments: Vec<DiscreteAssignmentWire>,
    continuous_families: Vec<StructuredFamilyWire>,
    initialization_families: Vec<StructuredFamilyWire>,
    continuous_equation_owners: Vec<EquationOwnerWire>,
    initialization_equation_owners: Vec<EquationOwnerWire>,
    equation_family_bodies: Vec<u32>,
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

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionEntryWire {
    name: rumoca_core::VarName,
    parameters: Vec<u32>,
    results: Vec<u32>,
    parameter_values: Vec<FunctionParameterEntryInput>,
    values: Vec<FunctionValueEntryInput>,
    output_values: Vec<u32>,
    definitions: Vec<FunctionSsaDefinitionInput>,
    folds: Vec<u32>,
    #[serde(deserialize_with = "deserialize_provenance")]
    declaration: DaeProvenance,
    definition: Option<FunctionDefinitionInput>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionParameterEntryInput {
    name: rumoca_core::VarName,
    value_type: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    declaration: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionValueEntryInput {
    name: rumoca_core::VarName,
    value_type: u32,
    role: FunctionValueRole,
    #[serde(deserialize_with = "deserialize_provenance")]
    declaration: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionDefinitionInput {
    statements: Vec<FunctionStatementInput>,
    results: Vec<u32>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionSsaDefinitionInput {
    target: u32,
    rhs: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum FunctionStatementInput {
    Assignment {
        definition: u32,
    },
    For {
        fold: u32,
        statements: Vec<FunctionStatementInput>,
        #[serde(deserialize_with = "deserialize_provenance")]
        provenance: DaeProvenance,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionFoldEntryWire {
    function: u32,
    ordinal: u32,
    domain: u32,
    targets: Vec<u32>,
    parameter_definitions: Vec<u32>,
    initial_definitions: Vec<u32>,
    update_definitions: Vec<u32>,
    output_definitions: Vec<u32>,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DomainEntryWire {
    parent: Option<u32>,
    domain: rumoca_core::StructuredIndexDomain,
    extents: Box<[u32]>,
    scalar_count: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ResidualEquationWire {
    residual: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DiscreteAssignmentWire {
    target: u32,
    value: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(deny_unknown_fields)]
struct FamilyBodyRangeWire {
    start: u32,
    len: u32,
}

impl FamilyBodyRangeWire {
    fn indices(self) -> Option<std::ops::Range<usize>> {
        let start = self.start as usize;
        let end = start.checked_add(self.len as usize)?;
        Some(start..end)
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StructuredFamilyWire {
    domain: u32,
    scalar_view: rumoca_core::ComprehensionScalarView,
    bodies: FamilyBodyRangeWire,
    scalar_rows: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum EquationOwnerWire {
    Residual(u32),
    Structured(u32),
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
    AssignDiscreteReal { target: u32, value: u32 },
    AssignDiscreteValue { target: u32, value: u32 },
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
    role: ClockedVariableRoleWire,
    clock: u32,
    #[serde(deserialize_with = "deserialize_provenance")]
    provenance: DaeProvenance,
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum ClockedVariableRoleWire {
    DiscreteReal,
    DiscreteValue,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PreviousEntryWire {
    variable: u32,
    role: ClockedVariableRoleWire,
    clock: u32,
    value_type: u32,
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
#[serde(deny_unknown_fields)]
struct DelayEntryWire {
    source: u32,
    delay_time: u32,
    delay_time_evidence: Option<PositiveParameterWire>,
    delay_max: Option<PositiveParameterWire>,
    value_type: u32,
    variability: ExpressionVariability,
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

struct WireIds<'dae> {
    types: Vec<ValueTypeId<'dae>>,
    variables: Vec<VariableId<'dae>>,
    functions: Vec<FunctionId<'dae>>,
    function_folds: Vec<Option<FunctionFoldId<'dae>>>,
    domains: Vec<DomainId<'dae>>,
    conditions: Vec<ConditionId<'dae>>,
    relations: Vec<RelationId<'dae>>,
    clocks: Vec<ClockId<'dae>>,
    previous_values: Vec<PreviousId<'dae>>,
    terminals: Vec<TerminalId<'dae>>,
    delays: Vec<DelayId<'dae>>,
    expressions: Vec<ExprId<'dae>>,
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
    let types = reconstruct_types(wire, dae)?;
    let (variables, variable_reservations) = reconstruct_variables(wire, dae, &types)?;
    let (functions, function_reservations) = reconstruct_functions(wire, dae, &types)?;
    let domains = reconstruct_domains(wire, dae)?;
    let conditions = reconstruct_conditions(wire, dae)?;
    let mut ids = WireIds {
        types,
        variables,
        functions,
        function_folds: vec![None; wire.function_folds.len()],
        domains,
        conditions,
        relations: Vec::with_capacity(wire.relations.len()),
        clocks: Vec::with_capacity(wire.clocks.len()),
        previous_values: Vec::with_capacity(wire.previous_values.len()),
        terminals: Vec::with_capacity(wire.terminals.len()),
        delays: Vec::with_capacity(wire.delays.len()),
        expressions: Vec::with_capacity(wire.expressions.nodes.len()),
    };
    reconstruct_clocks(wire, dae, &mut ids)?;
    reconstruct_temporal(wire, dae, &mut ids)?;
    function_replay::reconstruct(wire, dae, &mut ids, function_reservations)?;
    reconstruct_relations(wire, dae, &mut ids)?;
    define_variables(wire, dae, &ids, variable_reservations)?;
    define_conditions(wire, dae, &ids)?;
    reconstruct_roots(wire, dae, &ids)?;
    reconstruct_events(wire, dae, &ids)?;
    reconstruct_equation_systems(wire, dae, &ids)
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

fn reconstruct_functions<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
) -> Result<(Vec<FunctionId<'dae>>, Vec<FunctionReservation<'dae>>), DaeConstructionError> {
    let mut ids = Vec::with_capacity(wire.functions.len());
    let mut reservations = Vec::with_capacity(wire.functions.len());
    for function in &wire.functions {
        let parameters = map_many(
            types,
            &function.parameters,
            "value type",
            function.declaration,
        )?;
        let results = map_many(types, &function.results, "value type", function.declaration)?;
        let (id, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(
                function.name.clone(),
                parameters,
                results,
                function.declaration,
            )
        })?;
        reconstruct_function_values(function, dae, types, &reservation)?;
        ids.push(id);
        reservations.push(reservation);
    }
    Ok((ids, reservations))
}

fn reconstruct_function_values<'dae>(
    function: &FunctionEntryWire,
    dae: &mut DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
    reservation: &FunctionReservation<'dae>,
) -> Result<(), DaeConstructionError> {
    for (ordinal, parameter) in function.parameter_values.iter().enumerate() {
        let rebuilt = dae.functions(|functions| {
            functions.parameter(
                reservation,
                parameter.name.clone(),
                ordinal,
                parameter.declaration,
            )
        })?;
        let wire_type = mapped(
            types,
            parameter.value_type,
            "value type",
            parameter.declaration,
        )?;
        let rebuilt_type = dae.storage.functions[reservation.function().index() as usize]
            .parameter_values[rebuilt.ordinal() as usize]
            .value_type;
        if rebuilt_type != wire_type.index() {
            return Err(DaeConstructionError::ShapeMismatch {
                span: parameter.declaration.span(),
            });
        }
        expect_ordinal(
            "function parameter",
            ordinal,
            rebuilt.ordinal(),
            parameter.declaration,
        )?;
    }
    if function.output_values.len() > function.values.len() {
        return Err(malformed("functions.output_values"));
    }
    for (raw, value) in function.values.iter().enumerate() {
        if raw < function.output_values.len() {
            let expected = u32::try_from(raw).map_err(|_| malformed("functions.values"))?;
            if function.output_values[raw] != expected || value.role != FunctionValueRole::Output {
                return Err(malformed("functions.output_values"));
            }
            let rebuilt = dae.functions(|functions| {
                functions.output(reservation, value.name.clone(), raw, value.declaration)
            })?;
            expect_function_value_type(dae, types, reservation, value, rebuilt)?;
            expect_ordinal("function value", raw, rebuilt.ordinal(), value.declaration)?;
            continue;
        }
        if value.role != FunctionValueRole::Local {
            return Err(malformed("functions.values"));
        }
        let value_type = mapped(types, value.value_type, "value type", value.declaration)?;
        let rebuilt = dae.functions(|functions| {
            functions.local(
                reservation,
                value.name.clone(),
                value_type,
                value.declaration,
            )
        })?;
        expect_ordinal("function value", raw, rebuilt.ordinal(), value.declaration)?;
    }
    Ok(())
}

fn expect_function_value_type<'dae>(
    dae: &DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
    reservation: &FunctionReservation<'dae>,
    value: &FunctionValueEntryInput,
    rebuilt: FunctionValueId<'dae>,
) -> Result<(), DaeConstructionError> {
    let wire_type = mapped(types, value.value_type, "value type", value.declaration)?;
    let rebuilt_type = dae.storage.functions[reservation.function().index() as usize].values
        [rebuilt.ordinal() as usize]
        .value_type;
    if rebuilt_type == wire_type.index() {
        Ok(())
    } else {
        Err(DaeConstructionError::ShapeMismatch {
            span: value.declaration.span(),
        })
    }
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
        let rebuilt = &dae.storage.domains[id.index() as usize];
        if rebuilt.extents != domain.extents || rebuilt.scalar_count != domain.scalar_count {
            return Err(DaeConstructionError::ShapeMismatch {
                span: domain.provenance.span(),
            });
        }
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
    expect_next_packed_ranges(wire, dae, node)?;
    let expected_type = mapped(&ids.types, expression.value_type, "value type", provenance)?;
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
            rebuild_node(
                wire,
                ids,
                expressions.at(provenance),
                node,
                expected_type,
                provenance,
            )
        })?,
    };
    if id.index() != raw {
        return Err(DaeConstructionError::MalformedWire {
            column: "expressions.nodes",
        });
    }
    verify_rebuilt_expression(wire, dae, index, id, expected_type, provenance)?;
    ids.expressions.push(id);
    Ok(true)
}

fn expect_next_packed_ranges(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
    node: &ExprNodeWire,
) -> Result<(), DaeConstructionError> {
    match node {
        ExprNodeWire::Conditional { operands }
        | ExprNodeWire::Array { operands }
        | ExprNodeWire::Record { operands }
        | ExprNodeWire::Builtin { operands, .. }
        | ExprNodeWire::Call { operands, .. } => expect_packed_range(
            *operands,
            dae.storage.expressions.operands.len(),
            wire.expressions.operands.len(),
            "expressions.operands",
        ),
        ExprNodeWire::Index { subscripts, .. } | ExprNodeWire::ArrayUpdate { subscripts, .. } => {
            expect_packed_range(
                *subscripts,
                dae.storage.expressions.subscripts.len(),
                wire.expressions.subscripts.len(),
                "expressions.subscripts",
            )
        }
        _ => Ok(()),
    }
}

fn expect_packed_range(
    range: OperandRangeWire,
    cursor: usize,
    buffer_len: usize,
    column: &'static str,
) -> Result<(), DaeConstructionError> {
    let indices = range.indices().ok_or_else(|| malformed(column))?;
    if indices.start == cursor && indices.end <= buffer_len {
        Ok(())
    } else {
        Err(malformed(column))
    }
}

fn verify_rebuilt_expression(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
    index: usize,
    id: ExprId<'_>,
    expected_type: ValueTypeId<'_>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let raw = id.index() as usize;
    let expression = wire_expression(wire, index)?;
    if dae.storage.expressions.provenance[raw] != provenance
        || dae.storage.expressions.value_types[raw] != expected_type.index()
        || dae.storage.expressions.variability[raw] != expression.variability
        || dae.storage.expressions.binder_domains[raw] != expression.binder_domain
        || dae.storage.expressions.function_scopes[raw] != expression.function_scope
    {
        return Err(DaeConstructionError::ShapeMismatch {
            span: provenance.span(),
        });
    }
    Ok(())
}

fn expect_expression_arena_consumed(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
) -> Result<(), DaeConstructionError> {
    let count = wire.expressions.nodes.len();
    if wire.expressions.provenance.len() != count
        || wire.expressions.value_types.len() != count
        || wire.expressions.variability.len() != count
        || wire.expressions.binder_domains.len() != count
        || wire.expressions.function_scopes.len() != count
        || dae.storage.expressions.nodes.len() != count
        || dae.storage.expressions.operands != wire.expressions.operands
        || dae.storage.expressions.subscripts.len() != wire.expressions.subscripts.len()
    {
        return Err(malformed("expressions"));
    }
    for (actual, expected) in dae
        .storage
        .expressions
        .nodes
        .iter()
        .zip(&wire.expressions.nodes)
    {
        if !expression_node_matches(actual, expected) {
            return Err(malformed("expressions.nodes"));
        }
    }
    for (actual, expected) in dae
        .storage
        .expressions
        .subscripts
        .iter()
        .zip(&wire.expressions.subscripts)
    {
        if actual.provenance != expected.provenance
            || !subscript_kind_matches(&actual.kind, expected.kind)
        {
            return Err(malformed("expressions.subscripts"));
        }
    }
    Ok(())
}

fn expression_node_matches(actual: &ExprNode, expected: &ExprNodeWire) -> bool {
    match (actual, expected) {
        (ExprNode::Literal(actual), ExprNodeWire::Literal(expected)) => {
            literal_matches(actual, expected)
        }
        (ExprNode::Coordinate(actual), ExprNodeWire::Coordinate(expected)) => {
            coordinate_matches(*actual, expected)
        }
        (ExprNode::FunctionValue { .. }, ExprNodeWire::FunctionValue { .. })
        | (ExprNode::FunctionFoldParameter { .. }, ExprNodeWire::FunctionFoldParameter { .. })
        | (ExprNode::FunctionFoldOutput { .. }, ExprNodeWire::FunctionFoldOutput { .. }) => {
            function_node_matches(actual, expected)
        }
        (ExprNode::Index { .. }, ExprNodeWire::Index { .. })
        | (ExprNode::ArrayUpdate { .. }, ExprNodeWire::ArrayUpdate { .. })
        | (ExprNode::Builtin { .. }, ExprNodeWire::Builtin { .. })
        | (ExprNode::Call { .. }, ExprNodeWire::Call { .. }) => {
            buffer_node_matches(actual, expected)
        }
        _ => operator_node_matches(actual, expected),
    }
}

fn operator_node_matches(actual: &ExprNode, expected: &ExprNodeWire) -> bool {
    match (actual, expected) {
        (
            ExprNode::Unary {
                operator: ao,
                operand: aa,
            },
            ExprNodeWire::Unary {
                operator: eo,
                operand: ea,
            },
        ) => ao == eo && aa == ea,
        (
            ExprNode::Binary {
                operator: ao,
                lhs: al,
                rhs: ar,
            },
            ExprNodeWire::Binary {
                operator: eo,
                lhs: el,
                rhs: er,
            },
        ) => ao == eo && al == el && ar == er,
        (ExprNode::Conditional { operands: a }, ExprNodeWire::Conditional { operands: e })
        | (ExprNode::Array { operands: a }, ExprNodeWire::Array { operands: e })
        | (ExprNode::Record { operands: a }, ExprNodeWire::Record { operands: e }) => {
            range_matches(*a, *e)
        }
        (
            ExprNode::Field {
                base: ab,
                field: af,
            },
            ExprNodeWire::Field {
                base: eb,
                field: ef,
            },
        ) => ab == eb && af == ef,
        (
            ExprNode::Range {
                start: aa,
                step: ab,
                stop: ac,
            },
            ExprNodeWire::Range {
                start: ea,
                step: eb,
                stop: ec,
            },
        ) => aa == ea && ab == eb && ac == ec,
        (
            ExprNode::Comprehension {
                domain: ad,
                body: ab,
            },
            ExprNodeWire::Comprehension {
                domain: ed,
                body: eb,
            },
        ) => ad == ed && ab == eb,
        _ => false,
    }
}

fn buffer_node_matches(actual: &ExprNode, expected: &ExprNodeWire) -> bool {
    match (actual, expected) {
        (
            ExprNode::Index {
                base: ab,
                subscripts: as_,
            },
            ExprNodeWire::Index {
                base: eb,
                subscripts: es,
            },
        ) => ab == eb && range_matches(*as_, *es),
        (
            ExprNode::ArrayUpdate {
                base: ab,
                value: av,
                subscripts: as_,
            },
            ExprNodeWire::ArrayUpdate {
                base: eb,
                value: ev,
                subscripts: es,
            },
        ) => ab == eb && av == ev && range_matches(*as_, *es),
        (
            ExprNode::Builtin {
                builtin: ab,
                operands: ao,
            },
            ExprNodeWire::Builtin {
                builtin: eb,
                operands: eo,
            },
        ) => ab == eb && range_matches(*ao, *eo),
        (
            ExprNode::Call {
                function: af,
                output: ao,
                operands: aa,
            },
            ExprNodeWire::Call {
                function: ef,
                output: eo,
                operands: ea,
            },
        ) => af == ef && ao == eo && range_matches(*aa, *ea),
        _ => false,
    }
}

fn function_node_matches(actual: &ExprNode, expected: &ExprNodeWire) -> bool {
    match (actual, expected) {
        (
            ExprNode::FunctionValue {
                function: af,
                value: av,
                definition_ordinal: ad,
            },
            ExprNodeWire::FunctionValue {
                function: ef,
                value: ev,
                definition_ordinal: ed,
            },
        ) => af == ef && av == ev && ad == ed,
        (
            ExprNode::FunctionFoldParameter {
                function: af,
                fold: ax,
                carried: ac,
                definition_ordinal: ad,
            },
            ExprNodeWire::FunctionFoldParameter {
                function: ef,
                fold: ex,
                carried: ec,
                definition_ordinal: ed,
            },
        )
        | (
            ExprNode::FunctionFoldOutput {
                function: af,
                fold: ax,
                carried: ac,
                definition_ordinal: ad,
            },
            ExprNodeWire::FunctionFoldOutput {
                function: ef,
                fold: ex,
                carried: ec,
                definition_ordinal: ed,
            },
        ) => af == ef && ax == ex && ac == ec && ad == ed,
        _ => false,
    }
}

fn literal_matches(actual: &DaeLiteral, expected: &DaeLiteralWire) -> bool {
    match (actual, expected) {
        (DaeLiteral::Real(a), DaeLiteralWire::Real(e)) => a.to_bits() == e.to_bits(),
        (DaeLiteral::Integer(a), DaeLiteralWire::Integer(e)) => a == e,
        (DaeLiteral::Enumeration(a), DaeLiteralWire::Enumeration(e)) => a == e,
        (DaeLiteral::Boolean(a), DaeLiteralWire::Boolean(e)) => a == e,
        (DaeLiteral::String(a), DaeLiteralWire::String(e)) => a == e,
        _ => false,
    }
}

fn coordinate_matches(actual: Coordinate, expected: &CoordinateWire) -> bool {
    match (actual, expected) {
        (Coordinate::Parameter(a), CoordinateWire::Parameter(e))
        | (Coordinate::Input(a), CoordinateWire::Input(e))
        | (Coordinate::State(a), CoordinateWire::State(e))
        | (Coordinate::Derivative(a), CoordinateWire::Derivative(e))
        | (Coordinate::Algebraic(a), CoordinateWire::Algebraic(e))
        | (Coordinate::DiscreteReal(a), CoordinateWire::DiscreteReal(e))
        | (Coordinate::DiscreteValue(a), CoordinateWire::DiscreteValue(e))
        | (Coordinate::PreDiscreteReal(a), CoordinateWire::PreDiscreteReal(e))
        | (Coordinate::PreDiscreteValue(a), CoordinateWire::PreDiscreteValue(e))
        | (Coordinate::Condition(a), CoordinateWire::Condition(e))
        | (Coordinate::Delay(a), CoordinateWire::Delay(e))
        | (Coordinate::Previous(a), CoordinateWire::Previous(e))
        | (Coordinate::Terminal(a), CoordinateWire::Terminal(e)) => a == *e,
        (Coordinate::Time, CoordinateWire::Time) => true,
        (
            Coordinate::Binder {
                domain: ad,
                ordinal: ao,
            },
            CoordinateWire::Binder {
                domain: ed,
                ordinal: eo,
            },
        )
        | (
            Coordinate::FunctionParameter {
                function: ad,
                ordinal: ao,
            },
            CoordinateWire::FunctionParameter {
                function: ed,
                ordinal: eo,
            },
        ) => ad == *ed && ao == *eo,
        _ => false,
    }
}

const fn range_matches(
    actual: crate::expression::OperandRange,
    expected: OperandRangeWire,
) -> bool {
    actual.start == expected.start && actual.len == expected.len
}

const fn subscript_kind_matches(
    actual: &PackedSubscriptKind,
    expected: PackedSubscriptKindWire,
) -> bool {
    match (actual, expected) {
        (PackedSubscriptKind::Index(a), PackedSubscriptKindWire::Index(e))
        | (PackedSubscriptKind::Slice(a), PackedSubscriptKindWire::Slice(e)) => *a == e,
        (PackedSubscriptKind::Whole, PackedSubscriptKindWire::Whole) => true,
        _ => false,
    }
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
    let coordinate = match (&delay.delay_time_evidence, &delay.delay_max) {
        (Some(evidence), None) if evidence.expression == delay.delay_time => {
            let expression = mapped(
                &ids.expressions,
                evidence.expression,
                "expression",
                evidence.provenance,
            )?;
            dae.temporal(|temporal| {
                let positive =
                    temporal.positive_parameter(expression, evidence.value, evidence.provenance)?;
                temporal.delay(source, positive, delay.provenance, coordinate_provenance)
            })?
        }
        (None, Some(maximum)) => {
            let delay_time = mapped(
                &ids.expressions,
                delay.delay_time,
                "expression",
                delay.provenance,
            )?;
            let maximum_expression = mapped(
                &ids.expressions,
                maximum.expression,
                "expression",
                maximum.provenance,
            )?;
            dae.temporal(|temporal| {
                let maximum = temporal.positive_parameter(
                    maximum_expression,
                    maximum.value,
                    maximum.provenance,
                )?;
                temporal.bounded_delay(
                    source,
                    delay_time,
                    maximum,
                    delay.provenance,
                    coordinate_provenance,
                )
            })?
        }
        _ => {
            return Err(DaeConstructionError::MalformedWire {
                column: "delay evidence",
            });
        }
    };
    expect_ordinal("delay", index, coordinate.id().index(), delay.provenance)?;
    let rebuilt = &dae.storage.delays[coordinate.id().index() as usize];
    if rebuilt.value_type != delay.value_type || rebuilt.variability != delay.variability {
        return Err(DaeConstructionError::ShapeMismatch {
            span: delay.provenance.span(),
        });
    }
    ids.delays.push(coordinate.id());
    Ok(coordinate.expression())
}

fn rebuild_node<'dae>(
    wire: &StorageWire,
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    node: &ExprNodeWire,
    expected_type: ValueTypeId<'dae>,
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
        ExprNodeWire::Conditional { operands } => {
            rebuild_conditional(wire, ids, at, *operands, provenance)
        }
        ExprNodeWire::Array { operands } => {
            let operands = map_expression_operands(wire, ids, *operands, provenance)?;
            if operands.is_empty() {
                at.empty_array(expected_type)
            } else {
                at.array(operands)
            }
        }
        ExprNodeWire::Record { operands } => at.record(
            expected_type,
            map_expression_operands(wire, ids, *operands, provenance)?,
        ),
        ExprNodeWire::Field { base, field } => at.field(
            mapped(&ids.expressions, *base, "expression", provenance)?,
            *field as usize,
        ),
        ExprNodeWire::Range { start, step, stop } => at.range(*start, *step, *stop),
        ExprNodeWire::Comprehension { domain, body } => at.comprehension(
            mapped(&ids.domains, *domain, "domain", provenance)?,
            mapped(&ids.expressions, *body, "expression", provenance)?,
        ),
        ExprNodeWire::Index { base, subscripts } => {
            rebuild_index(wire, ids, at, *base, *subscripts, provenance)
        }
        ExprNodeWire::ArrayUpdate {
            base,
            value,
            subscripts,
        } => at.array_update(
            mapped(&ids.expressions, *base, "expression", provenance)?,
            mapped(&ids.expressions, *value, "expression", provenance)?,
            rebuild_subscripts(wire, ids, *subscripts, provenance)?,
        ),
        ExprNodeWire::Builtin { builtin, operands } => at.builtin(
            *builtin,
            map_expression_operands(wire, ids, *operands, provenance)?,
        ),
        ExprNodeWire::Call {
            function,
            output,
            operands,
        } => at.call(
            mapped(&ids.functions, *function, "function", provenance)?,
            *output as usize,
            map_expression_operands(wire, ids, *operands, provenance)?,
        ),
        ExprNodeWire::FunctionValue { .. } => Err(malformed("expressions.nodes.function_value")),
        ExprNodeWire::FunctionFoldParameter { .. } | ExprNodeWire::FunctionFoldOutput { .. } => {
            Err(malformed("expressions.nodes.function_fold"))
        }
    }
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
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    range: OperandRangeWire,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let operands = wire_operands(wire, range, provenance)?;
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
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    base: u32,
    range: OperandRangeWire,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let subscripts = rebuild_subscripts(wire, ids, range, provenance)?;
    at.index(
        mapped(&ids.expressions, base, "expression", provenance)?,
        subscripts,
    )
}

fn rebuild_subscripts<'dae>(
    wire: &StorageWire,
    ids: &WireIds<'dae>,
    range: OperandRangeWire,
    provenance: DaeProvenance,
) -> Result<Vec<Subscript<'dae>>, DaeConstructionError> {
    let packed = wire
        .expressions
        .subscripts
        .get(range.indices().ok_or(DaeConstructionError::MalformedWire {
            column: "subscript range",
        })?)
        .ok_or_else(|| unknown("subscript range", range.start, provenance))?;
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
            ConditionInput::Clock(mapped(&ids.clocks, raw, "clock", at)?)
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
            EventActionKindWire::AssignDiscreteReal { target, value } => {
                let target = mapped(&ids.variables, target, "variable", action.provenance)?;
                events.assign_discrete_real(
                    trigger,
                    guard,
                    crate::DiscreteRealId::from_raw(target.index()),
                    mapped(&ids.expressions, value, "expression", action.provenance)?,
                    action.provenance,
                )
            }
            EventActionKindWire::AssignDiscreteValue { target, value } => {
                let target = mapped(&ids.variables, target, "variable", action.provenance)?;
                events.assign_discrete_value(
                    trigger,
                    guard,
                    crate::DiscreteValueId::from_raw(target.index()),
                    mapped(&ids.expressions, value, "expression", action.provenance)?,
                    action.provenance,
                )
            }
        })?;
        expect_ordinal("event action", index, id.index(), action.provenance)?;
    }
    Ok(())
}

fn reconstruct_clocks<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, clock) in wire.clocks.iter().enumerate() {
        let id = dae.clocks(|clocks| match clock.kind {
            ClockKindWire::Periodic(lattice) => {
                clocks.periodic(lattice.checked(clock.provenance)?, clock.provenance)
            }
            ClockKindWire::Triggered(condition) => clocks.triggered(
                mapped(&ids.conditions, condition, "condition", clock.provenance)?,
                clock.provenance,
            ),
        })?;
        expect_ordinal("clock", index, id.index(), clock.provenance)?;
        ids.clocks.push(id);
    }
    for (index, ownership) in wire.clock_ownerships.iter().enumerate() {
        let clock = mapped(&ids.clocks, ownership.clock, "clock", ownership.provenance)?;
        let variable = mapped(
            &ids.variables,
            ownership.variable,
            "variable",
            ownership.provenance,
        )?;
        let id = dae.clocks(|clocks| match ownership.role {
            ClockedVariableRoleWire::DiscreteReal => clocks.own_discrete_real(
                clock,
                DiscreteRealId::from_raw(variable.index()),
                ownership.provenance,
            ),
            ClockedVariableRoleWire::DiscreteValue => clocks.own_discrete_value(
                clock,
                DiscreteValueId::from_raw(variable.index()),
                ownership.provenance,
            ),
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
        let clock = mapped(&ids.clocks, previous.clock, "clock", previous.provenance)?;
        let variable = mapped(
            &ids.variables,
            previous.variable,
            "variable",
            previous.provenance,
        )?;
        let id = dae.temporal(|temporal| match previous.role {
            ClockedVariableRoleWire::DiscreteReal => temporal.previous_discrete_real(
                clock,
                DiscreteRealId::from_raw(variable.index()),
                previous.provenance,
            ),
            ClockedVariableRoleWire::DiscreteValue => temporal.previous_discrete_value(
                clock,
                DiscreteValueId::from_raw(variable.index()),
                previous.provenance,
            ),
        })?;
        expect_ordinal("previous value", index, id.index(), previous.provenance)?;
        if dae.storage.previous_values[id.index() as usize].value_type != previous.value_type {
            return Err(DaeConstructionError::ShapeMismatch {
                span: previous.provenance.span(),
            });
        }
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

const fn malformed(column: &'static str) -> DaeConstructionError {
    DaeConstructionError::MalformedWire { column }
}
