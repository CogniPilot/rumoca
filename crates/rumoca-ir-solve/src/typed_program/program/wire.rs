use rumoca_core::StructuredIndexDomain;
use serde::{Deserialize, Deserializer};

use super::*;
use crate::typed_program::call::SolvePureCallTableView;

#[derive(Clone, Deserialize)]
pub(in crate::typed_program) struct TypedProgramWire {
    pub(super) arithmetic: SolveArithmeticProfile,
    pub(super) slots: Vec<SolveSlotWire>,
    pub(super) register_types: Vec<SolveValueType>,
    pub(super) operations: Vec<SolveSpannedOperationWire>,
    assigned_states: AssignedStateLayoutWire,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct AssignedStateLayoutWire {
    types: Vec<SolveValueType>,
    inputs: Vec<SolveValueType>,
    outputs: Vec<SolveValueType>,
}

#[derive(Clone, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
enum AssignedStateOperationWire {
    Absent {
        destination: SolveAssignedStateId,
        payload_type: SolveValueType,
    },
    Present {
        destination: SolveAssignedStateId,
        source: SolveRegisterId,
    },
    Input {
        destination: SolveAssignedStateId,
        input: usize,
    },
    Output {
        output: usize,
        source: SolveAssignedStateId,
    },
    IsPresent {
        destination: SolveRegisterId,
        source: SolveAssignedStateId,
    },
    AllPresent {
        destination: SolveRegisterId,
        sources: Box<[SolveAssignedStateId]>,
    },
    ProjectChecked {
        destination: SolveRegisterId,
        source: SolveAssignedStateId,
        predicate_output: usize,
    },
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct AssignedConditionalWire {
    condition: SolveRegisterId,
    captures: Box<[SolveRegisterId]>,
    state_captures: Box<[SolveAssignedStateId]>,
    destinations: Box<[SolveRegisterId]>,
    state_destinations: Box<[SolveAssignedStateId]>,
    if_true: SolveProgramRegionWire,
    if_false: SolveProgramRegionWire,
}

#[derive(Clone, Deserialize)]
pub(super) struct SolveSlotWire {
    id: SolveSlotId,
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    provenance: Span,
}

#[derive(Clone, Deserialize)]
pub(super) struct SolveSpannedOperationWire {
    operation: SolveOperationWire,
    provenance: Span,
}

#[derive(Clone, Deserialize)]
struct AssertionForwardingWire {
    child_predicate: usize,
    parent_predicate: usize,
}

#[derive(Clone, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case")]
enum SolveOperationWire {
    AssignedState {
        #[serde(rename = "state_operation")]
        operation: AssignedStateOperationWire,
    },
    AssignedConditional {
        program: Box<AssignedConditionalWire>,
    },
    CheckAssertion {
        predicate_output: usize,
        message_outputs: Box<[usize]>,
        condition: SolveRegisterId,
        captures: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
        message: SolveAssertionMessageWire,
    },
    Constant {
        destination: SolveRegisterId,
        value: SolveValue,
    },
    Load {
        destination: SolveRegisterId,
        slot: SolveSlotId,
    },
    Store {
        slot: SolveSlotId,
        source: SolveRegisterId,
    },
    Unary {
        destination: SolveRegisterId,
        operator: SolveUnaryOperator,
        operand: SolveRegisterId,
    },
    Binary {
        destination: SolveRegisterId,
        operator: SolveBinaryOperator,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Compare {
        destination: SolveRegisterId,
        operator: SolveCompareOperator,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Convert {
        destination: SolveRegisterId,
        operator: SolveConversionOperator,
        operand: SolveRegisterId,
    },
    Select {
        destination: SolveRegisterId,
        condition: SolveRegisterId,
        if_true: SolveRegisterId,
        if_false: SolveRegisterId,
    },
    Conditional {
        condition: SolveRegisterId,
        captures: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
        if_true: Box<SolveProgramRegionWire>,
        if_false: Box<SolveProgramRegionWire>,
    },
    Map {
        domain: StructuredIndexDomain,
        captures: Box<[SolveRegisterId]>,
        destination: SolveRegisterId,
        body: Box<SolveProgramRegionWire>,
    },
    Fold {
        domain: StructuredIndexDomain,
        initial: Box<[SolveRegisterId]>,
        captures: Box<[SolveRegisterId]>,
        state_captures: Box<[SolveAssignedStateId]>,
        destinations: Box<[SolveRegisterId]>,
        transition: Box<SolveProgramRegionWire>,
        #[serde(default)]
        continuation: Option<Box<SolveProgramRegionWire>>,
    },
    Scale {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
    },
    BroadcastBinary {
        destination: SolveRegisterId,
        operator: SolveBinaryOperator,
        aggregate: SolveRegisterId,
        scalar: SolveRegisterId,
        scalar_on_lhs: bool,
    },
    Transpose {
        destination: SolveRegisterId,
        operand: SolveRegisterId,
    },
    MatrixMultiply {
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Cross {
        destination: SolveRegisterId,
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Reduce {
        destination: SolveRegisterId,
        operator: SolveReductionOperator,
        operand: SolveRegisterId,
    },
    Identity {
        destination: SolveRegisterId,
    },
    Diagonal {
        destination: SolveRegisterId,
        operand: SolveRegisterId,
    },
    Concatenate {
        destination: SolveRegisterId,
        axis: u32,
        operands: Box<[SolveRegisterId]>,
    },
    Fill {
        destination: SolveRegisterId,
        value: SolveRegisterId,
    },
    ConstructAggregate {
        destination: SolveRegisterId,
        elements: Box<[SolveRegisterId]>,
    },
    ProjectElement {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: Box<[u32]>,
    },
    ProjectElementDynamic {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: Box<[SolveRegisterId]>,
    },
    ProjectSlice {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        origin: Box<[u32]>,
    },
    ProjectView {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        axes: Box<[SolveTensorViewAxis]>,
    },
    SelectElement {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        indices: Box<[SolveRegisterId]>,
        out_of_range: SolveRegisterId,
    },
    UpdateElement {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        indices: Box<[SolveRegisterId]>,
    },
    UpdateSlice {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        origin: Box<[u32]>,
    },
    UpdateView {
        destination: SolveRegisterId,
        aggregate: SolveRegisterId,
        value: SolveRegisterId,
        axes: Box<[SolveTensorViewAxis]>,
    },
    Call {
        assertion_forwarding: Box<[AssertionForwardingWire]>,
        owner: SolvePureCallOwnerId,
        arguments: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
    },
    LinearSolve {
        destination: SolveRegisterId,
        matrix: SolveRegisterId,
        rhs: SolveRegisterId,
    },
    Native {
        body: NativeBody,
        operands: Box<[SolveRegisterId]>,
        destinations: Box<[SolveRegisterId]>,
    },
}

#[derive(Clone, Deserialize)]
struct SolveProgramRegionWire {
    inputs: Box<[SolveValueType]>,
    outputs: Box<[SolveValueType]>,
    body: TypedProgramWire,
    provenance: Span,
}

#[derive(Clone, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum SolveAssertionMessageWire {
    NoCaptures,
    Captures {
        program: Box<SolveProgramRegionWire>,
    },
}

impl<'de> Deserialize<'de> for TypedProgram {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = TypedProgramWire::deserialize(deserializer)?;
        replay_program(&wire, SolvePureCallTableView::default()).map_err(serde::de::Error::custom)
    }
}

pub(in crate::typed_program) fn replay_program(
    wire: &TypedProgramWire,
    available_calls: SolvePureCallTableView<'_>,
) -> Result<TypedProgram, SolveProgramConstructionError> {
    replay_owner_program(wire, available_calls, &[])
}

pub(in crate::typed_program) fn replay_owner_program(
    wire: &TypedProgramWire,
    available_calls: SolvePureCallTableView<'_>,
    assertion_outputs: &[SolvePureCallOutput],
) -> Result<TypedProgram, SolveProgramConstructionError> {
    replay_program_interface(wire, available_calls, assertion_outputs, None)
}

fn replay_program_interface(
    wire: &TypedProgramWire,
    available_calls: SolvePureCallTableView<'_>,
    assertion_outputs: &[SolvePureCallOutput],
    region_provenance: Option<Span>,
) -> Result<TypedProgram, SolveProgramConstructionError> {
    if region_provenance.is_none()
        && (!wire.assigned_states.inputs.is_empty() || !wire.assigned_states.outputs.is_empty())
    {
        return Err(SolveProgramConstructionError::WireMismatch);
    }
    let program = TypedProgram::construct_with_assigned_interface(
        wire.arithmetic,
        available_calls,
        assertion_outputs,
        wire.assigned_states.inputs.clone(),
        wire.assigned_states.outputs.clone(),
        region_provenance,
        |builder| {
            // A wire names every register it issued, so replay issues each
            // operation exactly as recorded.
            builder.numbered_values = false;
            let slots = replay_slots(builder, &wire.slots)?;
            let mut registers = Vec::with_capacity(wire.register_types.len());
            for operation in &wire.operations {
                registers.extend(replay_operation(
                    builder, &slots, &registers, wire, operation,
                )?);
            }
            if registers.len() != wire.register_types.len() {
                return Err(SolveProgramConstructionError::WireMismatch);
            }
            Ok(())
        },
    )?;
    if program.slots != expected_slots(&wire.slots)
        || program.register_types.as_ref() != wire.register_types
        || program.operations.len() != wire.operations.len()
        || program.assigned_states.types != wire.assigned_states.types
    {
        return Err(SolveProgramConstructionError::WireMismatch);
    }
    Ok(program)
}

fn replay_slots<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    wire: &[SolveSlotWire],
) -> Result<Vec<ProgramSlot<'program>>, SolveProgramConstructionError> {
    wire.iter()
        .enumerate()
        .map(|(index, slot)| {
            if slot.id.0 as usize != index {
                return Err(SolveProgramConstructionError::WireMismatch);
            }
            builder.declare_slot(
                slot.value_type.clone(),
                slot.storage,
                slot.access,
                slot.provenance,
            )
        })
        .collect()
}

// SPEC_0021: Exception — one exhaustive match replays every serialized typed
// operation through its corresponding checked builder method.
// SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
#[allow(clippy::too_many_lines)]
fn replay_operation<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    slots: &[ProgramSlot<'program>],
    registers: &[ProgramRegister<'program>],
    wire: &TypedProgramWire,
    spanned: &SolveSpannedOperationWire,
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let at = spanned.provenance;
    let result = match &spanned.operation {
        SolveOperationWire::AssignedState { operation } => {
            return replay_assigned_operation(builder, registers, wire, operation, at);
        }
        SolveOperationWire::AssignedConditional { program } => {
            return replay_assigned_conditional(builder, registers, wire, program, at);
        }
        SolveOperationWire::CheckAssertion {
            predicate_output,
            message_outputs,
            condition,
            captures,
            destinations,
            message,
        } => {
            let assertion = builder.assertion_output(*predicate_output, at)?;
            if builder.message_outputs(*predicate_output).as_slice() != message_outputs.as_ref() {
                return Err(SolveProgramConstructionError::WireMismatch);
            }
            let captures = captures
                .iter()
                .map(|capture| register_at(registers, *capture))
                .collect::<Result<Vec<_>, _>>()?;
            let message = match message {
                SolveAssertionMessageWire::NoCaptures => SolveAssertionMessage::NoCaptures,
                SolveAssertionMessageWire::Captures { program } => {
                    SolveAssertionMessage::Captures {
                        program: Box::new(replay_region(
                            program,
                            builder.available_calls,
                            builder.assertion_outputs,
                        )?),
                    }
                }
            };
            let actual = builder.check_assertion_from_region(
                assertion,
                register_at(registers, *condition)?,
                &captures,
                message,
                at,
            )?;
            require_destinations(&actual, destinations, registers, wire)?;
            return Ok(actual);
        }
        SolveOperationWire::Constant { destination, value } => produced(
            *destination,
            builder.constant(value.clone(), at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::Load { destination, slot } => produced(
            *destination,
            builder.load(slot_at(slots, *slot)?, at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::Store { slot, source } => {
            builder.store(slot_at(slots, *slot)?, register_at(registers, *source)?, at)?;
            return Ok(Vec::new());
        }
        SolveOperationWire::Unary {
            destination,
            operator,
            operand,
        } => produced(
            *destination,
            builder.unary(*operator, register_at(registers, *operand)?, at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::Binary {
            destination,
            operator,
            lhs,
            rhs,
        } => produced(
            *destination,
            builder.binary(
                *operator,
                register_at(registers, *lhs)?,
                register_at(registers, *rhs)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Compare {
            destination,
            operator,
            lhs,
            rhs,
        } => produced(
            *destination,
            builder.compare(
                *operator,
                register_at(registers, *lhs)?,
                register_at(registers, *rhs)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Convert {
            destination,
            operator,
            operand,
        } => produced(
            *destination,
            builder.convert(*operator, register_at(registers, *operand)?, at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::Select {
            destination,
            condition,
            if_true,
            if_false,
        } => produced(
            *destination,
            builder.select(
                register_at(registers, *condition)?,
                register_at(registers, *if_true)?,
                register_at(registers, *if_false)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Conditional {
            condition,
            captures,
            destinations,
            if_true,
            if_false,
        } => {
            let captures = captures
                .iter()
                .map(|capture| register_at(registers, *capture))
                .collect::<Result<Vec<_>, _>>()?;
            let if_true =
                replay_region(if_true, builder.available_calls, builder.assertion_outputs)?;
            let if_false =
                replay_region(if_false, builder.available_calls, builder.assertion_outputs)?;
            let actual = builder.conditional_from_regions(
                register_at(registers, *condition)?,
                &captures,
                if_true,
                if_false,
                at,
            )?;
            require_destinations(&actual, destinations, registers, wire)?;
            return Ok(actual);
        }
        SolveOperationWire::Map {
            domain,
            captures,
            destination,
            body,
        } => {
            let captures = captures
                .iter()
                .map(|capture| register_at(registers, *capture))
                .collect::<Result<Vec<_>, _>>()?;
            let body = replay_region(body, builder.available_calls, builder.assertion_outputs)?;
            produced(
                *destination,
                builder.map_from_region(domain.clone(), &captures, body, at)?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::Fold {
            domain,
            initial,
            captures,
            state_captures,
            destinations,
            transition,
            continuation,
        } => {
            let initial = initial
                .iter()
                .map(|value| register_at(registers, *value))
                .collect::<Result<Vec<_>, _>>()?;
            let captures = captures
                .iter()
                .map(|capture| register_at(registers, *capture))
                .collect::<Result<Vec<_>, _>>()?;
            let state_captures = state_captures
                .iter()
                .map(|state| assigned_at(builder, *state, at))
                .collect::<Result<Vec<_>, _>>()?;
            let transition = replay_region(
                transition,
                builder.available_calls,
                builder.assertion_outputs,
            )?;
            let continuation = continuation
                .as_ref()
                .map(|predicate| {
                    replay_region(
                        predicate,
                        builder.available_calls,
                        builder.assertion_outputs,
                    )
                })
                .transpose()?;
            let actual = builder.fold_from_region(
                domain.clone(),
                &initial,
                (&captures, &state_captures),
                transition,
                continuation,
                at,
            )?;
            require_destinations(&actual, destinations, registers, wire)?;
            return Ok(actual);
        }
        SolveOperationWire::Scale {
            destination,
            aggregate,
            scalar,
        } => produced(
            *destination,
            builder.scale(
                register_at(registers, *aggregate)?,
                register_at(registers, *scalar)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::BroadcastBinary {
            destination,
            operator,
            aggregate,
            scalar,
            scalar_on_lhs,
        } => produced(
            *destination,
            builder.broadcast_binary(
                *operator,
                register_at(registers, *aggregate)?,
                register_at(registers, *scalar)?,
                *scalar_on_lhs,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Transpose {
            destination,
            operand,
        } => produced(
            *destination,
            builder.transpose(register_at(registers, *operand)?, at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::MatrixMultiply {
            destination,
            lhs,
            rhs,
        } => produced(
            *destination,
            builder.matrix_multiply(
                register_at(registers, *lhs)?,
                register_at(registers, *rhs)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::LinearSolve {
            destination,
            matrix,
            rhs,
        } => produced(
            *destination,
            builder.linear_solve(
                register_at(registers, *matrix)?,
                register_at(registers, *rhs)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Cross {
            destination,
            lhs,
            rhs,
        } => produced(
            *destination,
            builder.cross(
                register_at(registers, *lhs)?,
                register_at(registers, *rhs)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Reduce {
            destination,
            operator,
            operand,
        } => produced(
            *destination,
            builder.reduce(*operator, register_at(registers, *operand)?, at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::Identity { destination } => produced(
            *destination,
            builder.identity(
                destination_type(wire, *destination)?.element_type(),
                destination_type(wire, *destination)?
                    .dimensions()
                    .first()
                    .copied()
                    .ok_or(SolveProgramConstructionError::WireMismatch)?,
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::Diagonal {
            destination,
            operand,
        } => produced(
            *destination,
            builder.diagonal(register_at(registers, *operand)?, at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::Concatenate {
            destination,
            axis,
            operands,
        } => {
            let operands = operands
                .iter()
                .map(|operand| register_at(registers, *operand))
                .collect::<Result<Vec<_>, _>>()?;
            produced(
                *destination,
                builder.concatenate(*axis, &operands, at)?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::Fill { destination, value } => produced(
            *destination,
            builder.fill(
                register_at(registers, *value)?,
                destination_type(wire, *destination)?.dimensions().to_vec(),
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::ConstructAggregate {
            destination,
            elements,
        } => {
            let elements = elements
                .iter()
                .map(|element| register_at(registers, *element))
                .collect::<Result<Vec<_>, _>>()?;
            produced(
                *destination,
                builder.construct_aggregate(
                    &elements,
                    destination_type(wire, *destination)?.dimensions().to_vec(),
                    at,
                )?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::ProjectElement {
            destination,
            aggregate,
            indices,
        } => produced(
            *destination,
            builder.project_element(register_at(registers, *aggregate)?, indices.to_vec(), at)?,
            registers,
            wire,
        )?,
        SolveOperationWire::ProjectElementDynamic {
            destination,
            aggregate,
            indices,
        } => {
            let indices = indices
                .iter()
                .map(|index| register_at(registers, *index))
                .collect::<Result<Vec<_>, _>>()?;
            produced(
                *destination,
                builder.project_element_dynamic(
                    register_at(registers, *aggregate)?,
                    &indices,
                    at,
                )?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::ProjectSlice {
            destination,
            aggregate,
            origin,
        } => produced(
            *destination,
            builder.project_slice(
                register_at(registers, *aggregate)?,
                origin.to_vec(),
                destination_type(wire, *destination)?.dimensions().to_vec(),
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::ProjectView {
            destination,
            aggregate,
            axes,
        } => {
            let axes = replay_view_axes(registers, axes)?;
            produced(
                *destination,
                builder.project_view(register_at(registers, *aggregate)?, &axes, at)?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::SelectElement {
            destination,
            aggregate,
            indices,
            out_of_range,
        } => {
            let indices = indices
                .iter()
                .map(|index| register_at(registers, *index))
                .collect::<Result<Vec<_>, _>>()?;
            produced(
                *destination,
                builder.select_element(
                    register_at(registers, *aggregate)?,
                    &indices,
                    register_at(registers, *out_of_range)?,
                    at,
                )?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::UpdateElement {
            destination,
            aggregate,
            value,
            indices,
        } => {
            let indices = indices
                .iter()
                .map(|index| register_at(registers, *index))
                .collect::<Result<Vec<_>, _>>()?;
            produced(
                *destination,
                builder.update_element(
                    register_at(registers, *aggregate)?,
                    register_at(registers, *value)?,
                    &indices,
                    at,
                )?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::UpdateSlice {
            destination,
            aggregate,
            value,
            origin,
        } => produced(
            *destination,
            builder.update_slice(
                register_at(registers, *aggregate)?,
                register_at(registers, *value)?,
                origin.to_vec(),
                at,
            )?,
            registers,
            wire,
        )?,
        SolveOperationWire::UpdateView {
            destination,
            aggregate,
            value,
            axes,
        } => {
            let axes = replay_view_axes(registers, axes)?;
            produced(
                *destination,
                builder.update_view(
                    register_at(registers, *aggregate)?,
                    register_at(registers, *value)?,
                    &axes,
                    at,
                )?,
                registers,
                wire,
            )?
        }
        SolveOperationWire::Call {
            assertion_forwarding,
            owner,
            arguments,
            destinations,
        } => {
            let arguments = arguments
                .iter()
                .map(|argument| register_at(registers, *argument))
                .collect::<Result<Vec<_>, _>>()?;
            let call = builder.emit_call(*owner, &arguments, at)?;
            require_destinations(call.registers(), destinations, registers, wire)?;
            for forwarding in assertion_forwarding {
                let parent = builder.assertion_output(forwarding.parent_predicate, at)?;
                builder.forward_assertion(&call, forwarding.child_predicate, parent, at)?;
            }
            let actual = call.into_registers();
            require_destinations(&actual, destinations, registers, wire)?;
            return Ok(actual);
        }
        SolveOperationWire::Native {
            body,
            operands,
            destinations,
        } => {
            let operands = operands
                .iter()
                .map(|operand| register_at(registers, *operand))
                .collect::<Result<Vec<_>, _>>()?;
            let actual = builder.native(*body, &operands, at)?;
            require_destinations(&actual, destinations, registers, wire)?;
            return Ok(actual);
        }
    };
    Ok(vec![result])
}

fn produced<'program>(
    expected: SolveRegisterId,
    actual: ProgramRegister<'program>,
    prior: &[ProgramRegister<'program>],
    wire: &TypedProgramWire,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    if expected.0 as usize != prior.len()
        || actual.id != expected
        || wire.register_types.get(expected.0 as usize).is_none()
    {
        return Err(SolveProgramConstructionError::WireMismatch);
    }
    Ok(actual)
}

fn require_destinations(
    actual: &[ProgramRegister<'_>],
    expected: &[SolveRegisterId],
    prior: &[ProgramRegister<'_>],
    wire: &TypedProgramWire,
) -> Result<(), SolveProgramConstructionError> {
    if actual.len() != expected.len() {
        return Err(SolveProgramConstructionError::WireMismatch);
    }
    for (offset, (actual, expected)) in actual.iter().zip(expected).enumerate() {
        if expected.0 as usize != prior.len() + offset
            || actual.id != *expected
            || wire.register_types.get(expected.0 as usize).is_none()
        {
            return Err(SolveProgramConstructionError::WireMismatch);
        }
    }
    Ok(())
}

fn replay_region(
    wire: &SolveProgramRegionWire,
    available_calls: SolvePureCallTableView<'_>,
    assertion_outputs: &[SolvePureCallOutput],
) -> Result<SolveProgramRegion, SolveProgramConstructionError> {
    let body = replay_program_interface(
        &wire.body,
        available_calls,
        assertion_outputs,
        Some(wire.provenance),
    )?;
    construct_region(
        wire.inputs.to_vec(),
        wire.outputs.to_vec(),
        body,
        wire.provenance,
    )
}

fn slot_at<'program>(
    slots: &[ProgramSlot<'program>],
    id: SolveSlotId,
) -> Result<ProgramSlot<'program>, SolveProgramConstructionError> {
    slots
        .get(id.0 as usize)
        .copied()
        .ok_or(SolveProgramConstructionError::WireMismatch)
}

fn register_at<'program>(
    registers: &[ProgramRegister<'program>],
    id: SolveRegisterId,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    registers
        .get(id.0 as usize)
        .copied()
        .ok_or(SolveProgramConstructionError::WireMismatch)
}

fn replay_view_axes<'program>(
    registers: &[ProgramRegister<'program>],
    axes: &[SolveTensorViewAxis],
) -> Result<Vec<ProgramTensorViewAxis<'program>>, SolveProgramConstructionError> {
    axes.iter()
        .map(|axis| match *axis {
            SolveTensorViewAxis::Index(index) => {
                register_at(registers, index).map(ProgramTensorViewAxis::Index)
            }
            SolveTensorViewAxis::Span { origin, extent } => {
                Ok(ProgramTensorViewAxis::Span { origin, extent })
            }
        })
        .collect()
}

fn destination_type(
    wire: &TypedProgramWire,
    id: SolveRegisterId,
) -> Result<&SolveValueType, SolveProgramConstructionError> {
    wire.register_types
        .get(id.0 as usize)
        .ok_or(SolveProgramConstructionError::WireMismatch)
}

fn expected_slots(wire: &[SolveSlotWire]) -> Box<[SolveSlot]> {
    wire.iter()
        .map(|slot| SolveSlot {
            id: slot.id,
            value_type: slot.value_type.clone(),
            storage: slot.storage,
            access: slot.access,
            provenance: slot.provenance,
        })
        .collect()
}

fn assigned_at<'program>(
    builder: &TypedProgramBuilder<'program>,
    id: SolveAssignedStateId,
    at: Span,
) -> Result<ProgramAssignedState<'program>, SolveProgramConstructionError> {
    builder.replay_assigned_state(id, at)
}

fn require_assigned_destination(
    actual: ProgramAssignedState<'_>,
    expected: SolveAssignedStateId,
) -> Result<(), SolveProgramConstructionError> {
    if actual.id != expected {
        return Err(SolveProgramConstructionError::WireMismatch);
    }
    Ok(())
}

fn replay_assigned_operation<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    registers: &[ProgramRegister<'program>],
    wire: &TypedProgramWire,
    operation: &AssignedStateOperationWire,
    at: Span,
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    match operation {
        AssignedStateOperationWire::Absent {
            destination,
            payload_type,
        } => {
            let actual = builder.absent_assigned_state(payload_type.clone(), at)?;
            require_assigned_destination(actual, *destination)?;
        }
        AssignedStateOperationWire::Present {
            destination,
            source,
        } => {
            let actual = builder.present_assigned_state(register_at(registers, *source)?, at)?;
            require_assigned_destination(actual, *destination)?;
        }
        AssignedStateOperationWire::Input { destination, input } => {
            let actual = builder.assigned_input(*input, at)?;
            require_assigned_destination(actual, *destination)?;
        }
        AssignedStateOperationWire::Output { output, source } => {
            let state = assigned_at(builder, *source, at)?;
            builder.store_assigned_state(*output, state, at)?;
        }
        AssignedStateOperationWire::IsPresent {
            destination,
            source,
        } => {
            let state = assigned_at(builder, *source, at)?;
            let actual = builder.assigned_state_is_present(state, at)?;
            return Ok(vec![produced(*destination, actual, registers, wire)?]);
        }
        AssignedStateOperationWire::AllPresent {
            destination,
            sources,
        } => {
            let states = sources
                .iter()
                .map(|source| assigned_at(builder, *source, at))
                .collect::<Result<Vec<_>, _>>()?;
            let actual = builder.assigned_states_all_present(&states, at)?;
            return Ok(vec![produced(*destination, actual, registers, wire)?]);
        }
        AssignedStateOperationWire::ProjectChecked {
            destination,
            source,
            predicate_output,
        } => {
            let state = assigned_at(builder, *source, at)?;
            let actual = builder.project_checked_assigned_state(state, *predicate_output, at)?;
            return Ok(vec![produced(*destination, actual, registers, wire)?]);
        }
    }
    Ok(Vec::new())
}

fn replay_assigned_conditional<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    registers: &[ProgramRegister<'program>],
    wire: &TypedProgramWire,
    source: &AssignedConditionalWire,
    at: Span,
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let captures = source
        .captures
        .iter()
        .map(|id| register_at(registers, *id))
        .collect::<Result<Vec<_>, _>>()?;
    let states = source
        .state_captures
        .iter()
        .map(|id| assigned_at(builder, *id, at))
        .collect::<Result<Vec<_>, _>>()?;
    let if_true = replay_region(
        &source.if_true,
        builder.available_calls,
        builder.assertion_outputs,
    )?;
    let if_false = replay_region(
        &source.if_false,
        builder.available_calls,
        builder.assertion_outputs,
    )?;
    let (actual, actual_states) = builder.conditional_assigned_from_regions(
        register_at(registers, source.condition)?,
        &captures,
        &states,
        if_true,
        if_false,
        at,
    )?;
    require_destinations(&actual, &source.destinations, registers, wire)?;
    if actual_states.len() != source.state_destinations.len() {
        return Err(SolveProgramConstructionError::WireMismatch);
    }
    for (actual, expected) in actual_states.iter().zip(&source.state_destinations) {
        require_assigned_destination(*actual, *expected)?;
    }
    Ok(actual)
}
