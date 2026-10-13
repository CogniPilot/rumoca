//! Closed-vocabulary register visitation of one typed operation.

use super::*;

impl SolveOperation {
    /// Visits every register read by this operation in semantic operand order.
    ///
    /// This is the closed-vocabulary source of truth for checked backend
    /// liveness. Region bodies own separate register namespaces, so structured
    /// operations visit only their captures and other enclosing-program
    /// operands here.
    pub fn visit_input_registers(&self, mut visit: impl FnMut(SolveRegisterId)) {
        match self {
            Self::AssignedConditional { program } => {
                visit_register_then_list(program.condition(), program.captures(), &mut visit)
            }
            Self::AssignedState { operation } => visit_assigned_state_inputs(operation, &mut visit),
            Self::Constant { .. } | Self::Load { .. } | Self::Identity { .. } => {}
            Self::Store { source, .. } => visit(*source),
            Self::Unary { operand, .. }
            | Self::Convert { operand, .. }
            | Self::Transpose { operand, .. }
            | Self::Reduce { operand, .. }
            | Self::Diagonal { operand, .. } => visit(*operand),
            Self::Binary { lhs, rhs, .. }
            | Self::Compare { lhs, rhs, .. }
            | Self::MatrixMultiply { lhs, rhs, .. }
            | Self::Cross { lhs, rhs, .. } => visit_register_pair(*lhs, *rhs, &mut visit),
            Self::LinearSolve { matrix, rhs, .. } => {
                visit_register_pair(*matrix, *rhs, &mut visit);
            }
            Self::Select {
                condition,
                if_true,
                if_false,
                ..
            } => visit_register_then_list(*condition, &[*if_true, *if_false], &mut visit),
            Self::CheckAssertion {
                condition,
                captures,
                ..
            }
            | Self::Conditional {
                condition,
                captures,
                ..
            } => visit_register_then_list(*condition, captures, &mut visit),
            Self::Map { captures, .. } => captures.iter().copied().for_each(&mut visit),
            Self::Fold {
                initial, captures, ..
            } => {
                initial.iter().copied().for_each(&mut visit);
                captures.iter().copied().for_each(&mut visit);
            }
            Self::Scale {
                aggregate, scalar, ..
            }
            | Self::BroadcastBinary {
                aggregate, scalar, ..
            } => visit_register_pair(*aggregate, *scalar, &mut visit),
            Self::Concatenate { operands, .. }
            | Self::ConstructAggregate {
                elements: operands, ..
            } => operands.iter().copied().for_each(&mut visit),
            Self::Fill { value, .. } => visit(*value),
            Self::ProjectElement { aggregate, .. } | Self::ProjectSlice { aggregate, .. } => {
                visit(*aggregate)
            }
            Self::ProjectElementDynamic {
                aggregate, indices, ..
            } => visit_register_then_list(*aggregate, indices, &mut visit),
            Self::ProjectView {
                aggregate, axes, ..
            } => {
                visit(*aggregate);
                visit_view_axis_registers(axes, &mut visit);
            }
            Self::SelectElement {
                aggregate,
                indices,
                out_of_range,
                ..
            } => {
                visit_register_then_list(*aggregate, indices, &mut visit);
                visit(*out_of_range);
            }
            Self::UpdateElement {
                aggregate,
                value,
                indices,
                ..
            } => visit_register_pair_then_list(*aggregate, *value, indices, &mut visit),
            Self::UpdateSlice {
                aggregate, value, ..
            } => visit_register_pair(*aggregate, *value, &mut visit),
            Self::UpdateView {
                aggregate,
                value,
                axes,
                ..
            } => {
                visit(*aggregate);
                visit(*value);
                visit_view_axis_registers(axes, &mut visit);
            }
            Self::Call { arguments, .. }
            | Self::Native {
                operands: arguments,
                ..
            } => arguments.iter().copied().for_each(&mut visit),
        }
    }

    /// Visits every register defined by this operation in result-tuple order.
    pub fn visit_output_registers(&self, mut visit: impl FnMut(SolveRegisterId)) {
        match self {
            Self::AssignedConditional { program } => {
                program.destinations().iter().copied().for_each(&mut visit)
            }
            Self::AssignedState { operation } => match operation {
                SolveAssignedStateOperation::IsPresent { destination, .. }
                | SolveAssignedStateOperation::AllPresent { destination, .. }
                | SolveAssignedStateOperation::ProjectChecked { destination, .. } => {
                    visit(*destination)
                }
                _ => {}
            },
            Self::Store { .. } => {}
            Self::Constant { destination, .. }
            | Self::Load { destination, .. }
            | Self::Unary { destination, .. }
            | Self::Binary { destination, .. }
            | Self::Compare { destination, .. }
            | Self::Convert { destination, .. }
            | Self::Select { destination, .. }
            | Self::Map { destination, .. }
            | Self::MatrixMultiply { destination, .. }
            | Self::LinearSolve { destination, .. }
            | Self::Scale { destination, .. }
            | Self::BroadcastBinary { destination, .. }
            | Self::Transpose { destination, .. }
            | Self::Cross { destination, .. }
            | Self::Reduce { destination, .. }
            | Self::Identity { destination }
            | Self::Diagonal { destination, .. }
            | Self::Concatenate { destination, .. }
            | Self::Fill { destination, .. }
            | Self::ConstructAggregate { destination, .. }
            | Self::ProjectElement { destination, .. }
            | Self::ProjectElementDynamic { destination, .. }
            | Self::ProjectSlice { destination, .. }
            | Self::ProjectView { destination, .. }
            | Self::SelectElement { destination, .. }
            | Self::UpdateElement { destination, .. }
            | Self::UpdateSlice { destination, .. }
            | Self::UpdateView { destination, .. } => visit(*destination),
            Self::CheckAssertion { destinations, .. }
            | Self::Conditional { destinations, .. }
            | Self::Fold { destinations, .. }
            | Self::Call { destinations, .. }
            | Self::Native { destinations, .. } => {
                destinations.iter().copied().for_each(&mut visit);
            }
        }
    }
}

fn visit_view_axis_registers(
    axes: &[SolveTensorViewAxis],
    visit: &mut impl FnMut(SolveRegisterId),
) {
    for axis in axes {
        if let SolveTensorViewAxis::Index(register) = axis {
            visit(*register);
        }
    }
}

/// Visits two operand registers in semantic order.
fn visit_register_pair(
    first: SolveRegisterId,
    second: SolveRegisterId,
    visit: &mut impl FnMut(SolveRegisterId),
) {
    visit(first);
    visit(second);
}

/// Visits one leading operand register, then a compact operand list.
fn visit_register_then_list(
    first: SolveRegisterId,
    registers: &[SolveRegisterId],
    visit: &mut impl FnMut(SolveRegisterId),
) {
    visit(first);
    registers.iter().copied().for_each(visit);
}

/// Visits two leading operand registers, then a compact operand list.
fn visit_register_pair_then_list(
    first: SolveRegisterId,
    second: SolveRegisterId,
    registers: &[SolveRegisterId],
    visit: &mut impl FnMut(SolveRegisterId),
) {
    visit(first);
    visit(second);
    registers.iter().copied().for_each(visit);
}

fn visit_assigned_state_inputs(
    operation: &SolveAssignedStateOperation,
    visit: &mut impl FnMut(SolveRegisterId),
) {
    if let SolveAssignedStateOperation::Present { source, .. } = operation {
        visit(*source);
    }
}
