//! Canonical store projections and exact Integer sources of discrete outputs.

use super::*;

/// Borrow the canonical program of a discrete row and locate its stored value.
/// Shared call outputs retain one operation list and canonical operation indices.
pub(in crate::refresh::native_assignment) fn row_program(
    rhs: &ScalarProgramBlock,
    row: usize,
) -> Result<(usize, &[LinearOp], Reg, usize), NativeEvaluationRefusal> {
    let (program, ordinal) = rhs
        .output_position(row)
        .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
    let operations = rhs
        .program(program)
        .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
    let mut stored = 0usize;
    for (position, operation) in operations.iter().enumerate() {
        let (start, count, stride) = match *operation {
            LinearOp::StoreOutput { src } => (src, 1, 1),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => (start, count, stride),
            _ => continue,
        };
        if ordinal >= stored && ordinal - stored < count {
            let register = (ordinal - stored)
                .checked_mul(stride)
                .and_then(|offset| u32::try_from(offset).ok())
                .and_then(|offset| start.checked_add(offset))
                .ok_or(NativeEvaluationRefusal::MultiOutputDiscreteProgram)?;
            return Ok((program, operations, register, position));
        }
        stored = stored
            .checked_add(count)
            .ok_or(NativeEvaluationRefusal::MultiOutputDiscreteProgram)?;
    }
    Err(NativeEvaluationRefusal::MultiOutputDiscreteProgram)
}

/// The exact Integer source of `row`: the defining operation of its stored
/// register must be an Integer pure-call output cell, a load of a typed
/// Integer input, or an integral literal within the exact Binary64 range.
pub(super) fn integer_source(
    rhs: &ScalarProgramBlock,
    row: usize,
    inputs: &super::typed_inputs::TypedInputs,
) -> Result<NativeIntegerSource, NativeEvaluationRefusal> {
    let (_, operations, value, stored_at) = row_program(rhs, row)?;
    let Some((operation, definition)) = operations[..stored_at]
        .iter()
        .enumerate()
        .rev()
        .find(|(_, op)| defines(op, value))
    else {
        return Err(NativeEvaluationRefusal::IntegerComputedInReal);
    };
    match definition {
        LinearOp::Const { value, .. }
            if value.fract() == 0.0 && value.abs() <= 9_007_199_254_740_992.0 =>
        {
            Ok(NativeIntegerSource::Literal(*value as i64))
        }
        LinearOp::LoadP { index, .. } => inputs
            .integer_lane(*index)
            .map(|lane_offset| NativeIntegerSource::Input { lane_offset })
            .ok_or(NativeEvaluationRefusal::IntegerComputedInReal),
        LinearOp::PureCall {
            dst_start, site, ..
        } => {
            let cell = value
                .checked_sub(*dst_start)
                .ok_or(NativeEvaluationRefusal::IntegerComputedInReal)?;
            match cell_type(site, cell) {
                Some(SolveScalarType::Integer(_)) => {
                    Ok(NativeIntegerSource::CallCell { operation, cell })
                }
                _ => Err(NativeEvaluationRefusal::IntegerComputedInReal),
            }
        }
        _ => Err(NativeEvaluationRefusal::IntegerComputedInReal),
    }
}

/// The scalar type of output cell `cell` of a pure call, in scalar order.
fn cell_type(site: &crate::SolvePureCallSite, cell: u32) -> Option<SolveScalarType> {
    let mut first = 0u32;
    for output in site.outputs() {
        let count = output.value_type().scalar_count();
        if cell < first.checked_add(count)? {
            return Some(output.value_type().element_type());
        }
        first += count;
    }
    None
}

fn defines(operation: &LinearOp, register: Reg) -> bool {
    operation.dst_register().is_some_and(|first| {
        register
            .checked_sub(first)
            .is_some_and(|offset| (offset as usize) < operation.dst_register_count())
    })
}
