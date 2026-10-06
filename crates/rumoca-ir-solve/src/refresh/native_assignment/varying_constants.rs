//! Fixed target coefficients from exact original scalar producers.

use super::*;
use crate::BinaryOp;
use crate::refresh::assignment_shape::producers::UniqueProgram;

/// A bare target load in the terminal subtraction has coefficient +/-1 in
/// every domain tuple. Varying constants can only affect the independent value
/// or retained prefix; no base-tuple numeric coefficient is reused as proof.
pub(super) fn fixed_target_coefficient(
    operations: &[LinearOp],
    shape: &TargetAssignmentShape,
    target: usize,
) -> bool {
    let Some((LinearOp::StoreOutput { src }, prefix)) = operations.split_last() else {
        return false;
    };
    let Some(producers) = UniqueProgram::new(prefix) else {
        return false;
    };
    let program = producers.view();
    let producer = |reg| {
        program
            .producer_position(reg)
            .and_then(|position| program.operation(position))
    };
    let target_load = |reg| {
        matches!(producer(reg), Some(LinearOp::LoadY { dst, index })
            if *dst == reg && *index == target)
    };
    match shape {
        TargetAssignmentShape::Zero { target_y_index, .. } => {
            *target_y_index == target && target_load(*src)
        }
        TargetAssignmentShape::Direct {
            target_y_index,
            expr_reg,
            target_scale,
            ..
        } => {
            let Some(LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            }) = producer(*src)
            else {
                return false;
            };
            *target_y_index == target
                && ((target_load(*lhs) && *expr_reg == *rhs && *target_scale == 1.0)
                    || (target_load(*rhs) && *expr_reg == *lhs && *target_scale == -1.0))
        }
        _ => false,
    }
}
