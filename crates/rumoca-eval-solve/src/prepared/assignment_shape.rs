use rumoca_ir_solve::{LinearOp, TargetAssignmentShape};

use crate::EvalSolveError;

pub(super) fn eval_assignment_shape(
    shape: TargetAssignmentShape,
    row_idx: usize,
    regs: &[f64],
    span: Option<rumoca_core::Span>,
) -> Result<f64, EvalSolveError> {
    match shape {
        TargetAssignmentShape::Direct { expr_reg, .. } => read_shape_reg(regs, expr_reg, span),
        TargetAssignmentShape::Affine {
            target_y_index,
            offset_reg,
            coefficient_reg,
            offset_scale,
            coefficient_scale,
            ..
        } => {
            let offset = offset_scale * read_shape_reg(regs, offset_reg, span)?;
            let coefficient = coefficient_scale
                * coefficient_reg.map_or(Ok(1.0), |reg| read_shape_reg(regs, reg, span))?;
            if coefficient == 0.0 || !coefficient.is_finite() {
                return Err(EvalSolveError::SingularTargetAssignment {
                    row: row_idx,
                    target_y_index,
                    coefficient,
                    span,
                });
            }
            Ok(-offset / coefficient)
        }
        TargetAssignmentShape::AffineResidual {
            target_y_index,
            target_reg,
            residual_reg,
            coefficient,
            ..
        } => {
            if coefficient == 0.0 || !coefficient.is_finite() {
                return Err(EvalSolveError::SingularTargetAssignment {
                    row: row_idx,
                    target_y_index,
                    coefficient,
                    span,
                });
            }
            let target = read_shape_reg(regs, target_reg, span)?;
            let residual = read_shape_reg(regs, residual_reg, span)?;
            Ok(target - residual / coefficient)
        }
    }
}

/// Recognize the first scalar target assignment owned by one residual row.
pub fn target_assignment_shape(
    row: &[LinearOp],
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    Ok(target_assignment_shapes(row)?.into_iter().next())
}

pub fn target_assignment_shapes(
    row: &[LinearOp],
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    let mut shapes = Vec::new();
    for (_, shape) in target_assignment_shapes_with_output_offsets(row)? {
        if shapes.iter().all(|existing: &TargetAssignmentShape| {
            existing.target_y_index() != shape.target_y_index()
        }) {
            shapes.push(shape);
        }
    }
    Ok(shapes)
}

pub(super) fn target_assignment_shapes_with_output_offsets(
    row: &[LinearOp],
) -> Result<Vec<(usize, TargetAssignmentShape)>, EvalSolveError> {
    Ok(rumoca_ir_solve::derive_target_assignment_shapes(row))
}

fn read_shape_reg(
    regs: &[f64],
    reg: u32,
    span: Option<rumoca_core::Span>,
) -> Result<f64, EvalSolveError> {
    regs.get(reg as usize)
        .copied()
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: reg,
            len: regs.len(),
            span,
        })
}

#[cfg(test)]
pub(super) fn checked_expr_eval_len(pos: usize) -> Result<usize, EvalSolveError> {
    pos.checked_add(1)
        .ok_or_else(|| super::invalid_prepared_row("target assignment expression length overflows"))
}
