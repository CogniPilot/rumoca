use rumoca_ir_solve::{LinearOp, TargetAssignmentShape};

use crate::EvalSolveError;

pub(super) fn eval_assignment_shape(
    shape: &TargetAssignmentShape,
    row_idx: usize,
    regs: &[f64],
    span: Option<rumoca_core::Span>,
) -> Result<f64, EvalSolveError> {
    match shape {
        TargetAssignmentShape::Zero { .. } => Ok(0.0),
        TargetAssignmentShape::TensorAffine { .. } => Err(super::invalid_prepared_row(
            "tensor-affine assignments require their prepared materialization",
        )),
        TargetAssignmentShape::Direct { expr_reg, .. } => read_shape_reg(regs, *expr_reg, span),
        TargetAssignmentShape::Affine {
            target_y_index,
            coefficient_reg,
            coefficient_scale,
            ..
        } => {
            let coefficient = match coefficient_reg {
                Some(register) => rumoca_ir_solve::register_coefficient(
                    read_shape_reg(regs, *register, span)?,
                    *coefficient_scale,
                ),
                None => *coefficient_scale,
            };
            if coefficient == 0.0 || !coefficient.is_finite() {
                return Err(EvalSolveError::SingularTargetAssignment {
                    row: row_idx,
                    target_y_index: *target_y_index,
                    coefficient,
                    span,
                });
            }
            isolated_value(shape, regs, span)
        }
        TargetAssignmentShape::Additive {
            target_y_index,
            coefficient,
            ..
        } => {
            if *coefficient == 0.0 || !coefficient.is_finite() {
                return Err(EvalSolveError::SingularTargetAssignment {
                    row: row_idx,
                    target_y_index: *target_y_index,
                    coefficient: *coefficient,
                    span,
                });
            }
            isolated_value(shape, regs, span)
        }
    }
}

/// The isolated value of an affine or additive shape, in the arithmetic of
/// its materialized isolator ([`rumoca_ir_solve::IsolatedValue`]).
fn isolated_value(
    shape: &TargetAssignmentShape,
    regs: &[f64],
    span: Option<rumoca_core::Span>,
) -> Result<f64, EvalSolveError> {
    rumoca_ir_solve::eval_isolated_value(shape, |register| read_shape_reg(regs, register, span))
        .unwrap_or_else(|| {
            Err(super::invalid_prepared_row(
                "only affine and additive shapes have an isolated value",
            ))
        })
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
