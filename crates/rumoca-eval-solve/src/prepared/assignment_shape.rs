use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock, UnaryOp};

use super::dependency::{YDependencyAnalyzer, reg_depends_on_y_index};
use super::{invalid_prepared_row, producer};
use crate::EvalSolveError;

/// Scalar Solve-IR row shape that can update one solver-Y slot directly.
///
/// Code generators use the same analysis as the interpreter so compiled
/// projection sweeps preserve assignment-row semantics.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TargetAssignmentShape {
    Direct {
        target_y_index: usize,
        expr_reg: u32,
        target_scale: f64,
        expr_eval_len: usize,
    },
    Affine {
        target_y_index: usize,
        offset_reg: u32,
        coefficient_reg: Option<u32>,
        offset_scale: f64,
        coefficient_scale: f64,
        expr_eval_len: usize,
    },
    AffineResidual {
        target_y_index: usize,
        target_reg: u32,
        residual_reg: u32,
        coefficient: f64,
        expr_eval_len: usize,
    },
}

impl TargetAssignmentShape {
    pub fn target_y_index(self) -> usize {
        match self {
            Self::Direct { target_y_index, .. }
            | Self::Affine { target_y_index, .. }
            | Self::AffineResidual { target_y_index, .. } => target_y_index,
        }
    }

    pub fn expr_eval_len(self) -> usize {
        match self {
            Self::Direct { expr_eval_len, .. }
            | Self::Affine { expr_eval_len, .. }
            | Self::AffineResidual { expr_eval_len, .. } => expr_eval_len,
        }
    }

    pub(super) fn eval_value(
        self,
        row_idx: usize,
        regs: &[f64],
        span: Option<rumoca_core::Span>,
    ) -> Result<f64, EvalSolveError> {
        match self {
            Self::Direct { expr_reg, .. } => read_shape_reg(regs, expr_reg, span),
            Self::Affine {
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
            Self::AffineResidual {
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
}

/// Recognize a scalar row that can be evaluated as `target = expression`.
pub fn target_assignment_shape(
    row: &[LinearOp],
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    Ok(target_assignment_shapes(row)?.into_iter().next())
}

pub fn target_assignment_shapes(
    row: &[LinearOp],
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    if ScalarProgramBlock::program_output_count(row) > 1 {
        return Ok(Vec::new());
    }
    let Some(output_reg) = store_output_reg(row) else {
        return Ok(Vec::new());
    };
    let mut shapes = Vec::new();
    if let Some(shape) = direct_assignment_shape(row, output_reg)? {
        shapes.push(shape);
    }
    for shape in affine_assignment_shapes(row, output_reg)? {
        if shapes
            .iter()
            .all(|existing| existing.target_y_index() != shape.target_y_index())
        {
            shapes.push(shape);
        }
    }
    for shape in affine_residual_shapes(row, output_reg)? {
        if shapes
            .iter()
            .all(|existing| existing.target_y_index() != shape.target_y_index())
        {
            shapes.push(shape);
        }
    }
    Ok(shapes)
}

fn affine_residual_shapes(
    row: &[LinearOp],
    residual_reg: u32,
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    let Some(residual_pos) = producer_pos(row, residual_reg) else {
        return Ok(Vec::new());
    };
    let expr_eval_len = checked_expr_eval_len(residual_pos)?;
    let mut targets = Vec::new();
    let mut dependencies = YDependencyAnalyzer::new(row, 0);
    for op in row {
        let LinearOp::LoadY {
            dst: target_reg,
            index: target_y_index,
        } = *op
        else {
            continue;
        };
        if targets
            .iter()
            .any(|shape: &TargetAssignmentShape| shape.target_y_index() == target_y_index)
        {
            continue;
        }
        dependencies.set_target(target_y_index);
        let Some(coefficient) =
            additive_target_coefficient(row, residual_reg, target_y_index, &mut dependencies)
        else {
            continue;
        };
        if coefficient == 0.0 || !coefficient.is_finite() {
            continue;
        }
        targets.push(TargetAssignmentShape::AffineResidual {
            target_y_index,
            target_reg,
            residual_reg,
            coefficient,
            expr_eval_len,
        });
    }
    Ok(targets)
}

fn additive_target_coefficient(
    row: &[LinearOp],
    reg: u32,
    target_y_index: usize,
    dependencies: &mut YDependencyAnalyzer<'_>,
) -> Option<f64> {
    if !dependencies.depends_on(reg) {
        return Some(0.0);
    }
    match producer(row, reg)? {
        LinearOp::LoadY { index, .. } if *index == target_y_index => Some(1.0),
        LinearOp::Move { src, .. } => {
            additive_target_coefficient(row, *src, target_y_index, dependencies)
        }
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => {
            additive_target_coefficient(row, *arg, target_y_index, dependencies).map(|value| -value)
        }
        LinearOp::Binary {
            op: BinaryOp::Add,
            lhs,
            rhs,
            ..
        } => Some(
            additive_target_coefficient(row, *lhs, target_y_index, dependencies)?
                + additive_target_coefficient(row, *rhs, target_y_index, dependencies)?,
        ),
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => Some(
            additive_target_coefficient(row, *lhs, target_y_index, dependencies)?
                - additive_target_coefficient(row, *rhs, target_y_index, dependencies)?,
        ),
        _ => None,
    }
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

fn direct_assignment_shape(
    row: &[LinearOp],
    output_reg: u32,
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    let Some((target_reg, expr_reg, target_scale)) = assignment_expr_reg(row, output_reg) else {
        return Ok(None);
    };
    let Some(target_y_index) = target_load_index(row, target_reg) else {
        return Ok(None);
    };
    if reg_depends_on_y_index(row, expr_reg, target_y_index) {
        return Ok(None);
    }
    let Some(expr_pos) = producer_pos(row, expr_reg) else {
        return Ok(None);
    };
    let expr_eval_len = checked_expr_eval_len(expr_pos)?;
    Ok(Some(TargetAssignmentShape::Direct {
        target_y_index,
        expr_reg,
        target_scale,
        expr_eval_len,
    }))
}

fn affine_assignment_shapes(
    row: &[LinearOp],
    output_reg: u32,
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    let (output_reg, output_scale) = strip_affine_output_wrappers(row, output_reg);
    let Some(output_op) = producer(row, output_reg) else {
        return Ok(Vec::new());
    };
    let (lhs, rhs, lhs_scale, rhs_scale) = match *output_op {
        LinearOp::Binary {
            op: BinaryOp::Add,
            lhs,
            rhs,
            ..
        } => (lhs, rhs, output_scale, output_scale),
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => (lhs, rhs, output_scale, -output_scale),
        _ => return Ok(Vec::new()),
    };
    let mut shapes = Vec::new();
    for (target_reg, coefficient_reg) in affine_target_terms(row, lhs).into_iter().flatten() {
        push_affine_sum_side_shape(
            &mut shapes,
            row,
            target_reg,
            coefficient_reg,
            lhs_scale,
            rhs,
            rhs_scale,
        )?;
    }
    for (target_reg, coefficient_reg) in affine_target_terms(row, rhs).into_iter().flatten() {
        push_affine_sum_side_shape(
            &mut shapes,
            row,
            target_reg,
            coefficient_reg,
            rhs_scale,
            lhs,
            lhs_scale,
        )?;
    }
    Ok(shapes)
}

fn strip_affine_output_wrappers(row: &[LinearOp], mut reg: u32) -> (u32, f64) {
    let mut scale = 1.0;
    loop {
        match producer(row, reg) {
            Some(LinearOp::Unary {
                op: UnaryOp::Neg,
                arg,
                ..
            }) => {
                reg = *arg;
                scale = -scale;
            }
            Some(LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            }) if is_zero_literal(row, *lhs) => {
                reg = *rhs;
                scale = -scale;
            }
            Some(LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            }) if is_zero_literal(row, *rhs) => {
                reg = *lhs;
            }
            _ => return (reg, scale),
        }
    }
}

fn is_zero_literal(row: &[LinearOp], reg: u32) -> bool {
    matches!(producer(row, reg), Some(LinearOp::Const { value: 0.0, .. }))
}

fn push_affine_sum_side_shape(
    shapes: &mut Vec<TargetAssignmentShape>,
    row: &[LinearOp],
    target_reg: u32,
    coefficient_reg: Option<u32>,
    coefficient_scale: f64,
    offset_reg: u32,
    offset_scale: f64,
) -> Result<(), EvalSolveError> {
    let Some(shape) = affine_sum_side_shape(
        row,
        target_reg,
        coefficient_reg,
        coefficient_scale,
        offset_reg,
        offset_scale,
    )?
    else {
        return Ok(());
    };
    if shapes
        .iter()
        .all(|existing| existing.target_y_index() != shape.target_y_index())
    {
        shapes.push(shape);
    }
    Ok(())
}

fn affine_sum_side_shape(
    row: &[LinearOp],
    target_reg: u32,
    coefficient_reg: Option<u32>,
    coefficient_scale: f64,
    offset_reg: u32,
    offset_scale: f64,
) -> Result<Option<TargetAssignmentShape>, EvalSolveError> {
    let Some(target_y_index) = target_load_index(row, target_reg) else {
        return Ok(None);
    };
    if coefficient_reg.is_some_and(|reg| reg_depends_on_y_index(row, reg, target_y_index))
        || reg_depends_on_y_index(row, offset_reg, target_y_index)
    {
        return Ok(None);
    }
    let coefficient_pos = coefficient_reg
        .and_then(|reg| producer_pos(row, reg))
        .unwrap_or(0);
    let Some(offset_pos) = producer_pos(row, offset_reg) else {
        return Ok(None);
    };
    let expr_eval_len = checked_expr_eval_len(coefficient_pos.max(offset_pos))?;
    Ok(Some(TargetAssignmentShape::Affine {
        target_y_index,
        offset_reg,
        coefficient_reg,
        offset_scale,
        coefficient_scale,
        expr_eval_len,
    }))
}

pub(super) fn checked_expr_eval_len(pos: usize) -> Result<usize, EvalSolveError> {
    pos.checked_add(1)
        .ok_or_else(|| invalid_prepared_row("target assignment expression length overflows"))
}

fn affine_target_terms(row: &[LinearOp], reg: u32) -> [Option<(u32, Option<u32>)>; 2] {
    if is_y_load(row, reg) {
        return [Some((reg, None)), None];
    }
    let Some(producer) = producer(row, reg) else {
        return [None, None];
    };
    let LinearOp::Binary {
        op: BinaryOp::Mul,
        lhs,
        rhs,
        ..
    } = *producer
    else {
        return [None, None];
    };
    match (is_y_load(row, lhs), is_y_load(row, rhs)) {
        (true, false) => [Some((lhs, Some(rhs))), None],
        (false, true) => [Some((rhs, Some(lhs))), None],
        (true, true) => [Some((lhs, Some(rhs))), Some((rhs, Some(lhs)))],
        (false, false) => [None, None],
    }
}

fn store_output_reg(row: &[LinearOp]) -> Option<u32> {
    row.iter().rev().find_map(|op| match *op {
        LinearOp::StoreOutput { src } => Some(src),
        _ => None,
    })
}

fn target_load_index(row: &[LinearOp], target_reg: u32) -> Option<usize> {
    row.iter().find_map(|op| match *op {
        LinearOp::LoadY { dst, index } if dst == target_reg => Some(index),
        _ => None,
    })
}

fn assignment_expr_reg(row: &[LinearOp], output_reg: u32) -> Option<(u32, u32, f64)> {
    let output_op = producer(row, output_reg)?;
    match *output_op {
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => sub_assignment_expr_reg(row, lhs, rhs, 1.0),
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => {
            let LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            } = *producer(row, arg)?
            else {
                return None;
            };
            sub_assignment_expr_reg(row, lhs, rhs, -1.0)
        }
        _ => None,
    }
}

fn producer_pos(row: &[LinearOp], dst_reg: u32) -> Option<usize> {
    row.iter()
        .rposition(|op| op.dst_register() == Some(dst_reg))
}

fn sub_assignment_expr_reg(
    row: &[LinearOp],
    lhs: u32,
    rhs: u32,
    output_scale: f64,
) -> Option<(u32, u32, f64)> {
    if is_y_load(row, lhs) {
        Some((lhs, rhs, output_scale))
    } else if is_y_load(row, rhs) {
        Some((rhs, lhs, -output_scale))
    } else {
        None
    }
}

fn is_y_load(row: &[LinearOp], reg: u32) -> bool {
    matches!(producer(row, reg), Some(LinearOp::LoadY { .. }))
}
