use rumoca_ir_solve::{BinaryOp, LinearOp, UnaryOp};

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
    if store_output_regs(row).next().is_none() {
        return Ok(Vec::new());
    }
    let mut shapes = Vec::new();
    let mut dependencies = YDependencyAnalyzer::new(row, 0);
    // Prefer the direct owner of every output before considering more general
    // affine isolators. A shared program may read another output's target, but
    // that does not transfer equation ownership to the dependent output.
    for (output_offset, output_reg) in store_output_regs(row).enumerate() {
        for shape in direct_assignment_shapes(row, output_reg, &mut dependencies)? {
            if shapes.iter().all(
                |(existing_output, existing): &(usize, TargetAssignmentShape)| {
                    *existing_output != output_offset
                        || existing.target_y_index() != shape.target_y_index()
                },
            ) {
                shapes.push((output_offset, shape));
            }
        }
    }
    for (output_offset, output_reg) in store_output_regs(row).enumerate() {
        for shape in affine_assignment_shapes(row, output_reg)? {
            if shapes.iter().all(|(existing_output, existing)| {
                *existing_output != output_offset
                    || existing.target_y_index() != shape.target_y_index()
            }) {
                shapes.push((output_offset, shape));
            }
        }
    }
    // Discover target lanes by walking each scalar output's arithmetic DAG.
    // A TensorLoad remains one compact range operation; only registers that
    // actually reach this output become scalar target views.
    for (output_offset, output_reg) in store_output_regs(row).enumerate() {
        for shape in affine_residual_shapes(row, output_reg)? {
            if shapes.iter().all(|(existing_output, existing)| {
                *existing_output != output_offset
                    || existing.target_y_index() != shape.target_y_index()
            }) {
                shapes.push((output_offset, shape));
            }
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
    let mut target_loads = Vec::new();
    collect_affine_y_loads(
        row,
        residual_reg,
        &mut std::collections::BTreeSet::new(),
        &mut target_loads,
    );
    for (target_y_index, target_reg) in target_loads {
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
        _ if target_load_index(row, reg) == Some(target_y_index) => Some(1.0),
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

fn collect_affine_y_loads(
    row: &[LinearOp],
    reg: u32,
    visited: &mut std::collections::BTreeSet<u32>,
    targets: &mut Vec<(usize, u32)>,
) {
    if !visited.insert(reg) {
        return;
    }
    if let Some(index) = target_load_index(row, reg) {
        if targets.iter().all(|&(existing, _)| existing != index) {
            targets.push((index, reg));
        }
        return;
    }
    match producer(row, reg) {
        Some(LinearOp::Move { src, .. })
        | Some(LinearOp::Unary {
            op: UnaryOp::Neg,
            arg: src,
            ..
        }) => collect_affine_y_loads(row, *src, visited, targets),
        Some(LinearOp::Binary {
            op: BinaryOp::Add | BinaryOp::Sub,
            lhs,
            rhs,
            ..
        }) => {
            collect_affine_y_loads(row, *lhs, visited, targets);
            collect_affine_y_loads(row, *rhs, visited, targets);
        }
        _ => {}
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

fn direct_assignment_shapes(
    row: &[LinearOp],
    output_reg: u32,
    dependencies: &mut YDependencyAnalyzer<'_>,
) -> Result<Vec<TargetAssignmentShape>, EvalSolveError> {
    let mut shapes = Vec::with_capacity(2);
    for (target_reg, expr_reg, target_scale) in
        assignment_expr_regs(row, output_reg).into_iter().flatten()
    {
        let Some(target_y_index) = target_load_index(row, target_reg) else {
            continue;
        };
        dependencies.set_target(target_y_index);
        if dependencies.depends_on(expr_reg) {
            continue;
        }
        let Some(expr_pos) = producer_pos(row, expr_reg) else {
            continue;
        };
        let expr_eval_len = checked_expr_eval_len(expr_pos)?;
        shapes.push(TargetAssignmentShape::Direct {
            target_y_index,
            expr_reg,
            target_scale,
            expr_eval_len,
        });
    }
    Ok(shapes)
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

fn store_output_regs(row: &[LinearOp]) -> impl Iterator<Item = u32> + '_ {
    row.iter().flat_map(|op| {
        let (start, count, stride) = match *op {
            LinearOp::StoreOutput { src } => (src, 1, 0),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => (start, count, stride),
            _ => (0, 0, 0),
        };
        (0..count).map(move |offset| {
            let register_offset = offset
                .checked_mul(stride)
                .and_then(|value| u32::try_from(value).ok())
                .expect("checked output range offset fits a register");
            start
                .checked_add(register_offset)
                .expect("checked output range register fits u32")
        })
    })
}

fn target_load_index(row: &[LinearOp], target_reg: u32) -> Option<usize> {
    row.iter().find_map(|op| match *op {
        LinearOp::LoadY { dst, index } if dst == target_reg => Some(index),
        LinearOp::TensorLoad {
            dst_start,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start,
            count,
            lanes,
            ..
        } => {
            let offset = target_reg.checked_sub(dst_start)? as usize;
            (lanes != 0 && offset < count.checked_mul(lanes)? && offset.is_multiple_of(lanes))
                .then(|| input_start.checked_add(offset / lanes))?
        }
        _ => None,
    })
}

fn assignment_expr_regs(row: &[LinearOp], output_reg: u32) -> [Option<(u32, u32, f64)>; 2] {
    let Some(output_op) = producer(row, output_reg) else {
        return [None, None];
    };
    match *output_op {
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => sub_assignment_expr_regs(row, lhs, rhs, 1.0),
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => {
            let Some(argument_op) = producer(row, arg) else {
                return [None, None];
            };
            let LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            } = *argument_op
            else {
                return [None, None];
            };
            sub_assignment_expr_regs(row, lhs, rhs, -1.0)
        }
        LinearOp::TensorBinary {
            dst_start,
            op: BinaryOp::Sub,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
        } => {
            let Some((lhs, rhs)) = tensor_binary_operands(
                output_reg, dst_start, lhs_start, rhs_start, count, lhs_stride, rhs_stride, lanes,
            ) else {
                return [None, None];
            };
            sub_assignment_expr_regs(row, lhs, rhs, 1.0)
        }
        _ => [None, None],
    }
}

#[allow(clippy::too_many_arguments)]
fn tensor_binary_operands(
    output_reg: u32,
    dst_start: u32,
    lhs_start: u32,
    rhs_start: u32,
    count: usize,
    lhs_stride: usize,
    rhs_stride: usize,
    lanes: usize,
) -> Option<(u32, u32)> {
    let offset = output_reg.checked_sub(dst_start)? as usize;
    if lanes == 0 || offset >= count.checked_mul(lanes)? || !offset.is_multiple_of(lanes) {
        return None;
    }
    let element = offset / lanes;
    let lhs_offset = element.checked_mul(lhs_stride)?.checked_mul(lanes)?;
    let rhs_offset = element.checked_mul(rhs_stride)?.checked_mul(lanes)?;
    Some((
        lhs_start.checked_add(u32::try_from(lhs_offset).ok()?)?,
        rhs_start.checked_add(u32::try_from(rhs_offset).ok()?)?,
    ))
}

fn producer_pos(row: &[LinearOp], dst_reg: u32) -> Option<usize> {
    row.iter()
        .rposition(|op| super::operation_writes_register(op, dst_reg))
}

fn sub_assignment_expr_regs(
    row: &[LinearOp],
    lhs: u32,
    rhs: u32,
    output_scale: f64,
) -> [Option<(u32, u32, f64)>; 2] {
    [
        is_y_load(row, lhs).then_some((lhs, rhs, output_scale)),
        is_y_load(row, rhs).then_some((rhs, lhs, -output_scale)),
    ]
}

fn is_y_load(row: &[LinearOp], reg: u32) -> bool {
    target_load_index(row, reg).is_some()
}
