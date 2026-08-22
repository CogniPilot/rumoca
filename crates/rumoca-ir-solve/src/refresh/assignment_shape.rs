use super::dependency::{ScalarProgramYDependency, register_is_written_by, y_load_indices};
use crate::{BinaryOp, LinearOp, StridedOperand, TargetAssignmentShape, UnaryOp};

pub(super) fn canonical_assignment_shape_for_output(
    program: &[LinearOp],
    output_offset: usize,
    target_y_index: usize,
) -> Option<TargetAssignmentShape> {
    let (output, store_position) = store_output_registers(program).nth(output_offset)?;
    let prefix = program.get(..store_position)?;
    if !writes_unique_registers(prefix) {
        return None;
    }
    let dependencies = ScalarProgramYDependency::new(prefix);
    canonical_assignment_shape(prefix, output, target_y_index, &dependencies)
}

fn canonical_assignment_shape(
    prefix: &[LinearOp],
    output: u32,
    target_y_index: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<TargetAssignmentShape> {
    let direct = assignment_expression_registers(prefix, output)
        .into_iter()
        .flatten()
        .find_map(|(target, expression, target_scale)| {
            if target_load_index(prefix, target) != Some(target_y_index)
                || dependencies.depends_on(expression, target_y_index)
            {
                return None;
            }
            Some(TargetAssignmentShape::Direct {
                target_y_index,
                expr_reg: expression,
                target_scale,
                expr_eval_len: producer_position(prefix, expression)?.checked_add(1)?,
            })
        });
    direct
        .or_else(|| {
            affine_assignment_shapes(prefix, output, dependencies)
                .into_iter()
                .find(|shape| shape.target_y_index() == target_y_index)
        })
        .or_else(|| {
            affine_residual_assignment_shapes(prefix, output, dependencies)
                .into_iter()
                .find(|shape| shape.target_y_index() == target_y_index)
        })
}

/// Derive every assignment isolator owned by one checked scalar program.
///
/// Each output is analyzed only against the operation prefix that reaches its
/// exact store. Programs that reuse a destination register before that store
/// remain executable residual programs but expose no unversioned assignment
/// certificate for that output.
#[must_use]
pub fn derive_target_assignment_shapes(
    program: &[LinearOp],
) -> Vec<(usize, TargetAssignmentShape)> {
    let mut shapes = Vec::new();
    for (output_offset, (output, store_position)) in store_output_registers(program).enumerate() {
        let Some(prefix) = program.get(..store_position) else {
            continue;
        };
        if !writes_unique_registers(prefix) {
            continue;
        }
        let dependencies = ScalarProgramYDependency::new(prefix);
        for target in y_load_indices(prefix) {
            let Some(shape) = canonical_assignment_shape(prefix, output, target, &dependencies)
            else {
                continue;
            };
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
    shapes
}

#[must_use]
pub fn derive_target_assignment_shape_for_output(
    program: &[LinearOp],
    output_offset: usize,
    target_y_index: usize,
) -> Option<TargetAssignmentShape> {
    canonical_assignment_shape_for_output(program, output_offset, target_y_index)
}

fn writes_unique_registers(program: &[LinearOp]) -> bool {
    let mut written = std::collections::BTreeSet::new();
    program.iter().all(|operation| {
        let Some(start) = operation.dst_register() else {
            return true;
        };
        let Ok(count) = u32::try_from(operation.dst_register_count()) else {
            return false;
        };
        (0..count).all(|offset| {
            start
                .checked_add(offset)
                .is_some_and(|dst| written.insert(dst))
        })
    })
}

fn affine_assignment_shapes(
    program: &[LinearOp],
    output: u32,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Vec<TargetAssignmentShape> {
    let (output, output_scale) = strip_affine_output_wrappers(program, output);
    let Some(LinearOp::Binary { op, lhs, rhs, .. }) = producer(program, output) else {
        return Vec::new();
    };
    let (lhs_scale, rhs_scale) = match op {
        BinaryOp::Add => (output_scale, output_scale),
        BinaryOp::Sub => (output_scale, -output_scale),
        _ => return Vec::new(),
    };
    let mut shapes = Vec::new();
    for (target, coefficient) in affine_target_terms(program, *lhs).into_iter().flatten() {
        push_affine_assignment_shape(
            &mut shapes,
            program,
            (target, coefficient, lhs_scale),
            *rhs,
            rhs_scale,
            dependencies,
        );
    }
    for (target, coefficient) in affine_target_terms(program, *rhs).into_iter().flatten() {
        push_affine_assignment_shape(
            &mut shapes,
            program,
            (target, coefficient, rhs_scale),
            *lhs,
            lhs_scale,
            dependencies,
        );
    }
    shapes
}

fn push_affine_assignment_shape(
    shapes: &mut Vec<TargetAssignmentShape>,
    program: &[LinearOp],
    target_term: (u32, Option<u32>, f64),
    offset: u32,
    offset_scale: f64,
    dependencies: &ScalarProgramYDependency<'_>,
) {
    let (target, coefficient, coefficient_scale) = target_term;
    let Some(target_y_index) = target_load_index(program, target) else {
        return;
    };
    if dependencies.depends_on(offset, target_y_index)
        || coefficient.is_some_and(|register| dependencies.depends_on(register, target_y_index))
    {
        return;
    }
    let coefficient_position = coefficient
        .and_then(|register| producer_position(program, register))
        .unwrap_or(0);
    let Some(offset_position) = producer_position(program, offset) else {
        return;
    };
    let Some(expr_eval_len) = coefficient_position.max(offset_position).checked_add(1) else {
        return;
    };
    let shape = TargetAssignmentShape::Affine {
        target_y_index,
        offset_reg: offset,
        coefficient_reg: coefficient,
        offset_scale,
        coefficient_scale,
        expr_eval_len,
    };
    if shapes
        .iter()
        .all(|existing| existing.target_y_index() != target_y_index)
    {
        shapes.push(shape);
    }
}

fn affine_target_terms(program: &[LinearOp], register: u32) -> [Option<(u32, Option<u32>)>; 2] {
    if target_load_index(program, register).is_some() {
        return [Some((register, None)), None];
    }
    let Some(LinearOp::Binary {
        op: BinaryOp::Mul,
        lhs,
        rhs,
        ..
    }) = producer(program, register)
    else {
        return [None, None];
    };
    match (
        target_load_index(program, *lhs).is_some(),
        target_load_index(program, *rhs).is_some(),
    ) {
        (true, false) => [Some((*lhs, Some(*rhs))), None],
        (false, true) => [Some((*rhs, Some(*lhs))), None],
        (true, true) => [Some((*lhs, Some(*rhs))), Some((*rhs, Some(*lhs)))],
        (false, false) => [None, None],
    }
}

fn affine_residual_assignment_shapes(
    program: &[LinearOp],
    residual: u32,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Vec<TargetAssignmentShape> {
    let Some(expr_eval_len) =
        producer_position(program, residual).and_then(|position| position.checked_add(1))
    else {
        return Vec::new();
    };
    let mut loads = Vec::new();
    collect_affine_y_loads(
        program,
        residual,
        &mut std::collections::BTreeSet::new(),
        &mut loads,
    );
    let mut shapes = Vec::new();
    for (target_y_index, target_reg) in loads {
        let Some(coefficient) =
            additive_target_coefficient(program, residual, target_y_index, dependencies)
        else {
            continue;
        };
        if coefficient == 0.0 || !coefficient.is_finite() {
            continue;
        }
        shapes.push(TargetAssignmentShape::AffineResidual {
            target_y_index,
            target_reg,
            residual_reg: residual,
            coefficient,
            expr_eval_len,
        });
    }
    shapes
}

fn additive_target_coefficient(
    program: &[LinearOp],
    register: u32,
    target_y_index: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<f64> {
    if !dependencies.depends_on(register, target_y_index) {
        return Some(0.0);
    }
    match producer(program, register)? {
        _ if target_load_index(program, register) == Some(target_y_index) => Some(1.0),
        LinearOp::Move { src, .. } => {
            additive_target_coefficient(program, *src, target_y_index, dependencies)
        }
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => additive_target_coefficient(program, *arg, target_y_index, dependencies)
            .map(|value| -value),
        LinearOp::Binary {
            op: BinaryOp::Add,
            lhs,
            rhs,
            ..
        } => Some(
            additive_target_coefficient(program, *lhs, target_y_index, dependencies)?
                + additive_target_coefficient(program, *rhs, target_y_index, dependencies)?,
        ),
        LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => Some(
            additive_target_coefficient(program, *lhs, target_y_index, dependencies)?
                - additive_target_coefficient(program, *rhs, target_y_index, dependencies)?,
        ),
        _ => None,
    }
}

fn collect_affine_y_loads(
    program: &[LinearOp],
    register: u32,
    visited: &mut std::collections::BTreeSet<u32>,
    loads: &mut Vec<(usize, u32)>,
) {
    if !visited.insert(register) {
        return;
    }
    if let Some(index) = target_load_index(program, register) {
        if loads.iter().all(|(existing, _)| *existing != index) {
            loads.push((index, register));
        }
        return;
    }
    match producer(program, register) {
        Some(LinearOp::Move { src, .. })
        | Some(LinearOp::Unary {
            op: UnaryOp::Neg,
            arg: src,
            ..
        }) => collect_affine_y_loads(program, *src, visited, loads),
        Some(LinearOp::Binary {
            op: BinaryOp::Add | BinaryOp::Sub,
            lhs,
            rhs,
            ..
        }) => {
            collect_affine_y_loads(program, *lhs, visited, loads);
            collect_affine_y_loads(program, *rhs, visited, loads);
        }
        _ => {}
    }
}

fn strip_affine_output_wrappers(program: &[LinearOp], mut register: u32) -> (u32, f64) {
    let mut scale = 1.0;
    loop {
        match producer(program, register) {
            Some(LinearOp::Unary {
                op: UnaryOp::Neg,
                arg,
                ..
            }) => {
                register = *arg;
                scale = -scale;
            }
            Some(LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            }) if is_zero_literal(program, *lhs) => {
                register = *rhs;
                scale = -scale;
            }
            Some(LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            }) if is_zero_literal(program, *rhs) => register = *lhs,
            _ => return (register, scale),
        }
    }
}

fn is_zero_literal(program: &[LinearOp], register: u32) -> bool {
    matches!(
        producer(program, register),
        Some(LinearOp::Const { value: 0.0, .. })
    )
}

pub(super) fn non_causal_assignment_operation(operation: &LinearOp) -> bool {
    matches!(
        operation,
        LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }
    )
}

fn store_output_registers(program: &[LinearOp]) -> impl Iterator<Item = (u32, usize)> + '_ {
    program
        .iter()
        .enumerate()
        .flat_map(|(position, operation)| {
            let (start, count, stride) = match *operation {
                LinearOp::StoreOutput { src } => (src, 1, 0),
                LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride,
                } => (start, count, stride),
                _ => (0, 0, 0),
            };
            (0..count).filter_map(move |offset| {
                u32::try_from(offset.checked_mul(stride)?)
                    .ok()
                    .and_then(|offset| start.checked_add(offset))
                    .map(|register| (register, position))
            })
        })
}

fn assignment_expression_registers(
    program: &[LinearOp],
    output: u32,
) -> [Option<(u32, u32, f64)>; 2] {
    match producer(program, output) {
        Some(LinearOp::Binary {
            op: BinaryOp::Sub,
            lhs,
            rhs,
            ..
        }) => subtraction_assignment_registers(program, *lhs, *rhs, 1.0),
        Some(LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        }) => match producer(program, *arg) {
            Some(LinearOp::Binary {
                op: BinaryOp::Sub,
                lhs,
                rhs,
                ..
            }) => subtraction_assignment_registers(program, *lhs, *rhs, -1.0),
            _ => [None, None],
        },
        Some(LinearOp::TensorBinary {
            dst_start,
            op: BinaryOp::Sub,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
        }) => tensor_binary_operands(
            output,
            *dst_start,
            (
                StridedOperand {
                    start: *lhs_start,
                    stride: *lhs_stride,
                },
                StridedOperand {
                    start: *rhs_start,
                    stride: *rhs_stride,
                },
            ),
            *count,
            *lanes,
        )
        .map_or([None, None], |(lhs, rhs)| {
            subtraction_assignment_registers(program, lhs, rhs, 1.0)
        }),
        _ => [None, None],
    }
}

fn tensor_binary_operands(
    output: u32,
    destination_start: u32,
    operands: (StridedOperand, StridedOperand),
    count: usize,
    lanes: usize,
) -> Option<(u32, u32)> {
    let (lhs, rhs) = operands;
    let offset = output.checked_sub(destination_start)? as usize;
    if lanes == 0 || offset >= count.checked_mul(lanes)? || !offset.is_multiple_of(lanes) {
        return None;
    }
    let element = offset / lanes;
    let lhs_offset = element.checked_mul(lhs.stride)?.checked_mul(lanes)?;
    let rhs_offset = element.checked_mul(rhs.stride)?.checked_mul(lanes)?;
    Some((
        lhs.start.checked_add(u32::try_from(lhs_offset).ok()?)?,
        rhs.start.checked_add(u32::try_from(rhs_offset).ok()?)?,
    ))
}

fn subtraction_assignment_registers(
    program: &[LinearOp],
    lhs: u32,
    rhs: u32,
    scale: f64,
) -> [Option<(u32, u32, f64)>; 2] {
    [
        target_load_index(program, lhs).map(|_| (lhs, rhs, scale)),
        target_load_index(program, rhs).map(|_| (rhs, lhs, -scale)),
    ]
}

fn target_load_index(program: &[LinearOp], register: u32) -> Option<usize> {
    match *producer(program, register)? {
        LinearOp::LoadY { dst, index } if dst == register => Some(index),
        LinearOp::TensorLoad {
            dst_start,
            input: crate::TensorInputKind::Y,
            input_start,
            count,
            lanes,
            ..
        } => {
            let offset = register.checked_sub(dst_start)? as usize;
            (lanes != 0 && offset < count.checked_mul(lanes)? && offset.is_multiple_of(lanes))
                .then(|| input_start.checked_add(offset / lanes))?
        }
        _ => None,
    }
}

fn producer(program: &[LinearOp], register: u32) -> Option<&LinearOp> {
    program
        .iter()
        .rev()
        .find(|operation| register_is_written_by(operation, register))
}

fn producer_position(program: &[LinearOp], register: u32) -> Option<usize> {
    program
        .iter()
        .rposition(|operation| register_is_written_by(operation, register))
}
