mod additive;
mod producers;
pub(super) mod tensor_affine;

#[cfg(test)]
mod tests;

use super::dependency::{ScalarProgramYDependency, y_load_indices};
use crate::{BinaryOp, LinearOp, StridedOperand, TargetAssignmentShape, UnaryOp};
use producers::{ProgramPrefix, UniqueProgram};
use std::cell::OnceCell;

pub(super) fn canonical_assignment_shape_for_output(
    program: &[LinearOp],
    output_offset: usize,
    target_y_index: usize,
) -> Option<TargetAssignmentShape> {
    let (output, store_position) = store_output_registers(program).nth(output_offset)?;
    let prefix = program.get(..store_position)?;
    OutputAssignmentDerivation::new(prefix, output)?.for_target(target_y_index)
}

/// Construction-local analysis bound to one exact store prefix and output.
struct OutputAssignmentDerivation<'a> {
    producers: UniqueProgram<'a>,
    output: u32,
    dependencies: ScalarProgramYDependency<'a>,
    affine_shapes: OnceCell<Vec<TargetAssignmentShape>>,
}

impl<'a> OutputAssignmentDerivation<'a> {
    fn new(prefix: &'a [LinearOp], output: u32) -> Option<Self> {
        Some(Self {
            producers: UniqueProgram::new(prefix)?,
            output,
            dependencies: ScalarProgramYDependency::new(prefix),
            affine_shapes: OnceCell::new(),
        })
    }

    fn for_target(&self, target_y_index: usize) -> Option<TargetAssignmentShape> {
        let prefix = self.producers.view();
        let output = self.output;
        let dependencies = &self.dependencies;
        let direct = assignment_expression_registers(prefix, output)
            .into_iter()
            .flatten()
            .find_map(|(target, expression, target_scale)| {
                let load = target_load(prefix, target)?;
                if load.index != target_y_index
                    || dependencies.depends_on(expression, target_y_index)
                {
                    return None;
                }
                Some(TargetAssignmentShape::Direct {
                    target_y_index,
                    expr_reg: expression,
                    target_scale,
                    expr_eval_len: producer_position(prefix, expression)?
                        .checked_add(1)?
                        .max(load.required_eval_len),
                })
            });
        direct
            .or_else(|| zero_assignment_shape(prefix, output, target_y_index))
            .or_else(|| {
                self.affine_shapes
                    .get_or_init(|| affine_assignment_shapes(prefix, output, dependencies))
                    .iter()
                    .find(|shape| shape.target_y_index() == target_y_index)
                    .cloned()
            })
            .or_else(|| additive::derive(prefix, output, target_y_index, dependencies))
            .or_else(|| {
                Some(TargetAssignmentShape::TensorAffine {
                    target_y_index,
                    projection: tensor_affine::derive(
                        prefix,
                        output,
                        target_y_index,
                        dependencies,
                    )?,
                    expr_eval_len: prefix.len(),
                })
            })
    }
}

fn zero_assignment_shape(
    prefix: ProgramPrefix<'_>,
    output: u32,
    target_y_index: usize,
) -> Option<TargetAssignmentShape> {
    let (register, _) = strip_affine_output_wrappers(prefix, output);
    let load = target_load(prefix, register)?;
    (load.index == target_y_index).then_some(TargetAssignmentShape::Zero {
        target_y_index,
        expr_eval_len: prefix.len(),
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
        let Some(derivation) = OutputAssignmentDerivation::new(prefix, output) else {
            continue;
        };
        for target in y_load_indices(prefix) {
            let Some(shape) = derivation.for_target(target) else {
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

fn affine_assignment_shapes(
    program: ProgramPrefix<'_>,
    output: u32,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Vec<TargetAssignmentShape> {
    #[cfg(test)]
    tests::record_affine_derivation();
    let (output, output_scale) = strip_affine_output_wrappers(program, output);
    let Some((op, lhs, rhs)) = binary_operands(program, output) else {
        return Vec::new();
    };
    let (lhs_scale, rhs_scale) = match op {
        BinaryOp::Add => (output_scale, output_scale),
        BinaryOp::Sub => (output_scale, -output_scale),
        _ => return Vec::new(),
    };
    let mut shapes = Vec::new();
    for (target, coefficient) in affine_target_terms(program, lhs).into_iter().flatten() {
        push_affine_assignment_shape(
            &mut shapes,
            program,
            (target, coefficient, lhs_scale),
            rhs,
            rhs_scale,
            dependencies,
        );
    }
    for (target, coefficient) in affine_target_terms(program, rhs).into_iter().flatten() {
        push_affine_assignment_shape(
            &mut shapes,
            program,
            (target, coefficient, rhs_scale),
            lhs,
            lhs_scale,
            dependencies,
        );
    }
    shapes
}

fn push_affine_assignment_shape(
    shapes: &mut Vec<TargetAssignmentShape>,
    program: ProgramPrefix<'_>,
    target_term: (u32, Option<u32>, f64),
    offset: u32,
    offset_scale: f64,
    dependencies: &ScalarProgramYDependency<'_>,
) {
    let (target, coefficient, coefficient_scale) = target_term;
    let Some(load) = target_load(program, target) else {
        return;
    };
    let target_y_index = load.index;
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
        expr_eval_len: expr_eval_len.max(load.required_eval_len),
    };
    if shapes
        .iter()
        .all(|existing| existing.target_y_index() != target_y_index)
    {
        shapes.push(shape);
    }
}

fn affine_target_terms(
    program: ProgramPrefix<'_>,
    register: u32,
) -> [Option<(u32, Option<u32>)>; 2] {
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

fn strip_affine_output_wrappers(program: ProgramPrefix<'_>, mut register: u32) -> (u32, f64) {
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

fn is_zero_literal(program: ProgramPrefix<'_>, register: u32) -> bool {
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
    program: ProgramPrefix<'_>,
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

fn binary_operands(program: ProgramPrefix<'_>, output: u32) -> Option<(BinaryOp, u32, u32)> {
    match *producer(program, output)? {
        LinearOp::Binary { op, lhs, rhs, .. } => Some((op, lhs, rhs)),
        LinearOp::TensorBinary {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
        } => {
            let (lhs, rhs) = tensor_binary_operands(
                output,
                dst_start,
                (
                    StridedOperand {
                        start: lhs_start,
                        stride: lhs_stride,
                    },
                    StridedOperand {
                        start: rhs_start,
                        stride: rhs_stride,
                    },
                ),
                count,
                lanes,
            )?;
            Some((op, lhs, rhs))
        }
        _ => None,
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
    program: ProgramPrefix<'_>,
    lhs: u32,
    rhs: u32,
    scale: f64,
) -> [Option<(u32, u32, f64)>; 2] {
    [
        target_load_index(program, lhs).map(|_| (lhs, rhs, scale)),
        target_load_index(program, rhs).map(|_| (rhs, lhs, -scale)),
    ]
}

fn target_load_index(program: ProgramPrefix<'_>, register: u32) -> Option<usize> {
    target_load(program, register).map(|load| load.index)
}

struct TargetLoad {
    index: usize,
    required_eval_len: usize,
}

fn target_load(mut program: ProgramPrefix<'_>, mut register: u32) -> Option<TargetLoad> {
    let mut required_eval_len = 0;
    loop {
        let position = producer_position(program, register)?;
        match program.operation(position)? {
            LinearOp::LoadY { index, .. } => {
                return Some(TargetLoad {
                    index: *index,
                    required_eval_len,
                });
            }
            LinearOp::TensorLoad {
                dst_start,
                input: crate::TensorInputKind::Y,
                input_start,
                count,
                lanes,
                ..
            } => {
                let offset = register.checked_sub(*dst_start)? as usize;
                if *lanes == 0
                    || offset >= count.checked_mul(*lanes)?
                    || !offset.is_multiple_of(*lanes)
                {
                    return None;
                }
                return Some(TargetLoad {
                    index: input_start.checked_add(offset / lanes)?,
                    required_eval_len,
                });
            }
            LinearOp::Move { src, .. } => register = *src,
            operation => {
                register = projected_input_register(operation, register)?;
                required_eval_len = required_eval_len.max(position.checked_add(1)?);
            }
        }
        program = program.before(position)?;
    }
}

fn projected_input_register(operation: &LinearOp, register: u32) -> Option<u32> {
    let (starts, projection) = match operation {
        LinearOp::PureCall {
            dst_start,
            input_starts,
            site,
        } => (
            input_starts,
            site.projected_input_coordinate(register.checked_sub(*dst_start)? as usize)?,
        ),
        LinearOp::PureCallDirectional {
            dst_start,
            input_starts,
            site,
        } => (
            input_starts,
            site.projected_input_coordinate(register.checked_sub(*dst_start)? as usize)?,
        ),
        _ => return None,
    };
    starts
        .get(projection.0)?
        .checked_add(u32::try_from(projection.1).ok()?)
}

fn producer(program: ProgramPrefix<'_>, register: u32) -> Option<&LinearOp> {
    program.operation(program.producer_position(register)?)
}

fn producer_position(program: ProgramPrefix<'_>, register: u32) -> Option<usize> {
    program.producer_position(register)
}
