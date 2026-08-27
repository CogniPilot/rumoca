use std::collections::{BTreeMap, BTreeSet};

use crate::{LinearOp, TargetAssignmentShape};

pub(super) fn assignment_y_dependencies_for_shapes(
    source_program: &[LinearOp],
    shapes: &[TargetAssignmentShape],
) -> Box<[Box<[usize]>]> {
    let mut prefix_dependencies = BTreeMap::new();
    shapes
        .iter()
        .map(|shape| {
            let prefix_len = shape.expr_eval_len();
            let (indices, dependency) =
                prefix_dependencies.entry(prefix_len).or_insert_with(|| {
                    let prefix = source_program.get(..prefix_len).unwrap_or(source_program);
                    (
                        y_load_indices(prefix),
                        ScalarProgramYDependency::new(prefix),
                    )
                });
            let registers = shape_value_registers(*shape);
            indices
                .iter()
                .copied()
                .filter(|index| {
                    registers
                        .into_iter()
                        .flatten()
                        .any(|register| dependency.depends_on(register, *index))
                })
                .collect::<Vec<_>>()
                .into_boxed_slice()
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn shape_value_registers(shape: TargetAssignmentShape) -> [Option<u32>; 3] {
    match shape {
        TargetAssignmentShape::Direct { expr_reg, .. } => [Some(expr_reg), None, None],
        TargetAssignmentShape::Affine {
            offset_reg,
            coefficient_reg,
            ..
        } => [Some(offset_reg), coefficient_reg, None],
        TargetAssignmentShape::AffineResidual {
            target_reg,
            residual_reg,
            ..
        } => [Some(target_reg), Some(residual_reg), None],
    }
}

pub(super) fn y_load_indices(program: &[LinearOp]) -> BTreeSet<usize> {
    let mut indices = BTreeSet::new();
    collect_y_load_indices(program, &mut indices);
    indices
}

fn collect_y_load_indices(program: &[LinearOp], indices: &mut BTreeSet<usize>) {
    for operation in program {
        match operation {
            LinearOp::LoadY { index, .. } => {
                indices.insert(*index);
            }
            LinearOp::TensorLoad {
                input: crate::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => indices.extend(*input_start..input_start.saturating_add(*count)),
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                collect_y_load_indices(&program.update, indices);
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_y_load_indices(&arm.condition, indices);
                    collect_y_load_indices(&arm.result, indices);
                }
                collect_y_load_indices(&program.fallback, indices);
            }
            _ => {}
        }
    }
}

/// Fail-closed solver-Y dependence query for registers in one checked scalar
/// program. The exhaustive dependency walk is owned by `StructuralPattern`;
/// refresh construction consumes that owner instead of maintaining another
/// interpretation of compact tensor and call operations.
pub struct ScalarProgramYDependency<'a> {
    dependencies: Option<Vec<Option<BTreeSet<usize>>>>,
    program: std::marker::PhantomData<&'a [LinearOp]>,
}

impl<'a> ScalarProgramYDependency<'a> {
    pub fn new(program: &'a [LinearOp]) -> Self {
        Self {
            dependencies: crate::structural_pattern::program_register_y_dependencies(program).ok(),
            program: std::marker::PhantomData,
        }
    }

    pub fn depends_on(&self, register: u32, target: usize) -> bool {
        self.dependencies
            .as_ref()
            .and_then(|dependencies| dependencies.get(register as usize))
            .and_then(Option::as_ref)
            .is_none_or(|dependencies| dependencies.contains(&target))
    }
}

pub(super) fn register_is_written_by(operation: &LinearOp, register: u32) -> bool {
    operation.dst_register().is_some_and(|start| {
        u32::try_from(operation.dst_register_count())
            .ok()
            .and_then(|count| start.checked_add(count))
            .is_some_and(|end| register >= start && register < end)
    })
}
