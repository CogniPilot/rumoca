use std::collections::{BTreeMap, BTreeSet};

use crate::{LinearOp, TargetAssignmentShape};

pub(super) fn assignment_y_dependencies_for_shapes(
    source_program: &[LinearOp],
    shapes: &[TargetAssignmentShape],
) -> Box<[Box<[usize]>]> {
    shapes
        .iter()
        .map(assignment_dependency_query(source_program))
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn assignment_y_dependencies_for_shape(
    source_program: &[LinearOp],
    shape: &TargetAssignmentShape,
) -> Box<[usize]> {
    assignment_dependency_query(source_program)(shape)
}

fn assignment_dependency_query(
    source_program: &[LinearOp],
) -> impl FnMut(&TargetAssignmentShape) -> Box<[usize]> + '_ {
    let mut prefix_dependencies = BTreeMap::new();
    move |shape| {
        let prefix_len = shape.expr_eval_len();
        let (indices, dependency) = prefix_dependencies.entry(prefix_len).or_insert_with(|| {
            let prefix = source_program.get(..prefix_len).unwrap_or(source_program);
            (
                y_load_indices(prefix),
                ScalarProgramYDependency::new(prefix),
            )
        });
        indices
            .iter()
            .copied()
            .filter(|index| dependency.assignment_depends_on(shape, *index))
            .collect::<Vec<_>>()
            .into_boxed_slice()
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
    can_prune_tensor_lanes: bool,
    program: std::marker::PhantomData<&'a [LinearOp]>,
}

impl<'a> ScalarProgramYDependency<'a> {
    pub fn new(program: &'a [LinearOp]) -> Self {
        Self {
            dependencies: crate::structural_pattern::program_register_y_dependencies(program).ok(),
            can_prune_tensor_lanes: program.iter().all(arithmetic_lane_operation),
            program: std::marker::PhantomData,
        }
    }

    /// Dependencies of a checked isolated value and its coefficient guard.
    /// Tensor shapes retain whole operand ranges, including unused lanes. The
    /// selected residual output bounds the inputs of both affine components;
    /// an unknown output dependency retains the conservative range evidence.
    /// Calls and other potentially value-failing operations keep all prior
    /// dependencies: their selected value does not prove execution can succeed.
    pub fn assignment_depends_on(&self, shape: &TargetAssignmentShape, target: usize) -> bool {
        if self.can_prune_tensor_lanes
            && let TargetAssignmentShape::TensorAffine { projection, .. } = shape
            && !self.depends_on(projection.output_register(), target)
        {
            return false;
        }
        shape
            .value_registers()
            .any(|register| self.depends_on(register, target))
    }

    pub fn depends_on(&self, register: u32, target: usize) -> bool {
        self.dependencies
            .as_ref()
            .and_then(|dependencies| dependencies.get(register as usize))
            .and_then(Option::as_ref)
            .is_none_or(|dependencies| dependencies.contains(&target))
    }
}

// These operations can fail bounds/shape checks, but not according to the
// numerical value of another solver coordinate. Their original execution is
// unchanged. Everything else retains conservative whole-range dependencies.
fn arithmetic_lane_operation(operation: &LinearOp) -> bool {
    matches!(
        operation,
        LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorCross { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. }
            | LinearOp::TensorLoad { .. }
            | LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. }
    )
}
