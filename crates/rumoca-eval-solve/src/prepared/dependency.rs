use std::{collections::BTreeSet, ops::Range};

use rumoca_ir_solve::{
    BinaryOp, LinearOp, Reg, ScalarProgramRegisterFlow, ScalarProgramYDependency, UnaryOp,
};

use crate::required_registers;

pub(super) struct YDependencyAnalyzer<'a> {
    dependency: ScalarProgramYDependency<'a>,
    target_y_index: usize,
}

impl<'a> YDependencyAnalyzer<'a> {
    pub(super) fn new(row: &'a [LinearOp], target_y_index: usize) -> Self {
        Self {
            dependency: ScalarProgramYDependency::new(row),
            target_y_index,
        }
    }

    pub(super) fn depends_on(&mut self, reg: u32) -> bool {
        self.dependency.depends_on(reg, self.target_y_index)
    }
}

pub(crate) fn row_reads_y_index(program: &[LinearOp], target: usize) -> bool {
    row_y_input_ranges(program)
        .iter()
        .any(|range| range.contains(&target))
}

/// Compact runtime-Y intervals read by one retained scalar/tensor program.
///
/// Tensor loads remain ranges here. A consumer that genuinely needs scalar
/// dependency views can enumerate only producer coordinates intersecting the
/// ranges without rebuilding one load operation per lane.
pub(crate) fn row_y_input_ranges(program: &[LinearOp]) -> Vec<Range<usize>> {
    let mut ranges = Vec::new();
    collect_y_input_ranges(program, &mut ranges);
    ranges.sort_unstable_by_key(|range| (range.start, range.end));
    let mut merged: Vec<Range<usize>> = Vec::with_capacity(ranges.len());
    for range in ranges {
        if range.is_empty() {
            continue;
        }
        if let Some(previous) = merged.last_mut()
            && range.start <= previous.end
        {
            previous.end = previous.end.max(range.end);
        } else {
            merged.push(range);
        }
    }
    merged
}

fn collect_y_input_ranges(program: &[LinearOp], ranges: &mut Vec<Range<usize>>) {
    for op in program {
        match op {
            LinearOp::LoadY { index, .. } => {
                ranges.push(*index..index.saturating_add(1));
            }
            LinearOp::TensorLoad {
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => {
                ranges.push(*input_start..input_start.saturating_add(*count));
            }
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                collect_y_input_ranges(&program.update, ranges);
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_y_input_ranges(&arm.condition, ranges);
                    collect_y_input_ranges(&arm.result, ranges);
                }
                collect_y_input_ranges(&program.fallback, ranges);
            }
            _ => {}
        }
    }
}

pub(super) fn reg_depends_on_y_index(row: &[LinearOp], reg: u32, target_y_index: usize) -> bool {
    YDependencyAnalyzer::new(row, target_y_index).depends_on(reg)
}

/// Whether an operation of `prefix` that can fail on its operand values
/// ([`op_can_fail`]) reads any of `y_indices`. A failure depends on every
/// operand, so each such operation is judged by what it reads: a register
/// version depending on one of `y_indices` when it executes (whatever its
/// output summaries say), or one of them directly inside a nested body.
pub(super) fn failable_op_reads_any_y_index(prefix: &[LinearOp], y_indices: &[usize]) -> bool {
    if y_indices.is_empty() {
        return false;
    }
    let Ok(flow) = ScalarProgramRegisterFlow::derive(prefix) else {
        return true;
    };
    let targets: BTreeSet<usize> = y_indices.iter().copied().collect();
    prefix.iter().enumerate().any(|(position, op)| {
        if !op_can_fail(op) {
            return false;
        }
        if op.reads_y_index_in(&targets) {
            return true;
        }
        let dependency = ScalarProgramYDependency::new(&prefix[..position]);
        let independent = (0..flow.register_count())
            .map(|register| {
                !y_indices
                    .iter()
                    .any(|&y_index| dependency.depends_on(register as Reg, y_index))
            })
            .collect::<Vec<_>>();
        !ScalarProgramRegisterFlow::op_reads_only(op, position, &independent)
    })
}

/// Whether evaluating `op` can return an error, rather than a non-finite
/// value, depending on its operand values: a singular dense solve, a pure
/// call whose body raises, an external-table query, a random-generator op
/// on runtime state, or a function fold or conditional whose body holds one
/// of these. Structural errors (register bounds, uninitialized registers)
/// fail the same way on every input.
pub(super) fn op_can_fail(op: &LinearOp) -> bool {
    match op {
        LinearOp::LinearSolveComponent { .. }
        | LinearOp::PureCall { .. }
        | LinearOp::PureCallDirectional { .. }
        | LinearOp::TableBounds { .. }
        | LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. }
        | LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. } => true,
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            program.update.iter().any(op_can_fail)
        }
        LinearOp::FunctionConditional { program, .. } => program
            .arms
            .iter()
            .flat_map(|arm| arm.condition.iter().chain(arm.result.iter()))
            .chain(program.fallback.iter())
            .any(op_can_fail),
        _ => false,
    }
}

#[derive(Clone, Copy, Default)]
struct GradientDependency {
    value_parameter_only: bool,
    gradient_zero: bool,
    gradient_parameter_only: bool,
}

impl GradientDependency {
    const PARAMETER_VALUE: Self = Self {
        value_parameter_only: true,
        gradient_zero: true,
        gradient_parameter_only: true,
    };
    const TIME_VALUE: Self = Self {
        value_parameter_only: false,
        gradient_zero: true,
        gradient_parameter_only: true,
    };
    const Y_VALUE: Self = Self {
        value_parameter_only: false,
        gradient_zero: false,
        gradient_parameter_only: true,
    };
}

pub(super) fn parameter_static_y_gradient(row: &[LinearOp]) -> bool {
    parameter_static_y_gradient_inner(row).unwrap_or(false)
}

pub(super) fn row_parameter_indices(row: &[LinearOp]) -> Vec<usize> {
    let mut indices = BTreeSet::new();
    collect_parameter_indices(row, &mut indices);
    indices.into_iter().collect()
}

fn collect_parameter_indices(row: &[LinearOp], indices: &mut BTreeSet<usize>) {
    for op in row {
        match op {
            LinearOp::LoadP { index, .. } => {
                indices.insert(*index);
            }
            LinearOp::LoadIndexedP { base, count, .. } => {
                indices.extend(*base..base.saturating_add(*count));
            }
            LinearOp::TensorLoad {
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start,
                count,
                ..
            } => {
                indices.extend(*input_start..input_start.saturating_add(*count));
            }
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                collect_parameter_indices(&program.update, indices);
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_parameter_indices(&arm.condition, indices);
                    collect_parameter_indices(&arm.result, indices);
                }
                collect_parameter_indices(&program.fallback, indices);
            }
            _ => {}
        }
    }
}

fn parameter_static_y_gradient_inner(row: &[LinearOp]) -> Option<bool> {
    let register_count = required_registers(row).ok()?;
    let mut registers = vec![GradientDependency::default(); register_count];
    let mut output = None;
    for op in row {
        let dependency = match *op {
            LinearOp::Const { dst, .. } | LinearOp::LoadP { dst, .. } => {
                Some((dst, GradientDependency::PARAMETER_VALUE))
            }
            LinearOp::LoadTime { dst } => Some((dst, GradientDependency::TIME_VALUE)),
            LinearOp::LoadY { dst, .. } => Some((dst, GradientDependency::Y_VALUE)),
            LinearOp::LoadIndexedP { .. } => return Some(false),
            LinearOp::Move { dst, src } => Some((dst, dependency_at(&registers, src)?)),
            LinearOp::Unary { dst, op, arg } => {
                let arg = dependency_at(&registers, arg)?;
                let result = if matches!(op, UnaryOp::Neg) || arg.value_parameter_only {
                    arg
                } else {
                    GradientDependency::default()
                };
                Some((dst, result))
            }
            LinearOp::Binary { dst, op, lhs, rhs } => Some((
                dst,
                binary_gradient_dependency(
                    op,
                    dependency_at(&registers, lhs)?,
                    dependency_at(&registers, rhs)?,
                ),
            )),
            LinearOp::Compare { dst, lhs, rhs, .. } => {
                let lhs = dependency_at(&registers, lhs)?;
                let rhs = dependency_at(&registers, rhs)?;
                Some((
                    dst,
                    GradientDependency {
                        value_parameter_only: lhs.value_parameter_only && rhs.value_parameter_only,
                        gradient_zero: true,
                        gradient_parameter_only: true,
                    },
                ))
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => Some((
                dst,
                select_gradient_dependency(
                    dependency_at(&registers, cond)?,
                    dependency_at(&registers, if_true)?,
                    dependency_at(&registers, if_false)?,
                ),
            )),
            LinearOp::StoreOutput { src } => {
                if output.is_some() {
                    return Some(false);
                }
                output = Some(dependency_at(&registers, src)?);
                None
            }
            LinearOp::LoadSeed { .. } | LinearOp::LinearSolveComponent { .. } => {
                return Some(false);
            }
            _ => return Some(false),
        };
        if let Some((dst, dependency)) = dependency {
            *registers.get_mut(dst as usize)? = dependency;
        }
    }
    Some(output.is_some_and(|dependency| dependency.gradient_parameter_only))
}

fn dependency_at(registers: &[GradientDependency], register: u32) -> Option<GradientDependency> {
    registers.get(register as usize).copied()
}

fn binary_gradient_dependency(
    op: BinaryOp,
    lhs: GradientDependency,
    rhs: GradientDependency,
) -> GradientDependency {
    if lhs.value_parameter_only && rhs.value_parameter_only {
        return GradientDependency::PARAMETER_VALUE;
    }
    let value_parameter_only = false;
    match op {
        BinaryOp::Add | BinaryOp::Sub => GradientDependency {
            value_parameter_only,
            gradient_zero: lhs.gradient_zero && rhs.gradient_zero,
            gradient_parameter_only: lhs.gradient_parameter_only && rhs.gradient_parameter_only,
        },
        BinaryOp::Mul => GradientDependency {
            value_parameter_only,
            gradient_zero: lhs.gradient_zero && rhs.gradient_zero,
            gradient_parameter_only: lhs.gradient_parameter_only
                && rhs.gradient_parameter_only
                && (lhs.gradient_zero || rhs.value_parameter_only)
                && (rhs.gradient_zero || lhs.value_parameter_only),
        },
        BinaryOp::Div => GradientDependency {
            value_parameter_only,
            gradient_zero: lhs.gradient_zero,
            gradient_parameter_only: rhs.value_parameter_only && lhs.gradient_parameter_only,
        },
        BinaryOp::Pow
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::Atan2
        | BinaryOp::Min
        | BinaryOp::Max => GradientDependency::default(),
    }
}

fn select_gradient_dependency(
    condition: GradientDependency,
    if_true: GradientDependency,
    if_false: GradientDependency,
) -> GradientDependency {
    GradientDependency {
        value_parameter_only: condition.value_parameter_only
            && if_true.value_parameter_only
            && if_false.value_parameter_only,
        gradient_zero: if_true.gradient_zero && if_false.gradient_zero,
        gradient_parameter_only: condition.value_parameter_only
            && if_true.gradient_parameter_only
            && if_false.gradient_parameter_only,
    }
}
