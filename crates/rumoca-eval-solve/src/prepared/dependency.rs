use std::{collections::BTreeSet, ops::Range};

use rumoca_ir_solve::{BinaryOp, LinearOp, UnaryOp};

use crate::required_registers;

pub(super) struct YDependencyAnalyzer<'a> {
    row: &'a [LinearOp],
    producers: Vec<(u32, u32, usize)>,
    memo_generation: Vec<u32>,
    memo_value: Vec<bool>,
    generation: u32,
    target_y_index: usize,
}

impl<'a> YDependencyAnalyzer<'a> {
    pub(super) fn new(row: &'a [LinearOp], target_y_index: usize) -> Self {
        // Register programs form a DAG: a register computed once can feed many
        // downstream ops, so memoize dependence on a fixed `y` index by register.
        let register_count = required_registers(row).unwrap_or(0);
        let mut producers = Vec::with_capacity(row.len());
        for (index, operation) in row.iter().enumerate() {
            if let Some(dst) = operation.dst_register() {
                let count = u32::try_from(operation.dst_register_count()).unwrap_or(u32::MAX);
                producers.push((dst, dst.saturating_add(count), index));
            }
        }
        producers.sort_unstable_by_key(|&(start, _, _)| start);
        Self {
            row,
            producers,
            memo_generation: vec![0; register_count],
            memo_value: vec![false; register_count],
            generation: 1,
            target_y_index,
        }
    }

    pub(super) fn set_target(&mut self, target_y_index: usize) {
        self.target_y_index = target_y_index;
        self.generation = self.generation.wrapping_add(1);
        if self.generation == 0 {
            self.memo_generation.fill(0);
            self.generation = 1;
        }
    }

    pub(super) fn depends_on(&mut self, reg: u32) -> bool {
        let reg_index = reg as usize;
        if self.memo_generation.get(reg_index).copied() == Some(self.generation) {
            return self.memo_value[reg_index];
        }
        // Register programs are checked DAGs. Seed `false` before recursion so
        // malformed fixture cycles still terminate conservatively.
        let Some(memo_entry) = self.memo_generation.get_mut(reg_index) else {
            return false;
        };
        *memo_entry = self.generation;
        self.memo_value[reg_index] = false;
        let result = self
            .producer(reg)
            .is_some_and(|operation| self.operation_depends_on_target(reg, operation));
        self.memo_value[reg_index] = result;
        result
    }

    fn producer(&self, register: u32) -> Option<LinearOp> {
        self.producers
            .partition_point(|&(start, _, _)| start <= register)
            .checked_sub(1)
            .and_then(|position| self.producers.get(position))
            .filter(|&&(_, end, _)| register < end)
            .and_then(|&(_, _, index)| self.row.get(index))
            .cloned()
    }

    fn operation_depends_on_target(&mut self, output: u32, operation: LinearOp) -> bool {
        match operation {
            LinearOp::LoadY { index, .. } => index == self.target_y_index,
            LinearOp::Move { src, .. }
            | LinearOp::Unary { arg: src, .. }
            | LinearOp::LoadIndexedP { index: src, .. }
            | LinearOp::LoadIndexedSeed { index: src, .. } => self.depends_on(src),
            LinearOp::LoadIndexedRegister {
                base,
                stride,
                dimensions,
                indices,
                ..
            } => {
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize));
                count.is_none_or(|count| {
                    (0..count).any(|offset| self.depends_on(base + (offset * stride) as u32))
                })
                    || indices.iter().any(|index| match index {
                        rumoca_ir_solve::TensorIndex::Constant(_) => false,
                        rumoca_ir_solve::TensorIndex::Runtime(register) => {
                            self.depends_on(*register)
                        }
                    })
            }
            LinearOp::LoadIndexedFoldCarried { indices, .. }
            | LinearOp::LoadIndexedFoldCapture { indices, .. } => {
                indices.iter().any(|index| {
                    matches!(index, rumoca_ir_solve::TensorIndex::Runtime(register) if self.depends_on(*register))
                })
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                dimensions,
                updates,
                nodes,
                lanes,
                ..
            } => nodes.iter().any(|node| {
                matches!(
                    node,
                    rumoca_ir_solve::FoldTensorNode::Select { condition, .. }
                        if self.depends_on(*condition)
                )
            }) || updates.iter().any(|update| {
                let value_count = dimensions.iter().zip(update.subscripts.iter()).try_fold(
                    1usize,
                    |count, (&extent, subscript)| {
                        if matches!(subscript, rumoca_ir_solve::TensorSubscript::Whole) {
                            count.checked_mul(extent as usize)
                        } else {
                            Some(count)
                        }
                    },
                );
                update
                    .condition
                    .is_some_and(|condition| self.depends_on(condition))
                    || value_count.is_none_or(|count| {
                        (0..count).any(|element| {
                            (0..lanes).any(|lane| {
                                self.depends_on(
                                    update.value_start
                                        + (element * update.value_stride + lane) as u32,
                                )
                            })
                        })
                    })
                    || update.subscripts.iter().any(|subscript| {
                        matches!(
                            subscript,
                            rumoca_ir_solve::TensorSubscript::Index(
                                rumoca_ir_solve::TensorIndex::Runtime(register)
                            ) if self.depends_on(*register)
                        )
                    })
                }),
            LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
                self.any_register_depends([lhs, rhs])
            }
            LinearOp::Select {
                cond,
                if_true,
                if_false,
                ..
            } => self.any_register_depends([cond, if_true, if_false]),
            LinearOp::LinearSolveComponent {
                matrix_start,
                rhs_start,
                n,
                ..
            } => self.linear_solve_depends(matrix_start, rhs_start, n),
            LinearOp::DotProduct {
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                ..
            } => (0..count).any(|term| {
                let lhs = lhs_start as usize + term * lhs_stride;
                let rhs = rhs_start as usize + term * rhs_stride;
                self.depends_on(lhs as u32) || self.depends_on(rhs as u32)
            }),
            LinearOp::MatrixMultiply {
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
                ..
            } => rows
                .checked_mul(inner)
                .and_then(|count| count.checked_mul(lanes))
                .is_none_or(|count| self.register_range_depends(lhs_start, count))
                || inner
                    .checked_mul(columns)
                    .and_then(|count| count.checked_mul(lanes))
                    .is_none_or(|count| self.register_range_depends(rhs_start, count)),
            LinearOp::TensorBinary {
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
                ..
            } => {
                (0..count).any(|element| {
                    self.register_range_depends(
                        lhs_start + (element * lhs_stride * lanes) as u32,
                        lanes,
                    )
                }) || (0..count).any(|element| {
                    self.register_range_depends(
                        rhs_start + (element * rhs_stride * lanes) as u32,
                        lanes,
                    )
                })
            }
            LinearOp::TensorCross {
                lhs_start,
                rhs_start,
                lanes,
                ..
            } => lanes
                .checked_mul(3)
                .is_none_or(|count| self.register_range_depends(lhs_start, count))
                || lanes
                    .checked_mul(3)
                    .is_none_or(|count| self.register_range_depends(rhs_start, count)),
            LinearOp::TensorTranspose {
                src_start,
                rows,
                columns,
                element_width,
                lanes,
                ..
            } => rows
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(element_width))
                .and_then(|count| count.checked_mul(lanes))
                .is_none_or(|count| self.register_range_depends(src_start, count)),
            LinearOp::TensorConcatenate { sources, lanes, .. } => sources.iter().any(|source| {
                source
                    .dimensions
                    .iter()
                    .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                    .is_none_or(|count| self.register_range_depends(source.start, count))
            }),
            LinearOp::TensorUpdate {
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
                ..
            } => {
                let base_count = dimensions
                    .iter()
                    .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize));
                let mut value_count = Some(lanes);
                let mut subscript_depends = false;
                for (subscript, extent) in subscripts.iter().zip(dimensions.iter()) {
                    match subscript {
                        rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                            value_count = value_count.and_then(|count| {
                                count.checked_mul(*extent as usize)
                            });
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Runtime(register),
                        ) => subscript_depends |= self.depends_on(*register),
                        rumoca_ir_solve::TensorUpdateSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Constant(_),
                        ) => {}
                        rumoca_ir_solve::TensorUpdateSubscript::Slice {
                            start,
                            dimensions,
                        } => {
                            let count = dimensions.iter().try_fold(1usize, |count, extent| {
                                count.checked_mul(*extent as usize)
                            });
                            value_count = value_count
                                .and_then(|value| count.and_then(|count| value.checked_mul(count)));
                            subscript_depends |= count
                                .is_none_or(|count| self.register_range_depends(*start, count));
                        }
                    }
                }
                base_count.is_none_or(|count| self.register_range_depends(base_start, count))
                    || value_count
                        .is_none_or(|count| self.register_range_depends(value_start, count))
                    || subscript_depends
            }
            LinearOp::TensorFill {
                value_start, lanes, ..
            } => self.register_range_depends(value_start, lanes),
            LinearOp::TensorIdentity { .. } => false,
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                lanes,
                ..
            } => {
                let offset = output.saturating_sub(dst_start) as usize;
                input == rumoca_ir_solve::TensorInputKind::Y
                    && lanes != 0
                    && offset < count.saturating_mul(lanes)
                    && offset.is_multiple_of(lanes)
                    && input_start.saturating_add(offset / lanes) == self.target_y_index
            }
            LinearOp::TableBounds { table_id, .. } => self.depends_on(table_id),
            LinearOp::TableLookup {
                table_id,
                column,
                input,
                ..
            }
            | LinearOp::TableLookupSlope {
                table_id,
                column,
                input,
                ..
            } => self.any_register_depends([table_id, column, input]),
            LinearOp::TableNextEvent { table_id, time, .. } => {
                self.any_register_depends([table_id, time])
            }
            LinearOp::RandomInitialState {
                local_seed,
                global_seed,
                ..
            } => self.any_register_depends([local_seed, global_seed]),
            LinearOp::RandomResult {
                state_start,
                state_len,
                ..
            }
            | LinearOp::RandomState {
                state_start,
                state_len,
                ..
            } => self.register_range_depends(state_start, state_len),
            LinearOp::ImpureRandomInit { seed, .. } => self.depends_on(seed),
            LinearOp::ImpureRandom { id, .. } => self.depends_on(id),
            LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
                self.any_register_depends([id, imin, imax])
            }
            LinearOp::FunctionFold {
                initial_start,
                capture_start,
                program,
                ..
            } => {
                self.register_range_depends(initial_start, program.carried_count)
                    || self.register_range_depends(capture_start, program.capture_count)
                    || row_reads_y_index(&program.update, self.target_y_index)
            }
            LinearOp::GuardedFunctionFold {
                initial_start,
                capture_start,
                activation,
                program,
                ..
            } => {
                self.depends_on(activation)
                    || self.register_range_depends(initial_start, program.carried_count)
                    || self.register_range_depends(capture_start, program.capture_count)
                    || row_reads_y_index(&program.update, self.target_y_index)
            }
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                condition,
                ..
            } => {
                initial.iter().any(|source| match *source {
                    rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                        self.register_range_depends(start, count)
                    }
                    // This query has no fold-carried dependency context. Keep
                    // the answer conservative instead of dropping an edge.
                    rumoca_ir_solve::FoldInitialSource::ParentCarried { .. } => true,
                }) || self.register_range_depends(capture_start, program.capture_count)
                    || condition.is_some_and(|condition| self.depends_on(condition))
                    || row_reads_y_index(&program.update, self.target_y_index)
            }
            LinearOp::FunctionConditional {
                capture_start,
                program,
                ..
            } => {
                self.register_range_depends(capture_start, program.capture_count)
                    || program.arms.iter().any(|arm| {
                        row_reads_y_index(&arm.condition, self.target_y_index)
                            || row_reads_y_index(&arm.result, self.target_y_index)
                    })
                    || row_reads_y_index(&program.fallback, self.target_y_index)
            }
            LinearOp::PureCall {
                input_starts,
                site,
                ..
            } => input_starts
                .iter()
                .zip(site.inputs())
                .any(|(start, value_type)| {
                    self.register_range_depends(*start, value_type.scalar_count() as usize)
                }),
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::LoadFoldCarried { .. }
            | LinearOp::LoadFoldIndex { .. }
            | LinearOp::LoadFoldCapture { .. }
            | LinearOp::LoadFunctionConditionalCapture { .. }
            | LinearOp::LoadFunctionConditionalCaptureRange { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutput { .. } => false,
        }
    }

    fn any_register_depends<const N: usize>(&mut self, registers: [u32; N]) -> bool {
        registers
            .into_iter()
            .any(|register| self.depends_on(register))
    }

    fn linear_solve_depends(&mut self, matrix_start: u32, rhs_start: u32, n: usize) -> bool {
        let Some(matrix_len) = n.checked_mul(n) else {
            return true;
        };
        self.register_range_depends(matrix_start, matrix_len)
            || self.register_range_depends(rhs_start, n)
    }

    fn register_range_depends(&mut self, start: u32, len: usize) -> bool {
        (0..len).any(|offset| {
            checked_reg_offset(start, offset).is_none_or(|register| self.depends_on(register))
        })
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
            _ => {}
        }
    }
}

pub(super) fn reg_depends_on_y_index(row: &[LinearOp], reg: u32, target_y_index: usize) -> bool {
    YDependencyAnalyzer::new(row, target_y_index).depends_on(reg)
}

fn checked_reg_offset(start: u32, offset: usize) -> Option<u32> {
    let offset = u32::try_from(offset).ok()?;
    start.checked_add(offset)
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
