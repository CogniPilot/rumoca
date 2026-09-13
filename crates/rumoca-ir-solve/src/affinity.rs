//! Constructor-owned degree bounds relative to a projection's unknowns.

mod registers;
#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    BinaryOp, ComputeBlock, ComputeNode, LinearOp, RefreshPlan, ScalarProgramBlock,
    TensorInputKind, UnaryOp,
};
use registers::Registers;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Degree {
    Independent,
    Affine,
    #[default]
    Nonlinear,
}

impl Degree {
    fn product(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Independent, rhs) => rhs,
            (lhs, Self::Independent) => lhs,
            _ => Self::Nonlinear,
        }
    }

    fn binary(self, operator: BinaryOp, rhs: Self) -> Self {
        match operator {
            BinaryOp::Add | BinaryOp::Sub => self.max(rhs),
            BinaryOp::Mul => self.product(rhs),
            BinaryOp::Div if rhs == Self::Independent => self,
            _ if self == Self::Independent && rhs == Self::Independent => Self::Independent,
            _ => Self::Nonlinear,
        }
    }
}

pub(crate) fn projection_affinities(
    source: &ComputeBlock,
    plan: &RefreshPlan,
) -> BTreeMap<usize, bool> {
    let Some(programs) = scalar_output_programs(source) else {
        return BTreeMap::new();
    };
    plan.simultaneous_block_indices
        .iter()
        .zip(&plan.simultaneous_plan.blocks)
        .map(|(&index, block)| {
            let targets = block.y_indices.iter().copied().collect::<BTreeSet<_>>();
            let affine = block.rows.iter().all(|row| {
                let Some((program, offset)) = programs.get(row) else {
                    return false;
                };
                program_degree(program, &targets, *offset)
                    .is_some_and(|degree| degree != Degree::Nonlinear)
            });
            (index, affine)
        })
        .fold(BTreeMap::new(), |mut proofs, (index, affine)| {
            proofs
                .entry(index)
                .and_modify(|prior| *prior &= affine)
                .or_insert(affine);
            proofs
        })
}

fn scalar_output_programs(source: &ComputeBlock) -> Option<BTreeMap<usize, (&[LinearOp], usize)>> {
    let mut programs = BTreeMap::new();
    let mut cursor = 0;
    for (node_index, node) in source.nodes.iter().enumerate() {
        let ComputeNode::ScalarPrograms(block) = node else {
            return None;
        };
        let outputs = block
            .compute_block_output_indices("projection affinity", node_index, cursor)
            .ok()?;
        cursor = block
            .advance_compute_block_output_cursor("projection affinity", node_index, cursor)
            .ok()?;
        let selected = block.programs().iter().flat_map(|program| {
            (0..ScalarProgramBlock::program_output_count(program))
                .map(|offset| (program.as_slice(), offset))
        });
        for (row, selected) in outputs.into_iter().zip(selected) {
            if programs.insert(row, selected).is_some() {
                return None;
            }
        }
    }
    Some(programs)
}

fn program_degree(
    program: &[LinearOp],
    targets: &BTreeSet<usize>,
    output_offset: usize,
) -> Option<Degree> {
    let mut registers = Registers::default();
    let mut output_start = 0;
    for operation in program {
        match operation {
            LinearOp::StoreOutput { src } => {
                if output_start == output_offset {
                    return registers.read(*src, 1);
                }
                output_start += 1;
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                let offset = output_offset.checked_sub(output_start)?;
                if offset < *count {
                    let register =
                        start.checked_add(u32::try_from(offset.checked_mul(*stride)?).ok()?)?;
                    return registers.read(register, 1);
                }
                output_start = output_start.checked_add(*count)?;
            }
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => {
                let inputs = call_inputs(&registers, input_starts, site.inputs())?;
                store_call(
                    &mut registers,
                    *dst_start,
                    site.output_degrees(&inputs)?,
                    site.outputs(),
                )?;
            }
            LinearOp::PureCallDirectional {
                dst_start,
                input_starts,
                site,
            } => {
                let inputs = call_inputs(&registers, input_starts, site.inputs())?;
                store_call(
                    &mut registers,
                    *dst_start,
                    site.output_degrees(&inputs)?,
                    site.outputs(),
                )?;
            }
            _ => {
                let degree = operation_degree(operation, &registers, targets)?;
                registers.write(
                    operation.dst_register()?,
                    operation.dst_register_count(),
                    degree,
                )?;
            }
        }
    }
    None
}

fn call_inputs(
    registers: &Registers,
    starts: &[u32],
    types: &[crate::SolveValueType],
) -> Option<Vec<Degree>> {
    if starts.len() != types.len() {
        return None;
    }
    starts
        .iter()
        .zip(types)
        .map(|(&start, value)| registers.read(start, value.scalar_count() as usize))
        .collect()
}

fn store_call(
    registers: &mut Registers,
    mut start: u32,
    degrees: Vec<Degree>,
    outputs: &[crate::SolvePureCallOutput],
) -> Option<()> {
    if degrees.len() != outputs.len() {
        return None;
    }
    for (degree, output) in degrees.into_iter().zip(outputs) {
        let count = output.value_type().scalar_count();
        registers.write(start, count as usize, degree)?;
        start = start.checked_add(count)?;
    }
    Some(())
}

fn operation_degree(
    operation: &LinearOp,
    registers: &Registers,
    targets: &BTreeSet<usize>,
) -> Option<Degree> {
    let read = |register| registers.read(register, 1);
    Some(match *operation {
        LinearOp::Const { value, .. } if value.is_finite() => Degree::Independent,
        LinearOp::LoadP { .. } | LinearOp::LoadTime { .. } => Degree::Independent,
        LinearOp::LoadY { index, .. } => {
            if targets.contains(&index) {
                Degree::Affine
            } else {
                Degree::Independent
            }
        }
        LinearOp::Move { src, .. } => read(src)?,
        LinearOp::Unary { op, arg, .. } => match (op, read(arg)?) {
            (UnaryOp::Neg, value) | (_, value @ Degree::Independent) => value,
            _ => Degree::Nonlinear,
        },
        LinearOp::Binary { op, lhs, rhs, .. } => read(lhs)?.binary(op, read(rhs)?),
        LinearOp::Compare { lhs, rhs, .. } => {
            if read(lhs)? == Degree::Independent && read(rhs)? == Degree::Independent {
                Degree::Independent
            } else {
                Degree::Nonlinear
            }
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            if read(cond)? == Degree::Independent {
                read(if_true)?.max(read(if_false)?)
            } else {
                Degree::Nonlinear
            }
        }
        LinearOp::TensorLoad {
            input,
            input_start,
            count,
            lanes: 1,
            ..
        } => match input {
            TensorInputKind::P => Degree::Independent,
            TensorInputKind::Y => {
                if targets
                    .range(input_start..input_start.checked_add(count)?)
                    .next()
                    .is_some()
                {
                    Degree::Affine
                } else {
                    Degree::Independent
                }
            }
        },
        LinearOp::TensorFill {
            value_start,
            lanes: 1,
            ..
        } => read(value_start)?,
        LinearOp::TensorBinary {
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
            ..
        } => registers
            .read_strided(lhs_start, count, lhs_stride)?
            .binary(op, registers.read_strided(rhs_start, count, rhs_stride)?),
        LinearOp::TensorCross {
            lhs_start,
            rhs_start,
            lanes: 1,
            ..
        } => registers
            .read(lhs_start, 3)?
            .product(registers.read(rhs_start, 3)?),
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes: 1,
            ..
        } => registers
            .read(lhs_start, rows.checked_mul(inner)?)?
            .product(registers.read(rhs_start, inner.checked_mul(columns)?)?),
        _ => return None,
    })
}
