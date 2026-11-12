//! Exact register provenance for rhs = force - coefficient * velocity.
use super::{MechanicalStepError, refused};
use crate::{BinaryOp, LinearOp, RegisterVersionProgram, TensorInputKind};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub enum MechanicalDampingSource {
    Literal { bits: u64 },
    Parameter { index: usize },
}

#[derive(Clone, Copy, Default)]
enum Origin {
    #[default]
    Other,
    Coefficient(MechanicalDampingSource),
    Velocity(usize),
    Product(usize, MechanicalDampingSource),
    DampedForce(usize, MechanicalDampingSource),
}

/// A structural query, not a physical classification of a named model.
/// Unknown writes invalidate provenance; no numeric sampling proves damping.
pub fn linear_damping_sources(
    setup: &[LinearOp],
    rhs_start: u32,
    velocities: &[usize],
) -> Result<Vec<MechanicalDampingSource>, MechanicalStepError> {
    RegisterVersionProgram::derive(setup)
        .ok_or_else(|| refused("invalid acceleration setup register flow"))?;
    let mut origins = BTreeMap::new();
    for op in setup {
        if crate::ScalarProgramBlock::checked_program_output_count(std::slice::from_ref(op))
            != Some(0)
        {
            return Err(refused("acceleration setup already publishes an output"));
        }
        let origin = match *op {
            LinearOp::Const { value, .. } if value.is_finite() && value >= 0.0 => {
                Origin::Coefficient(MechanicalDampingSource::Literal {
                    bits: value.to_bits(),
                })
            }
            LinearOp::LoadP { index, .. } => {
                Origin::Coefficient(MechanicalDampingSource::Parameter { index })
            }
            LinearOp::LoadY { index, .. } => Origin::Velocity(index),
            LinearOp::Move { src, .. } => read(&origins, src as usize),
            LinearOp::Binary { op, lhs, rhs, .. } => binary(
                op,
                read(&origins, lhs as usize),
                read(&origins, rhs as usize),
            ),
            _ => Origin::Other,
        };
        let tensor = tensor_origins(op, &origins)?;
        write_origins(&mut origins, op, origin, tensor)?;
    }
    velocities
        .iter()
        .enumerate()
        .map(|(row, &velocity)| {
            let index = (rhs_start as usize)
                .checked_add(row)
                .ok_or_else(|| refused("rhs extent overflows"))?;
            match origins.get(&index) {
                Some(Origin::DampedForce(source, coefficient)) if *source == velocity => {
                    Ok(*coefficient)
                }
                _ => Err(refused(
                    "rhs has no exact coefficient-times-original-velocity subtraction",
                )),
            }
        })
        .collect()
}

fn write_origins(
    origins: &mut BTreeMap<usize, Origin>,
    op: &LinearOp,
    scalar: Origin,
    tensor: Option<Vec<Origin>>,
) -> Result<(), MechanicalStepError> {
    let Some(start) = op.dst_register().map(|r| r as usize) else {
        return Ok(());
    };
    let end = start
        .checked_add(op.dst_register_count())
        .ok_or_else(|| refused("destination extent overflows"))?;
    let overwritten: Vec<_> = origins.range(start..end).map(|(&key, _)| key).collect();
    for key in overwritten {
        origins.remove(&key);
    }
    match tensor {
        Some(values) => {
            for (offset, origin) in values.into_iter().enumerate() {
                origins.insert(start + offset, origin);
            }
        }
        None if op.dst_register_count() == 1 => {
            origins.insert(start, scalar);
        }
        None => {}
    }
    Ok(())
}

fn read(origins: &BTreeMap<usize, Origin>, register: usize) -> Origin {
    origins.get(&register).copied().unwrap_or_default()
}

fn tensor_origins(
    op: &LinearOp,
    origins: &BTreeMap<usize, Origin>,
) -> Result<Option<Vec<Origin>>, MechanicalStepError> {
    match *op {
        LinearOp::TensorLoad {
            input,
            input_start,
            count,
            seed_start: None,
            lanes: 1,
            ..
        } => (0..count)
            .map(|offset| {
                let index = input_start
                    .checked_add(offset)
                    .ok_or_else(|| refused("input extent overflows"))?;
                Ok(match input {
                    TensorInputKind::Y => Origin::Velocity(index),
                    TensorInputKind::P => {
                        Origin::Coefficient(MechanicalDampingSource::Parameter { index })
                    }
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Some),
        LinearOp::TensorBinary {
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
            ..
        } => (0..count)
            .map(|offset| {
                let lhs = (lhs_start as usize)
                    .checked_add(
                        offset
                            .checked_mul(lhs_stride)
                            .ok_or_else(|| refused("tensor source overflows"))?,
                    )
                    .ok_or_else(|| refused("tensor source overflows"))?;
                let rhs = (rhs_start as usize)
                    .checked_add(
                        offset
                            .checked_mul(rhs_stride)
                            .ok_or_else(|| refused("tensor source overflows"))?,
                    )
                    .ok_or_else(|| refused("tensor source overflows"))?;
                Ok(binary(op, read(origins, lhs), read(origins, rhs)))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(Some),
        _ => Ok(None),
    }
}

fn binary(op: BinaryOp, lhs: Origin, rhs: Origin) -> Origin {
    match (op, lhs, rhs) {
        (BinaryOp::Mul, Origin::Coefficient(coefficient), Origin::Velocity(state))
        | (BinaryOp::Mul, Origin::Velocity(state), Origin::Coefficient(coefficient)) => {
            Origin::Product(state, coefficient)
        }
        (BinaryOp::Sub, _, Origin::Product(state, coefficient)) => {
            Origin::DampedForce(state, coefficient)
        }
        _ => Origin::Other,
    }
}
