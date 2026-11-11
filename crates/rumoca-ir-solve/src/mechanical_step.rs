//! Source-bound coupled inertia and explicit linear damping for Cartesian steps.
mod damping;

use crate::{CartesianPairPlan, ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock};
use serde::{Deserialize, Serialize};

pub use damping::{MechanicalDampingSource, linear_damping_sources};

/// Complete direct kinematics plus one original coupled acceleration solve.
/// Root wire replay checks this relation against the actual derivative source.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MechanicalStepPlan {
    cartesian: CartesianPairPlan,
    dynamics_node: usize,
    damping: Vec<MechanicalDampingSource>,
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("mechanical step refused: {0}")]
pub struct MechanicalStepError(pub String);

impl MechanicalStepPlan {
    pub fn checked(
        cartesian: CartesianPairPlan,
        dynamics_node: usize,
        derivative: &ComputeBlock,
    ) -> Result<Self, MechanicalStepError> {
        cartesian
            .validate(derivative)
            .map_err(|e| refused(e.to_string()))?;
        let mut cursor = 0usize;
        let mut damping = None;
        for (index, node) in derivative.nodes.iter().enumerate() {
            if index == dynamics_node {
                damping = Some(dynamics_damping(node, cursor, &cartesian)?);
            } else {
                require_identity_kinematics(node, cartesian.state_count())?;
            }
            cursor = crate::tensor::advance_compute_output_cursor(
                "mechanical step",
                index,
                node,
                cursor,
            )
            .map_err(|e| refused(e.to_string()))?;
        }
        let damping = damping.ok_or_else(|| refused("acceleration source is absent"))?;
        Ok(Self {
            cartesian,
            dynamics_node,
            damping,
        })
    }

    pub fn cartesian(&self) -> &CartesianPairPlan {
        &self.cartesian
    }
    pub fn dynamics_node(&self) -> usize {
        self.dynamics_node
    }
    pub fn damping(&self) -> &[MechanicalDampingSource] {
        &self.damping
    }

    pub fn validate(&self, derivative: &ComputeBlock) -> Result<(), MechanicalStepError> {
        let checked = Self::checked(self.cartesian.clone(), self.dynamics_node, derivative)?;
        if checked != *self {
            return Err(refused("retained damping differs from the actual source"));
        }
        Ok(())
    }

    /// The original setup once, then M (row-major), already-damped force and D.
    /// This is prepared once; a backend does not reconstruct the source relation.
    pub fn coefficient_program(
        &self,
        derivative: &ComputeBlock,
    ) -> Result<ScalarProgramBlock, MechanicalStepError> {
        self.validate(derivative)?;
        let ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            next_reg,
            span,
            ..
        } = &derivative.nodes[self.dynamics_node]
        else {
            return Err(refused(
                "mechanical coefficient source is not a coupled solve",
            ));
        };
        let matrix_count = n
            .checked_mul(*n)
            .ok_or_else(|| refused("matrix extent overflows"))?;
        let mut program = setup_ops.clone();
        program.push(LinearOp::StoreOutputRange {
            start: *matrix_start,
            count: matrix_count,
            stride: 1,
        });
        program.push(LinearOp::StoreOutputRange {
            start: *rhs_start,
            count: *n,
            stride: 1,
        });
        for coefficient in &self.damping {
            program.push(match coefficient {
                MechanicalDampingSource::Literal { bits } => LinearOp::Const {
                    dst: *next_reg,
                    value: f64::from_bits(*bits),
                },
                MechanicalDampingSource::Parameter { index } => LinearOp::LoadP {
                    dst: *next_reg,
                    index: *index,
                },
            });
            program.push(LinearOp::StoreOutput { src: *next_reg });
        }
        ScalarProgramBlock::with_program_spans(vec![program], vec![*span])
            .map_err(|e| refused(e.to_string()))
    }
}

fn dynamics_damping(
    node: &ComputeNode,
    cursor: usize,
    cartesian: &CartesianPairPlan,
) -> Result<Vec<MechanicalDampingSource>, MechanicalStepError> {
    let ComputeNode::LinSolve {
        setup_ops,
        rhs_start,
        n,
        ..
    } = node
    else {
        return Err(refused("acceleration owner is not a coupled LinSolve"));
    };
    let end = cursor
        .checked_add(*n)
        .ok_or_else(|| refused("velocity extent overflows"))?;
    let mut velocities: Vec<_> = cartesian.pairs().iter().map(|pair| pair[1]).collect();
    velocities.sort_unstable();
    if *n == 0 || velocities != (cursor..end).collect::<Vec<_>>() {
        return Err(refused("one coupled solve must own every velocity row"));
    }
    linear_damping_sources(setup_ops, *rhs_start, &velocities)
}

fn require_identity_kinematics(
    node: &ComputeNode,
    count: usize,
) -> Result<(), MechanicalStepError> {
    if matches!(node, ComputeNode::Map { .. }) {
        return crate::cartesian_pairs::identity_map_reads(node, count)
            .map(|_| ())
            .ok_or_else(|| refused("Map has work not covered by the direct update"));
    }
    let ComputeNode::ScalarPrograms(block) = node else {
        return Err(refused("unsupported mechanical update node"));
    };
    for op in block.programs().iter().flatten() {
        match op {
            LinearOp::LoadY { index, .. } if *index < count => {}
            LinearOp::Move { .. }
            | LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. } => {}
            LinearOp::TensorLoad {
                input: crate::TensorInputKind::Y,
                input_start,
                count: width,
                seed_start: None,
                lanes: 1,
                ..
            } if input_start
                .checked_add(*width)
                .is_some_and(|end| end <= count) => {}
            _ => {
                return Err(refused(
                    "kinematic prefix has work not covered by the direct update",
                ));
            }
        }
    }
    Ok(())
}

fn refused(message: impl Into<String>) -> MechanicalStepError {
    MechanicalStepError(message.into())
}

#[cfg(test)]
mod tests;
