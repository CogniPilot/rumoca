//! Checked direct-read kinematics for the optional Cartesian step profile.

use serde::{Deserialize, Serialize};

use crate::{ComputeBlock, ComputeNode, LinearOp};

/// Complete, disjoint position/velocity pairs bound to their derivative rows.
/// No caller-provided names or coordinate offsets establish kinematics.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CartesianPairPlan {
    state_count: usize,
    pairs: Vec<[usize; 2]>,
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Cartesian partition refused: {0}")]
pub struct CartesianPairError(pub String);

impl CartesianPairPlan {
    /// Check a producer's complete partition against the actual derivative
    /// program. Replay invokes this same check at the Solve root boundary.
    pub fn checked(
        state_count: usize,
        pairs: Vec<[usize; 2]>,
        derivative: &ComputeBlock,
    ) -> Result<Self, CartesianPairError> {
        let reads = direct_state_derivative_reads(derivative, state_count)
            .ok_or_else(|| CartesianPairError("unsupported derivative output form".into()))?;
        if state_count == 0 || pairs.len().checked_mul(2) != Some(state_count) {
            return Err(CartesianPairError(
                "partition does not cover all states".into(),
            ));
        }
        let mut covered = vec![false; state_count];
        for &[position, velocity] in &pairs {
            if position >= state_count || velocity >= state_count || position == velocity {
                return Err(CartesianPairError(
                    "invalid position/velocity extent".into(),
                ));
            }
            if covered[position] || covered[velocity] {
                return Err(CartesianPairError(
                    "overlapping position/velocity pairs".into(),
                ));
            }
            if reads[position] != Some(velocity) || reads[velocity].is_some() {
                return Err(CartesianPairError(
                    "position derivative is not its velocity".into(),
                ));
            }
            covered[position] = true;
            covered[velocity] = true;
        }
        Ok(Self { state_count, pairs })
    }

    pub fn state_count(&self) -> usize {
        self.state_count
    }

    pub fn pairs(&self) -> &[[usize; 2]] {
        &self.pairs
    }

    pub fn validate(&self, derivative: &ComputeBlock) -> Result<(), CartesianPairError> {
        Self::checked(self.state_count, self.pairs.clone(), derivative).map(|_| ())
    }
}

/// Read-only register provenance: `Some(i)` means this output is exactly a
/// direct `Y[i]` read, including copies. Arithmetic and calls never prove one.
/// Checked affine identity Maps retain their original index maps. A coupled
/// LinSolve owns nonidentity acceleration rows; other aggregates decline.
pub fn direct_state_derivative_reads(
    block: &ComputeBlock,
    state_count: usize,
) -> Option<Vec<Option<usize>>> {
    Some(
        crate::state_derivative_sources::derive(block, state_count, None)?
            .rows
            .into_iter()
            .map(|source| source.and_then(|source| source.direct()))
            .collect(),
    )
}

/// This query proves only a single direct state read per affine invocation.
/// Every executed op is accounted for, including unused loads. It does not
/// turn a Map into backend scalar programs or infer physical coordinates.
pub(crate) fn identity_map_reads(
    node: &ComputeNode,
    state_count: usize,
) -> Option<(Vec<usize>, Vec<usize>)> {
    use std::collections::BTreeMap;
    let ComputeNode::Map {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        ..
    } = node
    else {
        return None;
    };
    node.validate_shape_contract("Cartesian identity Map", 0)
        .ok()?;
    if domain.scalar_count().ok()? > state_count || !const_strides.is_empty() {
        return None;
    }
    crate::RegisterVersionProgram::derive(base_ops)?;
    let mut registers = BTreeMap::new();
    let mut output = None;
    for (position, op) in base_ops.iter().enumerate() {
        match *op {
            LinearOp::LoadY { dst, index } => {
                let map = crate::TensorOutputMap {
                    start: index,
                    strides: load_strides
                        .iter()
                        .find(|s| s.op_position == position)
                        .map(|s| s.terms.clone())
                        .unwrap_or_default(),
                };
                if map.output_count(domain).ok()? > state_count {
                    return None;
                }
                registers.insert(dst, map);
            }
            LinearOp::Move { dst, src } => {
                let source = registers.get(&src)?.clone();
                registers.insert(dst, source);
            }
            LinearOp::StoreOutput { src } if output.is_none() => {
                output = Some(registers.get(&src)?.clone());
            }
            _ => return None,
        }
    }
    let indices = output_map.output_indices(domain).ok()?;
    let sources = output?.output_indices(domain).ok()?;
    (indices.len() == sources.len()).then_some((indices, sources))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ScalarProgramBlock, TensorInputKind, UnaryOp};
    use rumoca_core::{ProvenanceSpan, SourceId, Span};

    fn provenance() -> ProvenanceSpan {
        Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
            .require_provenance("Cartesian fixture")
            .unwrap()
    }

    fn derivative(second: Vec<LinearOp>) -> ComputeBlock {
        ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(
                vec![
                    vec![
                        LinearOp::LoadY { dst: 0, index: 1 },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                    second,
                ],
                provenance(),
            )
            .unwrap(),
        )
    }

    fn oscillator() -> ComputeBlock {
        derivative(vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ])
    }

    #[test]
    fn actual_direct_read_is_required_and_pairs_cover_exactly_once() {
        let rhs = oscillator();
        assert!(CartesianPairPlan::checked(2, vec![[0, 1]], &rhs).is_ok());
        for pairs in [
            vec![],
            vec![[1, 0]],
            vec![[0, 0]],
            vec![[0, 2]],
            vec![[0, 1], [0, 1]],
        ] {
            assert!(CartesianPairPlan::checked(2, pairs, &rhs).is_err());
        }
    }

    #[test]
    fn packed_loads_and_copies_preserve_exact_state_components() {
        let rhs = ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(
                vec![
                    vec![
                        LinearOp::TensorLoad {
                            dst_start: 0,
                            input: TensorInputKind::Y,
                            input_start: 2,
                            count: 2,
                            seed_start: None,
                            lanes: 1,
                        },
                        LinearOp::Move { dst: 2, src: 0 },
                        LinearOp::Const {
                            dst: 0,
                            value: 99.0,
                        },
                        LinearOp::StoreOutput { src: 2 },
                        LinearOp::StoreOutputRange {
                            start: 1,
                            count: 1,
                            stride: 1,
                        },
                    ],
                    vec![
                        LinearOp::Const { dst: 0, value: 0.0 },
                        LinearOp::Move { dst: 1, src: 0 },
                        LinearOp::StoreOutputRange {
                            start: 0,
                            count: 2,
                            stride: 1,
                        },
                    ],
                ],
                provenance(),
            )
            .unwrap(),
        );
        let plan = CartesianPairPlan::checked(4, vec![[0, 2], [1, 3]], &rhs).unwrap();
        assert_eq!(plan.pairs(), [[0, 2], [1, 3]]);
    }

    #[test]
    fn register_overwrites_invalidate_identity_and_wire_rebinds_to_source() {
        let good = oscillator();
        let plan = CartesianPairPlan::checked(2, vec![[0, 1]], &good).unwrap();
        let mut bad = good.clone();
        bad.nodes = derivative(vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ])
        .nodes;
        let ComputeNode::ScalarPrograms(rows) = &bad.nodes[0] else {
            unreachable!()
        };
        let mut programs = rows.programs().to_vec();
        programs[0].insert(1, LinearOp::Const { dst: 0, value: 0.0 });
        bad = ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(programs, provenance()).unwrap(),
        );
        let replay: CartesianPairPlan =
            serde_json::from_str(&serde_json::to_string(&plan).unwrap()).unwrap();
        assert!(replay.validate(&bad).is_err());
    }

    #[test]
    fn root_json_and_binary_replay_bind_the_partition_to_the_actual_rows() {
        let rhs = oscillator();
        let layout = crate::VarLayout::from_parts(
            [
                ("q".into(), crate::scalar_slot_y(0)),
                ("v".into(), crate::scalar_slot_y(1)),
            ]
            .into(),
            2,
            0,
        );
        let mut problem = crate::SolveProblem::with_derivative_rhs(rhs, layout).unwrap();
        problem.cartesian_pairs = Some(
            CartesianPairPlan::checked(2, vec![[0, 1]], &problem.continuous.derivative_rhs)
                .unwrap(),
        );
        let wire = serde_json::to_value(&problem).unwrap();
        let replay: crate::SolveProblem = serde_json::from_value(wire.clone()).unwrap();
        assert_eq!(replay.cartesian_pairs, problem.cartesian_pairs);
        let binary: crate::SolveProblem =
            bincode::deserialize(&bincode::serialize(&problem).unwrap()).unwrap();
        assert_eq!(binary.cartesian_pairs, problem.cartesian_pairs);

        let mut forged = wire.clone();
        forged["cartesian_pairs"]["pairs"][0][1] = 0.into();
        assert!(serde_json::from_value::<crate::SolveProblem>(forged).is_err());
        let mut old = wire.clone();
        old["schema_version"] = (crate::SOLVE_SCHEMA_VERSION - 1).into();
        assert!(serde_json::from_value::<crate::SolveProblem>(old).is_err());
        let mut absent = wire;
        absent.as_object_mut().unwrap().remove("cartesian_pairs");
        assert!(
            serde_json::from_value::<crate::SolveProblem>(absent)
                .unwrap()
                .cartesian_pairs
                .is_none()
        );
    }
}
