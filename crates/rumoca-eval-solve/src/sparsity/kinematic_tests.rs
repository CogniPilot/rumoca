//! The kinematic state partition over tensor compute nodes: each cell is
//! judged by the scalar row it expands to.

use rumoca_core::{IndexBinderProvenance, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilIndexStrideTerm, AffineStencilLoadStride, AlgebraicProjectionBlock,
    AlgebraicProjectionPlan, BinaryOp, CompareOp, ComputeBlock, ComputeNode, ContinuousSolveSystem,
    LinearOp, NonKinematicReason, ScalarProgramBlock, TensorNodeMetadata, TensorOutputMap,
};

use super::derive_kinematic_states;

fn span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("kinematic_tensor.mo"),
        1,
        2,
    )
}

/// `i in 1:n`.
fn domain(n: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: n,
            step: 1,
        }],
    }
}

/// One cell per `i in 1:n` writing output `output + i - 1` from `base_ops`,
/// whose first operation is a `LoadY` advancing by one slot per cell.
fn map(n: i64, output: usize, base_ops: Vec<LinearOp>) -> ComputeNode {
    let domain = domain(n);
    ComputeNode::Map {
        output_map: TensorOutputMap::dense_contiguous(output, &domain).unwrap(),
        domain,
        base_ops,
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

/// `y[index]`, strided per cell.
fn copy(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::StoreOutput { src: 0 },
    ]
}

/// `-y[index]`, strided per cell.
fn negated(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::Const {
            dst: 1,
            value: -1.0,
        },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// `y[lhs] - y[rhs]`, the residual of `y[lhs] = y[rhs]`.
fn residual(lhs: usize, rhs: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: lhs },
        LinearOp::LoadY { dst: 1, index: rhs },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn scalar_node(programs: Vec<Vec<LinearOp>>) -> ComputeNode {
    let spans = vec![span(); programs.len()];
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_program_spans(programs, spans).expect("valid test programs"),
    )
}

/// One single-unknown projection block per `(row, y)`.
fn projection(blocks: &[(usize, usize)]) -> AlgebraicProjectionPlan {
    AlgebraicProjectionPlan {
        blocks: blocks
            .iter()
            .map(|&(row, y)| AlgebraicProjectionBlock {
                rows: vec![row],
                y_indices: vec![y],
                ..AlgebraicProjectionBlock::default()
            })
            .collect::<Vec<_>>()
            .into(),
    }
}

/// Positions `q[1:3]` (slots 0..3) and velocities `v[1:3]` (slots 3..6):
/// `der(q) = v` and `der(v) = -q`, both as tensor maps.
#[test]
fn a_vector_position_with_a_vector_velocity_is_kinematic_cell_by_cell() {
    let continuous = ContinuousSolveSystem {
        derivative_rhs: ComputeBlock {
            nodes: vec![map(3, 0, copy(3)), map(3, 3, negated(0))],
        },
        ..ContinuousSolveSystem::default()
    };
    let partition = derive_kinematic_states(&continuous, 6, 6);
    assert_eq!(partition.states(), [0, 1, 2]);
    for cell in 0..3 {
        assert_eq!(
            partition.reason(3 + cell),
            Some(NonKinematicReason::Velocity { of: cell })
        );
    }
}

/// `der(q) = v` and `der(v) = -q` for one position, then one tensor map
/// `der(z[i]) = a[i]` with `a[1] = q` and `a[2] = v`: the copied scalar row
/// and the computed cells sit in one derivative block, and the two cells of
/// the one map land on either side of the partition by what each reads.
#[test]
fn a_tensor_node_mixing_copied_and_computed_cells_is_judged_per_cell() {
    let continuous = ContinuousSolveSystem {
        derivative_rhs: ComputeBlock {
            nodes: vec![scalar_node(vec![copy(1), negated(0)]), map(2, 2, copy(4))],
        },
        implicit_rhs: ComputeBlock {
            nodes: vec![scalar_node(vec![residual(4, 0), residual(5, 1)])],
        },
        algebraic_projection_plan: projection(&[(0, 4), (1, 5)]),
        ..ContinuousSolveSystem::default()
    };
    let partition = derive_kinematic_states(&continuous, 4, 6);
    assert_eq!(partition.states(), [0, 2]);
    assert_eq!(
        partition.reason(1),
        Some(NonKinematicReason::Velocity { of: 0 })
    );
    assert_eq!(
        partition.reason(3),
        Some(NonKinematicReason::Closure { through: 1 })
    );
}

/// `der(z[i]) = a[i]` with `a[1] = z[1]` and `a[2] = (z[1] > 0)`: only the
/// second cell reaches a relation.
#[test]
fn a_relation_reached_by_one_cell_removes_only_that_cell() {
    let relation = vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Const { dst: 2, value: 0.0 },
        LinearOp::Compare {
            dst: 3,
            op: CompareOp::Gt,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let continuous = ContinuousSolveSystem {
        derivative_rhs: ComputeBlock {
            nodes: vec![map(2, 0, copy(2))],
        },
        implicit_rhs: ComputeBlock {
            nodes: vec![scalar_node(vec![residual(2, 0), relation])],
        },
        algebraic_projection_plan: projection(&[(0, 2), (1, 3)]),
        ..ContinuousSolveSystem::default()
    };
    let partition = derive_kinematic_states(&continuous, 2, 4);
    assert_eq!(partition.states(), [0]);
    assert_eq!(partition.reason(1), Some(NonKinematicReason::Relation));
}
