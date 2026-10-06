use super::*;
use crate::{ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock, TensorNodeMetadata};

fn owner_span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::from_source_name("stacked.mo"), 0, 1)
}

fn constant_row_node() -> ComputeNode {
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            owner_span()
                .require_provenance("stacked fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("constant row is computable"),
    )
}

fn linear_solve_node(n: usize) -> ComputeNode {
    ComputeNode::LinSolve {
        setup_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
        matrix_start: 0,
        rhs_start: 0,
        n,
        next_reg: 1,
        matrix_pattern: crate::fixture_pattern(n, n, false),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn matrix_product_node(m: usize, n: usize) -> ComputeNode {
    ComputeNode::MatMul {
        lhs_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
        lhs_start: 0,
        rhs_ops: vec![LinearOp::LoadY { dst: 1, index: 0 }],
        rhs_start: 1,
        m,
        k: 1,
        n,
        lhs_pattern: crate::fixture_pattern(m, 1, false),
        rhs_pattern: crate::fixture_pattern(1, n, false),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

/// A stacked block mixing a constant row, a linear solve and a matrix product:
/// the dense nodes read every column over their own rows and the constant row
/// reads none, so the stacked pattern equals that scalar view cell for cell.
#[test]
fn dense_nodes_in_a_stacked_block_are_full_over_their_own_rows() {
    let block = ComputeBlock {
        nodes: vec![
            constant_row_node(),
            linear_solve_node(2),
            matrix_product_node(2, 1),
        ],
    };
    let (rows, columns) = (5usize, 3usize);
    let pattern = StructuralPattern::derive_from_compute_jvp(&block, rows, columns, owner_span())
        .expect("stacked block derives");
    for row in 0..rows as u32 {
        for column in 0..columns as u32 {
            assert_eq!(pattern.contains(row, column), row != 0, "({row}, {column})");
        }
    }
    assert_eq!(pattern.nonzero_upper_bound(), Some(4 * columns));
}
