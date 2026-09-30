//! Issue #362: an explicit state-derivative row over array slices,
//! `der(u[2:N-1]) = k * (u[1:N-2] - 2 * u[2:N-1] + u[3:N])`, is one tensor
//! program with `N - 2` outputs. Residual preparation certifies every output as
//! a target assignment and materializes one program per output, which grows
//! with the square of the slice length; an evaluation-only block derives none.

use super::*;
use rumoca_ir_solve::{ComputeBlock, ComputeNode, TensorInputKind};

const STATES: usize = 12;
const INTERIOR: usize = STATES - 2;

// `k * (u[0..] - weight * u[1..] + u[2..])` over the interior of `u`,
/// `k = p[0]`.
fn stencil_row(weight: f64) -> Vec<LinearOp> {
    let interior = INTERIOR as u32;
    let load = |dst, input_start| LinearOp::TensorLoad {
        dst_start: dst,
        input: TensorInputKind::Y,
        input_start,
        seed_start: None,
        count: INTERIOR,
        lanes: 1,
    };
    let binary = |dst, op, lhs, rhs, lhs_stride| LinearOp::TensorBinary {
        dst_start: dst,
        op,
        lhs_start: lhs,
        rhs_start: rhs,
        count: INTERIOR,
        lhs_stride,
        rhs_stride: 1,
        lanes: 1,
    };
    let left = 2;
    let centre = left + interior;
    let right = centre + interior;
    let doubled = right + interior;
    let difference = doubled + interior;
    let sum = difference + interior;
    let scaled = sum + interior;
    vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const {
            dst: 1,
            value: weight,
        },
        load(left, 0),
        load(centre, 1),
        load(right, 2),
        binary(doubled, BinaryOp::Mul, 1, centre, 0),
        binary(difference, BinaryOp::Sub, left, doubled, 1),
        binary(sum, BinaryOp::Add, difference, right, 1),
        binary(scaled, BinaryOp::Mul, 0, sum, 0),
        LinearOp::StoreOutputRange {
            start: scaled,
            count: INTERIOR,
            stride: 1,
        },
    ]
}

fn stencil_block() -> ScalarProgramBlock {
    weighted_stencil_block(2.0)
}

fn weighted_stencil_block(weight: f64) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("heat_slices.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_source_span(
        vec![stencil_row(weight)],
        span.require_provenance("sliced heat derivative").unwrap(),
    )
    .unwrap()
}

fn tensor_affine_count(block: &PreparedScalarProgramBlock) -> usize {
    block
        .row_tensor_affine_assignments
        .iter()
        .map(std::collections::BTreeMap::len)
        .sum()
}

fn assignment_shape_count(block: &PreparedScalarProgramBlock) -> usize {
    block
        .row_assignment_shapes
        .iter()
        .map(|row| row.len())
        .sum()
}

fn state() -> Vec<f64> {
    (0..STATES)
        .map(|index| (index as f64 * 0.7).sin())
        .collect()
}

#[test]
fn residual_preparation_materializes_one_program_per_slice_output() {
    let full = PreparedScalarProgramBlock::new(stencil_block()).unwrap();
    // Each output isolates every state it reads: the certificates grow with
    // the square of the slice length.
    assert!(tensor_affine_count(&full) >= INTERIOR * INTERIOR);
}

#[test]
fn evaluation_blocks_derive_no_assignment_certificates_for_slice_rows() {
    let evaluation = PreparedEvaluationBlock::new(stencil_block()).unwrap();
    assert_eq!(assignment_shape_count(&evaluation.0), 0);
    assert_eq!(tensor_affine_count(&evaluation.0), 0);

    let compute = PreparedComputeBlock::new(&ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(stencil_block())],
    })
    .unwrap();
    let [PreparedComputeNode::ScalarPrograms(node)] = compute.nodes.as_slice() else {
        panic!("one scalar-program node");
    };
    assert_eq!(assignment_shape_count(node), 0);
    assert_eq!(tensor_affine_count(node), 0);

    let replaced = PreparedEvaluationBlock::with_replaced_programs(
        &evaluation,
        weighted_stencil_block(3.0),
        &[0],
    )
    .unwrap();
    assert_eq!(tensor_affine_count(&replaced.0), 0);
}

#[test]
fn evaluation_blocks_evaluate_the_residual_block_outputs() {
    let y = state();
    let p = [3.0];
    let expected = (0..INTERIOR)
        .map(|index| 3.0 * (y[index] - 2.0 * y[index + 1] + y[index + 2]))
        .collect::<Vec<_>>();
    let mut full = vec![0.0; INTERIOR];
    PreparedScalarProgramBlock::new(stencil_block())
        .unwrap()
        .eval_with_context(&y, &p, 0.0, RowEvalContext::default(), &mut full)
        .unwrap();
    let mut evaluation = vec![0.0; INTERIOR];
    PreparedEvaluationBlock::new(stencil_block())
        .unwrap()
        .eval_with_context(&y, &p, 0.0, RowEvalContext::default(), &mut evaluation)
        .unwrap();
    let mut compute = vec![0.0; INTERIOR];
    PreparedComputeBlock::new(&ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(stencil_block())],
    })
    .unwrap()
    .eval_with_context(&y, &p, 0.0, RowEvalContext::default(), &mut compute)
    .unwrap();
    assert_eq!(full, expected);
    assert_eq!(evaluation, expected);
    assert_eq!(compute, expected);
}
