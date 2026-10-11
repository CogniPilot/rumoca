//! Constructor-backed comparisons against aggregate snapshot semantics.

use super::*;
use rumoca_ir_solve::ScalarProgramBlock;

fn assert_projection(values: &[f64], op: LinearOp, expected: &[f64]) {
    let dst = op.dst_register().unwrap();
    let mut program = values
        .iter()
        .enumerate()
        .map(|(index, value)| LinearOp::Const {
            dst: Reg::try_from(index).unwrap(),
            value: *value,
        })
        .collect::<Vec<_>>();
    program.push(op);
    program.push(LinearOp::StoreOutputRange {
        start: dst,
        count: expected.len(),
        stride: 1,
    });
    let projected = lower_scalar_ops(&program, None).expect("bounded primal projection");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("snapshot_projection.mo"),
        0,
        1,
    );
    for (label, ops) in [("canonical", program), ("projected", projected)] {
        let block = ScalarProgramBlock::with_output_indices(
            vec![ops],
            vec![span],
            (0..expected.len()).collect(),
        )
        .expect("checked register flow admits the alias");
        let mut actual = vec![f64::NAN; expected.len()];
        crate::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut actual).unwrap();
        assert_eq!(actual, expected, "{label}");
    }
}

#[test]
fn scalar_projection_transpose_preserves_in_place_and_shifted_sources() {
    for (dst, src) in [(0, 0), (1, 0), (0, 1), (6, 0)] {
        let mut values = vec![0.0; 12];
        values[src as usize..src as usize + 6].copy_from_slice(&[1., 2., 3., 4., 5., 6.]);
        assert_projection(
            &values,
            LinearOp::TensorTranspose {
                dst_start: dst,
                src_start: src,
                rows: 3,
                columns: 2,
                element_width: 1,
                lanes: 1,
            },
            &[1., 4., 2., 5., 3., 6.],
        );
    }
}

#[test]
fn scalar_projection_matrix_multiply_preserves_either_overlapping_operand() {
    for dst in [0, 1, 4, 8] {
        assert_projection(
            &[1., 2., 3., 4., 5., 6., 7., 8., 0., 0., 0., 0.],
            LinearOp::MatrixMultiply {
                dst_start: dst,
                lhs_start: 0,
                rhs_start: 4,
                rows: 2,
                inner: 2,
                columns: 2,
                lanes: 1,
            },
            &[19., 22., 43., 50.],
        );
    }
}

#[test]
fn scalar_projection_binary_preserves_shifted_and_broadcast_operands() {
    for (dst, lhs, rhs, lhs_stride, rhs_stride, expected) in [
        (1, 0, 4, 1, 1, [11., 22., 33.]),
        (0, 1, 4, 1, 1, [12., 23., 34.]),
        (0, 1, 0, 1, 0, [3., 4., 5.]),
        (1, 0, 4, 0, 1, [11., 21., 31.]),
        (7, 0, 4, 1, 1, [11., 22., 33.]),
    ] {
        assert_projection(
            &[1., 2., 3., 4., 10., 20., 30., 0., 0., 0.],
            LinearOp::TensorBinary {
                dst_start: dst,
                op: BinaryOp::Add,
                lhs_start: lhs,
                rhs_start: rhs,
                count: 3,
                lanes: 1,
                lhs_stride,
                rhs_stride,
            },
            &expected,
        );
    }
}

#[test]
fn scalar_projection_snapshot_admission_is_compact_and_bounded() {
    let op = LinearOp::TensorBinary {
        dst_start: 1,
        op: BinaryOp::Add,
        lhs_start: 0,
        rhs_start: 0,
        count: EXPANSION_LIMIT / 2,
        lanes: 1,
        lhs_stride: 1,
        rhs_stride: 0,
    };
    assert!(lower_scalar_ops(&[op], None).is_none());
    let overflow = LinearOp::TensorBinary {
        dst_start: 0,
        op: BinaryOp::Add,
        lhs_start: 1,
        rhs_start: 0,
        count: 3,
        lanes: 1,
        lhs_stride: usize::MAX,
        rhs_stride: 1,
    };
    assert!(lower_scalar_ops(&[overflow], None).is_none());
}

#[test]
fn scalar_projection_snapshot_intersection_preserves_stride_gaps() {
    let op = LinearOp::TensorBinary {
        dst_start: 2,
        op: BinaryOp::Add,
        lhs_start: 0,
        rhs_start: 0,
        count: 2,
        lanes: 1,
        lhs_stride: 4,
        rhs_stride: 4,
    };
    assert_eq!(snapshots::result_count(&op), Some(0));
    assert_projection(&[1., 0., 0., 0., 3.], op, &[2., 6.]);
}
