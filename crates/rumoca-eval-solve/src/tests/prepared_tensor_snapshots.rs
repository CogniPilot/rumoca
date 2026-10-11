//! Before probes at the ordinary checked construction/prepared execution boundary.

use super::*;

fn assert_prepared_snapshot(values: &[f64], operation: LinearOp, expected: &[f64]) {
    let destination = operation.dst_register().unwrap();
    let mut row = values
        .iter()
        .enumerate()
        .map(|(index, value)| LinearOp::Const {
            dst: Reg::try_from(index).unwrap(),
            value: *value,
        })
        .collect::<Vec<_>>();
    row.push(operation);
    row.push(LinearOp::StoreOutputRange {
        start: destination,
        count: expected.len(),
        stride: 1,
    });
    let block = ScalarProgramBlock::with_output_indices(
        vec![row],
        vec![fixture_span()],
        (0..expected.len()).collect(),
    )
    .expect("canonical issuer admits initialized aggregate operand aliases");
    let mut reference = vec![f64::NAN; expected.len()];
    eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut reference).unwrap();
    assert_eq!(reference, expected, "checked aggregate snapshot reference");
    // This public preparation path issues its own register/input facts. It has
    // no state, parameters, branch selectors or manually selected fast flag.
    let compute = rumoca_ir_solve::ComputeBlock::from_scalar_program_block(block);
    let prepared = PreparedComputeBlock::new(&compute).expect("checked compute block prepares");
    let mut actual = vec![f64::NAN; expected.len()];
    prepared
        .eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut actual)
        .unwrap();
    assert_eq!(actual, expected, "ordinary prepared native execution");
}

#[test]
fn prepared_tensor_snapshot_cross_keeps_both_operand_values() {
    assert_prepared_snapshot(
        &[1., 2., 3., 4., 5., 6.],
        LinearOp::TensorCross {
            dst_start: 0,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        },
        &[-3., 6., -3.],
    );
}

#[test]
fn prepared_tensor_snapshot_transpose_in_place_matches_checked_reference() {
    for dst in [6, 0] {
        assert_prepared_snapshot(
            &[1., 2., 3., 4., 5., 6., 0., 0., 0., 0., 0., 0.],
            LinearOp::TensorTranspose {
                dst_start: dst,
                src_start: 0,
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
fn prepared_tensor_snapshot_matmul_alias_matches_checked_reference() {
    for dst in [8, 0, 4] {
        assert_prepared_snapshot(
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
fn prepared_tensor_snapshot_binary_broadcast_matches_checked_reference() {
    for dst in [4, 0] {
        assert_prepared_snapshot(
            &[1., 2., 3., 4., 0., 0., 0.],
            LinearOp::TensorBinary {
                dst_start: dst,
                op: BinaryOp::Add,
                lhs_start: 1,
                rhs_start: 0,
                count: 3,
                lanes: 1,
                lhs_stride: 1,
                rhs_stride: 0,
            },
            &[3., 4., 5.],
        );
    }
}
