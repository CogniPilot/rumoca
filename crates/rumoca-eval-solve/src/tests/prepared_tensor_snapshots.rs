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

#[test]
fn prepared_tensor_snapshot_binary_preserves_broadcast_dual_lanes() {
    assert_prepared_snapshot(
        &[1., 10., 2., 20., 3., 30.],
        LinearOp::TensorBinary {
            dst_start: 0,
            op: BinaryOp::Add,
            lhs_start: 2,
            rhs_start: 0,
            count: 2,
            lanes: 2,
            lhs_stride: 1,
            rhs_stride: 0,
        },
        &[3., 30., 4., 40.],
    );
}

#[test]
fn prepared_tensor_snapshot_matmul_preserves_both_dual_operands() {
    for dst in [0, 8] {
        assert_prepared_snapshot(
            &[
                1., 10., 2., 20., 3., 30., 4., 40., 5., 50., 6., 60., 7., 70., 8., 80.,
            ],
            LinearOp::MatrixMultiply {
                dst_start: dst,
                lhs_start: 0,
                rhs_start: 8,
                rows: 2,
                inner: 2,
                columns: 2,
                lanes: 2,
            },
            &[19., 380., 22., 440., 43., 860., 50., 1000.],
        );
    }
}

#[test]
fn prepared_tensor_snapshot_transpose_preserves_trailing_axes_and_dual_lanes() {
    let values = (1..=24).map(f64::from).collect::<Vec<_>>();
    assert_prepared_snapshot(
        &values,
        LinearOp::TensorTranspose {
            dst_start: 0,
            src_start: 0,
            rows: 3,
            columns: 2,
            element_width: 2,
            lanes: 2,
        },
        &[
            1., 2., 3., 4., 13., 14., 15., 16., 5., 6., 7., 8., 17., 18., 19., 20., 9., 10., 11.,
            12., 21., 22., 23., 24.,
        ],
    );
}

#[test]
fn prepared_tensor_snapshot_long_selected_row_preserves_ordered_aliases() {
    let mut row = (0..64)
        .map(|dst| LinearOp::Const {
            dst,
            value: f64::from(dst + 1),
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::TensorTranspose {
        dst_start: 0,
        src_start: 0,
        rows: 3,
        columns: 2,
        element_width: 1,
        lanes: 1,
    });
    row.push(LinearOp::Select {
        dst: 64,
        cond: 63,
        if_true: 1,
        if_false: 2,
    });
    row.push(LinearOp::StoreOutput { src: 64 });
    let block = ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()])
        .expect("ordered initialized aliases retain a checked owner");
    let mut reference = [f64::NAN];
    eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut reference).unwrap();
    assert_eq!(reference, [4.0]);
    let compute = rumoca_ir_solve::ComputeBlock::from_scalar_program_block(block);
    let prepared = PreparedComputeBlock::new(&compute).unwrap();
    let mut actual = [f64::NAN];
    prepared
        .eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut actual)
        .unwrap();
    assert_eq!(actual, [4.0]);
}

#[test]
fn prepared_tensor_snapshot_unique_selected_row_retains_cached_lazy_execution() {
    let mut row = (0..63)
        .map(|dst| LinearOp::Const {
            dst,
            value: f64::from(dst + 1),
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::LoadY { dst: 63, index: 0 });
    row.push(LinearOp::TensorTranspose {
        dst_start: 64,
        src_start: 0,
        rows: 3,
        columns: 2,
        element_width: 1,
        lanes: 1,
    });
    row.push(LinearOp::Select {
        dst: 70,
        cond: 63,
        if_true: 65,
        if_false: 66,
    });
    row.push(LinearOp::StoreOutput { src: 70 });
    let block = ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()]).unwrap();
    assert!(
        PreparedLazyRowPlan::new(
            block.program(0).unwrap(),
            block.program_register_count(0).unwrap()
        )
        .is_some(),
        "a checked unique-writer row retains demand scheduling"
    );
    let compute = rumoca_ir_solve::ComputeBlock::from_scalar_program_block(block.clone());
    let prepared = PreparedComputeBlock::new(&compute).unwrap();
    for (condition, expected) in [(1.0, 4.0), (0.0, 2.0), (1.0, 4.0)] {
        let mut reference = [f64::NAN];
        eval_scalar_program_block(&block, &[condition], &[], 0.0, None, &mut reference).unwrap();
        assert_eq!(reference, [expected]);
        let mut actual = [f64::NAN];
        prepared
            .eval_with_context(
                &[condition],
                &[],
                0.0,
                RowEvalContext::default(),
                &mut actual,
            )
            .unwrap();
        assert_eq!(actual, [expected]);
    }
}
