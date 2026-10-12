//! Before probes at the ordinary checked construction/prepared execution boundary.

use super::*;
use rumoca_ir_solve::{TensorConcatenateSource, TensorUpdateSubscript};

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

fn assert_disjoint_snapshot_lazy(values: &[f64], operation: LinearOp, expected: &[f64]) {
    let destination = operation.dst_register().unwrap();
    assert_eq!(destination, 64);
    let mut initialized = values.to_vec();
    assert!(
        initialized.len() <= 63,
        "operands precede the lazy selection lane"
    );
    initialized.resize(63, 0.0);
    let mut row = initialized
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: Reg::try_from(dst).unwrap(),
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::LoadY { dst: 63, index: 0 });
    row.push(operation);
    let selected = destination + Reg::try_from(expected.len()).unwrap();
    row.push(LinearOp::Select {
        dst: selected,
        cond: 63,
        if_true: destination,
        if_false: selected - 1,
    });
    row.push(LinearOp::StoreOutput { src: selected });
    let block = ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()])
        .expect("initialized disjoint aggregate has a checked owner");
    assert!(
        PreparedLazyRowPlan::new(
            block.program(0).unwrap(),
            block.program_register_count(0).unwrap()
        )
        .is_some(),
        "unique-writer aggregate retains lazy admission"
    );
    let compute = rumoca_ir_solve::ComputeBlock::from_scalar_program_block(block.clone());
    let prepared = PreparedComputeBlock::new(&compute).unwrap();
    for (condition, expected) in [
        (1.0, expected[0]),
        (0.0, *expected.last().unwrap()),
        (1.0, expected[0]),
    ] {
        let mut reference = [f64::NAN];
        eval_scalar_program_block(&block, &[condition], &[], 0.0, None, &mut reference).unwrap();
        assert_eq!(
            reference,
            [expected],
            "checked selected aggregate reference"
        );
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
        assert_eq!(
            actual,
            [expected],
            "fresh and cached lazy aggregate execution"
        );
    }
}

#[test]
fn prepared_tensor_additional_alias_concatenate_keeps_later_source_values() {
    let operation = |dst_start| LinearOp::TensorConcatenate {
        dst_start,
        sources: vec![
            TensorConcatenateSource {
                start: 2,
                dimensions: vec![2].into(),
            },
            TensorConcatenateSource {
                start: 0,
                dimensions: vec![2].into(),
            },
        ]
        .into(),
        dimensions: vec![4].into(),
        axis: 0,
        lanes: 1,
    };
    let expected = [3., 4., 1., 2.];
    assert_prepared_snapshot(&[1., 2., 3., 4., 0., 0., 0., 0.], operation(4), &expected);
    assert_disjoint_snapshot_lazy(&[1., 2., 3., 4.], operation(64), &expected);
    assert_prepared_snapshot(&[1., 2., 3., 4.], operation(0), &expected);
}

#[test]
fn prepared_tensor_additional_alias_update_keeps_replacement_values() {
    let operation = |dst_start| LinearOp::TensorUpdate {
        dst_start,
        base_start: 0,
        value_start: 0,
        dimensions: vec![3].into(),
        subscripts: vec![TensorUpdateSubscript::Slice {
            start: 3,
            dimensions: vec![2].into(),
        }]
        .into(),
        lanes: 1,
    };
    let expected = [1., 1., 2.];
    assert_prepared_snapshot(&[1., 2., 3., 2., 3., 0., 0., 0.], operation(5), &expected);
    assert_disjoint_snapshot_lazy(&[1., 2., 3., 2., 3.], operation(64), &expected);
    assert_prepared_snapshot(&[1., 2., 3., 2., 3.], operation(0), &expected);
}

#[test]
fn prepared_tensor_additional_alias_fill_keeps_packed_lane_values() {
    let operation = |dst_start| LinearOp::TensorFill {
        dst_start,
        value_start: 1,
        count: 2,
        lanes: 2,
    };
    let expected = [1., 2., 1., 2.];
    assert_prepared_snapshot(&[0., 1., 2., 0., 0., 0., 0., 0.], operation(4), &expected);
    assert_disjoint_snapshot_lazy(&[0., 1., 2., 0.], operation(64), &expected);
    assert_prepared_snapshot(&[0., 1., 2., 0.], operation(0), &expected);
}

#[test]
fn prepared_tensor_snapshot_update_keeps_runtime_index_before_destination_writes() {
    let operation = |dst_start| LinearOp::TensorUpdate {
        dst_start,
        base_start: 0,
        value_start: 3,
        dimensions: vec![3].into(),
        subscripts: vec![TensorUpdateSubscript::Index(
            rumoca_ir_solve::TensorIndex::Runtime(4),
        )]
        .into(),
        lanes: 1,
    };
    let values = [10., 20., 30., 99., 2., 0., 0., 0., 0., 0.];
    let expected = [10., 99., 30.];
    assert_prepared_snapshot(&values, operation(7), &expected);
    assert_disjoint_snapshot_lazy(&values, operation(64), &expected);
    assert_prepared_snapshot(&values, operation(4), &expected);
}

#[test]
fn prepared_tensor_snapshot_update_keeps_slice_coordinates_before_destination_writes() {
    let operation = |dst_start| LinearOp::TensorUpdate {
        dst_start,
        base_start: 0,
        value_start: 6,
        dimensions: vec![3].into(),
        subscripts: vec![TensorUpdateSubscript::Slice {
            start: 3,
            dimensions: vec![2].into(),
        }]
        .into(),
        lanes: 1,
    };
    let values = [1., 2., 3., 2., 3., 0., 7., 8., 0., 0., 0.];
    let expected = [1., 7., 8.];
    assert_prepared_snapshot(&values, operation(8), &expected);
    assert_disjoint_snapshot_lazy(&values, operation(64), &expected);
    assert_prepared_snapshot(&values, operation(3), &expected);
}

#[test]
fn prepared_tensor_snapshot_update_keeps_shape_projected_replacement_values() {
    let operation = |dst_start| LinearOp::TensorUpdate {
        dst_start,
        base_start: 0,
        value_start: 4,
        dimensions: vec![2, 2].into(),
        subscripts: vec![
            TensorUpdateSubscript::Whole,
            TensorUpdateSubscript::Index(rumoca_ir_solve::TensorIndex::Constant(1)),
        ]
        .into(),
        lanes: 1,
    };
    let values = [1., 2., 3., 4., 9., 8., 0., 0., 0., 0., 0., 0.];
    let expected = [1., 9., 3., 8.];
    assert_prepared_snapshot(&values, operation(8), &expected);
    assert_disjoint_snapshot_lazy(&values, operation(64), &expected);
    assert_prepared_snapshot(&values, operation(4), &expected);
}
