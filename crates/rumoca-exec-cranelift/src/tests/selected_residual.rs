use super::*;
use rumoca_ir_solve::BinaryOp;

#[test]
fn selected_residual_does_not_split_a_shared_conditional_owner() {
    use rumoca_ir_solve::{FunctionConditionalOwnerId, FunctionConditionalProgram};
    let owner = std::sync::Arc::new(
        FunctionConditionalProgram::checked_owned(
            FunctionConditionalOwnerId::checked(1).unwrap(),
            0,
            [1, 1],
            [(
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::Const {
                        dst: 0,
                        value: 11.0,
                    },
                    LinearOp::Const {
                        dst: 1,
                        value: 19.0,
                    },
                    LinearOp::StoreOutputRange {
                        start: 0,
                        count: 2,
                        stride: 1,
                    },
                ],
            )],
            vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::Const { dst: 1, value: 7.0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
        )
        .unwrap(),
    );
    let rows = (0..2)
        .map(|offset| {
            vec![
                LinearOp::FunctionConditional {
                    dst_start: 0,
                    capture_start: 0,
                    program: owner.clone(),
                },
                LinearOp::StoreOutput { src: offset },
            ]
        })
        .collect();
    let block = ScalarProgramBlock::with_program_spans(rows, vec![fixture_span(); 2]).unwrap();
    let compiled = compile_selectable_expression_scalar_program_block(&block, None).unwrap();
    assert_eq!(
        compiled
            .call_program_output((0, 0), &[], &[], 0.0, &[])
            .unwrap(),
        None
    );
    let mut output = [0.0; 2];
    compiled.call(&[], &[], 0.0, &mut output).unwrap();
    assert_eq!(output, [3.0, 7.0]);
}

fn aggregate_and_table() -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::LoadTime { dst: 2 },
                LinearOp::Binary {
                    dst: 3,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 2,
                },
                LinearOp::StoreOutputRange {
                    start: 3,
                    count: 2,
                    stride: 1,
                },
            ],
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: 42.0,
                },
                LinearOp::Const { dst: 1, value: 1.0 },
                LinearOp::Const { dst: 2, value: 1.0 },
                LinearOp::TableLookup {
                    dst: 3,
                    table_id: 0,
                    column: 1,
                    input: 2,
                },
                LinearOp::StoreOutput { src: 3 },
            ],
        ],
        vec![fixture_span(); 2],
        vec![7, 3, 11],
    )
    .unwrap()
}

#[test]
fn selected_native_residual_skips_unrelated_programs_and_preserves_coordinates() {
    let block = aggregate_and_table();
    let compiled = compile_selectable_expression_scalar_program_block(&block, None).unwrap();
    for (y, p, t, expected) in [(3.0, 2.0, 0.5, [6.0, 3.5]), (-1.0, 8.0, 2.0, [-8.0, 1.0])] {
        for (offset, value) in expected.into_iter().enumerate() {
            assert_eq!(
                compiled
                    .call_program_output((0, offset), &[y], &[p], t, &[])
                    .unwrap(),
                Some(value)
            );
        }
        assert!(
            compiled.call(&[y], &[p], t, &mut [0.0; 12]).is_err(),
            "the unrelated table program must fail without its table"
        );
    }
    let batched = compile_expression_scalar_program_block(&block).unwrap();
    assert_eq!(
        batched
            .call_program_output((0, 0), &[3.0], &[2.0], 0.5, &[])
            .unwrap(),
        None
    );
}

#[test]
fn selected_native_residual_returns_the_complete_program_once_and_prevalidates() {
    let block = aggregate_and_table();
    let compiled = compile_selectable_expression_scalar_program_block(&block, None).unwrap();
    let mut out = vec![99.0];
    for (run, (y, p, t, expected)) in [(3.0, 2.0, 0.5, [6.0, 3.5]), (-1.0, 8.0, 2.0, [-8.0, 1.0])]
        .into_iter()
        .enumerate()
    {
        assert!(
            compiled
                .call_program_outputs(0, &[y], &[p], t, &[], &mut out)
                .unwrap()
        );
        assert_eq!(out, expected);
        assert_eq!(compiled.jit.jit_call_count(), run + 1);
    }
    let preserved = out.clone();
    for (program, y, p) in [
        (2, vec![3.0], vec![2.0]),
        (0, vec![], vec![2.0]),
        (0, vec![3.0], vec![]),
    ] {
        assert!(
            compiled
                .call_program_outputs(program, &y, &p, 0.0, &[], &mut out)
                .is_err()
        );
        assert_eq!(out, preserved);
        assert_eq!(compiled.jit.jit_call_count(), 2);
    }
    assert!(
        compiled
            .call_program_outputs(1, &[3.0], &[2.0], 0.0, &[], &mut out)
            .is_err(),
        "an admitted table failure cannot become a decline"
    );
    let batch = compile_expression_scalar_program_block(&block).unwrap();
    let mut untouched = vec![99.0];
    assert!(
        !batch
            .call_program_outputs(0, &[3.0], &[2.0], 0.0, &[], &mut untouched)
            .unwrap()
    );
    assert_eq!(untouched, [99.0]);
}

#[test]
fn selected_native_residual_preserves_tables_and_full_sparse_output_placement() {
    let compiled =
        compile_selectable_expression_scalar_program_block(&aggregate_and_table(), None).unwrap();
    let tables = [ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    assert!(
        compiled
            .call_program_output((1, 0), &[], &[], 0.5, &[])
            .is_err()
    );
    assert_eq!(
        compiled
            .call_program_output((1, 0), &[], &[], 0.5, &tables)
            .unwrap(),
        Some(12.0)
    );
    let mut actual = [-99.0; 12];
    compiled
        .call_with_external_tables(&[3.0], &[2.0], 0.5, &tables, &mut actual)
        .unwrap();
    let mut expected = [-99.0; 12];
    expected[7] = 6.0;
    expected[3] = 3.5;
    expected[11] = 12.0;
    assert_eq!(actual, expected);
}

#[test]
fn selected_native_residual_checks_program_output_and_input_extents() {
    let compiled =
        compile_selectable_expression_scalar_program_block(&aggregate_and_table(), None).unwrap();
    for coordinate in [(2, 0), (0, 2), (usize::MAX, 0), (0, usize::MAX)] {
        assert!(
            compiled
                .call_program_output(coordinate, &[3.0], &[2.0], 0.5, &[])
                .is_err()
        );
    }
    assert!(
        compiled
            .call_program_output((0, 0), &[], &[2.0], 0.5, &[])
            .is_err()
    );
    assert!(
        compiled
            .call_program_output((0, 0), &[3.0], &[], 0.5, &[])
            .is_err()
    );
}
