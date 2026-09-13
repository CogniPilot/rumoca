use super::*;
use rumoca_ir_solve::BinaryOp;

#[test]
fn grouped_native_jvp_returns_all_outputs_in_one_invocation() {
    let compiled = compile_jacobian_scalar_program_block(&aggregate_and_table()).unwrap();
    let mut out = vec![99.0; 12];
    for (i, (y, p, t, seed, expected)) in [
        (3.0, 2.0, 0.5, [4.0, -2.0], [14.0, -1.0]),
        (-1.0, 8.0, 2.0, [2.0, 3.0], [6.0, 6.0]),
    ]
    .into_iter()
    .enumerate()
    {
        compiled
            .call_program_outputs(
                0,
                rumoca_eval_solve::JacobianEvalInputs {
                    y: &[y],
                    p: &[p],
                    t,
                    seed: &seed,
                },
                &[],
                &mut out,
            )
            .unwrap();
        assert_eq!(out, expected);
        assert_eq!(compiled.jit.program_call_count(), i + 1);
    }
}

#[test]
fn grouped_native_jvp_checks_extents_and_external_tables() {
    let compiled = compile_jacobian_scalar_program_block(&aggregate_and_table()).unwrap();
    let mut out = Vec::new();
    let inputs = rumoca_eval_solve::JacobianEvalInputs {
        y: &[3.0],
        p: &[2.0],
        t: 0.5,
        seed: &[4.0, -2.0],
    };
    assert!(
        compiled
            .call_program_outputs(2, inputs, &[], &mut out)
            .is_err()
    );
    for invalid in [
        rumoca_eval_solve::JacobianEvalInputs { y: &[], ..inputs },
        rumoca_eval_solve::JacobianEvalInputs { p: &[], ..inputs },
        rumoca_eval_solve::JacobianEvalInputs {
            seed: &[4.0],
            ..inputs
        },
    ] {
        assert!(
            compiled
                .call_program_outputs(0, invalid, &[], &mut out)
                .is_err()
        );
    }
    assert_eq!(compiled.jit.program_call_count(), 0);
    assert!(
        compiled
            .call_program_outputs(1, inputs, &[], &mut out)
            .is_err()
    );
    let tables = [ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    compiled
        .call_program_outputs(1, inputs, &tables, &mut out)
        .unwrap();
    assert_eq!(out, [12.0]);
}

fn aggregate_and_table() -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadSeed { dst: 1, index: 0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::LoadP { dst: 3, index: 0 },
                LinearOp::LoadTime { dst: 4 },
                LinearOp::LoadSeed { dst: 5, index: 1 },
                LinearOp::Binary {
                    dst: 6,
                    op: BinaryOp::Add,
                    lhs: 2,
                    rhs: 3,
                },
                LinearOp::Binary {
                    dst: 7,
                    op: BinaryOp::Mul,
                    lhs: 4,
                    rhs: 5,
                },
                LinearOp::StoreOutputRange {
                    start: 6,
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
fn selected_native_jvp_preserves_aggregate_coordinates_and_skips_other_programs() {
    let compiled = compile_jacobian_scalar_program_block(&aggregate_and_table()).unwrap();
    for (y, p, time, seed, expected) in [
        (3.0, 2.0, 0.5, [4.0, -2.0], [14.0, -1.0]),
        (-1.0, 8.0, 2.0, [2.0, 3.0], [6.0, 6.0]),
    ] {
        for (offset, value) in expected.into_iter().enumerate() {
            assert_eq!(
                compiled
                    .call_program_output((0, offset), &[y], &[p], time, &seed, &[])
                    .unwrap(),
                value,
            );
        }
        let mut all = [-1.0; 12];
        assert!(
            compiled.call(&[y], &[p], time, &seed, &mut all).is_err(),
            "the unrelated table program must fail without its external table"
        );
    }
}

#[test]
fn selected_native_jvp_preserves_external_tables_and_sparse_full_output_placement() {
    let compiled = compile_jacobian_scalar_program_block(&aggregate_and_table()).unwrap();
    let tables = [ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
    assert!(
        compiled
            .call_program_output((1, 0), &[3.0], &[2.0], 0.5, &[4.0, -2.0], &[])
            .is_err()
    );
    assert_eq!(
        compiled
            .call_program_output((1, 0), &[3.0], &[2.0], 0.5, &[4.0, -2.0], &tables)
            .unwrap(),
        12.0
    );
    let mut all = [-99.0; 12];
    compiled
        .call_with_external_tables(&[3.0], &[2.0], 0.5, &[4.0, -2.0], &tables, &mut all)
        .unwrap();
    let mut expected = [-99.0; 12];
    expected[7] = 14.0;
    expected[3] = -1.0;
    expected[11] = 12.0;
    assert_eq!(all, expected);
}

#[test]
fn selected_native_jvp_checks_coordinates_and_input_extents_before_execution() {
    let compiled = compile_jacobian_scalar_program_block(&aggregate_and_table()).unwrap();
    for coordinate in [(2, 0), (0, 2), (usize::MAX, 0), (0, usize::MAX)] {
        assert!(
            compiled
                .call_program_output(coordinate, &[3.0], &[2.0], 0.5, &[4.0, -2.0], &[])
                .is_err()
        );
    }
    assert!(
        compiled
            .call_program_output((0, 0), &[], &[2.0], 0.5, &[4.0, -2.0], &[])
            .is_err()
    );
    assert!(
        compiled
            .call_program_output((0, 0), &[3.0], &[], 0.5, &[4.0, -2.0], &[])
            .is_err()
    );
    assert!(
        compiled
            .call_program_output((0, 0), &[3.0], &[2.0], 0.5, &[4.0], &[])
            .is_err()
    );
}
