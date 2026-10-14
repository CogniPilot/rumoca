use super::*;

fn native_table_transaction_source() -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_table_transaction.mo"),
        1,
        2,
    );
    ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 4.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
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
        vec![span, span],
    )
    .unwrap()
}

fn native_table_transaction_data() -> [ExternalTableData; 1] {
    [ExternalTableData {
        id: 42,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }]
}

#[test]
fn native_table_later_program_fault_preserves_whole_output_transaction() {
    let source = native_table_transaction_source();
    let native = compile_expression_scalar_program_block(&source).unwrap();
    let tables = native_table_transaction_data();
    let mut output = [99.0, 101.0];
    let error = native
        .call_with_external_tables(&[], &[43.0], 0.0, &tables, &mut output)
        .unwrap_err();
    assert!(
        matches!(error, CompileError::SourceOperation {
        kind: NativeSourceFault::ExternalTable, ref message, ..
    } if message.contains("table id 43")),
        "{error}"
    );
    assert_eq!(output, [99.0, 101.0]);
    native
        .call_with_external_tables(&[], &[42.0], 0.0, &tables, &mut output)
        .unwrap();
    assert_eq!(output, [4.0, 12.0]);
}

#[test]
fn native_table_fault_preserves_directional_whole_and_selected_bulk_outputs() {
    let source = native_table_transaction_source();
    let native = compile_selectable_expression_scalar_program_block(&source, None).unwrap();
    let directional = compile_jacobian_scalar_program_block(&source).unwrap();
    let tables = native_table_transaction_data();
    let mut output = [99.0, 101.0];
    let error = directional
        .call_with_external_tables(&[], &[43.0], 0.0, &[], &tables, &mut output)
        .unwrap_err();
    assert!(
        matches!(
            error,
            CompileError::SourceOperation {
                kind: NativeSourceFault::ExternalTable,
                ..
            }
        ),
        "{error}"
    );
    assert_eq!(output, [99.0, 101.0]);
    directional
        .call_with_external_tables(&[], &[42.0], 0.0, &[], &tables, &mut output)
        .unwrap();
    assert_eq!(output, [4.0, 12.0]);
    for coordinates in [[(0, 0), (1, 0)], [(1, 0), (0, 0)]] {
        output = [99.0, 101.0];
        let error = native
            .call_program_outputs_at(&coordinates, (&[], &[43.0], 0.0), &tables, &mut output)
            .unwrap_err();
        assert!(
            matches!(
                error,
                CompileError::SourceOperation {
                    kind: NativeSourceFault::ExternalTable,
                    ..
                }
            ),
            "{error}"
        );
        assert_eq!(output, [99.0, 101.0]);
        assert!(
            native
                .call_program_outputs_at(&coordinates, (&[], &[42.0], 0.0), &tables, &mut output)
                .unwrap()
        );
        let expected = if coordinates[0].0 == 0 {
            [4.0, 12.0]
        } else {
            [12.0, 4.0]
        };
        assert_eq!(output, expected);

        output = [99.0, 101.0];
        let error = directional
            .call_program_outputs_at(
                &coordinates,
                rumoca_eval_solve::JacobianEvalInputs {
                    y: &[],
                    p: &[43.0],
                    t: 0.0,
                    seed: &[],
                },
                &tables,
                &mut output,
            )
            .unwrap_err();
        assert!(
            matches!(
                error,
                CompileError::SourceOperation {
                    kind: NativeSourceFault::ExternalTable,
                    ..
                }
            ),
            "{error}"
        );
        assert_eq!(output, [99.0, 101.0]);
        directional
            .call_program_outputs_at(
                &coordinates,
                rumoca_eval_solve::JacobianEvalInputs {
                    y: &[],
                    p: &[42.0],
                    t: 0.0,
                    seed: &[],
                },
                &tables,
                &mut output,
            )
            .unwrap();
        assert_eq!(output, expected);
    }
}

#[test]
fn native_table_fault_preserves_selected_vec_extent_before_successful_reuse() {
    let source = native_table_transaction_source();
    let native = compile_selectable_expression_scalar_program_block(&source, None).unwrap();
    let directional = compile_jacobian_scalar_program_block(&source).unwrap();
    let tables = native_table_transaction_data();
    let mut output = vec![99.0, 101.0];
    let error = native
        .call_program_outputs(1, &[], &[43.0], 0.0, &tables, &mut output)
        .unwrap_err();
    assert!(
        matches!(
            error,
            CompileError::SourceOperation {
                kind: NativeSourceFault::ExternalTable,
                ..
            }
        ),
        "{error}"
    );
    assert_eq!(output, [99.0, 101.0]);
    assert!(
        native
            .call_program_outputs(1, &[], &[42.0], 0.0, &tables, &mut output)
            .unwrap()
    );
    assert_eq!(output, [12.0]);
    output = vec![99.0, 101.0];
    let error = directional
        .call_program_outputs(
            1,
            rumoca_eval_solve::JacobianEvalInputs {
                y: &[],
                p: &[43.0],
                t: 0.0,
                seed: &[],
            },
            &tables,
            &mut output,
        )
        .unwrap_err();
    assert!(
        matches!(
            error,
            CompileError::SourceOperation {
                kind: NativeSourceFault::ExternalTable,
                ..
            }
        ),
        "{error}"
    );
    assert_eq!(output, [99.0, 101.0]);
    directional
        .call_program_outputs(
            1,
            rumoca_eval_solve::JacobianEvalInputs {
                y: &[],
                p: &[42.0],
                t: 0.0,
                seed: &[],
            },
            &tables,
            &mut output,
        )
        .unwrap();
    assert_eq!(output, [12.0]);
}
