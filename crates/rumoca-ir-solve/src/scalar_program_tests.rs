use super::*;

fn source_span(source: &'static str, start: usize, end: usize) -> Span {
    Span::from_offsets(SourceId::from_source_name(source), start, end)
}

#[test]
fn scalar_program_construction_rejects_missing_output_at_its_source() {
    let span = source_span("MissingOutput.mo", 23, 34);
    let programs = vec![vec![LinearOp::Const { dst: 0, value: 4.0 }]];

    let error = ScalarProgramBlock::with_program_spans(programs, vec![span])
        .expect_err("a complete scalar program must produce a value");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ScalarProgramMissingOutput {
            node_index: 0,
            program_index: 0,
            ..
        }
    ));
}

#[test]
fn scalar_program_construction_accepts_one_program_with_several_outputs() {
    let span = source_span("VectorExpression.mo", 12, 27);
    let program = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];

    let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span])
        .expect("a tensor scalar fallback may store several explicit outputs");

    assert_eq!(block.row_count(), 1);
    assert_eq!(block.stored_output_count(), 2);
    assert_eq!(block.program_span(0), Some(span));
}
