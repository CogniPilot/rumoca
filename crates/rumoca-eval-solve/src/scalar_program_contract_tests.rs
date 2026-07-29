use super::*;
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("ScalarProgramContract.mo"),
        18,
        31,
    )
}

#[test]
fn single_program_eval_rejects_a_missing_output_instead_of_returning_zero() {
    let row = vec![LinearOp::Const { dst: 0, value: 9.0 }];

    let error =
        eval_row(&row, &[], &[], 0.0, None).expect_err("a row without an output is not computable");

    assert!(matches!(
        error,
        EvalSolveError::InvalidRow { message, span: None }
            if message.contains("expected 1 outputs, found 0")
    ));
}

#[test]
fn single_program_eval_rejects_ambiguous_multiple_outputs() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::Const { dst: 1, value: 7.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];

    let error = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("the single-output API must not choose one of several values");

    assert!(matches!(
        error,
        EvalSolveError::InvalidRow { message, span: None }
            if message.contains("expected 1 outputs, found 2")
    ));
}

#[test]
fn prepared_construction_rejects_missing_output_with_source_span() {
    let source_span = span();
    let malformed = ScalarProgramBlock {
        programs: vec![vec![LinearOp::Const { dst: 0, value: 9.0 }]],
        program_spans: vec![source_span],
        output_indices: vec![],
    };

    let error = match PreparedScalarProgramBlock::new(malformed) {
        Ok(_) => panic!("prepared evaluation must reject malformed public IR immediately"),
        Err(error) => error,
    };

    assert_eq!(error.source_span(), Some(source_span));
    assert!(matches!(
        error,
        EvalSolveError::ShapeContract { message, .. }
            if message.contains("scalar program 0 stores no output")
    ));
}

#[test]
fn prepared_construction_rejects_bypassed_undefined_register_with_source_span() {
    let source_span = span();
    let malformed = ScalarProgramBlock {
        programs: vec![vec![
            LinearOp::Move { dst: 0, src: 4 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        program_spans: vec![source_span],
        output_indices: vec![0],
    };

    let error = match PreparedScalarProgramBlock::new(malformed) {
        Ok(_) => panic!("prepared evaluation must reject undefined register reads immediately"),
        Err(error) => error,
    };

    assert_eq!(error.source_span(), Some(source_span));
    assert!(matches!(
        error,
        EvalSolveError::ShapeContract { message, .. }
            if message.contains("Move op 0 reads undefined register r4")
    ));
}

#[test]
fn single_program_eval_returns_its_one_explicit_output() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 9.0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    assert_eq!(
        eval_row(&row, &[], &[], 0.0, None).expect("one explicit output is computable"),
        9.0
    );
}
