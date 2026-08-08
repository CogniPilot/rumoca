use super::*;

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
