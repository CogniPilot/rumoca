use super::*;

struct RefusingTargetBackend;

impl SolveExecutionBackend for RefusingTargetBackend {
    fn compile_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Err("private target tuple is unsupported".into())
    }

    fn compile_jacobian_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        panic!("target preparation must not compile a Jacobian")
    }

    fn compile_assignment_schedule(
        &self,
        _: &solve::ComputeBlock,
        _: &solve::ContinuousRefreshOwners,
        _: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        panic!("target preparation must not compile a schedule")
    }

    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        panic!("target preparation must not compile a transaction")
    }
}

#[test]
fn issued_target_query_refusal_is_reported_during_preparation_with_its_source() {
    let block = spanned_block(
        vec![shifted_variable_residual_row(0, 1.0)],
        "unsupported_target.mo",
    );
    let span = block.program_span(0).unwrap();
    let source = PreparedScalarProgramBlock::new(block).unwrap();
    let error = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0)],
        &RefusingTargetBackend,
        RowEvalContext::default(),
    )
    .err()
    .expect("required query code must exist before a runnable query is issued");
    assert!(matches!(
        error,
        EvalSolveError::CompiledExecution { span: Some(owner), .. } if owner == span
    ));
}

#[test]
fn eager_target_query_preserves_tiny_values_without_mutating_solver_state() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(0, 1e-17)],
        "tiny_eager_target.mo",
    ))
    .unwrap();
    let backend = super::super::test_kernels::ReferenceBackend;
    let queries = super::super::compiled_targets::TargetQueries::prepare(
        &source,
        [(0, 0)],
        &backend,
        RowEvalContext::default(),
    )
    .unwrap();
    let y = [1e20];
    assert_eq!(
        queries
            .call((0, 0, 0), (&y, &[], 0.0), RowEvalContext::default())
            .unwrap(),
        Some(1e-17)
    );
    assert_eq!(y, [1e20]);
    assert_eq!(
        queries
            .call((0, 0, 1), (&y, &[], 0.0), RowEvalContext::default())
            .unwrap(),
        None
    );
}

#[test]
fn invalid_issued_target_row_is_a_preparation_error() {
    let source = PreparedScalarProgramBlock::new(spanned_block(
        vec![shifted_variable_residual_row(0, 1.0)],
        "bad_target_row.mo",
    ))
    .unwrap();
    let backend = super::super::test_kernels::ReferenceBackend;
    assert!(matches!(
        super::super::compiled_targets::TargetQueries::prepare(
            &source,
            [(2, 0)],
            &backend,
            RowEvalContext::default()
        ),
        Err(EvalSolveError::InvalidRow { .. })
    ));
}
