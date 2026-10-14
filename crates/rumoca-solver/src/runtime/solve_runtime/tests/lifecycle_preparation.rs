use super::*;

struct Backend {
    refused: rumoca_core::Span,
    executions: Rc<std::cell::Cell<usize>>,
}

struct Expression {
    inner: Rc<dyn CompiledSolveExpression>,
    executions: Rc<std::cell::Cell<usize>>,
}

impl CompiledSolveExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.executions.set(self.executions.get() + 1);
        self.inner.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        if block.program_spans().contains(&self.refused) {
            return Err("unsupported cold lifecycle program".into());
        }
        Ok(Rc::new(Expression {
            inner: super::super::test_kernels::expression(block)?,
            executions: self.executions.clone(),
        }))
    }
    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        super::super::test_kernels::jacobian(block)
    }
    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }
    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("fixture has no transaction".into())
    }
}

#[test]
fn unsupported_lifecycle_program_fails_before_any_execution_with_source_span() {
    let mut model = warm_start_test_model();
    let span = test_span("cold_assignment.mo");
    model.problem.discrete.runtime_assignment_rhs = spanned_block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 3.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "cold_assignment.mo",
    );
    model.problem.discrete.runtime_assignment_targets = vec![solve::scalar_slot_y(1)];
    let executions = Rc::new(std::cell::Cell::new(0));
    let backend = Rc::new(Backend {
        refused: span,
        executions: executions.clone(),
    });
    let error = SolveRuntime::new_fixture_with_backend(&model, backend)
        .err()
        .expect("the entire runtime must reject unsupported lifecycle code");
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(issued), ref message } if issued == span && message.contains("runtime_assignments[0]")),
        "{error}"
    );
    assert_eq!(
        executions.get(),
        0,
        "preparation must not execute source code"
    );
    let reference =
        SolveRuntime::new_fixture(&model).expect("explicit reference execution stays available");
    let mut y = model.initial_y.to_vec();
    reference
        .apply_runtime_assignments_until_stable(&mut y, &mut [], 0.0, 1e-10, 4)
        .unwrap();
    assert_eq!(y[1], 3.0);
}

#[test]
fn lifecycle_inventory_rejects_a_foreign_prepared_source() {
    let model = warm_start_test_model();
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let scalar = runtime.scalar_execution().unwrap();
    let foreign = scalar.scalar.discrete_rhs.clone();
    assert!(scalar.scalar.lifecycle_programs(&foreign).is_err());
    assert!(
        scalar
            .scalar
            .lifecycle_programs(&scalar.scalar.discrete_rhs)
            .unwrap()
            .is_some()
    );
}
