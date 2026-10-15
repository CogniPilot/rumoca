use super::*;
use std::cell::Cell;

struct Backend {
    calls: Rc<Cell<usize>>,
    fail: Rc<Cell<bool>>,
}

struct Expression {
    inner: Rc<dyn CompiledSolveExpression>,
    calls: Rc<Cell<usize>>,
    fail: Rc<Cell<bool>>,
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
        self.calls.set(self.calls.get() + 1);
        if self.fail.get() {
            return Err(RuntimeSolveError::solve_ir("refresh compiled failure"));
        }
        self.inner.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(Expression {
            inner: test_kernels::expression(block)?,
            calls: self.calls.clone(),
            fail: self.fail.clone(),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        test_kernels::jacobian(block)
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        test_kernels::assignment(source, owners, schedule)
    }
}

fn prepared_runtime() -> (SolveRuntime, Rc<Cell<usize>>, Rc<Cell<bool>>) {
    let model = super::super::tests::warm_start_test_model();
    let calls = Rc::new(Cell::new(0));
    let fail = Rc::new(Cell::new(false));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            calls: calls.clone(),
            fail: fail.clone(),
        }),
    )
    .unwrap();
    (runtime, calls, fail)
}

#[test]
fn refresh_dispatch_residual_uses_prepared_kernel() {
    let (runtime, calls, _) = prepared_runtime();
    let scalar = runtime.scalar_execution().unwrap();
    let row = scalar.scalar.algebraic_refresh.rows.first().unwrap();
    let reference = SolveRuntime::new_fixture(&runtime.model).unwrap();
    let expected = reference
        .scalar_execution()
        .unwrap()
        .refresh_row_residual(row, 0.0, &[1.0, 2.0], &[])
        .unwrap();
    let value = scalar
        .refresh_row_residual(row, 0.0, &[1.0, 2.0], &[])
        .unwrap();
    assert_eq!(value, expected);
    assert_eq!(calls.get(), 1);
}

#[test]
fn refresh_dispatch_assignment_uses_prepared_kernel() {
    let (runtime, calls, _) = prepared_runtime();
    let scalar = runtime.scalar_execution().unwrap();
    let row = scalar.scalar.algebraic_refresh.rows.first().unwrap();
    let value = scalar.eval_refresh_row(row, 0.0, &[1.0, 2.0], &[]).unwrap();
    assert_eq!(value, 0.0);
    assert!(calls.get() > 0, "assignment must execute compiled code");
}

#[test]
fn refresh_dispatch_compiled_failure_is_not_interpreted() {
    let (runtime, calls, fail) = prepared_runtime();
    let scalar = runtime.scalar_execution().unwrap();
    let row = scalar.scalar.algebraic_refresh.rows.first().unwrap();
    fail.set(true);
    let error = scalar
        .eval_refresh_row(row, 0.0, &[1.0, 2.0], &[])
        .expect_err("compiled failure must propagate");
    assert!(error.to_string().contains("refresh compiled failure"));
    assert_eq!(calls.get(), 1);
}
