//! Count calls without changing the production backend's capability selection.
use rumoca_core::ExternalTableData as Table;
use rumoca_eval_solve as eval;
use rumoca_ir_solve as solve;
use rumoca_solver::*;
use std::{cell::Cell, rc::Rc};

#[derive(Default)]
pub(super) struct Calls {
    pub compiles: Cell<usize>,
    pub projection_calls: Cell<usize>,
    pub succeeded: Cell<usize>,
    pub failed: Cell<usize>,
}
impl Calls {
    fn prepare(&self) {
        self.compiles.set(self.compiles.get() + 1);
    }
    fn observe<T>(&self, result: &Result<T, RuntimeSolveError>, executed: bool) {
        if result.is_err() {
            self.failed.set(self.failed.get() + 1);
        } else if executed {
            self.succeeded.set(self.succeeded.get() + 1);
        }
    }
}
struct Backend {
    inner: Rc<dyn SolveExecutionBackend>,
    calls: Rc<Calls>,
}
struct Jacobian {
    inner: Rc<dyn CompiledSolveJacobianExpression>,
    calls: Rc<Calls>,
}
struct Projection {
    inner: Rc<dyn CompiledSolveProjectionJacobian>,
    calls: Rc<Calls>,
}
impl Projection {
    fn wrap(
        inner: Rc<dyn CompiledSolveProjectionJacobian>,
        calls: &Rc<Calls>,
    ) -> Rc<dyn CompiledSolveProjectionJacobian> {
        Rc::new(Self {
            inner,
            calls: calls.clone(),
        })
    }
}
impl CompiledSolveProjectionJacobian for Projection {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[Table],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.calls
            .projection_calls
            .set(self.calls.projection_calls.get() + 1);
        let r = self.inner.call(y, p, t, tables, out);
        self.calls.observe(&r, true);
        r
    }
}
impl Jacobian {
    fn wrap(
        inner: Rc<dyn CompiledSolveJacobianExpression>,
        calls: &Rc<Calls>,
    ) -> Rc<dyn CompiledSolveJacobianExpression> {
        Rc::new(Self {
            inner,
            calls: calls.clone(),
        })
    }
}
impl CompiledSolveJacobianExpression for Jacobian {
    fn bind_program(
        &self,
        source: &solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        self.inner
            .bind_program(source, program)
            .map(|v| v.map(|inner| Self::wrap(inner, &self.calls)))
    }
    fn prepare_projections(
        &self,
        apps: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        let values = self.inner.prepare_projections(apps)?;
        Ok(values
            .into_iter()
            .map(|value| value.map(|inner| Projection::wrap(inner, &self.calls)))
            .collect())
    }
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[Table],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let r = self.inner.call(y, p, t, seed, tables, out);
        self.calls.observe(&r, true);
        r
    }
    fn call_program_output(
        &self,
        at: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[Table],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let r = self.inner.call_program_output(at, y, p, t, seed, tables);
        self.calls.observe(&r, matches!(r, Ok(Some(_))));
        r
    }
    fn call_program_outputs(
        &self,
        program: usize,
        inputs: eval::JacobianEvalInputs<'_>,
        tables: &[Table],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        let r = self
            .inner
            .call_program_outputs(program, inputs, tables, out);
        self.calls.observe(&r, matches!(r, Ok(true)));
        r
    }
    fn call_program_outputs_at(
        &self,
        at: &[(usize, usize)],
        inputs: eval::JacobianEvalInputs<'_>,
        tables: &[Table],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let r = self.inner.call_program_outputs_at(at, inputs, tables, out);
        self.calls.observe(&r, matches!(r, Ok(true)));
        r
    }
}
impl SolveExecutionBackend for Backend {
    fn validate_model_context(&self, model: &solve::SolveModel) -> Result<(), String> {
        self.inner.validate_model_context(model)
    }
    fn pure_call_execution(&self) -> Option<&dyn eval::PureCallExecution> {
        self.inner.pure_call_execution()
    }
    fn compile_target_values(
        &self,
        plan: &eval::PreparedTargetValuePlan,
        context: eval::RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        self.calls.prepare();
        self.inner.compile_target_values(plan, context)
    }
    fn compile_event_observation(
        &self,
        program: solve::RetainedEventProgram,
    ) -> Result<Rc<dyn CompiledSolveEventObservation>, String> {
        self.calls.prepare();
        self.inner.compile_event_observation(program)
    }
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.calls.prepare();
        self.inner.compile_expression(block)
    }
    fn compile_selectable_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.calls.prepare();
        self.inner.compile_selectable_expression(block)
    }
    fn compile_compute_expression(
        &self,
        block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        self.calls.prepare();
        self.inner.compile_compute_expression(block)
    }
    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        self.calls.prepare();
        self.inner
            .compile_jacobian_expression(block)
            .map(|inner| Jacobian::wrap(inner, &self.calls))
    }
    fn compile_compute_jacobian_expression(
        &self,
        block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        self.calls.prepare();
        self.inner
            .compile_compute_jacobian_expression(block)
            .map(|v| v.map(|inner| Jacobian::wrap(inner, &self.calls)))
    }
    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        self.calls.prepare();
        self.inner
            .compile_assignment_schedule(source, owners, schedule)
    }
    fn compile_torn_assignment_rows(
        &self,
        rows: &[Vec<solve::LinearOp>],
        targets: &[usize],
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        self.calls.prepare();
        self.inner.compile_torn_assignment_rows(rows, targets)
    }
    fn compile_event_transaction(
        &self,
        program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        self.calls.prepare();
        self.inner.compile_event_transaction(program)
    }
}
pub(super) fn counted(model: &solve::SolveModel) -> (Rc<dyn SolveExecutionBackend>, Rc<Calls>) {
    let calls = Rc::new(Calls::default());
    let inner = crate::native_execution::backend(&model.pure_calls).unwrap();
    (
        Rc::new(Backend {
            inner,
            calls: calls.clone(),
        }),
        calls,
    )
}

mod forwarding_tests;
