//! Record required requests while preserving the numeric fixtures' factories.
use super::*;
use std::cell::Cell;

pub(crate) struct RecordingBackend {
    inner: Rc<dyn SolveExecutionBackend>,
    requests: Cell<(usize, usize, usize)>,
}

impl RecordingBackend {
    pub(crate) fn new(inner: Rc<dyn SolveExecutionBackend>) -> Self {
        Self {
            inner,
            requests: Cell::new((0, 0, 0)),
        }
    }

    /// Jacobian, assignment-schedule and event-transaction requests, in order.
    pub(crate) fn requests(&self) -> (usize, usize, usize) {
        self.requests.get()
    }
}

impl SolveExecutionBackend for RecordingBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.inner.compile_expression(block)
    }

    fn compile_selectable_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.inner.compile_selectable_expression(block)
    }

    fn compile_compute_expression(
        &self,
        block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        self.inner.compile_compute_expression(block)
    }

    fn compile_compute_jacobian_expression(
        &self,
        block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        self.inner.compile_compute_jacobian_expression(block)
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        let (jacobians, assignments, transactions) = self.requests.get();
        self.requests
            .set((jacobians + 1, assignments, transactions));
        self.inner.compile_jacobian_expression(block)
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        let (jacobians, assignments, transactions) = self.requests.get();
        self.requests
            .set((jacobians, assignments + 1, transactions));
        self.inner
            .compile_assignment_schedule(source, owners, schedule)
    }

    fn compile_event_transaction(
        &self,
        program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        let (jacobians, assignments, transactions) = self.requests.get();
        self.requests
            .set((jacobians, assignments, transactions + 1));
        self.inner.compile_event_transaction(program)
    }
}
