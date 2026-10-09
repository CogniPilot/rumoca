//! Records every compile request an execution backend declines.
//!
//! The runtime compiles programs lazily and runs a declined one in the
//! interpreter. Wrapping the backend at the one handle every host hands to the
//! component makes each decline observable at a single site, so the session's
//! execution receipt can name what did not run compiled instead of leaving the
//! fallback silent.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_eval_solve as solve_eval;
use rumoca_eval_solve::RowEvalContext;
use rumoca_ir_solve as solve;

use crate::SimNativeDecline;
use crate::{
    CompiledSolveAssignmentSchedule, CompiledSolveEventTransaction, CompiledSolveExpression,
    CompiledSolveJacobianExpression, CompiledSolveTargetValues, SolveExecutionBackend,
};

/// Declined requests, shared by every clone of one backend handle.
#[derive(Clone, Default)]
pub(super) struct DeclineLog(Rc<RefCell<Vec<SimNativeDecline>>>);

impl DeclineLog {
    pub(super) fn snapshot(&self) -> Vec<SimNativeDecline> {
        self.0.borrow().clone()
    }

    fn record(&self, program: &str, reason: &str) {
        let mut log = self.0.borrow_mut();
        match log
            .iter_mut()
            .find(|entry| entry.program == program && entry.reason == reason)
        {
            Some(entry) => entry.count += 1,
            None => log.push(SimNativeDecline {
                program: program.to_owned(),
                reason: reason.to_owned(),
                count: 1,
            }),
        }
    }

    fn observe<T>(&self, program: &str, result: Result<T, String>) -> Result<T, String> {
        if let Err(reason) = &result {
            self.record(program, reason);
        }
        result
    }
}

/// The backend a host supplied, with its declines recorded.
pub(super) struct RecordingBackend {
    inner: Rc<dyn SolveExecutionBackend>,
    log: DeclineLog,
}

impl RecordingBackend {
    pub(super) fn new(inner: Rc<dyn SolveExecutionBackend>, log: DeclineLog) -> Self {
        Self { inner, log }
    }
}

impl SolveExecutionBackend for RecordingBackend {
    fn validate_model_context(&self, model: &solve::SolveModel) -> Result<(), String> {
        self.inner.validate_model_context(model)
    }

    fn compile_target_values(
        &self,
        plan: &solve_eval::PreparedTargetValuePlan,
        context: RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        self.log.observe(
            "target_values",
            self.inner.compile_target_values(plan, context),
        )
    }

    fn pure_call_execution(&self) -> Option<&dyn solve_eval::PureCallExecution> {
        self.inner.pure_call_execution()
    }

    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.log
            .observe("expression", self.inner.compile_expression(block))
    }

    fn compile_selectable_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.log.observe(
            "selectable_expression",
            self.inner.compile_selectable_expression(block),
        )
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        self.log.observe(
            "jacobian_expression",
            self.inner.compile_jacobian_expression(block),
        )
    }

    fn compile_compute_jacobian_expression(
        &self,
        block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        self.log.observe(
            "compute_jacobian_expression",
            self.inner.compile_compute_jacobian_expression(block),
        )
    }

    fn compile_compute_expression(
        &self,
        block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        self.log.observe(
            "compute_expression",
            self.inner.compile_compute_expression(block),
        )
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        self.log.observe(
            "assignment_schedule",
            self.inner
                .compile_assignment_schedule(source, owners, schedule),
        )
    }

    fn compile_torn_assignment_rows(
        &self,
        rows: &[Vec<solve::LinearOp>],
        target_y_indices: &[usize],
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        self.log.observe(
            "torn_assignment_rows",
            self.inner
                .compile_torn_assignment_rows(rows, target_y_indices),
        )
    }

    fn compile_event_transaction(
        &self,
        program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        self.log.observe(
            "event_transaction",
            self.inner.compile_event_transaction(program),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_declined_request_is_recorded_once_per_reason_with_a_count() {
        let log = DeclineLog::default();
        let declined: Result<(), String> = Err("no compact form".into());
        assert!(log.observe("expression", declined.clone()).is_err());
        assert!(log.observe("expression", declined).is_err());
        assert!(log.observe("expression", Ok(())).is_ok());
        assert!(
            log.observe::<()>("event_transaction", Err("other".into()))
                .is_err()
        );
        assert_eq!(
            log.snapshot(),
            vec![
                SimNativeDecline {
                    program: "expression".into(),
                    reason: "no compact form".into(),
                    count: 2,
                },
                SimNativeDecline {
                    program: "event_transaction".into(),
                    reason: "other".into(),
                    count: 1,
                },
            ]
        );
    }
}
