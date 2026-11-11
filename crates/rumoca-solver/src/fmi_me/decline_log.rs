//! Records every compile request an execution backend declines.
//!
//! The runtime prepares its required compiled inventory before admission.
//! Wrapping the backend handle every host hands to the component makes each
//! declined construction request observable in the session's execution receipt.

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

    fn observe<T, E: std::fmt::Display>(
        &self,
        program: &str,
        result: Result<T, E>,
    ) -> Result<T, E> {
        if let Err(reason) = &result {
            self.record(program, &reason.to_string());
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
    fn compile_model_state_jacobian(
        &self,
        source: solve::SolveExecutionSource,
    ) -> Result<crate::ModelStateJacobianPreparation, String> {
        let result = self.inner.compile_model_state_jacobian(source)?;
        if let crate::ModelStateJacobianPreparation::Unsupported { reason } = &result {
            self.log.record("model_state_jacobian", reason);
        }
        Ok(result)
    }

    fn validate_model_context(&self, model: &solve::SolveModel) -> Result<(), String> {
        self.inner.validate_model_context(model)
    }

    fn finish_preparation(&self) {
        self.inner.finish_preparation();
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
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        self.log
            .observe("expression", self.inner.compile_expression(block))
    }

    fn compile_event_observation(
        &self,
        program: solve::RetainedEventProgram,
    ) -> Result<Rc<dyn crate::CompiledSolveEventObservation>, String> {
        self.log.observe(
            "event_observation",
            self.inner.compile_event_observation(program),
        )
    }

    fn compile_selectable_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        self.log.observe(
            "selectable_expression",
            self.inner.compile_selectable_expression(block),
        )
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
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
        sweep: &solve_eval::TornSweepComposite,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        self.log.observe(
            "torn_assignment_rows",
            self.inner.compile_torn_assignment_rows(sweep),
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

    struct NoModelJacobian;

    impl SolveExecutionBackend for NoModelJacobian {
        fn compile_expression(
            &self,
            _: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
            Err("no expression compiler".into())
        }
    }

    #[test]
    fn unsupported_model_jacobian_is_visible_in_the_component_decline_log() {
        let log = DeclineLog::default();
        let backend = RecordingBackend::new(Rc::new(NoModelJacobian), log.clone());
        let source = solve::SolveExecutionSource::new(solve::SolveModel::default());
        assert!(matches!(
            backend.compile_model_state_jacobian(source).unwrap(),
            crate::ModelStateJacobianPreparation::Unsupported { .. }
        ));
        let entries = log.snapshot();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].program, "model_state_jacobian");
        assert!(
            entries[0]
                .reason
                .contains("no prepared model state Jacobian")
        );
    }

    #[test]
    fn an_expression_decline_is_reported_under_its_own_program() {
        let log = DeclineLog::default();
        let backend = RecordingBackend::new(Rc::new(NoModelJacobian), log.clone());
        let error = match backend.compile_expression(&solve::ScalarProgramBlock::default()) {
            Ok(_) => panic!("the backend has no expression compiler"),
            Err(error) => error,
        };
        assert_eq!(error.to_string(), "no expression compiler");
        assert_eq!(
            log.snapshot(),
            vec![SimNativeDecline {
                program: "expression".into(),
                reason: "no expression compiler".into(),
                count: 1,
            }]
        );
    }

    #[test]
    fn a_declined_request_is_recorded_once_per_reason_with_a_count() {
        let log = DeclineLog::default();
        let declined: Result<(), String> = Err("no compact form".into());
        assert!(log.observe("expression", declined.clone()).is_err());
        assert!(log.observe("expression", declined).is_err());
        assert!(log.observe::<(), String>("expression", Ok(())).is_ok());
        assert!(
            log.observe::<(), String>("event_transaction", Err("other".into()))
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
