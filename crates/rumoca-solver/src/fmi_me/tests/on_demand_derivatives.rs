//! Completed-step bookkeeping must not invent derivative requests.

use super::{fixture_instance_config, harmonic_oscillator, refresh_owned};
use crate::fmi_me::{MeExecutionBackend, MeModelSource, MeTime, SolveMeKernel};
use crate::runtime::solve_runtime::{
    CompiledSolveAssignmentSchedule, CompiledSolveEventTransaction, CompiledSolveExpression,
    CompiledSolveJacobianExpression, SolveExecutionBackend,
};
use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};
use rumoca_ir_solve as solve;
use std::{cell::Cell, rc::Rc};

#[derive(Default)]
struct CountingBackend(Rc<Cell<usize>>);

struct Expression {
    prepared: PreparedScalarProgramBlock,
    calls: Rc<Cell<usize>>,
}

impl CompiledSolveExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        assert!(tables.is_empty(), "the fixture contains no table calls");
        self.calls.set(self.calls.get() + 1);
        self.prepared
            .eval_with_context(y, p, t, RowEvalContext::default(), out)
            .map_err(|error| error.to_string())
    }
}

impl SolveExecutionBackend for CountingBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        Ok(Rc::new(Expression {
            prepared: PreparedScalarProgramBlock::new(block.clone())
                .map_err(|error| error.to_string())?,
            calls: self.0.clone(),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        _: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        Err("directional execution uses the fixture interpreter".into())
    }

    fn compile_assignment_schedule(
        &self,
        _: &solve::ComputeBlock,
        _: &solve::ContinuousRefreshOwners,
        _: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("the fixture has no algebraic assignments".into())
    }

    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("the fixture has no event transactions".into())
    }
}

#[test]
fn completed_steps_do_not_evaluate_unrequested_derivatives() {
    let model = refresh_owned(harmonic_oscillator());
    let backend = Rc::new(CountingBackend::default());
    let mut kernel = SolveMeKernel::instantiate_with_execution_backend(
        MeModelSource::fixture(&model),
        &fixture_instance_config(),
        Some(MeExecutionBackend::new(backend.clone())),
    )
    .unwrap();
    kernel.enter_initialization_mode().unwrap();
    kernel.exit_initialization_mode().unwrap();
    kernel.update_discrete_states().unwrap();
    kernel.enter_continuous_time_mode().unwrap();
    backend.0.set(0);

    for (step, state) in [[1.5, -0.5], [2.5, -1.5]].iter().enumerate() {
        kernel
            .set_time(MeTime::at((step + 1) as f64 * 0.25))
            .unwrap();
        kernel.set_continuous_states(state).unwrap();
        assert!(
            !kernel
                .completed_integrator_step(true)
                .unwrap()
                .enter_event_mode
        );
        assert_eq!(
            backend.0.get(),
            step,
            "completion evaluated an unrequested RHS"
        );
        let mut out = Vec::new();
        kernel.get_continuous_state_derivatives(&mut out).unwrap();
        assert_eq!(out, [state[1], -4.0 * state[0]]);
        assert_eq!(backend.0.get(), step + 1);
        kernel.get_continuous_state_derivatives(&mut out).unwrap();
        assert_eq!(
            backend.0.get(),
            step + 1,
            "an identical requested point remains cached"
        );
    }
}
