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
struct CountingBackend(Rc<Cell<usize>>, Cell<usize>, bool);

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
    ) -> Result<(), crate::RuntimeSolveError> {
        assert!(tables.is_empty(), "the fixture contains no table calls");
        self.calls.set(self.calls.get() + 1);
        self.prepared
            .eval_with_context(y, p, t, RowEvalContext::default(), out)
            .map_err(Into::into)
    }
}

impl SolveExecutionBackend for CountingBackend {
    fn compile_model_state_jacobian(
        &self,
        _: solve::SolveExecutionSource,
    ) -> Result<crate::ModelStateJacobianPreparation, String> {
        self.1.set(self.1.get() + 1);
        if self.2 {
            return Err("injected model Jacobian preparation failure".into());
        }
        Ok(crate::ModelStateJacobianPreparation::Unsupported {
            reason: "fixture has no finite model product".into(),
        })
    }

    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        Ok(Rc::new(Expression {
            prepared: PreparedScalarProgramBlock::new(block.clone())
                .map_err(|error| error.to_string())?,
            calls: self.0.clone(),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        crate::runtime::solve_runtime::test_kernels::jacobian(block).map_err(Into::into)
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

#[test]
fn unrequested_model_jacobian_never_reaches_the_backend_or_decline_log() {
    let model = refresh_owned(harmonic_oscillator());
    let backend = Rc::new(CountingBackend(Rc::default(), Cell::new(0), true));
    let handle = MeExecutionBackend::new(backend.clone());
    let config = fixture_instance_config()
        .with_model_state_jacobian_request(crate::ModelStateJacobianRequest::NotRequested);
    let mut retained = crate::fmi_me::session::MeRetainedComponent::instantiate(
        MeModelSource::fixture(&model),
        &config,
        Some(handle.clone()),
    )
    .unwrap();
    retained.reset().unwrap();
    assert_eq!(backend.1.get(), 0);
    assert!(handle.declined().is_empty());

    let restarted = config.restarted_at(1.0, Some(2.0)).unwrap();
    SolveMeKernel::instantiate_with_execution_backend(
        MeModelSource::fixture(&model),
        &restarted,
        Some(handle.clone()),
    )
    .unwrap();
    assert_eq!(backend.1.get(), 0);
    assert!(handle.declined().is_empty());
}

#[test]
fn requested_unsupported_model_jacobian_retains_its_decline() {
    let model = refresh_owned(harmonic_oscillator());
    let backend = Rc::new(CountingBackend::default());
    let handle = MeExecutionBackend::new(backend.clone());
    SolveMeKernel::instantiate_with_execution_backend(
        MeModelSource::fixture(&model),
        &fixture_instance_config()
            .with_model_state_jacobian_request(crate::ModelStateJacobianRequest::FiniteAssembly),
        Some(handle.clone()),
    )
    .unwrap();
    assert_eq!(backend.1.get(), 1);
    let declines = handle.declined();
    assert_eq!(declines.len(), 1);
    assert_eq!(declines[0].program, "model_state_jacobian");
    assert_eq!(declines[0].reason, "fixture has no finite model product");
    assert_eq!(declines[0].count, 1);
}

#[test]
fn requested_model_jacobian_preparation_failure_is_fatal() {
    let model = refresh_owned(harmonic_oscillator());
    let backend = Rc::new(CountingBackend(Rc::default(), Cell::new(0), true));
    let handle = MeExecutionBackend::new(backend.clone());
    let result = SolveMeKernel::instantiate_with_execution_backend(
        MeModelSource::fixture(&model),
        &fixture_instance_config()
            .with_model_state_jacobian_request(crate::ModelStateJacobianRequest::FiniteAssembly),
        Some(handle.clone()),
    );
    let Err(error) = result else {
        panic!("requested preparation failure must refuse instantiation");
    };
    assert!(
        error
            .to_string()
            .contains("injected model Jacobian preparation failure")
    );
    assert_eq!(backend.1.get(), 1);
    assert!(
        handle.declined().is_empty(),
        "a fatal fault is not a decline"
    );
}

#[test]
fn alternate_runtime_keeps_the_primary_model_jacobian_demand() {
    let model = refresh_owned(harmonic_oscillator());
    for request in [
        crate::ModelStateJacobianRequest::NotRequested,
        crate::ModelStateJacobianRequest::FiniteAssembly,
    ] {
        let backend = Rc::new(CountingBackend::default());
        let primary = crate::SolveRuntime::new_with_model_owner(
            crate::SolveRuntimeModel::copied(&model),
            Some(backend.clone()),
            request,
        )
        .unwrap();
        let alternate = primary.new_alternate(&model).unwrap();
        assert_eq!(primary.model_state_jacobian_request(), request);
        assert_eq!(alternate.model_state_jacobian_request(), request);
        let expected = usize::from(request == crate::ModelStateJacobianRequest::FiniteAssembly) * 2;
        assert_eq!(backend.1.get(), expected);
    }
}
