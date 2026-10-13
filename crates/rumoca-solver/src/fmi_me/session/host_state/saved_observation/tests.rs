use super::*;
use crate::fmi_me as me;
use rumoca_ir_solve as solve;

fn input_model() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".into()].into();
    model.parameters = vec![1.0].into();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("saved-observation.mo"),
        1,
        2,
    );
    let visible_rows = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        span.require_provenance("saved observation fixture")
            .unwrap(),
    )
    .unwrap();
    model = crate::fixture_layout::with_visible_expressions(
        model,
        vec!["u".into()],
        visible_rows,
        Vec::new(),
    );
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .unwrap();
    model
}

fn host_with_input() -> MeHostState {
    let model = input_model();
    let config = me::MeInstanceConfig::new("saved-observation", 1e-10, 0.0, 1.0).unwrap();
    let retained =
        me::MeRetainedComponent::instantiate(me::MeModelSource::fixture(&model), &config, None)
            .unwrap();
    let options = me::driver::live_session_options(
        &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
        0.0,
        1e-8,
        1e-8,
        1e-10,
        None,
    )
    .unwrap();
    retained.into_lease(options).unwrap().host
}

fn set_input(host: &MeHostState, value: f64) {
    let reference = host.kernel.borrow().unwrap().value_reference("u").unwrap();
    host.kernel
        .borrow_mut()
        .unwrap()
        .set_float64(&[reference], &[value])
        .unwrap();
}

#[test]
fn saved_observation_reads_old_inputs_and_restores_the_complete_current_state() {
    let mut host = host_with_input();
    let saved = host.kernel.borrow().unwrap().fmu_state().unwrap();
    host.adopt_point(0.25, &[]).unwrap();
    set_input(&host, 2.0);
    let current = host.kernel.borrow().unwrap().fmu_state().unwrap();

    assert_eq!(host.observe_saved_point(&saved, 0.125, &[]).unwrap(), [1.0]);
    assert!(
        host.kernel
            .borrow()
            .unwrap()
            .verification_matches_snapshot(&current)
    );
    assert_eq!(host.observe_current().unwrap(), [2.0]);
    assert!(host.usability_loss().is_none());
}

#[test]
fn failed_saved_observation_restores_current_state_and_preserves_the_error() {
    let mut host = host_with_input();
    let saved = host.kernel.borrow().unwrap().fmu_state().unwrap();
    host.adopt_point(0.25, &[]).unwrap();
    set_input(&host, 2.0);
    let current = host.kernel.borrow().unwrap().fmu_state().unwrap();

    let error = host
        .with_saved_component_state::<()>(&saved, || {
            set_input(&host, 3.0);
            Err(MeSessionError::Timeout { seconds: 7.0 })
        })
        .unwrap_err();
    assert!(matches!(error, MeSessionError::Timeout { seconds: 7.0 }));
    assert!(
        host.kernel
            .borrow()
            .unwrap()
            .verification_matches_snapshot(&current)
    );
}

#[test]
fn unwinding_saved_observation_restores_current_state_and_the_original_payload() {
    let host = host_with_input();
    let saved = host.kernel.borrow().unwrap().fmu_state().unwrap();
    set_input(&host, 2.0);
    let current = host.kernel.borrow().unwrap().fmu_state().unwrap();
    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        host.with_saved_component_state::<()>(&saved, || {
            set_input(&host, 3.0);
            std::panic::panic_any(73_u32);
        })
    }))
    .unwrap_err();
    assert_eq!(panic.downcast_ref::<u32>(), Some(&73));
    assert!(
        host.kernel
            .borrow()
            .unwrap()
            .verification_matches_snapshot(&current)
    );
    assert_eq!(host.usability_loss(), Some(MeSessionLoss::NumericalStep));
}

#[test]
fn a_foreign_observation_snapshot_is_rejected_without_running_its_body() {
    let host = host_with_input();
    let foreign = host_with_input()
        .kernel
        .borrow()
        .unwrap()
        .fmu_state()
        .unwrap();
    let current = host.kernel.borrow().unwrap().fmu_state().unwrap();
    let error = host
        .with_saved_component_state::<()>(&foreign, || panic!("foreign snapshot entered"))
        .unwrap_err();
    assert!(matches!(error, MeSessionError::Component(_)));
    assert!(
        host.kernel
            .borrow()
            .unwrap()
            .verification_matches_snapshot(&current)
    );
}

#[test]
fn common_initialization_completes_actual_initialization_mode_once() {
    let model = input_model();
    let config = me::MeInstanceConfig::new("initialization-handoff", 1e-10, 0.0, 1.0).unwrap();
    let mut kernel =
        me::SolveMeKernel::instantiate(me::MeModelSource::fixture(&model), &config).unwrap();
    kernel.enter_initialization_mode().unwrap();
    let options = me::driver::live_session_options(
        &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
        0.0,
        1e-8,
        1e-8,
        1e-10,
        None,
    )
    .unwrap();
    let outcome = run_fmi_initialization(&mut kernel, &options, true).unwrap();
    assert_eq!(outcome.initial_values, Some(vec![1.0]));
    assert_eq!(
        kernel.initialization_state(),
        me::lifecycle::MeState::ContinuousTimeMode
    );
    let accepted = kernel.fmu_state().unwrap();
    assert!(run_fmi_initialization(&mut kernel, &options, true).is_err());
    assert!(kernel.verification_matches_snapshot(&accepted));
}

#[cfg(feature = "native-wasm")]
#[test]
fn native_handoff_refuses_a_scalar_kernel_without_consuming_its_state() {
    let model = input_model();
    let config = me::MeInstanceConfig::new("scalar-handoff-refusal", 1e-10, 0.0, 1.0).unwrap();
    let mut kernel =
        me::SolveMeKernel::instantiate(me::MeModelSource::fixture(&model), &config).unwrap();
    kernel.enter_initialization_mode().unwrap();
    let saved = kernel.fmu_state().unwrap();
    let (kernel, error) = match me::MeRetainedComponent::from_initializing_native_kernel(kernel) {
        Ok(_) => panic!("a scalar kernel has no admitted native handoff"),
        Err(error) => error,
    };
    assert!(matches!(error, me::MeError::Contract { .. }));
    assert!(kernel.verification_matches_snapshot(&saved));
}

#[test]
fn no_trace_session_defers_authored_expression_reporting_metadata() {
    let host = host_with_input();
    assert!(!host.options.records_trace());
    assert!(
        !host
            .kernel
            .borrow()
            .unwrap()
            .visible_report_is_materialized()
    );
    assert_eq!(
        host.kernel
            .borrow()
            .unwrap()
            .model_description()
            .output_names,
        ["u"]
    );
    assert!(
        host.kernel
            .borrow()
            .unwrap()
            .visible_report_is_materialized()
    );
    // Authored expression fixtures have no inferred source declaration metadata.
    assert!(
        host.kernel
            .borrow()
            .unwrap()
            .model_description()
            .output_meta
            .is_empty()
    );
}
