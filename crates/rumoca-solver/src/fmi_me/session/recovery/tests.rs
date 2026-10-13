use super::*;
use crate::fmi_me::{MeModelSource, driver::live_session_options};
use rumoca_ir_solve as solve;

fn options(start: f64) -> MeSessionOptions {
    live_session_options(
        &solve::fmi::RootLocationPlan::STANDARD,
        start,
        1e-8,
        1e-8,
        1e-10,
        None,
    )
    .unwrap()
}

fn input_model() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".into()].into();
    model.parameters = vec![1.0].into();
    model.problem.layout =
        solve::VarLayout::from_parts([("u".into(), solve::scalar_slot_p(0))].into(), 0, 1);
    crate::fmi_me::tests::refresh_owned(model)
}

fn retained(model: &solve::SolveModel) -> MeRetainedComponent {
    let config = MeInstanceConfig::new("recovery", 1e-10, 2.0, 6.0).unwrap();
    MeRetainedComponent::instantiate(MeModelSource::fixture(model), &config, None).unwrap()
}

fn input_value(component: &MeRetainedComponent) -> f64 {
    let kernel = component.kernel.borrow().unwrap();
    let reference = kernel.value_reference("u").unwrap();
    let mut values = [0.0];
    kernel.get_float64(&[reference], &mut values).unwrap();
    values[0]
}

#[test]
fn failed_input_scope_restoration_preserves_original_terminal_policy() {
    let model = input_model();
    let original = retained(&model);
    let mut session = match original.into_session_retaining(options(2.0), None) {
        Ok(session) => session,
        Err(_) => panic!("the existing zero-state driver attaches"),
    };
    crate::fmi_me::kernel::fail_input_restore();
    let error = session
        .write_input_transaction(|_| {
            Err(crate::fmi_me::MeError::Contract {
                reason: "injected input application failure".into(),
            })
        })
        .unwrap_err();
    match error {
        MeSessionError::AcceptedPointLost {
            restoration,
            attempted,
            ..
        } => {
            assert!(
                restoration
                    .to_string()
                    .contains("injected input restoration failure")
            );
            assert!(
                attempted
                    .unwrap()
                    .to_string()
                    .contains("injected input application failure")
            );
        }
        other => panic!("original accepted-point loss expected, got {other}"),
    }
    assert!(session.visible_values().is_err());
    let original = match session.into_retained_component() {
        Ok(original) => original,
        Err(_) => panic!("terminal session still returns the actual owned receipt"),
    };
    assert_eq!(input_value(&original), 1.0);
}

#[test]
fn consuming_recovery_keeps_same_cell_inputs_and_source_reset_then_start_five() {
    let model = input_model();
    let original = retained(&model);
    let cell = Rc::clone(&original.kernel);
    let mut session = match original.into_session_retaining(options(2.0), None) {
        Ok(session) => session,
        Err(_) => panic!("the existing zero-state driver attaches"),
    };
    session.set_input("u", -0.0).unwrap();
    let escaped = session.host.derivatives.issue_handle();
    let mut original = match session.into_retained_component() {
        Ok(original) => original,
        Err(_) => panic!("the consuming receipt returns the original component"),
    };
    assert!(Rc::ptr_eq(&cell, &original.kernel));
    assert!(escaped.derivatives(2.0, &[]).is_err());
    assert_eq!(input_value(&original).to_bits(), (-0.0_f64).to_bits());
    original.reset().unwrap();
    assert_eq!(input_value(&original).to_bits(), 1.0_f64.to_bits());
    let session = match original.into_session_retaining(options(5.0), None) {
        Ok(session) => session,
        Err(_) => panic!("source-reset scalar owner remains admissible at the next supplied start"),
    };
    assert_eq!(session.time(), 5.0);
    assert!(Rc::ptr_eq(&cell, &session.host.kernel));
}

#[test]
fn borrowed_lease_recovery_refuses_and_returns_the_actual_live_session() {
    let model = input_model();
    let mut original = retained(&model);
    let cell = Rc::clone(&original.kernel);
    let session = original
        .lease(options(2.0))
        .unwrap()
        .into_session(None)
        .unwrap();
    let (session, error) = match session.into_retained_component() {
        Err(failure) => failure,
        Ok(_) => panic!("a borrowed lease cannot mint a retained owner"),
    };
    assert!(matches!(error, MeSessionError::Contract { .. }));
    assert!(Rc::ptr_eq(&cell, &session.host.kernel));
    assert_eq!(session.time(), 2.0);
    assert!(session.output_values().is_ok());
    drop(session);
    original.reset().unwrap();
}

#[test]
fn plugin_attachment_failure_retains_actual_cell_and_accepted_inputs() {
    let model = crate::fmi_me::tests::refresh_owned(crate::fmi_me::tests::harmonic_oscillator());
    let original = retained(&model);
    let cell = Rc::clone(&original.kernel);
    let backend = Box::new(TimeOnlyIntegrator::new());
    let (mut original, error) = match original.into_session_retaining(options(2.0), Some(backend)) {
        Err(failure) => failure,
        Ok(_) => panic!("a time-only plugin cannot initialize a two-state point"),
    };
    assert!(matches!(error, MeSessionError::Integration(_)));
    assert!(Rc::ptr_eq(&cell, &original.kernel));
    assert_eq!(
        cell.borrow().unwrap().initialization_state(),
        crate::fmi_me::lifecycle::MeState::ContinuousTimeMode
    );
    original.reset().unwrap();
    assert!(Rc::ptr_eq(&cell, &original.kernel));
}

#[test]
fn failed_initialization_returns_same_actual_component_for_explicit_reset() {
    let mut model = crate::fmi_me::tests::nonlinear_right_limit_seed_model();
    model.initial_y = vec![-1.0, 2.0].into();
    let model = crate::fmi_me::tests::refresh_owned(model);
    let original = retained(&model);
    let cell = Rc::clone(&original.kernel);
    let (mut original, error) = match original.into_session_retaining(options(2.0), None) {
        Err(failure) => failure,
        Ok(_) => panic!("the original negative-state algebraic branch has no real solution"),
    };
    assert!(matches!(error, MeSessionError::Component(_)));
    assert!(Rc::ptr_eq(&cell, &original.kernel));
    original.reset().unwrap();
    let mut states = [0.0];
    cell.borrow()
        .unwrap()
        .get_continuous_states(&mut states)
        .unwrap();
    assert_eq!(states, [-1.0]);
}

#[cfg(feature = "native-wasm")]
#[test]
fn scalar_session_native_getters_refuse_without_touching_caller_buffers() {
    let model = input_model();
    let session = match retained(&model).into_session_retaining(options(2.0), None) {
        Ok(session) => session,
        Err(_) => panic!("scalar source session constructs"),
    };
    let mut real = [-0.0];
    let mut integer = [i64::MIN];
    let mut boolean = [true];
    assert!(session.get_native_float64(&[1], &mut real).is_err());
    assert!(session.get_native_integer(&[1], &mut integer).is_err());
    assert!(session.get_boolean(&[1], &mut boolean).is_err());
    assert_eq!(real[0].to_bits(), (-0.0_f64).to_bits());
    assert_eq!(integer, [i64::MIN]);
    assert_eq!(boolean, [true]);
}

#[cfg(feature = "native-wasm")]
#[test]
fn native_exit_initialization_preflight_retains_scalar_resource_without_inventing_mode() {
    let model = input_model();
    let original = retained(&model);
    let cell = Rc::clone(&original.kernel);
    let (original, error) = match original.complete_native_initialization_retaining(options(2.0)) {
        Err(failure) => failure,
        Ok(_) => panic!("a scalar Instantiated component has no native Initialization Mode"),
    };
    assert!(matches!(error, MeSessionError::Component(_)));
    assert!(Rc::ptr_eq(&cell, &original.kernel));
    assert_eq!(input_value(&original), 1.0);
    assert_eq!(
        cell.borrow().unwrap().initialization_state(),
        crate::fmi_me::lifecycle::MeState::Instantiated
    );
}

#[test]
fn initialization_options_are_read_from_the_actual_initialization_request() {
    let model = input_model();
    let config = MeInstanceConfig::new("recovery", 1e-10, 2.0, 6.0).unwrap();
    let mut kernel = SolveMeKernel::instantiate(MeModelSource::fixture(&model), &config).unwrap();
    let before = MeRetainedComponent::from_kernel(
        SolveMeKernel::instantiate(MeModelSource::fixture(&model), &config).unwrap(),
    );
    assert!(
        before.initialization_options().is_err(),
        "an instantiated component has no Initialization Mode request yet"
    );

    kernel.enter_initialization_mode().unwrap();
    let initializing = MeRetainedComponent::from_kernel(kernel);
    let read = initializing.initialization_options().unwrap();
    assert_eq!((read.start_time(), read.stop_time()), (2.0, Some(6.0)));
}
