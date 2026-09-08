//! Exhaustive facade witnesses for the FMI 3.0.2 ME operation relation.

use super::model_fixture::{
    divergent_initialization_model, divergent_runtime_event_model, single_state_indicator_model,
    single_state_input_model, single_state_model, single_state_time_event_model,
};
use crate::fmi_me::lifecycle::{
    MeConfigurationCapability, MeLifecycle, MeLifecycleOperation, MeState,
};
use crate::fmi_me::{
    MeDirectionalKnownBatch, MeDirectionalUnknownBatch, MeError, MeInstanceConfig, MeModelSource,
    MeStage, MeTime, SolveMeKernel,
};

const START_TIME: f64 = 0.0;
const STOP_TIME: f64 = 1.0;
const TOLERANCE: f64 = 1.0e-8;
const HOST_ITERATION_CEILING: usize = 4;

fn instantiate(model: rumoca_ir_solve::SolveModel) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::configuration_fixture(
            crate::test_support::fmi_component(model),
            MeConfigurationCapability::TunableStructuralParameter,
        ),
        &MeInstanceConfig::new("solve-verification", TOLERANCE, START_TIME, STOP_TIME)
            .expect("verification instance configuration constructs"),
    )
    .expect("bounded fixture instantiates")
}

fn state_directional_batches(
    kernel: &SolveMeKernel,
) -> (MeDirectionalUnknownBatch, MeDirectionalKnownBatch) {
    let knowns = kernel
        .continuous_state_value_references()
        .and_then(|references| kernel.directional_known_batch(references))
        .expect("state known batch constructs");
    let unknowns = kernel
        .continuous_state_derivative_value_references()
        .and_then(|references| kernel.directional_unknown_batch(references))
        .expect("state-derivative unknown batch constructs");
    (unknowns, knowns)
}

fn run_to_event_mode(kernel: &mut SolveMeKernel) {
    kernel
        .enter_initialization_mode(START_TIME)
        .expect("initialization starts");
    kernel
        .exit_initialization_mode()
        .expect("initialization settles");
}

fn settle_initial_event(kernel: &mut SolveMeKernel) {
    let _ = settled_initial_event(kernel);
}

fn settled_initial_event(kernel: &mut SolveMeKernel) -> crate::fmi_me::MeDiscreteStates {
    let mut update = kernel
        .update_discrete_states()
        .expect("initial event update succeeds");
    while update.discrete_states_need_update {
        update = kernel
            .update_discrete_states()
            .expect("initial event fixed point succeeds");
    }
    update
}

fn run_to_continuous_time_mode(kernel: &mut SolveMeKernel) {
    run_to_event_mode(kernel);
    settle_initial_event(kernel);
    kernel
        .enter_continuous_time_mode()
        .expect("continuous-time mode follows the initial event");
}

fn drive_to_state(kernel: &mut SolveMeKernel, state: MeState) {
    match state {
        MeState::Instantiated => {}
        MeState::ConfigurationMode => kernel
            .enter_configuration_mode()
            .expect("configuration mode is reachable"),
        MeState::InitializationMode => kernel
            .enter_initialization_mode(START_TIME)
            .expect("initialization mode is reachable"),
        MeState::EventMode => run_to_event_mode(kernel),
        MeState::ReconfigurationMode => {
            run_to_event_mode(kernel);
            kernel
                .enter_configuration_mode()
                .expect("reconfiguration mode is reachable");
        }
        MeState::ContinuousTimeMode => run_to_continuous_time_mode(kernel),
        MeState::Terminated => {
            run_to_event_mode(kernel);
            kernel
                .terminate()
                .expect("termination from Event Mode succeeds");
        }
    }
    assert_eq!(kernel.verification_observable_state().0, state);
}

fn kernel_for_operation(state: MeState, operation: MeLifecycleOperation) -> SolveMeKernel {
    let mut kernel = instantiate(single_state_indicator_model());
    drive_to_state(&mut kernel, state);
    if state == MeState::EventMode && operation == MeLifecycleOperation::EnterContinuousTimeMode {
        settle_initial_event(&mut kernel);
    }
    kernel
}

fn apply_operation(
    kernel: &mut SolveMeKernel,
    operation: MeLifecycleOperation,
) -> Result<(), MeError> {
    let state_count = kernel.model_description().continuous_state_count;
    let indicator_count = kernel.model_description().event_indicator_count;
    match operation {
        MeLifecycleOperation::EnterConfigurationMode => kernel.enter_configuration_mode(),
        MeLifecycleOperation::ExitConfigurationMode => kernel.exit_configuration_mode(),
        MeLifecycleOperation::EnterInitializationMode => {
            kernel.enter_initialization_mode(START_TIME)
        }
        MeLifecycleOperation::ExitInitializationMode => kernel.exit_initialization_mode(),
        MeLifecycleOperation::UpdateDiscreteStates => kernel.update_discrete_states().map(|_| ()),
        MeLifecycleOperation::EnterContinuousTimeMode => kernel.enter_continuous_time_mode(),
        MeLifecycleOperation::EnterEventMode => kernel.enter_event_mode(),
        MeLifecycleOperation::Terminate => kernel.terminate(),
        MeLifecycleOperation::SetTime => kernel.set_time(MeTime::at(START_TIME)),
        MeLifecycleOperation::SetContinuousStates => {
            kernel.set_continuous_states(&vec![1.0; state_count])
        }
        MeLifecycleOperation::GetContinuousStates => {
            kernel.get_continuous_states(&mut vec![0.0; state_count])
        }
        MeLifecycleOperation::GetNominalsOfContinuousStates => {
            kernel.get_nominals_of_continuous_states(&mut vec![0.0; state_count])
        }
        MeLifecycleOperation::GetContinuousStateDerivatives => {
            kernel.get_continuous_state_derivatives(&mut vec![0.0; state_count])
        }
        MeLifecycleOperation::GetDirectionalDerivative => {
            let (unknowns, knowns) = state_directional_batches(kernel);
            kernel.get_directional_derivative(
                &unknowns,
                &knowns,
                &vec![1.0; state_count],
                &mut vec![0.0; state_count],
            )
        }
        MeLifecycleOperation::GetEventIndicators => {
            kernel.get_event_indicators(&mut vec![0.0; indicator_count])
        }
        MeLifecycleOperation::SetFloat64 => kernel.set_float64(&[], &[]),
        MeLifecycleOperation::GetFloat64 => kernel.get_float64(&[], &mut []),
        MeLifecycleOperation::CompletedIntegratorStep => {
            kernel.completed_integrator_step(true).map(|_| ())
        }
        MeLifecycleOperation::GetFmuState => {
            let _ = kernel.fmu_state();
            Ok(())
        }
        MeLifecycleOperation::SetFmuState => {
            let saved = kernel.fmu_state();
            kernel.reset_to_fmu_state(&saved)
        }
    }
}

fn assert_facade_operation(state: MeState, operation: MeLifecycleOperation) {
    let mut kernel = kernel_for_operation(state, operation);
    let before = kernel.verification_observable_state();
    let checkpoint = kernel.fmu_state();
    let expected = MeLifecycle::relation_for_verification(
        MeConfigurationCapability::TunableStructuralParameter,
        state,
        operation,
    );
    let actual = apply_operation(&mut kernel, operation);
    match expected {
        Ok(target) => {
            actual.unwrap_or_else(|error| {
                panic!("{operation:?} in {state:?} was admitted but failed: {error}")
            });
            assert_eq!(
                kernel.verification_observable_state().0,
                target.unwrap_or(state)
            );
        }
        Err(_) => {
            assert!(
                actual.is_err(),
                "{operation:?} unexpectedly succeeded in {state:?}"
            );
            assert_eq!(kernel.verification_observable_state(), before);
            assert!(kernel.verification_matches_snapshot(&checkpoint));
        }
    }
}

#[test]
fn facade_operation_matrix_is_exact_and_refusals_are_atomic() {
    for state in MeState::ALL {
        for operation in MeLifecycleOperation::ALL {
            assert_facade_operation(state, operation);
        }
    }
}

#[test]
fn exact_fmi_buffers_reject_short_and_surplus_without_mutation() {
    let mut kernel = instantiate(single_state_indicator_model());
    run_to_continuous_time_mode(&mut kernel);
    assert_eq!(kernel.model_description().continuous_state_count, 1);
    assert_eq!(kernel.model_description().event_indicator_count, 1);

    for mut buffer in [Vec::new(), vec![3.0, 5.0]] {
        let component = kernel.fmu_state();
        let before = buffer.clone();
        kernel
            .get_continuous_states(&mut buffer)
            .expect_err("non-exact state result buffers are refused");
        assert_eq!(buffer, before);
        assert!(kernel.verification_matches_snapshot(&component));
    }
    for states in [Vec::new(), vec![7.0, 11.0]] {
        let component = kernel.fmu_state();
        kernel
            .set_continuous_states(&states)
            .expect_err("non-exact state input buffers are refused");
        assert!(kernel.verification_matches_snapshot(&component));
    }
    for mut buffer in [Vec::new(), vec![13.0, 17.0]] {
        let component = kernel.fmu_state();
        let before = buffer.clone();
        kernel
            .get_nominals_of_continuous_states(&mut buffer)
            .expect_err("non-exact nominal buffers are refused");
        assert_eq!(buffer, before);
        assert!(kernel.verification_matches_snapshot(&component));
    }
    for mut buffer in [Vec::new(), vec![19.0, 23.0]] {
        let component = kernel.fmu_state();
        let before = buffer.clone();
        kernel
            .get_continuous_state_derivatives(&mut buffer)
            .expect_err("non-exact derivative buffers are refused");
        assert_eq!(buffer, before);
        assert!(kernel.verification_matches_snapshot(&component));
    }
    let (unknowns, knowns) = state_directional_batches(&kernel);
    for seed in [Vec::new(), vec![29.0, 31.0]] {
        let component = kernel.fmu_state();
        let mut sensitivity = vec![37.0];
        kernel
            .get_directional_derivative(&unknowns, &knowns, &seed, &mut sensitivity)
            .expect_err("non-exact serialized seed buffers are refused");
        assert_eq!(sensitivity, [37.0]);
        assert!(kernel.verification_matches_snapshot(&component));
    }
    for mut sensitivity in [Vec::new(), vec![41.0, 43.0]] {
        let component = kernel.fmu_state();
        let before = sensitivity.clone();
        kernel
            .get_directional_derivative(&unknowns, &knowns, &[1.0], &mut sensitivity)
            .expect_err("non-exact serialized sensitivity buffers are refused");
        assert_eq!(sensitivity, before);
        assert!(kernel.verification_matches_snapshot(&component));
    }
    for mut buffer in [Vec::new(), vec![47.0, 53.0]] {
        let component = kernel.fmu_state();
        let before = buffer.clone();
        kernel
            .get_event_indicators(&mut buffer)
            .expect_err("non-exact indicator buffers are refused");
        assert_eq!(buffer, before);
        assert!(kernel.verification_matches_snapshot(&component));
    }
}

#[test]
fn host_event_iteration_terminates_after_one_argumentless_event_entry() {
    let mut kernel = instantiate(single_state_model());
    run_to_continuous_time_mode(&mut kernel);
    kernel
        .enter_event_mode()
        .expect("argumentless state-event entry is reachable");
    let mut iterations = 0usize;
    loop {
        iterations += 1;
        assert!(iterations <= HOST_ITERATION_CEILING);
        let update = kernel
            .update_discrete_states()
            .expect("the component-owned event fixed point settles");
        if !update.discrete_states_need_update {
            break;
        }
    }
    assert_eq!(iterations, 1);
}

#[test]
fn scheduled_time_event_terminates_after_one_argumentless_event_entry() {
    let mut kernel = instantiate(single_state_time_event_model());
    run_to_event_mode(&mut kernel);
    let initial = settled_initial_event(&mut kernel);
    let stop = initial
        .next_event_time
        .expect("the initial event update announces the scheduled event");
    assert_eq!(stop.to_bits(), 0.5_f64.to_bits());
    kernel
        .enter_continuous_time_mode()
        .expect("continuous-time mode follows the initial event");
    kernel
        .set_time(MeTime::at(stop))
        .expect("the announced event coordinate is finite");
    kernel
        .enter_event_mode()
        .expect("argumentless entry consumes the component-owned time event");
    let update = kernel
        .update_discrete_states()
        .expect("the scheduled event settles");
    assert!(!update.discrete_states_need_update);
    assert!(!update.nominals_of_continuous_states_changed);
}

fn non_convergent_initialization_is_bounded(increment: f64) {
    let mut kernel = instantiate(divergent_initialization_model(increment));
    kernel
        .enter_initialization_mode(START_TIME)
        .expect("initialization starts");
    let error = kernel
        .exit_initialization_mode()
        .expect_err("the divergent initialization fixed point reaches its bound");
    assert_eq!(error.stage(), Some(MeStage::Initialization));
    assert!(matches!(error.kind(), MeError::Evaluation { .. }));
    assert!(error.to_string().contains("did not converge"));
}

fn non_convergent_runtime_event_is_bounded(increment: f64) {
    let mut kernel = instantiate(divergent_runtime_event_model(increment));
    run_to_continuous_time_mode(&mut kernel);
    kernel
        .enter_event_mode()
        .expect("argumentless runtime state-event entry is reachable");
    let error = kernel
        .update_discrete_states()
        .expect_err("the divergent runtime event fixed point reaches its bound");
    assert_eq!(error.stage(), Some(MeStage::EventIteration));
    assert!(matches!(error.kind(), MeError::Evaluation { .. }));
    assert!(error.to_string().contains("did not converge"));
}

#[test]
fn bounded_non_convergent_event_fixtures_remain_used() {
    for increment in [1.0, 2.0, 3.0, 4.0] {
        non_convergent_initialization_is_bounded(increment);
        non_convergent_runtime_event_is_bounded(increment);
    }
}

mod bounded_event_properties {
    use proptest::prelude::*;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(64))]

        #[test]
        fn non_convergent_initialization_reaches_its_bound(increment in 1.0f64..=4.0f64) {
            super::non_convergent_initialization_is_bounded(increment);
        }

        #[test]
        fn non_convergent_runtime_event_reaches_its_bound(increment in 1.0f64..=4.0f64) {
            super::non_convergent_runtime_event_is_bounded(increment);
        }
    }
}

#[test]
fn float64_batches_validate_all_references_before_writes_or_results() {
    let mut first = instantiate(single_state_input_model());
    let mut second = instantiate(single_state_input_model());
    let first_u = first.value_reference("u").expect("first input is issued");
    let second_u = second.value_reference("u").expect("second input is issued");

    let first_before = first.fmu_state();
    first
        .set_float64(&[first_u.clone(), second_u.clone()], &[2.0, 3.0])
        .expect_err("a foreign trailing reference rejects the write batch");
    assert!(first.verification_matches_snapshot(&first_before));

    let second_before = second.fmu_state();
    let mut values = [31.0, 37.0];
    second
        .get_float64(&[second_u, first_u], &mut values)
        .expect_err("a foreign trailing reference rejects the read batch");
    assert_eq!(values, [31.0, 37.0]);
    assert!(second.verification_matches_snapshot(&second_before));
}

#[test]
fn terminated_allows_final_getters_but_no_mutation() {
    let mut kernel = instantiate(single_state_input_model());
    run_to_event_mode(&mut kernel);
    kernel.terminate().expect("Event Mode may terminate");
    let state_ref = kernel
        .value_reference("x")
        .expect("state reference is issued");
    let input_ref = kernel
        .value_reference("u")
        .expect("input reference is issued");
    let mut one = [f64::NAN];
    kernel
        .get_continuous_states(&mut one)
        .expect("final states");
    kernel
        .get_nominals_of_continuous_states(&mut one)
        .expect("final nominals");
    kernel
        .get_continuous_state_derivatives(&mut one)
        .expect("final derivatives");
    let (unknowns, knowns) = state_directional_batches(&kernel);
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0], &mut one)
        .expect("final directional derivative");
    kernel
        .get_event_indicators(&mut [])
        .expect("empty final indicator batch");
    kernel
        .get_float64(&[state_ref], &mut one)
        .expect("final Float64 output");
    let terminal_after_final_reads = kernel.fmu_state();

    for refusal in [
        kernel.set_time(MeTime::at(0.5)),
        kernel.set_continuous_states(&[2.0]),
        kernel.set_float64(&[input_ref], &[2.0]),
    ] {
        refusal.expect_err("Terminated is mutation-absorbing");
        assert!(kernel.verification_matches_snapshot(&terminal_after_final_reads));
    }
}

#[test]
fn opaque_snapshots_restore_all_issued_states_and_are_reusable() {
    for target in MeState::ALL {
        let mut kernel = instantiate(single_state_indicator_model());
        let instantiated = kernel.fmu_state();
        drive_to_state(&mut kernel, target);
        let saved = kernel.fmu_state();
        kernel
            .reset_to_fmu_state(&instantiated)
            .expect("same-instance pristine snapshot restores");
        kernel
            .reset_to_fmu_state(&saved)
            .expect("same-instance target snapshot restores");
        assert_eq!(kernel.verification_observable_state().0, target);
        kernel
            .reset_to_fmu_state(&instantiated)
            .expect("pristine snapshot may be reused");
        kernel
            .reset_to_fmu_state(&saved)
            .expect("FMI permits repeated SetFMUState from one snapshot");
        assert_eq!(kernel.verification_observable_state().0, target);
    }
}
