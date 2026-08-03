//! Bounded proofs that the dynamic FMI 3 ME facade preserves the verified
//! lifecycle relation and rejects invalid scalar payloads without mutation.
//!
//! The pure transition table is proved in `me_lifecycle`; this module proves
//! that the production `SolveMeKernel` facade actually consults that table.
//! Each property makes one bounded facade call rather than asking a model
//! checker to explore long, mostly-invalid command sequences.

use super::model_fixture::{
    divergent_initialization_model, divergent_runtime_event_model, single_state_indicator_model,
    single_state_input_model, single_state_model, single_state_time_event_model,
};
use crate::fmi_me::lifecycle::{MeLifecycle, MeLifecycleCommand, MeState};
use crate::fmi_me::{
    MeError, MeEventCause, MeEventEntry, MeFmuState, MeIndicatorCrossing, MeInstanceConfig,
    MeModelSource, MeOutputSeries, MeRootProfile, MeStage, MeTime, ModelExchangeKernel,
    SolveMeKernel,
};

const START_TIME: f64 = 0.0;
const STOP_TIME: f64 = 1.0;
const TOLERANCE: f64 = 1.0e-8;
const MIN_TIME_GAP: f64 = 1.0e-9;
const HOST_ITERATION_CEILING: usize = 4;

#[derive(Debug, PartialEq, Eq)]
struct ObservableKernelState {
    lifecycle: MeState,
    time: u64,
    states: Vec<u64>,
    parameters: Vec<u64>,
}

fn instantiate(model: &rumoca_ir_solve::SolveModel) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::new(model),
        &MeInstanceConfig {
            instance_name: "solve-verification",
            tolerance: TOLERANCE,
            start_time: START_TIME,
            stop_time: STOP_TIME,
            root_profile: MeRootProfile::Component,
            numerics_profile: crate::fmi_me::MeNumericsProfile::Component,
        },
    )
    .expect("the bounded fixture is a well-formed ME model")
}

fn run_to_event_mode(kernel: &mut SolveMeKernel) {
    kernel
        .enter_initialization_mode()
        .expect("initialization mode is reachable from Instantiated");
    kernel
        .exit_initialization_mode()
        .expect("the one-state fixture settles initialization");
}

fn run_to_continuous_time_mode(kernel: &mut SolveMeKernel) {
    run_to_event_mode(kernel);
    kernel
        .update_discrete_states()
        .expect("the initial event boundary completes");
    kernel
        .enter_continuous_time_mode()
        .expect("continuous-time mode follows the initial event");
}

fn drive_to_state(kernel: &mut SolveMeKernel, state: MeState) {
    match state {
        MeState::Instantiated => {}
        MeState::InitializationMode => kernel
            .enter_initialization_mode()
            .expect("InitializationMode is reachable"),
        MeState::EventMode => run_to_event_mode(kernel),
        MeState::ContinuousTimeMode => run_to_continuous_time_mode(kernel),
        MeState::Terminated => kernel.terminate().expect("Terminated is reachable"),
    }
    assert_eq!(kernel.verification_observable_state().0, state);
}

fn kernel_in_state(state: MeState) -> SolveMeKernel {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    drive_to_state(&mut kernel, state);
    kernel
}

fn observable_state(kernel: &SolveMeKernel) -> ObservableKernelState {
    let (lifecycle, time, states, parameters) = kernel.verification_observable_state();
    ObservableKernelState {
        lifecycle,
        time,
        states,
        parameters,
    }
}

fn apply_lifecycle_command(
    kernel: &mut SolveMeKernel,
    command: MeLifecycleCommand,
) -> Result<(), MeError> {
    match command {
        MeLifecycleCommand::EnterInitializationMode => kernel.enter_initialization_mode(),
        MeLifecycleCommand::ExitInitializationMode => kernel.exit_initialization_mode(),
        MeLifecycleCommand::UpdateDiscreteStates => kernel.update_discrete_states().map(|_| ()),
        MeLifecycleCommand::EnterContinuousTimeMode => kernel.enter_continuous_time_mode(),
        MeLifecycleCommand::EnterEventMode => kernel.enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: START_TIME,
            horizon: STOP_TIME,
        }),
        MeLifecycleCommand::Terminate => kernel.terminate(),
    }
}

fn relation_rejects(state: MeState, command: MeLifecycleCommand) -> bool {
    let mut lifecycle = MeLifecycle::instantiated();
    lifecycle.restore_for_verification(state);
    lifecycle.next(command).is_err()
}

/// ME-LIFE-001/002 facade clause: every lifecycle transition rejected by the
/// pure relation is rejected by the production facade before any externally
/// observable component state changes.
fn property_rejected_facade_transition_preserves_state(
    state: MeState,
    command: MeLifecycleCommand,
) {
    if !relation_rejects(state, command) {
        return;
    }
    let mut kernel = kernel_in_state(state);
    let before = observable_state(&kernel);
    let checkpoint = kernel.fmu_state();
    let error = apply_lifecycle_command(&mut kernel, command)
        .expect_err("the dynamic facade must reject an edge absent from the pure relation");
    assert!(
        matches!(error.kind(), MeError::Contract { .. }),
        "an invalid lifecycle call is a host contract violation"
    );
    assert_eq!(
        observable_state(&kernel),
        before,
        "a rejected lifecycle transition must not mutate observable kernel state"
    );
    assert!(kernel.verification_matches_snapshot(&checkpoint));
}

fn non_finite_from_index(index: u8) -> f64 {
    match index % 3 {
        0 => f64::NAN,
        1 => f64::INFINITY,
        2 => f64::NEG_INFINITY,
        _ => unreachable!("index modulo three is covered above"),
    }
}

/// ME-BUF-001 scalar clause: non-finite time, event-boundary, and
/// continuous-state writes are rejected before any observable state changes.
fn property_non_finite_setters_are_transactional(index: u8) {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    kernel
        .set_time(MeTime::at(0.25))
        .expect("the finite baseline time is valid");
    kernel
        .set_continuous_states(&[1.5])
        .expect("the finite baseline state is valid");
    let before = observable_state(&kernel);
    let checkpoint = kernel.fmu_state();
    let non_finite = non_finite_from_index(index);

    let error = kernel
        .set_time(MeTime::at(non_finite))
        .expect_err("a non-finite time must be rejected");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(observable_state(&kernel), before);
    assert!(kernel.verification_matches_snapshot(&checkpoint));

    let error = kernel
        .set_time(MeTime::new(0.25, Some(non_finite)))
        .expect_err("a non-finite event boundary must be rejected");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(observable_state(&kernel), before);
    assert!(kernel.verification_matches_snapshot(&checkpoint));

    let error = kernel
        .set_continuous_states(&[non_finite])
        .expect_err("a non-finite continuous state must be rejected");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(observable_state(&kernel), before);
    assert!(kernel.verification_matches_snapshot(&checkpoint));
}

/// ME-BUF-001 value-reference clause: every bounded contract-validation
/// rejection happens before a valid prefix can update any parameter.
fn property_rejected_value_reference_batch_is_transactional(case: u8) {
    let model = single_state_input_model();
    let mut kernel = instantiate(&model);
    let other = instantiate(&model);
    let valid = kernel
        .value_reference("u")
        .expect("the fixture exposes one input");
    let foreign = other
        .value_reference("u")
        .expect("the second fixture exposes its own input");
    let mut out_of_range = valid.clone();
    out_of_range.index = usize::MAX;
    let before = observable_state(&kernel);
    let checkpoint = kernel.fmu_state();

    let result = match case % 6 {
        0 => kernel.set_float64(&[valid], &[]),
        1 => kernel.set_float64(&[valid], &[f64::NAN]),
        2 => kernel.set_float64(&[valid], &[f64::INFINITY]),
        3 => kernel.set_float64(&[valid], &[f64::NEG_INFINITY]),
        4 => kernel.set_float64(&[valid, foreign], &[2.0, 3.0]),
        5 => kernel.set_float64(&[valid, out_of_range], &[2.0, 3.0]),
        _ => unreachable!("case modulo six is covered above"),
    };
    let error = result.expect_err("the malformed Float64 batch must be rejected");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(observable_state(&kernel), before);
    assert!(kernel.verification_matches_snapshot(&checkpoint));
}

/// ME-BUF-001 buffer clause: invalid bounded host buffers remain untouched,
/// and their rejection cannot mutate component state.
fn property_rejected_host_buffers_are_transactional(case: u8) {
    let model = if case % 9 == 7 {
        single_state_indicator_model()
    } else {
        single_state_model()
    };
    let mut kernel = instantiate(&model);
    let before = observable_state(&kernel);
    let checkpoint = kernel.fmu_state();
    match case % 9 {
        0 => {
            let mut states = vec![7.0, 8.0];
            let error = kernel
                .get_continuous_states(&mut states)
                .expect_err("the oversized state buffer must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(states, vec![7.0, 8.0]);
        }
        1 => {
            let mut sensitivity = [9.0];
            let error = kernel
                .get_directional_derivative(&[], &mut sensitivity)
                .expect_err("the undersized seed must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(sensitivity, [9.0]);
        }
        2 => {
            let mut crossings = vec![MeIndicatorCrossing {
                index: 7,
                post_indicator_value: 1.0,
            }];
            let error = kernel
                .event_indicator_crossings(&[0.0], &[1.0], &mut crossings)
                .expect_err("buffers cannot name an undeclared event indicator");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(crossings.len(), 1);
            assert_eq!(crossings[0].index, 7);
        }
        3 => {
            let error = kernel
                .arm_state_event(&[MeIndicatorCrossing {
                    index: 7,
                    post_indicator_value: 1.0,
                }])
                .expect_err("an undeclared event indicator must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
        }
        4 => {
            let mut nominals = [7.0, 8.0];
            let error = kernel
                .get_nominals_of_continuous_states(&mut nominals)
                .expect_err("the oversized nominal buffer must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(nominals, [7.0, 8.0]);
        }
        5 => {
            let error = kernel
                .set_continuous_states(&[2.0, 3.0])
                .expect_err("the oversized state input must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
        }
        6 => {
            let mut sensitivity = [9.0, 10.0];
            let error = kernel
                .get_directional_derivative(&[1.0], &mut sensitivity)
                .expect_err("the oversized sensitivity buffer must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(sensitivity, [9.0, 10.0]);
        }
        7 => {
            let mut crossings = vec![MeIndicatorCrossing {
                index: 7,
                post_indicator_value: 1.0,
            }];
            let error = kernel
                .event_indicator_crossings(&[f64::NAN], &[1.0], &mut crossings)
                .expect_err("non-finite indicator buffers must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(crossings.len(), 1);
            assert_eq!(crossings[0].index, 7);
        }
        8 => {
            let observation = kernel.observe().expect("the fixture is observable");
            let mut series = MeOutputSeries::default();
            let error = kernel
                .record_outputs(&observation, START_TIME, &mut series)
                .expect_err("an output series with no declared columns must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert!(series.into_columns().is_empty());
        }
        _ => unreachable!("case modulo nine is covered above"),
    }
    assert_eq!(observable_state(&kernel), before);
    assert!(kernel.verification_matches_snapshot(&checkpoint));
}

/// ME-BRAND-001: value references, observations, and component snapshots are
/// unforgeable per-instance capabilities; a foreign capability is rejected
/// before component or host output state changes.
fn property_foreign_instance_capabilities_are_rejected(case: u8) {
    let model = single_state_input_model();
    let first = instantiate(&model);
    let mut second = instantiate(&model);
    let foreign_ref = first
        .value_reference("u")
        .expect("the first fixture exposes one input");
    let foreign_observation = first.observe().expect("the first fixture is observable");
    let foreign_snapshot = first.fmu_state();
    let before = observable_state(&second);
    let checkpoint = second.fmu_state();

    match case % 3 {
        0 => {
            let error = second
                .set_float64(&[foreign_ref], &[2.0])
                .expect_err("a foreign value reference must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
        }
        1 => {
            let mut outputs = vec![7.0];
            let error = second
                .get_outputs(&foreign_observation, START_TIME, &mut outputs)
                .expect_err("a foreign observation must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
            assert_eq!(outputs, vec![7.0]);
        }
        2 => {
            let error = second
                .reset_to_fmu_state(&foreign_snapshot)
                .expect_err("a foreign component snapshot must be rejected");
            assert!(matches!(error.kind(), MeError::Contract { .. }));
        }
        _ => unreachable!("case modulo three is covered above"),
    }
    assert_eq!(observable_state(&second), before);
    assert!(second.verification_matches_snapshot(&checkpoint));
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ActiveFacadeOperation {
    GetStates,
    GetDerivatives,
    GetDirectionalDerivative,
    GetIndicators,
    ProjectStates,
    CompleteStep,
    NextEventStop,
    ClassifyCrossings,
    CapturePreEvent,
    ArmStateEvent,
    Observe,
    RecordOutputs,
    GetOutputs,
    SetFloat64,
    SetTime,
    SetStates,
    ExtendStopTime,
}

impl ActiveFacadeOperation {
    const ALL: [Self; 17] = [
        Self::GetStates,
        Self::GetDerivatives,
        Self::GetDirectionalDerivative,
        Self::GetIndicators,
        Self::ProjectStates,
        Self::CompleteStep,
        Self::NextEventStop,
        Self::ClassifyCrossings,
        Self::CapturePreEvent,
        Self::ArmStateEvent,
        Self::Observe,
        Self::RecordOutputs,
        Self::GetOutputs,
        Self::SetFloat64,
        Self::SetTime,
        Self::SetStates,
        Self::ExtendStopTime,
    ];
}

fn active_operation_from_index(index: u8) -> ActiveFacadeOperation {
    ActiveFacadeOperation::ALL[index as usize % ActiveFacadeOperation::ALL.len()]
}

/// ME-LIFE-003 facade clause: every operation requiring an active component
/// rejects Terminated before it can mutate component state.
fn property_terminated_facade_is_fail_closed(operation: ActiveFacadeOperation) {
    let model = single_state_input_model();
    let mut kernel = instantiate(&model);
    let observation = kernel.observe().expect("the active fixture is observable");
    let value_ref = kernel
        .value_reference("u")
        .expect("the fixture exposes one input");
    kernel.terminate().expect("the fixture can terminate");
    let before = observable_state(&kernel);
    let checkpoint = kernel.fmu_state();
    let mut scalar_buffer = vec![7.0];
    let mut state_buffer = [7.0];
    let mut sensitivity = [7.0];
    let mut output_series =
        MeOutputSeries::with_capacity(1, 1).expect("the proof-sized output series is allocatable");

    let result = match operation {
        ActiveFacadeOperation::GetStates => kernel.get_continuous_states(&mut state_buffer),
        ActiveFacadeOperation::GetDerivatives => {
            kernel.get_continuous_state_derivatives(&mut scalar_buffer)
        }
        ActiveFacadeOperation::GetDirectionalDerivative => {
            kernel.get_directional_derivative(&[1.0], &mut sensitivity)
        }
        ActiveFacadeOperation::GetIndicators => kernel.get_event_indicators(&mut scalar_buffer),
        ActiveFacadeOperation::ProjectStates => kernel
            .project_continuous_states(&mut state_buffer)
            .map(|_| ()),
        ActiveFacadeOperation::CompleteStep => kernel.completed_integrator_step(true).map(|_| ()),
        ActiveFacadeOperation::NextEventStop => kernel.next_event_stop(STOP_TIME).map(|_| ()),
        ActiveFacadeOperation::ClassifyCrossings => {
            kernel.event_indicator_crossings(&[], &[], &mut Vec::new())
        }
        ActiveFacadeOperation::CapturePreEvent => kernel.capture_pre_event_state(),
        ActiveFacadeOperation::ArmStateEvent => kernel.arm_state_event(&[]),
        ActiveFacadeOperation::Observe => kernel.observe().map(|_| ()),
        ActiveFacadeOperation::RecordOutputs => {
            kernel.record_outputs(&observation, START_TIME, &mut output_series)
        }
        ActiveFacadeOperation::GetOutputs => {
            kernel.get_outputs(&observation, START_TIME, &mut scalar_buffer)
        }
        ActiveFacadeOperation::SetFloat64 => kernel.set_float64(&[value_ref], &[2.0]),
        ActiveFacadeOperation::SetTime => kernel.set_time(MeTime::at(0.25)),
        ActiveFacadeOperation::SetStates => kernel.set_continuous_states(&[2.0]),
        ActiveFacadeOperation::ExtendStopTime => kernel.extend_stop_time(0.0, 2.0),
    };
    let error = result.expect_err("Terminated must reject every active-component operation");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(observable_state(&kernel), before);
    assert!(kernel.verification_matches_snapshot(&checkpoint));
}

/// ME-STATE-001 facade clause: restoring an opaque snapshot restores its
/// lifecycle and observable time/state/parameter values exactly, including
/// restoration out of Terminated.
fn property_snapshot_restores_observable_state(target: MeState) {
    let model = single_state_input_model();
    let mut kernel = instantiate(&model);
    let instantiated = kernel.fmu_state();
    if target != MeState::Terminated {
        drive_to_state(&mut kernel, target);
    }
    kernel
        .set_time(MeTime::at(0.375))
        .expect("the saved time is finite");
    kernel
        .set_continuous_states(&[2.0])
        .expect("the saved continuous state is finite");
    let input = kernel
        .value_reference("u")
        .expect("the fixture exposes one input");
    kernel
        .set_float64(&[input], &[3.0])
        .expect("the saved parameter is finite");
    if target == MeState::Terminated {
        kernel.terminate().expect("the saved state can terminate");
    }
    let expected_outputs = active_outputs(&kernel, target);
    let saved = kernel.fmu_state();
    let expected = observable_state(&kernel);
    mutate_away_from_snapshot(&mut kernel, target, &instantiated);
    kernel
        .reset_to_fmu_state(&saved)
        .expect("the same-instance snapshot is restorable");
    assert_eq!(observable_state(&kernel), expected);
    assert!(kernel.verification_matches_snapshot(&saved));
    assert_eq!(active_outputs(&kernel, target), expected_outputs);
    continue_after_restore(&mut kernel, target, &instantiated);
}

fn active_outputs(kernel: &SolveMeKernel, state: MeState) -> Option<Vec<f64>> {
    if state == MeState::Terminated {
        return None;
    }
    let observation = kernel.observe().expect("the active state is observable");
    let mut outputs = Vec::new();
    kernel
        .get_outputs(&observation, observation.time(), &mut outputs)
        .expect("the active outputs are readable");
    Some(outputs)
}

fn mutate_away_from_snapshot(
    kernel: &mut SolveMeKernel,
    target: MeState,
    instantiated: &MeFmuState,
) {
    if target == MeState::Terminated {
        kernel
            .reset_to_fmu_state(instantiated)
            .expect("the same instance can restore its initial snapshot");
        return;
    }
    kernel.set_time(MeTime::at(0.625)).expect("finite mutation");
    kernel
        .set_continuous_states(&[4.0])
        .expect("finite state mutation");
    let input = kernel
        .value_reference("u")
        .expect("the fixture exposes one input");
    kernel
        .set_float64(&[input], &[5.0])
        .expect("finite parameter mutation");
    kernel.terminate().expect("the active state can terminate");
}

fn continue_after_restore(kernel: &mut SolveMeKernel, target: MeState, instantiated: &MeFmuState) {
    match target {
        MeState::Instantiated => kernel
            .enter_initialization_mode()
            .expect("the restored Instantiated continuation is legal"),
        MeState::InitializationMode => kernel
            .exit_initialization_mode()
            .expect("the restored initialization continuation settles"),
        MeState::EventMode => {
            kernel
                .update_discrete_states()
                .expect("the restored pending event continuation settles");
        }
        MeState::ContinuousTimeMode => {
            kernel
                .enter_event_mode(MeEventEntry {
                    cause: MeEventCause::StateEvent,
                    event_time: 0.375,
                    horizon: STOP_TIME,
                })
                .expect("the restored continuous continuation can enter Event Mode");
            kernel
                .update_discrete_states()
                .expect("the restored runtime event continuation settles");
        }
        MeState::Terminated => {
            kernel
                .observe()
                .expect_err("the restored Terminated state remains fail-closed");
            kernel
                .reset_to_fmu_state(instantiated)
                .expect("a saved active snapshot is the only legal exit");
            assert_eq!(
                kernel.verification_observable_state().0,
                MeState::Instantiated
            );
        }
    }
}

/// A conforming FMI host's `while discreteStatesNeedUpdate` loop terminates
/// after one call because the component owns the whole discrete fixed point.
fn property_host_event_iteration_terminates_after_one_call() -> bool {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    run_to_continuous_time_mode(&mut kernel);
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: START_TIME,
            horizon: STOP_TIME,
        })
        .expect("event mode is reachable from continuous-time mode");
    let mut iterations = 0_usize;
    loop {
        iterations += 1;
        assert!(iterations <= HOST_ITERATION_CEILING);
        let states = kernel
            .update_discrete_states()
            .expect("the armed event boundary completes");
        if !states.discrete_states_need_update {
            break;
        }
    }
    let completed_after_one_call = iterations == 1;
    assert!(completed_after_one_call);
    completed_after_one_call
}

/// ME-LIFE-004 time-event clause: the distinct component-scheduled entry path
/// consumes its pending stop and completes in one host update.
fn property_scheduled_time_event_terminates_after_one_call() -> bool {
    let model = single_state_time_event_model();
    let mut kernel = instantiate(&model);
    run_to_continuous_time_mode(&mut kernel);
    let stop = kernel
        .next_event_stop(STOP_TIME)
        .expect("the component can schedule its time event");
    assert!(stop.is_event);
    assert_eq!(stop.time, 0.5);
    kernel
        .set_time(MeTime::at(stop.time))
        .expect("the scheduled time is finite");
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::TimeEvent,
            event_time: stop.time,
            horizon: STOP_TIME,
        })
        .expect("the pending time event can enter Event Mode");
    let states = kernel
        .update_discrete_states()
        .expect("the scheduled time event settles");
    let update_complete = !states.discrete_states_need_update;
    assert!(update_complete);
    assert!(!states.nominals_of_continuous_states_changed);
    let component_time = f64::from_bits(kernel.verification_observable_state().1);
    assert!(component_time >= stop.time);
    update_complete
}

/// The transitional event-entry payload cannot move the component backwards:
/// its numerical right-limit continuation remains at the importer-set time.
fn property_event_boundary_does_not_move_time_backward(earlier: f64, later: f64) {
    let model = single_state_model();
    let mut kernel = instantiate(&model);
    run_to_continuous_time_mode(&mut kernel);
    kernel
        .set_time(MeTime::at(later))
        .expect("the bounded finite time is valid");
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: earlier,
            horizon: STOP_TIME,
        })
        .expect("event mode is reachable from continuous-time mode");
    kernel
        .update_discrete_states()
        .expect("the state event applies");
    assert_eq!(kernel.verification_observable_state().1, later.to_bits());
}

/// A non-convergent initialization fixed point reaches its iteration bound and
/// returns a staged evaluation error instead of looping or panicking.
fn property_non_convergent_fixed_point_returns_error(increment: f64) {
    let model = divergent_initialization_model(increment);
    let mut kernel = instantiate(&model);
    kernel
        .enter_initialization_mode()
        .expect("initialization mode is reachable");
    let error = kernel
        .exit_initialization_mode()
        .expect_err("a divergent fixed point must fail");
    assert_eq!(error.stage(), Some(MeStage::Initialization));
    assert!(matches!(error.kind(), MeError::Evaluation { .. }));
    assert!(error.to_string().contains("did not converge"));
}

/// ME-LIFE-004 bounded-error clause: an ordinary runtime event whose discrete
/// fixed point diverges reaches the component ceiling and returns a staged
/// evaluation error rather than looping, panicking, or claiming completion.
fn property_non_convergent_runtime_event_returns_error(increment: f64) {
    let model = divergent_runtime_event_model(increment);
    let mut kernel = instantiate(&model);
    run_to_continuous_time_mode(&mut kernel);
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: START_TIME,
            horizon: STOP_TIME,
        })
        .expect("the runtime state event is reachable");
    let error = kernel
        .update_discrete_states()
        .expect_err("a divergent runtime event fixed point must fail");
    assert_eq!(error.stage(), Some(MeStage::EventIteration));
    assert!(matches!(error.kind(), MeError::Evaluation { .. }));
    assert!(error.to_string().contains("did not converge"));
}

fn state_from_index(index: u8) -> MeState {
    MeState::ALL[index as usize % MeState::ALL.len()]
}

fn command_from_index(index: u8) -> MeLifecycleCommand {
    MeLifecycleCommand::ALL[index as usize % MeLifecycleCommand::ALL.len()]
}

#[cfg(kani)]
mod proof {
    fn prove_rejection_from(state: super::MeState) {
        let command = super::command_from_index(kani::any());
        kani::cover!(super::relation_rejects(state, command));
        kani::cover!(!super::relation_rejects(state, command));
        super::property_rejected_facade_transition_preserves_state(state, command);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn rejected_instantiated_transition_preserves_state() {
        prove_rejection_from(super::MeState::Instantiated);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn rejected_initialization_transition_preserves_state() {
        prove_rejection_from(super::MeState::InitializationMode);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn rejected_event_transition_preserves_state() {
        prove_rejection_from(super::MeState::EventMode);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn rejected_continuous_transition_preserves_state() {
        prove_rejection_from(super::MeState::ContinuousTimeMode);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn terminated_is_dynamically_absorbing() {
        let command = super::command_from_index(kani::any());
        kani::cover!(super::relation_rejects(super::MeState::Terminated, command));
        super::property_rejected_facade_transition_preserves_state(
            super::MeState::Terminated,
            command,
        );
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn non_finite_setters_are_transactional() {
        let index: u8 = kani::any();
        let value = super::non_finite_from_index(index);
        kani::cover!(value.is_nan());
        kani::cover!(value == f64::INFINITY);
        kani::cover!(value == f64::NEG_INFINITY);
        super::property_non_finite_setters_are_transactional(index);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn rejected_value_reference_batch_is_transactional() {
        let case: u8 = kani::any();
        kani::cover!(case % 6 == 0);
        kani::cover!(case % 6 == 1);
        kani::cover!(case % 6 == 2);
        kani::cover!(case % 6 == 3);
        kani::cover!(case % 6 == 4);
        kani::cover!(case % 6 == 5);
        super::property_rejected_value_reference_batch_is_transactional(case);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn rejected_host_buffers_are_transactional() {
        let case: u8 = kani::any();
        kani::cover!(case % 9 == 0);
        kani::cover!(case % 9 == 1);
        kani::cover!(case % 9 == 2);
        kani::cover!(case % 9 == 3);
        kani::cover!(case % 9 == 4);
        kani::cover!(case % 9 == 5);
        kani::cover!(case % 9 == 6);
        kani::cover!(case % 9 == 7);
        kani::cover!(case % 9 == 8);
        super::property_rejected_host_buffers_are_transactional(case);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn foreign_instance_capabilities_are_rejected() {
        let case: u8 = kani::any();
        kani::cover!(case % 3 == 0);
        kani::cover!(case % 3 == 1);
        kani::cover!(case % 3 == 2);
        super::property_foreign_instance_capabilities_are_rejected(case);
    }

    #[kani::proof]
    #[kani::unwind(20)]
    fn terminated_facade_is_fail_closed() {
        let operation = super::active_operation_from_index(kani::any());
        kani::cover!(operation == super::ActiveFacadeOperation::GetStates);
        kani::cover!(operation == super::ActiveFacadeOperation::GetDerivatives);
        kani::cover!(operation == super::ActiveFacadeOperation::GetDirectionalDerivative);
        kani::cover!(operation == super::ActiveFacadeOperation::GetIndicators);
        kani::cover!(operation == super::ActiveFacadeOperation::ProjectStates);
        kani::cover!(operation == super::ActiveFacadeOperation::CompleteStep);
        kani::cover!(operation == super::ActiveFacadeOperation::NextEventStop);
        kani::cover!(operation == super::ActiveFacadeOperation::ClassifyCrossings);
        kani::cover!(operation == super::ActiveFacadeOperation::CapturePreEvent);
        kani::cover!(operation == super::ActiveFacadeOperation::ArmStateEvent);
        kani::cover!(operation == super::ActiveFacadeOperation::Observe);
        kani::cover!(operation == super::ActiveFacadeOperation::RecordOutputs);
        kani::cover!(operation == super::ActiveFacadeOperation::GetOutputs);
        kani::cover!(operation == super::ActiveFacadeOperation::SetFloat64);
        kani::cover!(operation == super::ActiveFacadeOperation::SetTime);
        kani::cover!(operation == super::ActiveFacadeOperation::SetStates);
        kani::cover!(operation == super::ActiveFacadeOperation::ExtendStopTime);
        super::property_terminated_facade_is_fail_closed(operation);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn snapshot_restores_observable_state() {
        let target = super::state_from_index(kani::any());
        kani::cover!(target == super::MeState::Instantiated);
        kani::cover!(target == super::MeState::InitializationMode);
        kani::cover!(target == super::MeState::EventMode);
        kani::cover!(target == super::MeState::ContinuousTimeMode);
        kani::cover!(target == super::MeState::Terminated);
        super::property_snapshot_restores_observable_state(target);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn host_event_iteration_terminates_after_one_call() {
        let completed_after_one_call =
            super::property_host_event_iteration_terminates_after_one_call();
        kani::cover!(completed_after_one_call);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn scheduled_time_event_terminates_after_one_call() {
        let update_complete = super::property_scheduled_time_event_terminates_after_one_call();
        kani::cover!(update_complete);
    }

    #[kani::proof]
    #[kani::unwind(16)]
    fn event_boundary_does_not_move_time_backward() {
        let earlier: f64 = kani::any();
        let later: f64 = kani::any();
        kani::assume(earlier.is_finite() && later.is_finite());
        kani::assume(earlier >= super::START_TIME && later <= super::STOP_TIME);
        kani::assume(earlier + super::MIN_TIME_GAP <= later);
        kani::cover!(earlier < later);
        super::property_event_boundary_does_not_move_time_backward(earlier, later);
    }

    #[kani::proof]
    #[kani::unwind(64)]
    fn non_convergent_fixed_point_returns_error() {
        let increment: f64 = kani::any();
        kani::assume(increment.is_finite() && increment >= 1.0 && increment <= 4.0);
        kani::cover!(increment > 1.0);
        super::property_non_convergent_fixed_point_returns_error(increment);
    }

    #[kani::proof]
    #[kani::unwind(128)]
    fn non_convergent_runtime_event_returns_error() {
        let increment: f64 = kani::any();
        kani::assume(increment.is_finite() && increment >= 1.0 && increment <= 4.0);
        kani::cover!(increment > 1.0);
        super::property_non_convergent_runtime_event_returns_error(increment);
    }
}

#[cfg(all(test, not(kani)))]
mod fallback {
    use proptest::prelude::*;

    fn any_ordered_times() -> impl Strategy<Value = (f64, f64)> {
        (
            super::START_TIME..=(super::STOP_TIME - super::MIN_TIME_GAP),
            super::MIN_TIME_GAP..=(super::STOP_TIME - super::START_TIME),
        )
            .prop_map(|(earlier, gap)| (earlier, (earlier + gap).min(super::STOP_TIME)))
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(64))]

        #[test]
        fn rejected_facade_transition_preserves_state(
            state_index in any::<u8>(),
            command_index in any::<u8>(),
        ) {
            super::property_rejected_facade_transition_preserves_state(
                super::state_from_index(state_index),
                super::command_from_index(command_index),
            );
        }

        #[test]
        fn non_finite_setters_are_transactional(index in any::<u8>()) {
            super::property_non_finite_setters_are_transactional(index);
        }

        #[test]
        fn rejected_value_reference_batch_is_transactional(case in any::<u8>()) {
            super::property_rejected_value_reference_batch_is_transactional(case);
        }

        #[test]
        fn rejected_host_buffers_are_transactional(case in any::<u8>()) {
            super::property_rejected_host_buffers_are_transactional(case);
        }

        #[test]
        fn foreign_instance_capabilities_are_rejected(case in any::<u8>()) {
            super::property_foreign_instance_capabilities_are_rejected(case);
        }

        #[test]
        fn terminated_facade_is_fail_closed(index in any::<u8>()) {
            super::property_terminated_facade_is_fail_closed(
                super::active_operation_from_index(index),
            );
        }

        #[test]
        fn snapshot_restores_observable_state(state_index in any::<u8>()) {
            super::property_snapshot_restores_observable_state(super::state_from_index(state_index));
        }

        #[test]
        fn event_boundary_does_not_move_time_backward(
            (earlier, later) in any_ordered_times(),
        ) {
            super::property_event_boundary_does_not_move_time_backward(earlier, later);
        }

        #[test]
        fn non_convergent_fixed_point_returns_error(increment in 1.0f64..=4.0f64) {
            super::property_non_convergent_fixed_point_returns_error(increment);
        }

        #[test]
        fn non_convergent_runtime_event_returns_error(increment in 1.0f64..=4.0f64) {
            super::property_non_convergent_runtime_event_returns_error(increment);
        }
    }

    #[test]
    fn host_event_iteration_terminates_after_one_call() {
        assert!(super::property_host_event_iteration_terminates_after_one_call());
    }

    #[test]
    fn scheduled_time_event_terminates_after_one_call() {
        assert!(super::property_scheduled_time_event_terminates_after_one_call());
    }
}
