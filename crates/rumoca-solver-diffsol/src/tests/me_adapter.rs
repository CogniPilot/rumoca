//! Focused behavior-freeze checks for the Diffsol FMI 3 ME adapter.

use super::*;
use rumoca_solver::fmi_me::MeStage;

const CALLBACK_TOLERANCE: f64 = 1.0e-4;
const CALLBACK_MAX_ITERS: usize = 256;

#[test]
fn me_assertion_crosses_the_driver_with_its_kind_and_stage() {
    let recorder = StageRecorder::default();
    let driver_error = staged_sim_to_driver(
        &recorder,
        SimError::AssertionFailed {
            time: 0.25,
            message: "fixture assertion".to_string(),
        }
        .at_stage(SimFailureStage::EventIteration),
    );
    assert!(matches!(
        &driver_error,
        SimDriverError::AssertionFailed { time, message }
            if time.to_bits() == 0.25f64.to_bits() && message == "fixture assertion"
    ));

    let returned = stage_driver_failure(&recorder, driver_error);
    assert!(matches!(
        returned.kind(),
        SimError::AssertionFailed { time, message }
            if time.to_bits() == 0.25f64.to_bits() && message == "fixture assertion"
    ));
    assert_eq!(returned.stage(), Some(SimFailureStage::EventIteration));
}

#[test]
fn me_runtime_contract_crosses_the_driver_with_its_kind_and_stage() {
    let recorder = StageRecorder::default();
    let driver_error = staged_sim_to_driver(
        &recorder,
        SimError::RuntimeContract {
            reason: "fixture contract".to_string(),
        }
        .at_stage(SimFailureStage::ManifoldProjection),
    );
    assert!(matches!(
        &driver_error,
        SimDriverError::RuntimeContract { reason } if reason == "fixture contract"
    ));

    let returned = stage_driver_failure(&recorder, driver_error);
    assert!(matches!(
        returned.kind(),
        SimError::RuntimeContract { reason } if reason == "fixture contract"
    ));
    assert_eq!(returned.stage(), Some(SimFailureStage::ManifoldProjection));
}

/// SPEC_0038 phase 2 is behavior-freezing: rejected Diffsol trial points may
/// project algebraics speculatively, but only an accepted step may replace the
/// algebraic warm start used by later callbacks.
///
/// The nonlinear projection makes that policy observable. At this tolerance,
/// solving `z^2 = 9` from the accepted `z = 1` and from a leaked trial near
/// `z = 2` stops at different floating-point values.
#[test]
fn me_rhs_callbacks_preserve_frozen_algebraic_seed_and_commit_policy() {
    let model = nonlinear_projection_backed_derivative_model();
    require_state_only_bdf(&model).expect("fixture must use the reduced state-only path");
    let opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        atol: CALLBACK_TOLERANCE,
        ..Default::default()
    };

    let runtime = SolveRuntime::new(&model).expect("fixture runtime should prepare");
    let equilibrium_model = OdeModel::new(&model).expect("fixture ODE projection should prepare");
    let mut accepted_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    let mut time = opts.t_start;
    initialize_state_runtime_values(
        &model,
        &opts,
        &runtime,
        &equilibrium_model,
        &mut accepted_y,
        &mut params,
        &mut time,
    )
    .expect("state-only initialization should settle the accepted full solver vector");

    let host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(&model), &opts)
        .expect("ME host should instantiate");
    host.initialize(&accepted_y, &params)
        .expect("ME initialization should match the frozen full solver vector");

    let (expected_at_four, accepted_at_four) =
        frozen_rhs_trial(&runtime, &accepted_y, &params, time, 4.0);
    let (expected_at_nine, _) = frozen_rhs_trial(&runtime, &accepted_y, &params, time, 9.0);

    let actual_at_four = host_rhs(&host, time, 4.0);
    assert_eq!(
        actual_at_four.to_bits(),
        expected_at_four.to_bits(),
        "the first ME callback must use the frozen Diffsol initialization seed and tolerance"
    );

    // Diffsol may reject this x=4 trial. Its projected z must therefore not
    // become the seed for the following x=9 callback.
    let actual_at_nine = host_rhs(&host, time, 9.0);
    assert_eq!(
        actual_at_nine.to_bits(),
        expected_at_nine.to_bits(),
        "a rejected callback trial must not mutate the accepted algebraic warm start"
    );

    let projected = host
        .accept_continuous_step(time, &[4.0], &accepted_at_four, &params)
        .expect("accepted step should commit its algebraic projection");
    assert_eq!(projected[0].to_bits(), 4.0f64.to_bits());

    let (expected_after_accept, _) =
        frozen_rhs_trial(&runtime, &accepted_at_four, &params, time, 9.0);
    let actual_after_accept = host_rhs(&host, time, 9.0);
    assert_eq!(
        actual_after_accept.to_bits(),
        expected_after_accept.to_bits(),
        "the accepted x=4 step must seed the next callback"
    );
    assert_ne!(
        actual_after_accept.to_bits(),
        actual_at_nine.to_bits(),
        "fixture must distinguish the accepted z≈2 seed from the original z=1 seed"
    );
}

/// SPEC_0038 phase 2 freezes the retired driver's event-vector ownership.
/// When a located root is tolerance-equal to an output target just before it,
/// the event clock snaps to the target but the driver's non-state lanes still
/// come from the located full vector. A strict condition can distinguish those
/// two floating-point instants even though the projection accepts either seed.
#[test]
fn frozen_me_state_event_keeps_located_nonstate_lanes_when_time_snaps_back() {
    const HORIZON: f64 = 0.22;
    const LOCATED_ROOT: f64 = 0.220_000_000_000_002_5;
    const THRESHOLD: f64 = 2.000_009_079_934_896;

    let model = strict_algebraic_threshold_condition_memory_model();
    let opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        atol: CALLBACK_TOLERANCE,
        ..Default::default()
    };
    const { assert!(LOCATED_ROOT > HORIZON) };

    let runtime = SolveRuntime::new(&model).expect("fixture runtime should prepare");
    let equilibrium_model = OdeModel::new(&model).expect("fixture ODE projection should prepare");
    let mut accepted_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    let mut time = opts.t_start;
    initialize_state_runtime_values(
        &model,
        &opts,
        &runtime,
        &equilibrium_model,
        &mut accepted_y,
        &mut params,
        &mut time,
    )
    .expect("state-only initialization should settle the accepted full solver vector");

    let mut located_solver_y = accepted_y.clone();
    runtime
        .full_solver_y_with_guess(
            LOCATED_ROOT,
            &accepted_y[..1],
            &params,
            &mut located_solver_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("the located full vector should project");
    let mut target_solver_y = accepted_y.clone();
    runtime
        .full_solver_y_with_guess(
            HORIZON,
            &accepted_y[..1],
            &params,
            &mut target_solver_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("the exact-target full vector should project");
    assert!(target_solver_y[1] <= THRESHOLD);
    assert!(located_solver_y[1] > THRESHOLD);

    let right_time = runtime_root_event_application_time(LOCATED_ROOT, HORIZON, CALLBACK_TOLERANCE);
    assert_eq!(right_time.to_bits(), HORIZON.to_bits());

    let host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(&model), &opts)
        .expect("ME host should instantiate");
    host.initialize(&accepted_y, &params)
        .expect("ME initialization should preserve the frozen seed");
    let root_states = [LOCATED_ROOT];
    let event = host
        .process_state_event(LOCATED_ROOT, 0, &root_states, right_time, HORIZON)
        .expect("the snapped state event should settle");
    assert_eq!(event.time.to_bits(), HORIZON.to_bits());

    // Mirror the accepted-reset seam: the frozen driver's warm start is
    // rebuilt at the snapped time, while its event-updated parameters retain
    // the strict condition observed from the located vector.
    let expected_params = [1.0];
    let mut frozen_solver_y = accepted_y;
    runtime
        .full_solver_y_with_guess(
            HORIZON,
            &event.states,
            &expected_params,
            &mut frozen_solver_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("the frozen accepted vector should rebuild at the snapped time");
    assert!(
        frozen_solver_y[1] <= THRESHOLD,
        "target={} located={}",
        frozen_solver_y[1],
        located_solver_y[1]
    );
    host.sync_continuous_point(HORIZON, &event.states)
        .expect("the component should accept the reset point");
    host.prepare_integrator_initial_seed(&frozen_solver_y, MeStage::EventIteration)
        .expect("the accepted frozen seed should transfer");
    host.verify_frozen_compatibility_state(
        &frozen_solver_y,
        &expected_params,
        MeStage::EventIteration,
    )
    .expect("the located strict condition must survive the snapped event application");
}

/// An ordinary state event is evaluated where the host actually positioned
/// the component.  When a tolerance-equal root is just beyond the requested
/// horizon, the frozen driver snaps the application point back to the horizon;
/// evaluating the component's discrete row at the semantic root instead would
/// put a strict time relation on the opposite side of its boundary.
#[test]
fn frozen_me_state_event_uses_snapped_host_time_for_discrete_rows() {
    const HORIZON: f64 = 0.215;
    const LOCATED_ROOT: f64 = 0.215_000_000_000_000_02;

    let model = snapped_time_condition_memory_model(HORIZON, LOCATED_ROOT);

    let opts = SimOptions {
        t_start: 0.0,
        t_end: HORIZON,
        atol: CALLBACK_TOLERANCE,
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("fixture runtime should prepare");
    let equilibrium_model = OdeModel::new(&model).expect("fixture ODE should prepare");
    let mut accepted_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    let mut time = opts.t_start;
    initialize_state_runtime_values(
        &model,
        &opts,
        &runtime,
        &equilibrium_model,
        &mut accepted_y,
        &mut params,
        &mut time,
    )
    .expect("state-only initialization should settle");

    let right_time = runtime_root_event_application_time(LOCATED_ROOT, HORIZON, CALLBACK_TOLERANCE);
    assert_eq!(right_time.to_bits(), HORIZON.to_bits());
    let host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(&model), &opts)
        .expect("ME host should instantiate");
    host.initialize(&accepted_y, &params)
        .expect("ME initialization should preserve the frozen seed");
    let event = host
        .process_state_event(LOCATED_ROOT, 0, &[LOCATED_ROOT], right_time, HORIZON)
        .expect("snapped state event should settle at the host-owned time");
    let pre_flag = event
        .pre_observation
        .as_ref()
        .expect("state event should preserve its left limit")
        .values[1];
    let post_flag = event
        .observation
        .as_ref()
        .expect("state event should expose its settled right limit")
        .values[1];
    assert_eq!(
        pre_flag, 0.0,
        "left-limit observation must precede the update"
    );
    assert_eq!(
        post_flag, 1.0,
        "right-limit observation must include the update"
    );

    let expected_params = [1.0];
    let mut frozen_solver_y = accepted_y;
    runtime
        .full_solver_y_with_guess(
            HORIZON,
            &event.states,
            &expected_params,
            &mut frozen_solver_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("frozen accepted vector should rebuild at the horizon");
    host.prepare_integrator_initial_seed(&frozen_solver_y, MeStage::EventIteration)
        .expect("accepted frozen seed should transfer");
    host.verify_frozen_compatibility_state(
        &frozen_solver_y,
        &expected_params,
        MeStage::EventIteration,
    )
    .expect("ordinary rows must use the host-owned snapped event time");
}

fn snapped_time_condition_memory_model(horizon: f64, located_root: f64) -> solve::SolveModel {
    let mut model = unit_integrator_model();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["flag".to_string()];
    model.parameters = vec![0.0];
    model.visible_names.push("flag".to_string());
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: located_root,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: horizon,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    ordinary_equation_row_metadata(&mut model);
    model
}

/// The positive-`dt` located-root path owns one hybrid event-entry vector: its
/// continuous-state prefix is the left bracket, but its algebraic lanes are
/// the full vector Diffsol reconstructed at the located root. Rebuilding every
/// lane at the semantic left probe would put this falling alias on the other
/// side of the strict condition and retain the wrong condition memory.
#[test]
fn frozen_me_positive_dt_state_event_keeps_located_nonstate_lanes() {
    const ROOT_TIME: f64 = 0.5;
    const HORIZON: f64 = 1.0;
    const THRESHOLD: f64 = 2.000_041;

    let model = falling_algebraic_threshold_condition_memory_model(THRESHOLD);
    let opts = SimOptions {
        t_start: 0.0,
        t_end: HORIZON,
        atol: CALLBACK_TOLERANCE,
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("fixture runtime should prepare");
    let equilibrium_model = OdeModel::new(&model).expect("fixture ODE projection should prepare");
    let mut accepted_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    let mut time = opts.t_start;
    initialize_state_runtime_values(
        &model,
        &opts,
        &runtime,
        &equilibrium_model,
        &mut accepted_y,
        &mut params,
        &mut time,
    )
    .expect("state-only initialization should settle the accepted full solver vector");
    assert_eq!(params, vec![1.0], "the falling condition must start true");

    let root_states = [ROOT_TIME];
    let mut located_y = accepted_y.clone();
    runtime
        .full_solver_y_with_guess(
            ROOT_TIME,
            &root_states,
            &params,
            &mut located_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("the located full vector should project");
    let left_time = rumoca_solver::timeline::event_left_probe_time(ROOT_TIME, CALLBACK_TOLERANCE);
    let mut rebuilt_left_y = accepted_y.clone();
    runtime
        .full_solver_y_with_guess(
            left_time,
            &root_states,
            &params,
            &mut rebuilt_left_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("the left-probe full vector should project");
    assert!(located_y[1] <= THRESHOLD, "located alias={}", located_y[1]);
    assert!(
        rebuilt_left_y[1] > THRESHOLD,
        "left-probe alias={}",
        rebuilt_left_y[1]
    );

    let right_time = runtime_root_event_application_time(ROOT_TIME, HORIZON, CALLBACK_TOLERANCE);
    assert!(right_time > ROOT_TIME, "fixture must exercise positive dt");
    let host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(&model), &opts)
        .expect("ME host should instantiate");
    host.initialize(&accepted_y, &params)
        .expect("ME initialization should preserve the frozen seed");
    let event = host
        .process_state_event(ROOT_TIME, 0, &root_states, right_time, HORIZON)
        .expect("the positive-dt state event should settle");
    assert_eq!(event.time.to_bits(), right_time.to_bits());

    let expected_params = [0.0];
    let mut frozen_solver_y = accepted_y;
    runtime
        .full_solver_y_with_guess(
            right_time,
            &event.states,
            &expected_params,
            &mut frozen_solver_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("the accepted frozen vector should rebuild at the right limit");
    host.prepare_integrator_initial_seed(&frozen_solver_y, MeStage::EventIteration)
        .expect("the accepted frozen seed should transfer");
    host.verify_frozen_compatibility_state(
        &frozen_solver_y,
        &expected_params,
        MeStage::EventIteration,
    )
    .expect("condition memory must come from the located non-state lane");
}

/// A coincident periodic tick and located root form one ordered superdense
/// transition. The clock owner executes at the semantic tick; the frozen
/// Diffsol profile accepts the numerical right-limit coordinate without
/// physically integrating the exact post-clock state to that probe.
#[test]
fn frozen_coincident_clock_root_preserves_the_exact_post_clock_state() {
    const ROOT_TIME: f64 = 0.05;
    const HORIZON: f64 = 0.1;

    let model = super::root_events::clock_owned_sample_with_coincident_root();
    let opts = SimOptions {
        t_start: 0.0,
        t_end: HORIZON,
        atol: CALLBACK_TOLERANCE,
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("fixture runtime should prepare");
    let equilibrium_model = OdeModel::new(&model).expect("fixture ODE should prepare");
    let mut accepted_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    let mut time = opts.t_start;
    initialize_state_runtime_values(
        &model,
        &opts,
        &runtime,
        &equilibrium_model,
        &mut accepted_y,
        &mut params,
        &mut time,
    )
    .expect("state-only initialization should settle");

    let host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(&model), &opts)
        .expect("ME host should instantiate");
    host.initialize(&accepted_y, &params)
        .expect("ME initialization should preserve the frozen seed");
    let right_time = runtime_root_event_application_time(ROOT_TIME, HORIZON, CALLBACK_TOLERANCE);
    assert!(
        right_time > ROOT_TIME,
        "fixture requires a positive right limit"
    );
    let event = host
        .process_state_event(ROOT_TIME, 0, &[ROOT_TIME], right_time, HORIZON)
        .expect("coincident clock/root transition should settle");

    assert_eq!(event.time.to_bits(), right_time.to_bits());
    assert_eq!(
        event.states[0].to_bits(),
        0.0_f64.to_bits(),
        "the right-limit ordering coordinate must not Euler-advance the post-reinit state"
    );
}

/// The root-classification probe is wider than the semantic event boundary.
/// When a located root coincides with a typed tick, that probe may classify
/// relation memory but cannot replace the continuous state owned by the shared
/// event entry.
#[test]
fn frozen_clock_root_samples_event_entry_state() {
    const ROOT_TIME: f64 = 0.05;
    const HORIZON: f64 = 0.1;

    let model = super::root_events::clock_owned_sample_with_coincident_root();
    let opts = SimOptions {
        t_start: 0.0,
        t_end: HORIZON,
        atol: CALLBACK_TOLERANCE,
        ..Default::default()
    };
    let host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(&model), &opts)
        .expect("ME host should instantiate");
    host.initialize_component()
        .expect("ME component should initialize");
    let right_time = runtime_root_event_application_time(ROOT_TIME, HORIZON, CALLBACK_TOLERANCE);
    let event = host
        .process_state_event(ROOT_TIME, 0, &[ROOT_TIME], right_time, HORIZON)
        .expect("nearby clock/root transition should settle");
    let y_last = event
        .observation
        .expect("event should be observable")
        .values[1];
    let expected = 100.0 * ROOT_TIME;
    assert!(
        (y_last - expected).abs() <= 1.0e-7,
        "clock pre-state widened to the numerical root probe: expected {expected}, got {y_last}"
    );
}

fn host_rhs(host: &DiffsolMeHost, time: f64, state: f64) -> f64 {
    let mut out = [0.0];
    host.derivatives_into(time, &[state], &mut out);
    if let Some(error) = host.take_callback_error() {
        panic!("ME derivative callback failed: {error}");
    }
    out[0]
}

fn frozen_rhs_trial(
    runtime: &SolveRuntime,
    accepted_y: &[f64],
    params: &[f64],
    time: f64,
    state: f64,
) -> (f64, Vec<f64>) {
    let mut speculative_y = accepted_y.to_vec();
    let values = runtime
        .eval_state_derivatives_with_guess(
            time,
            &[state],
            params,
            &mut speculative_y,
            CALLBACK_TOLERANCE,
            CALLBACK_MAX_ITERS,
        )
        .expect("frozen Diffsol RHS trial should settle its algebraic dependency");
    (values[0], speculative_y)
}

fn nonlinear_projection_backed_derivative_model() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "z".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("z".to_string(), 1)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        indexmap::IndexMap::from([("x".to_string(), vec![0]), ("z".to_string(), vec![1])]);

    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        scalar_program_block!(vec![load_y_row(1)], fixture_span!()),
    );
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        scalar_program_block!(vec![zero_row(), z_squared_minus_x_row()], fixture_span!()),
    );
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
        }],
    };

    let implicit_jvp =
        scalar_program_block!(vec![zero_row(), z_squared_jvp_row()], fixture_span!(),);
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(implicit_jvp.clone());
    model.artifacts.continuous.implicit_jacobian_v_scalar = implicit_jvp;
    model.artifacts.continuous.full_jacobian_v =
        scalar_program_block!(vec![load_seed_row(1)], fixture_span!());
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.initial_y = vec![1.0, 1.0];
    model
}

fn strict_algebraic_threshold_condition_memory_model() -> solve::SolveModel {
    strict_algebraic_condition_memory_model(flow_residual_row(), 2.000_009_079_934_896)
}

fn falling_algebraic_threshold_condition_memory_model(threshold: f64) -> solve::SolveModel {
    strict_algebraic_condition_memory_model(time_varying_flow_residual_row(0.5, -0.1), threshold)
}

fn strict_algebraic_condition_memory_model(
    flow_residual: Vec<solve::LinearOp>,
    threshold: f64,
) -> solve::SolveModel {
    let mut model = unit_integrator_model();
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model
        .problem
        .solve_layout
        .solver_maps
        .names
        .push("flow".to_string());
    model
        .problem
        .solve_layout
        .solver_maps
        .name_to_idx
        .insert("flow".to_string(), 1);
    model
        .problem
        .solve_layout
        .solver_maps
        .base_to_indices
        .insert("flow".to_string(), vec![1]);

    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        scalar_program_block!(vec![state_residual_row(), flow_residual], fixture_span!()),
    );
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
        }],
    };
    let implicit_jvp = scalar_program_block!(
        vec![zero_row(), z_squared_minus_x_jvp_row()],
        fixture_span!(),
    );
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(implicit_jvp.clone());
    model.artifacts.continuous.implicit_jacobian_v_scalar = implicit_jvp;

    model.problem.events.root_conditions = scalar_program_block!(
        vec![strict_threshold_residual_row(threshold)],
        fixture_span!(),
    );
    model.problem.events.root_relation_memory_targets = vec![None];
    model.problem.events.condition_memory_parameter_indices = vec![0];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = scalar_program_block!(
        vec![strict_threshold_condition_row(threshold)],
        fixture_span!(),
    );
    model.problem.discrete.row_roles = vec![solve::DiscreteRowRole::ConditionMemory];
    model.problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::FollowCurrent];
    model.problem.discrete.observation_refresh = vec![false];
    model.problem.discrete.clock_owners = vec![None];

    model.initial_y.push(1.0);
    model.solver_nominals.push(1.0);
    model.parameters = vec![0.0];
    model
}

fn flow_residual_row() -> Vec<solve::LinearOp> {
    time_varying_flow_residual_row(0.22, 10.0)
}

fn time_varying_flow_residual_row(anchor: f64, slope: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Binary {
            dst: 1,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        solve::LinearOp::LoadTime { dst: 2 },
        solve::LinearOp::Const {
            dst: 3,
            value: anchor,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Const {
            dst: 5,
            value: slope,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Mul,
            lhs: 5,
            rhs: 4,
        },
        solve::LinearOp::Const { dst: 7, value: 4.0 },
        solve::LinearOp::Binary {
            dst: 8,
            op: solve::BinaryOp::Add,
            lhs: 7,
            rhs: 6,
        },
        solve::LinearOp::Binary {
            dst: 9,
            op: solve::BinaryOp::Sub,
            lhs: 1,
            rhs: 8,
        },
        solve::LinearOp::StoreOutput { src: 9 },
    ]
}

fn strict_threshold_residual_row(threshold: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const {
            dst: 1,
            value: threshold,
        },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn strict_threshold_condition_row(threshold: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const {
            dst: 1,
            value: threshold,
        },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn z_squared_jvp_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadSeed { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 2,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn zero_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn load_y_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn load_seed_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadSeed { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn z_squared_minus_x_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Binary {
            dst: 1,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        solve::LinearOp::LoadY { dst: 2, index: 0 },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Sub,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn z_squared_minus_x_jvp_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadSeed { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 2,
            rhs: 2,
        },
        solve::LinearOp::LoadSeed { dst: 4, index: 0 },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Sub,
            lhs: 3,
            rhs: 4,
        },
        solve::LinearOp::StoreOutput { src: 5 },
    ]
}
