//! Sequencing tests for the event-entry snapshot the kernel hands to
//! `pre` seeding.
//!
//! A scheduled tick uses the adjacent representable coordinate for its
//! semantic left limit, while a located root carries a tolerance-clearing
//! numerical probe. Both snapshots use the importer's exact event-entry state,
//! but their time-dependent algebraics must be evaluated at the coordinate
//! owned by the respective event cause.

use super::*;
use rumoca_ir_solve as solve;

fn block(rows: Vec<Vec<solve::LinearOp>>, name: &'static str) -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2);
    solve::ScalarProgramBlock::with_source_span(
        rows,
        span.require_provenance("fmi_me event-entry fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

/// Slope of the fixture's algebraic lane.
///
/// It is deliberately steep: a snapshot evaluated at the event's widened left
/// probe instead of the event time differs by ~`ALGEBRAIC_SLOPE * 2 * tol`,
/// which no bit-exact comparison can absorb.
const ALGEBRAIC_SLOPE: f64 = 1.0e6;

#[test]
fn a_coincident_dynamic_deadline_keeps_the_latest_owner_coordinate() {
    let scheduled_time = 0.026_f64.next_down();
    let dynamic_time = 0.026000000000000002_f64;
    let scheduled = RuntimeEventStop::static_event(EventPreMode::EventEntry);
    let dynamic = RuntimeEventStop::dynamic_time_event();

    let (event_time, event) = merge_coincident_event_stops(
        Some((scheduled_time, scheduled)),
        Some((dynamic_time, dynamic)),
    )
    .expect("the coincident owners should form one event");

    assert_eq!(event_time.to_bits(), dynamic_time.to_bits());
    assert_eq!(event.pre_mode, EventPreMode::EventEntry);
    assert!(event.observe_right_limit);
}

/// `der(x) = 1`, `a = ALGEBRAIC_SLOPE * time`, with a time event at `t = 1`.
fn steep_algebraic_time_event_model() -> solve::SolveModel {
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadTime, LoadY, StoreOutput};
    let derivative = block(
        vec![vec![Const { dst: 0, value: 1.0 }, StoreOutput { src: 0 }]],
        "fmi_me_event_entry_derivative.mo",
    );
    let implicit = block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![
                LoadY { dst: 0, index: 1 },
                LoadTime { dst: 1 },
                Const {
                    dst: 2,
                    value: ALGEBRAIC_SLOPE,
                },
                Binary {
                    dst: 3,
                    op: solve::BinaryOp::Mul,
                    lhs: 1,
                    rhs: 2,
                },
                Binary {
                    dst: 4,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                StoreOutput { src: 4 },
            ],
        ],
        "fmi_me_event_entry_implicit.mo",
    );
    let implicit_jvp = block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![LoadSeed { dst: 0, index: 1 }, StoreOutput { src: 0 }],
        ],
        "fmi_me_event_entry_implicit_jvp.mo",
    );
    let derivative_jvp = block(
        vec![vec![Const { dst: 0, value: 0.0 }, StoreOutput { src: 0 }]],
        "fmi_me_event_entry_derivative_jvp.mo",
    );
    crate::fixture_layout::with_visible_expressions(
        solve::SolveModel {
            problem: solve::SolveProblem {
                layout: crate::fixture_layout::named_y_layout(
                    rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 0),
                    vec!["x".to_string(), "a".to_string()],
                ),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: solve::ComputeBlock::from_scalar_program_block(implicit),
                    implicit_row_targets: (vec![
                        Some(solve::scalar_slot_y(0)),
                        Some(solve::scalar_slot_y(1)),
                    ])
                    .into(),
                    derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                        blocks: (vec![solve::AlgebraicProjectionBlock {
                            rows: vec![1],
                            y_indices: vec![1],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        }])
                        .into(),
                    },
                    ..Default::default()
                },
                events: solve::SolveEventPartition {
                    scheduled_time_events: vec![1.0],
                    ..Default::default()
                },
                solve_layout: solve::SolveLayout {
                    state_scalar_count: 1,
                    algebraic_scalar_count: 1,
                    ..Default::default()
                },
                ..Default::default()
            },
            artifacts: solve::SolveArtifacts {
                discrete: Default::default(),
                continuous: solve::ContinuousSolveArtifacts {
                    implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(
                        implicit_jvp.clone(),
                    ),
                    implicit_jacobian_v_scalar: implicit_jvp,
                    full_jacobian_v: derivative_jvp,
                    ..Default::default()
                },
                ..Default::default()
            },
            initial_y: vec![0.0, 0.0].into(),
            solver_nominals: vec![1.0, 1.0],

            ..Default::default()
        },
        vec!["x".to_string(), "a".to_string()],
        Default::default(),
        Vec::new(),
    )
}

fn instantiate(model: &solve::SolveModel) -> SolveMeKernel {
    let mut model = model.clone();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .expect("event-entry fixture refresh owners construct");
    SolveMeKernel::instantiate(
        MeModelSource::fixture(&model),
        &MeInstanceConfig::new("fmi-me-event-entry", 1.0e-4, 0.0, 2.0)
            .expect("event-entry instance configuration constructs"),
    )
    .expect("event-entry fixture instantiates")
}

#[test]
fn scheduled_boundary_preserves_the_continuous_derivative_coordinate() {
    use solve::LinearOp::{Binary, Const, LoadTime, StoreOutput};
    let mut model = steep_algebraic_time_event_model();
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(block(
            vec![vec![
                LoadTime { dst: 0 },
                Const {
                    dst: 1,
                    value: ALGEBRAIC_SLOPE,
                },
                Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                StoreOutput { src: 2 },
            ]],
            "scheduled_derivative_coordinate.mo",
        ));
    let mut kernel = instantiate(&model);
    for time in [0.9, 1.0] {
        kernel.set_time(MeTime::new(time, Some(1.0))).unwrap();
        let mut derivative = [f64::NAN];
        kernel
            .continuous_state_derivatives_into(&mut derivative)
            .unwrap();
        // The left limit of a smooth function agrees with its value at the
        // boundary. Solver state tolerances cannot change physical time.
        assert!(
            (derivative[0] - ALGEBRAIC_SLOPE * time).abs() < 1e-6,
            "at {time}: derivative={}, expected {}",
            derivative[0],
            ALGEBRAIC_SLOPE * time
        );
    }
}

#[test]
fn a_scheduled_event_snapshot_uses_a_one_ulp_left_limit() {
    let mut kernel = instantiate(&steep_algebraic_time_event_model());
    let event_time = 1.0_f64;
    let probe_time = timeline::event_left_probe_time(event_time, kernel.tolerance);
    let left_time = timeline::event_left_limit_time(event_time);
    assert!(
        probe_time < event_time,
        "the fixture's tolerance must widen the left probe away from the event time"
    );
    let settle = kernel.numerics_settle();

    // Reproduce the position a time-event boundary leaves the kernel in: the
    // continuous state is already at the event time, while the retained
    // solver guess was last refreshed at the widened left probe.
    let stale_guess = kernel
        .runtime
        .full_solver_y(
            probe_time,
            &[probe_time],
            &kernel.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the probe-time evaluation succeeds");
    kernel.states = vec![event_time];
    kernel.time = event_time;
    kernel.advance_state_to_event_right_limit = true;
    *kernel.solver_y_guess.borrow_mut() = stale_guess.clone();

    let left = kernel
        .runtime
        .full_solver_y(
            left_time,
            &[event_time],
            &kernel.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the adjacent left-limit evaluation succeeds");
    assert!(
        (left[1] - ALGEBRAIC_SLOPE * left_time).abs() < 1.0e-6,
        "the fixture's algebraic lane tracks time, got {}",
        left[1]
    );
    assert!(
        (stale_guess[1] - left[1]).abs() > 1.0,
        "the retained guess must be distinguishable from the adjacent left limit"
    );

    let (event_pre_y, _) = kernel
        .event_pre_for_update(
            event_time,
            RuntimeEventStop::static_event(EventPreMode::EventEntry),
        )
        .expect("the event-entry snapshot is available");

    assert_eq!(
        event_pre_y.len(),
        left.len(),
        "the snapshot spans the full solver layout"
    );
    for (index, (snapshot, expected)) in event_pre_y.iter().zip(&left).enumerate() {
        assert_eq!(
            snapshot.to_bits(),
            expected.to_bits(),
            "solver lane {index} must be freshly evaluated at the adjacent left limit"
        );
    }
    assert!(
        (event_pre_y[1] - ALGEBRAIC_SLOPE * event_time).abs() < 1.0e-6,
        "a smooth time expression remains numerically at its semantic tick"
    );
    assert!(
        (event_pre_y[1] - stale_guess[1]).abs() > 1.0,
        "a scheduled left limit must not inherit the tolerance-wide root probe"
    );
}

#[test]
fn a_located_root_snapshot_uses_its_left_probe() {
    let mut kernel = instantiate(&steep_algebraic_time_event_model());
    let event_time = 1.0_f64;
    let probe_time = timeline::event_left_probe_time(event_time, kernel.tolerance);
    let settle = kernel.numerics_settle();
    kernel.states = vec![event_time];
    kernel.time = event_time;
    kernel.advance_state_to_event_right_limit = false;
    *kernel.solver_y_guess.borrow_mut() = vec![event_time, -1.0];

    let left = kernel
        .runtime
        .full_solver_y(
            probe_time,
            &[event_time],
            &kernel.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the left-probe evaluation succeeds");
    let exact = kernel
        .runtime
        .full_solver_y(
            event_time,
            &[event_time],
            &kernel.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the event-time evaluation succeeds");

    let (event_pre_y, _) = kernel
        .event_pre_for_update(
            event_time,
            RuntimeEventStop::static_event(EventPreMode::EventEntry),
        )
        .expect("the event-entry snapshot is available");

    assert_eq!(event_pre_y[0].to_bits(), event_time.to_bits());
    assert_eq!(event_pre_y[1].to_bits(), left[1].to_bits());
    assert_ne!(
        event_pre_y[1].to_bits(),
        exact[1].to_bits(),
        "a located root must retain its numerical left-probe coordinate"
    );
}

fn observation_clock_alias_model() -> solve::SolveModel {
    let schedule = solve::PeriodicEventSchedule::from_seconds(0.1, 0.0)
        .expect("the observation fixture has an exact periodic schedule");
    let rhs = block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_observation_clock_alias.mo",
    );
    solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 2),
            discrete: solve::DiscreteSolveSystem {
                rhs,
                update_targets: vec![solve::scalar_slot_p(1)],
                row_roles: vec![solve::DiscreteRowRole::Equation],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![true],
                observation_refresh_reads_y: false,
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
                clock_owners: vec![None],
                ..Default::default()
            },
            clocks: solve::SolveClockPartition {
                periodic_event_schedules: vec![schedule],
                activation_parameter_indices: vec![0],
            },
            solve_layout: solve::SolveLayout {
                parameter_count: 2,
                compiled_parameter_len: 2,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![1.0, 1.0].into(),
        ..Default::default()
    }
}

#[test]
fn public_observation_refreshes_clock_alias_without_mutating_event_state() {
    let event_time = 0.1_f64;
    for observation_time in [event_time.next_down(), event_time, event_time.next_up()] {
        let mut kernel = instantiate(&observation_clock_alias_model());
        kernel.time = observation_time;
        kernel.params = vec![1.0, 1.0];

        let observation = kernel
            .observe()
            .expect("the public clock-alias observation refresh converges");

        assert_eq!(
            observation.parameters,
            vec![0.0, 0.0],
            "public observation sees neither the event-engine clock leaf nor its alias"
        );
        assert_eq!(
            kernel.params,
            vec![1.0, 1.0],
            "observation must not change the canonical event-iteration coordinate"
        );
    }
}

/// Standalone test construction copies its bare model once without granting
/// correlated assertion authority (ME-LIFE-006).
#[test]
fn instantiation_copies_a_scheduleless_model_exactly_once() {
    let mut model = steep_algebraic_time_event_model();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .expect("fixture refresh owners construct");
    let anchored = model
        .resolved_periodic_schedules_at(0.0)
        .expect("a scheduleless model anchors");
    assert!(
        matches!(&anchored, std::borrow::Cow::Borrowed(borrowed) if std::ptr::eq(*borrowed, &model)),
        "anchoring a scheduleless model must not copy it"
    );
    drop(anchored);
    let kernel = SolveMeKernel::instantiate(
        MeModelSource::fixture(&model),
        &MeInstanceConfig::new("fmi-me-single-copy", 1.0e-4, 0.0, 2.0)
            .expect("instance configuration constructs"),
    )
    .expect("fixture instantiates");
    let owned = kernel.runtime.model();
    assert_eq!(owned.initial_y, model.initial_y);
    assert_eq!(owned.visible_names(), model.visible_names());
    assert!(
        !std::ptr::eq(owned.initial_y.as_ptr(), model.initial_y.as_ptr())
            && !std::ptr::eq(
                owned.visible_projection.inputs().as_ptr(),
                model.visible_projection.inputs().as_ptr()
            ),
        "the runtime owns its own copy of the model"
    );
}

#[test]
fn correlated_instantiation_retains_the_exact_scheduleless_root() {
    let mut model = solve::SolveModel::default();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .expect("empty model refresh owners construct");
    let component = solve::fmi::FmiComponent::construct(model, vec![]).unwrap();
    let original = component.runtime_view().retain_assertion_root();
    let kernel = SolveMeKernel::instantiate(
        MeModelSource::new(&component),
        &MeInstanceConfig::new("retained-source", 1.0e-8, 2.0, 3.0).unwrap(),
    )
    .unwrap();
    let root = kernel.runtime.model().retained_assertion_root().unwrap();
    assert!(root.shares_root_owner(&original));
    assert!(std::ptr::eq(root.model(), &**kernel.runtime.model()));
    assert_eq!(
        root.resolved_periodic_schedules_at(5.0).unwrap_err(),
        solve::RetainedAssertionResolutionError::AlreadyResolved
    );
    let standalone = crate::SolveRuntime::new(original.model()).unwrap();
    assert!(standalone.model().retained_assertion_root().is_none());
    let alternate = kernel.runtime.new_alternate(root.model()).unwrap();
    assert!(alternate.model().retained_assertion_root().is_none());
    drop(component);
    assert!(root.shares_root_owner(&original));
}

#[test]
fn correlated_clocked_instances_resolve_from_the_original_component() {
    let mut model = solve::SolveModel::default();
    let lattice = rumoca_core::ClockLattice::from_seconds(0.25, 0.25).unwrap();
    let schedule = rumoca_core::PeriodicClockSchedule::simulation_start_relative(lattice).unwrap();
    model.problem.clocks.periodic_event_schedules =
        vec![solve::PeriodicEventSchedule::from_schedule(schedule).unwrap()];
    model.problem.clocks.activation_parameter_indices = vec![0];
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 0, 1);
    model.parameters = solve::SolveInitialValues::repeat(0.0, 1).unwrap();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .expect("clocked model refresh owners construct");
    let component = solve::fmi::FmiComponent::construct(model, vec![]).unwrap();
    let original = component.runtime_view().retain_assertion_root();
    let instantiate = |start| {
        SolveMeKernel::instantiate(
            MeModelSource::new(&component),
            &MeInstanceConfig::new("clocked-source", 1.0e-8, start, start + 1.0).unwrap(),
        )
        .unwrap()
    };
    let first = instantiate(2.0);
    let second = instantiate(5.0);
    let first_root = first.runtime.model().retained_assertion_root().unwrap();
    let second_root = second.runtime.model().retained_assertion_root().unwrap();
    assert!(!first_root.shares_root_owner(&original));
    assert!(!second_root.shares_root_owner(&original));
    assert!(!first_root.shares_root_owner(second_root));
    for (kernel, root, phase) in [(&first, first_root, 2.25), (&second, second_root, 5.25)] {
        assert!(std::ptr::eq(root.model(), &**kernel.runtime.model()));
        assert_eq!(
            root.model().problem.clocks.periodic_event_schedules[0].phase_seconds(),
            phase
        );
    }
    assert_eq!(
        original.model().problem.clocks.periodic_event_schedules[0].anchor(),
        rumoca_core::ClockPhaseAnchor::SimulationStart
    );
}

#[test]
fn frozen_relation_memory_orients_only_original_exact_zero_indicators() {
    let mut model = steep_algebraic_time_event_model();
    model.problem.events.root_conditions = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_exact_zero_root.mo",
    );
    model.problem.events.root_relation_memory_targets = vec![Some(solve::scalar_slot_p(0))];
    model.problem.events.root_zero_domains = vec![solve::RootZeroDomain::Previous];
    model.problem.events.root_relation_refresh_roles = vec![solve::RootRelationRefreshRole::Frozen];
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.relation_memory_parameter_indices = vec![0];
    model.parameters = vec![0.0].into();
    let mut kernel = instantiate(&model);
    kernel.enter_initialization_mode().unwrap();
    kernel.exit_initialization_mode().unwrap();
    kernel.update_discrete_states().unwrap();
    kernel.enter_continuous_time_mode().unwrap();
    for (state, memory, expected) in [
        (1.0e-12_f64, 1.0, 1.0e-12_f64),
        (-1.0e-12_f64, 0.0, -1.0e-12_f64),
        (0.0, 0.0, f64::EPSILON),
        (0.0, 1.0, -f64::EPSILON),
    ] {
        kernel.states[0] = state;
        kernel.params[0] = memory;
        kernel.clear_runtime_caches();
        let mut values = vec![0.0; kernel.indicator_plan.len()];
        kernel.event_indicators_into(&mut values).unwrap();
        assert_eq!(values[0].to_bits(), expected.to_bits());
        kernel.seed_settled_indicator_domains().unwrap();
        assert_eq!(kernel.frozen_indicator_positive[0], expected > 0.0);
    }
}

#[test]
fn settled_views_and_root_values_require_identical_time_states_and_parameters() {
    let mut model = steep_algebraic_time_event_model();
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![0.0].into();
    let kernel = instantiate(&model);
    let time = 0.25_f64;
    let states = kernel.states.clone();
    let parameters = [0.0_f64];
    let adjacent_parameters = [f64::from_bits(1)];
    let mut adjacent_states = states.clone();
    adjacent_states[0] = f64::from_bits(states[0].to_bits() + 1);
    let complete = kernel.solver_y_at_parameters(time, &parameters).unwrap();
    assert!((complete[1] - ALGEBRAIC_SLOPE * time).abs() < 1e-6);
    kernel.cache_algebraic_coordinate(time, &states, &parameters, &complete);
    kernel.solver_y_guess.borrow_mut()[1] = -123.0;
    let view = kernel
        .cached_continuous_solver_y(time, &states, &parameters)
        .unwrap();
    assert_eq!(view.kind, SettledCoordinateKind::Algebraic);
    assert_eq!(view.values, complete);
    kernel.cache_root_conditions_at_parameters(time, &states, &parameters, &[3.0]);
    for (at, state, params) in [
        (time.next_up(), states.as_slice(), parameters.as_slice()),
        (time, adjacent_states.as_slice(), parameters.as_slice()),
        (time, states.as_slice(), adjacent_parameters.as_slice()),
    ] {
        assert!(
            kernel
                .cached_continuous_solver_y(at, state, params)
                .is_none()
        );
        assert!(!kernel.copy_cached_root_conditions_into(at, state, params, &mut [0.0]));
    }
    assert!(kernel.copy_cached_root_conditions_into(time, &states, &parameters, &mut [0.0]));
}

#[test]
fn a_deadline_read_completes_the_checked_remainder_of_a_partial_derivative_view() {
    let mut model = steep_algebraic_time_event_model();
    model.problem.events.dynamic_time_event_names = vec!["algebraicDeadline".to_owned()];
    model.problem.events.dynamic_time_event_rhs = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_partial_derivative_deadline.mo",
    );
    let mut kernel = instantiate(&model);
    let time = 0.25;
    kernel.set_time(MeTime::at(time)).unwrap();
    kernel
        .continuous_state_derivatives_into(&mut [0.0])
        .unwrap();
    let view = kernel
        .cached_continuous_solver_y(time, &kernel.states, &kernel.params)
        .unwrap();
    assert_eq!(view.kind, SettledCoordinateKind::Derivative);
    assert_eq!(
        view.values[1], 0.0,
        "the derivative does not own the deadline's algebraic lane"
    );
    let mut indicators = vec![0.0; kernel.indicator_plan.len()];
    kernel.event_indicators_into(&mut indicators).unwrap();
    assert_eq!(indicators.len(), 1);
    assert!((indicators[0] - (ALGEBRAIC_SLOPE * time - time)).abs() < 1e-6);
}

#[test]
fn certified_algebraic_delay_history_uses_the_owned_values_after_a_warm_guess_changes() {
    let mut model = steep_algebraic_time_event_model();
    let duration = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.5 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_owned_delay_duration.mo",
    );
    model.problem.events.delays = solve::SolveDelayPartition {
        source_rhs: block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_owned_delay_source.mo",
        ),
        delay_time_rhs: duration.clone(),
        delay_max_rhs: duration,
        value_parameter_indices: vec![0],
        source_is_discrete: vec![false],
    };
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![0.0].into();
    let mut kernel = instantiate(&model);
    kernel
        .runtime
        .initialize_delay_history(0.0, &model.initial_y, &mut kernel.params)
        .unwrap();
    kernel
        .runtime
        .commit_delay_history(0.0, &model.initial_y, &kernel.params)
        .unwrap();
    kernel.time = 0.25;
    let complete = kernel
        .solver_y_at_parameters(kernel.time, &kernel.params)
        .unwrap();
    kernel.cache_algebraic_coordinate(kernel.time, &kernel.states, &kernel.params, &complete);
    kernel.solver_y_guess.borrow_mut()[1] = -123.0;
    kernel.commit_delay_point().unwrap();
    let mut queried = kernel.params.clone();
    kernel
        .runtime
        .refresh_delay_values(0.75, &complete, &mut queried)
        .unwrap();
    assert_eq!(
        queried[0], complete[1],
        "accepted delay history must use the certified coordinate"
    );
}

#[test]
fn certified_algebraic_delay_inputs_use_owned_values_before_duration_evaluation() {
    let mut model = steep_algebraic_time_event_model();
    model.problem.events.delays = solve::SolveDelayPartition {
        source_rhs: block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_owned_dynamic_delay_source.mo",
        ),
        delay_time_rhs: block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: 2.0e-6,
                },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            "fmi_me_owned_dynamic_delay_duration.mo",
        ),
        delay_max_rhs: block(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.5 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_owned_dynamic_delay_max.mo",
        ),
        value_parameter_indices: vec![0],
        source_is_discrete: vec![false],
    };
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![0.0].into();
    let mut kernel = instantiate(&model);
    kernel
        .runtime
        .initialize_delay_history(0.0, &model.initial_y, &mut kernel.params)
        .unwrap();
    kernel
        .runtime
        .commit_delay_history(0.0, &model.initial_y, &kernel.params)
        .unwrap();
    kernel.time = 0.25;
    let complete = kernel
        .solver_y_at_parameters(kernel.time, &kernel.params)
        .unwrap();
    kernel.cache_algebraic_coordinate(kernel.time, &kernel.states, &kernel.params, &complete);
    kernel.solver_y_guess.borrow_mut()[1] = -123.0;
    assert_eq!(
        kernel
            .with_delay_params_unseeded(kernel.time, &kernel.states, |p| p[0])
            .unwrap(),
        0.0
    );
    kernel.commit_delay_point().unwrap();
    assert_eq!(kernel.max_step_duration, Some(0.5));
}
