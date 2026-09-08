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

use crate::test_support::empty_binary64_first_product_model;

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

struct SteepAlgebraicSystems {
    solve_layout: solve::SolveLayout,
    continuous: solve::ContinuousSolveSystem,
    discrete: solve::DiscreteSolveSystem,
    events: solve::SolveEventPartition,
    clocks: solve::SolveClockPartition,
}

fn steep_algebraic_implicit_program() -> solve::ScalarProgramBlock {
    use solve::LinearOp::{Binary, Const, LoadTime, LoadY, StoreOutput};

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_event_entry_implicit.mo"),
        1,
        2,
    )
    .require_provenance("fmi_me event-entry implicit fixture")
    .expect("fixture span is source-backed");
    solve::ScalarProgramBlock::with_output_indices(
        vec![vec![
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
        ]],
        vec![span.into()],
        vec![1],
    )
    .expect("event-entry implicit program is computable")
}

fn steep_algebraic_systems(
    implicit: solve::ScalarProgramBlock,
    derivative: solve::ScalarProgramBlock,
) -> SteepAlgebraicSystems {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "a".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let events = solve::SolveEventPartition {
        root_conditions: block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::LoadTime { dst: 1 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            "fmi_me_event_entry_root.mo",
        ),
        root_relation_memory_targets: vec![None],
        root_zero_domains: vec![solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
        scheduled_time_events: vec![1.0],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(implicit),
        implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    SteepAlgebraicSystems {
        solve_layout,
        continuous,
        discrete,
        events,
        clocks,
    }
}

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
    use solve::LinearOp::{Const, LoadSeed, StoreOutput};

    let derivative = block(
        vec![vec![Const { dst: 0, value: 1.0 }, StoreOutput { src: 0 }]],
        "fmi_me_event_entry_derivative.mo",
    );
    let implicit = steep_algebraic_implicit_program();
    let implicit_jvp_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_event_entry_implicit_jvp.mo"),
        1,
        2,
    );
    let implicit_jvp = solve::ScalarProgramBlock::with_output_indices(
        vec![vec![LoadSeed { dst: 0, index: 1 }, StoreOutput { src: 0 }]],
        vec![implicit_jvp_span],
        vec![1],
    )
    .expect("event-entry implicit JVP matches the sparse implicit row owner");
    let derivative_jvp = block(
        vec![vec![Const { dst: 0, value: 0.0 }, StoreOutput { src: 0 }]],
        "fmi_me_event_entry_derivative_jvp.mo",
    );
    let SteepAlgebraicSystems {
        solve_layout,
        continuous,
        discrete,
        events,
        clocks,
    } = steep_algebraic_systems(implicit, derivative);
    let model = crate::test_support::custom_artifact_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("event-entry fixture satisfies the checked root contract"),
        artifacts: solve::SolveArtifactInputs {
            continuous: solve::ContinuousSolveArtifacts {
                implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(
                    implicit_jvp.clone(),
                ),
                implicit_jacobian_v_scalar: implicit_jvp,
                full_jacobian_v: derivative_jvp,
                ..Default::default()
            },
            ..solve::SolveArtifactInputs::empty()
        },
        initial_y: vec![0.0, 0.0],
        solver_nominals: vec![1.0, 1.0],
        ..empty_binary64_first_product_model()
    };
    with_steep_algebraic_event_catalog(model)
}

/// Attach the explicit Real scalar catalog for the steep-algebraic event
/// model. Split out for length; every fixture field is unchanged.
fn with_steep_algebraic_event_catalog(model: solve::SolveModel) -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_event_entry_variables.mo"),
        1,
        2,
    );
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![
            crate::test_support::RealScalarVariableFixture {
                source_occurrence: crate::test_support::fixture_source_occurrence(1),
                name: "x".to_string(),
                storage: solve::scalar_slot_y(0),
                role: solve::SolveVariableStorageRole::State,
                causality: solve::SolveVariableCausality::Local,
                variability: solve::SolveVariableVariability::Continuous,
                fixed: rumoca_core::Fixity::Fixed,
                start: 0.0,
                nominal: Some(1.0),
                provenance,
            },
            crate::test_support::RealScalarVariableFixture {
                source_occurrence: crate::test_support::fixture_source_occurrence(2),
                name: "a".to_string(),
                storage: solve::scalar_slot_y(1),
                role: solve::SolveVariableStorageRole::Algebraic,
                causality: solve::SolveVariableCausality::Local,
                variability: solve::SolveVariableVariability::Continuous,
                fixed: rumoca_core::Fixity::Free,
                start: 0.0,
                nominal: Some(1.0),
                provenance,
            },
        ],
        block(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 1 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            "fmi_me_event_entry_visible.mo",
        ),
    )
}

fn instantiate(model: solve::SolveModel) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &MeInstanceConfig::new("fmi-me-event-entry", 1.0e-4, 0.0, 2.0)
            .expect("event-entry instance configuration constructs"),
    )
    .expect("event-entry fixture instantiates")
}

#[test]
fn a_scheduled_event_snapshot_uses_a_one_ulp_left_limit() {
    let mut kernel = instantiate(steep_algebraic_time_event_model());
    let event_time = 1.0_f64;
    let probe_time = timeline::event_left_probe_time(event_time, kernel.body.tolerance);
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
        .body
        .runtime
        .full_solver_y(
            probe_time,
            &[probe_time],
            &kernel.body.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the probe-time evaluation succeeds");
    kernel.body.states = vec![event_time];
    kernel.body.time = event_time;
    kernel.body.advance_state_to_event_right_limit = true;
    *kernel.body.solver_y_guess.borrow_mut() = stale_guess.clone();

    let left = kernel
        .body
        .runtime
        .full_solver_y(
            left_time,
            &[event_time],
            &kernel.body.params,
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

    kernel
        .body
        .prepare_event_pre_for_update(
            event_time,
            RuntimeEventStop::static_event(EventPreMode::EventEntry),
        )
        .expect("the event-entry snapshot is available");
    let event_pre_y = kernel
        .body
        .pending_event_pre_y
        .get("test event-entry snapshot")
        .expect("test latch")
        .to_vec();

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
    let mut kernel = instantiate(steep_algebraic_time_event_model());
    let event_time = 1.0_f64;
    let probe_time = timeline::event_left_probe_time(event_time, kernel.body.tolerance);
    let settle = kernel.numerics_settle();
    kernel.body.states = vec![event_time];
    kernel.body.time = event_time;
    kernel.body.advance_state_to_event_right_limit = false;
    *kernel.body.solver_y_guess.borrow_mut() = vec![event_time, -1.0];

    let left = kernel
        .body
        .runtime
        .full_solver_y(
            probe_time,
            &[event_time],
            &kernel.body.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the left-probe evaluation succeeds");
    let exact = kernel
        .body
        .runtime
        .full_solver_y(
            event_time,
            &[event_time],
            &kernel.body.params,
            settle.tol,
            settle.max_iters,
        )
        .expect("the event-time evaluation succeeds");

    kernel
        .body
        .prepare_event_pre_for_update(
            event_time,
            RuntimeEventStop::static_event(EventPreMode::EventEntry),
        )
        .expect("the event-entry snapshot is available");
    let event_pre_y = kernel
        .body
        .pending_event_pre_y
        .get("test event-entry snapshot")
        .expect("test latch")
        .to_vec();

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
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 2,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem {
        rhs,
        update_targets: vec![solve::scalar_slot_p(1)],
        row_roles: vec![solve::DiscreteRowRole::EventAction],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
        observation_refresh: vec![true],
        observation_refresh_reads_y: false,
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
        clock_owners: vec![None],
        ..Default::default()
    };
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition {
        periodic_event_schedules: vec![schedule],
        activation_parameter_indices: vec![0],
    };
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 2),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("observation fixture satisfies the checked root contract"),
        parameters: vec![1.0, 1.0],
        ..empty_binary64_first_product_model()
    }
}

#[test]
fn private_observation_refreshes_clock_alias_without_mutating_event_state() {
    let event_time = 0.1_f64;
    for observation_time in [event_time.next_down(), event_time, event_time.next_up()] {
        let mut kernel = instantiate(observation_clock_alias_model());
        kernel.body.time = observation_time;
        kernel.body.params = vec![1.0, 1.0];

        let (_, observed_parameters) = kernel
            .body
            .observation_coordinate()
            .expect("the private clock-alias observation refresh converges");

        assert_eq!(
            observed_parameters,
            vec![0.0, 0.0],
            "public observation sees neither the event-engine clock leaf nor its alias"
        );
        assert_eq!(
            kernel.body.params,
            vec![1.0, 1.0],
            "observation must not change the canonical event-iteration coordinate"
        );
    }
}

#[test]
fn snapshot_oracle_detects_a_frozen_indicator_domain_bit_ablation() {
    let mut kernel = instantiate(steep_algebraic_time_event_model());
    kernel
        .enter_initialization_mode(0.0)
        .expect("initialization starts");
    kernel
        .exit_initialization_mode()
        .expect("initialization settles");
    kernel
        .update_discrete_states()
        .expect("the initial event settles");
    kernel
        .enter_continuous_time_mode()
        .expect("continuous-time entry freezes the settled indicator domain");
    assert_eq!(kernel.body.indicator_storage.frozen_domains().len(), 1);
    let saved = kernel.fmu_state();
    assert!(kernel.verification_matches_snapshot(&saved));

    kernel.body.indicator_storage.flip_frozen_domain(0);
    assert!(!kernel.verification_matches_snapshot(&saved));
}

#[test]
fn nonfinite_evaluator_event_timestamps_are_not_replaced() {
    let mut kernel = instantiate(steep_algebraic_time_event_model());
    for outcome in [
        EventActionOutcome::AssertionFailed {
            time: f64::NAN,
            message: "invalid assertion time".to_owned(),
        },
        EventActionOutcome::Terminated {
            time: f64::INFINITY,
            message: "invalid termination time".to_owned(),
        },
    ] {
        let error = kernel
            .body
            .record_event_action_outcome(outcome, 1.25)
            .expect_err("the component must preserve evaluator timestamp refusal");
        assert!(matches!(
            error.kind(),
            MeError::NonFiniteEventActionTime { .. }
        ));
        assert!(kernel.body.termination.is_none());
    }
}
