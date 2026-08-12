//! Sequencing tests for the event-entry snapshot the kernel hands to
//! `pre` seeding.
//!
//! `pre(a)` of a continuous algebraic is only the left limit when *every*
//! lane of the entry snapshot belongs to the same generation. A snapshot
//! assembled from a retained integrator guess mixes generations: its state
//! prefix is at the event time while its algebraic lanes still hold whatever
//! the last continuous evaluation left there.

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
    solve::SolveModel {
        problem: solve::SolveProblem {
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(implicit),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                    }],
                },
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                scheduled_time_events: vec![1.0],
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "a".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            ..Default::default()
        },
        artifacts: solve::SolveArtifacts {
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
        initial_y: vec![0.0, 0.0],
        solver_nominals: vec![1.0, 1.0],
        visible_names: vec!["x".to_string(), "a".to_string()],
        ..Default::default()
    }
}

fn instantiate(model: &solve::SolveModel) -> SolveMeKernel {
    let mut model = model.clone();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&model.problem)
            .expect("event-entry fixture refresh owners construct");
    SolveMeKernel::instantiate(
        MeModelSource::new(&model),
        &MeInstanceConfig {
            instance_name: "fmi-me-event-entry",
            tolerance: 1.0e-4,
            start_time: 0.0,
            stop_time: 2.0,
            root_profile: MeRootProfile::Component,
            numerics_profile: MeNumericsProfile::Component,
        },
    )
    .expect("event-entry fixture instantiates")
}

#[test]
fn the_event_entry_snapshot_is_a_full_evaluation_at_the_event_time() {
    let mut kernel = instantiate(&steep_algebraic_time_event_model());
    let event_time = 1.0_f64;
    let probe_time = timeline::event_left_probe_time(event_time, kernel.tolerance);
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

    let states = kernel.states.clone();
    let params = kernel.params.clone();
    let fresh = kernel
        .runtime
        .full_solver_y(event_time, &states, &params, settle.tol, settle.max_iters)
        .expect("the event-time evaluation succeeds");
    assert!(
        (fresh[1] - ALGEBRAIC_SLOPE * event_time).abs() < 1.0e-6,
        "the fixture's algebraic lane tracks time, got {}",
        fresh[1]
    );
    assert!(
        (stale_guess[1] - fresh[1]).abs() > 1.0,
        "the stale guess must be distinguishable from the event-time value, \
         got {} against {}",
        stale_guess[1],
        fresh[1]
    );

    let (event_pre_y, _) = kernel
        .event_pre_for_update(
            event_time,
            RuntimeEventStop::static_event(EventPreMode::EventEntry),
        )
        .expect("the event-entry snapshot is available");

    assert_eq!(
        event_pre_y.len(),
        fresh.len(),
        "the snapshot spans the full solver layout"
    );
    for (index, (snapshot, expected)) in event_pre_y.iter().zip(&fresh).enumerate() {
        assert_eq!(
            snapshot.to_bits(),
            expected.to_bits(),
            "solver lane {index} of the entry snapshot must equal the fresh \
             event-time evaluation, got {snapshot} against {expected}"
        );
    }
}
