use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::{
    ordinary_equation_row_metadata, periodic_schedule, set_equation_row_metadata,
    unit_integrator_model,
};
use crate::simulate;

macro_rules! fixture_span {
    () => {
        solve::source_span_from_offsets(48, 0, 1)
    };
}

#[test]
fn root_reinit_does_not_interpolate_from_mutated_diffsol_state() {
    let mut model = rising_state_with_root_reinit();

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("root-triggered reinit should restart the BDF state cleanly");

    let first_output = result
        .times
        .iter()
        .position(|time| (*time - 0.1).abs() < 1.0e-12)
        .expect("the requested output grid must remain present");
    assert!(result.data[0][first_output] > 2.0);
    assert!(result.data[0].last().copied().unwrap() > 2.1);

    model.problem.events.root_conditions = solve::ScalarProgramBlock::default();
    let no_event = simulate(
        &model,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("baseline without the root should integrate");
    assert!(no_event.data[0].last().copied().unwrap() < 0.25);
}

#[test]
fn located_root_right_limit_is_recorded_as_a_trace_observation() {
    let result = simulate(
        rising_state_with_root_reinit(),
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("a located root should be observable independently of the output grid");

    assert!(
        result
            .times
            .iter()
            .zip(&result.data[0])
            .any(|(&time, &x)| time > 0.05 && time < 0.1 && x >= 2.0),
        "the trace omitted the root right limit: times={:?} x={:?}",
        result.times,
        result.data[0]
    );
}

/// Diffsol reports a located root while retaining the accepted step endpoint
/// as its internal state.  If those times are within the schedule tolerance,
/// the component must still receive the exact dense-output root state rather
/// than the nearby endpoint state that the frozen driver has not accepted.
#[test]
fn state_event_component_uses_the_exact_located_root_state() {
    const ENDPOINT: f64 = 1.0e-4;
    const ROOT: f64 = ENDPOINT - 5.0e-13;

    assert_ne!(ENDPOINT.to_bits(), ROOT.to_bits());
    assert!(rumoca_solver::timeline::sample_time_match_with_tol(
        ENDPOINT, ROOT
    ));

    let mut model = unit_integrator_model();
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: ROOT,
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

    let result = simulate(
        &model,
        &SimOptions {
            t_end: ENDPOINT,
            dt: Some(ENDPOINT),
            ..Default::default()
        },
    )
    .expect("the component and frozen driver must share the located root state");

    assert_eq!(result.times.last().copied(), Some(ENDPOINT));
}

#[test]
fn me_callbacks_observe_a_discrete_parameter_changed_by_a_root_event() {
    let mut model = unit_integrator_model();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![1.0];
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ));
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
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
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 2.0 },
            solve::LinearOp::LoadP { dst: 4, index: 0 },
            solve::LinearOp::Select {
                dst: 5,
                cond: 2,
                if_true: 3,
                if_false: 4,
            },
            solve::LinearOp::StoreOutput { src: 5 },
        ]],
        fixture_span!(),
    );
    ordinary_equation_row_metadata(&mut model);

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.2),
            ..Default::default()
        },
    )
    .expect("the ME callback path must retain a root event's discrete parameter update");

    let final_x = result.data[0].last().copied().expect("final x sample");
    assert!(
        final_x > 0.3,
        "der(x) must switch from 1 to 2 after the root event; x(0.2)={final_x}"
    );
}

/// A state root is not allowed to consume a later deadline computed by the
/// tensor-native dynamic-time-event program. The ME host must ask for the next
/// runtime stop again after event iteration, using the settled current y/p
/// values, so the later time event is both announced and applied.
#[test]
fn dynamic_time_event_deadline_survives_an_unrelated_state_root() {
    const ROOT_TIME: f64 = 0.05;
    const DEADLINE: f64 = 0.08;

    let mut model = unit_integrator_model();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["fired".to_string()];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: ROOT_TIME,
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
    model.problem.events.dynamic_time_event_rhs = scalar_program_block!(
        vec![vec![
            solve::LinearOp::Const {
                dst: 0,
                value: DEADLINE,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: DEADLINE,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 1.0 },
            solve::LinearOp::LoadP { dst: 4, index: 0 },
            solve::LinearOp::Select {
                dst: 5,
                cond: 2,
                if_true: 3,
                if_false: 4,
            },
            solve::LinearOp::StoreOutput { src: 5 },
        ]],
        fixture_span!(),
    );
    ordinary_equation_row_metadata(&mut model);
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "fired".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("the later dynamic deadline should survive the unrelated root");

    let root_index = result
        .times
        .iter()
        .position(|time| (*time - ROOT_TIME).abs() <= 1.0e-12)
        .expect("the unrelated state root should be observed");
    let deadline_index = result
        .times
        .iter()
        .rposition(|time| (*time - DEADLINE).abs() <= 1.0e-12)
        .expect("the dynamic time event should be announced after the root");
    assert!(deadline_index > root_index);
    assert_eq!(result.data[1][root_index], 0.0);
    assert_eq!(result.data[1][deadline_index], 1.0);
}

/// A located root ends one continuous mode and starts another at the exact
/// event instant. Bracketing may inspect a state just to the right of the root,
/// but that synthetic interval must use the post-event derivative. Otherwise
/// an unrelated accumulator receives a small, deterministic pre-event drift.
#[test]
fn root_mode_switch_advances_untouched_state_with_post_event_derivative() {
    let result = simulate(
        two_state_root_derivative_switch(),
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("a root derivative switch should restart both continuous states exactly");

    let accumulator = result.data[1]
        .last()
        .copied()
        .expect("the untouched accumulator should be recorded");
    assert!(
        accumulator.abs() <= 1.0e-8,
        "equal pre/post intervals should cancel exactly; synthetic root drift={accumulator}"
    );
}

#[test]
fn state_only_root_event_is_independent_of_output_grid() {
    let model = rising_state_with_root_reinit();
    let simulate_with_dt = |dt| {
        simulate(
            &model,
            &SimOptions {
                t_end: 0.2,
                dt: Some(dt),
                ..Default::default()
            },
        )
        .expect("root-triggered reinit should integrate on any output grid")
        .data[0]
            .last()
            .copied()
            .expect("the final state should be recorded")
    };

    let coarse = simulate_with_dt(0.1);
    let fine = simulate_with_dt(0.001);
    assert!(
        (coarse - fine).abs() <= 2.0e-6,
        "output sampling changed the event trajectory: coarse={coarse}, fine={fine}"
    );
}

#[test]
fn root_at_scheduled_stop_resumes_to_simulation_horizon() {
    let mut model = rising_state_with_root_reinit();
    model.problem.clocks.periodic_event_schedules = vec![
        periodic_schedule(10.0, 0.05),
        periodic_schedule(10.0, 0.075),
    ];

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("a root at a scheduled stop must resume continuous integration");

    assert_eq!(result.times.last().copied(), Some(0.1));
    assert!(
        result
            .times
            .iter()
            .any(|time| (*time - 0.075).abs() < 1.0e-12)
    );
    assert!(result.data[0].last().copied().unwrap() > 2.0);
}

#[test]
fn root_at_simulation_horizon_finishes_event_iteration() {
    let result = simulate(
        rising_state_with_root_reinit(),
        &SimOptions {
            t_end: 0.05,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("a root at the simulation horizon must not install a current-time stop");

    assert_eq!(result.times.last().copied(), Some(0.05));
    assert!(result.data[0].last().copied().unwrap() >= 2.0);
}

#[test]
fn strict_post_crossing_reinit_evaluates_on_event_right_limit() {
    let model = falling_ball_with_strict_reinit_guard();

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 2.0,
            dt: Some(0.02),
            ..Default::default()
        },
    )
    .expect("strict root-triggered reinit should bounce");

    let final_x = result.data[0].last().copied().unwrap();
    let final_v = result.data[1].last().copied().unwrap();
    assert!(
        final_x > 0.0,
        "x should rebound above the floor after reinit; times={:?} x={:?} v={:?}",
        result.times,
        result.data[0],
        result.data[1]
    );
    assert!(
        final_v > 0.0,
        "v should be reset upward at the first crossing; times={:?} x={:?} v={:?}",
        result.times,
        result.data[0],
        result.data[1]
    );
}

#[test]
fn state_only_bdf_uses_search_values_for_parameter_static_roots() {
    let mut model = unit_integrator_model();
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![0.0];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(0)];
    model.problem.discrete.rhs = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 0.0 },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const {
                dst: 3,
                value: 100.0,
            },
            solve::LinearOp::Select {
                dst: 4,
                cond: 2,
                if_true: 3,
                if_false: 0,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        fixture_span!(),
    );
    ordinary_equation_row_metadata(&mut model);

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.001,
            dt: Some(0.001),
            ..Default::default()
        },
    )
    .expect("a parameter-static root should not retrigger the BDF solver");

    let final_x = result.data[0]
        .last()
        .copied()
        .expect("x should be recorded");
    assert!(
        (final_x - 0.001).abs() <= 1.0e-8,
        "a static zero root incorrectly fired a state reinit: x={final_x}"
    );
}

/// `Modelica.Blocks.Math.Mean` in a rectifier samples at exactly the diode
/// commutation instants. MLS 3.7 §8.5 handles every event at one instant in a
/// single event iteration, so the zero-crossing must not consume the instant on
/// its own: the sampled `y_last = f*pre(x)` (with `reinit(x, 0)` in the same
/// `when`) has to see the pre-event integrator state, not the value the root's
/// right-limit application left behind.
///
/// The `when sample(...)` body is only active *at* the tick, so an application
/// at the root's right limit leaves `y_last` at its start value.
#[test]
fn scheduled_sample_still_fires_when_a_root_lands_on_its_instant() {
    let result = simulate(
        sampled_mean_with_coincident_root(),
        &SimOptions {
            t_end: 0.1,
            // The sample instant is deliberately off the output grid: an output
            // point at 0.05 would pull the root's own application time onto the
            // instant and hide the defect.
            dt: Some(0.003),
            ..Default::default()
        },
    )
    .expect("a root at the sample instant must not swallow the scheduled event");

    let y_last = result.data[1]
        .last()
        .copied()
        .expect("the sampled mean should be recorded");
    assert!(
        (y_last - 5.0).abs() <= 1.0e-3,
        "sampled mean should be 100*pre(x)=5 at t=0.05, got {y_last}; times={:?} y_last={:?}",
        result.times,
        result.data[1]
    );
}

/// Dense output may locate a root just beyond one output point and defer it to
/// the next. If that root lies immediately before a typed clock tick, resolving
/// it must not advance across the tick and then let the schedule cursor discard
/// the clock owner without executing it.
#[test]
fn deferred_root_right_limit_does_not_skip_the_next_typed_tick() {
    const ROOT: f64 = 0.05 - 1.0e-9;
    let mut model = clock_owned_sample_with_coincident_root();
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: ROOT,
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

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.1,
            // t=0.049 is recorded before the root and t=0.056 after it, so the
            // backend discovers the root by dense-output overshoot and defers
            // it before the driver reaches the t=0.05 clock stop.
            dt: Some(0.007),
            ..Default::default()
        },
    )
    .expect("a deferred root must preserve the typed tick immediately after it");

    let y_last = result.data[1]
        .last()
        .copied()
        .expect("the clock-owned sample should be recorded");
    assert!(
        (y_last - 5.0).abs() <= 1.0e-3,
        "the t=0.05 clock tick was skipped after the deferred root: y_last={y_last}"
    );
}

/// A clock leaf inside a mixed activation DAG has no row-wide clock owner.
/// Its hidden P lane is derived from the same typed periodic schedule instead.
/// A coincident root must therefore replay the lane at the semantic tick in
/// both the ME component and the frozen driver, or their discrete P state
/// diverges at the post-event compatibility seam.
#[test]
fn mixed_clock_activation_lane_still_fires_at_a_coincident_root() {
    let result = simulate(
        sampled_mean_with_mixed_clock_activation(),
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.003),
            ..Default::default()
        },
    )
    .expect("a mixed clock leaf must agree across the ME/frozen event seam");

    let y_last = result.data[1]
        .last()
        .copied()
        .expect("the mixed-clock sampled mean should be recorded");
    assert!(
        (y_last - 5.0).abs() <= 1.0e-3,
        "mixed-clock sample should be 100*pre(x)=5, got {y_last}"
    );
}

/// A typed clock row executes once at a coincident clock/root instant. The
/// root's right-limit pass still settles unowned rows, but must not replay the
/// clock owner after a same-event reinit changed the sampled source.
///
/// This is the compact form of `Blocks.Math.Mean` in the PowerConverter
/// cohort: the first pass stores `100*pre(x) = 5`; a duplicate clock pass would
/// sample the reinitialized `x = 0` and overwrite that authoritative value.
#[test]
fn clock_owned_sample_is_not_replayed_at_coincident_root_right_limit() {
    let result = simulate(
        clock_owned_sample_with_coincident_root(),
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.003),
            ..Default::default()
        },
    )
    .expect("the ME component and frozen driver must keep one clock-owned sample");

    let y_last = result.data[1]
        .last()
        .copied()
        .expect("the clock-owned sample should be recorded");
    assert!(
        (y_last - 5.0).abs() <= 1.0e-3,
        "clock-owned sample should remain 100*pre(x)=5, got {y_last}"
    );
}

/// A periodic owner is authoritative on every tick, not only the first event
/// the scheduler exposes. This is the compact execution shape of the three
/// sampled `Blocks.Math.Mean` instances in the center-tap rectifier examples:
/// every tick stores the just-finished integral and reinitializes that integral
/// before continuous integration resumes.
#[test]
fn clock_owned_sample_and_reinit_execute_on_every_periodic_tick() {
    let result = simulate(
        clock_owned_sample_with_repeated_ticks(),
        &SimOptions {
            t_end: 0.16,
            // Keep all three ticks off the output grid so output stops cannot
            // accidentally provide the event boundary.
            dt: Some(0.007),
            ..Default::default()
        },
    )
    .expect("a periodic clock owner must execute at every typed tick");

    let x = result.data[0]
        .last()
        .copied()
        .expect("the accumulator state should be recorded");
    let y_last = result.data[1]
        .last()
        .copied()
        .expect("the sampled mean should be recorded");
    assert!(
        (y_last - 5.0).abs() <= 1.0e-3,
        "each tick must sample one 0.05 s interval, got y_last={y_last}; times={:?}",
        result.times
    );
    assert!(
        (x - 0.01).abs() <= 1.0e-3,
        "the third tick must reinitialize x before the last 0.01 s interval, got x={x}"
    );
}

/// Once a periodic tick has executed, a distinct root in its numerical
/// neighbourhood is still only a state event. The consumed schedule must not
/// be rediscovered globally and replay the sampled `Mean` row from the tiny
/// post-tick accumulator interval.
#[test]
fn root_after_consumed_tick_does_not_replay_clock_owned_mean() {
    const ROOT: f64 = 0.05 + 5.0e-13;
    let mut model = clock_owned_sample_with_repeated_ticks();
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: ROOT,
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

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.06,
            dt: Some(0.007),
            ..Default::default()
        },
    )
    .expect("a post-tick root must not reactivate the consumed periodic owner");

    let y_last = result.data[1]
        .last()
        .copied()
        .expect("the sampled mean should be recorded");
    assert!(
        (y_last - 5.0).abs() <= 1.0e-3,
        "the nearby root replayed the consumed tick: y_last={y_last}"
    );
}

#[test]
fn clock_owned_counter_executes_once_at_every_coincident_root_tick() {
    let mut model = clock_owned_sample_with_repeated_ticks();
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
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
    model.problem.solve_layout.compiled_parameter_len = 3;
    model
        .problem
        .solve_layout
        .pre_param_bindings
        .push(solve::PreParamBinding {
            dest_p_index: 2,
            source: solve::PreParamSource::P { index: 0 },
            clock_schedule: None,
        });
    model.parameters.push(0.0);
    model.problem.discrete.rhs = repeated_clock_counter_rhs();
    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.16,
            dt: Some(0.007),
            ..Default::default()
        },
    )
    .expect("clock-owner execution count must agree across the root seam");

    let tick_count = result.data[1]
        .last()
        .copied()
        .expect("the clock-owned counter should be recorded");
    assert_eq!(
        tick_count, 3.0,
        "one execution is required for each typed tick; times={:?}, data={:?}",
        result.times, result.data
    );
}

fn clock_owned_sample_with_repeated_ticks() -> solve::SolveModel {
    let mut model = sampled_mean_with_coincident_root();
    model.problem.events.root_conditions = solve::ScalarProgramBlock::default();
    model.problem.clocks.periodic_event_schedules = vec![periodic_schedule(0.05, 0.05)];
    let owner = model
        .problem
        .clocks
        .periodic_clock_id(0)
        .expect("fixture has one typed periodic clock");
    model.problem.discrete.clock_owners = vec![Some(owner), Some(owner)];
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::EventEntry,
    ];
    model.problem.discrete.row_roles[0] = solve::DiscreteRowRole::EventAction;
    model.problem.discrete.rhs = repeated_clock_sampled_mean_rhs();
    model
}

fn repeated_clock_sampled_mean_rhs() -> solve::ScalarProgramBlock {
    scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 1 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: 100.0,
                },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 1,
                    rhs: 0,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
        ],
        fixture_span!(),
    )
}

fn repeated_clock_counter_rhs() -> solve::ScalarProgramBlock {
    scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
        ],
        fixture_span!(),
    )
}

pub(super) fn clock_owned_sample_with_coincident_root() -> solve::SolveModel {
    let mut model = sampled_mean_with_coincident_root();
    let owner = model
        .problem
        .clocks
        .periodic_clock_id(0)
        .expect("fixture has one typed periodic clock");
    model.problem.discrete.clock_owners = vec![Some(owner), Some(owner)];
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::EventEntry,
    ];
    model
}

fn sampled_mean_with_mixed_clock_activation() -> solve::SolveModel {
    let mut model = sampled_mean_with_coincident_root();
    model.problem.clocks.activation_parameter_indices = vec![2];
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.discrete.clock_owners.fill(None);
    model.problem.discrete.rhs = mixed_clock_sampled_mean_rhs();
    model.parameters.push(0.0);
    model
}

fn mixed_clock_sampled_mean_rhs() -> solve::ScalarProgramBlock {
    // p[2] is the schedule-derived clock activation lane. The p[0] guard
    // stands in for the AnyRise condition memory and keeps the update
    // single-shot within one event iteration.
    let sampled = vec![
        solve::LinearOp::LoadP { dst: 0, index: 2 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::Const { dst: 2, value: 0.0 },
        solve::LinearOp::Compare {
            dst: 3,
            op: solve::CompareOp::Eq,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::And,
            lhs: 0,
            rhs: 3,
        },
    ];
    let mut reinit_row = sampled.clone();
    reinit_row.extend([
        solve::LinearOp::LoadY { dst: 5, index: 0 },
        solve::LinearOp::Select {
            dst: 6,
            cond: 4,
            if_true: 2,
            if_false: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]);
    let mut mean_row = sampled;
    mean_row.extend([
        solve::LinearOp::LoadP { dst: 5, index: 1 },
        solve::LinearOp::Const {
            dst: 6,
            value: 100.0,
        },
        solve::LinearOp::Binary {
            dst: 7,
            op: solve::BinaryOp::Mul,
            lhs: 6,
            rhs: 5,
        },
        solve::LinearOp::Select {
            dst: 8,
            cond: 4,
            if_true: 7,
            if_false: 1,
        },
        solve::LinearOp::StoreOutput { src: 8 },
    ]);
    scalar_program_block!(vec![reinit_row, mean_row], fixture_span!())
}

/// `der(x) = 1`, a zero-crossing of `x - 0.05` (so the root instant is exactly
/// `t = 0.05`), and a single scheduled tick at `t = 0.05` whose `when` body sets
/// `y_last = 100*pre(x)` and reinitialises `x` to 0.
fn sampled_mean_with_coincident_root() -> solve::SolveModel {
    let mut model = rising_state_with_root_reinit();
    model.problem.clocks.periodic_event_schedules = vec![periodic_schedule(10.0, 0.05)];
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.discrete_real_scalar_names = vec!["y_last".to_string()];
    // p[1] is the `pre(x)` slot the sampled row reads; p[0] is `y_last`.
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 1,
        source: solve::PreParamSource::Y { index: 0 },
        clock_schedule: None,
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(0), solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = sampled_mean_discrete_rhs();
    set_equation_row_metadata(
        &mut model,
        vec![
            solve::DiscreteEventPreMode::FollowCurrent,
            solve::DiscreteEventPreMode::Fixed,
        ],
        vec![false, false],
    );
    model.parameters = vec![0.0, 0.0];
    model.visible_names = vec!["x".to_string(), "y_last".to_string()];
    model
}

/// Row 0: `x = if <sampled> then 0 else x` (the `reinit`).
/// Row 1: `y_last = if <sampled> then 100*pre(x) else y_last`.
///
/// `<sampled>` is `time == 0.05 and y_last == 0`. `when sample(...)` bodies are
/// active only *at* the tick, so an application at the root's right limit does
/// not satisfy them; the `y_last == 0` conjunct stands in for the edge, keeping
/// the body single-shot.
fn sampled_mean_discrete_rhs() -> solve::ScalarProgramBlock {
    // r0 = time, r1 = 0.05, r2 = time == tick, r3 = y_last, r4 = 0,
    // r5 = y_last == 0, r6 = <sampled>.
    let sampled = vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const {
            dst: 1,
            value: 0.05,
        },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Eq,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadP { dst: 3, index: 0 },
        solve::LinearOp::Const { dst: 4, value: 0.0 },
        solve::LinearOp::Compare {
            dst: 5,
            op: solve::CompareOp::Eq,
            lhs: 3,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::And,
            lhs: 2,
            rhs: 5,
        },
    ];
    let mut reinit_row = sampled.clone();
    reinit_row.extend([
        solve::LinearOp::LoadY { dst: 7, index: 0 },
        solve::LinearOp::Select {
            dst: 8,
            cond: 6,
            if_true: 4,
            if_false: 7,
        },
        solve::LinearOp::StoreOutput { src: 8 },
    ]);
    let mut mean_row = sampled;
    mean_row.extend([
        solve::LinearOp::LoadP { dst: 7, index: 1 },
        solve::LinearOp::Const {
            dst: 8,
            value: 100.0,
        },
        solve::LinearOp::Binary {
            dst: 9,
            op: solve::BinaryOp::Mul,
            lhs: 8,
            rhs: 7,
        },
        solve::LinearOp::Select {
            dst: 10,
            cond: 6,
            if_true: 9,
            if_false: 3,
        },
        solve::LinearOp::StoreOutput { src: 10 },
    ]);
    scalar_program_block!(vec![reinit_row, mean_row], fixture_span!())
}

fn rising_state_with_root_reinit() -> solve::SolveModel {
    let rhs = scalar_program_block!(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    let zero = scalar_program_block!(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );

    let mut model = solve::SolveModel::default();
    // `der(x) = 1`. The mass matrix is the identity, so the explicit derivative
    // program and the implicit residual carry the same row; Solve emits both,
    // and the reduced state-only system integrates the derivative program.
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(zero.clone());
    // d(der(x))/dx = 0 — `der(x) = 1` is constant, so the state Jacobian the
    // reduced system linearises against is exactly zero.
    model.artifacts.continuous.full_jacobian_v = zero.clone();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx = IndexMap::from([("x".to_string(), 0)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        IndexMap::from([("x".to_string(), vec![0])]);
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(0)];
    model.problem.discrete.rhs = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 2.0 },
            solve::LinearOp::Select {
                dst: 4,
                cond: 2,
                if_true: 3,
                if_false: 0,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        fixture_span!(),
    );
    ordinary_equation_row_metadata(&mut model);
    model.initial_y = vec![0.0];
    model.visible_names = vec!["x".to_string()];
    model
}

fn two_state_root_derivative_switch() -> solve::SolveModel {
    let rhs = scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    let zero = scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );

    let mut model = solve::SolveModel::default();
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(rhs);
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(zero.clone());
    model.artifacts.continuous.full_jacobian_v = zero;
    model.problem.solve_layout.state_scalar_count = 2;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.solver_maps.names =
        vec!["trigger".to_string(), "accumulator".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        IndexMap::from([("trigger".to_string(), 0), ("accumulator".to_string(), 1)]);
    model.problem.solve_layout.solver_maps.base_to_indices = IndexMap::from([
        ("trigger".to_string(), vec![0]),
        ("accumulator".to_string(), vec![1]),
    ]);
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
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
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const {
                dst: 3,
                value: -1.0,
            },
            solve::LinearOp::LoadP { dst: 4, index: 0 },
            solve::LinearOp::Select {
                dst: 5,
                cond: 2,
                if_true: 3,
                if_false: 4,
            },
            solve::LinearOp::StoreOutput { src: 5 },
        ]],
        fixture_span!(),
    );
    ordinary_equation_row_metadata(&mut model);
    model.initial_y = vec![0.0, 0.0];
    model.parameters = vec![1.0];
    model.visible_names = vec!["trigger".to_string(), "accumulator".to_string()];
    model
}

fn falling_ball_with_strict_reinit_guard() -> solve::SolveModel {
    let (rhs, zero) = falling_ball_continuous_blocks();

    let mut model = solve::SolveModel::default();
    // `der(x) = v`, `der(v) = -9.81`. Identity mass matrix, so the derivative
    // program and the implicit residual carry the same two rows, and both rows
    // read only states — the reduced state-only system integrates directly.
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(zero.clone());
    // Exact state Jacobian-vector product for `der(x) = v`, `der(v) = -9.81`:
    // row 0 is `v`'s seed component, row 1 is zero.
    model.artifacts.continuous.full_jacobian_v = scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    model.problem.solve_layout.state_scalar_count = 2;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "v".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        IndexMap::from([("x".to_string(), 0), ("v".to_string(), 1)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        IndexMap::from([("x".to_string(), vec![0]), ("v".to_string(), vec![1])]);
    model.problem.solve_layout.compiled_parameter_len = 2;
    // The full-Jacobian seed spans `y ++ p`, so the variable layout has to
    // declare both states and both parameter slots for the JVP above to be
    // in bounds.
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 2, 2);
    model.problem.solve_layout.pre_param_bindings = vec![
        solve::PreParamBinding {
            dest_p_index: 0,
            source: solve::PreParamSource::Y { index: 0 },
            clock_schedule: None,
        },
        solve::PreParamBinding {
            dest_p_index: 1,
            source: solve::PreParamSource::Y { index: 1 },
            clock_schedule: None,
        },
    ];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(1)];
    model.problem.discrete.rhs = falling_ball_strict_reinit_rhs();
    set_equation_row_metadata(
        &mut model,
        vec![solve::DiscreteEventPreMode::Fixed],
        vec![false],
    );
    model.initial_y = vec![10.0, 1.0];
    model.parameters = vec![10.0, 1.0];
    model.visible_names = vec!["x".to_string(), "v".to_string()];
    model
}

fn falling_ball_continuous_blocks() -> (solve::ScalarProgramBlock, solve::ScalarProgramBlock) {
    let rhs = scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const {
                    dst: 0,
                    value: -9.81,
                },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    let zero = scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    (rhs, zero)
}

fn falling_ball_strict_reinit_rhs() -> solve::ScalarProgramBlock {
    scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 0.0 },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Lt,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::LoadP { dst: 3, index: 0 },
            solve::LinearOp::Compare {
                dst: 4,
                op: solve::CompareOp::Lt,
                lhs: 3,
                rhs: 1,
            },
            solve::LinearOp::Unary {
                dst: 5,
                op: solve::UnaryOp::Not,
                arg: 4,
            },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::And,
                lhs: 2,
                rhs: 5,
            },
            solve::LinearOp::LoadP { dst: 7, index: 1 },
            solve::LinearOp::Const {
                dst: 8,
                value: -0.8,
            },
            solve::LinearOp::Binary {
                dst: 9,
                op: solve::BinaryOp::Mul,
                lhs: 8,
                rhs: 7,
            },
            solve::LinearOp::LoadY { dst: 10, index: 1 },
            solve::LinearOp::Select {
                dst: 11,
                cond: 6,
                if_true: 9,
                if_false: 10,
            },
            solve::LinearOp::StoreOutput { src: 11 },
        ]],
        fixture_span!(),
    )
}
