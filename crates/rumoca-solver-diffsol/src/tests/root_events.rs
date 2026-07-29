use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use super::{
    ordinary_equation_row_metadata, periodic_schedule, scalar_program_block,
    set_equation_row_metadata, unit_integrator_model,
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

    assert!(
        result.data[0][1] > 2.0,
        "times={:?} x={:?}",
        result.times,
        result.data[0]
    );
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
        &rising_state_with_root_reinit(),
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
    model.problem.events.root_conditions = scalar_program_block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(0)];
    model.problem.discrete.rhs = scalar_program_block(
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
        &sampled_mean_with_coincident_root(),
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
    scalar_program_block(vec![reinit_row, mean_row], fixture_span!())
}

fn rising_state_with_root_reinit() -> solve::SolveModel {
    let rhs = scalar_program_block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    let zero = scalar_program_block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );

    let mut model = solve::SolveModel::default();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(zero.clone());
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx = IndexMap::from([("x".to_string(), 0)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        IndexMap::from([("x".to_string(), vec![0])]);
    model.problem.events.root_conditions = scalar_program_block(
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
    model.problem.discrete.rhs = scalar_program_block(
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

fn falling_ball_with_strict_reinit_guard() -> solve::SolveModel {
    let (rhs, zero) = falling_ball_continuous_blocks();

    let mut model = solve::SolveModel::default();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(rhs.clone());
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(zero.clone());
    model.problem.solve_layout.state_scalar_count = 2;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "v".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        IndexMap::from([("x".to_string(), 0), ("v".to_string(), 1)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        IndexMap::from([("x".to_string(), vec![0]), ("v".to_string(), vec![1])]);
    model.problem.solve_layout.compiled_parameter_len = 2;
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
    model.problem.events.root_conditions = scalar_program_block(
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
    let rhs = scalar_program_block(
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
    let zero = scalar_program_block(
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
    scalar_program_block(
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
