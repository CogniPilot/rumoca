use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

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

fn rising_state_with_root_reinit() -> solve::SolveModel {
    let rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    let zero = solve::ScalarProgramBlock::with_source_span(
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
    model.artifacts.continuous.mass_matrix = vec![vec![1.0]];
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(zero.clone());
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx = IndexMap::from([("x".to_string(), 0)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        IndexMap::from([("x".to_string(), vec![0])]);
    model.problem.events.root_conditions = solve::ScalarProgramBlock::with_source_span(
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
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
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
    model.artifacts.continuous.mass_matrix = vec![vec![1.0, 0.0], vec![0.0, 1.0]];
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
        },
        solve::PreParamBinding {
            dest_p_index: 1,
            source: solve::PreParamSource::Y { index: 1 },
        },
    ];
    model.problem.events.root_conditions = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(1)];
    model.problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::Fixed];
    model.problem.discrete.rhs = falling_ball_strict_reinit_rhs();
    model.initial_y = vec![10.0, 1.0];
    model.parameters = vec![10.0, 1.0];
    model.visible_names = vec!["x".to_string(), "v".to_string()];
    model
}

fn falling_ball_continuous_blocks() -> (solve::ScalarProgramBlock, solve::ScalarProgramBlock) {
    let rhs = solve::ScalarProgramBlock::with_source_span(
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
    let zero = solve::ScalarProgramBlock::with_source_span(
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
    solve::ScalarProgramBlock::with_source_span(
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
