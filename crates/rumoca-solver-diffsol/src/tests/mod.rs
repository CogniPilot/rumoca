use super::*;

macro_rules! fixture_span {
    () => {
        solve::source_span_from_offsets(49, 0, 1)
    };
}

mod root_events;
mod tensor_runtime;

mod runtime_value_tests;

#[test]
fn speculative_algebraic_guess_does_not_mutate_accepted_state() {
    let warm_start = AlgebraicWarmStart::new(vec![1.0, 2.0]);
    let mut rejected_trial = warm_start.speculative();
    rejected_trial[1] = 99.0;

    assert_eq!(warm_start.speculative(), vec![1.0, 2.0]);

    warm_start.commit(vec![3.0, 4.0]);
    assert_eq!(warm_start.speculative(), vec![3.0, 4.0]);
}

#[test]
fn algebraic_candidate_commits_only_for_the_exact_accepted_state() {
    let warm_start = AlgebraicWarmStart::new(vec![1.0, 2.0]);
    warm_start.record_candidate(0.5, &[3.0, 4.0], vec![5.0, 6.0]);

    assert!(!warm_start.commit_matching_candidate(0.5, &[3.0, 4.1]));
    assert_eq!(warm_start.speculative(), vec![1.0, 2.0]);
    assert!(warm_start.commit_matching_candidate(0.5, &[3.0, 4.0]));
    assert_eq!(warm_start.speculative(), vec![5.0, 6.0]);
}

#[test]
fn speculative_continuation_uses_only_a_closer_same_time_candidate() {
    let warm_start = AlgebraicWarmStart::new(vec![0.0, 0.0, 10.0]);
    warm_start.record_candidate(0.5, &[0.9, 0.9], vec![0.9, 0.9, 20.0]);

    assert_eq!(
        warm_start.speculative_nearest(0.5, &[1.0, 1.0]),
        vec![0.9, 0.9, 20.0]
    );
    assert_eq!(
        warm_start.speculative_nearest(0.5, &[-0.1, -0.1]),
        vec![0.0, 0.0, 10.0]
    );
    assert_eq!(
        warm_start.speculative_nearest(0.6, &[1.0, 1.0]),
        vec![0.0, 0.0, 10.0]
    );
    assert_eq!(
        warm_start.speculative(),
        vec![0.0, 0.0, 10.0],
        "speculative continuation must not commit a rejected solver trial"
    );
}

fn install_dense_algebraic_projection_plan(model: &mut solve::SolveModel) {
    let state_count = model.problem.solve_layout.state_scalar_count;
    let algebraic_count = model.problem.solve_layout.algebraic_scalar_count;
    if algebraic_count == 0 {
        model.problem.continuous.algebraic_projection_plan =
            solve::AlgebraicProjectionPlan::default();
        return;
    }
    let y_indices = (state_count..state_count + algebraic_count).collect::<Vec<_>>();
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: y_indices.clone(),
            y_indices,
            tearing: None,
        }],
    };
}

fn install_scalar_initial_projection_plan(
    model: &mut solve::SolveModel,
    row: usize,
    y_index: usize,
) {
    model.problem.initialization.projection_indices = vec![y_index];
    model.problem.initialization.projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![row],
            y_indices: vec![y_index],
            tearing: None,
        }],
    };
}

#[test]
fn state_only_bdf_accepts_projection_backed_derivative_dependencies() {
    let mut model = projected_derivative_model();

    assert!(
        can_use_state_only_bdf(&model).expect("valid model should check BDF eligibility"),
        "state derivative rows may read non-state slots when the projection plan can refresh them"
    );

    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan::default();
    assert!(
        !can_use_state_only_bdf(&model).expect("valid model should check BDF eligibility"),
        "state-only BDF must not treat missing algebraic refresh data as a default"
    );
}

#[test]
fn general_path_integrates_with_sdirk_tableaus() {
    // A non-BDF `diffsol_method` routes the model through the general/implicit
    // path, where ESDIRK34 / TR-BDF2 are wired. The ramp model (x' = 1,
    // x(0) = 0) has the closed form x(t) = t, so every tableau must land on
    // x(1) = 1 and agree with the BDF baseline.
    let model = general_ramp_model();
    let bdf = run_ramp(&model, DiffsolMethod::Bdf);
    assert!((bdf - 1.0).abs() <= 1.0e-6, "BDF baseline: x(1) = {bdf}");
    for method in [DiffsolMethod::Esdirk34, DiffsolMethod::TrBdf2] {
        let x = run_ramp(&model, method);
        assert!((x - 1.0).abs() <= 1.0e-6, "{method:?}: x(1) = {x}");
        assert!(
            (x - bdf).abs() <= 1.0e-6,
            "{method:?} disagrees with BDF: {x} vs {bdf}"
        );
    }
}

fn run_ramp(model: &solve::SolveModel, method: DiffsolMethod) -> f64 {
    let result = simulate(
        model,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.25),
            diffsol_method: method,
            ..Default::default()
        },
    )
    .unwrap_or_else(|err| panic!("{method:?} should integrate the ramp: {err}"));
    *result
        .data
        .first()
        .and_then(|series| series.last())
        .expect("x series should have a final sample")
}

/// A single-state ramp `x' = 1` (so `x(t) = t`) whose residual/jacobian are
/// consistently formed for the general/implicit solver path: `M·x' = f` with
/// `M = I` and `f = 1`, exact JVP `df/dx = 0`, visible value reading the state
/// directly.
fn general_ramp_model() -> solve::SolveModel {
    let mut model = unit_integrator_model();
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.visible_value_rows = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model
}

#[test]
fn state_only_bdf_accepts_transitive_projection_dependencies() {
    let mut model = projected_derivative_model();
    model
        .problem
        .solve_layout
        .solver_maps
        .names
        .push("b".to_string());
    model
        .problem
        .solve_layout
        .solver_maps
        .name_to_idx
        .insert("b".to_string(), 2);
    model
        .problem
        .solve_layout
        .solver_maps
        .base_to_indices
        .insert("b".to_string(), vec![2]);
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![
                state_residual_row(),
                y_minus_y_row(1, 0),
                y_minus_y_row(2, 1),
            ],
            fixture_span!(),
        ),
    );
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![2],
                y_indices: vec![2],
                tearing: None,
            },
        ],
    };

    assert!(
        can_use_state_only_bdf(&model).expect("valid model should check BDF eligibility"),
        "projection coverage should be checked transitively through producer rows"
    );
}

#[test]
fn state_only_periodic_events_do_not_advance_to_right_limit_without_state_integration() {
    let mut model = unit_integrator_model();
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        phase_seconds: 0.05,
        period_seconds: 10.0,
    }];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("ordinary scheduled events should not skip state integration over right-limit time");

    assert_eq!(result.times, vec![0.0, 0.05, 0.1]);
    assert!((result.data[0][1] - 0.05).abs() <= 1.0e-8);
    assert!((result.data[0][2] - 0.1).abs() <= 1.0e-8);
}

fn projected_derivative_model() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "a".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("a".to_string(), 1)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        indexmap::IndexMap::from([("x".to_string(), vec![0]), ("a".to_string(), vec![1])]);
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![state_residual_row(), y_minus_y_row(1, 0)],
            fixture_span!(),
        ),
    );
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
            tearing: None,
        }],
    };
    model.initial_y = vec![0.0, 0.0];
    model
}

fn unit_integrator_model() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        indexmap::IndexMap::from([("x".to_string(), vec![0])]);
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(vec![state_residual_row()], fixture_span!()),
    );
    model.artifacts.continuous.mass_matrix = vec![vec![1.0]];
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.initial_y = vec![0.0];
    model.visible_names = vec!["x".to_string()];
    model
}

fn state_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn y_minus_y_row(lhs_index: usize, rhs_index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: lhs_index,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: rhs_index,
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

#[test]
fn simulate_accepts_zero_state_solve_ir_without_building_ode_problem() {
    let model = solve::SolveModel::default();
    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("zero-state solve-IR model should use no-state simulation path");

    assert_eq!(result.times, vec![0.0, 0.1, 0.2]);
    assert!(result.names.is_empty());
    assert!(result.data.is_empty());
}

#[test]
fn simulate_records_visible_runtime_tail_values_for_no_state_models() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.parameters = vec![7.0];
    model.visible_names = vec!["m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("runtime-tail-only solve-IR model should simulate");

    assert_eq!(result.names, vec!["m".to_string()]);
    assert_eq!(result.data, vec![vec![7.0, 7.0, 7.0]]);
}

#[test]
fn simulate_no_state_solve_ir_recomputes_external_time_table_runtime_assignment() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.discrete_real_scalar_names = vec!["y".to_string()];
    model.problem.discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(1)];
    model.problem.discrete.runtime_assignment_rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 1.0 },
            solve::LinearOp::LoadTime { dst: 2 },
            solve::LinearOp::TableLookup {
                dst: 3,
                table_id: 0,
                column: 1,
                input: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![1.0, 0.0];
    model
        .external_tables
        .push_table(1, vec![vec![1.0, 1.0], vec![3.0, 0.0]], vec![2], 3, 1);
    model.visible_names = vec!["y".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 4.0,
            dt: Some(2.0),
            ..Default::default()
        },
    )
    .expect("no-state solve-IR table assignment should simulate");

    // MLS §12.2: pure function calls in equations are evaluated from the
    // current inputs. A table-backed runtime assignment must therefore observe
    // the current simulation time at every output sample.
    assert_eq!(result.data, vec![vec![1.0, 1.0, 0.0]]);
}

#[test]
fn simulate_no_state_dynamic_table_event_updates_projected_runtime_alias() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["table_y".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("table_y".to_string(), 0)]);
    model.problem.solve_layout.discrete_real_scalar_names = vec!["table_u".to_string()];
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::LoadP { dst: 1, index: 0 },
                solve::LinearOp::Const { dst: 2, value: 1.0 },
                solve::LinearOp::LoadTime { dst: 3 },
                solve::LinearOp::TableLookup {
                    dst: 4,
                    table_id: 1,
                    column: 2,
                    input: 3,
                },
                solve::LinearOp::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 4,
                },
                solve::LinearOp::StoreOutput { src: 5 },
            ]],
            fixture_span!(),
        ),
    );
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
    install_dense_algebraic_projection_plan(&mut model);
    model.problem.discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(1)];
    model.problem.discrete.runtime_assignment_rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.events.dynamic_time_event_rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::LoadTime { dst: 1 },
            solve::LinearOp::TableNextEvent {
                dst: 2,
                table_id: 0,
                time: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![1.0, 0.0];
    model.initial_y = vec![0.0];
    model.visible_names = vec!["table_y".to_string(), "table_u".to_string()];
    model.external_tables.push_table(
        1,
        vec![vec![0.05, 0.0], vec![0.05, 1.0], vec![0.15, 0.0]],
        vec![2],
        3,
        1,
    );

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("dynamic table event should refresh projected algebraic aliases");

    assert_eq!(result.times.len(), 4);
    assert!((result.times[1] - 0.05).abs() <= 1.0e-12);
    assert!(result.times[2] > 0.05);
    assert_eq!(result.data[0], vec![0.0, 0.0, 1.0, 1.0]);
    assert_eq!(result.data[1], vec![0.0, 0.0, 1.0, 1.0]);
}

#[test]
fn simulate_applies_discrete_runtime_tail_updates_at_initial_event() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 3.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![0.0];
    model.visible_names = vec!["m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("initial event should apply discrete solve-IR updates");

    assert_eq!(result.data, vec![vec![3.0, 3.0, 3.0]]);
}

#[test]
fn simulate_discrete_updates_can_read_initial_event_flag() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.solve_layout.initial_event_parameter_index = Some(1);
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![0.0, 0.0];
    model.visible_names = vec!["m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("initial event flag should be visible to solve-IR discrete rows");

    // MLS §8.6: initial() is true while the initial event is settled and false
    // afterwards; the startup assignment must keep its settled value.
    assert_eq!(result.data, vec![vec![1.0, 1.0, 1.0]]);
}

#[test]
fn simulate_applies_start_time_clock_tick_after_initial_mode() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.discrete_valued_scalar_names =
        vec!["source".to_string(), "held".to_string()];
    model.problem.solve_layout.initial_event_parameter_index = Some(2);
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        period_seconds: 0.1,
        phase_seconds: 0.0,
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(1)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 2 },
            solve::LinearOp::Const { dst: 1, value: 0.0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            solve::LinearOp::Select {
                dst: 3,
                cond: 0,
                if_true: 1,
                if_false: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![2.0, 0.0, 0.0];
    model.visible_names = vec!["held".to_string()];
    model.visible_value_rows = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("start-time clock tick should settle after initial() becomes false");

    assert_eq!(result.data, vec![vec![2.0, 2.0, 2.0]]);
}

#[test]
fn simulate_initial_event_iterates_pre_to_current_runtime_tail() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.discrete_valued_scalar_names =
        vec!["aux".to_string(), "y".to_string()];
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 2,
        source: solve::PreParamSource::P { index: 0 },
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 3.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    model.parameters = vec![1.0, 0.0, 0.0];
    model.visible_names = vec!["aux".to_string(), "y".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("initial event should iterate pre values to the initialized fixed point");

    assert_eq!(result.data, vec![vec![3.0, 3.0], vec![3.0, 3.0]]);
}

#[test]
fn fixed_static_event_keeps_follow_current_pre_rows_iterating() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.discrete_valued_scalar_names =
        vec!["aux".to_string(), "y".to_string()];
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 2,
        source: solve::PreParamSource::P { index: 0 },
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 3.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
    ];
    model.parameters = vec![1.0, 0.0, 0.0];

    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let ode_model = OdeModel::new(&model).expect("solve model should build");
    let mut y = Vec::new();
    let mut p = model.parameters.clone();

    apply_event_updates(&runtime, &ode_model, &mut y, &mut p, 0.0, 1.0e-12)
        .expect("row-level FollowCurrent pre feedback should settle at fixed events");

    assert_eq!(&p[..2], [3.0, 3.0]);
}

#[test]
fn dynamic_time_event_window_keeps_strict_future_event_within_tolerance() {
    let current_t = 4.999999999999981;

    assert!(rumoca_solver::timeline::event_time_in_window(
        5.0, current_t, 5.02
    ));
    assert!(!rumoca_solver::timeline::event_time_in_window(
        5.0,
        f64::next_up(5.0),
        5.02
    ));
}

#[test]
fn simulate_no_state_solve_ir_records_dynamic_time_event_between_outputs() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_real_scalar_names = vec!["next".to_string()];
    model.problem.events.dynamic_time_event_names = vec!["next".to_string()];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 0 },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 7.0 },
            solve::LinearOp::Select {
                dst: 4,
                cond: 2,
                if_true: 3,
                if_false: 1,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![5.0];
    model.visible_names = vec!["next".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 4.98,
            t_end: 5.02,
            dt: Some(0.02),
            ..Default::default()
        },
    )
    .expect("dynamic solve-IR time events should stop no-state simulation");

    assert_eq!(result.times.len(), 4);
    assert_eq!(result.times[0], 4.98);
    assert_eq!(result.times[1], 5.0);
    assert!(result.times[2] > 5.0);
    assert!(result.times[2] < 5.02);
    assert_eq!(result.times[3], 5.02);
    assert_eq!(result.data, vec![vec![5.0, 7.0, 7.0, 7.0]]);
}

#[test]
fn simulate_no_state_solve_ir_records_direct_time_threshold_event() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 2;
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["pulse".to_string()];
    model.problem.events.dynamic_time_event_rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(2)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 0 },
            solve::LinearOp::LoadP { dst: 2, index: 1 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Add,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Compare {
                dst: 4,
                op: solve::CompareOp::Lt,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::Const { dst: 5, value: 1.0 },
            solve::LinearOp::Const { dst: 6, value: 0.0 },
            solve::LinearOp::Select {
                dst: 7,
                cond: 4,
                if_true: 5,
                if_false: 6,
            },
            solve::LinearOp::StoreOutput { src: 7 },
        ]],
        fixture_span!(),
    );
    model.parameters = vec![0.0, 0.2, 0.0];
    model.visible_names = vec!["pulse".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.4,
            dt: Some(0.3),
            ..Default::default()
        },
    )
    .expect("direct time threshold rows should stop no-state simulation");

    let event_idx = result
        .times
        .iter()
        .position(|time| (*time - 0.2).abs() <= 1.0e-12)
        .expect("pulse falling edge should be recorded at its event time");
    assert_eq!(result.data[0][0], 1.0);
    assert_eq!(result.data[0][event_idx], 0.0);
}

#[test]
fn simulate_no_state_solve_ir_refreshes_periodic_event_indicator_between_ticks() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.discrete_valued_scalar_names =
        vec!["pulse".to_string(), "derived".to_string()];
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        phase_seconds: 0.0,
        period_seconds: 0.5,
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![periodic_tick_row(0.0, 0.5), not_parameter_row(0)],
        fixture_span!(),
    );
    model.problem.discrete.observation_refresh = vec![true, true];
    model.parameters = vec![0.0, 0.0];
    model.visible_names = vec!["pulse".to_string(), "derived".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.75,
            dt: Some(0.25),
            ..Default::default()
        },
    )
    .expect("MLS §16.5.1 sample(start, interval) event indicators should simulate");

    assert_eq!(result.times, vec![0.0, 0.25, 0.5, 0.75]);
    assert_eq!(
        result.data,
        vec![vec![1.0, 0.0, 1.0, 0.0], vec![0.0, 1.0, 0.0, 1.0]]
    );
}

#[test]
fn observation_refresh_keeps_pre_values_fixed_during_settle() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.discrete_valued_scalar_names =
        vec!["tick".to_string(), "state".to_string()];
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 2,
        source: solve::PreParamSource::P { index: 1 },
    }];
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        phase_seconds: 0.0,
        period_seconds: 0.1,
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![
            periodic_tick_row(0.0, 0.1),
            increment_pre_state_on_tick_row(),
        ],
        fixture_span!(),
    );
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::Fixed,
    ];
    model.problem.discrete.observation_refresh = vec![true, true];
    let mut y = Vec::new();
    let mut p = vec![0.0, 0.0, 0.0];
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let changed = runtime
        .refresh_observation_discrete_rows(y.as_mut_slice(), p.as_mut_slice(), 0.1, 1.0e-12, 8)
        .expect("MLS §8.6 pre(state) should remain the event-entry value during settle");

    assert!(changed);
    assert_eq!(p, vec![1.0, 1.0, 0.0]);
}

#[test]
fn observation_refresh_resets_fixed_pre_event_history_rows() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 4;
    model.problem.solve_layout.discrete_valued_scalar_names = vec![
        "source".to_string(),
        "held".to_string(),
        "pulse".to_string(),
    ];
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 3,
        source: solve::PreParamSource::P { index: 1 },
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(1), solve::scalar_slot_p(2)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![load_parameter_row(0), change_parameter_row(1, 3)],
        fixture_span!(),
    );
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::Fixed,
    ];
    model.problem.discrete.observation_refresh = vec![true, true];
    let mut y = Vec::new();
    let mut p = vec![1.0, 0.0, 0.0, 0.0];
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let changed = runtime
        .refresh_observation_discrete_rows(y.as_mut_slice(), p.as_mut_slice(), 0.0, 1.0e-12, 8)
        .expect("observation refresh should settle event-history rows");

    assert!(changed);
    assert_eq!(
        p,
        vec![1.0, 1.0, 1.0, 0.0],
        "fixed-pre change rows should see the event-entry value through the refresh pass"
    );
}

#[test]
fn simulate_no_state_solve_ir_updates_clocked_previous_feedback_at_periodic_ticks() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 8;
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.solve_layout.solver_maps.names =
        vec!["assignClock.u".to_string(), "assignClock.clock".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx = indexmap::IndexMap::from([
        ("assignClock.u".to_string(), 0),
        ("assignClock.clock".to_string(), 1),
    ]);
    model.problem.solve_layout.discrete_real_scalar_names = vec![
        "periodicClock.c".to_string(),
        "unitDelay.u".to_string(),
        "unitDelay.y".to_string(),
        "add.u1".to_string(),
        "assignClock.y".to_string(),
    ];
    model.problem.solve_layout.pre_param_bindings = vec![
        solve::PreParamBinding {
            dest_p_index: 5,
            source: solve::PreParamSource::Y { index: 1 },
        },
        solve::PreParamBinding {
            dest_p_index: 6,
            source: solve::PreParamSource::P { index: 4 },
        },
        solve::PreParamBinding {
            dest_p_index: 7,
            source: solve::PreParamSource::P { index: 1 },
        },
    ];
    let rhs_rows = vec![y_minus_p_plus_const_row(0, 3, 1.0), y_minus_p_row(1, 0)];
    let jvp_rows = vec![seed_output_row(0), seed_output_row(1)];
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_block(rhs_rows.clone()));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(scalar_block(jvp_rows.clone()));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    install_dense_algebraic_projection_plan(&mut model);
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        phase_seconds: 0.0,
        period_seconds: 0.02,
    }];
    model.problem.discrete.update_targets = vec![
        solve::scalar_slot_p(0),
        solve::scalar_slot_p(2),
        solve::scalar_slot_p(3),
        solve::scalar_slot_p(1),
        solve::scalar_slot_p(4),
    ];
    model.problem.discrete.rhs = scalar_block(vec![
        periodic_tick_row(0.0, 0.02),
        load_parameter_row(7),
        load_parameter_row(2),
        load_parameter_row(4),
        assign_on_clock_edge_row(),
    ]);
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::EventEntry,
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
    ];
    model.problem.discrete.observation_refresh = vec![true, false, false, false, false];
    model.parameters = vec![0.0; 8];
    model.initial_y = vec![0.0; 2];
    model.visible_names = vec![
        "assignClock.y".to_string(),
        "unitDelay.y".to_string(),
        "assignClock.u".to_string(),
    ];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.05,
            dt: Some(0.02),
            ..Default::default()
        },
    )
    .expect("MLS §16.4 clocked previous feedback should update once per periodic tick");

    assert_eq!(result.times, vec![0.0, 0.02, 0.04, 0.05]);
    assert_eq!(
        result.data,
        vec![
            vec![1.0, 2.0, 3.0, 3.0],
            vec![0.0, 1.0, 2.0, 2.0],
            vec![1.0, 2.0, 3.0, 3.0],
        ]
    );
}

fn scalar_block(rows: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(rows, fixture_span!())
}

fn not_parameter_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::Unary {
            dst: 1,
            op: solve::UnaryOp::Not,
            arg: 0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]
}

fn load_parameter_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn change_parameter_row(current_index: usize, pre_index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP {
            dst: 0,
            index: current_index,
        },
        solve::LinearOp::LoadP {
            dst: 1,
            index: pre_index,
        },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Ne,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn increment_pre_state_on_tick_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::LoadP { dst: 1, index: 2 },
        solve::LinearOp::Const { dst: 2, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Select {
            dst: 4,
            cond: 0,
            if_true: 3,
            if_false: 1,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn y_minus_p_row(y_index: usize, p_index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: y_index,
        },
        solve::LinearOp::LoadP {
            dst: 1,
            index: p_index,
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

fn y_minus_p_plus_const_row(y_index: usize, p_index: usize, offset: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: y_index,
        },
        solve::LinearOp::LoadP {
            dst: 1,
            index: p_index,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: offset,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn seed_output_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadSeed { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn assign_on_clock_edge_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadP { dst: 1, index: 5 },
        solve::LinearOp::Unary {
            dst: 2,
            op: solve::UnaryOp::Not,
            arg: 1,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::And,
            lhs: 0,
            rhs: 2,
        },
        solve::LinearOp::LoadY { dst: 4, index: 0 },
        solve::LinearOp::LoadP { dst: 5, index: 6 },
        solve::LinearOp::Select {
            dst: 6,
            cond: 3,
            if_true: 4,
            if_false: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

fn periodic_tick_row(phase: f64, period: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const {
            dst: 1,
            value: phase,
        },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::Const {
            dst: 3,
            value: -1.0e-9,
        },
        solve::LinearOp::Compare {
            dst: 4,
            op: solve::CompareOp::Ge,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Const {
            dst: 5,
            value: period,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Div,
            lhs: 2,
            rhs: 5,
        },
        solve::LinearOp::Const { dst: 7, value: 0.5 },
        solve::LinearOp::Binary {
            dst: 8,
            op: solve::BinaryOp::Add,
            lhs: 6,
            rhs: 7,
        },
        solve::LinearOp::Unary {
            dst: 9,
            op: solve::UnaryOp::Floor,
            arg: 8,
        },
        solve::LinearOp::Binary {
            dst: 10,
            op: solve::BinaryOp::Mul,
            lhs: 9,
            rhs: 5,
        },
        solve::LinearOp::Binary {
            dst: 11,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 10,
        },
        solve::LinearOp::Unary {
            dst: 12,
            op: solve::UnaryOp::Abs,
            arg: 11,
        },
        solve::LinearOp::Const {
            dst: 13,
            value: 1.0e-9,
        },
        solve::LinearOp::Compare {
            dst: 14,
            op: solve::CompareOp::Le,
            lhs: 12,
            rhs: 13,
        },
        solve::LinearOp::Binary {
            dst: 15,
            op: solve::BinaryOp::And,
            lhs: 4,
            rhs: 14,
        },
        solve::LinearOp::StoreOutput { src: 15 },
    ]
}

#[test]
fn event_update_converges_boolean_pre_feedback_loop_row_by_row() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 6;
    model.problem.solve_layout.discrete_valued_scalar_names = vec![
        "pre.u".to_string(),
        "pre.y".to_string(),
        "nor1.y".to_string(),
        "nor.u2".to_string(),
        "nor.y".to_string(),
    ];
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 5,
        source: solve::PreParamSource::P { index: 0 },
    }];
    model.problem.discrete.update_targets = (0..5).map(solve::scalar_slot_p).collect();
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 4 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 5 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 1 },
                solve::LinearOp::Unary {
                    dst: 1,
                    op: solve::UnaryOp::Not,
                    arg: 0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 3 },
                solve::LinearOp::Unary {
                    dst: 1,
                    op: solve::UnaryOp::Not,
                    arg: 0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
        ],
        fixture_span!(),
    );
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::Fixed,
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
    ];
    model.parameters = vec![1.0, 1.0, 1.0, 1.0, 0.0, 0.0];

    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let ode_model = OdeModel::new(&model).expect("solve model should build");
    let mut y = Vec::new();
    let mut p = model.parameters.clone();

    apply_event_updates(&runtime, &ode_model, &mut y, &mut p, 1.2, 1.0e-12)
        .expect("MLS Appendix B event iteration should converge the Boolean pre feedback loop");

    assert_eq!(&p[..5], [1.0, 1.0, 0.0, 0.0, 1.0]);
}

#[test]
fn fixed_time_event_does_not_freeze_follow_current_rows() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 6;
    model.problem.solve_layout.discrete_valued_scalar_names = vec![
        "pre.u".to_string(),
        "pre.y".to_string(),
        "nor1.y".to_string(),
        "nor.u2".to_string(),
        "nor.y".to_string(),
    ];
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 5,
        source: solve::PreParamSource::P { index: 0 },
    }];
    model.problem.discrete.update_targets = (0..5).map(solve::scalar_slot_p).collect();
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![
            load_parameter_row(4),
            load_parameter_row(5),
            not_parameter_row(1),
            load_parameter_row(2),
            not_parameter_row(3),
        ],
        fixture_span!(),
    );
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::Fixed,
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::FollowCurrent,
    ];
    model.parameters = vec![1.0, 1.0, 1.0, 1.0, 0.0, 0.0];

    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let ode_model = OdeModel::new(&model).expect("solve model should build");
    let mut y = Vec::new();
    let mut p = model.parameters.clone();

    apply_event_updates(&runtime, &ode_model, &mut y, &mut p, 1.2, 1.0e-12)
        .expect("row-level FollowCurrent pre modes should still event-iterate");

    assert_eq!(&p[..5], [1.0, 1.0, 0.0, 0.0, 1.0]);
}

#[test]
fn event_update_rechecks_change_guard_after_runtime_alias_refresh() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 5;
    model.problem.solve_layout.discrete_valued_scalar_names = vec![
        "source".to_string(),
        "alias".to_string(),
        "latched".to_string(),
    ];
    model.problem.solve_layout.pre_param_bindings = vec![
        solve::PreParamBinding {
            dest_p_index: 3,
            source: solve::PreParamSource::P { index: 1 },
        },
        solve::PreParamBinding {
            dest_p_index: 4,
            source: solve::PreParamSource::P { index: 2 },
        },
    ];
    model.problem.discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(1)];
    model.problem.discrete.runtime_assignment_rhs =
        solve::ScalarProgramBlock::with_source_span(vec![load_parameter_row(0)], fixture_span!());
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0), solve::scalar_slot_p(2)];
    model.problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
        vec![
            vec![
                solve::LinearOp::Const { dst: 0, value: 3.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 1 },
                solve::LinearOp::LoadP { dst: 1, index: 3 },
                solve::LinearOp::Compare {
                    dst: 2,
                    op: solve::CompareOp::Ne,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::Const { dst: 3, value: 7.0 },
                solve::LinearOp::LoadP { dst: 4, index: 4 },
                solve::LinearOp::Select {
                    dst: 5,
                    cond: 2,
                    if_true: 3,
                    if_false: 4,
                },
                solve::LinearOp::StoreOutput { src: 5 },
            ],
        ],
        fixture_span!(),
    );
    model.problem.discrete.pre_modes = vec![
        solve::DiscreteEventPreMode::FollowCurrent,
        solve::DiscreteEventPreMode::Fixed,
    ];
    model.parameters = vec![1.0, 1.0, 0.0, 0.0, 0.0];

    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let ode_model = OdeModel::new(&model).expect("solve model should build");
    let mut y = Vec::new();
    let mut p = model.parameters.clone();

    apply_event_updates(&runtime, &ode_model, &mut y, &mut p, 1.0, 1.0e-12)
        .expect("MLS Appendix B event iteration should include runtime alias equations");

    // MLS Appendix B: all discrete equations are solved with the same pre(..)
    // snapshot for an event-iteration pass. Alias equations produced as
    // runtime assignments must therefore be refreshed before dependent
    // change(..)/edge(..) guards advance to the next pre snapshot.
    assert_eq!(&p[..3], [3.0, 3.0, 7.0]);
}

#[test]
fn event_update_refreshes_runtime_aliases_before_parameter_only_projection() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.solver_maps.names = vec!["dummy".to_string()];
    model.initial_y = vec![0.0];
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::LoadP { dst: 1, index: 1 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_span!(),
        ),
    );
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.discrete.runtime_assignment_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.runtime_assignment_rhs =
        solve::ScalarProgramBlock::with_source_span(vec![load_parameter_row(2)], fixture_span!());
    model.parameters = vec![0.0, 2.0, 2.0];

    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let ode_model = OdeModel::new(&model).expect("ODE model should build");
    let mut y = model.initial_y.clone();
    let mut p = model.parameters.clone();

    apply_event_updates(&runtime, &ode_model, &mut y, &mut p, 0.0, 1.0e-12)
        .expect("runtime aliases should settle before parameter-only residual projection");

    assert_eq!(p[0], 2.0);
}
