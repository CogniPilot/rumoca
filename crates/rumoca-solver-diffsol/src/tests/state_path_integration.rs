//! Continuous state-path integration tests.
//!
//! The state-only BDF eligibility rules, the initialization check that must
//! take the same branch the simulation build takes, the general/implicit
//! tableaus, transport delays replayed from accepted history, terminal events,
//! scheduled events on the state path, and the wall-clock budget.

use super::*;

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

/// `der(x) = -x` in the shape Solve lowers it to: the derivative program carries
/// the whole continuous system and `implicit_rhs` is empty *by design* (pinned by
/// `rumoca-phase-solve`'s `explicit_state_equation_lowers_to_derivative_program`),
/// so there is no implicit Jacobian pattern to derive.
fn explicit_decay_model() -> solve::SolveModel {
    let mut model = unit_integrator_model();
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Unary {
                    dst: 1,
                    op: solve::UnaryOp::Neg,
                    arg: 0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ]],
            fixture_span!(),
        ));
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::default();
    model.problem.continuous.implicit_row_targets = Vec::new();
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::default();
    model.initial_y = vec![1.0];
    model.visible_value_rows = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model
}

/// A pure explicit ODE has no implicit residual rows, so the general/implicit
/// path cannot derive the implicit Jacobian pattern it requires. The
/// initialization check must therefore take the *same* branch the simulation
/// build takes, and settle the model on the reduced state-only path.
#[test]
fn check_initialization_settles_pure_explicit_odes_on_the_state_only_path() {
    let model = explicit_decay_model();
    assert!(
        model.problem.continuous.implicit_rhs.is_empty(),
        "fixture must keep the pure-explicit shape Solve emits for der(x) = -x"
    );
    let opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        dt: Some(0.1),
        ..Default::default()
    };
    assert!(
        uses_state_only_path(&model, &opts).expect("valid model should check BDF eligibility"),
        "a pure explicit ODE integrates on the reduced state-only path"
    );

    check_initialization(&model, &opts).expect(
        "initialization must be checked on the path the simulation builds, not the implicit one",
    );

    // The check and the run agree: the model that passed the check also runs.
    let result = simulate(&model, &opts).expect("pure explicit ODE integrates");
    let x = *result
        .data
        .first()
        .and_then(|series| series.last())
        .expect("x series should have a final sample");
    assert!(
        (x - (-1.0f64).exp()).abs() <= 1.0e-5,
        "x(1) = {x} should match exp(-1)"
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

#[test]
fn bdf_integrates_continuous_transport_delay_from_accepted_history() {
    let model = delayed_ramp_model();
    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("BDF should integrate a continuous transport delay");

    let final_x = result.data[0][result.data[0].len() - 1];
    let final_delayed_x = result.data[1][result.data[1].len() - 1];
    assert!((final_x - 1.0).abs() <= 1.0e-6, "x(1)={final_x}");
    assert!(
        (final_delayed_x - 0.8).abs() <= 1.0e-5,
        "delay(x, 0.2) at t=1 was {final_delayed_x}"
    );
}

#[test]
fn bdf_sets_terminal_marker_only_at_final_event() {
    let model = terminal_marker_model();

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("BDF should process the terminal event at the horizon");

    assert_eq!(result.data[0][0], 0.0);
    assert_eq!(result.data[0][result.data[0].len() - 1], 1.0);
}

#[test]
fn bdf_session_processes_terminal_event_at_horizon() {
    let model = terminal_marker_model();
    let mut session = session::SimulationSession::new(
        &model,
        SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("BDF session should initialize");

    assert_eq!(
        session
            .get("terminal_marker")
            .expect("initial marker should evaluate"),
        Some(0.0)
    );
    session
        .advance_to(0.1)
        .expect("BDF session should process the terminal event");
    assert_eq!(
        session
            .get("terminal_marker")
            .expect("final marker should evaluate"),
        Some(1.0)
    );
}

fn terminal_marker_model() -> solve::SolveModel {
    let mut model = general_ramp_model();
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 1, 1);
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.terminal_event_parameter_index = Some(0);
    model.problem.events.has_terminal_event = true;
    model.parameters = vec![0.0];
    model.visible_names = vec!["terminal_marker".to_string()];
    model.visible_value_rows = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model
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
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ));
    model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ));
    model.artifacts.continuous.full_jacobian_v = scalar_program_block!(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.visible_value_rows = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model
}

fn delayed_ramp_model() -> solve::SolveModel {
    let mut model = general_ramp_model();
    let source = scalar_program_block!(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    let delay = scalar_program_block!(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 1, 1);
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.events.delays = solve::SolveDelayPartition {
        source_rhs: source,
        delay_time_rhs: delay.clone(),
        delay_max_rhs: delay,
        value_parameter_indices: vec![0],
        source_is_discrete: vec![false],
    };
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "delayed_x".to_string()];
    model.visible_value_rows = scalar_program_block!(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
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
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ));
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![
                state_residual_row(),
                y_minus_y_row(1, 0),
                y_minus_y_row(2, 1),
            ],
            fixture_span!(),
        ));
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![
            solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
            },
            solve::AlgebraicProjectionBlock {
                rows: vec![2],
                y_indices: vec![2],
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
    model.problem.clocks.periodic_event_schedules = vec![periodic_schedule(10.0, 0.05)];

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
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ));
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(scalar_program_block!(
            vec![state_residual_row(), y_minus_y_row(1, 0)],
            fixture_span!(),
        ));
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
        }],
    };
    model.initial_y = vec![0.0, 0.0];
    model
}

#[test]
fn default_bdf_honors_wall_clock_budget() {
    let error = simulate(
        &unit_integrator_model(),
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.1),
            max_wall_seconds: Some(f64::MIN_POSITIVE),
            ..Default::default()
        },
    )
    .expect_err("the default BDF path must enforce max_wall_seconds");

    // The budget is enforced inside the integration loop, so the failure is
    // annotated with the stage that raised it; the variant is unchanged.
    assert!(matches!(error.kind(), SimError::Timeout { .. }));
    assert!(
        error.stage().is_some(),
        "an in-loop failure must record the stage that raised it"
    );
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
