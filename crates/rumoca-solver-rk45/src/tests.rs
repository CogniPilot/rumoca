use indexmap::IndexMap;
use rumoca_ir_solve::{
    ComputeBlock, LinearOp, ScalarProgramBlock, SolveLayout, SolveProblem, SolverNameIndexMaps,
};

use super::*;

macro_rules! fixture_span {
    () => {
        rumoca_ir_solve::source_span_from_offsets(51, 0, 1)
    };
}

macro_rules! scalar_program_block {
    ($rows:expr, $span:expr $(,)?) => {{
        let span = $span;
        ScalarProgramBlock::with_source_span(
            $rows,
            span.require_provenance("RK45 fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable")
    }};
}

fn periodic_schedule(period: f64, phase: f64) -> solve::PeriodicEventSchedule {
    solve::PeriodicEventSchedule::from_seconds(period, phase).unwrap()
}

fn advance_by(session: &mut SimulationSession, dt: f64, context: &str) {
    session.step(dt).expect(context);
}

#[test]
fn combine_stage_rejects_short_stage_vectors() {
    let err = combine_stage(&[1.0, 2.0], 0.1, &[(&[3.0], 1.0)])
        .expect_err("stage length mismatch should fail");

    assert!(
        err.to_string().contains("RK45 stage 0 length"),
        "unexpected error: {err}"
    );
}

#[test]
fn error_norm_rejects_mismatched_estimate_vectors() {
    let err = error_norm(&[1.0, 2.0], &[1.0], &[1.0, 2.0], &[1.0e-6, 1.0e-6], 1.0e-6)
        .expect_err("estimate length mismatch should fail");

    assert!(
        err.to_string().contains("high-order estimate length"),
        "unexpected error: {err}"
    );
}

#[test]
fn error_norm_uses_per_variable_absolute_tolerances() {
    let norm = error_norm(
        &[0.0, 0.0],
        &[1.0e-3, 1.0e-3],
        &[0.0, 0.0],
        &[1.0e-2, 1.0e-6],
        0.0,
    )
    .expect("aligned estimates and tolerances should produce a norm");

    assert!((norm - 1.0e3).abs() <= 1.0e-9);
}

#[test]
fn rk45_simulates_solve_ir_integrator() {
    let model = single_state_model(vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("rk45 simulation should succeed");

    let final_x = result.data[0][result.data[0].len() - 1];
    assert!((final_x - 0.2_f64.exp()).abs() <= 1.0e-4);
}

#[test]
fn rk45_integrates_continuous_transport_delay_from_accepted_history() {
    let model = delayed_ramp_model();
    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.05),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("RK45 should integrate a continuous transport delay");

    let final_x = result.data[0][result.data[0].len() - 1];
    let final_delayed_x = result.data[1][result.data[1].len() - 1];
    assert!((final_x - 1.0).abs() <= 1.0e-6, "x(1)={final_x}");
    assert!(
        (final_delayed_x - 0.8).abs() <= 1.0e-5,
        "delay(x, 0.2) at t=1 was {final_delayed_x}"
    );
}

#[test]
fn rk45_localizes_variable_discrete_delay_event_at_query_time() {
    let model = variable_discrete_delay_model();
    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.8,
            dt: Some(0.1),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("RK45 should localize the delayed discrete discontinuity");

    assert_eq!(
        result.data[0].last().copied(),
        Some(2.0),
        "the marker update must run at t=0.6, where t - (0.1 + 0.5*t) = 0.2"
    );
}

#[test]
fn rk45_sets_terminal_marker_only_at_final_event() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.terminal_event_parameter_index = Some(0);
    model.problem.events.has_terminal_event = true;
    model.problem.discrete.rhs = ScalarProgramBlock::default();
    model.parameters = vec![0.0];
    model.visible_names = vec!["terminal_marker".to_string()];
    model.visible_value_rows = scalar_program_block!(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.05),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("RK45 should process the terminal event at the horizon");

    assert_eq!(result.data[0][0], 0.0);
    assert_eq!(result.data[0][result.data[0].len() - 1], 1.0);
}

#[test]
fn rk45_emits_one_series_per_visible_name() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["c[1]".to_string()];
    model.parameters = vec![1.0];
    model.visible_names = vec!["x".to_string(), "c[1]".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.05),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("rk45 simulation should succeed");

    assert_eq!(result.names, ["x", "c[1]"]);
    assert_eq!(
        result.data.len(),
        result.names.len(),
        "simulation payload requires one data series per visible name"
    );
    assert!(
        result.data[1]
            .iter()
            .all(|value| (*value - 1.0).abs() <= f64::EPSILON)
    );
}

#[test]
fn rk45_refreshes_algebraic_solve_ir_layout() {
    let mut model = single_state_model(vec![
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::Const { dst: 2, value: 2.0 },
            LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ],
    ]);
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "a".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        IndexMap::from([("x".to_string(), 0), ("a".to_string(), 1)]);
    model.problem.solve_layout.solver_maps.base_to_indices =
        IndexMap::from([("x".to_string(), vec![0]), ("a".to_string(), vec![1])]);
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
        }],
    };
    model.initial_y = vec![1.0, 2.0];
    model.visible_names = vec!["x".to_string(), "a".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.01),
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("rk45 should refresh explicit algebraic slots");

    let final_x = result.data[0][result.data[0].len() - 1];
    let final_a = result.data[1][result.data[1].len() - 1];
    assert!((final_x - 0.2_f64.exp()).abs() <= 1.0e-4);
    assert!((final_a - 2.0 * final_x).abs() <= 1.0e-8);
}

#[test]
fn rk45_snapshots_pre_params_before_event_updates() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.initial_y = vec![10.0];
    model.parameters = vec![0.0];
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
        dest_p_index: 0,
        source: solve::PreParamSource::Y { index: 0 },
        clock_schedule: None,
    }];
    model.problem.events.scheduled_time_events = vec![0.05];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_y(0),
        solve::DiscreteEventPreMode::EventEntry,
        scalar_program_block!(
            vec![vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const {
                    dst: 1,
                    value: -0.8,
                },
                LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_span!(),
        ),
    );

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("rk45 should snapshot pre(v) before applying event updates");

    assert_eq!(result.times, vec![0.0, 0.05, 0.1]);
    assert_eq!(result.data[0], vec![10.0, -8.0, -8.0]);
}

#[test]
fn rk45_terminate_returns_partial_success() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.initial_y = vec![0.0];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    model.problem.discrete.rhs = ScalarProgramBlock::default();
    model.problem.events.action_conditions = const_scalar_program_block(1.0);
    model.problem.events.actions = vec![solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Terminate,
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text("finished".to_string())],
        },
        span: solve::SolveVariableMeta::empty_with_span(fixture_span!()).source_span,
        origin: "terminate(\"finished\")".to_string(),
    }];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("Modelica terminate should return partial simulation data");

    let termination = result.termination.expect("termination metadata");
    assert_eq!(termination.message, "finished");
    assert!((termination.time - 0.05).abs() <= 1.0e-6);
    assert!((result.times[result.times.len() - 1] - 0.05).abs() <= 1.0e-6);
    assert_eq!(result.data[0].len(), result.times.len());
}

#[test]
fn rk45_applies_scheduled_time_event_update() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.events.scheduled_time_events = vec![0.05];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(0),
        solve::DiscreteEventPreMode::FollowCurrent,
        const_scalar_program_block(2.0),
    );
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("rk45 should handle scheduled Solve-IR events");

    assert_eq!(result.times, vec![0.0, 0.05, 0.1]);
    assert_eq!(result.data[1], vec![0.0, 2.0, 2.0]);
}

#[test]
fn rk45_simulate_records_initialization_updates_at_start_sample() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.initial_y = vec![1.0];
    model.problem.initialization.update_targets = vec![solve::scalar_slot_y(0)];
    model.problem.initialization.update_rhs = const_scalar_program_block(4.0);

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("rk45 simulate should apply initialization before first sample");

    assert_eq!(result.data[0], vec![4.0, 4.0]);
}

#[test]
fn rk45_applies_root_event_update() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.initial_y = vec![0.0];
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(0),
        solve::DiscreteEventPreMode::FollowCurrent,
        const_scalar_program_block(2.0),
    );
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("rk45 should locate root events with Solve-IR residual rows");

    assert!((result.data[0][1] - 0.1).abs() <= 1.0e-6);
    assert_eq!(result.data[1], vec![0.0, 2.0]);
}

#[test]
fn rk45_dense_root_localization_selects_earliest_crossing_and_state() {
    let mut model = single_state_model(vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.initial_y = vec![1.0];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![root_threshold_row(1.08), root_threshold_row(1.05)],
        fixture_span!(),
    );
    let runtime = SolveRuntime::new(&model).expect("root localization model should prepare");
    let backend = Rk45Backend::new(
        Rc::new(runtime),
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.1),
            atol: 1.0e-10,
            rtol: 1.0e-10,
            ..Default::default()
        },
    )
    .expect("RK45 backend should initialize");
    let old_state = vec![1.0];
    let trial = backend
        .trial_step_from(0.0, &old_state, 0.1, None)
        .expect("DOPRI5 trial should evaluate");
    let dense_output = Dopri5DenseOutput::new(
        0.0,
        0.1,
        &old_state,
        trial.stages.each_ref().map(Vec::as_slice),
    )
    .expect("accepted stages should build dense output");
    let old_roots = backend
        .eval_root_conditions(0.0, &old_state)
        .expect("left roots should evaluate");
    let new_roots = backend
        .eval_root_conditions(0.1, &trial.y_next)
        .expect("right roots should evaluate");
    let crossings = [
        RootCrossing {
            index: 0,
            post_relation_memory_value: 1.0,
        },
        RootCrossing {
            index: 1,
            post_relation_memory_value: 1.0,
        },
    ];

    let (root, simultaneous) = backend
        .locate_step_roots(
            &dense_output,
            RootLocalizationInput {
                old_roots: &old_roots,
                new_roots: &new_roots,
                crossings: &crossings,
                event_boundary: None,
            },
        )
        .expect("dense output should localize both roots");

    let expected_time = 1.05_f64.ln();
    assert!(
        (root.time - expected_time).abs() < 1.0e-8,
        "root time {} differs from {expected_time}",
        root.time
    );
    assert!(
        (root.state[0] - 1.05).abs() < 1.0e-8,
        "root state should lie on the earliest surface: {}",
        root.state[0]
    );
    assert_eq!(
        simultaneous,
        [crossings[1]],
        "the later index-0 crossing must not hide the earlier index-1 root"
    );
}

fn root_threshold_row(threshold: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const {
            dst: 1,
            value: threshold,
        },
        LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

#[test]
fn rk45_root_event_updates_relation_memory_for_continuous_if_branch() {
    let mut model = single_state_model(vec![vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.5 },
        LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::Const {
            dst: 4,
            value: -1.0,
        },
        LinearOp::Select {
            dst: 5,
            cond: 2,
            if_true: 3,
            if_false: 4,
        },
        LinearOp::StoreOutput { src: 5 },
    ]]);
    model.initial_y = vec![0.1];
    model.parameters = vec![0.0];
    model.problem.solve_layout.compiled_parameter_len = 1;
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(0),
        solve::DiscreteEventPreMode::FollowCurrent,
        scalar_program_block!(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Compare {
                    dst: 2,
                    op: solve::CompareOp::Lt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_span!(),
        ),
    );
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.events.root_relation_memory_targets = vec![Some(solve::scalar_slot_p(0))];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.2,
            dt: Some(0.2),
            ..Default::default()
        },
    )
    .expect("rk45 should update relation memory after root events");

    let final_x = result.data[0][result.data[0].len() - 1];
    assert!(
        final_x > 0.05,
        "relation memory should flip the continuous branch after root crossing; x={final_x}"
    );
}

#[test]
fn rk45_applies_periodic_event_update() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.clocks.periodic_event_schedules = vec![periodic_schedule(0.05, 0.05)];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(0),
        solve::DiscreteEventPreMode::FollowCurrent,
        const_scalar_program_block(3.0),
    );
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("rk45 should handle periodic Solve-IR events");

    assert_eq!(result.data[1], vec![0.0, 3.0]);
}

#[test]
fn rk45_periodic_event_seeds_scheduled_sample_relation_memory() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 3;
    model.problem.solve_layout.discrete_valued_scalar_names = vec![
        "sample_a".to_string(),
        "sample_b".to_string(),
        "m".to_string(),
    ];
    model.problem.solve_layout.relation_memory_parameter_indices = vec![0, 1];
    model.problem.clocks.periodic_event_schedules =
        vec![periodic_schedule(0.05, 0.05), periodic_schedule(0.07, 0.07)];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    model.problem.events.root_relation_memory_targets =
        vec![Some(solve::scalar_slot_p(0)), Some(solve::scalar_slot_p(1))];
    model.problem.events.scheduled_root_conditions = vec![
        solve::ScheduledRootCondition {
            root_index: 0,
            period_seconds: 0.05,
            phase_seconds: 0.05,
        },
        solve::ScheduledRootCondition {
            root_index: 1,
            period_seconds: 0.07,
            phase_seconds: 0.07,
        },
    ];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(2),
        solve::DiscreteEventPreMode::FollowCurrent,
        scalar_program_block!(
            vec![vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const {
                    dst: 1,
                    value: 10.0,
                },
                LinearOp::LoadP { dst: 2, index: 1 },
                LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::Mul,
                    lhs: 1,
                    rhs: 2,
                },
                LinearOp::Binary {
                    dst: 4,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 3,
                },
                LinearOp::StoreOutput { src: 4 },
            ]],
            fixture_span!(),
        ),
    );
    model.parameters = vec![0.0, 0.0, 0.0];
    model.visible_names = vec![
        "x".to_string(),
        "sample_a".to_string(),
        "sample_b".to_string(),
        "m".to_string(),
    ];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.06,
            dt: Some(0.06),
            ..Default::default()
        },
    )
    .expect("rk45 should seed scheduled sample relation memory before event rows");

    assert_eq!(result.data[3], vec![0.0, 1.0]);
}

#[test]
fn rk45_clears_scheduled_sample_relation_memory_between_ticks() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 4;
    model.problem.solve_layout.discrete_valued_scalar_names = vec![
        "__pre__.sample".to_string(),
        "sample".to_string(),
        "count".to_string(),
        "__pre__.count".to_string(),
    ];
    model.problem.solve_layout.pre_param_bindings = vec![
        solve::PreParamBinding {
            dest_p_index: 0,
            source: solve::PreParamSource::P { index: 1 },
            clock_schedule: None,
        },
        solve::PreParamBinding {
            dest_p_index: 3,
            source: solve::PreParamSource::P { index: 2 },
            clock_schedule: None,
        },
    ];
    model.problem.clocks.periodic_event_schedules = vec![periodic_schedule(0.05, 0.0)];
    model.problem.events.root_conditions = scalar_program_block!(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.events.root_relation_memory_targets = vec![Some(solve::scalar_slot_p(1))];
    model.problem.events.scheduled_root_conditions = vec![solve::ScheduledRootCondition {
        root_index: 0,
        period_seconds: 0.05,
        phase_seconds: 0.0,
    }];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(2),
        solve::DiscreteEventPreMode::Fixed,
        scalar_program_block!(
            vec![vec![
                LinearOp::LoadP { dst: 0, index: 1 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Unary {
                    dst: 2,
                    op: solve::UnaryOp::Not,
                    arg: 1,
                },
                LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::And,
                    lhs: 0,
                    rhs: 2,
                },
                LinearOp::LoadP { dst: 4, index: 3 },
                LinearOp::Const { dst: 5, value: 1.0 },
                LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Add,
                    lhs: 4,
                    rhs: 5,
                },
                LinearOp::Select {
                    dst: 7,
                    cond: 3,
                    if_true: 6,
                    if_false: 4,
                },
                LinearOp::StoreOutput { src: 7 },
            ]],
            fixture_span!(),
        ),
    );
    // Mimic a phase-zero sample that already fired during initialization:
    // current sample memory is true and count has advanced once.
    model.parameters = vec![0.0, 1.0, 1.0, 1.0];
    model.visible_names = vec![
        "x".to_string(),
        "__pre__.sample".to_string(),
        "sample".to_string(),
        "count".to_string(),
        "__pre__.count".to_string(),
    ];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.11,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("rk45 should re-arm scheduled sample relation memory after each tick");

    assert_eq!(result.times, vec![0.0, 0.05, 0.1, 0.11]);
    assert_eq!(result.data[3], vec![1.0, 2.0, 3.0, 3.0]);
}

#[test]
fn rk45_applies_dynamic_time_event_update() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.discrete_real_scalar_names = vec!["next".to_string()];
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.events.dynamic_time_event_names = vec!["next".to_string()];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(1),
        solve::DiscreteEventPreMode::FollowCurrent,
        const_scalar_program_block(4.0),
    );
    model.parameters = vec![0.05, 0.0];
    model.visible_names = vec!["x".to_string(), "next".to_string(), "m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("rk45 should handle named dynamic Solve-IR time events");

    assert_eq!(result.data[2], vec![0.0, 4.0]);
}

#[test]
fn runtime_contract_step_until_advances_rk45_backend() {
    let prepared = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let model = SolveRuntime::new(&prepared).expect("valid runtime contract model should prepare");
    let mut backend = Rk45Backend::new(
        Rc::new(model),
        &SimOptions {
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .expect("backend should build");

    backend.init().expect("init should succeed");
    let outcome = backend.step_until(0.1).expect("backend should step");

    assert_eq!(outcome, StepUntilOutcome::StopReached);
    assert!((backend.read_state().t - 0.1).abs() <= 1.0e-12);
    assert!((backend.state[0] - 1.2).abs() <= 1.0e-6);
}

#[test]
fn rk45_tiny_final_remainder_is_not_clamped_past_target() {
    let time = 1.0;
    let target = time + 0.5 * MIN_STEP;
    let h = trial_step_size(time, target, 0.1, None).expect("final remainder is representable");

    assert_eq!(h, target - time);
    assert!(h < MIN_STEP);
    assert_eq!(time + h, target);
}

#[test]
fn rk45_tiny_final_remainder_keeps_state_and_time_consistent() {
    let prepared = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let runtime = Rc::new(SolveRuntime::new(&prepared).expect("runtime should prepare"));
    let start = 1.0;
    let target = start + 0.5 * MIN_STEP;
    let mut backend = Rk45Backend::new(
        runtime,
        &SimOptions {
            t_start: start,
            t_end: target,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("backend should build");
    backend.init().expect("backend should initialize");

    backend
        .step_until(target)
        .expect("tiny final remainder should be accepted");

    assert_eq!(backend.time, target);
    assert_eq!(backend.state[0], 1.0 + 2.0 * (target - start));
}

#[test]
fn rk45_rejected_minimum_step_reports_underflow() {
    assert!(matches!(
        rejected_step_size(MIN_STEP, 2.0, 1.0),
        Err(SimError::StepSizeUnderflow { target_t: 1.0 })
    ));
}

#[test]
fn rk45_session_reset_restores_cached_initial_state() {
    let mut model = single_state_model(vec![vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()];
    model.parameters = vec![0.0];
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .expect("session should build");

    session.set_input("u", 4.0).expect("input should exist");
    advance_by(&mut session, 0.1, "session should advance");
    let advanced_x = session
        .get("x")
        .expect("session read should succeed")
        .expect("x should be visible");
    assert!(advanced_x > 1.1);

    session
        .reset(12.5)
        .expect("reset should restore cached initial state");

    assert!((session.time() - 12.5).abs() <= 1.0e-12);
    let reset_x = session
        .get("x")
        .expect("session read should succeed")
        .expect("x should be visible");
    assert!((reset_x - 1.0).abs() <= 1.0e-12);
    assert_eq!(
        session.get("u").expect("session read should succeed"),
        None,
        "reset should clear stale input overrides"
    );
}

#[test]
fn rk45_session_advance_to_clamps_to_sim_options_end_time() {
    let mut model = single_state_model(vec![vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()];
    model.parameters = vec![0.0];
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            t_end: 0.05,
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .expect("session should build");

    session.set_input("u", 4.0).expect("input should exist");
    session
        .step(0.1)
        .expect("step past final time should clamp");

    assert!(
        (session.time() - 0.05).abs() <= 1.0e-12,
        "session should stop at t_end, got t={}",
        session.time()
    );
}

#[test]
fn rk45_incremental_session_can_extend_past_initial_end_time() {
    let mut model = single_state_model(vec![vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()];
    model.parameters = vec![0.0];
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            t_end: 0.05,
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .expect("session should build");

    session.set_input("u", 4.0).expect("input should exist");
    session.ensure_end_time(0.1);
    session
        .advance_to(0.1)
        .expect("extended session should advance");

    assert!((session.time() - 0.1).abs() <= 1.0e-12);
}

#[test]
fn rk45_session_extension_schedules_events_beyond_initial_end_time() {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.clocks.periodic_event_schedules = vec![periodic_schedule(0.05, 0.05)];
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(0),
        solve::DiscreteEventPreMode::FollowCurrent,
        const_scalar_program_block(3.0),
    );
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "m".to_string()];
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            t_end: 0.04,
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .expect("session should build");

    session.ensure_end_time(0.1);
    session
        .advance_to(0.1)
        .expect("extended session should process periodic events");

    assert_eq!(session.get("m").expect("read m"), Some(3.0));
}

#[test]
fn rk45_session_runs_no_state_discrete_controller() {
    let model = no_state_input_accumulator_model();
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            t_end: 0.05,
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        },
    )
    .expect("no-state session should build");

    session.set_input("u", 1.5).expect("input should exist");
    session.step(0.02).expect("first controller tick");
    assert!((session.time() - 0.02).abs() <= 1.0e-12);
    assert_eq!(session.get("y").expect("read y"), Some(1.5));

    session.set_input("u", 2.0).expect("input should update");
    session.step(0.02).expect("second controller tick");
    assert_eq!(session.get("y").expect("read y"), Some(3.5));

    session.step(1.0).expect("step should clamp at t_end");
    assert!((session.time() - 0.05).abs() <= 1.0e-12);
    assert_eq!(session.get("y").expect("read y"), Some(5.5));

    session
        .step(1.0)
        .expect("further steps at t_end should be stable");
    assert_eq!(session.get("y").expect("read y"), Some(5.5));

    session.ensure_end_time(0.07);
    session.step(0.02).expect("extended controller should tick");
    assert!((session.time() - 0.07).abs() <= 1.0e-12);
    assert_eq!(session.get("y").expect("read y"), Some(7.5));
}

#[test]
fn rk45_session_uses_adaptive_event_integration_for_stiff_contact() {
    let model = stiff_contact_model();
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.02),
            ..Default::default()
        },
    )
    .expect("stiff contact session should initialize");

    for _ in 0..50 {
        advance_by(&mut session, 0.02, "session should advance");
    }

    let x = session
        .get("x")
        .expect("session read should succeed")
        .expect("x should be visible");
    let contact = session
        .get("contact")
        .expect("session read should succeed")
        .expect("contact should be visible");
    assert!(
        x > -0.01 && x < 0.03,
        "adaptive session should settle near the contact surface without frame-step penetration; x={x}"
    );
    assert_eq!(contact, 1.0);
}

#[test]
fn rk45_session_redetects_contact_after_thrust_liftoff() {
    let model = stiff_contact_model();
    let mut session = SimulationSession::new(
        &model,
        SimOptions {
            solver_mode: SimSolverMode::RkLike,
            dt: Some(0.02),
            t_end: 6.0,
            ..Default::default()
        },
    )
    .expect("stiff contact session should initialize");

    for _ in 0..50 {
        advance_by(&mut session, 0.02, "initial settle should advance");
    }
    session
        .set_input("thrust", 40.0)
        .expect("thrust input should exist");
    for _ in 0..10 {
        advance_by(&mut session, 0.02, "liftoff should advance");
    }
    let lifted_x = session
        .get("x")
        .expect("session read should succeed")
        .expect("x should be visible");
    assert!(
        lifted_x > 0.05,
        "thrust phase should lift the mass above the contact surface; x={lifted_x}"
    );

    session
        .set_input("thrust", 0.0)
        .expect("thrust input should update");
    for _ in 0..180 {
        advance_by(&mut session, 0.02, "descent should advance");
    }

    let x = session
        .get("x")
        .expect("session read should succeed")
        .expect("x should be visible");
    let contact = session
        .get("contact")
        .expect("session read should succeed")
        .expect("contact should be visible");
    assert!(
        x > -0.02 && x < 0.04,
        "re-contact after liftoff should not miss the ground event; x={x}"
    );
    assert_eq!(contact, 1.0);
}

// SPEC_0021: Exception - test fixture declares the full Solve-IR model
// inline so contact/event semantics stay visible in one place.
#[allow(clippy::too_many_lines)]
fn stiff_contact_model() -> solve::SolveModel {
    let dx = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let dv = vec![
        LinearOp::LoadP { dst: 0, index: 2 },
        LinearOp::Const { dst: 1, value: 0.5 },
        LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Const {
            dst: 3,
            value: -9.81,
        },
        LinearOp::LoadP { dst: 4, index: 1 },
        LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 3,
            rhs: 4,
        },
        LinearOp::LoadY { dst: 6, index: 0 },
        LinearOp::Const {
            dst: 7,
            value: -30000.0,
        },
        LinearOp::Binary {
            dst: 8,
            op: solve::BinaryOp::Mul,
            lhs: 6,
            rhs: 7,
        },
        LinearOp::LoadY { dst: 9, index: 1 },
        LinearOp::Const {
            dst: 10,
            value: -200.0,
        },
        LinearOp::Binary {
            dst: 11,
            op: solve::BinaryOp::Mul,
            lhs: 9,
            rhs: 10,
        },
        LinearOp::Binary {
            dst: 12,
            op: solve::BinaryOp::Add,
            lhs: 5,
            rhs: 8,
        },
        LinearOp::Binary {
            dst: 13,
            op: solve::BinaryOp::Add,
            lhs: 12,
            rhs: 11,
        },
        LinearOp::Select {
            dst: 14,
            cond: 2,
            if_true: 13,
            if_false: 5,
        },
        LinearOp::StoreOutput { src: 14 },
    ];
    solve::SolveModel {
        problem: SolveProblem {
            schema_version: solve::SOLVE_SCHEMA_VERSION,
            layout: solve::VarLayout::from_parts(Default::default(), 2, 1),
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: ComputeBlock::from_scalar_program_block(scalar_program_block!(
                    vec![dx.clone(), dv.clone()],
                    fixture_span!(),
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                residual: ComputeBlock::from_scalar_program_block(scalar_program_block!(
                    vec![dx.clone(), dv.clone()],
                    fixture_span!(),
                )),
                derivative_rhs: ComputeBlock::from_scalar_program_block(scalar_program_block!(
                    vec![dx, dv],
                    fixture_span!(),
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                manifold_residual: ComputeBlock::default(),
                manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
            },
            initialization: solve::InitializationSolveSystem::default(),
            discrete: solve::DiscreteSolveSystem {
                rhs: scalar_program_block!(
                    vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::Const { dst: 1, value: 0.0 },
                        LinearOp::Compare {
                            dst: 2,
                            op: solve::CompareOp::Lt,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ]],
                    fixture_span!(),
                ),
                update_targets: vec![solve::scalar_slot_p(2)],
                row_roles: vec![solve::DiscreteRowRole::ConditionMemory],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                clock_owners: vec![None],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: scalar_program_block!(
                    vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ]],
                    fixture_span!(),
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(2))],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                ..Default::default()
            },
            clocks: solve::SolveClockPartition::default(),
            solve_layout: SolveLayout {
                solver_maps: SolverNameIndexMaps {
                    names: vec!["x".to_string(), "v".to_string()],
                    name_to_idx: IndexMap::from([("x".to_string(), 0), ("v".to_string(), 1)]),
                    base_to_indices: IndexMap::from([
                        ("x".to_string(), vec![0]),
                        ("v".to_string(), vec![1]),
                    ]),
                },
                variable_base_slots: Vec::new(),
                state_scalar_count: 2,
                algebraic_scalar_count: 0,
                output_scalar_count: 0,
                parameter_count: 1,
                compiled_parameter_len: 3,
                input_scalar_names: vec!["thrust".to_string()],
                discrete_real_scalar_names: Vec::new(),
                discrete_valued_scalar_names: vec!["contact".to_string()],
                relation_memory_parameter_indices: Vec::new(),
                initial_event_parameter_index: None,
                initial_homotopy_parameter_index: None,
                terminal_event_parameter_index: None,
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts::default(),
            ..Default::default()
        },
        initial_y: vec![0.02, 0.0],
        solver_nominals: vec![1.0, 1.0],
        parameters: vec![0.0, 0.0, 0.0],
        external_tables: solve::ExternalTables::default(),
        visible_names: vec!["x".to_string(), "v".to_string(), "contact".to_string()],
        visible_value_rows: solve::ScalarProgramBlock::default(),
        variable_meta: Vec::new(),
    }
}

fn single_state_model(rhs_rows: Vec<Vec<LinearOp>>) -> solve::SolveModel {
    let derivative_rows = rhs_rows.iter().take(1).cloned().collect::<Vec<_>>();
    let zero = scalar_program_block!(
        vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    solve::SolveModel {
        problem: SolveProblem {
            schema_version: solve::SOLVE_SCHEMA_VERSION,
            layout: solve::VarLayout::from_parts(Default::default(), 1, 1),
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: ComputeBlock::from_scalar_program_block(scalar_program_block!(
                    rhs_rows.clone(),
                    fixture_span!(),
                )),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                residual: ComputeBlock::from_scalar_program_block(scalar_program_block!(
                    rhs_rows.clone(),
                    fixture_span!(),
                )),
                derivative_rhs: ComputeBlock::from_scalar_program_block(scalar_program_block!(
                    derivative_rows,
                    fixture_span!(),
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                manifold_residual: ComputeBlock::default(),
                manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
            },
            initialization: solve::InitializationSolveSystem {
                residual: ComputeBlock::from_scalar_program_block(zero.clone()),
                row_targets: Vec::new(),
                projection_unknowns: Vec::new(),
                projection_plan: solve::InitializationProjectionPlan::default(),
                update_rhs: solve::ScalarProgramBlock::default(),
                update_targets: Vec::new(),
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: zero.clone(),
                ..Default::default()
            },
            events: solve::SolveEventPartition::default(),
            clocks: solve::SolveClockPartition::default(),
            solve_layout: SolveLayout {
                solver_maps: SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                    base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                },
                variable_base_slots: Vec::new(),
                state_scalar_count: 1,
                algebraic_scalar_count: 0,
                output_scalar_count: 0,
                parameter_count: 0,
                compiled_parameter_len: 0,
                input_scalar_names: Vec::new(),
                discrete_real_scalar_names: Vec::new(),
                discrete_valued_scalar_names: Vec::new(),
                relation_memory_parameter_indices: Vec::new(),
                initial_event_parameter_index: None,
                initial_homotopy_parameter_index: None,
                terminal_event_parameter_index: None,
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts {
                structural: solve::ContinuousStructuralArtifacts::default(),
                mass_matrix: solve::MassMatrix::Identity,
                implicit_jacobian_v: ComputeBlock::from_scalar_program_block(zero.clone()),
                implicit_jacobian_v_scalar: zero.clone(),
                manifold_jacobian_v: ComputeBlock::default(),
                full_jacobian_v: zero.clone(),
            },
            ..Default::default()
        },
        initial_y: vec![1.0],
        solver_nominals: vec![1.0],
        parameters: Vec::new(),
        external_tables: solve::ExternalTables::default(),
        visible_names: vec!["x".to_string()],
        visible_value_rows: solve::ScalarProgramBlock::default(),
        variable_meta: Vec::new(),
    }
}

fn delayed_ramp_model() -> solve::SolveModel {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let source = scalar_program_block!(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    let delay = scalar_program_block!(
        vec![vec![
            LinearOp::Const { dst: 0, value: 0.2 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.events.delays = solve::SolveDelayPartition {
        source_rhs: source,
        delay_time_rhs: delay.clone(),
        delay_max_rhs: delay,
        value_parameter_indices: vec![0],
        source_is_discrete: vec![false],
    };
    model.initial_y = vec![0.0];
    model.parameters = vec![0.0];
    model.visible_names = vec!["x".to_string(), "delayed_x".to_string()];
    model.visible_value_rows = scalar_program_block!(
        vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span!(),
    );
    model
}

fn variable_discrete_delay_model() -> solve::SolveModel {
    let mut model = single_state_model(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let source = scalar_program_block!(
        vec![vec![
            LinearOp::LoadTime { dst: 0 },
            LinearOp::Const { dst: 1, value: 0.2 },
            LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Ge,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        fixture_span!(),
    );
    let delay_time = scalar_program_block!(
        vec![vec![
            LinearOp::LoadTime { dst: 0 },
            LinearOp::Const { dst: 1, value: 0.5 },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Const { dst: 3, value: 0.1 },
            LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Add,
                lhs: 2,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ]],
        fixture_span!(),
    );
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 1, 2);
    model.problem.solve_layout.parameter_count = 2;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.discrete_real_scalar_names = vec!["marker".to_string()];
    model.problem.events.scheduled_time_events = vec![0.2];
    model.problem.events.delays = solve::SolveDelayPartition {
        source_rhs: source,
        delay_time_rhs: delay_time,
        delay_max_rhs: const_scalar_program_block(1.0),
        value_parameter_indices: vec![0],
        source_is_discrete: vec![true],
    };
    set_single_discrete_equation(
        &mut model,
        solve::scalar_slot_p(1),
        solve::DiscreteEventPreMode::FollowCurrent,
        scalar_program_block!(
            vec![vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 0.5 },
                LinearOp::Compare {
                    dst: 2,
                    op: solve::CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::Const { dst: 3, value: 2.0 },
                LinearOp::LoadP { dst: 4, index: 1 },
                LinearOp::Select {
                    dst: 5,
                    cond: 2,
                    if_true: 3,
                    if_false: 4,
                },
                LinearOp::StoreOutput { src: 5 },
            ]],
            fixture_span!(),
        ),
    );
    model.parameters = vec![0.0, 0.0];
    model.visible_names = vec!["marker".to_string()];
    model.visible_value_rows = scalar_program_block!(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    model
}

fn const_scalar_program_block(value: f64) -> ScalarProgramBlock {
    scalar_program_block!(
        vec![vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    )
}

fn set_single_discrete_equation(
    model: &mut solve::SolveModel,
    target: solve::ScalarSlot,
    pre_mode: solve::DiscreteEventPreMode,
    rhs: ScalarProgramBlock,
) {
    model.problem.discrete = solve::DiscreteSolveSystem {
        update_targets: vec![target],
        row_roles: vec![solve::DiscreteRowRole::Equation],
        pre_modes: vec![pre_mode],
        observation_refresh: vec![false],
        clock_owners: vec![None],
        rhs,
        ..solve::DiscreteSolveSystem::default()
    };
}

fn no_state_input_accumulator_model() -> solve::SolveModel {
    let zero = const_scalar_program_block(0.0);
    let preserve_y = scalar_program_block!(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    );
    solve::SolveModel {
        problem: SolveProblem {
            schema_version: solve::SOLVE_SCHEMA_VERSION,
            layout: solve::VarLayout::from_parts(Default::default(), 0, 1),
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: ComputeBlock::from_scalar_program_block(preserve_y.clone()),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                residual: ComputeBlock::from_scalar_program_block(preserve_y.clone()),
                derivative_rhs: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::default(),
                ),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                manifold_residual: ComputeBlock::default(),
                manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
            },
            initialization: solve::InitializationSolveSystem::default(),
            discrete: solve::DiscreteSolveSystem {
                rhs: scalar_program_block!(
                    vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::LoadP { dst: 1, index: 0 },
                        LinearOp::Binary {
                            dst: 2,
                            op: solve::BinaryOp::Add,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ]],
                    fixture_span!(),
                ),
                update_targets: vec![solve::scalar_slot_y(0)],
                row_roles: vec![solve::DiscreteRowRole::Equation],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                clock_owners: vec![None],
                ..Default::default()
            },
            events: solve::SolveEventPartition::default(),
            clocks: solve::SolveClockPartition::default(),
            solve_layout: SolveLayout {
                solver_maps: SolverNameIndexMaps {
                    names: vec!["y".to_string()],
                    name_to_idx: IndexMap::from([("y".to_string(), 0)]),
                    base_to_indices: IndexMap::from([("y".to_string(), vec![0])]),
                },
                variable_base_slots: Vec::new(),
                state_scalar_count: 0,
                algebraic_scalar_count: 0,
                output_scalar_count: 1,
                parameter_count: 0,
                compiled_parameter_len: 1,
                input_scalar_names: vec!["u".to_string()],
                discrete_real_scalar_names: vec!["y".to_string()],
                discrete_valued_scalar_names: Vec::new(),
                relation_memory_parameter_indices: Vec::new(),
                initial_event_parameter_index: None,
                initial_homotopy_parameter_index: None,
                terminal_event_parameter_index: None,
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts {
                structural: solve::ContinuousStructuralArtifacts::default(),
                mass_matrix: solve::MassMatrix::Identity,
                implicit_jacobian_v: ComputeBlock::from_scalar_program_block(zero.clone()),
                implicit_jacobian_v_scalar: zero,
                manifold_jacobian_v: ComputeBlock::default(),
                full_jacobian_v: ScalarProgramBlock::default(),
            },
            ..Default::default()
        },
        initial_y: vec![0.0],
        solver_nominals: vec![1.0],
        parameters: vec![0.0],
        external_tables: solve::ExternalTables::default(),
        visible_names: vec!["y".to_string()],
        visible_value_rows: scalar_program_block!(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
        variable_meta: Vec::new(),
    }
}
