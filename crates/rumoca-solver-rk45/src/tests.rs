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
    let err = error_norm(&[1.0, 2.0], &[1.0], &[1.0, 2.0], 1.0e-6, 1.0e-6)
        .expect_err("estimate length mismatch should fail");

    assert!(
        err.to_string().contains("high-order estimate length"),
        "unexpected error: {err}"
    );
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
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
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
    }];
    model.problem.events.scheduled_time_events = vec![0.05];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_y(0)];
    model.problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::EventEntry];
    model.problem.discrete.rhs = ScalarProgramBlock::with_source_span(
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
    model.problem.events.root_conditions = ScalarProgramBlock::with_source_span(
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = const_scalar_program_block(2.0);
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
    model.problem.events.root_conditions = ScalarProgramBlock::with_source_span(
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = const_scalar_program_block(2.0);
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = ScalarProgramBlock::with_source_span(
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
    );
    model.problem.events.root_conditions = ScalarProgramBlock::with_source_span(
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
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        period_seconds: 0.05,
        phase_seconds: 0.05,
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = const_scalar_program_block(3.0);
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
    model.problem.clocks.periodic_event_schedules = vec![
        solve::PeriodicEventSchedule {
            period_seconds: 0.05,
            phase_seconds: 0.05,
        },
        solve::PeriodicEventSchedule {
            period_seconds: 0.07,
            phase_seconds: 0.07,
        },
    ];
    model.problem.events.root_conditions = ScalarProgramBlock::with_source_span(
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(2)];
    model.problem.discrete.rhs = ScalarProgramBlock::with_source_span(
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
        },
        solve::PreParamBinding {
            dest_p_index: 3,
            source: solve::PreParamSource::P { index: 2 },
        },
    ];
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        period_seconds: 0.05,
        phase_seconds: 0.0,
    }];
    model.problem.events.root_conditions = ScalarProgramBlock::with_source_span(
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(2)];
    model.problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::Fixed];
    model.problem.discrete.rhs = ScalarProgramBlock::with_source_span(
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
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(1)];
    model.problem.discrete.rhs = const_scalar_program_block(4.0);
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
        &model,
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
    model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
        period_seconds: 0.05,
        phase_seconds: 0.05,
    }];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.discrete.rhs = const_scalar_program_block(3.0);
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
                implicit_rhs: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::with_source_span(
                        vec![dx.clone(), dv.clone()],
                        fixture_span!(),
                    ),
                ),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                residual: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::with_source_span(
                        vec![dx.clone(), dv.clone()],
                        fixture_span!(),
                    ),
                ),
                derivative_rhs: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::with_source_span(vec![dx, dv], fixture_span!()),
                ),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
            },
            initialization: solve::InitializationSolveSystem::default(),
            discrete: solve::DiscreteSolveSystem {
                rhs: ScalarProgramBlock::with_source_span(
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
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: ScalarProgramBlock::with_source_span(
                    vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ]],
                    fixture_span!(),
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(2))],
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
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts::default(),
        },
        initial_y: vec![0.02, 0.0],
        parameters: vec![0.0, 0.0, 0.0],
        external_tables: solve::ExternalTables::default(),
        visible_names: vec!["x".to_string(), "v".to_string(), "contact".to_string()],
        visible_value_rows: solve::ScalarProgramBlock::default(),
        variable_meta: Vec::new(),
    }
}

fn single_state_model(rhs_rows: Vec<Vec<LinearOp>>) -> solve::SolveModel {
    let derivative_rows = rhs_rows.iter().take(1).cloned().collect::<Vec<_>>();
    let zero = ScalarProgramBlock::with_source_span(
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
                implicit_rhs: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::with_source_span(rhs_rows.clone(), fixture_span!()),
                ),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                residual: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::with_source_span(rhs_rows.clone(), fixture_span!()),
                ),
                derivative_rhs: ComputeBlock::from_scalar_program_block(
                    ScalarProgramBlock::with_source_span(derivative_rows, fixture_span!()),
                ),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
            },
            initialization: solve::InitializationSolveSystem {
                residual: ComputeBlock::from_scalar_program_block(zero.clone()),
                row_targets: Vec::new(),
                projection_indices: Vec::new(),
                projection_plan: solve::AlgebraicProjectionPlan::default(),
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
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts {
                mass_matrix: vec![vec![1.0]],
                implicit_jacobian_v: ComputeBlock::from_scalar_program_block(zero.clone()),
                implicit_jacobian_v_scalar: zero.clone(),
                full_jacobian_v: zero.clone(),
            },
        },
        initial_y: vec![1.0],
        parameters: Vec::new(),
        external_tables: solve::ExternalTables::default(),
        visible_names: vec!["x".to_string()],
        visible_value_rows: solve::ScalarProgramBlock::default(),
        variable_meta: Vec::new(),
    }
}

fn const_scalar_program_block(value: f64) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span!(),
    )
}

fn no_state_input_accumulator_model() -> solve::SolveModel {
    let zero = const_scalar_program_block(0.0);
    let preserve_y = ScalarProgramBlock::with_source_span(
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
            },
            initialization: solve::InitializationSolveSystem::default(),
            discrete: solve::DiscreteSolveSystem {
                rhs: ScalarProgramBlock::with_source_span(
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
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts {
                mass_matrix: Vec::new(),
                implicit_jacobian_v: ComputeBlock::from_scalar_program_block(zero.clone()),
                implicit_jacobian_v_scalar: zero,
                full_jacobian_v: ScalarProgramBlock::default(),
            },
        },
        initial_y: vec![0.0],
        parameters: vec![0.0],
        external_tables: solve::ExternalTables::default(),
        visible_names: vec!["y".to_string()],
        visible_value_rows: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span!(),
        ),
        variable_meta: Vec::new(),
    }
}
