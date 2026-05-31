use super::*;

#[test]
fn event_sample_replaces_near_duplicate_output_sample() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_real_scalar_names = vec!["next".to_string()];
    model.parameters = vec![5.0];
    model.visible_names = vec!["next".to_string()];
    let mut times = Vec::new();
    let mut data = vec![Vec::new()];

    record_sample_if_new(
        None,
        &model,
        &[],
        &[5.0],
        &mut times,
        &mut data,
        4.999999999999981,
    )
    .expect("pre-event output sample should be recorded");
    record_sample_if_new(None, &model, &[], &[7.0], &mut times, &mut data, 5.0)
        .expect("near-duplicate event sample should replace the stale output value");

    assert_eq!(times, vec![5.0]);
    assert_eq!(data, vec![vec![7.0]]);
}

#[test]
fn simulate_no_state_solve_ir_stops_for_root_event_updates() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.events.root_conditions = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::LoadTime { dst: 0 },
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
    ]]);
    model.problem.discrete.rhs = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::LoadTime { dst: 0 },
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
    ]]);
    model.parameters = vec![0.0];
    model.visible_names = vec!["m".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("MLS Appendix B root events should settle no-state solve-IR updates");

    assert_eq!(result.times.len(), 3);
    assert_eq!(result.times[0], 0.0);
    assert!((result.times[1] - 0.05).abs() <= 2.0e-6);
    assert_eq!(result.times[2], 0.1);
    assert_eq!(result.data, vec![vec![0.0, 2.0, 2.0]]);
}

#[test]
fn no_state_root_inside_start_tolerance_is_not_retriggered() {
    let model = root_event_update_model(1.0e-11);

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            atol: 1.0e-10,
            ..Default::default()
        },
    )
    .expect("near-start root should be treated as already consumed");

    assert_eq!(
        result.times,
        vec![0.0, 0.1],
        "root inside the start tolerance must not create a duplicate right-limit event"
    );
    assert_eq!(
        result.data,
        vec![vec![0.0, 0.0]],
        "event update should not fire for a relation root consumed by the start tolerance"
    );
}

#[test]
fn no_state_root_at_target_tolerance_applies_at_target() {
    let model = root_event_update_model(0.1 - 5.0e-13);

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            atol: 1.0e-12,
            ..Default::default()
        },
    )
    .expect("root within target tolerance should settle at the output boundary");

    assert_eq!(
        result.times,
        vec![0.0, 0.1],
        "target-tolerance root should replace the target sample, not add a near duplicate"
    );
    assert_eq!(result.data, vec![vec![0.0, 2.0]]);
}

fn root_event_update_model(root_time: f64) -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.parameter_count = 0;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.events.root_conditions = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const {
            dst: 1,
            value: root_time,
        },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]]);
    model.problem.discrete.rhs = solve::ScalarProgramBlock::new(vec![vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const {
            dst: 1,
            value: root_time,
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
    ]]);
    model.parameters = vec![0.0];
    model.visible_names = vec!["m".to_string()];
    model
}

#[test]
fn project_algebraics_recovers_from_misordered_singular_direct_chain_guess() {
    let mut model = solve::SolveModel::default();
    let rhs_rows = vec![
        vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 1.0e-12,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Lt,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 1.0 },
            solve::LinearOp::LoadY { dst: 4, index: 0 },
            solve::LinearOp::Unary {
                dst: 5,
                op: solve::UnaryOp::Sin,
                arg: 4,
            },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Div,
                lhs: 5,
                rhs: 4,
            },
            solve::LinearOp::Select {
                dst: 7,
                cond: 2,
                if_true: 3,
                if_false: 6,
            },
            solve::LinearOp::LoadY { dst: 8, index: 1 },
            solve::LinearOp::Binary {
                dst: 9,
                op: solve::BinaryOp::Sub,
                lhs: 8,
                rhs: 7,
            },
            solve::LinearOp::StoreOutput { src: 9 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadTime { dst: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ];
    let jvp_rows = vec![
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    ];
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::new(rhs_rows.clone()),
    );
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::new(jvp_rows.clone()),
    );
    model.problem.solve_layout.algebraic_scalar_count = 2;
    install_dense_algebraic_projection_plan(&mut model);
    model.initial_y = vec![0.0, 1.0];

    let ode_model = OdeModel::new(&model).expect("ODE model should build from solve-IR rows");
    let mut y = model.initial_y.clone();
    project_algebraics(&ode_model, &mut y, &[], 0.01, 0, 1.0e-12)
        .expect("projection should seed finite direct-chain values before Newton");

    assert!((y[0] - 0.01).abs() <= 1.0e-12);
    assert!((y[1] - 0.01f64.sin() / 0.01).abs() <= 1.0e-12);
}

#[test]
fn project_algebraics_sanitizes_nonfinite_algebraic_guess() {
    let mut model = solve::SolveModel::default();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]]));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]));
    model.problem.solve_layout.algebraic_scalar_count = 1;
    install_dense_algebraic_projection_plan(&mut model);
    model.initial_y = vec![f64::NAN];

    let ode_model = OdeModel::new(&model).expect("ODE model should build from solve-IR rows");
    let mut y = model.initial_y.clone();
    project_algebraics(&ode_model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("projection should sanitize non-finite algebraic guesses before Newton");

    assert!((y[0] - 2.0).abs() <= 1.0e-12);
}

#[test]
fn project_algebraics_uses_solve_ir_row_targets_before_large_pivots() {
    let mut model = solve::SolveModel::default();
    let rhs_rows = vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::Const {
                dst: 1,
                value: 3000.0,
            },
            solve::LinearOp::LoadY { dst: 2, index: 0 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
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
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ];
    let jvp_rows = vec![
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::Const {
                dst: 1,
                value: 3000.0,
            },
            solve::LinearOp::LoadSeed { dst: 2, index: 0 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
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
        ],
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    ];
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::new(rhs_rows.clone()),
    );
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::new(jvp_rows.clone()),
    );
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(1)), Some(solve::scalar_slot_y(0))];
    model.problem.solve_layout.solver_maps.names = vec!["h".to_string(), "f".to_string()];
    model.problem.solve_layout.algebraic_scalar_count = 2;
    install_dense_algebraic_projection_plan(&mut model);
    model.initial_y = vec![1.0, 0.0];

    let ode_model = OdeModel::new(&model).expect("ODE model should build from solve-IR rows");
    let mut y = model.initial_y.clone();
    project_algebraics(&ode_model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("projection should honor row targets before generic AD pivots");

    assert!((y[0] - 2.0).abs() <= 1.0e-12);
    assert!((y[1] - 6000.0).abs() <= 1.0e-8);
}

#[test]
fn project_algebraics_preserves_state_values_for_consistency_residuals() {
    let mut model = solve::SolveModel::default();
    let rhs_rows = vec![
        vec![solve::LinearOp::Const { dst: 0, value: 0.0 }],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ];
    let jvp_rows = vec![
        vec![solve::LinearOp::Const { dst: 0, value: 0.0 }],
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
    ];
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::new(rhs_rows.clone()),
    );
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::new(jvp_rows.clone()),
    );
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(0))];
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "a".to_string()];
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.initial_y = vec![0.0, 0.0];

    let ode_model = OdeModel::new(&model).expect("ODE model should build from solve-IR rows");
    let mut y = model.initial_y.clone();
    project_algebraics(&ode_model, &mut y, &[], 0.0, 1, 1.0e-12)
        .expect("algebraic projection should ignore state-targeted consistency residuals");

    // State initialization belongs to the initialization problem and fixed
    // starts, not to the algebraic projector used at event boundaries.
    assert_eq!(y[0], 0.0);
    assert_eq!(y[1], 0.0);
}

#[test]
fn simulate_seeds_algebraics_from_initial_residual_before_runtime_projection() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "z".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("z".to_string(), 1)]);
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            quadratic_algebraic_row(),
        ]));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            quadratic_algebraic_jvp_row(),
        ]));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    install_dense_algebraic_projection_plan(&mut model);
    model.problem.initialization.residual =
        solve::ScalarProgramBlock::new(vec![zero_row(), z_minus_one()]);
    model.initial_y = vec![0.0, 0.0];
    model.visible_names = vec!["z".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("initial residual should seed the algebraic basin");

    assert!((result.data[0][0] - 1.0).abs() <= 1.0e-10);
}

#[test]
fn initialization_projects_demoted_state_layout_slots() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "d".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("d".to_string(), 1)]);
    model.problem.solve_layout.state_scalar_count = 2;
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            zero_row(),
        ]));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            zero_row(),
        ]));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.problem.initialization.residual = solve::ScalarProgramBlock::new(vec![z_minus_one()]);
    model.problem.initialization.row_targets = vec![Some(solve::scalar_slot_y(1))];
    model.initial_y = vec![0.0, 0.0];
    model.variable_meta = vec![
        solve::SolveVariableMeta {
            name: "x".to_string(),
            role: "state".to_string(),
            is_state: true,
            fixed: Some(true),
            ..Default::default()
        },
        solve::SolveVariableMeta {
            name: "d".to_string(),
            role: "state".to_string(),
            is_state: false,
            fixed: Some(false),
            ..Default::default()
        },
    ];

    let ode_model = OdeModel::new(&model).expect("ODE model should build from solve-IR rows");
    let runtime = SolveRuntime::new(&model);
    let mut y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    initialize_state_runtime_values(
        &model,
        &SimOptions::default(),
        &runtime,
        &ode_model,
        &mut y,
        &mut params,
        0.0,
    )
    .expect("demoted state-layout slots should be projected by initialization");

    assert_eq!(y[0], 0.0, "fixed state starts remain fixed");
    assert!((y[1] - 1.0).abs() <= 1.0e-10);
}

#[test]
fn initial_projection_indices_preserve_true_state_slots() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec![
        "x".to_string(),
        "d".to_string(),
        "z".to_string(),
        "o".to_string(),
    ];
    model.problem.solve_layout.solver_maps.name_to_idx = indexmap::IndexMap::from([
        ("x".to_string(), 0),
        ("d".to_string(), 1),
        ("z".to_string(), 2),
        ("o".to_string(), 3),
    ]);
    model.problem.solve_layout.state_scalar_count = 2;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.output_scalar_count = 1;
    model.variable_meta = vec![
        solve::SolveVariableMeta {
            name: "x".to_string(),
            is_state: true,
            fixed: Some(false),
            ..Default::default()
        },
        solve::SolveVariableMeta {
            name: "d".to_string(),
            is_state: false,
            fixed: Some(false),
            ..Default::default()
        },
        solve::SolveVariableMeta {
            name: "z".to_string(),
            is_state: false,
            fixed: None,
            ..Default::default()
        },
        solve::SolveVariableMeta {
            name: "o".to_string(),
            is_state: false,
            fixed: None,
            ..Default::default()
        },
    ];

    assert_eq!(initial_projection_indices(&model), vec![1, 2]);
}

#[test]
fn simulate_records_algebraically_consistent_initial_sample() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "z".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("z".to_string(), 1)]);
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            z_zero_row(),
        ]));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            algebraic_identity_jvp_row(),
        ]));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    install_dense_algebraic_projection_plan(&mut model);
    model.problem.initialization.residual =
        solve::ScalarProgramBlock::new(vec![zero_row(), z_plus_one()]);
    model.initial_y = vec![0.0, 0.0];
    model.visible_names = vec!["z".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("initial observation should preserve the finalized initialization solution");

    assert!((result.data[0][0]).abs() <= 1.0e-10);
    assert!((result.data[0][1]).abs() <= 1.0e-10);
}

#[test]
fn simulate_runs_solve_ir_initial_updates_after_initial_projection() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "z".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("z".to_string(), 1)]);
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.discrete_valued_scalar_names = vec!["c[1]".to_string()];
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            z_zero_row(),
        ]));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            algebraic_identity_jvp_row(),
        ]));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    install_dense_algebraic_projection_plan(&mut model);
    model.problem.initialization.residual =
        solve::ScalarProgramBlock::new(vec![zero_row(), z_minus_two()]);
    model.problem.initialization.update_rhs =
        solve::ScalarProgramBlock::new(vec![z_greater_one_condition_row()]);
    model.problem.initialization.update_targets = vec![solve::scalar_slot_p(0)];
    model.initial_y = vec![0.0, 0.0];
    model.parameters = vec![0.0];
    model.visible_names = vec!["branch".to_string()];
    model.visible_value_rows = solve::ScalarProgramBlock::new(vec![relation_branch_value_row()]);

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("Solve IR initial updates should follow projected initial algebraics");

    assert!((result.data[0][0] - 2.0).abs() <= 1.0e-10);
    assert!((result.data[0][1] - 2.0).abs() <= 1.0e-10);
}

#[test]
fn simulate_seeds_initial_discrete_conditions_before_initial_residual() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "z".to_string()];
    model.problem.solve_layout.solver_maps.name_to_idx =
        indexmap::IndexMap::from([("x".to_string(), 0), ("z".to_string(), 1)]);
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            z_plus_one(),
        ]));
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::new(vec![
            zero_row(),
            algebraic_identity_jvp_row(),
        ]));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    install_dense_algebraic_projection_plan(&mut model);
    model.problem.discrete.rhs = solve::ScalarProgramBlock::new(vec![const_one_row()]);
    model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
    model.problem.initialization.residual =
        solve::ScalarProgramBlock::new(vec![zero_row(), z_minus_selected_condition()]);
    model.initial_y = vec![0.0, 0.0];
    model.parameters = vec![0.0];
    model.visible_names = vec!["z".to_string()];

    let result = simulate(
        &model,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("initial discrete condition should seed the initialization residual branch");

    assert!((result.data[0][0] + 1.0).abs() <= 1.0e-10);
}

fn zero_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn const_one_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn z_minus_one() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn z_minus_two() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn z_zero_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn z_greater_one_condition_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn relation_branch_value_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Const {
            dst: 2,
            value: -2.0,
        },
        solve::LinearOp::Select {
            dst: 3,
            cond: 0,
            if_true: 1,
            if_false: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn z_minus_selected_condition() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::Const {
            dst: 2,
            value: -1.0,
        },
        solve::LinearOp::Const { dst: 3, value: 0.0 },
        solve::LinearOp::Select {
            dst: 4,
            cond: 1,
            if_true: 2,
            if_false: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 4,
        },
        solve::LinearOp::StoreOutput { src: 5 },
    ]
}

fn z_plus_one() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn algebraic_identity_jvp_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadSeed { dst: 0, index: 1 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn quadratic_algebraic_row() -> Vec<solve::LinearOp> {
    let mut row = z_minus_one();
    row.pop();
    row.extend([
        solve::LinearOp::LoadY { dst: 3, index: 1 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Mul,
            lhs: 3,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]);
    row
}

fn quadratic_algebraic_jvp_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadSeed { dst: 3, index: 1 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Mul,
            lhs: 3,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Add,
            lhs: 4,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

#[test]
fn simulate_rejects_nonempty_solver_layout_without_residual_rows() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string()];
    model.problem.solve_layout.state_scalar_count = 1;
    model.initial_y = vec![0.0];

    let err = simulate(&model, &SimOptions::default()).unwrap_err();
    assert!(matches!(err, SimError::EmptySystem));
}
