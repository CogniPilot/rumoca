use super::*;
use rumoca_eval_dae::{VarEnv, eval_expr};
use rumoca_ir_solve::RandomGenerator;

fn lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_lit(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn array(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    }
}

fn function_call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn table_expr() -> rumoca_core::Expression {
    array(
        vec![
            array(vec![lit(0.0), lit(10.0)], false),
            array(vec![lit(2.0), lit(14.0)], false),
        ],
        true,
    )
}

fn columns_expr() -> rumoca_core::Expression {
    array(vec![int_lit(2)], false)
}

fn time_table() -> (f64, Vec<rumoca_core::ExternalTableData>) {
    let env = VarEnv::<f64>::new();
    let table_id = eval_expr::<f64>(
        &function_call(
            "ExternalCombiTimeTable",
            vec![
                lit(0.0),
                lit(0.0),
                table_expr(),
                lit(0.0),
                columns_expr(),
                int_lit(1),
                int_lit(1),
            ],
        ),
        &env,
    )
    .expect("table id should evaluate");
    let tables =
        rumoca_eval_dae::eval::external_table_data_for_parameter_values_in(&env, &[table_id]);
    (table_id, tables)
}

#[test]
fn eval_row_compare_equality_is_exact_not_epsilon_based() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const {
            dst: 1,
            value: f64::MIN_POSITIVE,
        },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Eq,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Compare {
            dst: 3,
            op: CompareOp::Ne,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];

    let output = eval_row(&row, &[], &[], 0.0, None).expect("compare row evaluates");

    assert_eq!(output, 1.0);
}

#[test]
fn eval_row_supports_solve_ir_table_lookup_ops() {
    let (table_id, tables) = time_table();
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: table_id,
        },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let value = eval_row_with_context(
        &row,
        &[],
        &[],
        0.0,
        RowEvalContext {
            external_tables: Some(&tables),
            ..Default::default()
        },
    )
    .expect("table lookup row should evaluate");

    assert!((value - 12.0).abs() <= 1.0e-12);
}

#[test]
fn eval_row_supports_solve_ir_table_bounds_and_next_event_ops() {
    let (table_id, tables) = time_table();
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: table_id,
        },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::TableBounds {
            dst: 2,
            table_id: 0,
            max: true,
        },
        LinearOp::TableNextEvent {
            dst: 3,
            table_id: 0,
            time: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];

    let value = eval_row_with_context(
        &row,
        &[],
        &[],
        0.0,
        RowEvalContext {
            external_tables: Some(&tables),
            ..Default::default()
        },
    )
    .expect("table bounds row should evaluate");

    assert!((value - 4.0).abs() <= 1.0e-12);
}

#[test]
fn eval_row_hydrates_serialized_external_table_data() {
    let table_id = 424_242.0;
    let model = rumoca_ir_solve::SolveModel {
        parameters: vec![table_id],
        external_tables: rumoca_ir_solve::ExternalTables::new(vec![
            rumoca_core::ExternalTableData {
                id: table_id as u64,
                data: vec![vec![1.0, 0.0], vec![3.0, 1.0]],
                columns: vec![2],
                smoothness: 3,
                extrapolation: 1,
            },
        ]),
        ..Default::default()
    };
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::TableNextEvent {
            dst: 2,
            table_id: 0,
            time: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    let value = eval_row_with_context(
        &row,
        &[],
        &model.parameters,
        0.0,
        RowEvalContext {
            external_tables: Some(model.external_tables.as_slice()),
            ..Default::default()
        },
    )
    .expect("serialized table row should evaluate");

    assert!((value - 1.0).abs() <= 1.0e-12);
}

#[test]
fn eval_row_guarded_division_matches_jit_semantics() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    let value = eval_row(&row, &[], &[], 0.0, None).expect("0/0 row should evaluate");

    assert_eq!(value, 0.0);
}

#[test]
fn eval_row_uses_context_external_tables() {
    let table_id = 515_151.0;
    let local_tables = vec![rumoca_core::ExternalTableData {
        id: table_id as u64,
        data: vec![vec![1.0, 10.0], vec![3.0, 30.0]],
        columns: vec![2],
        smoothness: 3,
        extrapolation: 1,
    }];

    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: table_id,
        },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let value = eval_row_with_context(
        &row,
        &[],
        &[],
        0.0,
        RowEvalContext {
            external_tables: Some(&local_tables),
            ..Default::default()
        },
    )
    .expect("context table row should evaluate");

    assert!(
        (value - 10.0).abs() <= 1.0e-12,
        "expected local table value 10.0, got {value}"
    );
}

#[test]
fn eval_row_table_lookup_failure_is_error_not_silent_zero() {
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 42.0,
        },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let err = eval_row_with_context(
        &row,
        &[],
        &[],
        0.0,
        RowEvalContext {
            external_tables: Some(&[]),
            ..Default::default()
        },
    )
    .expect_err("missing table should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::ExternalTable {
            operation: "lookup",
            table_id: 42.0,
            column: Some(1.0)
        }
    );
}

#[test]
fn eval_row_table_lookup_invalid_column_is_error_not_clamped() {
    let (table_id, tables) = time_table();
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: table_id,
        },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::TableLookup {
            dst: 3,
            table_id: 0,
            column: 1,
            input: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let err = eval_row_with_context(
        &row,
        &[],
        &[],
        0.0,
        RowEvalContext {
            external_tables: Some(&tables),
            ..Default::default()
        },
    )
    .expect_err("invalid table column should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::ExternalTable {
            operation: "lookup",
            table_id,
            column: Some(2.0)
        }
    );
}

#[test]
fn eval_row_missing_y_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[5.0], &[], 0.0, None)
        .expect_err("undersized y vector should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "y",
            index: 1,
            len: 1,
        }
    );
}

#[test]
fn eval_row_missing_p_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("undersized p vector should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "p",
            index: 0,
            len: 0,
        }
    );
}

#[test]
fn eval_row_missing_seed_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("missing seed vector should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "seed",
            index: 0,
            len: 0,
        }
    );
}

#[test]
fn eval_row_missing_source_register_is_error_not_panic_or_zero() {
    let row = vec![
        LinearOp::Move { dst: 0, src: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("missing source register should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: 1,
            len: 1,
        }
    );
}

#[test]
fn eval_row_uninitialized_source_register_is_error_not_zero() {
    let row = vec![
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("uninitialized register read should report an evaluation error");

    assert_eq!(err, EvalSolveError::UninitializedRegister { register: 1 });
}

#[test]
fn eval_row_linsolve_missing_matrix_register_is_error_not_panic_or_zero() {
    let row = vec![
        LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 1,
            rhs_start: 5,
            n: 2,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("malformed linear solve row should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: 1,
            len: 1,
        }
    );
}

#[test]
fn eval_scalar_program_block_short_output_is_error_not_truncation() {
    let block = ScalarProgramBlock::new(vec![
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    ]);
    let mut out = [0.0];

    let err = eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut out)
        .expect_err("short output buffer should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::OutputTooSmall {
            required: 2,
            len: 1,
        }
    );
    assert_eq!(out, [0.0]);
}

#[test]
fn eval_scalar_program_block_prevalidates_inputs_before_mutating_output() {
    let block = ScalarProgramBlock::new(vec![
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ],
    ]);
    let mut out = [9.0, 9.0];

    let err = eval_scalar_program_block(&block, &[5.0], &[], 0.0, None, &mut out)
        .expect_err("block should validate all input lengths before execution");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "y",
            index: 1,
            len: 1,
        }
    );
    assert_eq!(out, [9.0, 9.0]);
}

#[test]
fn row_input_requirements_report_required_vector_lengths() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::LoadP { dst: 1, index: 3 },
        LinearOp::LoadSeed { dst: 2, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];

    assert_eq!(
        row_input_requirements(&row),
        RowInputRequirements {
            y_len: 3,
            p_len: 4,
            seed_len: 2,
        }
    );
}

#[test]
fn scalar_program_block_input_requirements_merge_all_rows() {
    let block = ScalarProgramBlock::new(vec![
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::LoadP { dst: 0, index: 2 },
            LinearOp::LoadSeed { dst: 1, index: 4 },
            LinearOp::StoreOutput { src: 1 },
        ],
    ]);

    assert_eq!(
        scalar_program_block_input_requirements(&block),
        RowInputRequirements {
            y_len: 1,
            p_len: 3,
            seed_len: 5,
        }
    );
}

#[test]
fn eval_row_supports_solve_ir_random_ops() {
    let initial_state = vec![
        LinearOp::Const {
            dst: 0,
            value: 11.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 17.0,
        },
        LinearOp::RandomInitialState {
            dst: 2,
            generator: RandomGenerator::Xorshift64Star,
            local_seed: 0,
            global_seed: 1,
            state_len: 2,
            state_index: 0,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let seed0 = eval_row(&initial_state, &[], &[], 0.0, None).expect("initial random state row");
    assert!(seed0.is_finite() && seed0 >= 1.0);

    let random_result = vec![
        LinearOp::Const {
            dst: 0,
            value: seed0,
        },
        LinearOp::Const {
            dst: 1,
            value: 23.0,
        },
        LinearOp::RandomResult {
            dst: 2,
            generator: RandomGenerator::Xorshift64Star,
            state_start: 0,
            state_len: 2,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let result = eval_row(&random_result, &[], &[], 0.0, None).expect("random result row");
    assert!(result.is_finite() && result > 0.0 && result <= 1.0);

    let random_state = vec![
        LinearOp::Const {
            dst: 0,
            value: seed0,
        },
        LinearOp::Const {
            dst: 1,
            value: 23.0,
        },
        LinearOp::RandomState {
            dst: 2,
            generator: RandomGenerator::Xorshift64Star,
            state_start: 0,
            state_len: 2,
            state_index: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let state1 = eval_row(&random_state, &[], &[], 0.0, None).expect("random state row");
    assert!(state1.is_finite() && state1 >= 1.0);
}

#[test]
fn eval_row_supports_event_stable_impure_random_ops() {
    clear_runtime_state();
    let init = vec![
        LinearOp::Const {
            dst: 0,
            value: 67867967.0,
        },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let runtime_state = SimulationRuntimeState::new();
    let id = eval_row(&init, &[], &[], 0.0, None).expect("impure random init row");
    assert!(id.is_finite() && id >= 1.0);

    let draw = vec![
        LinearOp::Const { dst: 0, value: id },
        LinearOp::ImpureRandom {
            dst: 1,
            id: 0,
            call_site: 42,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let draw_with_state = |t| {
        eval_row_with_context(
            &draw,
            &[],
            &[],
            t,
            RowEvalContext {
                runtime_state: Some(&runtime_state),
                ..Default::default()
            },
        )
        .expect("impure random draw row")
    };
    let first = draw_with_state(1.0);
    let repeated_same_event = draw_with_state(1.0);
    let next_event = draw_with_state(2.0);

    assert!(first > 0.0 && first <= 1.0);
    assert_eq!(first, repeated_same_event);
    assert_ne!(first, next_event);
}

#[test]
fn simulation_runtime_state_clear_resets_impure_random_streams() {
    let init = vec![
        LinearOp::Const {
            dst: 0,
            value: 67867967.0,
        },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let draw = |runtime_state: &SimulationRuntimeState, id| {
        eval_row_with_context(
            &[
                LinearOp::Const { dst: 0, value: id },
                LinearOp::ImpureRandom {
                    dst: 1,
                    id: 0,
                    call_site: 42,
                },
                LinearOp::StoreOutput { src: 1 },
            ],
            &[],
            &[],
            1.0,
            RowEvalContext {
                runtime_state: Some(runtime_state),
                ..Default::default()
            },
        )
        .expect("impure random draw row")
    };

    let runtime_state = SimulationRuntimeState::new();
    let first_id = eval_row(&init, &[], &[], 0.0, None).expect("first init row");
    let first_draw = draw(&runtime_state, first_id);
    let _advanced_draw = draw(&runtime_state, first_id);

    runtime_state.clear();
    let second_id = eval_row(&init, &[], &[], 0.0, None).expect("second init row");
    let second_draw = draw(&runtime_state, second_id);

    assert_eq!(first_id, second_id);
    assert_eq!(first_draw, second_draw);
}

#[test]
fn row_eval_context_keeps_impure_random_state_model_local() {
    let init = vec![
        LinearOp::Const {
            dst: 0,
            value: 67867967.0,
        },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let draw_row = |id| {
        vec![
            LinearOp::Const { dst: 0, value: id },
            LinearOp::ImpureRandom {
                dst: 1,
                id: 0,
                call_site: 42,
            },
            LinearOp::StoreOutput { src: 1 },
        ]
    };
    let draw_at = |runtime_state, id, t| {
        eval_row_with_context(
            &draw_row(id),
            &[],
            &[],
            t,
            RowEvalContext {
                runtime_state: Some(runtime_state),
                ..Default::default()
            },
        )
        .expect("impure random draw row")
    };

    let state_a = SimulationRuntimeState::new();
    let state_b = SimulationRuntimeState::new();
    let id = eval_row(&init, &[], &[], 0.0, None).expect("impure random init row");

    let a1 = draw_at(&state_a, id, 1.0);
    clear_runtime_state();
    let a2 = draw_at(&state_a, id, 2.0);

    let b1 = draw_at(&state_b, id, 1.0);
    let b2 = draw_at(&state_b, id, 2.0);

    assert_eq!(a1, b1);
    assert_eq!(a2, b2);
}
