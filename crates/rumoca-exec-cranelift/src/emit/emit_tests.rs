use super::host_runtime::{
    rumoca_host_table_bounds_min, rumoca_host_table_lookup, rumoca_host_table_lookup_slope,
    rumoca_host_table_next_event,
};
use super::*;

#[test]
fn plan_row_uses_simple_runtime_plan_for_plain_residual_rows() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    let plan = plan_row(&row).expect("simple plan");
    assert!(matches!(plan, RowPlan::Simple(_)));
}

#[test]
fn plan_row_keeps_seed_rows_on_general_runtime_plan() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let plan = plan_row(&row).expect("general plan");
    assert!(matches!(plan, RowPlan::General(_)));
}

#[test]
fn compile_residual_rows_accepts_linear_solve_component() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Const { dst: 4, value: 4.0 },
        LinearOp::Const { dst: 5, value: 6.0 },
        LinearOp::LinearSolveComponent {
            dst: 6,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            component: 0,
        },
        LinearOp::StoreOutput { src: 6 },
    ];

    let compiled = compile_residual_rows(&[row]).expect("compiled row");
    let mut out = [0.0];
    compiled.call(&[], &[], 0.0, &mut out).expect("row eval");
    assert!((out[0] - 3.0).abs() <= f64::EPSILON);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_residual_invokes_jit_and_matches_interpreter_for_representative_row() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::LoadTime { dst: 2 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Unary {
            dst: 4,
            op: UnaryOp::Sin,
            arg: 2,
        },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Add,
            lhs: 3,
            rhs: 4,
        },
        LinearOp::Compare {
            dst: 6,
            op: CompareOp::Gt,
            lhs: 5,
            rhs: 1,
        },
        LinearOp::Select {
            dst: 7,
            cond: 6,
            if_true: 5,
            if_false: 1,
        },
        LinearOp::StoreOutput { src: 7 },
    ];
    let plan = plan_row(&row).expect("plan row");
    let mut scratch = Vec::new();
    let expected =
        execute_row(&plan, &mut scratch, &[3.0], &[2.0], 0.5, None, &[]).expect("interp");
    let compiled = compile_residual_rows(&[row]).expect("compile row");
    let mut out = [0.0];

    compiled
        .call(&[3.0], &[2.0], 0.5, &mut out)
        .expect("jit row eval");

    assert!((out[0] - expected).abs() <= f64::EPSILON * 64.0);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_compare_equality_is_exact_not_epsilon_based() {
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
    let compiled = compile_residual_rows(&[row]).expect("compiled row");
    let mut out = [0.0];

    compiled.call(&[], &[], 0.0, &mut out).expect("row eval");

    assert_eq!(out[0], 1.0);
}

#[test]
fn compiled_logical_not_inverts_boolean_value() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Not,
            arg: 0,
        },
        LinearOp::Const { dst: 2, value: 0.0 },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Not,
            arg: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compiled row");
    let mut out = [0.0];

    compiled.call(&[], &[], 0.0, &mut out).expect("row eval");

    assert_eq!(out[0], 1.0);
}

#[test]
fn compiled_jacobian_invokes_jit_with_seed_pointer() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_jacobian_rows(&[row]).expect("compiled row");
    let mut out = [0.0];

    compiled
        .call(&[4.0], &[], 0.0, &[3.0], &mut out)
        .expect("jacobian row eval");

    assert_eq!(out[0], 7.0);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_table_row_invokes_jit_with_active_external_tables() {
    let table_id = 7.0;
    let tables = [ExternalTableData {
        id: table_id as u64,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
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
    let compiled = compile_residual_rows(&[row]).expect("compiled row");
    let mut out = [0.0];

    compiled
        .call_with_external_tables(&[], &[], 0.0, &tables, &mut out)
        .expect("table row eval");

    assert!((out[0] - 12.0).abs() <= f64::EPSILON);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn general_runtime_table_lookup_failure_is_error_not_silent_zero() {
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
    let plan = plan_row(&row).expect("general plan");
    let mut scratch = Vec::new();

    let err = execute_row(&plan, &mut scratch, &[], &[], 0.0, None, &[])
        .expect_err("missing table should report an evaluation error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("table id 42")));
}

#[test]
fn general_runtime_table_lookup_invalid_column_is_error_not_clamped() {
    let table_id = 7.0;
    let tables = [ExternalTableData {
        id: table_id as u64,
        data: vec![vec![0.0, 10.0], vec![2.0, 14.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }];
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
    let plan = plan_row(&row).expect("general plan");
    let mut scratch = Vec::new();

    let err = execute_row(&plan, &mut scratch, &[], &[], 0.0, None, &tables)
        .expect_err("invalid table column should report an evaluation error");

    assert!(
        matches!(err, CompileError::Input(message) if message.contains("column 2")),
        "invalid column error should mention the requested column"
    );
}

#[test]
fn simple_runtime_missing_y_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan_row(&row).expect("simple plan");
    let mut scratch = Vec::new();

    let err = execute_row(&plan, &mut scratch, &[5.0], &[], 0.0, None, &[])
        .expect_err("undersized y vector should report an error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing y[1]")));
}

#[test]
fn simple_runtime_missing_p_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan_row(&row).expect("simple plan");
    let mut scratch = Vec::new();

    let err = execute_row(&plan, &mut scratch, &[], &[], 0.0, None, &[])
        .expect_err("undersized p vector should report an error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing p[0]")));
}

#[test]
fn general_runtime_missing_seed_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan_row(&row).expect("general plan");
    let mut scratch = Vec::new();

    let err = execute_row(&plan, &mut scratch, &[], &[], 0.0, None, &[])
        .expect_err("missing seed vector should report an error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing seed[0]")));
}

#[test]
fn compiled_residual_prevalidates_inputs_before_mutating_output() {
    let rows = vec![
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ],
    ];
    let compiled = compile_residual_rows(&rows).expect("compiled rows");
    let mut out = [9.0, 9.0];

    let err = compiled
        .call(&[5.0], &[], 0.0, &mut out)
        .expect_err("compiled call should validate inputs before row execution");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing y[1]")));
    assert_eq!(out, [9.0, 9.0]);
}

#[test]
fn compiled_jacobian_prevalidates_seed_before_mutating_output() {
    let rows = vec![vec![
        LinearOp::LoadSeed { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ]];
    let compiled = compile_jacobian_rows(&rows).expect("compiled rows");
    let mut out = [9.0];

    let err = compiled
        .call(&[], &[], 0.0, &[1.0], &mut out)
        .expect_err("compiled Jacobian call should validate seed length before execution");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing seed[1]")));
    assert_eq!(out, [9.0]);
}

#[test]
fn table_host_trampolines_keep_abi_boundary_nan_sentinel() {
    assert!(rumoca_host_table_bounds_min(42.0).is_nan());
    assert!(rumoca_host_table_lookup(42.0, 1.0, 1.0).is_nan());
    assert!(rumoca_host_table_lookup_slope(42.0, 1.0, 1.0).is_nan());
    assert!(rumoca_host_table_next_event(42.0, 0.0).is_nan());
}
