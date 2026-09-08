use super::*;

#[test]
fn runtime_register_scratch_allocation_failure_is_error() {
    let plan = RowPlan::Simple(SimpleRowPlan {
        ops: Box::new([]),
        reg_count: usize::MAX,
        output_srcs: Box::new([0]),
        input_requirements: InputRequirements::default(),
    });
    let mut scratch = Vec::new();
    let mut out = [0.0];

    let err = execute_row(
        &plan,
        &mut scratch,
        row_inputs(&[], &[], 0.0, None),
        &mut out,
    )
    .expect_err("oversized register scratch should report an error");

    assert!(
        matches!(&err, CompileError::Backend(message) if message.contains("runtime register scratch allocation overflow")),
        "unexpected error: {err}"
    );
}

#[test]
fn checked_scalar_program_rejects_zero_size_linear_solve_component() {
    let row = vec![
        LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 0,
            rhs_start: 0,
            n: 0,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let Err(err) = plan_fixture_row(&row) else {
        panic!("zero-size linear solve must fail IR construction");
    };

    assert!(
        matches!(&err, CompileError::Input(message) if message.contains("range length 0")),
        "unexpected error: {err}"
    );
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
    let plan = plan_fixture_row(&row).expect("plan row");
    let mut scratch = Vec::new();
    let mut expected = [0.0];
    execute_row(
        &plan,
        &mut scratch,
        row_inputs(&[3.0], &[2.0], 0.5, None),
        &mut expected,
    )
    .expect("interp");
    let compiled = compile_residual_rows(&[row]).expect("compile row");
    let mut out = [0.0];

    compiled
        .call(&[3.0], &[2.0], 0.5, &mut out)
        .expect("jit row eval");

    assert!((out[0] - expected[0]).abs() <= f64::EPSILON * 64.0);
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
fn compiled_tensor_load_preserves_primal_and_seed_lanes() {
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 1,
            count: 2,
            seed_start: Some(0),
            lanes: 2,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
        LinearOp::StoreOutput { src: 2 },
        LinearOp::StoreOutput { src: 3 },
    ];
    let compiled = compile_jacobian_rows(&[row]).expect("compiled tensor load");
    let mut out = [0.0; 4];

    compiled
        .call(&[10.0, 20.0, 30.0], &[], 0.0, &[2.0, 3.0], &mut out)
        .expect("tensor load row eval");

    assert_eq!(out, [20.0, 2.0, 30.0, 3.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn simple_runtime_missing_y_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan_fixture_row(&row).expect("simple plan");
    let mut scratch = Vec::new();

    let err = execute_row(
        &plan,
        &mut scratch,
        row_inputs(&[5.0], &[], 0.0, None),
        &mut [0.0],
    )
    .expect_err("undersized y vector should report an error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing y[1]")));
}

#[test]
fn simple_runtime_missing_p_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan_fixture_row(&row).expect("simple plan");
    let mut scratch = Vec::new();

    let err = execute_row(
        &plan,
        &mut scratch,
        row_inputs(&[], &[], 0.0, None),
        &mut [0.0],
    )
    .expect_err("undersized p vector should report an error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing p[0]")));
}

#[test]
fn general_runtime_missing_seed_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan_fixture_row(&row).expect("general plan");
    let mut scratch = Vec::new();

    let err = execute_row(
        &plan,
        &mut scratch,
        row_inputs(&[], &[], 0.0, None),
        &mut [0.0],
    )
    .expect_err("missing seed vector should report an error");

    assert!(matches!(err, CompileError::Input(message) if message.contains("missing seed[0]")));
}

#[test]
fn checked_program_plan_rejects_input_requirement_overflow() {
    let row = vec![
        LinearOp::LoadY {
            dst: 0,
            index: usize::MAX,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = match plan_fixture_row(&row) {
        Ok(_) => panic!("input requirement overflow must fail planning"),
        Err(err) => err,
    };

    assert!(
        matches!(err, CompileError::Backend(ref message) if message.contains("y input requirement overflow")),
        "unexpected error: {err}"
    );
}

#[test]
fn checked_scalar_program_rejects_random_state_register_range_overflow() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::RandomResult {
            dst: 1,
            generator: rumoca_ir_solve::RandomGenerator::Xorshift64Star,
            state_start: u32::MAX,
            state_len: 2,
        },
        LinearOp::StoreOutput { src: 1 },
    ];

    let err = match plan_fixture_row(&row) {
        Ok(_) => panic!("random state register overflow must fail IR construction"),
        Err(err) => err,
    };

    assert!(
        matches!(err, CompileError::Input(ref message) if message.contains("RandomResult") && message.contains("register range") && message.contains("overflows")),
        "unexpected error: {err}"
    );
}

#[test]
fn checked_scalar_program_rejects_linear_solve_register_range_overflow() {
    let row = vec![
        LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 0,
            rhs_start: 0,
            n: usize::MAX,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = match plan_fixture_row(&row) {
        Ok(_) => panic!("linear solve matrix overflow must fail IR construction"),
        Err(err) => err,
    };

    assert!(
        matches!(err, CompileError::Input(ref message) if message.contains("LinearSolveComponent") && message.contains("register range") && message.contains("overflows")),
        "unexpected error: {err}"
    );
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
fn compiled_residual_program_writes_multiple_outputs() {
    // One self-contained program with two outputs that share register 0
    // (the operand-once shape produced by the scalarizer for matmul/linsolve).
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 }, // shared operand
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        }, // out0 = p0 * 2
        LinearOp::StoreOutput { src: 2 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 2,
        }, // out1 = p0 + out0
        LinearOp::StoreOutput { src: 3 },
    ];
    let plan = plan_fixture_row(&row).expect("plan");
    assert_eq!(plan.output_count(), 2);

    let compiled = compile_residual_rows(&[row]).expect("compiled program");
    assert_eq!(compiled.rows(), 1, "one program, two outputs");
    let mut out = [0.0, 0.0];
    compiled.call(&[], &[5.0], 0.0, &mut out).expect("eval");
    assert_eq!(out[0], 10.0); // 5 * 2
    assert_eq!(out[1], 15.0); // 5 + 10
}

#[test]
fn cranelift_panics_are_typed_backend_errors_at_the_adapter_boundary() {
    let error = catch_cranelift_unwind("oversized relocation", || -> () {
        panic!("relative relocation exceeds i32")
    })
    .expect_err("Cranelift panic must not cross the adapter boundary");
    assert_eq!(
        error.to_string(),
        "cranelift execution error: Cranelift oversized relocation failed: relative relocation exceeds i32"
    );
}
