// SPEC_0021 file-size exception: solve-model regression tests cover model
// inventory, start values, runtime parameters, and random initialization in one
// file. split plan: move runtime-parameter and initialization fixtures into
// focused test modules.
use super::*;

mod external_table_and_random_tests;

fn solve_model_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("solve_model_fixture.mo"),
        0,
        1,
    )
}

fn unspanned_solve_model_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        component_ref: Some(source_component_ref_from_name(name)),
        source_span: solve_model_test_span(),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: Vec::new(),
        span: solve_model_test_span(),
    }
}

fn plain_var(name: &str, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
    source_component_ref_from_name(name)
}

fn source_component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::from_flat_segments(
        name,
        solve_model_test_span(),
        Some(source_fixture_def_id(name)),
    )
}

fn source_fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

fn indexed_var(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(
            Box::new(int_expr(index)),
            solve_model_test_span(),
        )],
        span: solve_model_test_span(),
    }
}

fn field_access_expr(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span: solve_model_test_span(),
    }
}

fn slice_expr(base: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: vec![rumoca_core::Subscript::generated_colon(
            solve_model_test_span(),
        )],
        span: solve_model_test_span(),
    }
}

fn int_expr(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: solve_model_test_span(),
    }
}

fn string_expr(value: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(value.to_string()),
        span: solve_model_test_span(),
    }
}

fn array_expr(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: solve_model_test_span(),
    }
}

fn real_expr(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: solve_model_test_span(),
    }
}

fn call_expr(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span: solve_model_test_span(),
    }
}

fn constructor_call_expr(
    name: &str,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: true,
        span: solve_model_test_span(),
    }
}

fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: solve_model_test_span(),
    }
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: solve_model_test_span(),
    }
}

#[test]
fn lower_expands_visible_observation_for_scalarized_record_field_slice() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    for index in 1..=3 {
        let name = format!("pin[{index}].v");
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new(name.as_str()),
            scalar_var(name.as_str()),
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            sub(var(name.as_str()), real_expr(index as f64)),
            solve_model_test_span(),
            format!("{name} row"),
        ));
    }
    dae_model.continuous.equations.insert(
        0,
        dae::Equation::residual(
            sub(der(var("x")), real_expr(0.0)),
            solve_model_test_span(),
            "x row",
        ),
    );
    let visible = vec![VisibleExpression {
        name: "pin[:].v".to_string(),
        expr: field_access_expr(slice_expr(var("pin")), "v"),
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("scalarized record field slice observation should lower");

    assert_eq!(
        prepared.visible_names,
        ["pin[:].v[1]", "pin[:].v[2]", "pin[:].v[3]"]
    );
    assert_eq!(prepared.visible_value_rows.programs.len(), 3);
}

#[test]
fn value_only_lowering_skips_eager_sensitivity_artifacts() {
    let dae_model = single_state_zero_derivative_model();
    let value_only =
        lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata(
            dae_model.clone(),
            Vec::new(),
            &dae_model,
        )
        .expect("value-only lowering should produce a simulation problem");

    assert!(
        !value_only
            .problem
            .continuous
            .derivative_rhs
            .nodes
            .is_empty()
    );
    assert!(
        value_only
            .artifacts
            .continuous
            .implicit_jacobian_v
            .nodes
            .is_empty()
    );
    assert!(
        value_only
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .programs
            .is_empty()
    );
    assert!(
        value_only
            .artifacts
            .continuous
            .full_jacobian_v
            .programs
            .is_empty()
    );
    assert!(value_only.artifacts.continuous.mass_matrix.is_empty());

    let full = lower_dae_to_solve_model(&dae_model)
        .expect("full runtime lowering should still produce sensitivity artifacts");
    assert_eq!(full.artifacts.continuous.mass_matrix, vec![vec![1.0]]);
    assert!(
        !full
            .artifacts
            .continuous
            .full_jacobian_v
            .programs
            .is_empty()
    );
}

fn single_state_zero_derivative_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        solve_model_test_span(),
        "der(x) = 0",
    ));
    dae_model
}

#[test]
fn seed_var_values_rejects_empty_scalar_seed() {
    let var = scalar_var("p");
    let mut env = rumoca_eval_dae::VarEnv::<f64>::new();

    let err = seed_var_values(&mut env, "p", &var, &[])
        .expect_err("empty scalar seed must fail instead of defaulting to zero");

    let message = err.to_string();
    assert!(message.contains("start value for `p`"));
    assert!(message.contains("empty scalar start value"));
}

#[test]
fn lower_dae_to_solve_model_reports_invalid_variable_shape_span() {
    let mut dae_model = dae::Dae::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_43.mo"),
        11,
        29,
    );
    let mut bad = scalar_var("bad");
    bad.dims = vec![2, -1];
    bad.source_span = span;
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("bad"), bad);

    let err = lower_dae_to_solve_model(&dae_model)
        .expect_err("invalid DAE variable shape should bubble through solve-model lowering");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("DAE variable `bad` has negative dimension -1"),
        "unexpected error: {err}"
    );
}

#[test]
fn scalar_names_reports_invalid_variable_shape_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_44.mo"),
        3,
        17,
    );
    let mut bad = scalar_var("bad");
    bad.dims = vec![2, -1];
    bad.source_span = span;

    let err = scalar_names("bad", &bad)
        .expect_err("invalid DAE variable shape should bubble through scalar-name expansion");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("DAE variable `bad` has negative dimension -1"),
        "unexpected error: {err}"
    );
}

#[test]
fn expand_values_to_size_reports_capacity_overflow_with_source_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_45.mo"),
        7,
        15,
    );
    let err = expand_values_to_size(vec![1.0], usize::MAX, "huge_start", span)
        .expect_err("oversized start expansion should fail before allocating");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("solve-model start value capacity for `huge_start` overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn expand_values_to_size_reports_capacity_overflow_without_dummy_span() {
    let err = expand_values_to_size(
        vec![1.0],
        usize::MAX,
        "huge_start",
        unspanned_solve_model_test_span(),
    )
    .expect_err("oversized unspanned start expansion should fail before allocating");

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        SolveModelLowerError::Lower(LowerError::UnspannedContractViolation { .. })
    ));
    assert!(
        err.diagnostic_reason()
            .contains("solve-model start value capacity for `huge_start` overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn initial_solver_values_report_capacity_overflow_before_runtime_eval()
-> Result<(), SolveModelLowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_75.mo"),
        4,
        12,
    );
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            source_span: span,
            ..scalar_var("x")
        },
    );

    let err = match initial_solver_values(
        &dae_model,
        None,
        &[],
        usize::MAX,
        Arc::new(EvalRuntimeState::default()),
    ) {
        Ok(_) => {
            return Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
                reason: "oversized initial solver values should fail before allocating".to_string(),
                span,
            }));
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("initial solver values capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn expand_values_to_size_preserves_broadcast_and_padding_behavior() {
    assert_eq!(
        expand_values_to_size(vec![2.5], 3, "x", solve_model_test_span())
            .expect("scalar start should broadcast"),
        vec![2.5, 2.5, 2.5]
    );
    assert_eq!(
        expand_values_to_size(vec![1.0, 2.0], 4, "x", solve_model_test_span())
            .expect("short explicit start should pad with the last value"),
        vec![1.0, 2.0, 2.0, 2.0]
    );
}

#[test]
fn default_start_values_for_size_reports_capacity_overflow_with_source_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_46.mo"),
        2,
        8,
    );
    let mut var = scalar_var("huge_default");
    var.source_span = span;

    let err = default_start_values_for_size(&var, 0.0, usize::MAX)
        .expect_err("oversized default start expansion should fail before allocating");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("solve-model start value capacity for `huge_default` overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn default_start_values_for_size_preserves_scalar_default_start() {
    let var = scalar_var("x");

    assert_eq!(
        default_start_values_for_size(&var, 4.0, 0).expect("scalar default should be present"),
        vec![4.0]
    );
    assert_eq!(
        default_start_values_for_size(&var, 4.0, 3).expect("array default should expand"),
        vec![4.0, 4.0, 4.0]
    );
}

#[test]
fn identity_mass_matrix_reports_capacity_overflow() -> Result<(), SolveModelLowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_76.mo"),
        1,
        9,
    );

    let err = match state_identity_mass_matrix(usize::MAX, span) {
        Ok(_) => {
            return Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
                reason: "oversized identity mass matrix should fail before allocating".to_string(),
                span,
            }));
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("identity mass matrix rows capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn visible_subscripts_report_integer_range_overflow() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_77.mo"),
        3,
        11,
    );

    let err = match visible_subscripts_from_usize(vec![usize::MAX], span) {
        Ok(_) => {
            return Err(LowerError::ContractViolation {
                reason: "oversized visible subscript should fail before lowering".to_string(),
                span,
            });
        }
        Err(err) => err,
    };

    assert!(matches!(
        err,
        LowerError::ContractViolation { reason, span: actual }
            if actual == span
                && reason.contains("visible variable subscript exceeds Modelica integer range")
    ));
    Ok(())
}

#[test]
fn visible_subscripts_report_integer_range_overflow_without_dummy_span() {
    let err = visible_subscripts_from_usize(vec![usize::MAX], unspanned_solve_model_test_span())
        .expect_err("oversized unspanned visible subscript should fail before lowering");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("visible variable subscript exceeds Modelica integer range"),
        "unexpected error: {err}"
    );
}

#[test]
fn state_derivative_order_flags_report_capacity_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_47.mo"),
        9,
        17,
    );
    let err = reserve_state_derivative_order_flags(usize::MAX, span)
        .expect_err("oversized state-row ordering flags should fail before allocating");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("state derivative row ordering capacity overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn state_derivative_order_flags_report_capacity_overflow_without_dummy_span() {
    let err = reserve_state_derivative_order_flags(usize::MAX, unspanned_solve_model_test_span())
        .expect_err("oversized unspanned state-row flags should fail before allocating");

    assert_eq!(err.source_span(), None);
    assert!(matches!(
        err,
        SolveModelLowerError::Lower(LowerError::UnspannedContractViolation { .. })
    ));
    assert!(
        err.diagnostic_reason()
            .contains("state derivative row ordering capacity overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn state_derivative_order_capacity_reports_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_48.mo"),
        1,
        6,
    );
    let mut values = Vec::<dae::Equation>::new();
    let err = reserve_state_derivative_order_capacity(&mut values, usize::MAX, span)
        .expect_err("oversized state-row ordering buffer should fail before allocating");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.diagnostic_reason()
            .contains("state derivative row ordering capacity overflows"),
        "unexpected error: {err}"
    );
}

#[test]
fn initial_solver_values_preserve_vector_start_reference() {
    let mut dae_model = dae::Dae::default();
    let mut shape_type = scalar_var("body1.shapeType");
    shape_type.start = Some(string_expr("cylinder"));
    dae_model
        .variables
        .parameters
        .insert(shape_type.name.clone(), shape_type);

    let mut q_start = scalar_var("body1.Q_start");
    q_start.dims = vec![4];
    q_start.start = Some(array_expr(
        vec![
            real_expr(0.0),
            real_expr(0.0),
            real_expr(0.0),
            real_expr(1.0),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(q_start.name.clone(), q_start);

    let mut q = scalar_var("body1.Q");
    q.dims = vec![4];
    q.start = Some(var("body1.Q_start"));
    dae_model.variables.states.insert(q.name.clone(), q);

    let params = default_parameter_values(
        &dae_model,
        None,
        Arc::new(EvalRuntimeState::default()),
        &std::collections::HashMap::new(),
    )
    .expect("parameter vector should lower");
    assert_eq!(params, vec![0.0, 0.0, 0.0, 0.0, 1.0]);

    let initial_y = initial_solver_values(
        &dae_model,
        None,
        &params,
        4,
        Arc::new(EvalRuntimeState::default()),
    )
    .expect("state starts should lower");

    assert_eq!(initial_y, vec![0.0, 0.0, 0.0, 1.0]);
}

#[test]
fn lower_uses_default_numeric_slot_for_string_parameter_start() {
    let mut dae_model = dae::Dae::default();
    let mut method = scalar_var("periodicClock.solverMethod");
    method.start = Some(rumoca_core::Expression::Literal {
        value: Literal::String("ExplicitEuler".to_string()),
        span: solve_model_test_span(),
    });
    dae_model
        .variables
        .parameters
        .insert(method.name.clone(), method);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("string-valued parameter starts should not enter numeric evaluation");
    let Some(solve::ScalarSlot::P { index, .. }) = prepared
        .problem
        .layout
        .binding("periodicClock.solverMethod")
    else {
        panic!("expected string-valued parameter to keep a numeric P slot");
    };

    assert_eq!(prepared.parameters[index], 0.0);
}

#[test]
fn lower_uses_default_numeric_slot_for_nested_string_parameter_start() {
    let mut dae_model = dae::Dae::default();
    let mut terminal = scalar_var("settings.terminalConnection");
    terminal.start = Some(rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Literal {
                value: Literal::Boolean(true),
                span: solve_model_test_span(),
            },
            string_expr("TerminalBox"),
        )],
        else_branch: Box::new(string_expr("NoTerminalBox")),
        span: solve_model_test_span(),
    });
    dae_model
        .variables
        .parameters
        .insert(terminal.name.clone(), terminal);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("nested string-valued parameter starts should use numeric defaults");
    let Some(solve::ScalarSlot::P { index, .. }) = prepared
        .problem
        .layout
        .binding("settings.terminalConnection")
    else {
        panic!("expected string-valued parameter to keep a numeric P slot");
    };

    assert_eq!(prepared.parameters[index], 0.0);
}

#[test]
fn lower_sizes_initial_vector_from_final_solve_layout() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), scalar_var("v"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("v"), real_expr(1.0)),
        solve_model_test_span(),
        "visible algebraic residual",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        real_expr(0.0),
        solve_model_test_span(),
        "extra template residual",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("lowering should tolerate extra non-state residual rows");

    assert_eq!(prepared.problem.solve_layout.solver_scalar_count(), 1);
    assert_eq!(prepared.initial_y.len(), 1);
}

#[test]
fn lower_keeps_algebraic_binding_needed_by_derivative_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert("x".into(), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), var("a")),
        solve_model_test_span(),
        // MLS Appendix B B.1a: derivative rows may read algebraics from
        // the same continuous implicit system even when residual rows are
        // reduced before solve-IR lowering.
        "der(x) = a",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("algebraic dependency in derivative RHS should remain bound");

    assert_eq!(prepared.problem.solve_layout.solver_scalar_count(), 2);
    assert_eq!(
        prepared.problem.layout.binding("a"),
        Some(solve::scalar_slot_y(1))
    );
    assert_eq!(prepared.initial_y.len(), 2);
}

#[test]
fn lower_replaces_nonfinite_start_guess_with_type_default() {
    let mut dae_model = dae::Dae::default();
    let mut v = scalar_var("v");
    v.start = Some(binary_expr(div_op(), real_expr(0.0), real_expr(0.0)));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), v);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("v"), real_expr(1.0)),
        solve_model_test_span(),
        // MLS §4.4/§8.6: start is only an initialization guess; a
        // non-finite guess must not prevent equations from solving `v`.
        "non-finite start guess for algebraic",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("non-finite start guesses should be sanitized during Solve IR lowering");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_seeds_solver_visible_start_dependencies_from_default_guesses() {
    let mut dae_model = dae::Dae::default();
    let mut source = scalar_var("source");
    source.start = Some(real_expr(2.0));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("source"), source);

    let mut dependent = scalar_var("dependent");
    dependent.start = Some(binary_expr(OpBinary::Add, var("source"), real_expr(1.0)));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("dependent"), dependent);

    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("source"), real_expr(0.0)),
        solve_model_test_span(),
        "source algebraic residual",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("dependent"), real_expr(0.0)),
        solve_model_test_span(),
        "dependent algebraic residual",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("solver-visible start dependencies should be seeded");

    assert_eq!(prepared.initial_y, vec![2.0, 3.0]);
}

#[test]
fn lower_seeds_start_dependencies_removed_by_structural_metadata() {
    let mut dae_model = dae::Dae::default();
    let mut dependent = scalar_var("dependent");
    dependent.start = Some(var("alias.source"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("dependent"), dependent);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("dependent"), real_expr(0.0)),
        solve_model_test_span(),
        "dependent algebraic residual",
    ));

    let mut metadata_dae_model = dae_model.clone();
    metadata_dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("alias.source"),
        scalar_var("alias.source"),
    );

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata(
        dae_model,
        Vec::new(),
        &metadata_dae_model,
    )
    .expect("metadata defaults should seed structurally removed start aliases");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_rejects_missing_binding_in_explicit_start_guess() {
    let start_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_7.mo"),
        11,
        19,
    );
    let mut dae_model = dae::Dae::default();
    let mut v = scalar_var("v");
    v.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("missing").into(),
        subscripts: Vec::new(),
        span: start_span,
    });
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), v);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("v"), real_expr(1.0)),
        solve_model_test_span(),
        "explicit start references a missing binding",
    ));

    let err = lower_dae_to_solve_model(&dae_model)
        .expect_err("malformed explicit start expression should fail lowering");

    let SolveModelLowerError::Evaluation { source, .. } = &err else {
        panic!("explicit start should report an evaluation error: {err:?}");
    };
    assert_eq!(source.missing_binding_name(), Some("missing"));
    assert_eq!(err.source_span(), Some(start_span));
}

#[test]
fn lower_evaluates_size_start_from_dae_dims_without_array_binding() {
    let mut dae_model = dae::Dae::default();
    let mut lines = scalar_var("world.x_label.lines");
    lines.dims = vec![2, 2, 2];
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("world.x_label.lines"), lines);

    let mut n = scalar_var("world.x_label.n");
    n.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("world.x_label.lines"), int_expr(1)],
        span: solve_model_test_span(),
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("world.x_label.n"), n);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("size start should use DAE dimensions for known array variables");

    let Some(solve::ScalarSlot::P { index, .. }) =
        prepared.problem.layout.binding("world.x_label.n")
    else {
        panic!("world.x_label.n should lower to a parameter slot");
    };
    assert_eq!(prepared.parameters[index], 2.0);
}

#[test]
fn lower_propagates_fixed_start_across_alias_to_state_guess() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let mut y = scalar_var("y");
    y.start = Some(real_expr(0.1));
    y.fixed = Some(true);
    dae_model.variables.outputs.insert("y".into(), y);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        solve_model_test_span(),
        "derivative row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("y"), var("x")),
        solve_model_test_span(),
        "output y aliases state x",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("lowering");

    assert_eq!(prepared.initial_y, vec![0.1, 0.1]);
}

#[test]
fn lower_propagates_fixed_array_start_across_alias_to_solver_guess() {
    let mut dae_model = dae::Dae::default();
    let mut source = scalar_var("source");
    source.dims = vec![2];
    source.start = Some(array_expr(vec![real_expr(0.25), real_expr(0.75)], false));
    source.fixed = Some(true);
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("source"), source);

    let mut target = scalar_var("target");
    target.dims = vec![2];
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("target"), target);

    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("target"), var("source")),
        solve_model_test_span(),
        "target aliases source",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("array alias starts should seed scalar solver guesses");

    assert_eq!(prepared.initial_y, vec![0.25, 0.75, 0.25, 0.75]);
}

#[test]
fn lower_seeds_evaluable_continuous_assignment_to_initial_guess() {
    let mut dae_model = dae::Dae::default();
    let mut parameter = scalar_var("parameter");
    parameter.start = Some(real_expr(2.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("parameter"), parameter);

    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("computed"),
        scalar_var("computed"),
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("alias"), scalar_var("alias"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("computed"),
            binary_expr(OpBinary::Add, var("parameter"), real_expr(1.0)),
        ),
        solve_model_test_span(),
        "evaluable continuous assignment",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(var("alias"), var("computed")),
        solve_model_test_span(),
        "alias of computed continuous assignment",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("continuous assignment should seed");

    assert_eq!(prepared.initial_y, vec![3.0, 3.0]);
}

#[test]
fn lower_does_not_overwrite_fixed_start_with_continuous_assignment_seed() {
    let mut dae_model = dae::Dae::default();
    let mut parameter = scalar_var("parameter");
    parameter.start = Some(real_expr(2.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("parameter"), parameter);

    let mut computed = scalar_var("computed");
    computed.start = Some(real_expr(5.0));
    computed.fixed = Some(true);
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("computed"), computed);
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("computed"),
            binary_expr(OpBinary::Add, var("parameter"), real_expr(1.0)),
        ),
        solve_model_test_span(),
        "continuous assignment must not replace fixed start seed",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("fixed start should remain seed");

    assert_eq!(prepared.initial_y, vec![5.0]);
}

#[test]
fn lower_skips_nonfinite_continuous_assignment_seed() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("computed"),
        scalar_var("computed"),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(
            var("computed"),
            binary_expr(OpBinary::Div, real_expr(0.0), real_expr(0.0)),
        ),
        solve_model_test_span(),
        "non-finite continuous assignment seed",
    ));

    let prepared = lower_dae_to_solve_model(&dae_model).expect("non-finite seed should be skipped");

    assert_eq!(prepared.initial_y, vec![0.0]);
}

#[test]
fn lower_refines_parameter_start_from_array_max_dependency() {
    let mut dae_model = dae::Dae::default();
    let mut a = scalar_var("a");
    a.start = Some(real_expr(0.1));
    let mut b = scalar_var("b");
    b.start = Some(real_expr(10.0));
    let mut c = scalar_var("c");
    c.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Max,
        args: vec![array_expr(
            vec![
                binary_expr(div_op(), var("a"), var("b")),
                real_expr(1.0e-12),
            ],
            true,
        )],
        span: solve_model_test_span(),
    });
    dae_model.variables.parameters.insert("a".into(), a);
    dae_model.variables.parameters.insert("b".into(), b);
    dae_model.variables.parameters.insert("c".into(), c);

    let prepared =
        lower_dae_to_solve_model(&dae_model).expect("parameter start dependencies should lower");
    let Some(rumoca_ir_solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("c")
    else {
        panic!("expected c parameter binding");
    };

    assert!((prepared.parameters[index] - 0.01).abs() <= 1.0e-12);
}

#[test]
fn lower_keeps_runtime_tail_names_visible_for_traces() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("m"), scalar_var("m"));

    let prepared =
        lower_dae_to_solve_model(&dae_model).expect("no-state runtime-tail model should lower");

    assert_eq!(prepared.initial_y.len(), 0);
    assert_eq!(prepared.visible_names, ["u", "z", "m"]);
    assert_eq!(
        prepared
            .variable_meta
            .iter()
            .map(|meta| meta.role.as_str())
            .collect::<Vec<_>>(),
        ["input", "discrete-real", "discrete-valued"]
    );
}

#[test]
fn lower_skips_visible_observation_with_dynamic_subscript() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_78.mo"),
        5,
        23,
    );
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("a"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("a")
        },
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("k"), scalar_var("k"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        solve_model_test_span(),
        "derivative row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(indexed_var("a", 1).with_span(span), real_expr(1.0)).with_span(span),
        span,
        "a[1] row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(indexed_var("a", 2).with_span(span), real_expr(2.0)).with_span(span),
        span,
        "a[2] row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(plain_var("k", span), real_expr(1.0)).with_span(span),
        span,
        "dynamic index row",
    ));
    let visible = vec![VisibleExpression {
        name: "fn_dynamic".to_string(),
        expr: rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("pick").into(),
                args: Vec::new(),
                is_constructor: false,
                span,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_expr(
                Box::new(plain_var("k", span)),
                span,
            )],
            span,
        },
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("dynamic observation should not block solve lowering");

    assert!(!prepared.visible_names.contains(&"fn_dynamic".to_string()));
}

#[test]
fn variable_meta_marks_relation_driven_real_output_event_discontinuous() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("y")),
            rhs: Box::new(rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Lt,
                        lhs: Box::new(var("time")),
                        rhs: Box::new(real_expr(0.5)),
                        span: solve_model_test_span(),
                    },
                    real_expr(1.0),
                )],
                else_branch: Box::new(real_expr(0.0)),
                span: solve_model_test_span(),
            }),
            span: solve_model_test_span(),
        },
        span: solve_model_test_span(),
        origin: "relation driven output".to_string(),
        scalar_count: 1,
    });

    let meta =
        build_variable_meta(&dae_model, &["y".to_string()]).expect("valid meta should build");

    assert_eq!(meta.len(), 1);
    assert_eq!(meta[0].variability.as_deref(), Some("continuous"));
    assert_eq!(meta[0].time_domain.as_deref(), Some("event-discontinuous"));
}

#[test]
fn variable_meta_marks_guarded_residual_output_event_discontinuous() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable {
            name: rumoca_core::VarName::new("c"),
            dims: vec![5],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::If {
            branches: vec![(indexed_var("c", 5), sub(var("y"), real_expr(1.0)))],
            else_branch: Box::new(sub(var("y"), real_expr(0.0))),
            span: solve_model_test_span(),
        },
        span: solve_model_test_span(),
        origin: "event guarded residual".to_string(),
        scalar_count: 1,
    });

    let meta =
        build_variable_meta(&dae_model, &["y".to_string()]).expect("valid meta should build");

    assert_eq!(meta.len(), 1);
    assert_eq!(meta[0].variability.as_deref(), Some("continuous"));
    assert_eq!(meta[0].time_domain.as_deref(), Some("event-discontinuous"));
}

#[test]
fn variable_meta_propagates_event_discontinuity_through_algebraic_dependency() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::Reference::from_component_reference(
            source_component_ref_from_name("a"),
        )),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Ge,
                    lhs: Box::new(var("time")),
                    rhs: Box::new(real_expr(0.0)),
                    span: solve_model_test_span(),
                },
                real_expr(1.0),
            )],
            else_branch: Box::new(real_expr(0.0)),
            span: solve_model_test_span(),
        },
        span: solve_model_test_span(),
        origin: "relation driven algebraic".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::Reference::from_component_reference(
            source_component_ref_from_name("y"),
        )),
        rhs: var("a"),
        span: solve_model_test_span(),
        origin: "dependent output".to_string(),
        scalar_count: 1,
    });

    let meta =
        build_variable_meta(&dae_model, &["y".to_string()]).expect("valid meta should build");

    assert_eq!(meta.len(), 1);
    assert_eq!(meta[0].time_domain.as_deref(), Some("event-discontinuous"));
}

#[test]
fn lower_supports_visible_observation_with_array_literal_size_range() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.badSizeRange"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.badSizeRange"),
            def_id: None,
            instance_id: None,
            inputs: vec![],
            outputs: vec![rumoca_core::FunctionParam::new(
                "out",
                "Real",
                solve_model_test_span(),
            )],
            locals: vec![],
            body: vec![
                rumoca_core::Statement::Assignment {
                    comp: component_ref("out"),
                    value: real_expr(0.0),
                    span: solve_model_test_span(),
                },
                rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: rumoca_core::Expression::Range {
                            start: Box::new(int_expr(1)),
                            step: None,
                            end: Box::new(rumoca_core::Expression::BuiltinCall {
                                function: rumoca_core::BuiltinFunction::Size,
                                args: vec![array_expr(vec![real_expr(1.0), real_expr(2.0)], false)],
                                span: solve_model_test_span(),
                            }),
                            span: solve_model_test_span(),
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component_ref("out"),
                        value: rumoca_core::Expression::Binary {
                            op: OpBinary::Add,
                            lhs: Box::new(var("out")),
                            rhs: Box::new(var("x")),
                            span: solve_model_test_span(),
                        },
                        span: solve_model_test_span(),
                    }],
                    span: solve_model_test_span(),
                },
            ],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: solve_model_test_span(),
        },
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        solve_model_test_span(),
        "derivative row",
    ));
    let visible = vec![VisibleExpression {
        name: "bad_size_observation".to_string(),
        expr: call_expr("My.badSizeRange", vec![]),
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("array-literal size range should lower");

    assert!(
        prepared
            .visible_names
            .contains(&"bad_size_observation".to_string())
    );
}

#[test]
fn lower_skips_visible_observation_with_unbound_function_input() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.needsInput"),
        rumoca_core::Function {
            name: rumoca_core::VarName::new("My.needsInput"),
            def_id: None,
            instance_id: None,
            inputs: vec![rumoca_core::FunctionParam::new(
                "u",
                "Real",
                solve_model_test_span(),
            )],
            outputs: vec![rumoca_core::FunctionParam::new(
                "out",
                "Real",
                solve_model_test_span(),
            )],
            locals: vec![],
            body: vec![rumoca_core::Statement::Assignment {
                comp: component_ref("out"),
                value: var("u"),
                span: solve_model_test_span(),
            }],
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: vec![],
            span: solve_model_test_span(),
        },
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        sub(der(var("x")), real_expr(0.0)),
        solve_model_test_span(),
        "derivative row",
    ));
    let visible = vec![VisibleExpression {
        name: "missing_function_arg".to_string(),
        expr: call_expr("My.needsInput", vec![]),
    }];

    let prepared = lower_dae_to_solve_model_owned_with_visible_expressions(dae_model, visible)
        .expect("unsupported visible function observation should not block solve lowering");

    assert!(
        !prepared
            .visible_names
            .contains(&"missing_function_arg".to_string())
    );
}

#[test]
fn lower_propagates_initial_equation_through_runtime_alias() {
    let mut dae_model = dae::Dae::default();
    let mut active = scalar_var("active");
    active.start = Some(real_expr(0.0));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), active);
    let mut local_active = scalar_var("localActive");
    local_active.start = Some(real_expr(0.0));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("localActive"), local_active);
    let mut new_active = scalar_var("newActive");
    new_active.start = Some(real_expr(0.0));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("newActive"), new_active);
    let mut pre_new_active = scalar_var("__pre__.newActive");
    pre_new_active.start = Some(real_expr(0.0));
    pre_new_active.fixed = Some(true);
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.newActive"),
        pre_new_active,
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("active"), real_expr(1.0)),
            solve_model_test_span(),
            "initial equation active = true",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("active"),
            var("localActive"),
            solve_model_test_span(),
            "runtime alias active = localActive",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("localActive"),
            var("__pre__.newActive"),
            solve_model_test_span(),
            "runtime alias localActive = pre(newActive)",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("initial runtime alias propagation should lower");

    let Some(solve::ScalarSlot::P {
        index: active_index,
        ..
    }) = prepared.problem.layout.binding("active")
    else {
        panic!("active should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: local_index, ..
    }) = prepared.problem.layout.binding("localActive")
    else {
        panic!("localActive should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: new_index, ..
    }) = prepared.problem.layout.binding("newActive")
    else {
        panic!("newActive should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: pre_new_index,
        ..
    }) = prepared.problem.layout.binding("__pre__.newActive")
    else {
        panic!("__pre__.newActive should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[active_index], 1.0);
    assert_eq!(prepared.parameters[local_index], 1.0);
    assert_eq!(prepared.parameters[new_index], 1.0);
    assert_eq!(prepared.parameters[pre_new_index], 1.0);
}

#[test]
fn lower_seeds_current_value_from_lowered_pre_initial_equation() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.active"),
        scalar_var("__pre__.active"),
    );
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), scalar_var("active"));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("__pre__.active"), real_expr(1.0)),
            solve_model_test_span(),
            "lowered initial equation pre(active) = true",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("lowered pre initial value should seed current value");

    let Some(solve::ScalarSlot::P {
        index: pre_index, ..
    }) = prepared.problem.layout.binding("__pre__.active")
    else {
        panic!("__pre__.active should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: active_index,
        ..
    }) = prepared.problem.layout.binding("active")
    else {
        panic!("active should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[pre_index], 1.0);
    assert_eq!(prepared.parameters[active_index], 1.0);
}

#[test]
fn lower_applies_subscripted_discrete_initial_equations() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_solve_model_tests_source_79.mo"),
        7,
        25,
    );
    let mut dae_model = dae::Dae::default();
    dae_model
        .symbols
        .enum_literal_ordinals
        .insert("Logic.X".to_string(), 2);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("state"),
        dae::Variable {
            dims: vec![2],
            start: Some(int_expr(0)),
            ..scalar_var("state")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.state"),
        dae::Variable {
            dims: vec![2],
            fixed: Some(true),
            ..scalar_var("__pre__.state")
        },
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(indexed_var("state", 2).with_span(span), var("Logic.X")).with_span(span),
            span,
            "state[2] = Logic.X",
        ));

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("subscripted discrete initial equation should seed solve parameters");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("state[2]")
    else {
        panic!("state[2] should be a runtime parameter");
    };
    let Some(solve::ScalarSlot::P {
        index: pre_index, ..
    }) = prepared.problem.layout.binding("__pre__.state[2]")
    else {
        panic!("__pre__.state[2] should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[index], 2.0);
    assert_eq!(prepared.parameters[pre_index], 2.0);
}

#[test]
fn lower_evaluates_enum_literal_runtime_starts() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );
    let mut active = scalar_var("active");
    active.start = Some(var("Modelica.Electrical.Digital.Interfaces.Logic.'U'"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), active);

    let prepared = lower_dae_to_solve_model(&dae_model).expect("enum literal start should lower");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("active") else {
        panic!("active should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[index], 1.0);
}

#[test]
fn lower_broadcasts_enum_literal_start_to_array_runtime_starts() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );
    let mut active = scalar_var("active");
    active.dims = vec![2];
    active.start = Some(var("Modelica.Electrical.Digital.Interfaces.Logic.'U'"));
    let scalar_names = scalar_names("active", &active).expect("valid scalar names should build");
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("active"), active);

    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("enum literal scalar start should apply to every array element");

    for name in scalar_names {
        let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding(&name)
        else {
            panic!("{name} should be a runtime parameter");
        };
        assert_eq!(prepared.parameters[index], 1.0);
    }
}

#[test]
fn lower_propagates_enum_parameter_binding_chain_to_runtime_start() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
    ]);
    let mut root = scalar_var("Counter.q0");
    root.start = Some(var("Modelica.Electrical.Digital.Interfaces.Logic.'0'"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("Counter.q0"), root);

    let mut child = scalar_var("Counter.FF[1].q0");
    child.start = Some(var("Counter.q0"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("Counter.FF[1].q0"), child);

    let mut q = scalar_var("Counter.q[1]");
    q.start = Some(var("Counter.FF[1].q0"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("Counter.q[1]"), q);

    let prepared =
        lower_dae_to_solve_model(&dae_model).expect("enum parameter binding chain should lower");

    let Some(solve::ScalarSlot::P { index, .. }) = prepared.problem.layout.binding("Counter.q[1]")
    else {
        panic!("Counter.q[1] should be a runtime parameter");
    };
    assert_eq!(prepared.parameters[index], 3.0);
}
