use super::*;

fn unspanned_derivative_rhs_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

#[test]
fn derivative_vec_with_capacity_reports_capacity_overflow() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_94.mo",
        ),
        2,
        8,
    );

    let err = match derivative_vec_with_capacity::<u8>(usize::MAX, "derivative test vector", span) {
        Ok(_) => {
            return Err(LowerError::contract_violation(
                "oversized derivative test vector should fail before allocating",
                span,
            ));
        }
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("derivative test vector capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn derivative_vec_with_capacity_dummy_span_stays_unspanned() {
    let err = derivative_vec_with_capacity::<u8>(
        usize::MAX,
        "derivative test vector",
        unspanned_derivative_rhs_test_span(),
    )
    .expect_err("oversized derivative test vector should fail before allocating");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy derivative vector capacity span should stay unspanned: {err:?}"
    );
    assert!(
        err.reason()
            .contains("derivative test vector capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
}

fn derivative_rhs_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_derivative_rhs_fixture.mo"),
        1,
        2,
    )
}

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span: derivative_rhs_test_span(),
    }
}

fn scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn source_scalar_var(name: &str) -> dae::Variable {
    let span = derivative_rhs_test_span();
    let var_name = rumoca_core::VarName::new(name);
    dae::Variable {
        name: var_name.clone(),
        component_ref: rumoca_core::component_reference_from_flat_name(&var_name, span),
        ..dae::Variable::empty_with_span(span)
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: derivative_rhs_test_span(),
    }
}

fn derivative_test_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: derivative_rhs_test_span(),
    }
}

fn derivative_test_derivative(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: derivative_rhs_test_span(),
    }
}

fn derivative_test_binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: derivative_rhs_test_span(),
    }
}

#[test]
fn noncontiguous_coupled_derivative_states_keep_their_output_ownership() {
    let mut dae_model = dae::Dae::default();
    for name in ["x", "y", "z"] {
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new(name), source_scalar_var(name));
    }
    for name in ["u", "v", "w"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), source_scalar_var(name));
    }
    let sub = |lhs, rhs| derivative_test_binary(rumoca_core::OpBinary::Sub, lhs, rhs);
    let add = |lhs, rhs| derivative_test_binary(rumoca_core::OpBinary::Add, lhs, rhs);
    dae_model.continuous.equations.extend([
        dae::Equation::residual(
            sub(
                add(
                    derivative_test_derivative(derivative_test_var("x")),
                    derivative_test_derivative(derivative_test_var("z")),
                ),
                derivative_test_var("u"),
            ),
            derivative_rhs_test_span(),
            "x_z_coupling",
        ),
        dae::Equation::residual(
            sub(
                derivative_test_derivative(derivative_test_var("z")),
                derivative_test_var("v"),
            ),
            derivative_rhs_test_span(),
            "z_equation",
        ),
        dae::Equation::residual(
            sub(
                derivative_test_derivative(derivative_test_var("y")),
                derivative_test_var("w"),
            ),
            derivative_rhs_test_span(),
            "y_equation",
        ),
    ]);

    let layout = crate::build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("noncontiguous coupled derivatives should lower");
    assert!(matches!(
        block.nodes.first(),
        Some(rumoca_ir_solve::ComputeNode::LinSolve {
            n: 2,
            output_indices,
            ..
        }) if output_indices == &[0, 2]
    ));
    assert_eq!(
        block
            .len()
            .expect("derivative block output count should be valid"),
        3
    );
    let scalar = rumoca_eval_solve::to_scalar_program_block(&block)
        .expect("derivative block should scalarize");

    assert_eq!(scalar.output_indices, vec![0, 2, 1]);
}

#[test]
fn expression_result_dims_rejects_missing_scalar_binding() {
    let dae_model = dae::Dae::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_41.mo",
        ),
        0,
        7,
    );
    let err = expression_result_dims(&var_ref("missing"), &dae_model, &IndexMap::new(), span)
        .expect_err("missing binding must not default to scalar shape");
    assert!(matches!(err, LowerError::MissingBinding { name } if name == "missing"));
}

#[test]
fn expression_result_dims_accepts_existing_scalar_binding() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_41.mo",
        ),
        8,
        9,
    );
    let dims = expression_result_dims(&var_ref("x"), &dae_model, &IndexMap::new(), span)
        .expect("existing scalar binding has scalar shape");
    assert!(dims.is_empty());
}

#[test]
fn binding_keys_reject_missing_scalarized_binding() {
    let dae_model = dae::Dae::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_95.mo",
        ),
        3,
        7,
    );
    let subscripts = vec![rumoca_core::Subscript::generated_index(1, span)];
    let err =
        binding_keys_for_subscripted_name("x", &subscripts, &dae_model, &IndexMap::new(), span)
            .expect_err("missing scalarized binding must not be fabricated");
    assert!(matches!(err, LowerError::MissingBinding { name } if name == "x[1]"));
}

#[test]
fn binding_keys_accept_existing_scalarized_binding() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x[1]"), scalar_var("x[1]"));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_96.mo",
        ),
        4,
        8,
    );
    let subscripts = vec![rumoca_core::Subscript::generated_index(1, span)];
    let keys =
        binding_keys_for_subscripted_name("x", &subscripts, &dae_model, &IndexMap::new(), span)
            .expect("existing scalarized binding should lower");
    assert_eq!(keys, vec!["x[1]"]);
}

#[test]
fn lower_derivative_rhs_reports_missing_component_root_with_state_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_15.mo",
        ),
        5,
        9,
    );
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            source_span: span,
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let layout = crate::build_var_layout(&dae_model).expect("test DAE layout should build");
    let analysis = DerivativeRhsAnalysis {
        states: vec![StateScalar {
            name: "x".to_string(),
            base: "x".to_string(),
            component: 0,
            base_size: 1,
        }],
        equations: Vec::new(),
        direct_equations: IndexMap::new(),
        direct_assignments: Arc::new(IndexMap::new()),
        component_roots: vec![99],
        components: IndexMap::new(),
        structural_bindings: Arc::new(IndexMap::new()),
        equation_flags: Vec::new(),
    };

    let err = lower_derivative_rhs_with_analysis(&dae_model, &layout, &analysis)
        .expect_err("missing derivative RHS component root should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("derivative RHS component map is missing state `x`"),
        "{err:?}"
    );
}

#[test]
fn lower_derivative_rhs_reports_missing_component_root_entry_with_state_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_16.mo",
        ),
        7,
        11,
    );
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            source_span: span,
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let layout = crate::build_var_layout(&dae_model).expect("test DAE layout should build");
    let analysis = DerivativeRhsAnalysis {
        states: vec![StateScalar {
            name: "x".to_string(),
            base: "x".to_string(),
            component: 0,
            base_size: 1,
        }],
        equations: Vec::new(),
        direct_equations: IndexMap::new(),
        direct_assignments: Arc::new(IndexMap::new()),
        component_roots: Vec::new(),
        components: IndexMap::new(),
        structural_bindings: Arc::new(IndexMap::new()),
        equation_flags: Vec::new(),
    };

    let err = lower_derivative_rhs_with_analysis(&dae_model, &layout, &analysis)
        .expect_err("missing derivative RHS component root entry should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("derivative RHS component roots are missing state `x`"),
        "{err:?}"
    );
}

#[test]
fn lower_derivative_rhs_uses_equation_span_when_state_span_is_missing() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_20.mo",
        ),
        13,
        21,
    );
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            source_span: unspanned_derivative_rhs_test_span(),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model
        .continuous
        .equations
        .push(dae::Equation::residual(real(0.0), span, "eq"));
    let layout = crate::build_var_layout(&dae_model).expect("test DAE layout should build");
    let analysis = DerivativeRhsAnalysis {
        states: vec![StateScalar {
            name: "x".to_string(),
            base: "x".to_string(),
            component: 0,
            base_size: 1,
        }],
        equations: Vec::new(),
        direct_equations: IndexMap::new(),
        direct_assignments: Arc::new(IndexMap::new()),
        component_roots: Vec::new(),
        components: IndexMap::new(),
        structural_bindings: Arc::new(IndexMap::new()),
        equation_flags: Vec::new(),
    };

    let err = lower_derivative_rhs_with_analysis(&dae_model, &layout, &analysis)
        .expect_err("missing derivative RHS component root entry should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("derivative RHS component roots are missing state `x`"),
        "{err:?}"
    );
}

#[test]
fn state_output_y_range_rejects_component_underflow_with_state_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_17.mo",
        ),
        2,
        6,
    );
    let dae_model = dae_with_state_span("x", span);
    let state = StateScalar {
        name: "x[2]".to_string(),
        base: "x".to_string(),
        component: 1,
        base_size: 2,
    };

    let err = state_output_y_range(&dae_model, &state, 0)
        .expect_err("component offset greater than output index should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(err.reason().contains("is before component 1"), "{err:?}");
}

#[test]
fn state_output_y_range_rejects_end_overflow_with_state_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_18.mo",
        ),
        3,
        8,
    );
    let dae_model = dae_with_state_span("x", span);
    let state = StateScalar {
        name: "x".to_string(),
        base: "x".to_string(),
        component: 0,
        base_size: 2,
    };

    let err = state_output_y_range(&dae_model, &state, usize::MAX)
        .expect_err("output range end overflow should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason().contains("derivative output range overflows"),
        "{err:?}"
    );
}

fn dae_with_state_span(name: &str, span: rumoca_core::Span) -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new(name),
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            source_span: span,
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model
}

#[test]
fn literal_array_elements_flat_flattens_matrix_rows_once() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_tests_source_19.mo",
        ),
        0,
        8,
    );
    let rows = vec![
        rumoca_core::Expression::Array {
            elements: vec![real(1.0), real(2.0)],
            is_matrix: false,
            span,
        },
        rumoca_core::Expression::Array {
            elements: vec![real(3.0), real(4.0)],
            is_matrix: false,
            span,
        },
    ];

    let values = literal_array_elements_flat(&rows, span)?;
    let literals = values
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected real literal, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(literals, vec![1.0, 2.0, 3.0, 4.0]);
    Ok(())
}
