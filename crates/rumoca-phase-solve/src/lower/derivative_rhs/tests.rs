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

fn array_var(name: &str, dims: Vec<i64>) -> dae::Variable {
    dae::Variable {
        dims,
        ..scalar_var(name)
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: derivative_rhs_test_span(),
    }
}

fn derivative_rhs_function_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        type_def_id: None,
        name: name.to_string(),
        span: derivative_rhs_test_span(),
        type_name: "Real".to_string(),
        type_class: None,
        dims: Vec::new(),
        shape_expr: Vec::new(),
        default: None,
        min: None,
        max: None,
        description: None,
    }
}

#[test]
fn derivative_linear_parts_projects_scalar_function_call() {
    let span = derivative_rhs_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));

    let mut scale = rumoca_core::Function::new("Test.scale", span);
    scale.inputs.push(derivative_rhs_function_param("u"));
    scale.outputs.push(derivative_rhs_function_param("y"));
    scale.body.push(rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference::from_flat_segments("y", span, None),
        value: rumoca_core::Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(real(2.0)),
            rhs: Box::new(var_ref("u")),
            span,
        },
        span,
    });
    dae_model
        .symbols
        .functions
        .insert(scale.name.clone(), scale);

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Test.scale"),
        args: vec![rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![var_ref("x")],
            span,
        }],
        is_constructor: false,
        span,
    };
    let state_names = HashSet::from(["x".to_string()]);
    let structural_bindings = IndexMap::new();
    let ctx = DerivativeLinearCtx {
        state_names: &state_names,
        dae_model: &dae_model,
        structural_bindings: &structural_bindings,
    };

    let (coefficients, remainder) = derivative_linear_parts(&expr, &ctx, span)
        .expect("function derivative projection should succeed")
        .expect("function call should contain a state derivative");

    assert_eq!(
        coefficients.keys().map(String::as_str).collect::<Vec<_>>(),
        vec!["x"]
    );
    assert!(remainder.is_none());
}

fn structured_indexed_var_ref(base: &str, field: &str, index: i64) -> rumoca_core::Expression {
    let span = derivative_rhs_test_span();
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: base.to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: field.to_string(),
                    span,
                    subs: vec![rumoca_core::Subscript::generated_index(index, span)],
                },
            ],
            def_id: None,
        }),
        subscripts: Vec::new(),
        span,
    }
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
fn expression_result_dims_accepts_scalarized_element_of_aggregate_array() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("vehicle.q"),
        array_var("vehicle.q", vec![4]),
    );
    let span = derivative_rhs_test_span();

    let dims = expression_result_dims(
        &structured_indexed_var_ref("vehicle", "q", 1),
        &dae_model,
        &IndexMap::new(),
        span,
    )
    .expect("scalarized aggregate array element has scalar shape");

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
    let err = binding_keys_for_subscripted_name(
        &rumoca_core::Reference::new("x"),
        &subscripts,
        &dae_model,
        &IndexMap::new(),
        span,
    )
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
    let keys = binding_keys_for_subscripted_name(
        &rumoca_core::Reference::new("x"),
        &subscripts,
        &dae_model,
        &IndexMap::new(),
        span,
    )
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
