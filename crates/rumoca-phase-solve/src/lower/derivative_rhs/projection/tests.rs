use super::*;

#[test]
fn checked_projection_offset_rejects_host_index_overflow_with_span() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_21.mo",
        ),
        4,
        12,
    );

    let Err(mul_err) =
        checked_projection_offset(usize::MAX, 2, 0, "derivative projection flat index", span)
    else {
        return Err("overflowing derivative projection multiplication succeeded".to_string());
    };
    assert_eq!(mul_err.source_span(), Some(span));
    assert_eq!(
        mul_err.reason(),
        "invalid IR contract: derivative projection flat index multiplication overflows host index range"
            .to_string()
    );

    let Err(add_err) =
        checked_projection_offset(usize::MAX, 1, 1, "derivative projection flat index", span)
    else {
        return Err("overflowing derivative projection addition succeeded".to_string());
    };
    assert_eq!(add_err.source_span(), Some(span));
    assert_eq!(
        add_err.reason(),
        "invalid IR contract: derivative projection flat index addition overflows host index range"
            .to_string()
    );

    Ok(())
}

#[test]
fn checked_projection_offset_dummy_span_stays_unspanned() {
    let err = checked_projection_offset(
        usize::MAX,
        2,
        0,
        "derivative projection flat index",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("overflowing derivative projection multiplication must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy projection offset span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("multiplication overflows"));
}

#[test]
fn checked_usize_scalar_count_dummy_span_stays_unspanned() {
    let err = checked_usize_scalar_count(
        &[usize::MAX, 2],
        "derivative projection shape",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("overflowing derivative scalar count must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy scalar-count span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("scalar count overflows"));
}

#[test]
fn next_range_value_preserves_range_span_on_overflow() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_22.mo",
        ),
        2,
        18,
    );

    let Err(err) = next_range_value(i64::MAX, 1, span) else {
        return Err("overflowing derivative slice range step succeeded".to_string());
    };
    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: derivative slice range step overflows after index 9223372036854775807 with step 1"
            .to_string()
    );

    Ok(())
}

#[test]
fn derivative_slice_subscript_bounds_error_reports_subscript_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("derivative_slice_bounds.mo"),
        8,
        12,
    );
    let subscript = rumoca_core::Subscript::index(4, span);
    let err = slice_subscript_indices(&subscript, 3, &IndexMap::new(), span)
        .expect_err("out-of-bounds derivative slice index must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "derivative slice index is outside dimension bounds"
    );
}

#[test]
fn scalar_binding_indexed_dimension_error_reports_subscript_span() -> Result<(), String> {
    let subscript_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_23.mo",
        ),
        9,
        12,
    );
    let mut dae_model = dae::Dae::new();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let Err(err) = expression_dims_for_subscripted_binding(
        "x",
        &[rumoca_core::Subscript::index(1, subscript_span)],
        &dae_model,
        &IndexMap::new(),
        subscript_span,
    ) else {
        return Err("indexed scalar binding dimensions succeeded".to_string());
    };

    assert_eq!(err.source_span(), Some(subscript_span));
    assert_eq!(err.reason(), "scalar binding `x` was indexed");
    Ok(())
}

#[test]
fn unsupported_derivative_target_shape_reports_target_span() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_24.mo",
        ),
        4,
        16,
    );
    let target = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(1),
        span,
    };

    let Err(err) = derivative_target_result_dims(&target, &dae::Dae::new(), &IndexMap::new(), span)
    else {
        return Err("unsupported derivative target shape succeeded".to_string());
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "array derivative RHS target shape is unsupported"
    );
    Ok(())
}

#[test]
fn next_range_value_dummy_span_stays_unspanned() {
    let err = next_range_value(i64::MAX, 1, rumoca_core::Span::DUMMY)
        .expect_err("overflowing derivative slice range step must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy range-step span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("range step overflows"));
}

#[test]
fn expression_result_dims_rejects_der_without_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_11.mo",
        ),
        3,
        8,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: Vec::new(),
        span,
    };

    let err = expression_result_dims(&expr, &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("der() without an argument is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
}

#[test]
fn expression_result_dims_rejects_synthetic_der_without_argument_unspanned() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    };

    let err = expression_result_dims(
        &expr,
        &dae::Dae::new(),
        &IndexMap::new(),
        rumoca_core::Span::DUMMY,
    )
    .expect_err("synthetic der() without an argument is malformed IR");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy der() projection span should not be fabricated into a source span: {err:?}"
    );
}

#[test]
fn expression_result_dims_rejects_fill_without_dimension_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_12.mo",
        ),
        5,
        14,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span,
        }],
        span,
    };

    let err = expression_result_dims(&expr, &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("fill() without dimension arguments is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
}

#[test]
fn expression_result_dims_rejects_synthetic_fill_without_dimension_unspanned() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };

    let err = expression_result_dims(
        &expr,
        &dae::Dae::new(),
        &IndexMap::new(),
        rumoca_core::Span::DUMMY,
    )
    .expect_err("synthetic fill() without dimension arguments is malformed IR");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy fill() projection span should not be fabricated into a source span: {err:?}"
    );
}

#[test]
fn project_expression_scalars_rejects_der_without_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_14.mo",
        ),
        3,
        8,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: Vec::new(),
        span,
    };

    let err = project_expression_scalars(&expr, &[2], &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("der() without an argument is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: der() in derivative projection requires exactly one argument, found 0"
    );
}

#[test]
fn project_expression_scalars_passes_through_stream_operator() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_29.mo",
        ),
        3,
        18,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("inStream").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span,
                },
            ],
            is_matrix: false,
            span,
        }],
        is_constructor: false,
        span,
    };

    let scalars = project_expression_scalars(&expr, &[2], &dae::Dae::new(), &IndexMap::new(), span)
        .expect("stream passthrough should project its argument")
        .expect("stream passthrough argument should have projected scalars");
    let values = scalars
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected scalar literal, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(values, vec![1.0, 2.0]);
}

#[test]
fn project_expression_scalars_rejects_fill_without_value_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_15.mo",
        ),
        5,
        14,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: Vec::new(),
        span,
    };

    let err = project_expression_scalars(&expr, &[2], &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("fill() without a value argument is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: fill() in derivative projection requires a value argument"
    );
}

#[test]
fn project_expression_scalars_rejects_fill_without_dimension_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_16.mo",
        ),
        5,
        14,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span,
        }],
        span,
    };

    let err = project_expression_scalars(&expr, &[2], &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("fill() without dimension arguments is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: fill() in derivative projection requires at least one dimension argument"
    );
}

#[test]
fn project_expression_scalars_rejects_zeros_without_dimension_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_17.mo",
        ),
        9,
        16,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Zeros,
        args: Vec::new(),
        span,
    };

    let err = project_expression_scalars(&expr, &[2], &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("zeros() without dimension arguments is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: zeros() in derivative projection requires at least one dimension argument"
    );
}

#[test]
fn project_expression_scalars_rejects_ones_without_dimension_argument() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_18.mo",
        ),
        9,
        16,
    );
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Ones,
        args: Vec::new(),
        span,
    };

    let err = project_expression_scalars(&expr, &[2], &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("ones() without dimension arguments is malformed IR");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: ones() in derivative projection requires at least one dimension argument"
    );
}

#[test]
fn expression_result_dims_accepts_declared_scalar_function_output() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_13.mo",
        ),
        8,
        21,
    );
    let mut dae_model = dae::Dae::new();
    let mut function = rumoca_core::Function::new("Pkg.scalar", span);
    function
        .outputs
        .push(rumoca_core::FunctionParam::new("y", "Real", span));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Pkg.scalar"), function);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.scalar"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };

    let dims = expression_result_dims(&expr, &dae_model, &IndexMap::new(), span)?;

    assert!(dims.is_empty());
    Ok(())
}

#[test]
fn expression_result_dims_accepts_scalar_external_table_intrinsic() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_13.mo",
        ),
        22,
        43,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("getTimeTableValueNoDer"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };

    let dims = expression_result_dims(&expr, &dae::Dae::new(), &IndexMap::new(), span)?;

    assert!(dims.is_empty());
    Ok(())
}

#[test]
fn expression_result_dims_accepts_scalarized_record_root() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_13.mo",
        ),
        44,
        45,
    );
    let mut dae_model = dae::Dae::new();
    for field in ["u.re", "u.im"] {
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new(field),
            dae::Variable {
                name: rumoca_core::VarName::new(field),
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
    }
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("u"),
        subscripts: Vec::new(),
        span,
    };

    let dims = expression_result_dims(&expr, &dae_model, &IndexMap::new(), span)?;

    assert!(dims.is_empty());
    Ok(())
}

#[test]
fn expression_result_dims_accepts_unflagged_constructor_symbol() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_13.mo",
        ),
        46,
        62,
    );
    let mut dae_model = dae::Dae::new();
    let mut constructor = rumoca_core::Function::new("Complex", span);
    constructor.is_constructor = true;
    constructor
        .inputs
        .push(rumoca_core::FunctionParam::new("re", "Real", span));
    constructor
        .inputs
        .push(rumoca_core::FunctionParam::new("im", "Real", span));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Complex"), constructor);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Complex"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };

    let dims = expression_result_dims(&expr, &dae_model, &IndexMap::new(), span)?;

    assert!(dims.is_empty());
    Ok(())
}

#[test]
fn expression_result_dims_accepts_projected_matrix_function_output() -> Result<(), LowerError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_13.mo",
        ),
        8,
        24,
    );
    let mut dae_model = dae::Dae::new();
    let mut function = rumoca_core::Function::new("Pkg.mat9", span);
    function.outputs.push(
        rumoca_core::FunctionParam::new("M", "Real", span)
            .with_dims(vec![9, 9])
            .with_span(span),
    );
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("Pkg.mat9"), function);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.mat9.M[1,1]"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };

    let dims = expression_result_dims(&expr, &dae_model, &IndexMap::new(), span)?;

    assert!(dims.is_empty());
    Ok(())
}

#[test]
fn expression_result_dims_reports_missing_function_output_dims_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_13.mo",
        ),
        30,
        42,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.missing"),
        args: Vec::new(),
        is_constructor: false,
        span,
    };

    let err = expression_result_dims(&expr, &dae::Dae::new(), &IndexMap::new(), span)
        .expect_err("missing function metadata should be a lower error");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(
        err,
        LowerError::Spanned { source, .. }
            if matches!(*source, LowerError::MissingFunction { .. })
    ));
}

#[test]
fn derivative_slice_range_step_detects_final_overshoot() {
    assert!(range_would_overshoot_i64(i64::MAX - 1, i64::MAX, 2));
    assert!(range_would_overshoot_i64(i64::MIN + 1, i64::MIN, -2));
    assert!(!range_would_overshoot_i64(1, 5, 2));
}

#[test]
fn compile_time_positive_range_rejects_large_ranges() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_31.mo",
        ),
        4,
        9,
    );
    let int_expr = |value| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span,
    };
    let err = compile_time_positive_range(
        &int_expr(1),
        Some(&int_expr(1)),
        &int_expr(100_002),
        &IndexMap::new(),
        span,
    )
    .expect_err("oversized derivative slice range should fail");

    assert!(err.reason().contains("derivative slice range is too large"));
}

#[test]
fn compile_time_integer_rejects_i64_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_14.mo",
        ),
        2,
        18,
    );
    let expr = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(i64::MAX as f64),
        span,
    };

    let err = compile_time_integer(&expr, &IndexMap::new(), span)
        .expect_err("oversized derivative range bound must not saturate");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(err.reason().contains("exceeds i64 range"));
}

#[test]
fn scalar_keys_for_dims_rejects_scalar_count_overflow() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_projection_tests_source_25.mo",
        ),
        1,
        4,
    );
    let err = scalar_keys_for_dims("x", &[usize::MAX, 2], span)
        .expect_err("scalarized key enumeration must not overflow");

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert!(
        err.reason().contains("exceeds i64 range")
            || err
                .reason()
                .contains("scalarized binding dimensions scalar count overflows"),
        "got: {}",
        err.reason()
    );
}
