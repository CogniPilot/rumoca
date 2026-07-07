use super::*;

fn unspanned_statement_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

fn literal_i64(value: i64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span,
    }
}

fn literal_string(value: &str, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(value.to_string()),
        span,
    }
}

fn lower_builder<'a>(
    layout: &'a rumoca_ir_solve::VarLayout,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> LowerBuilder<'a> {
    LowerBuilder::new(layout, functions)
}

#[test]
fn eval_for_index_values_rejects_zero_step_with_step_span() {
    let range_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("for_zero_step.mo"),
        5,
        14,
    );
    let step_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("for_zero_step.mo"),
        9,
        10,
    );
    let range = rumoca_core::Expression::Range {
        start: Box::new(literal_i64(1, range_span)),
        step: Some(Box::new(literal_i64(0, step_span))),
        end: Box::new(literal_i64(3, range_span)),
        span: range_span,
    };
    let layout = rumoca_ir_solve::VarLayout::default();
    let functions = IndexMap::new();
    let builder = lower_builder(&layout, &functions);

    let err = builder
        .eval_for_index_values(&range, &IndexMap::new())
        .expect_err("zero for-loop range step must fail");

    assert_eq!(err.source_span(), Some(step_span));
    assert_eq!(err.reason(), "for range step cannot be zero");
}

#[test]
fn eval_compile_time_expr_rejects_unsupported_form_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("for_array_expr.mo"),
        3,
        8,
    );
    let expr = rumoca_core::Expression::Array {
        elements: Vec::new(),
        is_matrix: false,
        span,
    };
    let layout = rumoca_ir_solve::VarLayout::default();
    let functions = IndexMap::new();
    let builder = lower_builder(&layout, &functions);

    let err = builder
        .eval_compile_time_expr(&expr, &IndexMap::new())
        .expect_err("array expression is not a compile-time scalar range value");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(err.reason(), "unsupported expression in for-loop range");
}

#[test]
fn eval_compile_time_expr_evaluates_modelica_strings_is_equal() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("compile_time_string_equal.mo"),
        3,
        32,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Modelica.Utilities.Strings.isEqual"),
        args: vec![
            rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::Array {
                    elements: vec![literal_string("CO2", span)],
                    is_matrix: false,
                    span,
                }),
                subscripts: vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(literal_i64(1, span)),
                    span,
                }],
                span,
            },
            literal_string("CO2", span),
        ],
        is_constructor: false,
        span,
    };
    let layout = rumoca_ir_solve::VarLayout::default();
    let functions = IndexMap::new();
    let builder = lower_builder(&layout, &functions);

    let value = builder
        .eval_compile_time_expr(&expr, &IndexMap::new())
        .expect("Strings.isEqual over literal strings should fold");

    assert_eq!(value, 1.0);
}

#[test]
fn eval_compile_time_builtin_reports_zero_denominator_span() {
    let call_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("for_mod_zero.mo"),
        3,
        12,
    );
    let denom_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("for_mod_zero.mo"),
        10,
        11,
    );
    let args = [literal_i64(4, call_span), literal_i64(0, denom_span)];
    let layout = rumoca_ir_solve::VarLayout::default();
    let functions = IndexMap::new();
    let builder = lower_builder(&layout, &functions);

    let err = builder
        .eval_compile_time_builtin(
            rumoca_core::BuiltinFunction::Mod,
            &args,
            call_span,
            &IndexMap::new(),
        )
        .expect_err("mod denominator zero must fail");

    assert_eq!(err.source_span(), Some(denom_span));
    assert_eq!(
        err.reason(),
        "mod() denominator cannot be zero in for-loop range expression"
    );
}

#[test]
fn positive_size_dimension_rejects_zero_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_statements_tests_source_31.mo"),
        4,
        9,
    );

    let err = positive_size_dimension(0, span)
        .expect_err("size dimension zero must fail instead of saturating");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(err.reason(), "size dimension must be positive");
}

#[test]
fn positive_size_dimension_rejects_host_overflow_with_span() {
    if usize::BITS >= i64::BITS {
        return;
    }
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_statements_tests_source_32.mo"),
        6,
        15,
    );

    let err =
        positive_size_dimension(i64::MAX, span).expect_err("oversized size dimension must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason().contains("exceeds host index range"),
        "unexpected error: {err}"
    );
}

#[test]
fn missing_guarded_assignment_binding_rejects_dummy_span_without_fabricating_span() {
    let err = missing_guarded_assignment_binding("x[1]", unspanned_statement_test_span());

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason()
            .contains("guarded assignment to `x[1]` requires an existing binding"),
        "unexpected error: {err}"
    );
}

#[test]
fn real_fft_frequency_count_rejects_non_positive_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_statements_tests_source_33.mo"),
        2,
        8,
    );

    let err =
        checked_fft_frequency_count(0.0, span).expect_err("realFFT frequency count zero must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(err.reason(), "realFFT frequency count must be positive");
}

#[test]
fn real_fft_frequency_count_rejects_fractional_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_statements_tests_source_34.mo"),
        11,
        18,
    );

    let err = checked_fft_frequency_count(2.5, span)
        .expect_err("realFFT frequency count must be integral");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "realFFT frequency count must evaluate to an integer"
    );
}

#[test]
fn checked_usize_dims_to_i64_rejects_overflow_with_span() {
    let Some(dim) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_statements_tests_source_46.mo"),
        12,
        20,
    );

    let err = checked_usize_dims_to_i64(&[dim], "record component shape", span)
        .expect_err("record component dimensions must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!("invalid IR contract: record component shape dimension {dim} exceeds i64 range")
    );
}

#[test]
fn checked_usize_dims_to_i64_rejects_overflow_without_fabricating_span() {
    if usize::BITS < i64::BITS {
        return;
    }
    let err = checked_usize_dims_to_i64(
        &[usize::MAX],
        "record component shape",
        unspanned_statement_test_span(),
    )
    .expect_err("oversized dimensions must fail");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason().contains("exceeds i64 range"),
        "unexpected error: {err}"
    );
}

#[test]
fn checked_compile_time_i64_rejects_upper_bound_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_statements_tests_source_47.mo"),
        9,
        18,
    );

    let err = checked_compile_time_i64(i64::MAX as f64, "for index", Some(span))
        .expect_err("rounded f64 upper bound must not saturate to i64::MAX");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(err.reason(), "for index overflows i64");
}
