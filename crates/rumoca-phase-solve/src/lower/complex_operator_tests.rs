use super::*;

fn unspanned_complex_operator_test_span() -> rumoca_core::Span {
    rumoca_core::Span::DUMMY
}

#[test]
fn complex_binary_parts_rejects_unsupported_op_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_complex_operator_tests_source_7.mo",
        ),
        11,
        19,
    );

    let err = match builder.lower_complex_binary_parts(BinaryOp::Atan2, 0, 1, 2, 3, span) {
        Ok(_) => panic!("unsupported complex binary op should fail"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(matches!(err, LowerError::ContractViolation { .. }));
    assert_eq!(
        err.reason(),
        "invalid IR contract: complex operator call mapped to unsupported binary op Atan2"
    );
}

#[test]
fn builtin_argument_error_uses_stable_builtin_name_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_complex_operator_tests_source_8.mo",
        ),
        3,
        11,
    );

    let err = builder
        .lower_builtin(
            rumoca_core::BuiltinFunction::Smooth,
            &[],
            span,
            &Scope::new(),
            0,
        )
        .expect_err("missing smooth() argument must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(err.reason().contains("builtin smooth"), "{err:?}");
    assert!(!err.reason().contains("Smooth"), "{err:?}");
}

#[test]
fn builtin_argument_error_does_not_fabricate_dummy_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);

    let err = builder
        .lower_div_builtin(
            &[],
            &Scope::new(),
            0,
            unspanned_complex_operator_test_span(),
        )
        .expect_err("div() without arguments must fail");

    assert_eq!(err.source_span(), None);
    assert_eq!(
        err.reason(),
        "invalid IR contract: div() requires exactly 2 arguments, got 0"
    );
}

#[test]
fn unsupported_binary_operator_error_uses_operator_display_with_span() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_complex_operator_tests_source_9.mo",
        ),
        4,
        9,
    );
    let literal = |value| rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    };
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Assign,
        lhs: Box::new(literal(1.0)),
        rhs: Box::new(literal(2.0)),
        span,
    };

    let err = builder
        .lower_expr(&expr, &Scope::new(), 0)
        .expect_err("assignment operator must not lower as a solve expression");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(err.reason(), "binary operator `=` is unsupported");
    assert!(!err.reason().contains("Assign"), "{err:?}");
}

#[test]
fn complex_projection_source_reference_requires_span_metadata() {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("z"),
        subscripts: Vec::new(),
        span: unspanned_complex_operator_test_span(),
    };

    let err = builder
        .requires_complex_projection(&expr, &Scope::new())
        .expect_err("unspanned source reference should fail before projection key lookup");

    assert_eq!(err.source_span(), None);
    assert_eq!(
        err.reason(),
        "invalid IR contract: complex projection for `z` requires source span metadata"
    );
}
