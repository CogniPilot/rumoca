use super::*;

#[test]
fn reserve_array_capacity_dummy_span_stays_unspanned() {
    let mut values = Vec::<u8>::new();
    let err = reserve_array_capacity(
        &mut values,
        usize::MAX,
        "array helper",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("impossible capacity must be rejected");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy capacity span should not be fabricated into a source span: {err:?}"
    );
    assert!(
        err.to_string()
            .contains("array helper capacity exceeds host memory limits"),
        "unexpected error: {err}"
    );
}

#[test]
fn reserve_array_capacity_real_span_is_preserved() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("array_values_capacity.mo"),
        22,
        30,
    );
    let mut values = Vec::<u8>::new();
    let err = reserve_array_capacity(&mut values, usize::MAX, "array helper", span)
        .expect_err("impossible capacity must be rejected");

    assert!(
        matches!(err, LowerError::ContractViolation { span: err_span, .. } if err_span == span),
        "real capacity span should be preserved: {err:?}"
    );
}

#[test]
fn lower_array_operand_rejects_unspanned_shaped_operand() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let element_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("array_operand_elements.mo"),
        0,
        1,
    );
    let expr = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: element_span,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: element_span,
            },
        ],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };

    let Err(err) = builder.lower_array_operand(&expr, &Scope::new(), 0) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "unspanned shaped array operand must not use a dummy shape span".to_string(),
        });
    };

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "unspanned shaped operand should fail without dummy provenance: {err:?}"
    );
    assert!(
        err.reason()
            .contains("array operand shape lowering requires a source span"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn lower_array_operand_preserves_scalar_owner_span() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("scalar_operand_owner.mo"),
        1,
        5,
    );
    let expr = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(1.0),
        span,
    };

    let operand = builder.lower_array_operand(&expr, &Scope::new(), 0)?;

    assert_eq!(operand.dims, Vec::<usize>::new());
    assert_eq!(operand.values.len(), 1);
    assert_eq!(operand.shape_span, span);
    Ok(())
}

#[test]
fn lower_array_like_values_rejects_unspanned_scalar_value() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(1.0),
        span: rumoca_core::Span::DUMMY,
    };

    let Err(err) = builder.lower_array_like_values(&expr, &Scope::new(), 0) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "unspanned scalar array-like value must not use a dummy span".to_string(),
        });
    };

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "unspanned scalar array-like value should fail without dummy provenance: {err:?}"
    );
    assert!(
        err.reason()
            .contains("scalar array-like value lowering requires a source span"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn lower_array_like_values_rejects_unspanned_tuple() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Tuple {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };

    let Err(err) = builder.lower_array_like_values(&expr, &Scope::new(), 0) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "unspanned tuple array-like value must not use a dummy span".to_string(),
        });
    };

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "unspanned tuple should fail without dummy provenance: {err:?}"
    );
    assert!(
        err.reason()
            .contains("tuple array-like value lowering requires a source span"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn lower_array_like_values_uses_tuple_owner_for_unspanned_elements() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("tuple_owner.mo"),
        4,
        9,
    );
    let expr = rumoca_core::Expression::Tuple {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        span,
    };

    let values = builder.lower_array_like_values(&expr, &Scope::new(), 0)?;

    assert_eq!(values.len(), 1);
    Ok(())
}

#[test]
fn lower_min_max_builtin_rejects_empty_args_without_dummy_span() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);

    let Err(err) = builder.lower_min_max_builtin(
        &[],
        &Scope::new(),
        0,
        rumoca_core::Span::DUMMY,
        BinaryOp::Min,
        f64::INFINITY,
    ) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "empty min/max args must not emit a dummy-spanned identity".to_string(),
        });
    };

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "empty min/max args should fail without dummy provenance: {err:?}"
    );
    assert!(
        err.reason().contains("array min/max reduction"),
        "unexpected error: {err}"
    );
    Ok(())
}

#[test]
fn lower_min_max_builtin_rejects_unspanned_reduction_arg() -> Result<(), LowerError> {
    let layout = VarLayout::default();
    let functions = IndexMap::new();
    let mut builder = LowerBuilder::new(&layout, &functions);
    let expr = rumoca_core::Expression::Array {
        elements: Vec::new(),
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };

    let Err(err) = builder.lower_min_max_builtin(
        &[expr],
        &Scope::new(),
        0,
        rumoca_core::Span::DUMMY,
        BinaryOp::Min,
        f64::INFINITY,
    ) else {
        return Err(LowerError::UnspannedContractViolation {
            reason: "unspanned min/max arg must not emit a dummy-spanned identity".to_string(),
        });
    };

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "unspanned min/max arg should fail without dummy provenance: {err:?}"
    );
    assert!(
        err.reason()
            .contains("missing source provenance for array min/max reduction"),
        "unexpected error: {err}"
    );
    Ok(())
}
