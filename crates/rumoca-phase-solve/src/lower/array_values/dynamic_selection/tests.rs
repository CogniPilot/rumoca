use super::*;

fn output_param_with_dims(
    name: &str,
    dims: Vec<i64>,
    span: rumoca_core::Span,
) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        type_def_id: None,
        name: name.to_string(),
        span,
        type_name: "Real".to_string(),
        type_class: None,
        dims,
        shape_expr: Vec::new(),
        default: None,
        min: None,
        max: None,
        description: None,
    }
}

#[test]
fn projected_record_field_expression_preserves_value_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("project_record_field.mo"),
        6,
        12,
    );
    let value = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("pin"),
        subscripts: Vec::new(),
        span,
    };

    let projected = projected_record_field_expression(&value, "v")
        .expect("spanned field projection should be generated");

    assert_eq!(projected.span(), Some(span));
}

#[test]
fn projected_record_field_expression_rejects_unspanned_value() {
    let value = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("pin"),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    };

    let err = projected_record_field_expression(&value, "v")
        .expect_err("source-derived field projection requires an owner span");

    assert_eq!(err.source_span(), None);
    assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
    assert!(
        err.reason().contains("projected record field expression"),
        "error should name the missing provenance context: {err}"
    );
}

#[test]
fn function_output_dims_invalid_dummy_span_stays_unspanned() {
    let output = output_param_with_dims("y", vec![-1], rumoca_core::Span::DUMMY);
    let err = function_output_dims(&rumoca_core::Reference::new("f"), &output)
        .expect_err("negative output dimension must be rejected");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy output span should not be fabricated into a source span: {err:?}"
    );
    assert!(
        err.to_string().contains("invalid dimension `-1`"),
        "unexpected error: {err}"
    );
}

#[test]
fn function_output_dims_invalid_real_span_is_preserved() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_output_dims.mo"),
        14,
        18,
    );
    let output = output_param_with_dims("y", vec![-1], span);
    let err = function_output_dims(&rumoca_core::Reference::new("f"), &output)
        .expect_err("negative output dimension must be rejected");

    assert!(
        matches!(err, LowerError::ContractViolation { span: err_span, .. } if err_span == span),
        "real output span should be preserved: {err:?}"
    );
}
