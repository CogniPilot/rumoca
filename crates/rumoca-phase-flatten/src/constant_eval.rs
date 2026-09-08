//! Fail-closed adapter for opportunistic constant evaluation.

use rumoca_eval_flat::constant::{EvalContext, Value, eval_optional};

use crate::FlattenError;

pub(crate) fn evaluate_optional(
    expression: &rumoca_core::Expression,
    context: &EvalContext,
    operation: &'static str,
    owner_span: rumoca_core::Span,
) -> Result<Option<Value>, FlattenError> {
    match eval_optional(expression, context) {
        Ok(value) => Ok(value),
        Err(error) => Err(map_evaluation_error(error, operation, Some(owner_span))?),
    }
}

pub(crate) fn map_evaluation_error(
    error: rumoca_eval_flat::constant::EvalError,
    operation: &'static str,
    owner_span: Option<rumoca_core::Span>,
) -> Result<FlattenError, FlattenError> {
    let span = required_evaluation_span(error.span(), owner_span, operation)?;
    Ok(FlattenError::constant_evaluation_failed(
        operation,
        error.to_string(),
        span,
    ))
}

pub(crate) fn map_optional_evaluation<T>(
    result: Result<Option<T>, rumoca_eval_flat::constant::EvalError>,
    operation: &'static str,
    owner_span: Option<rumoca_core::Span>,
) -> Result<Option<T>, FlattenError> {
    match result {
        Ok(value) => Ok(value),
        Err(error) => Err(map_evaluation_error(error, operation, owner_span)?),
    }
}

fn required_evaluation_span(
    preferred: Option<rumoca_core::Span>,
    owner_span: Option<rumoca_core::Span>,
    operation: &'static str,
) -> Result<rumoca_core::Span, FlattenError> {
    if let Some(preferred) = preferred
        && let Ok(provenance) = preferred.require_provenance(operation)
    {
        return Ok(provenance.span());
    }
    let owner_span = owner_span.ok_or_else(|| {
        FlattenError::missing_source_context(format!(
            "{operation} has neither expression nor owner provenance"
        ))
    })?;
    crate::source_spans::required_span(owner_span, operation)
}

pub(crate) fn evaluate_optional_boolean(
    expression: &rumoca_core::Expression,
    context: &EvalContext,
    operation: &'static str,
    owner_span: rumoca_core::Span,
) -> Result<Option<bool>, FlattenError> {
    match evaluate_optional(expression, context, operation, owner_span)? {
        Some(Value::Bool(value)) => Ok(Some(value)),
        Some(value) => {
            let span = required_evaluation_span(expression.span(), Some(owner_span), operation)?;
            Err(FlattenError::constant_evaluation_failed(
                operation,
                format!("type mismatch: expected Boolean, got {}", value.type_name()),
                span,
            ))
        }
        None => Ok(None),
    }
}

pub(crate) fn evaluate_optional_string(
    expression: &rumoca_core::Expression,
    context: &EvalContext,
    operation: &'static str,
    owner_span: rumoca_core::Span,
) -> Result<Option<String>, FlattenError> {
    match evaluate_optional(expression, context, operation, owner_span)? {
        Some(Value::String(value)) => Ok(Some(value)),
        Some(value) => {
            let span = required_evaluation_span(expression.span(), Some(owner_span), operation)?;
            Err(FlattenError::constant_evaluation_failed(
                operation,
                format!("type mismatch: expected String, got {}", value.type_name()),
                span,
            ))
        }
        None => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("constant_eval_contract.mo"),
            7,
            11,
        )
    }

    #[test]
    fn runtime_dependent_value_is_the_only_accepted_absence() {
        let expression = rumoca_core::Expression::VarRef {
            name: "runtime_value".into(),
            subscripts: vec![],
            span: span(),
        };
        assert_eq!(
            evaluate_optional(
                &expression,
                &EvalContext::structural_preidentity(),
                "testing runtime absence",
                span(),
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn invalid_semantic_ir_is_rejected_at_its_exact_span() {
        let expression = rumoca_core::Expression::Empty { span: span() };
        let error = evaluate_optional(
            &expression,
            &EvalContext::structural_preidentity(),
            "testing invalid semantic IR",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("invalid semantic IR must not be treated as an unknown value");

        assert!(matches!(
            error,
            FlattenError::ConstantEvaluationFailed { span: actual, .. } if actual == span()
        ));
    }

    #[test]
    fn invalid_dummy_expression_uses_the_nearest_honest_owner() {
        let expression = rumoca_core::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        };
        let error = evaluate_optional(
            &expression,
            &EvalContext::structural_preidentity(),
            "testing owner provenance",
            span(),
        )
        .expect_err("dummy child provenance must fall back to its proven owner");

        assert!(matches!(
            error,
            FlattenError::ConstantEvaluationFailed { span: actual, .. } if actual == span()
        ));
    }

    #[test]
    fn invalid_dummy_expression_and_owner_fail_without_fabricated_provenance() {
        let expression = rumoca_core::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        };
        let error = evaluate_optional(
            &expression,
            &EvalContext::structural_preidentity(),
            "testing missing provenance",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("constant-evaluation diagnostics require honest provenance");

        assert!(matches!(error, FlattenError::MissingSourceContext { .. }));
    }

    #[test]
    fn typed_optional_evaluation_rejects_a_successful_value_of_the_wrong_type() {
        let real = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: span(),
        };
        let boolean_error = evaluate_optional_boolean(
            &real,
            &EvalContext::structural_preidentity(),
            "testing Boolean result type",
            span(),
        )
        .expect_err("a Real result cannot become an unknown Boolean");
        assert!(matches!(
            boolean_error,
            FlattenError::ConstantEvaluationFailed { .. }
        ));

        let string_error = evaluate_optional_string(
            &real,
            &EvalContext::structural_preidentity(),
            "testing String result type",
            span(),
        )
        .expect_err("a Real result cannot become an unknown String");
        assert!(matches!(
            string_error,
            FlattenError::ConstantEvaluationFailed { .. }
        ));
    }
}
