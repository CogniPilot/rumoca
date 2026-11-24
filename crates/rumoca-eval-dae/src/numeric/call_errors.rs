//! Refusals of a function call whose parameters, results or body this
//! evaluator cannot execute.

use rumoca_core::Span;
use rumoca_ir_dae as dae;

use super::{NumericEvaluationError, NumericEvaluationErrorKind, failure};

pub(super) fn function_parameter_error(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::NonStaticCoordinate,
        "function parameter escaped its checked call owner",
        span,
    )
}

pub(super) fn function_ordinal_error(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::OutOfBounds,
        "function parameter ordinal is out of range",
        span,
    )
}

pub(crate) fn external_function_failure(
    name: &rumoca_core::VarName,
    external: dae::ExternalFunctionView<'_>,
    span: Span,
) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::ExternalFunction,
        format!(
            "external {} function `{}` calls `{}`, which this runtime cannot execute",
            external.language().as_str(),
            name,
            external.symbol()
        ),
        span,
    )
}

pub(super) fn function_result_error(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::OutOfBounds,
        "function result ordinal is out of range",
        span,
    )
}
