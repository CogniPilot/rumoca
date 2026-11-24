//! Checks on the values numeric evaluation produces or a host supplies.

use rumoca_core::Span;
use rumoca_ir_dae as dae;

use super::{
    NumericEvaluationError, NumericEvaluationErrorKind, NumericInitialRun, Scalar, SourceValues,
    failure,
};

pub(super) fn host_scalar(
    variable: dae::VariableView<'_>,
    value: f64,
) -> Result<Scalar, NumericEvaluationError> {
    let payload = match variable.value_type().scalar_type() {
        dae::ScalarType::Real => Some(Scalar::Real(value)),
        dae::ScalarType::Integer | dae::ScalarType::Enumeration
            if value.is_finite()
                && value.fract() == 0.0
                && value.abs() <= 9_007_199_254_740_992.0 =>
        {
            Some(
                if variable.value_type().scalar_type() == dae::ScalarType::Integer {
                    Scalar::Integer(value as i64)
                } else {
                    Scalar::Enumeration(value as i64)
                },
            )
        }
        dae::ScalarType::Boolean if value == 0.0 || value == 1.0 => {
            Some(Scalar::Boolean(value == 1.0))
        }
        _ => None,
    };
    payload.ok_or_else(|| {
        failure(
            NumericEvaluationErrorKind::InvalidOverride,
            "numerical host override cannot represent the declared exact scalar type",
            variable.declaration().span(),
        )
    })
}

pub(super) fn require_finite(
    values: &SourceValues,
    span: Span,
) -> Result<(), NumericEvaluationError> {
    if values.runs().any(|run| match run {
        NumericInitialRun::Repeat { value, .. } => matches!(value, Scalar::Invalid),
        NumericInitialRun::Literal(values) => {
            values.iter().any(|value| matches!(value, Scalar::Invalid))
        }
    }) {
        return Err(failure(
            NumericEvaluationErrorKind::Overflow,
            "exact Integer operation has no supported checked result",
            span,
        ));
    }
    if values.runs().all(|run| match run {
        NumericInitialRun::Repeat { value, .. } => value.is_finite(),
        NumericInitialRun::Literal(values) => values.iter().all(|value| value.is_finite()),
    }) {
        Ok(())
    } else {
        Err(failure(
            NumericEvaluationErrorKind::InvalidValue,
            "numeric evaluation produced a non-finite result",
            span,
        ))
    }
}
