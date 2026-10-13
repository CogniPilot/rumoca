use super::*;

/// Evaluate the complete tuple of a SPEC_0040 DAE-C30 native body through its one
/// definitional evaluator, retaining the declared scalar payload types.
pub(super) fn native_function_call(
    binding: dae::NativeBodyView<'_>,
    inputs: &[SourceValues],
    span: Span,
) -> Result<Vec<SourceValues>, NumericEvaluationError> {
    use rumoca_core::native_body::{NativeBodyError, NativeElement, NativeScalar};
    let body = binding.body();
    let inputs = inputs
        .iter()
        .zip(body.inputs())
        .map(|(values, input)| {
            values
                .materialize()
                .into_iter()
                .map(|value| match input.element {
                    NativeElement::Integer => {
                        value.integer().map(NativeScalar::Integer).ok_or_else(|| {
                            failure(
                                NumericEvaluationErrorKind::InvalidValue,
                                "native Integer input lacks an exact Integer payload",
                                span,
                            )
                        })
                    }
                    NativeElement::Real => Ok(NativeScalar::Real(value.real())),
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;
    let inputs = inputs.iter().map(Vec::as_slice).collect::<Vec<_>>();
    let outputs = body.evaluate(&inputs).map_err(|error| {
        let kind = match error {
            NativeBodyError::Failure { .. } => NumericEvaluationErrorKind::AssertionFailed,
            NativeBodyError::OperandMismatch { .. } => NumericEvaluationErrorKind::InvalidValue,
        };
        failure(kind, error.to_string(), span)
    })?;
    if binding.results().len() != outputs.len() {
        return Err(function_result_error(span));
    }
    let mut tuple = vec![None; outputs.len()];
    for (result, values) in binding.results().iter().zip(outputs) {
        let destination = tuple
            .get_mut(*result as usize)
            .ok_or_else(|| function_result_error(span))?;
        if destination.is_some() {
            return Err(function_result_error(span));
        }
        *destination = Some(SourceValues::literal(
            values
                .into_iter()
                .map(|value| match value {
                    NativeScalar::Integer(value) => Scalar::Integer(value),
                    NativeScalar::Real(value) => Scalar::Real(value),
                })
                .collect(),
        ));
    }
    tuple
        .into_iter()
        .map(|value| value.ok_or_else(|| function_result_error(span)))
        .collect()
}
