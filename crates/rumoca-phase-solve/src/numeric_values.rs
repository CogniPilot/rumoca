//! Transfer source numeric runs without recovering repetition from dense values.

use rumoca_eval_dae::{NumericInitialRun, NumericInitialValues};
use rumoca_ir_solve::{SolveInitialValues, SolveInitialValuesError};

pub(crate) fn from_typed_source(
    values: &rumoca_eval_dae::SourceValues,
    kind: rumoca_ir_dae::ScalarType,
    span: rumoca_core::Span,
) -> Result<rumoca_ir_solve::SolveDeclarationStart, String> {
    use rumoca_ir_solve::SolveDeclarationStart as Start;
    if kind == rumoca_ir_dae::ScalarType::Real {
        return from_source(&values.numerical_values())
            .map(Start::Real)
            .map_err(|e| e.to_string());
    }
    match rumoca_eval_dae::exact_source_values(kind, values, span).map_err(|e| e.to_string())? {
        rumoca_eval_dae::ExactDeclarationValues::Integer(v) => from_exact(&v).map(Start::Integer),
        rumoca_eval_dae::ExactDeclarationValues::Enumeration(v) => {
            from_exact(&v).map(Start::Enumeration)
        }
        rumoca_eval_dae::ExactDeclarationValues::Boolean(v) => from_exact(&v).map(Start::Boolean),
    }
    .map_err(|e| e.to_string())
}

pub(crate) fn from_exact<T: Clone>(
    values: &rumoca_eval_dae::ExactInitialValues<T>,
) -> Result<rumoca_ir_solve::SolveExactInitialValues<T>, SolveInitialValuesError> {
    let mut start = 0usize;
    let runs = values
        .runs()
        .map(|run| {
            let result = match run {
                rumoca_eval_dae::ExactInitialRun::Repeat { value, count } => {
                    rumoca_ir_solve::SolveExactInitialRun::Repeat {
                        start,
                        value: value.clone(),
                        count: *count,
                    }
                }
                rumoca_eval_dae::ExactInitialRun::Literal(values) => {
                    rumoca_ir_solve::SolveExactInitialRun::Literal {
                        start,
                        values: values.clone(),
                    }
                }
            };
            start += result.count();
            result
        })
        .collect();
    rumoca_ir_solve::SolveExactInitialValues::construct(values.len(), runs)
}

pub(crate) fn from_source(
    values: &NumericInitialValues,
) -> Result<SolveInitialValues, SolveInitialValuesError> {
    SolveInitialValues::concatenate(
        values
            .runs()
            .map(|run| match run {
                NumericInitialRun::Repeat { value, count } => {
                    SolveInitialValues::repeat(value, count)
                }
                NumericInitialRun::Literal(values) => Ok(values.to_vec().into()),
            })
            .collect::<Result<Vec<_>, _>>()?,
    )
}
