//! Materialize the proof DAG as compact Boolean data, never numeric tests.

use super::*;
use rumoca_eval_dae::{ZeroCoefficientPattern, ZeroCoefficientPatternView};

pub(super) fn issue<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    pattern: &ZeroCoefficientPattern,
    memo: &mut HashMap<ZeroCoefficientPattern, solve::ProgramRegister<'program>>,
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    if let Some(&register) = memo.get(pattern) {
        return Ok(register);
    }
    let value = match pattern.view() {
        ZeroCoefficientPatternView::Uniform(zero) => {
            let value = builder.constant(solve::SolveValue::boolean(zero), at)?;
            if pattern.dimensions().is_empty() {
                value
            } else {
                builder.fill(value, pattern.dimensions().to_vec(), at)?
            }
        }
        ZeroCoefficientPatternView::Array(elements) => {
            let values = issue_elements(builder, elements, memo, at)?;
            builder.construct_aggregate(&values, pattern.dimensions().to_vec(), at)?
        }
        ZeroCoefficientPatternView::Concatenate(axis, elements) => {
            let values = issue_elements(builder, elements, memo, at)?;
            builder.concatenate(axis, &values, at)?
        }
        ZeroCoefficientPatternView::Transpose(base) => {
            let value = issue(builder, base, memo, at)?;
            builder.transpose(value, at)?
        }
        ZeroCoefficientPatternView::Index(base, indices) => {
            let value = issue(builder, base, memo, at)?;
            let axes = base
                .dimensions()
                .iter()
                .enumerate()
                .map(
                    |(axis, &extent)| match indices.get(axis).copied().flatten() {
                        Some(index) => {
                            integer(builder, index, at).map(solve::ProgramTensorViewAxis::Index)
                        }
                        None => Ok(solve::ProgramTensorViewAxis::Span { origin: 0, extent }),
                    },
                )
                .collect::<Result<Vec<_>, _>>()?;
            builder.project_view(value, &axes, at)?
        }
        ZeroCoefficientPatternView::Union(lhs, rhs) => {
            let lhs = issue(builder, lhs, memo, at)?;
            let rhs = issue(builder, rhs, memo, at)?;
            builder.binary(solve::SolveBinaryOperator::Or, lhs, rhs, at)?
        }
    };
    memo.insert(pattern.clone(), value);
    Ok(value)
}

fn issue_elements<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    elements: &[ZeroCoefficientPattern],
    memo: &mut HashMap<ZeroCoefficientPattern, solve::ProgramRegister<'program>>,
    at: rumoca_core::Span,
) -> Result<Vec<solve::ProgramRegister<'program>>, solve::SolveProgramConstructionError> {
    elements
        .iter()
        .map(|element| issue(builder, element, memo, at))
        .collect()
}

pub(super) fn integer<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    value: i64,
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    let value = solve::SolveValue::integer(arithmetic_profile(), value)
        .map_err(|_| solve::SolveProgramConstructionError::ProfileMismatch { provenance: at })?;
    builder.constant(value, at)
}
