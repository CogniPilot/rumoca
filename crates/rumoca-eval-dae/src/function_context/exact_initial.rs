//! Exact declaration payloads issued by the single ordered source evaluator.

use rumoca_core::Span;
use rumoca_ir_dae as dae;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone, thiserror::Error)]
#[error("exact initialization refused: {message}")]
pub struct ExactInitialError {
    pub message: String,
    pub span: Span,
}
#[derive(Debug, Clone)]
pub enum ExactInitialRun<T> {
    Repeat { value: T, count: usize },
    Literal(Vec<T>),
}
#[derive(Debug, Clone)]
pub struct ExactInitialValues<T> {
    runs: Vec<ExactInitialRun<T>>,
    count: usize,
}
impl<T> ExactInitialValues<T> {
    pub fn len(&self) -> usize {
        self.count
    }
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }
    pub fn runs(&self) -> impl Iterator<Item = &ExactInitialRun<T>> {
        self.runs.iter()
    }
}
#[derive(Debug, Clone)]
pub enum ExactDeclarationValues {
    Integer(ExactInitialValues<i64>),
    Enumeration(ExactInitialValues<i64>),
    Boolean(ExactInitialValues<bool>),
}

pub fn exact_initial_values<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    count: usize,
) -> Result<ExactDeclarationValues, ExactInitialError> {
    let node = view
        .expression(expression)
        .expect("branded expression resolves");
    let span = node.provenance().span();
    let source_count = node
        .value_type()
        .dimensions()
        .iter()
        .try_fold(1usize, |n, extent| n.checked_mul(*extent as usize))
        .ok_or_else(|| refusal("source extent overflows", span))?;
    if source_count != count && source_count != 1 {
        return Err(refusal("source extent does not match declaration", span));
    }
    let empty = crate::numeric::SourceValues::default();
    if count == 0 {
        return match node.value_type().scalar_type() {
            dae::ScalarType::Integer => {
                convert(&empty, span, |_| None::<i64>).map(ExactDeclarationValues::Integer)
            }
            dae::ScalarType::Enumeration => {
                convert(&empty, span, |_| None::<i64>).map(ExactDeclarationValues::Enumeration)
            }
            dae::ScalarType::Boolean => {
                convert(&empty, span, |_| None::<bool>).map(ExactDeclarationValues::Boolean)
            }
            _ => Err(refusal("source type is outside exact initialization", span)),
        };
    }
    let mut evaluator = crate::NumericEvaluator::with_input_policy(
        view,
        |_variable, _ordinal| None,
        dae::InputInitializationPolicy::HostDrivenStart,
    );
    let mut values = evaluator
        .exact_expression_values(expression)
        .map_err(|error| ExactInitialError {
            span: error.span(),
            message: error.to_string(),
        })?;
    if values.len() != source_count {
        return Err(refusal(
            "executed source extent does not match its checked type",
            span,
        ));
    }
    values.broadcast(count);
    exact_source_values(node.value_type().scalar_type(), &values, span)
}

/// Convert already issued source runs without reevaluating their expression.
pub fn exact_source_values(
    kind: dae::ScalarType,
    values: &crate::numeric::SourceValues,
    span: Span,
) -> Result<ExactDeclarationValues, ExactInitialError> {
    match kind {
        dae::ScalarType::Integer | dae::ScalarType::Enumeration => {
            let payload = convert(values, span, |value| match value {
                crate::numeric::Scalar::Integer(value)
                | crate::numeric::Scalar::Enumeration(value) => Some(value),
                _ => None,
            })?;
            Ok(if kind == dae::ScalarType::Integer {
                ExactDeclarationValues::Integer(payload)
            } else {
                ExactDeclarationValues::Enumeration(payload)
            })
        }
        dae::ScalarType::Boolean => convert(values, span, |value| match value {
            crate::numeric::Scalar::Boolean(value) => Some(value),
            _ => None,
        })
        .map(ExactDeclarationValues::Boolean),
        _ => Err(refusal("source type is outside exact initialization", span)),
    }
}

fn convert<T>(
    source: &crate::numeric::SourceValues,
    span: Span,
    mut value: impl FnMut(crate::numeric::Scalar) -> Option<T>,
) -> Result<ExactInitialValues<T>, ExactInitialError> {
    let mut runs = Vec::new();
    for run in source.runs() {
        runs.push(match run {
            crate::NumericInitialRun::Repeat {
                value: scalar,
                count,
            } => ExactInitialRun::Repeat {
                value: value(scalar).ok_or_else(|| {
                    refusal("executed source payload has a foreign scalar type", span)
                })?,
                count,
            },
            crate::NumericInitialRun::Literal(elements) => ExactInitialRun::Literal(
                elements
                    .iter()
                    .copied()
                    .map(&mut value)
                    .collect::<Option<Vec<_>>>()
                    .ok_or_else(|| {
                        refusal("executed source payload has a foreign scalar type", span)
                    })?,
            ),
        });
    }
    Ok(ExactInitialValues {
        runs,
        count: source.len(),
    })
}
fn refusal(message: &str, span: Span) -> ExactInitialError {
    ExactInitialError {
        message: message.into(),
        span,
    }
}
