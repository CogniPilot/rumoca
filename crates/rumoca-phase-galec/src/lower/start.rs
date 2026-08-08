use super::*;

#[cfg(test)]
mod tests;

pub(super) struct StartShape<'dae> {
    dimensions: Vec<usize>,
    scalar_count: usize,
    variable: &'dae rumoca_core::VarName,
}

#[derive(Debug)]
pub(super) enum StartValues {
    Scalar { value: f64, span: Span },
    Shaped { values: Vec<f64>, span: Span },
}

impl StartValues {
    pub(super) fn shaped(values: Vec<f64>, span: Span) -> Self {
        Self::Shaped { values, span }
    }

    pub(super) fn evaluated(
        values: Vec<f64>,
        scalar: bool,
        variable: &rumoca_core::VarName,
        span: Span,
    ) -> Result<Self, GalecTargetError> {
        if !scalar {
            return Ok(Self::Shaped { values, span });
        }
        match values.as_slice() {
            [value] => Ok(Self::Scalar {
                value: *value,
                span,
            }),
            _ => Err(GalecTargetError::AttributeNotEvaluable {
                variable: variable.to_string(),
                attribute: "start",
                reason: format!(
                    "checked scalar expression evaluated to {} scalar(s)",
                    values.len()
                ),
                span: Some(span),
            }),
        }
    }
}

impl<'dae> StartShape<'dae> {
    pub(super) fn checked(variable: dae::VariableView<'dae>) -> Result<Self, GalecTargetError> {
        let span = variable.declaration().span();
        let dimensions = variable
            .value_type()
            .dimensions()
            .iter()
            .enumerate()
            .map(|(index, extent)| {
                if *extent == 0 {
                    return Err(GalecTargetError::NonPositiveDimension {
                        variable: variable.name().to_string(),
                        dimension: index + 1,
                        size: 0,
                        span,
                    });
                }
                usize::try_from(*extent).map_err(|_| GalecTargetError::LoweringInternal {
                    detail: format!(
                        "dimension {} of `{}` exceeds usize",
                        index + 1,
                        variable.name()
                    ),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let scalar_count = dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent))
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("start shape for `{}` exceeds usize", variable.name()),
            })?;
        if scalar_count != variable.scalar_count() {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "checked shape for `{}` has {scalar_count} scalars but its DAE type reports {}",
                    variable.name(),
                    variable.scalar_count()
                ),
            });
        }
        Ok(Self {
            dimensions,
            scalar_count,
            variable: variable.name(),
        })
    }

    pub(super) fn real(&self, values: StartValues) -> Result<gast::Expression, GalecTargetError> {
        self.nested(values, |value| Ok(gast::Expression::Real(value)))
    }

    pub(super) fn integer(
        &self,
        values: StartValues,
    ) -> Result<gast::Expression, GalecTargetError> {
        let span = values.span();
        self.nested(values, move |value| {
            exact_integer(value, span).map(gast::Expression::Integer)
        })
    }

    pub(super) fn boolean(
        &self,
        values: StartValues,
    ) -> Result<gast::Expression, GalecTargetError> {
        let span = values.span();
        self.nested(values, move |value| match value {
            0.0 => Ok(gast::Expression::Bool(false)),
            1.0 => Ok(gast::Expression::Bool(true)),
            _ => Err(GalecTargetError::AttributeTypeMismatch {
                variable: self.variable.to_string(),
                attribute: "start",
                expected: "Boolean",
                found: "non-Boolean numeric",
                span: Some(span),
            }),
        })
    }

    fn nested(
        &self,
        values: StartValues,
        convert: impl Fn(f64) -> Result<gast::Expression, GalecTargetError>,
    ) -> Result<gast::Expression, GalecTargetError> {
        let expressions = self.expressions(values, convert)?;
        nest_row_major(&self.dimensions, expressions).ok_or_else(|| {
            GalecTargetError::LoweringInternal {
                detail: format!(
                    "start constructor for `{}` did not close to one expression",
                    self.variable
                ),
            }
        })
    }

    fn expressions(
        &self,
        values: StartValues,
        convert: impl Fn(f64) -> Result<gast::Expression, GalecTargetError>,
    ) -> Result<Vec<gast::Expression>, GalecTargetError> {
        match values {
            StartValues::Scalar { value, .. } => std::iter::repeat_n(value, self.scalar_count)
                .map(convert)
                .collect::<Result<Vec<_>, _>>(),
            StartValues::Shaped { values, .. } if values.len() == self.scalar_count => values
                .into_iter()
                .map(convert)
                .collect::<Result<Vec<_>, _>>(),
            StartValues::Shaped { values, span } => Err(GalecTargetError::AttributeNotEvaluable {
                variable: self.variable.to_string(),
                attribute: "start",
                reason: format!(
                    "evaluates to {} scalar(s), but the declared shape requires {}",
                    values.len(),
                    self.scalar_count
                ),
                span: Some(span),
            }),
        }
    }
}

impl StartValues {
    const fn span(&self) -> Span {
        match self {
            Self::Scalar { span, .. } | Self::Shaped { span, .. } => *span,
        }
    }
}

fn nest_row_major(
    dimensions: &[usize],
    mut expressions: Vec<gast::Expression>,
) -> Option<gast::Expression> {
    if dimensions.is_empty() {
        return (expressions.len() == 1).then(|| expressions.remove(0));
    }
    for extent in dimensions.iter().rev() {
        let group_count = expressions.len().checked_div(*extent)?;
        if group_count.checked_mul(*extent)? != expressions.len() {
            return None;
        }
        let mut elements = expressions.into_iter();
        expressions = (0..group_count)
            .map(|_| gast::Expression::Array(elements.by_ref().take(*extent).collect()))
            .collect();
    }
    (expressions.len() == 1).then(|| expressions.remove(0))
}
