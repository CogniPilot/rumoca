use super::*;

pub(super) fn real_start(
    dimensions: &[u64],
    values: Vec<f64>,
) -> Result<gast::Expression, GalecTargetError> {
    nested_start(dimensions, values.into_iter().map(gast::Expression::Real).collect())
}

pub(super) fn integer_start(
    dimensions: &[u64],
    values: Vec<f64>,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    nested_start(
        dimensions,
        values
            .into_iter()
            .map(|value| exact_integer(value, span).map(gast::Expression::Integer))
            .collect::<Result<Vec<_>, _>>()?,
    )
}

pub(super) fn boolean_start(
    dimensions: &[u64],
    values: Vec<f64>,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    nested_start(
        dimensions,
        values
            .into_iter()
            .map(|value| match value {
                0.0 => Ok(gast::Expression::Bool(false)),
                1.0 => Ok(gast::Expression::Bool(true)),
                _ => Err(GalecTargetError::AttributeTypeMismatch {
                    variable: "<checked Boolean>".to_owned(),
                    attribute: "start",
                    expected: "Boolean",
                    found: "non-Boolean numeric",
                    span: Some(span),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?,
    )
}

fn nested_start(
    dimensions: &[u64],
    values: Vec<gast::Expression>,
) -> Result<gast::Expression, GalecTargetError> {
    if dimensions.is_empty() {
        return values
            .into_iter()
            .next()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "scalar start has no value".to_owned(),
            });
    }
    nest_row_major(dimensions, &values)
}

fn nest_row_major(
    dimensions: &[u64],
    values: &[gast::Expression],
) -> Result<gast::Expression, GalecTargetError> {
    let Some((&first, rest)) = dimensions.split_first() else {
        return values
            .first()
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "scalar start has no value".to_owned(),
            });
    };
    let width = rest.iter().try_fold(1usize, |count, extent| {
        usize::try_from(*extent)
            .ok()
            .and_then(|extent| count.checked_mul(extent))
    });
    let width = width.ok_or_else(|| GalecTargetError::LoweringInternal {
        detail: "start dimension product overflowed".to_owned(),
    })?;
    let first = usize::try_from(first).map_err(|_| GalecTargetError::LoweringInternal {
        detail: "dimension exceeds usize".to_owned(),
    })?;
    if values.len() != first.saturating_mul(width) {
        return Err(GalecTargetError::LoweringInternal {
            detail: "start shape does not match its dimensions".to_owned(),
        });
    }
    values
        .chunks(width)
        .map(|chunk| nest_row_major(rest, chunk))
        .collect::<Result<Vec<_>, _>>()
        .map(gast::Expression::Array)
}
