use super::*;

pub(super) fn manifest_start_expression(
    variable: &ManifestVariable,
) -> Result<gast::Expression, GalecTargetError> {
    match variable {
        ManifestVariable::Real(variable) => {
            nested_start(&variable.common.dimensions, &variable.start, |value| {
                gast::Expression::Real(*value)
            })
        }
        ManifestVariable::Integer(variable) => {
            nested_start(&variable.common.dimensions, &variable.start, |value| {
                gast::Expression::Integer(i64::from(*value))
            })
        }
        ManifestVariable::Boolean(variable) => {
            nested_start(&variable.common.dimensions, &variable.start, |value| {
                gast::Expression::Bool(*value)
            })
        }
    }
}

fn nested_start<T>(
    dimensions: &[u64],
    start: &StartValue<T>,
    convert: impl Fn(&T) -> gast::Expression + Copy,
) -> Result<gast::Expression, GalecTargetError> {
    let values = match start {
        StartValue::Scalar(value) => {
            let count = dimensions.iter().try_fold(1usize, |count, extent| {
                usize::try_from(*extent)
                    .ok()
                    .and_then(|extent| count.checked_mul(extent))
            });
            vec![
                convert(value);
                count.ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "manifest start dimension product overflowed".to_owned(),
                })?
            ]
        }
        StartValue::Array(values) => values.iter().map(convert).collect(),
    };
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
        detail: "manifest start dimension product overflowed".to_owned(),
    })?;
    let first = usize::try_from(first).map_err(|_| GalecTargetError::LoweringInternal {
        detail: "manifest dimension exceeds usize".to_owned(),
    })?;
    if values.len() != first.saturating_mul(width) {
        return Err(GalecTargetError::LoweringInternal {
            detail: "manifest start shape does not match its dimensions".to_owned(),
        });
    }
    let elements = values
        .chunks(width)
        .map(|chunk| nest_row_major(rest, chunk))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(gast::Expression::Array(elements))
}
