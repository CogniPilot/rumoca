use super::*;

pub(super) enum DotOperand {
    Scalar,
    Vector(rumoca_core::Expression),
    Unsafe,
}

pub(super) fn is_colon_slice(expr: &rumoca_core::Expression) -> bool {
    matches!(expr, rumoca_core::Expression::Index { subscripts, .. } if subscripts_have_colon(subscripts))
}

pub(super) fn classify_dot_operand(
    expr: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<DotOperand, ToDaeError> {
    let (name, subscripts, span) = match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => (name, subscripts.as_slice(), *span),
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => match base.as_ref() {
            rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } if base_subscripts.is_empty() => (name, subscripts.as_slice(), *span),
            _ => return Ok(DotOperand::Unsafe),
        },
        rumoca_core::Expression::Array {
            elements,
            is_matrix: false,
            ..
        } => {
            for element in elements {
                if !matches!(
                    classify_dot_operand(element, array_dims)?,
                    DotOperand::Scalar
                ) {
                    return Ok(DotOperand::Unsafe);
                }
            }
            return Ok(DotOperand::Vector(expr.clone()));
        }
        rumoca_core::Expression::Array { .. } => return Ok(DotOperand::Unsafe),
        rumoca_core::Expression::Literal { .. } => return Ok(DotOperand::Scalar),
        _ => return Ok(DotOperand::Unsafe),
    };
    let Some(dims) = array_dims.get(name.as_str()) else {
        return Ok(DotOperand::Unsafe);
    };
    let Some(projected_dims) = projected_dims_for_subscripts(dims, subscripts) else {
        return Ok(DotOperand::Unsafe);
    };
    match projected_dims.as_slice() {
        [] => Ok(DotOperand::Scalar),
        [_] => {
            let Some(elements) = project_colon_slice_elements(
                name,
                dims,
                subscripts,
                compute_var_size(&projected_dims),
                span,
            )?
            else {
                return Ok(DotOperand::Unsafe);
            };
            Ok(DotOperand::Vector(rumoca_core::Expression::Array {
                elements,
                is_matrix: false,
                span,
            }))
        }
        _ => Ok(DotOperand::Unsafe),
    }
}
