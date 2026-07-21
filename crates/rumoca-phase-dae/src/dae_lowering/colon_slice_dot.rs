use super::*;
use rumoca_core::FallibleExpressionRewriter;

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
        rumoca_core::Expression::Unary { rhs, .. } => {
            return Ok(scalar_dot_operand(classify_dot_operand(rhs, array_dims)?));
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            return Ok(scalar_dot_operand_pair(
                classify_dot_operand(lhs, array_dims)?,
                classify_dot_operand(rhs, array_dims)?,
            ));
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if !matches!(
                    classify_dot_operand(condition, array_dims)?,
                    DotOperand::Scalar
                ) || !matches!(classify_dot_operand(value, array_dims)?, DotOperand::Scalar)
                {
                    return Ok(DotOperand::Unsafe);
                }
            }
            return Ok(scalar_dot_operand(classify_dot_operand(
                else_branch,
                array_dims,
            )?));
        }
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

fn scalar_dot_operand(operand: DotOperand) -> DotOperand {
    scalar_dot_operand_pair(operand, DotOperand::Scalar)
}

fn scalar_dot_operand_pair(lhs: DotOperand, rhs: DotOperand) -> DotOperand {
    if matches!(lhs, DotOperand::Scalar) && matches!(rhs, DotOperand::Scalar) {
        DotOperand::Scalar
    } else {
        DotOperand::Unsafe
    }
}

pub(super) fn lower_colon_slice_dot_products(
    expr: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    ColonSliceDotLowerer { array_dims }.rewrite_expression(expr)
}

struct ColonSliceDotLowerer<'a> {
    array_dims: &'a HashMap<String, Vec<i64>>,
}

impl FallibleExpressionRewriter for ColonSliceDotLowerer<'_> {
    type Error = ToDaeError;

    fn walk_binary_expression(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        lower_colon_slice_binary_expr(op, lhs, rhs, span, self.array_dims)
    }
}
