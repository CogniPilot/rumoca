//! Static shape modelling and flat-index projection for symbolic differentiation.
//!
//! These helpers answer two questions the differentiator asks constantly: what
//! shape does an expression have, and what does its `k`-th scalar element look
//! like. They are separated from the differentiation rules so that widening the
//! shape model stays reviewable on its own.

use super::*;

pub(super) fn expression_dims(expr: &Expression, dae: &Dae) -> Option<Vec<i64>> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => variable_dims_for_name(dae, name.var_name()),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_expression_dims(elements, *is_matrix),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => args.first().and_then(|arg| expression_dims(arg, dae)),
        Expression::Index {
            base, subscripts, ..
        } => sliced_dims(base, subscripts, dae),
        Expression::Unary { rhs, .. } => expression_dims(rhs, dae),
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs_dims = expression_dims(lhs, dae);
            let rhs_dims = expression_dims(rhs, dae);
            match op {
                OpBinary::Mul => matrix_product_dims(lhs_dims, rhs_dims),
                OpBinary::Div => lhs_dims,
                OpBinary::Add
                | OpBinary::AddElem
                | OpBinary::Sub
                | OpBinary::SubElem
                | OpBinary::MulElem
                | OpBinary::DivElem => lhs_dims.or(rhs_dims),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Shape of `base[subscripts]` when every subscript is a compile-time constant
/// selector: an index drops that dimension, a `:` keeps it whole (MLS 3.7 §10.5).
///
/// Returns `None` for a fully indexed element, matching [`expression_dims`]'s
/// convention that a scalar has no modelled shape.
pub(super) fn sliced_dims(
    base: &Expression,
    subscripts: &[Subscript],
    dae: &Dae,
) -> Option<Vec<i64>> {
    let base_dims = expression_dims(base, dae)?;
    if subscripts.len() != base_dims.len() {
        return None;
    }
    let mut dims = Vec::new();
    for (subscript, dim) in subscripts.iter().zip(&base_dims) {
        if matches!(subscript, Subscript::Colon { .. }) {
            dims.push(*dim);
            continue;
        }
        let [index] = static_subscript_indices(std::slice::from_ref(subscript))?[..] else {
            return None;
        };
        if index <= 0 || index > *dim {
            return None;
        }
    }
    (!dims.is_empty()).then_some(dims)
}

pub(super) fn matrix_product_dims(
    lhs: Option<Vec<i64>>,
    rhs: Option<Vec<i64>>,
) -> Option<Vec<i64>> {
    match (lhs, rhs) {
        (None, rhs) => rhs,
        (lhs, None) => lhs,
        (Some(lhs), Some(rhs)) => match (lhs.as_slice(), rhs.as_slice()) {
            ([rows, inner], [rhs_inner, cols]) if inner == rhs_inner => Some(vec![*rows, *cols]),
            ([rows, inner], [rhs_inner]) if inner == rhs_inner => Some(vec![*rows]),
            ([inner], [rhs_inner, cols]) if inner == rhs_inner => Some(vec![*cols]),
            ([lhs_n], [rhs_n]) if lhs_n == rhs_n => None,
            _ => None,
        },
    }
}

pub(super) fn expression_is_scalar(expr: &Expression, dae: &Dae) -> bool {
    if expression_dims(expr, dae).is_some() {
        return false;
    }
    match expr {
        Expression::Literal { .. } => true,
        Expression::VarRef {
            name, subscripts, ..
        } => {
            !subscripts.is_empty()
                || super::record_projection::variable_is_scalar(dae, name.var_name())
        }
        Expression::Unary { rhs, .. } => expression_is_scalar(rhs, dae),
        _ => false,
    }
}

pub(super) fn variable_dims_for_name(dae: &Dae, name: &VarName) -> Option<Vec<i64>> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.inputs.get(name))
        .or_else(|| dae.variables.parameters.get(name))
        .or_else(|| dae.variables.constants.get(name))
        .map(|var| var.dims.clone())
        .filter(|dims| !dims.is_empty())
}

pub(super) fn array_expression_dims(elements: &[Expression], is_matrix: bool) -> Option<Vec<i64>> {
    if !is_matrix {
        return Some(vec![elements.len() as i64]);
    }
    let cols = match elements.first()? {
        Expression::Array { elements, .. } => elements.len(),
        _ => return None,
    };
    Some(vec![elements.len() as i64, cols as i64])
}

pub(super) fn project_flat_index(
    expr: &Expression,
    dims: &[i64],
    flat_index: usize,
    dae: &Dae,
) -> Option<Expression> {
    project_flat_index_with_span(expr, dims, flat_index, None, dae)
}

pub(super) fn projection_span(expr: &Expression, fallback_span: Option<Span>) -> Option<Span> {
    expr.span()
        .or_else(|| fallback_span.filter(|span| !span.is_dummy()))
}

pub(super) fn project_flat_index_with_span(
    expr: &Expression,
    dims: &[i64],
    flat_index: usize,
    fallback_span: Option<Span>,
    dae: &Dae,
) -> Option<Expression> {
    match expr {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
            let projection_span = projection_span(expr, fallback_span)?;
            Some(Expression::VarRef {
                name: name.clone(),
                subscripts: generated_index_subscripts(
                    indices,
                    projection_span,
                    "flat-index projected variable reference",
                )?,
                span: if span.is_dummy() {
                    projection_span
                } else {
                    *span
                },
            })
        }
        Expression::Array { elements, .. } => {
            flatten_array_elements(elements).get(flat_index).cloned()
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => {
            let span = projection_span(expr, fallback_span)?;
            Some(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![project_flat_index_with_span(
                    &args[0],
                    dims,
                    flat_index,
                    Some(span),
                    dae,
                )?],
                span,
            })
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            // A matrix product distributes over its operands only when one side
            // is a proven scalar; otherwise the product's own shape is what is
            // being indexed, so index the product node itself.
            if matches!(op, OpBinary::Mul | OpBinary::Div)
                && !expression_is_scalar(lhs, dae)
                && !expression_is_scalar(rhs, dae)
            {
                return project_indexed_expression(expr, dims, flat_index, fallback_span);
            }
            let span = projection_span(expr, fallback_span)?;
            project_binary_operands(op, lhs, rhs, span, dims, flat_index, dae)
        }
        Expression::Unary { op, rhs, .. } => {
            let span = projection_span(expr, fallback_span)?;
            Some(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(project_flat_index_with_span(
                    rhs,
                    dims,
                    flat_index,
                    Some(span),
                    dae,
                )?),
                span,
            })
        }
        Expression::Index {
            base, subscripts, ..
        } if subscripts
            .iter()
            .any(|subscript| matches!(subscript, Subscript::Colon { .. })) =>
        {
            let span = projection_span(expr, fallback_span)?;
            project_slice_element(base, subscripts, span, dims, flat_index)
        }
        _ => project_indexed_expression(expr, dims, flat_index, fallback_span),
    }
}

/// Project both operands of a shape-preserving binary node, leaving a proven
/// scalar operand whole so `2 * v` projects to `2 * v[k]` rather than `2[k]`.
pub(super) fn project_binary_operands(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
    dims: &[i64],
    flat_index: usize,
    dae: &Dae,
) -> Option<Expression> {
    let project = |operand: &Expression| -> Option<Expression> {
        if expression_is_scalar(operand, dae) {
            return Some(operand.clone());
        }
        project_flat_index_with_span(operand, dims, flat_index, Some(span), dae)
    };
    Some(Expression::Binary {
        op: op.clone(),
        lhs: Box::new(project(lhs)?),
        rhs: Box::new(project(rhs)?),
        span,
    })
}

/// Replace the `:` subscripts of a slice with the concrete indices `flat_index`
/// selects, so element 2 of `A[1, :]` projects to `A[1, 3]` rather than to a
/// nested `A[1, :][3]` that no later stage can resolve to a scalar column.
pub(super) fn project_slice_element(
    base: &Expression,
    subscripts: &[Subscript],
    span: Span,
    dims: &[i64],
    flat_index: usize,
) -> Option<Expression> {
    let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
    let provenance = span
        .require_provenance("flat-index projected array slice")
        .ok()?;
    let mut selected = indices.into_iter();
    let mut resolved = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        if matches!(subscript, Subscript::Colon { .. }) {
            let index = i64::try_from(selected.next()?).ok()?;
            resolved.push(Subscript::generated_index_with_provenance(
                index, provenance,
            ));
        } else {
            resolved.push(subscript.clone());
        }
    }
    Some(merge_subscripts_into_base(base, resolved, span))
}

/// Fold resolved subscripts into the base reference when it carries none of its
/// own, so a projected slice element is a plain variable reference that the
/// incidence pass can attribute to a single scalar column.
pub(super) fn merge_subscripts_into_base(
    base: &Expression,
    subscripts: Vec<Subscript>,
    span: Span,
) -> Expression {
    if let Expression::VarRef {
        name,
        subscripts: base_subscripts,
        span: base_span,
    } = base
        && base_subscripts.is_empty()
    {
        return Expression::VarRef {
            name: name.clone(),
            subscripts,
            span: if base_span.is_dummy() {
                span
            } else {
                *base_span
            },
        };
    }
    Expression::Index {
        base: Box::new(base.clone()),
        subscripts,
        span,
    }
}

pub(super) fn project_indexed_expression(
    expr: &Expression,
    dims: &[i64],
    flat_index: usize,
    fallback_span: Option<Span>,
) -> Option<Expression> {
    let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
    let span = projection_span(expr, fallback_span)?;
    Some(Expression::Index {
        base: Box::new(expr.clone()),
        subscripts: generated_index_subscripts(indices, span, "flat-index projected expression")?,
        span,
    })
}

pub(super) fn generated_index_subscripts(
    indices: Vec<usize>,
    span: Span,
    context: &'static str,
) -> Option<Vec<Subscript>> {
    let provenance = span.require_provenance(context).ok()?;
    indices
        .into_iter()
        .map(|idx| {
            Some(Subscript::generated_index_with_provenance(
                i64::try_from(idx).ok()?,
                provenance,
            ))
        })
        .collect()
}

pub(super) fn static_subscript_indices(subscripts: &[Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => Some(*value),
                Expression::Literal {
                    value: Literal::Real(value),
                    ..
                } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
                _ => None,
            },
            Subscript::Colon { .. } => None,
        })
        .collect()
}

pub(super) fn flat_index_from_indices(dims: &[i64], indices: &[i64]) -> Option<usize> {
    if dims.len() != indices.len() || dims.is_empty() {
        return None;
    }
    let mut flat_index = 0usize;
    let mut stride = 1usize;
    for (&dim, &index) in dims.iter().rev().zip(indices.iter().rev()) {
        if dim <= 0 || index <= 0 || index > dim {
            return None;
        }
        flat_index = flat_index.checked_add((index as usize - 1).checked_mul(stride)?)?;
        stride = stride.checked_mul(dim as usize)?;
    }
    Some(flat_index)
}

pub(super) fn flatten_array_elements(elements: &[Expression]) -> Vec<Expression> {
    let mut flattened = Vec::new();
    for element in elements {
        match element {
            Expression::Array { elements, .. } => flattened.extend(elements.iter().cloned()),
            _ => flattened.push(element.clone()),
        }
    }
    flattened
}
