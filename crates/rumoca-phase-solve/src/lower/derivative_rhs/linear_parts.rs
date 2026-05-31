use super::*;

pub(in crate::lower) fn derivative_linear_parts(
    expr: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
) -> Option<(
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
)> {
    let (coefficients, remainder) = derivative_linear_parts_any(expr, ctx)?;
    (!coefficients.is_empty()).then_some((coefficients, remainder))
}

pub(in crate::lower) fn derivative_linear_parts_any(
    expr: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
) -> Option<(
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
)> {
    match expr {
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_add(op) => {
            let lhs_parts = derivative_linear_parts_any(lhs, ctx)?;
            let rhs_parts = derivative_linear_parts_any(rhs, ctx)?;
            Some(combine_linear_parts(lhs_parts, rhs_parts, false, *span))
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_sub(op) => {
            let lhs_parts = derivative_linear_parts_any(lhs, ctx)?;
            let rhs_parts = derivative_linear_parts_any(rhs, ctx)?;
            Some(combine_linear_parts(lhs_parts, rhs_parts, true, *span))
        }
        rumoca_core::Expression::Unary { op, rhs, span } if is_unary_minus(op) => {
            let parts = derivative_linear_parts_any(rhs, ctx)?;
            Some(negate_linear_parts(parts, *span))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => derivative_linear_parts_if(branches, else_branch, ctx, *span),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_mul(op) => {
            if let Some(parts) = derivative_dot_product_linear_parts(lhs, rhs, ctx, *span) {
                return Some(parts);
            }
            let lhs_has_der = lhs.contains_der();
            let rhs_has_der = rhs.contains_der();
            match (lhs_has_der, rhs_has_der) {
                (true, false) => {
                    let parts = derivative_linear_parts_any(lhs, ctx)?;
                    Some(scale_linear_parts(
                        parts,
                        rhs.as_ref().clone(),
                        false,
                        *span,
                    ))
                }
                (false, true) => {
                    let parts = derivative_linear_parts_any(rhs, ctx)?;
                    Some(scale_linear_parts(parts, lhs.as_ref().clone(), true, *span))
                }
                (false, false) => Some((IndexMap::new(), Some(expr.clone()))),
                (true, true) => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_div(op) => {
            if rhs.contains_der() {
                return None;
            }
            let parts = derivative_linear_parts_any(lhs, ctx)?;
            Some(divide_linear_parts(parts, rhs.as_ref().clone(), *span))
        }
        _ if !expr.contains_der() => Some((IndexMap::new(), Some(expr.clone()))),
        _ => derivative_term_coefficient(expr, ctx.state_names)
            .map(|(name, coeff)| (IndexMap::from([(name, coeff)]), None)),
    }
}

pub(in crate::lower) fn derivative_dot_product_linear_parts(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    span: rumoca_core::Span,
) -> Option<(
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
)> {
    let lhs_has_der = lhs.contains_der();
    let rhs_has_der = rhs.contains_der();
    if lhs_has_der == rhs_has_der {
        return None;
    }
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings).ok()?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings).ok()?;
    if lhs_dims.len() != 1 || lhs_dims != rhs_dims {
        return None;
    }
    let lhs_scalars =
        project_expression_scalars(lhs, &lhs_dims, ctx.dae_model, ctx.structural_bindings)
            .ok()??;
    let rhs_scalars =
        project_expression_scalars(rhs, &rhs_dims, ctx.dae_model, ctx.structural_bindings)
            .ok()??;
    if lhs_scalars.len() != rhs_scalars.len() {
        return None;
    }

    let mut combined = (IndexMap::new(), None);
    for (lhs_scalar, rhs_scalar) in lhs_scalars.into_iter().zip(rhs_scalars) {
        let parts = if lhs_has_der {
            let parts = derivative_linear_parts_any(&lhs_scalar, ctx)?;
            scale_linear_parts(parts, rhs_scalar, false, span)
        } else {
            let parts = derivative_linear_parts_any(&rhs_scalar, ctx)?;
            scale_linear_parts(parts, lhs_scalar, true, span)
        };
        combined = combine_linear_parts(combined, parts, false, span);
    }
    (!combined.0.is_empty()).then_some(combined)
}

pub(in crate::lower) fn derivative_linear_parts_if(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    span: rumoca_core::Span,
) -> Option<(
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
)> {
    if branches
        .iter()
        .any(|(condition, _)| condition.contains_der())
    {
        return None;
    }

    let branch_parts = branches
        .iter()
        .map(|(_, expr)| derivative_linear_parts_any(expr, ctx))
        .collect::<Option<Vec<_>>>()?;
    let else_parts = derivative_linear_parts_any(else_branch, ctx)?;

    let coefficient_keys = derivative_if_coefficient_keys(&branch_parts, &else_parts);
    let coefficients = coefficient_keys
        .into_iter()
        .map(|key| {
            let branch_coefficients = branches
                .iter()
                .zip(branch_parts.iter())
                .map(|((condition, _), (coefficients, _))| {
                    (
                        condition.clone(),
                        coefficients.get(&key).cloned().unwrap_or_else(zero_expr),
                    )
                })
                .collect();
            let else_coefficient = else_parts.0.get(&key).cloned().unwrap_or_else(zero_expr);
            (
                key,
                rumoca_core::Expression::If {
                    branches: branch_coefficients,
                    else_branch: Box::new(else_coefficient),
                    span,
                },
            )
        })
        .collect();

    let remainder = derivative_if_remainder(branches, &branch_parts, else_parts.1.as_ref(), span);
    Some((coefficients, remainder))
}

pub(in crate::lower) fn derivative_if_coefficient_keys(
    branch_parts: &[(
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    )],
    else_parts: &(
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
) -> Vec<String> {
    let mut keys = Vec::new();
    for (coefficients, _) in branch_parts {
        for key in coefficients.keys() {
            if !keys.contains(key) {
                keys.push(key.clone());
            }
        }
    }
    for key in else_parts.0.keys() {
        if !keys.contains(key) {
            keys.push(key.clone());
        }
    }
    keys
}

pub(in crate::lower) fn derivative_if_remainder(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    branch_parts: &[(
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    )],
    else_remainder: Option<&rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    if !branch_parts
        .iter()
        .any(|(_, remainder)| remainder.is_some())
        && else_remainder.is_none()
    {
        return None;
    }
    let branch_remainders = branches
        .iter()
        .zip(branch_parts.iter())
        .map(|((condition, _), (_, remainder))| {
            (
                condition.clone(),
                remainder.clone().unwrap_or_else(zero_expr),
            )
        })
        .collect();
    Some(rumoca_core::Expression::If {
        branches: branch_remainders,
        else_branch: Box::new(else_remainder.cloned().unwrap_or_else(zero_expr)),
        span,
    })
}

pub(in crate::lower) fn combine_linear_parts(
    lhs: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    rhs: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    subtract_rhs: bool,
    span: rumoca_core::Span,
) -> (
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
) {
    let (mut coefficients, lhs_remainder) = lhs;
    let (rhs_coefficients, rhs_remainder) = if subtract_rhs {
        negate_linear_parts(rhs, span)
    } else {
        rhs
    };
    for (name, coeff) in rhs_coefficients {
        coefficients
            .entry(name)
            .and_modify(|current| *current = add_with_span(current.clone(), coeff.clone(), span))
            .or_insert(coeff);
    }
    let remainder = combine_remainders(lhs_remainder, rhs_remainder, span);
    (coefficients, remainder)
}

pub(in crate::lower) fn combine_remainders(
    lhs: Option<rumoca_core::Expression>,
    rhs: Option<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => Some(add_with_span(lhs, rhs, span)),
        (Some(expr), None) | (None, Some(expr)) => Some(expr),
        (None, None) => None,
    }
}

pub(in crate::lower) fn scale_linear_parts(
    parts: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    factor: rumoca_core::Expression,
    factor_on_left: bool,
    span: rumoca_core::Span,
) -> (
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
) {
    let (coefficients, remainder) = parts;
    let coefficients = coefficients
        .into_iter()
        .map(|(name, coeff)| {
            let scaled = if factor_on_left {
                mul_with_span(factor.clone(), coeff, span)
            } else {
                mul_with_span(coeff, factor.clone(), span)
            };
            (name, scaled)
        })
        .collect();
    let remainder = remainder.map(|expr| {
        if factor_on_left {
            mul_with_span(factor, expr, span)
        } else {
            mul_with_span(expr, factor, span)
        }
    });
    (coefficients, remainder)
}

pub(in crate::lower) fn divide_linear_parts(
    parts: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    denominator: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> (
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
) {
    let (coefficients, remainder) = parts;
    let coefficients = coefficients
        .into_iter()
        .map(|(name, coeff)| (name, div_with_span(coeff, denominator.clone(), span)))
        .collect();
    let remainder = remainder.map(|expr| div_with_span(expr, denominator, span));
    (coefficients, remainder)
}

pub(in crate::lower) fn negate_linear_parts(
    parts: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    span: rumoca_core::Span,
) -> (
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
) {
    let (coefficients, remainder) = parts;
    let coefficients = coefficients
        .into_iter()
        .map(|(name, coeff)| (name, neg_with_span(coeff, span)))
        .collect();
    (
        coefficients,
        remainder.map(|expr| neg_with_span(expr, span)),
    )
}

pub(in crate::lower) fn rhs_without_remainder(
    rhs: rumoca_core::Expression,
    remainder: Option<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    match remainder {
        Some(remainder) => sub_with_span(rhs, remainder, rumoca_core::Span::DUMMY),
        None => rhs,
    }
}

pub(in crate::lower) fn derivative_term_coefficient(
    term: &rumoca_core::Expression,
    state_names: &[String],
) -> Option<(String, rumoca_core::Expression)> {
    if let Some(name) = der_state_name(term, state_names) {
        return Some((name, one_expr()));
    }
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = term else {
        return None;
    };
    if !is_mul(op) {
        return None;
    }
    if let Some(name) = der_state_name(lhs, state_names)
        && !rhs.contains_der()
    {
        return Some((name, rhs.as_ref().clone()));
    }
    if let Some(name) = der_state_name(rhs, state_names)
        && !lhs.contains_der()
    {
        return Some((name, lhs.as_ref().clone()));
    }
    None
}

pub(in crate::lower) fn der_state_name(
    expr: &rumoca_core::Expression,
    state_names: &[String],
) -> Option<String> {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    if *function != rumoca_core::BuiltinFunction::Der {
        return None;
    }
    let key = binding_key_for_der_arg(args.first()?).ok()?;
    state_names.iter().any(|name| name == &key).then_some(key)
}

pub(in crate::lower) fn binding_key_for_der_arg(
    expr: &rumoca_core::Expression,
) -> Result<String, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Ok(name.as_str().to_string());
            }
            let indices = super::super::static_subscript_indices(subscripts)?.ok_or_else(|| {
                LowerError::Unsupported {
                    reason: "dynamic der() subscript is unsupported in derivative RHS lowering"
                        .to_string(),
                }
            })?;
            Ok(dae::format_subscript_key(name.as_str(), &indices))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base_key = binding_key_for_der_arg(base)?;
            let indices = super::super::static_subscript_indices(subscripts)?.ok_or_else(|| {
                LowerError::Unsupported {
                    reason: "dynamic der() subscript is unsupported in derivative RHS lowering"
                        .to_string(),
                }
            })?;
            Ok(dae::format_subscript_key(&base_key, &indices))
        }
        _ => Err(LowerError::Unsupported {
            reason: "unsupported der() argument in derivative RHS lowering".to_string(),
        }),
    }
}

pub(in crate::lower) fn split_subtraction(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Expression, &rumoca_core::Expression)> {
    match expr {
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_sub(op) => Some((lhs, rhs)),
        rumoca_core::Expression::Unary { op, rhs, .. } if is_unary_minus(op) => {
            let rumoca_core::Expression::Binary {
                op: inner_op,
                lhs,
                rhs,
                span: rumoca_core::Span::DUMMY,
            } = rhs.as_ref()
            else {
                return None;
            };
            is_sub(inner_op).then_some((rhs, lhs))
        }
        _ => None,
    }
}

pub(in crate::lower) fn collect_state_scalars(dae_model: &dae::Dae) -> Vec<StateScalar> {
    let mut states = Vec::new();
    for (name, var) in &dae_model.variables.states {
        let base = name.as_str().to_string();
        let size = var.size();
        for idx in 0..size {
            let scalar_name = if var.dims.is_empty() {
                base.clone()
            } else {
                dae::scalar_name_text_for_flat_index(&base, &var.dims, idx)
            };
            states.push(StateScalar {
                name: scalar_name,
                base: base.clone(),
                component: idx,
                base_size: size,
            });
        }
    }
    states
}

pub(in crate::lower) fn array_expression_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Vec<i64> {
    if !is_matrix {
        return vec![elements.len() as i64];
    }
    let Some(rumoca_core::Expression::Array { elements: row, .. }) = elements.first() else {
        return Vec::new();
    };
    vec![elements.len() as i64, row.len() as i64]
}

pub(in crate::lower) fn one_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(1.0),
        span: rumoca_core::Span::DUMMY,
    }
}

pub(in crate::lower) fn zero_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    }
}

pub(in crate::lower) fn add_with_span(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

pub(in crate::lower) fn mul_with_span(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

pub(in crate::lower) fn div_with_span(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

pub(in crate::lower) fn neg_with_span(
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(rhs),
        span,
    }
}

pub(in crate::lower) fn sub_with_span(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

pub(in crate::lower) fn sum_expressions(
    mut terms: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if terms.is_empty() {
        return zero_expr();
    }
    let first = terms.remove(0);
    terms
        .into_iter()
        .fold(first, |lhs, rhs| add_with_span(lhs, rhs, span))
}

pub(in crate::lower) fn is_add(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Add | OpBinary::AddElem)
}

pub(in crate::lower) fn is_sub(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Sub | OpBinary::SubElem)
}

pub(in crate::lower) fn is_mul(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Mul | OpBinary::MulElem)
}

pub(in crate::lower) fn is_div(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Div | OpBinary::DivElem)
}

pub(in crate::lower) fn is_unary_minus(op: &OpUnary) -> bool {
    matches!(op, OpUnary::Minus | OpUnary::DotMinus)
}
