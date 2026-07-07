use indexmap::IndexSet;

use super::*;

pub(in crate::lower) type LinearParts = (
    IndexMap<String, rumoca_core::Expression>,
    Option<rumoca_core::Expression>,
);

fn linear_index_map_with_capacity<V>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<IndexMap<String, V>, LowerError> {
    let mut values = IndexMap::new();
    values.try_reserve(capacity).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn linear_index_set_with_capacity(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<IndexSet<String>, LowerError> {
    let mut values = IndexSet::new();
    values.try_reserve(capacity).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn inherited_linear_span(
    span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    if span.is_dummy() { owner_span } else { span }
}

fn linear_child_owner_span(
    expr: &rumoca_core::Expression,
    parent_span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    expr.span()
        .unwrap_or_else(|| inherited_linear_span(parent_span, owner_span))
}

fn linear_expr_pair_span(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    lhs.span().or_else(|| rhs.span()).unwrap_or(owner_span)
}

fn linear_expr_or_owner_span(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    if let Some(span) = expr.span().filter(|span| !span.is_dummy()) {
        return Ok(span);
    }
    if !owner_span.is_dummy() {
        return Ok(owner_span);
    }
    Err(LowerError::UnspannedContractViolation {
        reason: "derivative linear expression requires source span metadata".to_string(),
    })
}

pub(in crate::lower) fn derivative_linear_parts(
    expr: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<LinearParts>, LowerError> {
    let Some((coefficients, remainder)) = derivative_linear_parts_any(expr, ctx, owner_span)?
    else {
        return Ok(None);
    };
    Ok((!coefficients.is_empty()).then_some((coefficients, remainder)))
}

pub(in crate::lower) fn derivative_linear_parts_any(
    expr: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<LinearParts>, LowerError> {
    match expr {
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_add(op) => {
            let child_owner = inherited_linear_span(*span, owner_span);
            let Some(lhs_parts) = derivative_linear_parts_any(lhs, ctx, child_owner)? else {
                return Ok(None);
            };
            let Some(rhs_parts) = derivative_linear_parts_any(rhs, ctx, child_owner)? else {
                return Ok(None);
            };
            combine_linear_parts(lhs_parts, rhs_parts, false, *span).map(Some)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_sub(op) => {
            let child_owner = inherited_linear_span(*span, owner_span);
            let Some(lhs_parts) = derivative_linear_parts_any(lhs, ctx, child_owner)? else {
                return Ok(None);
            };
            let Some(rhs_parts) = derivative_linear_parts_any(rhs, ctx, child_owner)? else {
                return Ok(None);
            };
            combine_linear_parts(lhs_parts, rhs_parts, true, *span).map(Some)
        }
        rumoca_core::Expression::Unary { op, rhs, span } if is_unary_minus(op) => {
            let Some(parts) =
                derivative_linear_parts_any(rhs, ctx, inherited_linear_span(*span, owner_span))?
            else {
                return Ok(None);
            };
            negate_linear_parts(parts, *span).map(Some)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => derivative_linear_parts_if(
            branches,
            else_branch,
            ctx,
            inherited_linear_span(*span, owner_span),
        ),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_mul(op) => {
            if let Some(parts) = derivative_dot_product_linear_parts(lhs, rhs, ctx, *span)? {
                return Ok(Some(parts));
            }
            let lhs_has_der = lhs.contains_der();
            let rhs_has_der = rhs.contains_der();
            match (lhs_has_der, rhs_has_der) {
                (true, false) => {
                    let Some(parts) = derivative_linear_parts_any(
                        lhs,
                        ctx,
                        linear_child_owner_span(lhs, *span, owner_span),
                    )?
                    else {
                        return Ok(None);
                    };
                    scale_linear_parts(parts, rhs.as_ref().clone(), false, *span).map(Some)
                }
                (false, true) => {
                    let Some(parts) = derivative_linear_parts_any(
                        rhs,
                        ctx,
                        linear_child_owner_span(rhs, *span, owner_span),
                    )?
                    else {
                        return Ok(None);
                    };
                    scale_linear_parts(parts, lhs.as_ref().clone(), true, *span).map(Some)
                }
                (false, false) => Ok(Some((IndexMap::new(), Some(expr.clone())))),
                (true, true) => Ok(None),
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_div(op) => {
            if rhs.contains_der() {
                return Ok(None);
            }
            let Some(parts) = derivative_linear_parts_any(
                lhs,
                ctx,
                linear_child_owner_span(lhs, *span, owner_span),
            )?
            else {
                return Ok(None);
            };
            divide_linear_parts(parts, rhs.as_ref().clone(), *span).map(Some)
        }
        _ if !expr.contains_der() => Ok(Some((IndexMap::new(), Some(expr.clone())))),
        _ => derivative_terminal_linear_parts(expr, ctx.state_names, owner_span),
    }
}

fn derivative_terminal_linear_parts(
    expr: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    owner_span: rumoca_core::Span,
) -> Result<Option<LinearParts>, LowerError> {
    let Some((name, coeff)) = derivative_term_coefficient(expr, state_names, owner_span)? else {
        return Ok(None);
    };
    let span = linear_expr_or_owner_span(expr, owner_span)?;
    let mut coefficients =
        linear_index_map_with_capacity(1, "derivative term coefficient count", span)?;
    coefficients.insert(name, coeff);
    Ok(Some((coefficients, None)))
}

pub(in crate::lower) fn derivative_dot_product_linear_parts(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    span: rumoca_core::Span,
) -> Result<Option<LinearParts>, LowerError> {
    let lhs_has_der = lhs.contains_der();
    let rhs_has_der = rhs.contains_der();
    if lhs_has_der == rhs_has_der {
        return Ok(None);
    }
    let lhs_dims = match expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings, span) {
        Ok(dims) => dims,
        Err(
            LowerError::MissingBinding { .. }
            | LowerError::Unsupported { .. }
            | LowerError::UnsupportedAt { .. },
        ) => {
            return Ok(None);
        }
        Err(err) => return Err(err),
    };
    let rhs_dims = match expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings, span) {
        Ok(dims) => dims,
        Err(
            LowerError::MissingBinding { .. }
            | LowerError::Unsupported { .. }
            | LowerError::UnsupportedAt { .. },
        ) => {
            return Ok(None);
        }
        Err(err) => return Err(err),
    };
    if lhs_dims.len() != 1 || lhs_dims != rhs_dims {
        return Ok(None);
    }
    let lhs_scalars = match project_expression_scalars(
        lhs,
        &lhs_dims,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    ) {
        Ok(scalars) => scalars,
        Err(LowerError::MissingBinding { .. }) => return Ok(None),
        Err(err) => return Err(err),
    };
    let Some(lhs_scalars) = lhs_scalars else {
        return Ok(None);
    };
    let rhs_scalars = match project_expression_scalars(
        rhs,
        &rhs_dims,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    ) {
        Ok(scalars) => scalars,
        Err(LowerError::MissingBinding { .. }) => return Ok(None),
        Err(err) => return Err(err),
    };
    let Some(rhs_scalars) = rhs_scalars else {
        return Ok(None);
    };
    if lhs_scalars.len() != rhs_scalars.len() {
        return Ok(None);
    }

    let mut combined = (IndexMap::new(), None);
    for (lhs_scalar, rhs_scalar) in lhs_scalars.into_iter().zip(rhs_scalars) {
        let parts = if lhs_has_der {
            let Some(parts) = derivative_linear_parts_any(&lhs_scalar, ctx, span)? else {
                return Ok(None);
            };
            scale_linear_parts(parts, rhs_scalar, false, span)?
        } else {
            let Some(parts) = derivative_linear_parts_any(&rhs_scalar, ctx, span)? else {
                return Ok(None);
            };
            scale_linear_parts(parts, lhs_scalar, true, span)?
        };
        combined = combine_linear_parts(combined, parts, false, span)?;
    }
    Ok((!combined.0.is_empty()).then_some(combined))
}

pub(in crate::lower) fn derivative_linear_parts_if(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    span: rumoca_core::Span,
) -> Result<Option<LinearParts>, LowerError> {
    if branches
        .iter()
        .any(|(condition, _)| condition.contains_der())
    {
        return Ok(None);
    }

    let mut branch_parts =
        derivative_vec_with_capacity(branches.len(), "derivative if branch part count", span)?;
    for (_, expr) in branches {
        let Some(parts) = derivative_linear_parts_any(expr, ctx, span)? else {
            return Ok(None);
        };
        branch_parts.push(parts);
    }
    let Some(else_parts) = derivative_linear_parts_any(else_branch, ctx, span)? else {
        return Ok(None);
    };

    let coefficient_keys = derivative_if_coefficient_keys(&branch_parts, &else_parts, span)?;
    let mut coefficients = linear_index_map_with_capacity(
        coefficient_keys.len(),
        "derivative if coefficient count",
        span,
    )?;
    for key in coefficient_keys {
        let mut branch_coefficients = derivative_vec_with_capacity(
            branches.len(),
            "derivative if branch coefficient count",
            span,
        )?;
        for ((condition, _), (coefficients, _)) in branches.iter().zip(branch_parts.iter()) {
            branch_coefficients.push((
                condition.clone(),
                coefficients
                    .get(&key)
                    .cloned()
                    .unwrap_or_else(|| zero_expr_with_span(span)),
            ));
        }
        let else_coefficient = else_parts
            .0
            .get(&key)
            .cloned()
            .unwrap_or_else(|| zero_expr_with_span(span));
        coefficients.insert(
            key,
            rumoca_core::Expression::If {
                branches: branch_coefficients,
                else_branch: Box::new(else_coefficient),
                span,
            },
        );
    }

    let remainder = derivative_if_remainder(branches, &branch_parts, else_parts.1.as_ref(), span)?;
    Ok(Some((coefficients, remainder)))
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
    span: rumoca_core::Span,
) -> Result<IndexSet<String>, LowerError> {
    let mut capacity = else_parts.0.len();
    for (coefficients, _) in branch_parts {
        capacity = capacity.checked_add(coefficients.len()).ok_or_else(|| {
            LowerError::contract_violation(
                "derivative if coefficient key count overflows host index range",
                span,
            )
        })?;
    }
    let mut keys =
        linear_index_set_with_capacity(capacity, "derivative if coefficient key count", span)?;
    for (coefficients, _) in branch_parts {
        for key in coefficients.keys() {
            keys.insert(key.clone());
        }
    }
    for key in else_parts.0.keys() {
        keys.insert(key.clone());
    }
    Ok(keys)
}

pub(in crate::lower) fn derivative_if_remainder(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    branch_parts: &[(
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    )],
    else_remainder: Option<&rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if !branch_parts
        .iter()
        .any(|(_, remainder)| remainder.is_some())
        && else_remainder.is_none()
    {
        return Ok(None);
    }
    let mut branch_remainders =
        derivative_vec_with_capacity(branches.len(), "derivative if branch remainder count", span)?;
    for ((condition, _), (_, remainder)) in branches.iter().zip(branch_parts.iter()) {
        branch_remainders.push((
            condition.clone(),
            remainder
                .clone()
                .unwrap_or_else(|| zero_expr_with_span(span)),
        ));
    }
    Ok(Some(rumoca_core::Expression::If {
        branches: branch_remainders,
        else_branch: Box::new(
            else_remainder
                .cloned()
                .unwrap_or_else(|| zero_expr_with_span(span)),
        ),
        span,
    }))
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
) -> Result<LinearParts, LowerError> {
    let (mut coefficients, lhs_remainder) = lhs;
    let (rhs_coefficients, rhs_remainder) = if subtract_rhs {
        negate_linear_parts(rhs, span)?
    } else {
        rhs
    };
    coefficients
        .try_reserve(rhs_coefficients.len())
        .map_err(|_| {
            LowerError::contract_violation(
                "combined derivative coefficient count capacity exceeds host memory limits",
                span,
            )
        })?;
    for (name, coeff) in rhs_coefficients {
        coefficients
            .entry(name)
            .and_modify(|current| *current = add_with_span(current.clone(), coeff.clone(), span))
            .or_insert(coeff);
    }
    let remainder = combine_remainders(lhs_remainder, rhs_remainder, span);
    Ok((coefficients, remainder))
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
) -> Result<LinearParts, LowerError> {
    let (coefficients, remainder) = parts;
    let mut scaled_coefficients = linear_index_map_with_capacity(
        coefficients.len(),
        "scaled derivative coefficient count",
        span,
    )?;
    for (name, coeff) in coefficients {
        let scaled = if factor_on_left {
            mul_with_span(factor.clone(), coeff, span)
        } else {
            mul_with_span(coeff, factor.clone(), span)
        };
        scaled_coefficients.insert(name, scaled);
    }
    let remainder = remainder.map(|expr| {
        if factor_on_left {
            mul_with_span(factor, expr, span)
        } else {
            mul_with_span(expr, factor, span)
        }
    });
    Ok((scaled_coefficients, remainder))
}

pub(in crate::lower) fn divide_linear_parts(
    parts: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    denominator: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<LinearParts, LowerError> {
    let (coefficients, remainder) = parts;
    let mut divided_coefficients = linear_index_map_with_capacity(
        coefficients.len(),
        "divided derivative coefficient count",
        span,
    )?;
    for (name, coeff) in coefficients {
        divided_coefficients.insert(name, div_with_span(coeff, denominator.clone(), span));
    }
    let remainder = remainder.map(|expr| div_with_span(expr, denominator, span));
    Ok((divided_coefficients, remainder))
}

pub(in crate::lower) fn negate_linear_parts(
    parts: (
        IndexMap<String, rumoca_core::Expression>,
        Option<rumoca_core::Expression>,
    ),
    span: rumoca_core::Span,
) -> Result<LinearParts, LowerError> {
    let (coefficients, remainder) = parts;
    let mut negated_coefficients = linear_index_map_with_capacity(
        coefficients.len(),
        "negated derivative coefficient count",
        span,
    )?;
    for (name, coeff) in coefficients {
        negated_coefficients.insert(name, neg_with_span(coeff, span));
    }
    Ok((
        negated_coefficients,
        remainder.map(|expr| neg_with_span(expr, span)),
    ))
}

pub(in crate::lower) fn rhs_without_remainder(
    rhs: rumoca_core::Expression,
    remainder: Option<rumoca_core::Expression>,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Expression {
    match remainder {
        Some(remainder) => {
            let span = linear_expr_pair_span(&rhs, &remainder, owner_span);
            sub_with_span(rhs, remainder, span)
        }
        None => rhs,
    }
}

pub(in crate::lower) fn derivative_term_coefficient(
    term: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    owner_span: rumoca_core::Span,
) -> Result<Option<(String, rumoca_core::Expression)>, LowerError> {
    if let Some(name) = der_state_name(term, state_names)? {
        let span = linear_expr_or_owner_span(term, owner_span)?;
        return Ok(Some((name, one_expr_with_span(span))));
    }
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = term else {
        return Ok(None);
    };
    if !is_mul(op) {
        return Ok(None);
    }
    if !rhs.contains_der()
        && let Some(name) = der_state_name(lhs, state_names)?
    {
        return Ok(Some((name, rhs.as_ref().clone())));
    }
    if !lhs.contains_der()
        && let Some(name) = der_state_name(rhs, state_names)?
    {
        return Ok(Some((name, lhs.as_ref().clone())));
    }
    Ok(None)
}

pub(in crate::lower) fn der_state_name(
    expr: &rumoca_core::Expression,
    state_names: &HashSet<String>,
) -> Result<Option<String>, LowerError> {
    let rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span,
    } = expr
    else {
        return Ok(None);
    };
    if *function != rumoca_core::BuiltinFunction::Der {
        return Ok(None);
    }
    let Some(arg) = args.first() else {
        return Err(LowerError::contract_violation(
            "der() call has no argument in derivative RHS lowering",
            *span,
        ));
    };
    let key = binding_key_for_der_arg(arg)
        .map_err(|err| err.with_fallback_span(arg.span().unwrap_or(*span)))?;
    let Some(key) = key else {
        return Ok(None);
    };
    Ok(state_names.contains(&key).then_some(key))
}

pub(in crate::lower) fn binding_key_for_der_arg(
    expr: &rumoca_core::Expression,
) -> Result<Option<String>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => {
            if subscripts.is_empty() {
                return Ok(Some(name.as_str().to_string()));
            }
            let Some(indices) = der_arg_static_subscript_indices(subscripts, *span)? else {
                return Ok(None);
            };
            Ok(Some(dae::format_subscript_key(name.as_str(), &indices)))
        }
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let Some(base_key) = binding_key_for_der_arg(base)? else {
                return Ok(None);
            };
            let Some(indices) = der_arg_static_subscript_indices(subscripts, *span)? else {
                return Ok(None);
            };
            Ok(Some(dae::format_subscript_key(&base_key, &indices)))
        }
        _ => Ok(None),
    }
}

fn der_arg_static_subscript_indices(
    subscripts: &[rumoca_core::Subscript],
    fallback_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let span = der_arg_subscript_span(subscripts, fallback_span);
    let mut indices = derivative_vec_with_capacity(
        subscripts.len(),
        "derivative argument subscript index count",
        span,
    )?;
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, span } if *value > 0 => {
                indices.push(super::super::positive_i64_index(
                    *value,
                    super::super::span_or_owner(*span, fallback_span),
                )?);
            }
            rumoca_core::Subscript::Expr { expr, span } => {
                let Some(index) = super::super::lower_static_index_expr_with_owner(
                    expr,
                    super::super::span_or_owner(*span, fallback_span),
                )?
                else {
                    return Ok(None);
                };
                indices.push(index);
            }
            rumoca_core::Subscript::Colon { .. } => return Ok(None),
            _ => {
                return Err(super::super::unsupported_at(
                    "non-positive subscript is unsupported",
                    super::super::span_or_owner(subscript.span(), fallback_span),
                ));
            }
        }
    }
    Ok(Some(indices))
}

fn der_arg_subscript_span(
    subscripts: &[rumoca_core::Subscript],
    fallback_span: rumoca_core::Span,
) -> rumoca_core::Span {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
        .unwrap_or(fallback_span)
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
                span,
            } = rhs.as_ref()
            else {
                return None;
            };
            if !span.is_dummy() {
                return None;
            }
            is_sub(inner_op).then_some((rhs, lhs))
        }
        _ => None,
    }
}

pub(in crate::lower) fn collect_state_scalars(
    dae_model: &dae::Dae,
) -> Result<Vec<StateScalar>, LowerError> {
    let mut states = Vec::new();
    for (name, var) in &dae_model.variables.states {
        let base = name.as_str().to_string();
        let size = super::super::helpers::variable_size(var)?;
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
    Ok(states)
}

pub(in crate::lower) fn array_expression_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Result<Vec<usize>, LowerError> {
    if !is_matrix {
        return Ok(vec![elements.len()]);
    }
    let Some(rumoca_core::Expression::Array { elements: row, .. }) = elements.first() else {
        return Ok(Vec::new());
    };
    Ok(vec![elements.len(), row.len()])
}

pub(in crate::lower) fn one_expr_with_span(span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(1.0),
        span,
    }
}

pub(in crate::lower) fn zero_expr_with_span(span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(0.0),
        span,
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
        return zero_expr_with_span(span);
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

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("linear_parts_test"),
            start,
            end,
        )
    }

    fn unspanned_linear_parts_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    fn var_ref(
        name: &str,
        subscripts: Vec<rumoca_core::Subscript>,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts,
            span,
        }
    }

    fn der(arg: rumoca_core::Expression, span: rumoca_core::Span) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![arg],
            span,
        }
    }

    #[test]
    fn rhs_without_remainder_preserves_rhs_span() {
        let rhs_span = span(10, 20);
        let remainder_span = span(30, 40);
        let rhs = var_ref("rhs", Vec::new(), rhs_span);
        let remainder = var_ref("remainder", Vec::new(), remainder_span);

        let expr = rhs_without_remainder(rhs, Some(remainder), span(50, 60));

        assert_eq!(expr.span(), Some(rhs_span));
    }

    #[test]
    fn rhs_without_remainder_preserves_generated_rhs_owner_span() {
        let rhs_span = span(10, 20);
        let remainder_span = span(30, 40);
        let rhs = zero_expr_with_span(rhs_span);
        let remainder = var_ref("remainder", Vec::new(), remainder_span);

        let expr = rhs_without_remainder(rhs, Some(remainder), span(50, 60));

        assert_eq!(expr.span(), Some(rhs_span));
    }

    #[test]
    fn rhs_without_remainder_uses_shared_generated_owner_span() {
        let owner_span = span(50, 60);
        let expr = rhs_without_remainder(
            zero_expr_with_span(owner_span),
            Some(one_expr_with_span(owner_span)),
            owner_span,
        );

        assert_eq!(expr.span(), Some(owner_span));
    }

    #[test]
    fn linear_index_map_capacity_dummy_span_stays_unspanned() {
        let err = linear_index_map_with_capacity::<rumoca_core::Expression>(
            usize::MAX,
            "derivative coefficients",
            unspanned_linear_parts_test_span(),
        )
        .expect_err("impossible capacity must be rejected");

        assert!(
            matches!(err, LowerError::UnspannedContractViolation { .. }),
            "dummy capacity span should not be fabricated into a source span: {err:?}"
        );
        assert!(
            err.to_string()
                .contains("derivative coefficients capacity exceeds host memory limits"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn linear_index_map_capacity_real_span_is_preserved() {
        let real_span = span(2, 9);
        let err = linear_index_map_with_capacity::<rumoca_core::Expression>(
            usize::MAX,
            "derivative coefficients",
            real_span,
        )
        .expect_err("impossible capacity must be rejected");

        assert!(
            matches!(err, LowerError::ContractViolation { span: err_span, .. } if err_span == real_span),
            "real capacity span should be preserved: {err:?}"
        );
    }

    #[test]
    fn der_state_name_declines_non_state_derivative() -> Result<(), LowerError> {
        let expr = der(var_ref("p", Vec::new(), span(1, 2)), span(0, 3));

        assert!(der_state_name(&expr, &HashSet::from(["x".to_string()]))?.is_none());
        Ok(())
    }

    #[test]
    fn der_state_name_reports_missing_argument_with_call_span() {
        let call_span = span(0, 5);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: Vec::new(),
            span: call_span,
        };

        let err = der_state_name(&expr, &HashSet::from(["x".to_string()]))
            .expect_err("malformed der() call should fail");
        assert_eq!(err.source_span(), Some(call_span));
        assert!(err.reason().contains("der() call has no argument"));
    }

    #[test]
    fn der_state_name_missing_argument_dummy_span_stays_unspanned() {
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: Vec::new(),
            span: unspanned_linear_parts_test_span(),
        };

        let err = der_state_name(&expr, &HashSet::from(["x".to_string()]))
            .expect_err("malformed synthetic der() call should fail");
        assert!(
            matches!(err, LowerError::UnspannedContractViolation { .. }),
            "dummy der() call span should not be fabricated into a source span: {err:?}"
        );
        assert!(err.reason().contains("der() call has no argument"));
    }

    #[test]
    fn der_state_name_bubbles_invalid_static_subscript_span() {
        let subscript_span = span(3, 4);
        let expr = der(
            var_ref(
                "x",
                vec![rumoca_core::Subscript::Index {
                    value: 0,
                    span: subscript_span,
                }],
                span(1, 4),
            ),
            span(0, 5),
        );

        let err = der_state_name(&expr, &HashSet::from(["x[1]".to_string()]))
            .expect_err("invalid der() subscript should fail");
        assert_eq!(err.source_span(), Some(subscript_span));
        assert!(err.reason().contains("non-positive subscript"));
    }

    #[test]
    fn der_state_name_declines_slice_subscript() -> Result<(), LowerError> {
        let expr = der(
            var_ref(
                "x",
                vec![
                    rumoca_core::Subscript::Index {
                        value: 1,
                        span: span(3, 4),
                    },
                    rumoca_core::Subscript::Colon { span: span(5, 6) },
                ],
                span(1, 6),
            ),
            span(0, 7),
        );

        assert!(der_state_name(&expr, &HashSet::from(["x[1,1]".to_string()]))?.is_none());
        Ok(())
    }

    #[test]
    fn der_state_name_declines_dynamic_subscript() -> Result<(), LowerError> {
        let subscript_span = span(3, 4);
        let expr = der(
            var_ref(
                "x",
                vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(var_ref("i", Vec::new(), span(3, 4))),
                    span: subscript_span,
                }],
                span(1, 4),
            ),
            span(0, 5),
        );

        assert!(der_state_name(&expr, &HashSet::from(["x[1]".to_string()]))?.is_none());
        Ok(())
    }
}
