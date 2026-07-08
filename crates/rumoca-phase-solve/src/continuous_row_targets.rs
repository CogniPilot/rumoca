use super::*;

pub(super) fn lower_continuous_row_targets<'a>(
    dae_model: &dae::Dae,
    equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
    layout: &solve::VarLayout,
) -> Result<Vec<Option<solve::ScalarSlot>>, LowerError> {
    let mut equation_iter = equations.into_iter();
    let Some(first_equation) = equation_iter.next() else {
        return Ok(Vec::new());
    };
    let span = first_equation.1.span;
    let (lower_bound, upper_bound) = equation_iter.size_hint();
    let capacity = upper_bound
        .unwrap_or(lower_bound)
        .checked_add(1)
        .ok_or_else(|| {
            lower_contract_violation(
                "continuous row target equation count exceeds host index range".to_string(),
                span,
            )
        })?;
    let mut equations =
        lower_vec_with_capacity(capacity, "continuous row target equation count", span)?;
    equations.push(first_equation);
    for equation in equation_iter {
        equations.push(equation);
    }
    let mut targets =
        lower_vec_with_capacity(equations.len(), "continuous row target count", span)?;
    for (_, eq) in equations {
        let row_count = lower::residual_equation_effective_row_count(dae_model, eq)?.max(1);
        let equation_targets =
            lower_continuous_row_targets_for_equation(dae_model, eq, layout, row_count)?;
        reserve_lower_capacity(
            &mut targets,
            equation_targets.len(),
            "continuous row target count",
            eq.span,
        )?;
        targets.extend(equation_targets);
    }
    dedupe_continuous_y_targets(&mut targets);
    Ok(targets)
}

pub(super) fn dedupe_continuous_y_targets(targets: &mut [Option<solve::ScalarSlot>]) {
    let mut claimed_y_targets = BTreeSet::new();
    for target in targets {
        let Some(solve::ScalarSlot::Y { index, .. }) = target else {
            continue;
        };
        if !claimed_y_targets.insert(*index) {
            *target = None;
        }
    }
}

pub(super) fn lower_continuous_row_targets_for_equation(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    layout: &solve::VarLayout,
    row_count: usize,
) -> Result<Vec<Option<solve::ScalarSlot>>, LowerError> {
    let mut targets = lower_vec_with_capacity(row_count, "continuous row target count", eq.span)?;
    if row_count == 0 {
        return Ok(targets);
    }
    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(names) = scalarized_record_target_names(lhs.as_str(), layout)
    {
        push_bound_target_slots(layout, names, &mut targets, eq.span)?;
        return Ok(targets);
    }
    if let Some(names) = scalarized_record_residual_target_names(&eq.rhs, layout) {
        push_bound_target_slots(layout, names, &mut targets, eq.span)?;
        return Ok(targets);
    }
    for flat_index in 0..row_count {
        let name = continuous_row_target_name(dae_model, eq, layout, flat_index, row_count)?;
        let Some(name) = name else {
            targets.push(None);
            continue;
        };
        let Some(slot) = layout.binding(name.as_str()) else {
            return Err(LowerError::MissingBinding { name });
        };
        targets.push(Some(slot));
    }
    Ok(targets)
}

fn push_bound_target_slots(
    layout: &solve::VarLayout,
    names: Vec<String>,
    targets: &mut Vec<Option<solve::ScalarSlot>>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    reserve_lower_capacity(targets, names.len(), "bound row target count", span)?;
    for name in names {
        let Some(slot) = layout.binding(name.as_str()) else {
            return Err(LowerError::MissingBinding { name });
        };
        targets.push(Some(slot));
    }
    Ok(())
}

pub(super) fn scalarized_record_target_names(
    base: &str,
    layout: &solve::VarLayout,
) -> Option<Vec<String>> {
    lower::scalarized_record_field_binding_names(base, layout)
}

fn scalarized_record_residual_target_names(
    expr: &rumoca_core::Expression,
    layout: &solve::VarLayout,
) -> Option<Vec<String>> {
    let rumoca_core::Expression::Binary {
        op,
        lhs,
        rhs: _,
        span: _,
    } = expr
    else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    scalarized_record_target_names(name.as_str(), layout)
}

fn continuous_row_target_name(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    layout: &solve::VarLayout,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    if let Some(lhs) = eq.lhs.as_ref() {
        if let Some(name) =
            residual_expression_target_name(dae_model, layout, &eq.rhs, flat_index, scalar_count)?
        {
            return Ok(Some(name));
        }
        return continuous_equation_scalar_name(
            dae_model,
            lhs.var_name(),
            flat_index,
            scalar_count,
            eq.span,
        );
    }
    residual_expression_target_name(dae_model, layout, &eq.rhs, flat_index, scalar_count)
}

pub(super) fn continuous_equation_scalar_name(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<String>, LowerError> {
    if scalar_count <= 1 {
        return Ok(Some(lhs.as_str().to_string()));
    }
    let Some(dims) = continuous_equation_dims(dae_model, lhs) else {
        if continuous_equation_known_non_target_dims(dae_model, lhs).is_some() {
            return Ok(None);
        }
        return Err(lower_contract_violation(
            format!(
                "continuous equation array LHS `{}` must be a known DAE variable",
                lhs.as_str()
            ),
            span,
        ));
    };
    Ok(Some(dae::scalar_name_text_for_flat_index(
        lhs.as_str(),
        dims,
        flat_index,
    )))
}

fn continuous_equation_known_non_target_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .inputs
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
        .or_else(|| dae_model.variables.parameters.get(lhs))
        .map(|var| var.dims.as_slice())
}

fn residual_expression_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    // MLS §8.3: an equation `a = b` is represented in DAE as the residual
    // `a - b = 0`. Recover the solve-row target from that expression tree,
    // not from rendered source text, so projectors can use direct row targets.
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            if let Some(name) =
                target_expr_y_scalar_name(dae_model, layout, lhs, flat_index, scalar_count)?
            {
                return Ok(Some(name));
            }
            if expression_is_solver_y_free(dae_model, layout, lhs, flat_index, scalar_count)? {
                return target_expr_y_scalar_name(dae_model, layout, rhs, flat_index, scalar_count);
            }
            Ok(None)
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => {
            if let Some(name) =
                positive_additive_target_name(dae_model, layout, lhs, flat_index, scalar_count)?
            {
                return Ok(Some(name));
            }
            positive_additive_target_name(dae_model, layout, rhs, flat_index, scalar_count)
        }
        _ => Ok(None),
    }
}

fn positive_additive_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => {
            if let Some(name) =
                positive_additive_target_name(dae_model, layout, lhs, flat_index, scalar_count)?
            {
                return Ok(Some(name));
            }
            positive_additive_target_name(dae_model, layout, rhs, flat_index, scalar_count)
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => multiplicative_target_name(dae_model, layout, lhs, rhs, flat_index, scalar_count),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs,
            rhs,
            ..
        } => divisive_target_name(dae_model, layout, lhs, rhs, flat_index, scalar_count),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            ..
        } => Ok(None),
        rumoca_core::Expression::Unary {
            op:
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus | rumoca_core::OpUnary::Empty,
            rhs,
            ..
        } => positive_additive_target_name(dae_model, layout, rhs, flat_index, scalar_count),
        _ => target_expr_scalar_name(dae_model, expr, flat_index, scalar_count),
    }
}

fn multiplicative_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    let lhs_target = target_expr_y_scalar_name(dae_model, layout, lhs, flat_index, scalar_count)?;
    let rhs_target = target_expr_y_scalar_name(dae_model, layout, rhs, flat_index, scalar_count)?;
    if let (Some(name), None) = (lhs_target.as_ref(), rhs_target.as_ref())
        && expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count)?
    {
        return Ok(Some(name.clone()));
    }
    if let (None, Some(name)) = (lhs_target.as_ref(), rhs_target.as_ref())
        && expression_is_solver_y_free(dae_model, layout, lhs, flat_index, scalar_count)?
    {
        return Ok(Some(name.clone()));
    }
    Ok(None)
}

fn divisive_target_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    let lhs_target = target_expr_y_scalar_name(dae_model, layout, lhs, flat_index, scalar_count)?;
    if lhs_target.is_some()
        && !expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count)?
    {
        return Ok(None);
    }
    Ok(lhs_target)
}

fn target_expr_y_scalar_name(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    let Some(name) = target_expr_scalar_name(dae_model, expr, flat_index, scalar_count)? else {
        return Ok(None);
    };
    Ok(matches!(
        layout.binding(name.as_str()),
        Some(solve::ScalarSlot::Y { .. })
    )
    .then_some(name))
}

// SPEC_0021: Exception - solver-y freedom checks recurse over expression
// variants and preserve MLS operator semantics in one audit point.
#[allow(clippy::too_many_lines)]
fn expression_is_solver_y_free(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<bool, LowerError> {
    if let Some(name) = target_expr_scalar_name(dae_model, expr, flat_index, scalar_count)? {
        return Ok(!matches!(
            layout.binding(name.as_str()),
            Some(solve::ScalarSlot::Y { .. }) | None
        ));
    }
    match expr {
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            Ok(
                expression_is_solver_y_free(dae_model, layout, lhs, flat_index, scalar_count)?
                    && expression_is_solver_y_free(
                        dae_model,
                        layout,
                        rhs,
                        flat_index,
                        scalar_count,
                    )?,
            )
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            expression_is_solver_y_free(dae_model, layout, rhs, flat_index, scalar_count)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            all_expressions_solver_y_free(dae_model, layout, args, flat_index, scalar_count)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if !expression_is_solver_y_free(
                    dae_model,
                    layout,
                    condition,
                    flat_index,
                    scalar_count,
                )? || !expression_is_solver_y_free(
                    dae_model,
                    layout,
                    value,
                    flat_index,
                    scalar_count,
                )? {
                    return Ok(false);
                }
            }
            expression_is_solver_y_free(dae_model, layout, else_branch, flat_index, scalar_count)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            all_expressions_solver_y_free(dae_model, layout, elements, flat_index, scalar_count)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            if !expression_is_solver_y_free(dae_model, layout, start, flat_index, scalar_count)? {
                return Ok(false);
            }
            if let Some(step) = step
                && !expression_is_solver_y_free(dae_model, layout, step, flat_index, scalar_count)?
            {
                return Ok(false);
            }
            expression_is_solver_y_free(dae_model, layout, end, flat_index, scalar_count)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            if !expression_is_solver_y_free(dae_model, layout, expr, flat_index, scalar_count)? {
                return Ok(false);
            }
            for index in indices {
                if !expression_is_solver_y_free(
                    dae_model,
                    layout,
                    &index.range,
                    flat_index,
                    scalar_count,
                )? {
                    return Ok(false);
                }
            }
            if let Some(filter) = filter {
                return expression_is_solver_y_free(
                    dae_model,
                    layout,
                    filter,
                    flat_index,
                    scalar_count,
                );
            }
            Ok(true)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            if !expression_is_solver_y_free(dae_model, layout, base, flat_index, scalar_count)? {
                return Ok(false);
            }
            subscript_expressions_solver_y_free(
                dae_model,
                layout,
                subscripts,
                flat_index,
                scalar_count,
            )
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            expression_is_solver_y_free(dae_model, layout, base, flat_index, scalar_count)
        }
        rumoca_core::Expression::VarRef { subscripts, .. } => subscript_expressions_solver_y_free(
            dae_model,
            layout,
            subscripts,
            flat_index,
            scalar_count,
        ),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => Ok(true),
    }
}

fn all_expressions_solver_y_free(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    exprs: &[rumoca_core::Expression],
    flat_index: usize,
    scalar_count: usize,
) -> Result<bool, LowerError> {
    for expr in exprs {
        if !expression_is_solver_y_free(dae_model, layout, expr, flat_index, scalar_count)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn subscript_expressions_solver_y_free(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    subscripts: &[rumoca_core::Subscript],
    flat_index: usize,
    scalar_count: usize,
) -> Result<bool, LowerError> {
    for subscript in subscripts {
        let Some(expr) = subscript_expr(subscript) else {
            continue;
        };
        if !expression_is_solver_y_free(dae_model, layout, expr, flat_index, scalar_count)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn subscript_expr(subscript: &rumoca_core::Subscript) -> Option<&rumoca_core::Expression> {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => Some(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => None,
    }
}

pub(super) fn target_expr_scalar_name(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    flat_index: usize,
    scalar_count: usize,
) -> Result<Option<String>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => var_ref_target_name(
            dae_model,
            name,
            subscripts,
            flat_index,
            scalar_count,
            expr.span(),
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return Ok(None);
            };
            if !base_subscripts.is_empty() {
                return Ok(None);
            }
            var_ref_target_name(
                dae_model,
                name,
                subscripts,
                flat_index,
                scalar_count,
                expr.span().or_else(|| base.span()),
            )
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            ..
        } => {
            let [arg] = args.as_slice() else {
                return Ok(None);
            };
            target_expr_scalar_name(dae_model, arg, flat_index, scalar_count)
        }
        _ => Ok(None),
    }
}

fn var_ref_target_name(
    dae_model: &dae::Dae,
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    flat_index: usize,
    scalar_count: usize,
    owner_span: Option<rumoca_core::Span>,
) -> Result<Option<String>, LowerError> {
    if !subscripts.is_empty() {
        if continuous_equation_dims(dae_model, name.var_name()).is_some_and(|dims| dims.is_empty())
            && singleton_scalar_target_projection(subscripts)
        {
            return Ok(Some(name.as_str().to_string()));
        }
        if let Some(indices) = sliced_target_indices(
            dae_model,
            name.var_name(),
            subscripts,
            flat_index,
            owner_span,
        )? {
            return Ok(Some(dae::format_subscript_key(name.as_str(), &indices)));
        }
        if let Some(indices) = fixed_positive_indices(dae_model, subscripts, owner_span)? {
            return Ok(Some(dae::format_subscript_key(name.as_str(), &indices)));
        }
        let Some(indices) = checked_literal_positive_indices(subscripts, owner_span)? else {
            return Ok(None);
        };
        return Ok(Some(dae::format_subscript_key(name.as_str(), &indices)));
    }
    Ok(continuous_equation_scalar_name_if_known(
        dae_model,
        name.var_name(),
        flat_index,
        scalar_count,
    ))
}

fn continuous_equation_scalar_name_if_known(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
) -> Option<String> {
    if scalar_count <= 1 {
        return Some(lhs.as_str().to_string());
    }
    let dims = continuous_equation_dims(dae_model, lhs)?;
    Some(dae::scalar_name_text_for_flat_index(
        lhs.as_str(),
        dims,
        flat_index,
    ))
}

fn singleton_scalar_target_projection(subscripts: &[rumoca_core::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts.iter().all(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => *value == 1,
            rumoca_core::Subscript::Expr { expr, .. } => matches!(
                expr.as_ref(),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    ..
                }
            ),
            rumoca_core::Subscript::Colon { .. } => false,
        })
}

fn fixed_positive_indices(
    dae_model: &dae::Dae,
    subscripts: &[rumoca_core::Subscript],
    owner_span: Option<rumoca_core::Span>,
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let span = subscript_source_span(subscripts, owner_span, "fixed positive subscript")?;
    let mut indices = lower_vec_with_capacity(
        subscripts.len(),
        "fixed positive subscript index count",
        span,
    )?;
    for subscript in subscripts {
        let Some(value) = fixed_subscript_index(dae_model, subscript)? else {
            return Ok(None);
        };
        if value <= 0 {
            return Ok(None);
        }
        let index = usize::try_from(value).map_err(|_| {
            lower_contract_violation(
                format!("fixed subscript index {value} exceeds host index range"),
                span,
            )
        })?;
        indices.push(index);
    }
    Ok(Some(indices))
}

fn fixed_subscript_index(
    dae_model: &dae::Dae,
    subscript: &rumoca_core::Subscript,
) -> Result<Option<i64>, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Ok(Some(*value)),
        rumoca_core::Subscript::Expr { expr, .. } => fixed_index_expr(dae_model, expr),
        rumoca_core::Subscript::Colon { .. } => Ok(None),
    }
}

fn fixed_index_expr(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> Result<Option<i64>, LowerError> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Ok(Some(*value)),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if value.is_finite() && value.fract() == 0.0 => Ok(Some(*value as i64)),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => fixed_integer_parameter_start(dae_model, name.var_name()),
        rumoca_core::Expression::Unary { op, rhs, .. } => match op {
            rumoca_core::OpUnary::Plus => fixed_index_expr(dae_model, rhs),
            rumoca_core::OpUnary::Minus => {
                Ok(fixed_index_expr(dae_model, rhs)?.and_then(|value| value.checked_neg()))
            }
            _ => Ok(None),
        },
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let Some(lhs) = fixed_index_expr(dae_model, lhs)? else {
                return Ok(None);
            };
            let Some(rhs) = fixed_index_expr(dae_model, rhs)? else {
                return Ok(None);
            };
            Ok(match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => lhs.checked_add(rhs),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => lhs.checked_sub(rhs),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => lhs.checked_mul(rhs),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem
                    if rhs != 0 && lhs % rhs == 0 =>
                {
                    Some(lhs / rhs)
                }
                _ => None,
            })
        }
        _ => Ok(None),
    }
}

fn fixed_integer_parameter_start(
    dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
) -> Result<Option<i64>, LowerError> {
    let Some(var) = dae_model
        .variables
        .parameters
        .get(name)
        .filter(|var| !var.is_tunable)
        .or_else(|| dae_model.variables.constants.get(name))
    else {
        return Ok(None);
    };
    let Some(start) = var.start.as_ref() else {
        return Ok(None);
    };
    fixed_index_expr(dae_model, start)
}

fn sliced_target_indices(
    dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    flat_index: usize,
    owner_span: Option<rumoca_core::Span>,
) -> Result<Option<Vec<usize>>, LowerError> {
    let span = subscript_source_span(subscripts, owner_span, "sliced target subscript")?;
    let Some(dims) = continuous_equation_dims(dae_model, name) else {
        return Ok(None);
    };
    if subscripts.len() > dims.len() {
        return Ok(None);
    }
    let mut parts =
        lower_vec_with_capacity(dims.len(), "sliced target dimension part count", span)?;
    let mut selected_dims =
        lower_vec_with_capacity(dims.len(), "sliced target selected dimension count", span)?;
    for (dim_idx, dim) in dims.iter().copied().enumerate() {
        let dim = usize::try_from(dim).map_err(|_| {
            lower_contract_violation(
                format!("sliced target dimension {dim} is negative or exceeds host index range"),
                span,
            )
        })?;
        if dim == 0 {
            return Ok(None);
        }
        let indices = match subscripts.get(dim_idx) {
            Some(rumoca_core::Subscript::Index { value, .. }) if *value > 0 => {
                single_sliced_target_index(*value, span)?
            }
            Some(rumoca_core::Subscript::Expr { expr, span }) => {
                let Some(index) = checked_literal_positive_index_expr(expr, *span)? else {
                    return Ok(None);
                };
                single_sliced_target_usize_index(index, *span)?
            }
            Some(rumoca_core::Subscript::Colon { .. }) | None => {
                full_sliced_target_indices(dim, span)?
            }
            _ => return Ok(None),
        };
        if indices.iter().any(|index| *index == 0 || *index > dim) {
            return Ok(None);
        }
        if indices.len() > 1 {
            let len = i64::try_from(indices.len()).map_err(|_| {
                lower_contract_violation(
                    "sliced target selected dimension exceeds i64 range".to_string(),
                    span,
                )
            })?;
            selected_dims.push(len);
        }
        parts.push(indices);
    }
    let selected_subscripts = if selected_dims.is_empty() {
        if flat_index == 0 {
            Vec::new()
        } else {
            return Ok(None);
        }
    } else {
        let Some(subscripts) = dae::flat_index_to_subscripts(&selected_dims, flat_index) else {
            return Ok(None);
        };
        subscripts
    };
    let mut selected_iter = selected_subscripts.into_iter();
    let mut indices =
        lower_vec_with_capacity(parts.len(), "sliced target projected index count", span)?;
    for part in parts {
        if part.len() == 1 {
            indices.push(part[0]);
        } else {
            let Some(selected) = selected_iter.next() else {
                return Err(lower_contract_violation(
                    "sliced target selection rank is shorter than selected dimensions".to_string(),
                    span,
                ));
            };
            if selected == 0 || selected > part.len() {
                return Err(lower_contract_violation(
                    format!(
                        "sliced target selected index {selected} is out of bounds for dimension part of length {}",
                        part.len()
                    ),
                    span,
                ));
            }
            indices.push(part[selected - 1]);
        }
    }
    Ok(Some(indices))
}

fn single_sliced_target_index(
    value: i64,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let index = usize::try_from(value).map_err(|_| {
        lower_contract_violation(
            format!("sliced target subscript index {value} exceeds host index range"),
            span,
        )
    })?;
    single_sliced_target_usize_index(index, span)
}

fn single_sliced_target_usize_index(
    index: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut indices = lower_vec_with_capacity(1, "sliced target scalar index count", span)?;
    indices.push(index);
    Ok(indices)
}

fn full_sliced_target_indices(
    dim: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let mut indices =
        lower_vec_with_capacity(dim, "sliced target full dimension index count", span)?;
    indices.extend(1..=dim);
    Ok(indices)
}

fn checked_literal_positive_index_expr(
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return Ok(None);
    };
    if *value > 0 {
        let index = usize::try_from(*value).map_err(|_| {
            lower_contract_violation(
                format!("literal subscript index {value} exceeds host index range"),
                span,
            )
        })?;
        Ok(Some(index))
    } else {
        Ok(None)
    }
}

fn continuous_equation_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .states
        .get(lhs)
        .or_else(|| dae_model.variables.algebraics.get(lhs))
        .or_else(|| dae_model.variables.outputs.get(lhs))
        .map(|var| var.dims.as_slice())
}
