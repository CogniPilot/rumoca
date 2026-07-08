use super::*;

pub(in crate::lower) fn function_projected_residuals_with_owner(
    residual: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let Some((lhs, rhs)) = split_subtraction(residual) else {
        return Ok(None);
    };
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    if let Some((call, field)) = function_field_access(rhs)
        && let Some(call_outputs) = analysis.top_level_function_call_outputs(
            call,
            inherited_projection_source_span(call.span(), owner_span),
        )?
    {
        let Some(target_base) = plain_var_ref_name(lhs) else {
            return Ok(None);
        };
        return Ok(Some(analysis.projected_field_residuals_for_target(
            target_base,
            field,
            call_outputs,
            true,
            owner_span,
        )?));
    }
    if let Some((call, field)) = function_field_access(lhs)
        && let Some(call_outputs) = analysis.top_level_function_call_outputs(
            call,
            inherited_projection_source_span(call.span(), owner_span),
        )?
    {
        let Some(target_base) = plain_var_ref_name(rhs) else {
            return Ok(None);
        };
        return Ok(Some(analysis.projected_field_residuals_for_target(
            target_base,
            field,
            call_outputs,
            false,
            owner_span,
        )?));
    }
    if let Some(call_outputs) = analysis.top_level_function_call_outputs(
        rhs,
        inherited_projection_source_span(rhs.span(), owner_span),
    )? {
        let Some(target_base) = plain_var_ref_name(lhs) else {
            return Ok(None);
        };
        return Ok(Some(analysis.projected_residuals_for_target(
            target_base,
            call_outputs,
            true,
            owner_span,
        )?));
    }
    if let Some(call_outputs) = analysis.top_level_function_call_outputs(
        lhs,
        inherited_projection_source_span(lhs.span(), owner_span),
    )? {
        let Some(target_base) = plain_var_ref_name(rhs) else {
            return Ok(None);
        };
        return Ok(Some(analysis.projected_residuals_for_target(
            target_base,
            call_outputs,
            false,
            owner_span,
        )?));
    }
    Ok(None)
}

pub(in crate::lower::derivative_rhs) fn function_field_access(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Expression, &str)> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return None;
    };
    matches!(base.as_ref(), rumoca_core::Expression::FunctionCall { .. })
        .then_some((base.as_ref(), field.as_str()))
}

#[allow(clippy::excessive_nesting)]
pub(in crate::lower) fn function_call_projected_scalars_with_owner(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    if let Some((call, field)) = function_field_access(expr)
        && let Some(outputs) = analysis.top_level_function_call_outputs(
            call,
            inherited_projection_source_span(call.span(), owner_span),
        )?
    {
        let scope = FunctionProjectionScope::default();
        let mut selected = projection_vec_with_capacity(
            outputs.len(),
            "projected function field scalar count",
            owner_span,
        )?;
        for output in outputs {
            if let Some(expr) =
                analysis.project_output_field_value(output, field, &scope, owner_span)?
            {
                let dims = analysis
                    .expr_dims_with_owner(&expr, &scope, 0, owner_span)?
                    .unwrap_or_default();
                if dims.is_empty() {
                    selected.push(expr);
                } else if let Some(mut scalars) =
                    analysis.project_value_scalars(&expr, &dims, &scope, 0, owner_span)?
                {
                    selected.append(&mut scalars);
                } else {
                    selected.push(expr);
                }
            }
        }
        if !selected.is_empty() {
            return Ok(Some(selected));
        }
    }
    if let Some(outputs) = analysis.top_level_function_call_outputs(
        expr,
        inherited_projection_source_span(expr.span(), owner_span),
    )? {
        return projected_output_expressions(
            analysis.resolve_projected_scalar_field_outputs(outputs, owner_span)?,
            owner_span,
        )
        .map(Some);
    }
    if let Some(values) =
        projected_qualified_function_output_scalars(expr, dae_model, &analysis, owner_span)?
    {
        return Ok(Some(values));
    }
    let Some((call, scalar_index)) = selected_function_output_call(expr, dae_model)? else {
        return Ok(None);
    };
    let Some(outputs) = analysis.top_level_function_call_outputs(
        &call,
        inherited_projection_source_span(call.span(), owner_span),
    )?
    else {
        return Ok(None);
    };
    let output_expr = outputs
        .get(scalar_index)
        .ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "selected function output index {} is out of bounds for {} projected outputs",
                    scalar_index + 1,
                    outputs.len()
                ),
                owner_span,
            )
        })?
        .expr
        .clone();
    let span = owner_span;
    let mut values =
        projection_vec_with_capacity(1, "selected function output scalar count", span)?;
    values.push(output_expr);
    Ok(Some(values))
}

pub(in crate::lower) fn function_call_projected_output_groups_with_owner(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<Vec<rumoca_core::Expression>>>, LowerError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        is_constructor: false,
        ..
    } = expr
    else {
        return Ok(None);
    };
    let Some(function) = dae_model.symbols.functions.get(name.var_name()) else {
        return Ok(None);
    };
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    let Some(outputs) = analysis.top_level_function_call_outputs(
        expr,
        inherited_projection_source_span(expr.span(), owner_span),
    )?
    else {
        return Ok(None);
    };
    let mut groups = projection_vec_with_capacity(
        function.outputs.len(),
        "projected function output group count",
        owner_span,
    )?;
    for output_param in &function.outputs {
        let mut group = projection_vec_with_capacity(
            outputs.len(),
            "projected function output scalar group count",
            owner_span,
        )?;
        if function.outputs.len() == 1 {
            for output in &outputs {
                group.push(output.expr.clone());
            }
        } else {
            group.extend(
                outputs
                    .iter()
                    .filter(|output| {
                        output
                            .field_path
                            .first()
                            .is_some_and(|field| field == &output_param.name)
                    })
                    .map(|output| output.expr.clone()),
            );
        }
        groups.push(group);
    }
    Ok(Some(groups))
}

fn projected_qualified_function_output_scalars(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    analysis: &FunctionProjectionAnalysis<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        span,
    } = expr
    else {
        return Ok(None);
    };
    if dae_model.symbols.functions.contains_key(name.var_name()) {
        return Ok(None);
    }
    let selected_name = name.as_str();
    for (function_name, function) in &dae_model.symbols.functions {
        let prefix = format!("{}.", function_name.as_str());
        let Some(selector) = selected_name.strip_prefix(&prefix) else {
            continue;
        };
        let call = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_var_name(function_name.clone()),
            args: args.clone(),
            is_constructor: false,
            span: *span,
        };
        let Some(outputs) = analysis.top_level_function_call_outputs(
            &call,
            inherited_projection_source_span(call.span(), owner_span),
        )?
        else {
            return Ok(None);
        };
        let mut selected = projection_vec_with_capacity(
            outputs.len(),
            "qualified projected function output scalar count",
            owner_span,
        )?;
        for output in outputs {
            let output_selector = projected_output_selector(&output);
            if output_selector == selector
                || function.outputs.iter().any(|function_output| {
                    selector == format!("{}.{}", function_output.name, output_selector)
                })
            {
                selected.push(output.expr);
            }
        }
        return Ok((!selected.is_empty()).then_some(selected));
    }
    Ok(None)
}

fn projected_output_selector(output: &ProjectedFunctionOutput) -> String {
    let mut selector = output.field_path.join(".");
    for index in &output.selector_indices {
        selector.push('[');
        selector.push_str(&index.to_string());
        selector.push(']');
    }
    selector
}

pub(in crate::lower) fn project_array_like_scalars_with_owner(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    let scope = FunctionProjectionScope::default();
    let Some(dims) = analysis.expr_dims_with_owner(expr, &scope, 0, owner_span)? else {
        return Ok(None);
    };
    if dims.is_empty() {
        return Ok(None);
    }
    analysis.project_value_scalars(expr, &dims, &scope, 0, owner_span)
}

pub(in crate::lower) fn project_array_like_scalar_with_owner(
    expr: &rumoca_core::Expression,
    flat_index: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    let scope = FunctionProjectionScope::default();
    let Some(dims) = analysis.expr_dims_with_owner(expr, &scope, 0, owner_span)? else {
        return Ok(None);
    };
    if dims.is_empty() {
        return Ok(None);
    }
    analysis.project_value(expr, &dims, flat_index, &scope, 0, owner_span)
}

pub(in crate::lower::derivative_rhs) fn projected_output_expressions(
    outputs: Vec<ProjectedFunctionOutput>,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let mut expressions = projection_vec_with_capacity(
        outputs.len(),
        "projected function output scalar count",
        span,
    )?;
    for output in outputs {
        expressions.push(output.expr);
    }
    Ok(expressions)
}

pub(in crate::lower::derivative_rhs) fn plain_var_ref_name(
    expr: &rumoca_core::Expression,
) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        _ => None,
    }
}

pub(in crate::lower::derivative_rhs) fn project_target_scalar_outputs(
    dims: &[i64],
    values: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
    let mut outputs =
        projection_vec_with_capacity(values.len(), "projected function scalar output count", span)?;
    for (idx, expr) in values.into_iter().enumerate() {
        if matches!(expr, rumoca_core::Expression::Empty { .. }) {
            return Err(LowerError::InvalidFunction {
                name: "projected function output".to_string(),
                reason: format!("array output component {idx} was not assigned"),
            }
            .with_fallback_span(span));
        }
        outputs.push(ProjectedFunctionOutput {
            field_path: Vec::new(),
            selector_indices: required_flat_index_to_subscripts(
                dims,
                idx,
                expr.span().unwrap_or(span),
            )?,
            expr,
        });
    }
    Ok(outputs)
}

pub(in crate::lower::derivative_rhs) fn function_outputs_dims(
    output_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    if output_count == 1 {
        Ok(Vec::new())
    } else {
        let mut dims = projection_vec_with_capacity(1, "function output dimension count", span)?;
        dims.push(checked_usize_to_i64(
            output_count,
            "function output count",
            span,
        )?);
        Ok(dims)
    }
}

pub(in crate::lower::derivative_rhs) fn checked_generated_subscript_from_usize(
    index: usize,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, LowerError> {
    let index = checked_usize_to_i64(index, "function output projection subscript index", span)?;
    Ok(rumoca_core::Subscript::try_generated_index(
        index, span, context,
    )?)
}

pub(in crate::lower::derivative_rhs) fn checked_usize_to_i64(
    value: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    i64::try_from(value).map_err(|_| {
        LowerError::contract_violation(format!("{context} {value} exceeds i64 range"), span)
    })
}

pub(in crate::lower::derivative_rhs) fn checked_projection_offset(
    base: usize,
    stride: usize,
    offset: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let scaled = base.checked_mul(stride).ok_or_else(|| {
        LowerError::contract_violation(
            format!("{context} multiplication overflows host index range"),
            span,
        )
    })?;
    scaled.checked_add(offset).ok_or_else(|| {
        LowerError::contract_violation(
            format!("{context} addition overflows host index range"),
            span,
        )
    })
}

pub(in crate::lower::derivative_rhs) fn checked_usize_dims_to_i64(
    dims: &[usize],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut converted = projection_vec_with_capacity(dims.len(), context, span)?;
    for dim in dims {
        converted.push(checked_usize_to_i64(*dim, context, span)?);
    }
    Ok(converted)
}

pub(in crate::lower::derivative_rhs) fn variable_dims_i64(
    variable: &dae::Variable,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut dims =
        projection_vec_with_capacity(variable.dims.len(), "variable dimension count", span)?;
    for dim in &variable.dims {
        if *dim < 0 {
            return Err(LowerError::contract_violation(
                format!("variable dimension {dim} is negative"),
                span,
            ));
        }
        dims.push(*dim);
    }
    Ok(dims)
}

pub(in crate::lower::derivative_rhs) fn required_merged_projection_dims(
    name: &str,
    entry_scope: &FunctionProjectionScope,
    branch_scopes: &[FunctionProjectionScope],
    else_scope: &FunctionProjectionScope,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    merged_projection_dims(name, entry_scope, branch_scopes, else_scope, span)?.ok_or_else(|| {
        LowerError::contract_violation(
            format!("if-statement projection for `{name}` has scalar values but no dimensions"),
            span,
        )
    })
}
