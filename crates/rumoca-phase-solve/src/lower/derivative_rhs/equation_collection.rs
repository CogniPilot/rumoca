use super::*;

pub(in crate::lower) fn collect_derivative_equations(
    dae_model: &dae::Dae,
    state_names: &HashSet<String>,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<(Vec<DerivativeEquation>, Vec<bool>), LowerError> {
    let mut equations = Vec::new();
    let mut flags = Vec::new();
    if !dae_model.continuous.equations.is_empty() {
        reserve_derivative_vec_capacity(
            &mut flags,
            dae_model.continuous.equations.len(),
            "derivative equation flags",
            first_continuous_equation_span(dae_model)?,
        )?;
    }
    for (equation_index, equation) in dae_model.continuous.equations.iter().enumerate() {
        let before = equations.len();
        let projected = function_projected_residuals_with_owner(
            &equation.rhs,
            dae_model,
            structural_bindings,
            equation.span,
        )?;
        if let Some(projected) = projected {
            for residual in projected {
                append_derivative_rows_for_residual(
                    &mut equations,
                    &residual,
                    state_names,
                    dae_model,
                    structural_bindings,
                    ResidualDerivativeRowsContext {
                        scalar_count: Some(1),
                        dae_equation_index: equation_index,
                        owner_span: equation.span,
                    },
                )?;
            }
            flags.push(equations.len() > before);
            continue;
        }
        append_derivative_rows_for_residual(
            &mut equations,
            &equation.rhs,
            state_names,
            dae_model,
            structural_bindings,
            ResidualDerivativeRowsContext {
                scalar_count: Some(equation.scalar_count),
                dae_equation_index: equation_index,
                owner_span: equation.span,
            },
        )?;
        flags.push(equations.len() > before);
    }
    Ok((equations, flags))
}

#[derive(Clone, Copy)]
pub(in crate::lower) struct ResidualDerivativeRowsContext {
    scalar_count: Option<usize>,
    dae_equation_index: usize,
    owner_span: rumoca_core::Span,
}

pub(in crate::lower) fn append_derivative_rows_for_residual(
    equations: &mut Vec<DerivativeEquation>,
    residual: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    context: ResidualDerivativeRowsContext,
) -> Result<(), LowerError> {
    if let Some(mut rows) = derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
        context.scalar_count,
        context.owner_span,
    )? {
        reserve_derivative_vec_capacity(
            equations,
            rows.len(),
            "derivative equation rows",
            derivative_expr_span_or_owner(residual, context.owner_span)?,
        )?;
        for row in &mut rows {
            row.dae_equation_index = Some(context.dae_equation_index);
        }
        equations.append(&mut rows);
    }
    Ok(())
}

pub(in crate::lower) fn collect_direct_assignments(
    dae_model: &dae::Dae,
    equation_flags: &[bool],
    structural_bindings: &IndexMap<String, f64>,
) -> Result<IndexMap<String, DirectAssignmentValue>, LowerError> {
    let mut assignments = IndexMap::new();
    for (idx, equation) in dae_model.continuous.equations.iter().enumerate() {
        let Some(handled_by_derivative) = equation_flags.get(idx).copied() else {
            return Err(LowerError::contract_violation(
                format!("missing derivative-equation flag for continuous equation {idx}"),
                equation.span,
            ));
        };
        if handled_by_derivative {
            continue;
        }
        if let Some(projected) = function_projected_residuals_with_owner(
            &equation.rhs,
            dae_model,
            structural_bindings,
            equation.span,
        )? {
            insert_projected_direct_assignments(
                dae_model,
                &mut assignments,
                &projected,
                structural_bindings,
                equation.span,
            )?;
            continue;
        }
        let Some((target, rhs)) = direct_assignment_target_rhs(equation)? else {
            continue;
        };
        insert_direct_assignment(
            dae_model,
            &mut assignments,
            target,
            rhs,
            equation.scalar_count,
            structural_bindings,
            equation.span,
        )?;
    }
    Ok(assignments)
}

fn insert_projected_direct_assignments(
    dae_model: &dae::Dae,
    assignments: &mut IndexMap<String, DirectAssignmentValue>,
    residuals: &[rumoca_core::Expression],
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    for residual in residuals {
        let Some((target, rhs)) = direct_assignment_residual_target_rhs(residual)? else {
            continue;
        };
        insert_direct_assignment(
            dae_model,
            assignments,
            target,
            rhs,
            1,
            structural_bindings,
            span,
        )?;
    }
    Ok(())
}

pub(in crate::lower) fn collect_missing_indexed_record_field_assignments(
    dae_model: &dae::Dae,
    state_names: &HashSet<String>,
    layout: &VarLayout,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<IndexMap<String, DirectAssignmentValue>, LowerError> {
    let (_, equation_flags) =
        collect_derivative_equations(dae_model, state_names, structural_bindings)?;
    let direct_assignments =
        collect_direct_assignments(dae_model, &equation_flags, structural_bindings)?;
    let mut missing = IndexMap::new();
    if !direct_assignments.is_empty() {
        reserve_derivative_index_map_capacity(
            &mut missing,
            direct_assignments.len(),
            "missing indexed record-field assignments",
            first_continuous_equation_span(dae_model)?,
        )?;
    }
    for (key, assignment) in direct_assignments {
        if layout.binding(&key).is_none() && has_indexed_record_field_segment(&key) {
            missing.insert(key, assignment);
        }
    }
    Ok(missing)
}

pub(in crate::lower) fn has_indexed_record_field_segment(key: &str) -> bool {
    crate::path_utils::segments(key)
        .iter()
        .any(|segment| rumoca_core::split_trailing_subscript_suffix(segment).is_some())
}

pub(in crate::lower) fn direct_assignment_target_rhs(
    equation: &dae::Equation,
) -> Result<Option<(rumoca_core::VarName, rumoca_core::Expression)>, LowerError> {
    if let Some(lhs) = equation.lhs.as_ref()
        && !equation.rhs.contains_der()
    {
        return Ok(Some((lhs.var_name().clone(), equation.rhs.clone())));
    }

    direct_assignment_residual_target_rhs(&equation.rhs)
}

fn direct_assignment_residual_target_rhs(
    residual: &rumoca_core::Expression,
) -> Result<Option<(rumoca_core::VarName, rumoca_core::Expression)>, LowerError> {
    let Some((lhs, rhs)) = split_subtraction(residual) else {
        return Ok(None);
    };
    if !rhs.contains_der()
        && let Some(target) = direct_assignment_target(lhs)?
    {
        return Ok(Some((target, rhs.clone())));
    }
    if !lhs.contains_der()
        && let Some(target) = direct_assignment_target(rhs)?
    {
        return Ok(Some((target, lhs.clone())));
    }
    Ok(None)
}

pub(in crate::lower) fn direct_assignment_target(
    expr: &rumoca_core::Expression,
) -> Result<Option<rumoca_core::VarName>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => {
            if subscripts.is_empty() {
                return Ok(Some(name.var_name().clone()));
            }
            let Some(indices) = direct_assignment_static_subscript_indices(subscripts, *span)?
            else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::VarName::new(dae::format_subscript_key(
                name.as_str(),
                &indices,
            ))))
        }
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let Some(base) = direct_assignment_target(base)? else {
                return Ok(None);
            };
            let Some(indices) = direct_assignment_static_subscript_indices(subscripts, *span)?
            else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::VarName::new(dae::format_subscript_key(
                base.as_str(),
                &indices,
            ))))
        }
        _ => Ok(None),
    }
}

fn direct_assignment_static_subscript_indices(
    subscripts: &[rumoca_core::Subscript],
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    if subscripts
        .iter()
        .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
    {
        return Ok(None);
    }
    super::super::static_subscript_indices_with_owner(subscripts, owner_span).map_err(|err| {
        err.with_fallback_span(direct_assignment_subscript_span(subscripts).unwrap_or(owner_span))
    })
}

fn direct_assignment_subscript_span(
    subscripts: &[rumoca_core::Subscript],
) -> Option<rumoca_core::Span> {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
}

fn derivative_expr_span_or_owner(
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
        reason: "derivative expression requires source span metadata".to_string(),
    })
}

pub(in crate::lower) fn insert_direct_assignment(
    dae_model: &dae::Dae,
    assignments: &mut IndexMap<String, DirectAssignmentValue>,
    target: rumoca_core::VarName,
    rhs: rumoca_core::Expression,
    scalar_count: usize,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let target_key = target.as_str();
    let Some((base, dims, size)) = direct_assignment_shape(
        dae_model,
        assignments,
        target_key,
        scalar_count,
        &rhs,
        structural_bindings,
        owner_span,
    )?
    else {
        return Ok(());
    };
    if dae_model
        .variables
        .states
        .contains_key(&rumoca_core::VarName::new(base.as_str()))
    {
        return Ok(());
    }
    let rhs_span = derivative_expr_span_or_owner(&rhs, owner_span)?;

    if size <= 1 || base != target_key {
        reserve_derivative_index_map_capacity(
            assignments,
            1,
            "direct assignment entries",
            rhs_span,
        )?;
        assignments.insert(
            target_key.to_string(),
            DirectAssignmentValue::full(rhs, owner_span),
        );
        return Ok(());
    }

    let repeat_period = direct_assignment_repeat_period(
        dae_model,
        assignments,
        &dims,
        &rhs,
        structural_bindings,
        owner_span,
    )?;
    let capacity = size.checked_add(1).ok_or_else(|| {
        LowerError::contract_violation(
            format!("direct assignment entry count for `{base}` overflows"),
            rhs_span,
        )
    })?;
    reserve_derivative_index_map_capacity(
        assignments,
        capacity,
        "direct assignment entries",
        rhs_span,
    )?;
    assignments.insert(
        base.clone(),
        DirectAssignmentValue::full(rhs.clone(), owner_span),
    );
    for flat_index in 0..size {
        let key = dae::scalar_name_text_for_flat_index(&base, &dims, flat_index);
        assignments.insert(
            key,
            DirectAssignmentValue::scalar(rhs.clone(), flat_index, repeat_period, owner_span),
        );
    }
    Ok(())
}

fn first_continuous_equation_span(dae_model: &dae::Dae) -> Result<rumoca_core::Span, LowerError> {
    dae_model
        .continuous
        .equations
        .first()
        .and_then(equation_collection_span)
        .map(Result::Ok)
        .unwrap_or_else(missing_continuous_equation_collection_span)
}

fn equation_collection_span(equation: &dae::Equation) -> Option<rumoca_core::Span> {
    if !equation.span.is_dummy() {
        return Some(equation.span);
    }
    equation.rhs.span().filter(|span| !span.is_dummy())
}

fn missing_continuous_equation_collection_span() -> Result<rumoca_core::Span, LowerError> {
    Err(LowerError::UnspannedContractViolation {
        reason: "missing source provenance for continuous equation collection".to_string(),
    })
}

fn reserve_derivative_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| LowerError::contract_violation(format!("{context} capacity overflows"), span))
}

fn reserve_derivative_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError>
where
    K: std::hash::Hash + Eq,
{
    values
        .try_reserve(capacity)
        .map_err(|_| LowerError::contract_violation(format!("{context} capacity overflows"), span))
}

pub(in crate::lower) fn direct_assignment_repeat_period(
    dae_model: &dae::Dae,
    assignments: &IndexMap<String, DirectAssignmentValue>,
    target_dims: &[i64],
    rhs: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let rhs_span = derivative_expr_span_or_owner(rhs, owner_span)?;
    let Some(rhs_dims) =
        direct_assignment_rhs_dims(dae_model, assignments, rhs, structural_bindings, rhs_span)?
    else {
        return Ok(None);
    };
    if rhs_dims.is_empty() || target_dims.len() != rhs_dims.len() + 1 {
        return Ok(None);
    }
    let Some(target_tail) = target_dims.get(1..) else {
        return Ok(None);
    };
    let target_matches_rhs = target_tail
        .iter()
        .zip(rhs_dims.iter())
        .all(|(target_dim, rhs_dim)| usize::try_from(*target_dim).ok() == Some(*rhs_dim));
    if !target_matches_rhs {
        return Ok(None);
    }
    let period = rhs_dims
        .iter()
        .try_fold(1usize, |count, dim| count.checked_mul(*dim))
        .ok_or_else(|| {
            LowerError::contract_violation(
                "direct assignment RHS repeat period overflows host index range",
                rhs_span,
            )
        })?;
    Ok((period > 1).then_some(period))
}

pub(in crate::lower) fn direct_assignment_rhs_dims(
    dae_model: &dae::Dae,
    assignments: &IndexMap<String, DirectAssignmentValue>,
    rhs: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    match rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if let Some(dims) = variable_dims(dae_model, name.as_str())? {
                return Ok(Some(dims));
            }
            let Some(assignment) = assignments.get(name.as_str()) else {
                return Ok(None);
            };
            Ok(Some(expression_result_dims(
                &assignment.rhs,
                dae_model,
                structural_bindings,
                owner_span,
            )?))
        }
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => Ok(direct_assignment_array_dims(elements, *is_matrix)),
        _ => Ok(Some(expression_result_dims(
            rhs,
            dae_model,
            structural_bindings,
            owner_span,
        )?)),
    }
}

pub(in crate::lower) fn direct_assignment_array_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Option<Vec<usize>> {
    if !is_matrix {
        return Some(vec![elements.len()]);
    }
    let first = elements.first()?;
    let rumoca_core::Expression::Array {
        elements: first_row,
        ..
    } = first
    else {
        return None;
    };
    Some(vec![elements.len(), first_row.len()])
}

pub(in crate::lower) fn direct_assignment_shape(
    dae_model: &dae::Dae,
    assignments: &IndexMap<String, DirectAssignmentValue>,
    target: &str,
    scalar_count: usize,
    rhs: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<(String, Vec<i64>, usize)>, LowerError> {
    let rhs_span = derivative_expr_span_or_owner(rhs, owner_span)?;
    if let Some(var) = dae_model
        .variables
        .algebraics
        .get(&rumoca_core::VarName::new(target))
    {
        return Ok(Some((
            target.to_string(),
            var.dims.clone(),
            super::super::helpers::variable_size(var)?,
        )));
    }
    if let Some(var) = dae_model
        .variables
        .outputs
        .get(&rumoca_core::VarName::new(target))
    {
        return Ok(Some((
            target.to_string(),
            var.dims.clone(),
            super::super::helpers::variable_size(var)?,
        )));
    }
    let Some(base) = dae::component_base_name(target) else {
        return Ok(None);
    };
    if let Some(var) = dae_model
        .variables
        .algebraics
        .get(&rumoca_core::VarName::new(base.as_str()))
    {
        return Ok(Some((base, var.dims.clone(), 1)));
    }
    if let Some(var) = dae_model
        .variables
        .outputs
        .get(&rumoca_core::VarName::new(base.as_str()))
    {
        return Ok(Some((base, var.dims.clone(), 1)));
    }
    if let Some(rhs_dims) =
        direct_assignment_rhs_dims(dae_model, assignments, rhs, structural_bindings, rhs_span)?
    {
        if rhs_dims.len() <= 1 {
            return Ok((scalar_count == 1).then(|| (target.to_string(), Vec::new(), 1)));
        }
        let size = rhs_dims.iter().try_fold(1usize, |count, dim| {
            count.checked_mul(*dim).ok_or_else(|| {
                LowerError::contract_violation(
                    format!("direct assignment inferred shape for `{target}` overflows"),
                    rhs_span,
                )
            })
        })?;
        if size == scalar_count {
            let mut dims = derivative_i64_vec_with_capacity(
                rhs_dims.len(),
                "direct assignment dimension count",
                rhs,
                owner_span,
            )?;
            for dim in rhs_dims {
                dims.push(checked_direct_assignment_dim_i64(
                    dim, target, rhs, owner_span,
                )?);
            }
            return Ok(Some((target.to_string(), dims, size)));
        }
    }

    Ok((scalar_count == 1).then(|| (target.to_string(), Vec::new(), 1)))
}

fn checked_direct_assignment_dim_i64(
    dim: usize,
    target: &str,
    rhs: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    let rhs_span = derivative_expr_span_or_owner(rhs, owner_span)?;
    i64::try_from(dim).map_err(|_| {
        LowerError::contract_violation(
            format!("direct assignment inferred dimension {dim} for `{target}` exceeds i64"),
            rhs_span,
        )
    })
}

fn derivative_i64_vec_with_capacity(
    capacity: usize,
    context: &'static str,
    rhs: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut values = Vec::new();
    reserve_derivative_vec_capacity(
        &mut values,
        capacity,
        context,
        derivative_expr_span_or_owner(rhs, owner_span)?,
    )?;
    Ok(values)
}

pub(in crate::lower) fn derivative_equation_from_residual(
    residual: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<DerivativeEquation>, LowerError> {
    if !residual.contains_der() {
        return Ok(None);
    }
    let residual_span = derivative_expr_span_or_owner(residual, owner_span)?;
    if let Some(equation) = derivative_equation_from_if_residual(residual, ctx, residual_span)? {
        return Ok(Some(equation));
    }

    if let Some((lhs, rhs)) = split_subtraction(residual) {
        if let Some((coefficients, remainder)) =
            derivative_linear_parts_for_equation_probe(lhs, ctx, residual_span)?
            && !rhs.contains_der()
        {
            return Ok(Some(DerivativeEquation {
                coefficients,
                rhs: rhs_without_remainder(rhs.clone(), remainder, residual_span),
                span: residual_span,
                dae_equation_index: None,
            }));
        }
        if let Some((coefficients, remainder)) =
            derivative_linear_parts_for_equation_probe(rhs, ctx, residual_span)?
            && !lhs.contains_der()
        {
            return Ok(Some(DerivativeEquation {
                coefficients,
                rhs: rhs_without_remainder(lhs.clone(), remainder, residual_span),
                span: residual_span,
                dae_equation_index: None,
            }));
        }
    }
    if let Some((coefficients, remainder)) =
        derivative_linear_parts_for_equation_probe(residual, ctx, residual_span)?
    {
        return Ok(Some(DerivativeEquation {
            coefficients,
            rhs: rhs_without_remainder(
                zero_expr_with_span(residual_span),
                remainder,
                residual_span,
            ),
            span: residual_span,
            dae_equation_index: None,
        }));
    }
    Ok(None)
}

fn derivative_linear_parts_for_equation_probe(
    expr: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<LinearParts>, LowerError> {
    match derivative_linear_parts(expr, ctx, owner_span) {
        Ok(parts) => Ok(parts),
        Err(LowerError::MissingBinding { .. }) => Ok(None),
        Err(err) => Err(err),
    }
}

pub(in crate::lower) fn derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    scalar_count: Option<usize>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    if let Some(equations) = matrix_vector_derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
        owner_span,
    )? {
        return Ok(Some(equations));
    }
    if let Some(equations) = direct_derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
        owner_span,
    )? {
        return Ok(Some(equations));
    }
    if let Some(equations) = affine_vector_derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
        owner_span,
    )? {
        return Ok(Some(equations));
    }
    let ctx = DerivativeLinearCtx {
        state_names,
        dae_model,
        structural_bindings,
    };
    if let Some(equations) = projected_vector_derivative_equations_from_residual(
        residual,
        scalar_count,
        &ctx,
        owner_span,
    )? {
        return Ok(Some(equations));
    }
    let Some(equation) = derivative_equation_from_residual(residual, &ctx, owner_span)? else {
        return Ok(None);
    };
    let mut equations = derivative_vec_with_capacity(
        1,
        "single derivative equation row count",
        derivative_expr_span_or_owner(residual, owner_span)?,
    )?;
    equations.push(equation);
    Ok(Some(equations))
}

pub(in crate::lower) fn projected_vector_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    scalar_count: Option<usize>,
    ctx: &DerivativeLinearCtx<'_>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    if !residual.contains_der() {
        return Ok(None);
    }
    if scalar_count.is_none_or(|count| count <= 1) {
        return Ok(None);
    }
    let span = derivative_expr_span_or_owner(residual, owner_span)?;
    let dims = expression_result_dims(residual, ctx.dae_model, ctx.structural_bindings, span)?;
    let count = dims.iter().try_fold(1usize, |count, dim| {
        count.checked_mul(*dim).ok_or_else(|| {
            LowerError::contract_violation(
                "projected derivative equation scalar count overflows host index range",
                span,
            )
        })
    })?;
    if count <= 1 {
        return Ok(None);
    }
    let scalars = match project_expression_scalars(
        residual,
        &dims,
        ctx.dae_model,
        ctx.structural_bindings,
        span,
    ) {
        Ok(scalars) => scalars,
        Err(LowerError::MissingBinding { .. }) => return Ok(None),
        Err(err) => return Err(err),
    };
    let Some(scalars) = scalars else {
        return Ok(None);
    };
    if scalars.len() != count {
        return Ok(None);
    }
    let mut equations = derivative_vec_with_capacity(
        scalars.len(),
        "projected derivative equation row count",
        span,
    )?;
    for scalar in scalars {
        let Some(equation) = derivative_equation_from_residual(&scalar, ctx, span)? else {
            return Ok(None);
        };
        equations.push(equation);
    }
    Ok((!equations.is_empty()).then_some(equations))
}

pub(in crate::lower) fn direct_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    let Some((lhs, rhs)) = split_subtraction(residual) else {
        return Ok(None);
    };
    if let Some(target) = pure_der_arg(lhs)
        && !rhs.contains_der()
        && let Some(equations) = optional_derivative_probe(expanded_direct_derivative_equations(
            target,
            rhs,
            state_names,
            dae_model,
            structural_bindings,
            derivative_expr_span_or_owner(residual, owner_span)?,
        ))?
    {
        return Ok(Some(equations));
    }
    if let Some(target) = pure_der_arg(rhs)
        && !lhs.contains_der()
        && let Some(equations) = optional_derivative_probe(expanded_direct_derivative_equations(
            target,
            lhs,
            state_names,
            dae_model,
            structural_bindings,
            derivative_expr_span_or_owner(residual, owner_span)?,
        ))?
    {
        return Ok(Some(equations));
    }
    Ok(None)
}

pub(in crate::lower) struct MatrixDerivativeTerm {
    target: rumoca_core::Expression,
    coefficient_rows: Vec<IndexMap<String, rumoca_core::Expression>>,
}

pub(in crate::lower) struct AffineVectorDerivativeTerm {
    target: rumoca_core::Expression,
    coefficient: rumoca_core::Expression,
    remainder: Option<rumoca_core::Expression>,
}

pub(in crate::lower) fn affine_vector_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    let Some((lhs, rhs)) = split_subtraction(residual) else {
        return Ok(None);
    };
    if let Some(term) = affine_vector_derivative_term(rhs, owner_span)?
        && !lhs.contains_der()
        && let Some(equations) =
            optional_derivative_probe(build_affine_vector_derivative_equations(
                term,
                lhs,
                state_names,
                dae_model,
                structural_bindings,
                owner_span,
            ))?
    {
        return Ok(Some(equations));
    }
    if let Some(term) = affine_vector_derivative_term(lhs, owner_span)?
        && !rhs.contains_der()
        && let Some(equations) =
            optional_derivative_probe(build_affine_vector_derivative_equations(
                term,
                rhs,
                state_names,
                dae_model,
                structural_bindings,
                owner_span,
            ))?
    {
        return Ok(Some(equations));
    }
    Ok(None)
}

pub(in crate::lower) fn affine_vector_derivative_term(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<Option<AffineVectorDerivativeTerm>, LowerError> {
    if let Some(target) = pure_der_arg(expr) {
        let span = derivative_expr_span_or_owner(expr, owner_span)?;
        return Ok(Some(AffineVectorDerivativeTerm {
            target: target.clone(),
            coefficient: one_expr_with_span(span),
            remainder: None,
        }));
    }

    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = expr else {
        return Ok(None);
    };
    let expr_span = derivative_expr_span_or_owner(expr, owner_span)?;
    match op {
        op if is_add(op) => {
            if let Some(mut term) = affine_vector_derivative_term(lhs, expr_span)?
                && !rhs.contains_der()
            {
                term.remainder = Some(combine_optional_remainder(
                    term.remainder,
                    rhs.as_ref().clone(),
                    false,
                    expr_span,
                ));
                return Ok(Some(term));
            }
            if let Some(mut term) = affine_vector_derivative_term(rhs, expr_span)?
                && !lhs.contains_der()
            {
                term.remainder = Some(combine_optional_remainder(
                    term.remainder,
                    lhs.as_ref().clone(),
                    false,
                    expr_span,
                ));
                return Ok(Some(term));
            }
            Ok(None)
        }
        op if is_sub(op) => {
            if let Some(mut term) = affine_vector_derivative_term(lhs, expr_span)?
                && !rhs.contains_der()
            {
                term.remainder = Some(combine_optional_remainder(
                    term.remainder,
                    rhs.as_ref().clone(),
                    true,
                    expr_span,
                ));
                return Ok(Some(term));
            }
            if let Some(mut term) = affine_vector_derivative_term(rhs, expr_span)?
                && !lhs.contains_der()
            {
                term.coefficient = neg_with_span(term.coefficient, expr_span);
                term.remainder = Some(combine_optional_remainder(
                    Some(lhs.as_ref().clone()),
                    term.remainder
                        .unwrap_or_else(|| zero_expr_with_span(expr_span)),
                    true,
                    expr_span,
                ));
                return Ok(Some(term));
            }
            Ok(None)
        }
        op if is_mul(op) => {
            if let Some(target) = pure_der_arg(lhs)
                && !rhs.contains_der()
            {
                return Ok(Some(AffineVectorDerivativeTerm {
                    target: target.clone(),
                    coefficient: rhs.as_ref().clone(),
                    remainder: None,
                }));
            }
            if let Some(target) = pure_der_arg(rhs)
                && !lhs.contains_der()
            {
                return Ok(Some(AffineVectorDerivativeTerm {
                    target: target.clone(),
                    coefficient: lhs.as_ref().clone(),
                    remainder: None,
                }));
            }
            Ok(None)
        }
        _ => Ok(None),
    }
}

pub(in crate::lower) fn combine_optional_remainder(
    current: Option<rumoca_core::Expression>,
    next: rumoca_core::Expression,
    subtract_next: bool,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    match (current, subtract_next) {
        (Some(current), false) => add_with_span(current, next, span),
        (Some(current), true) => sub_with_span(current, next, span),
        (None, false) => next,
        (None, true) => neg_with_span(next, span),
    }
}

pub(in crate::lower) fn build_affine_vector_derivative_equations(
    term: AffineVectorDerivativeTerm,
    base_rhs: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    let target_keys =
        derivative_arg_binding_keys(&term.target, dae_model, structural_bindings, span)?;
    if target_keys.len() <= 1 || !target_keys.iter().all(|key| state_names.contains(key)) {
        return Ok(None);
    }
    let base_values = scalarized_rhs_expressions_with_owner(
        base_rhs,
        &term.target,
        target_keys.len(),
        dae_model,
        structural_bindings,
        span,
    )?;
    if base_values.len() != target_keys.len() {
        return Ok(None);
    }
    let remainder_values = term
        .remainder
        .as_ref()
        .map(|remainder| {
            scalarized_rhs_expressions_with_owner(
                remainder,
                &term.target,
                target_keys.len(),
                dae_model,
                structural_bindings,
                span,
            )
        })
        .transpose()?;
    let coefficient_values = scalarized_coefficient_expressions(
        &term.coefficient,
        &term.target,
        target_keys.len(),
        dae_model,
        structural_bindings,
        span,
    )?;
    if coefficient_values.len() != target_keys.len() {
        return Ok(None);
    }
    if let Some(remainders) = remainder_values.as_ref()
        && remainders.len() != target_keys.len()
    {
        return Ok(None);
    }

    let mut equations = derivative_vec_with_capacity(
        target_keys.len(),
        "affine vector derivative equation rows",
        span,
    )?;
    for (idx, key) in target_keys.into_iter().enumerate() {
        let rhs = match &remainder_values {
            Some(remainders) => {
                sub_with_span(base_values[idx].clone(), remainders[idx].clone(), span)
            }
            None => base_values[idx].clone(),
        };
        let mut coefficients = IndexMap::new();
        reserve_derivative_index_map_capacity(
            &mut coefficients,
            1,
            "affine vector derivative coefficient row",
            span,
        )?;
        coefficients.insert(key, coefficient_values[idx].clone());
        equations.push(DerivativeEquation {
            coefficients,
            rhs,
            span,
            dae_equation_index: None,
        });
    }
    Ok(Some(equations))
}

pub(in crate::lower) fn matrix_vector_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    let Some((lhs, rhs)) = split_subtraction(residual) else {
        return Ok(None);
    };
    if let Some(term) = optional_derivative_probe(matrix_vector_derivative_term(
        lhs,
        state_names,
        dae_model,
        structural_bindings,
        owner_span,
    ))? && !rhs.contains_der()
        && let Some(equations) = optional_derivative_probe(build_matrix_derivative_equations(
            term,
            rhs,
            dae_model,
            structural_bindings,
            owner_span,
        ))?
    {
        return Ok(Some(equations));
    }
    if let Some(term) = optional_derivative_probe(matrix_vector_derivative_term(
        rhs,
        state_names,
        dae_model,
        structural_bindings,
        owner_span,
    ))? && !lhs.contains_der()
        && let Some(equations) = optional_derivative_probe(build_matrix_derivative_equations(
            term,
            lhs,
            dae_model,
            structural_bindings,
            owner_span,
        ))?
    {
        return Ok(Some(equations));
    }
    Ok(None)
}

fn optional_derivative_probe<T>(
    result: Result<Option<T>, LowerError>,
) -> Result<Option<T>, LowerError> {
    match result {
        Ok(value) => Ok(value),
        Err(LowerError::MissingBinding { .. } | LowerError::Unsupported { .. }) => Ok(None),
        Err(LowerError::InvalidFunction { name, .. }) if name == "projected function output" => {
            Ok(None)
        }
        Err(err) => Err(err),
    }
}

pub(in crate::lower) fn matrix_vector_derivative_term(
    expr: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<MatrixDerivativeTerm>, LowerError> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return Ok(None);
    };

    if let Some(target) = pure_der_arg(rhs) {
        return matrix_times_derivative_coefficients(
            lhs,
            target,
            state_names,
            dae_model,
            structural_bindings,
            owner_span,
        );
    }
    if let Some(target) = pure_der_arg(lhs) {
        return derivative_times_matrix_coefficients(
            target,
            rhs,
            state_names,
            dae_model,
            structural_bindings,
            owner_span,
        );
    }
    Ok(None)
}

pub(in crate::lower) fn matrix_times_derivative_coefficients(
    matrix: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<MatrixDerivativeTerm>, LowerError> {
    let target_keys =
        derivative_arg_binding_keys(target, dae_model, structural_bindings, owner_span)?;
    let n = target_keys.len();
    if n == 0 || !target_keys.iter().all(|key| state_names.contains(key)) {
        return Ok(None);
    }
    let matrix_span = derivative_expr_span_or_owner(matrix, owner_span)?;
    let Some(matrix_coefficients) =
        expression_binding_expressions(matrix, dae_model, structural_bindings, matrix_span)?
    else {
        return Ok(None);
    };
    let coefficient_rows =
        matrix_derivative_coefficient_rows(&target_keys, &matrix_coefficients, false, matrix_span)?;
    if coefficient_rows.is_empty() {
        return Ok(None);
    }
    Ok(Some(MatrixDerivativeTerm {
        target: target.clone(),
        coefficient_rows,
    }))
}

pub(in crate::lower) fn derivative_times_matrix_coefficients(
    target: &rumoca_core::Expression,
    matrix: &rumoca_core::Expression,
    state_names: &HashSet<String>,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<MatrixDerivativeTerm>, LowerError> {
    let target_keys =
        derivative_arg_binding_keys(target, dae_model, structural_bindings, owner_span)?;
    let n = target_keys.len();
    if n == 0 || !target_keys.iter().all(|key| state_names.contains(key)) {
        return Ok(None);
    }
    let matrix_span = derivative_expr_span_or_owner(matrix, owner_span)?;
    let Some(matrix_coefficients) =
        expression_binding_expressions(matrix, dae_model, structural_bindings, matrix_span)?
    else {
        return Ok(None);
    };
    let coefficient_rows =
        matrix_derivative_coefficient_rows(&target_keys, &matrix_coefficients, true, matrix_span)?;
    if coefficient_rows.is_empty() {
        return Ok(None);
    }
    Ok(Some(MatrixDerivativeTerm {
        target: target.clone(),
        coefficient_rows,
    }))
}

pub(in crate::lower) fn build_matrix_derivative_equations(
    term: MatrixDerivativeTerm,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Option<Vec<DerivativeEquation>>, LowerError> {
    let rhs_values = scalarized_rhs_expressions_with_owner(
        rhs,
        &term.target,
        term.coefficient_rows.len(),
        dae_model,
        structural_bindings,
        owner_span,
    )?;
    if rhs_values.len() != term.coefficient_rows.len() {
        return Ok(None);
    }
    let span = derivative_expr_span_or_owner(rhs, owner_span)?;
    let mut equations = derivative_vec_with_capacity(
        term.coefficient_rows.len(),
        "matrix derivative equation rows",
        span,
    )?;
    for (coefficients, rhs) in term.coefficient_rows.into_iter().zip(rhs_values) {
        equations.push(DerivativeEquation {
            coefficients,
            rhs,
            span,
            dae_equation_index: None,
        });
    }
    Ok(Some(equations))
}

fn matrix_derivative_coefficient_rows(
    target_keys: &[String],
    matrix_coefficients: &[rumoca_core::Expression],
    transpose: bool,
    span: rumoca_core::Span,
) -> Result<Vec<IndexMap<String, rumoca_core::Expression>>, LowerError> {
    let n = target_keys.len();
    let expected = n.checked_mul(n).ok_or_else(|| {
        LowerError::contract_violation(
            "matrix derivative coefficient count overflows host index range",
            span,
        )
    })?;
    if matrix_coefficients.len() != expected {
        return Ok(Vec::new());
    }

    let mut coefficient_rows =
        derivative_vec_with_capacity(n, "matrix derivative coefficient rows", span)?;
    for outer in 0..n {
        let mut coefficients = IndexMap::new();
        reserve_derivative_index_map_capacity(
            &mut coefficients,
            n,
            "matrix derivative coefficient row",
            span,
        )?;
        for (inner, target_key) in target_keys.iter().enumerate() {
            let (row, col) = if transpose {
                (inner, outer)
            } else {
                (outer, inner)
            };
            let index = checked_matrix_coefficient_index(row, col, n, span)?;
            coefficients.insert(target_key.clone(), matrix_coefficients[index].clone());
        }
        coefficient_rows.push(coefficients);
    }
    Ok(coefficient_rows)
}

fn checked_matrix_coefficient_index(
    row: usize,
    col: usize,
    n: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    row.checked_mul(n)
        .and_then(|offset| offset.checked_add(col))
        .ok_or_else(|| {
            LowerError::contract_violation(
                "matrix derivative coefficient index overflows host index range",
                span,
            )
        })
}

pub(in crate::lower) fn pure_der_arg(
    expr: &rumoca_core::Expression,
) -> Option<&rumoca_core::Expression> {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    (*function == rumoca_core::BuiltinFunction::Der)
        .then(|| args.first())
        .flatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("equation_collection_test"),
            start,
            end,
        )
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

    fn literal(value: i64, span: rumoca_core::Span) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        }
    }

    #[test]
    fn reserve_derivative_vec_capacity_dummy_span_stays_unspanned() {
        let mut values = Vec::<DerivativeEquation>::new();
        let err = reserve_derivative_vec_capacity(
            &mut values,
            usize::MAX,
            "derivative equation rows",
            rumoca_core::Span::DUMMY,
        )
        .expect_err("impossible derivative row capacity must be rejected");

        assert!(
            matches!(err, LowerError::UnspannedContractViolation { .. }),
            "dummy derivative row capacity span should stay unspanned: {err:?}"
        );
        assert!(err.reason().contains("capacity overflows"));
    }

    #[test]
    fn checked_matrix_coefficient_index_preserves_real_span() {
        let real_span = span(4, 12);
        let err = checked_matrix_coefficient_index(usize::MAX, 0, 2, real_span)
            .expect_err("overflowing matrix coefficient index must fail");

        assert!(
            matches!(err, LowerError::ContractViolation { span: err_span, .. } if err_span == real_span),
            "real matrix coefficient span should be preserved: {err:?}"
        );
    }

    #[test]
    fn direct_assignment_target_declines_dynamic_subscript() -> Result<(), LowerError> {
        let expr = var_ref(
            "x",
            vec![rumoca_core::Subscript::Expr {
                expr: Box::new(var_ref("i", Vec::new(), span(1, 2))),
                span: span(3, 4),
            }],
            span(0, 4),
        );

        assert!(direct_assignment_target(&expr)?.is_none());
        Ok(())
    }

    #[test]
    fn direct_assignment_target_declines_slice_subscript() -> Result<(), LowerError> {
        let expr = var_ref(
            "x",
            vec![rumoca_core::Subscript::Colon { span: span(3, 4) }],
            span(0, 4),
        );

        assert!(direct_assignment_target(&expr)?.is_none());
        Ok(())
    }

    #[test]
    fn direct_assignment_target_bubbles_invalid_static_subscript_span() {
        let subscript_span = span(3, 4);
        let expr = var_ref(
            "x",
            vec![rumoca_core::Subscript::Index {
                value: 0,
                span: subscript_span,
            }],
            span(0, 4),
        );

        let err = direct_assignment_target(&expr)
            .expect_err("invalid direct-assignment target subscript should fail");
        assert_eq!(err.source_span(), Some(subscript_span));
        assert!(err.reason().contains("non-positive subscript"));
    }

    #[test]
    fn direct_assignment_target_rhs_skips_ineligible_subtraction_side() -> Result<(), LowerError> {
        let invalid = var_ref(
            "x",
            vec![rumoca_core::Subscript::Index {
                value: 0,
                span: span(3, 4),
            }],
            span(0, 4),
        );
        let derivative = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![var_ref("y", Vec::new(), span(6, 7))],
            span: span(5, 8),
        };
        let equation = dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(derivative),
                rhs: Box::new(invalid),
                span: span(0, 8),
            },
            span(0, 8),
            "test",
        );

        assert!(direct_assignment_target_rhs(&equation)?.is_none());
        Ok(())
    }

    #[test]
    fn direct_assignment_target_rhs_reports_invalid_eligible_side() {
        let invalid = var_ref(
            "x",
            vec![rumoca_core::Subscript::Index {
                value: 0,
                span: span(3, 4),
            }],
            span(0, 4),
        );
        let equation = dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(invalid),
                rhs: Box::new(literal(1, span(6, 7))),
                span: span(0, 7),
            },
            span(0, 7),
            "test",
        );

        let err = direct_assignment_target_rhs(&equation)
            .expect_err("invalid eligible direct-assignment target should fail");
        assert_eq!(err.source_span(), Some(span(3, 4)));
    }

    #[test]
    fn direct_assignment_collection_uses_projected_function_residual() -> Result<(), LowerError> {
        let owner_span = span(0, 10);
        let mut function = rumoca_core::Function::new("My.constant", owner_span);
        function
            .outputs
            .push(rumoca_core::FunctionParam::new("y", "Real", owner_span));
        function.body.push(rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference {
                local: false,
                span: owner_span,
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: "y".to_string(),
                    span: owner_span,
                    subs: Vec::new(),
                }],
                def_id: None,
            },
            value: literal(2, owner_span),
            span: owner_span,
        });

        let call = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("My.constant"),
            args: Vec::new(),
            is_constructor: false,
            span: owner_span,
        };
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable {
                name: rumoca_core::VarName::new("x"),
                origin: dae::VariableOrigin::Generated,
                ..dae::Variable::empty_with_span(owner_span)
            },
        );
        dae_model
            .symbols
            .functions
            .insert(function.name.clone(), function);
        dae_model.continuous.equations.push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref("x", Vec::new(), owner_span)),
                rhs: Box::new(call),
                span: owner_span,
            },
            owner_span,
            "test",
        ));

        let (_, equation_flags) =
            collect_derivative_equations(&dae_model, &HashSet::new(), &IndexMap::new())?;
        assert_eq!(equation_flags, vec![false]);

        let assignments =
            collect_direct_assignments(&dae_model, &equation_flags, &IndexMap::new())?;
        let assignment = assignments.get("x").expect("x assignment");
        assert!(
            matches!(
                &assignment.rhs,
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    ..
                }
            ),
            "projected function result should replace the original call: {:?}",
            assignment.rhs
        );
        Ok(())
    }
}
