use super::*;
use crate::lower::{function_projection::format_subscript_binding_key, unsupported_at};

pub(super) fn projection_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_projection_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_projection_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn reserve_projection_name_capacity(
    names: &mut IndexMap<String, ()>,
    additional: usize,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    names.try_reserve(additional).map_err(|_| {
        LowerError::contract_violation(
            "projection scope name count capacity exceeds host memory limits",
            span,
        )
    })
}

pub(super) fn indexed_var_selection(
    expr: &rumoca_core::Expression,
) -> Option<(
    &rumoca_core::Reference,
    &[rumoca_core::Subscript],
    rumoca_core::Span,
)> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } if !subscripts.is_empty() => Some((name, subscripts, *span)),
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            base_subscripts
                .is_empty()
                .then_some((name, subscripts.as_slice(), *span))
        }
        _ => None,
    }
}

pub(super) fn projected_scalar_selection(
    selection: ScalarSelectionCtx<'_>,
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
) -> Result<rumoca_core::Expression, LowerError> {
    if selection.subscripts.len() != selection.dims.len() {
        let dims = format_i64_dims(selection.dims);
        return Err(LowerError::contract_violation(
            format!(
                "projected scalar selection for `{}` has {} subscripts for dimensions {dims}",
                selection.name,
                selection.subscripts.len()
            ),
            selection.span,
        ));
    }
    let count = scalar_count_for_dims(
        selection.dims,
        "projected scalar selection dimensions",
        selection.span,
    )?;
    if count != selection.values.len() {
        return Err(LowerError::contract_violation(
            format!(
                "projected scalar selection for `{}` has {} values for {count} scalar slots",
                selection.name,
                selection.values.len()
            ),
            selection.span,
        ));
    }
    if let Some(indices) =
        static_subscript_indices(selection.subscripts, analysis, scope, selection.span)?
    {
        let flat_index = flat_index_from_indices(
            selection.dims,
            &indices,
            selection.span,
            "projected scalar selection flat index",
        )?
        .ok_or_else(|| {
            let dims = format_i64_dims(selection.dims);
            let indices = format_i64_dims(&indices);
            LowerError::contract_violation(
                format!(
                    "projected scalar selection for `{}` uses out-of-bounds index {indices} for dimensions {dims}",
                    selection.name
                ),
                selection.span,
            )
                })?;
        let value = assigned_projected_scalar_value(
            selection.name,
            selection.dims,
            selection.values,
            flat_index,
            selection.span,
        )?;
        return Ok(value.clone().with_span(selection.span));
    }
    ensure_projected_scalars_assigned(
        selection.name,
        selection.dims,
        selection.values,
        selection.span,
    )?;
    projected_dynamic_scalar_selection(&selection, analysis, scope)
}

pub(super) fn assigned_projected_scalar_value<'a>(
    name: &str,
    dims: &[i64],
    values: &'a [rumoca_core::Expression],
    flat_index: usize,
    span: rumoca_core::Span,
) -> Result<&'a rumoca_core::Expression, LowerError> {
    let value = values
        .get(flat_index)
        .ok_or_else(|| projected_scalar_slot_contract_error(name, flat_index, span))?;
    if matches!(value, rumoca_core::Expression::Empty { .. }) {
        let indices = required_flat_index_to_subscripts(dims, flat_index, span)?;
        let component_name = format_subscript_binding_key(name, &indices);
        return Err(unsupported_at(
            format!("projected local component `{component_name}` is unassigned"),
            span,
        ));
    }
    Ok(value)
}

fn ensure_projected_scalars_assigned(
    name: &str,
    dims: &[i64],
    values: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    for flat_index in 0..values.len() {
        assigned_projected_scalar_value(name, dims, values, flat_index, span)?;
    }
    Ok(())
}

fn projected_scalar_slot_contract_error(
    name: &str,
    flat_index: usize,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::contract_violation(
        format!("projected scalar selection for `{name}` flat index {flat_index} is missing"),
        span,
    )
}

pub(super) fn projected_dynamic_scalar_selection(
    selection: &ScalarSelectionCtx<'_>,
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
) -> Result<rumoca_core::Expression, LowerError> {
    let Some((last_index, last_value)) = selection.values.iter().enumerate().next_back() else {
        return Err(LowerError::contract_violation(
            "projected dynamic scalar selection has no values",
            selection.span,
        ));
    };
    let mut merged = last_value.clone().with_span(selection.span);
    for (flat_index, value) in selection.values.iter().enumerate().take(last_index).rev() {
        let indices =
            required_flat_index_to_subscripts(selection.dims, flat_index, selection.span)?;
        let condition = subscript_match_condition(
            selection.subscripts,
            &indices,
            selection.span,
            analysis,
            scope,
            selection.depth,
        )?;
        merged = rumoca_core::Expression::If {
            branches: single_projection_branch(
                condition,
                value.clone().with_span(selection.span),
                selection.span,
            )?,
            else_branch: Box::new(merged),
            span: selection.span,
        };
    }
    Ok(merged)
}

pub(super) fn static_subscript_indices(
    subscripts: &[rumoca_core::Subscript],
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let mut indices =
        projection_vec_with_capacity(subscripts.len(), "static subscript index count", span)?;
    for subscript in subscripts {
        let Some(index) = (match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Some(*value),
            rumoca_core::Subscript::Expr { expr, .. } => {
                let expr = analysis.substitute(expr, scope)?;
                let value = match analysis.compile_time_scalar(&expr) {
                    Some(value) => value,
                    None => return Ok(None),
                };
                positive_i64_from_compile_time_scalar(value)
            }
            _ => None,
        }) else {
            return Ok(None);
        };
        indices.push(index);
    }
    Ok(Some(indices))
}

fn positive_i64_from_compile_time_scalar(value: f64) -> Option<i64> {
    let rounded = value.round();
    if !rounded.is_finite()
        || rounded <= 0.0
        || (value - rounded).abs() >= f64::EPSILON
        || rounded >= i64::MAX as f64
    {
        return None;
    }
    // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
    Some(rounded as i64)
}

pub(super) fn subscript_match_condition(
    subscripts: &[rumoca_core::Subscript],
    indices: &[usize],
    span: rumoca_core::Span,
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
    depth: usize,
) -> Result<rumoca_core::Expression, LowerError> {
    let mut conditions =
        projection_vec_with_capacity(indices.len(), "projected selection condition count", span)?;
    for (subscript, index) in subscripts.iter().zip(indices.iter().copied()) {
        let selector = subscript_selector_expr(subscript, analysis, scope, depth)?;
        let index = checked_usize_to_i64(index, "projected scalar selection index", span)?;
        let expected = rumoca_core::Expression::Literal {
            value: Literal::Integer(index),
            span,
        };
        conditions.push(rumoca_core::Expression::Binary {
            op: OpBinary::Eq,
            lhs: Box::new(selector),
            rhs: Box::new(expected),
            span,
        });
    }
    conditions
        .into_iter()
        .reduce(|lhs, rhs| rumoca_core::Expression::Binary {
            op: OpBinary::And,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        })
        .ok_or_else(|| {
            LowerError::contract_violation(
                "projected dynamic scalar selection has no subscript conditions",
                span,
            )
        })
}

pub(super) fn subscript_selector_expr(
    subscript: &rumoca_core::Subscript,
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
    depth: usize,
) -> Result<rumoca_core::Expression, LowerError> {
    match subscript {
        rumoca_core::Subscript::Index { value, span } => Ok(rumoca_core::Expression::Literal {
            value: Literal::Integer(*value),
            span: *span,
        }),
        rumoca_core::Subscript::Expr { expr, .. } => {
            Ok(analysis.scalar_assignment_value(expr, scope, depth + 1)?)
        }
        rumoca_core::Subscript::Colon { span } => Err(unsupported_at(
            "colon subscript cannot select a scalar projected value",
            *span,
        )),
    }
}

pub(super) fn is_function_output_target(function: &rumoca_core::Function, target: &str) -> bool {
    function.outputs.iter().any(|output| {
        target == output.name || target.starts_with(format!("{}.", output.name).as_str())
    })
}

pub(super) fn projection_scope_names(
    entry_scope: &FunctionProjectionScope,
    branch_scopes: &[FunctionProjectionScope],
    else_scope: &FunctionProjectionScope,
    span: rumoca_core::Span,
) -> Result<Vec<String>, LowerError> {
    let mut names = IndexMap::<String, ()>::new();
    insert_projection_scope_names(&mut names, entry_scope, span)?;
    for scope in branch_scopes {
        insert_projection_scope_names(&mut names, scope, span)?;
    }
    insert_projection_scope_names(&mut names, else_scope, span)?;
    let mut ordered =
        projection_vec_with_capacity(names.len(), "projection scope name count", span)?;
    for name in names.into_keys() {
        ordered.push(name);
    }
    Ok(ordered)
}

pub(super) fn insert_projection_scope_names(
    names: &mut IndexMap<String, ()>,
    scope: &FunctionProjectionScope,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    for name in scope.full.keys() {
        reserve_projection_name_capacity(names, 1, span)?;
        names.insert(name.clone(), ());
    }
    for name in scope.scalars.keys() {
        reserve_projection_name_capacity(names, 1, span)?;
        names.insert(name.clone(), ());
    }
    for name in scope.dims.keys() {
        reserve_projection_name_capacity(names, 1, span)?;
        names.insert(name.clone(), ());
    }
    Ok(())
}

pub(super) fn projection_scope_has_scalars(
    name: &str,
    entry_scope: &FunctionProjectionScope,
    branch_scopes: &[FunctionProjectionScope],
    else_scope: &FunctionProjectionScope,
) -> bool {
    entry_scope.scalars.contains_key(name)
        || else_scope.scalars.contains_key(name)
        || branch_scopes
            .iter()
            .any(|scope| scope.scalars.contains_key(name))
}

pub(super) fn projection_scope_has_full(
    name: &str,
    entry_scope: &FunctionProjectionScope,
    branch_scopes: &[FunctionProjectionScope],
    else_scope: &FunctionProjectionScope,
) -> bool {
    entry_scope.full.contains_key(name)
        || else_scope.full.contains_key(name)
        || branch_scopes
            .iter()
            .any(|scope| scope.full.contains_key(name))
}

pub(super) fn merged_projection_dims(
    name: &str,
    entry_scope: &FunctionProjectionScope,
    branch_scopes: &[FunctionProjectionScope],
    else_scope: &FunctionProjectionScope,
    span: rumoca_core::Span,
) -> Result<Option<Vec<i64>>, LowerError> {
    let mut merged = None;
    merge_projection_dims(name, &mut merged, else_scope.dims.get(name), span)?;
    merge_projection_dims(name, &mut merged, entry_scope.dims.get(name), span)?;
    for branch_scope in branch_scopes {
        merge_projection_dims(name, &mut merged, branch_scope.dims.get(name), span)?;
    }
    Ok(merged)
}

fn merge_projection_dims(
    name: &str,
    merged: &mut Option<Vec<i64>>,
    candidate: Option<&Vec<i64>>,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let Some(candidate) = candidate else {
        return Ok(());
    };
    let Some(existing) = merged else {
        *merged = Some(candidate.clone());
        return Ok(());
    };
    if existing == candidate {
        return Ok(());
    }
    Err(LowerError::contract_violation(
        format!(
            "if-statement projection for `{name}` has mismatched dimensions: {} and {}",
            format_i64_dims(existing),
            format_i64_dims(candidate)
        ),
        span,
    ))
}

pub(super) fn merged_if_full_value(
    name: &str,
    entry_scope: &FunctionProjectionScope,
    branch_conditions: &[rumoca_core::Expression],
    branch_scopes: &[FunctionProjectionScope],
    else_scope: &FunctionProjectionScope,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    let mut merged = else_scope
        .full
        .get(name)
        .or_else(|| entry_scope.full.get(name))
        .cloned()
        .ok_or_else(|| guarded_assignment_without_base(name, span))?;
    for (condition, branch_scope) in branch_conditions.iter().zip(branch_scopes.iter()).rev() {
        let Some(branch_value) = branch_scope.full.get(name) else {
            continue;
        };
        merged = rumoca_core::Expression::If {
            branches: single_projection_branch(condition.clone(), branch_value.clone(), span)?,
            else_branch: Box::new(merged),
            span,
        };
    }
    Ok(merged)
}

pub(super) fn single_projection_branch(
    condition: rumoca_core::Expression,
    value: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Result<Vec<(rumoca_core::Expression, rumoca_core::Expression)>, LowerError> {
    let mut branches = projection_vec_with_capacity(1, "projection branch count", span)?;
    branches.push((condition, value));
    Ok(branches)
}

pub(super) fn guarded_assignment_without_base(name: &str, span: rumoca_core::Span) -> LowerError {
    unsupported_at(
        format!(
            "if-statement assignment to `{name}` requires an existing binding or an else assignment"
        ),
        span,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("projection_selection_test"),
            start,
            end,
        )
    }

    fn unspanned_projection_selection_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn reserve_projection_capacity_dummy_span_stays_unspanned() {
        let mut values = Vec::<rumoca_core::Expression>::new();
        let err = reserve_projection_capacity(
            &mut values,
            usize::MAX,
            "projection values",
            unspanned_projection_selection_test_span(),
        )
        .expect_err("impossible capacity must be rejected");

        assert!(
            matches!(err, LowerError::UnspannedContractViolation { .. }),
            "dummy capacity span should not be fabricated into a source span: {err:?}"
        );
        assert!(
            err.to_string()
                .contains("projection values capacity exceeds host memory limits"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn reserve_projection_name_capacity_real_span_is_preserved() {
        let real_span = span(4, 12);
        let mut names = IndexMap::new();
        let err = reserve_projection_name_capacity(&mut names, usize::MAX, real_span)
            .expect_err("impossible name capacity must be rejected");

        assert!(
            matches!(err, LowerError::ContractViolation { span: err_span, .. } if err_span == real_span),
            "real projection name span should be preserved: {err:?}"
        );
    }
}
