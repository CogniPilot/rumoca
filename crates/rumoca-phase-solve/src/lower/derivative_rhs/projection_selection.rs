use super::*;

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
        return Err(LowerError::ContractViolation {
            reason: format!(
                "projected scalar selection for `{}` has {} subscripts for dimensions {:?}",
                selection.name,
                selection.subscripts.len(),
                selection.dims
            ),
            span: selection.span,
        });
    }
    let count =
        scalar_count_for_dims(selection.dims).ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "projected scalar selection for `{}` has invalid dimensions {:?}",
                selection.name, selection.dims
            ),
            span: selection.span,
        })?;
    if count != selection.values.len() {
        return Err(LowerError::ContractViolation {
            reason: format!(
                "projected scalar selection for `{}` has {} values for {count} scalar slots",
                selection.name,
                selection.values.len()
            ),
            span: selection.span,
        });
    }
    if let Some(indices) = static_subscript_indices(selection.subscripts, analysis, scope) {
        let flat_index = flat_index_from_indices(selection.dims, &indices).ok_or_else(|| {
            LowerError::ContractViolation {
                reason: format!(
                    "projected scalar selection for `{}` uses out-of-bounds index {indices:?} for dimensions {:?}",
                    selection.name, selection.dims
                ),
                span: selection.span,
            }
        })?;
        let value =
            selection
                .values
                .get(flat_index)
                .ok_or_else(|| LowerError::ContractViolation {
                    reason: format!(
                        "projected scalar selection for `{}` flat index {flat_index} is missing",
                        selection.name
                    ),
                    span: selection.span,
                })?;
        return Ok(value.clone().with_span(selection.span));
    }
    projected_dynamic_scalar_selection(&selection, analysis, scope)
}

pub(super) fn projected_dynamic_scalar_selection(
    selection: &ScalarSelectionCtx<'_>,
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
) -> Result<rumoca_core::Expression, LowerError> {
    let Some((last_index, last_value)) = selection.values.iter().enumerate().next_back() else {
        return Err(LowerError::ContractViolation {
            reason: "projected dynamic scalar selection has no values".to_string(),
            span: selection.span,
        });
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
            branches: vec![(condition, value.clone().with_span(selection.span))],
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
) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Some(*value),
            rumoca_core::Subscript::Expr { expr, .. } => {
                let expr = analysis.substitute(expr, scope);
                let value = analysis.compile_time_scalar(&expr)?;
                let index = value as i64;
                (index > 0 && (value - index as f64).abs() < f64::EPSILON).then_some(index)
            }
            _ => None,
        })
        .collect()
}

pub(super) fn subscript_match_condition(
    subscripts: &[rumoca_core::Subscript],
    indices: &[usize],
    span: rumoca_core::Span,
    analysis: &FunctionProjectionAnalysis<'_>,
    scope: &FunctionProjectionScope,
    depth: usize,
) -> Result<rumoca_core::Expression, LowerError> {
    let mut conditions = Vec::with_capacity(indices.len());
    for (subscript, index) in subscripts.iter().zip(indices.iter().copied()) {
        let selector = subscript_selector_expr(subscript, analysis, scope, depth)?;
        let expected = rumoca_core::Expression::Literal {
            value: Literal::Integer(index as i64),
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
        .ok_or_else(|| LowerError::ContractViolation {
            reason: "projected dynamic scalar selection has no subscript conditions".to_string(),
            span,
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
        rumoca_core::Subscript::Colon { span } => Err(LowerError::Unsupported {
            reason: "colon subscript cannot select a scalar projected value".to_string(),
        }
        .with_fallback_span(*span)),
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
) -> Vec<String> {
    let mut names = IndexMap::<String, ()>::new();
    insert_projection_scope_names(&mut names, entry_scope);
    for scope in branch_scopes {
        insert_projection_scope_names(&mut names, scope);
    }
    insert_projection_scope_names(&mut names, else_scope);
    names.into_keys().collect()
}

pub(super) fn insert_projection_scope_names(
    names: &mut IndexMap<String, ()>,
    scope: &FunctionProjectionScope,
) {
    for name in scope.full.keys() {
        names.insert(name.clone(), ());
    }
    for name in scope.scalars.keys() {
        names.insert(name.clone(), ());
    }
    for name in scope.dims.keys() {
        names.insert(name.clone(), ());
    }
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
) -> Option<Vec<i64>> {
    else_scope
        .dims
        .get(name)
        .or_else(|| entry_scope.dims.get(name))
        .or_else(|| branch_scopes.iter().find_map(|scope| scope.dims.get(name)))
        .cloned()
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
            branches: vec![(condition.clone(), branch_value.clone())],
            else_branch: Box::new(merged),
            span,
        };
    }
    Ok(merged)
}

pub(super) fn guarded_assignment_without_base(name: &str, span: rumoca_core::Span) -> LowerError {
    LowerError::Unsupported {
        reason: format!(
            "if-statement assignment to `{name}` requires an existing binding or an else assignment"
        ),
    }
    .with_fallback_span(span)
}
