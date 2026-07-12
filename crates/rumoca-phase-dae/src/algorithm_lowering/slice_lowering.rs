use super::*;

struct SupportedSlicePlan {
    fixed_selectors: Vec<Option<(Expression, Span)>>,
    pass_axis: usize,
    pass_dim: i64,
    pass_span: Span,
}

fn supported_slice_plan(
    subscripts: &[Subscript],
    dims: &[i64],
    flat: &Model,
) -> Option<SupportedSlicePlan> {
    if subscripts.len() != dims.len() || subscripts.is_empty() {
        return None;
    }

    let mut fixed_selectors = Vec::with_capacity(subscripts.len());
    let mut pass_axis = None;
    let mut pass_span = None;
    for (axis, (subscript, dim)) in subscripts.iter().zip(dims.iter()).enumerate() {
        if is_full_dimension_subscript(subscript, *dim, flat) {
            if pass_axis.is_some() {
                return None;
            }
            pass_axis = Some(axis);
            pass_span = Some(subscript.span());
            fixed_selectors.push(None);
            continue;
        }
        fixed_selectors.push(Some(scalar_subscript_expr(subscript)?));
    }

    let pass_axis = pass_axis?;
    let pass_span = pass_span?;
    Some(SupportedSlicePlan {
        fixed_selectors,
        pass_axis,
        pass_dim: dims[pass_axis],
        pass_span,
    })
}

fn is_full_dimension_subscript(subscript: &Subscript, dim: i64, flat: &Model) -> bool {
    match subscript {
        Subscript::Colon { .. } => true,
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Range {
                start, step, end, ..
            } => {
                let Some(start_value) = eval_integer_expr(start, flat) else {
                    return false;
                };
                let end_value = eval_integer_expr(end, flat);
                let step_value = step
                    .as_ref()
                    .map(|value| eval_integer_expr(value, flat))
                    .unwrap_or(Some(1));
                start_value == 1 && end_value == Some(dim) && step_value == Some(1)
            }
            _ => false,
        },
        Subscript::Index { .. } => false,
    }
}

fn scalar_subscript_expr(subscript: &Subscript) -> Option<(Expression, Span)> {
    match subscript {
        Subscript::Index { value, span } => Some((
            Expression::Literal {
                value: Literal::Integer(*value),
                span: *span,
            },
            *span,
        )),
        Subscript::Expr { expr, span } => Some(((**expr).clone(), *span)),
        Subscript::Colon { .. } => None,
    }
}

fn materialize_scalar_subscripts(
    plan: &SupportedSlicePlan,
    pass_value: i64,
) -> Option<Vec<Subscript>> {
    plan.fixed_selectors
        .iter()
        .enumerate()
        .map(|(axis, selector)| {
            if axis == plan.pass_axis {
                return generated_index_subscript(
                    pass_value,
                    plan.pass_span,
                    "algorithm slice pass-axis subscript",
                );
            }
            selector
                .as_ref()
                .and_then(|(expr, span)| expr_to_subscript(expr, *span))
        })
        .collect()
}

fn expr_to_subscript(expr: &Expression, context_span: Span) -> Option<Subscript> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            span,
        } if !span.is_dummy() => {
            generated_index_subscript(*value, *span, "algorithm slice literal subscript")
        }
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => generated_index_subscript(
            *value,
            context_span,
            "algorithm slice context literal subscript",
        ),
        _ => generated_expr_subscript(
            Box::new(expr.clone()),
            expr.span().unwrap_or(context_span),
            "algorithm slice expression subscript",
        ),
    }
}

fn generated_index_subscript(index: i64, span: Span, context: &'static str) -> Option<Subscript> {
    Some(Subscript::generated_index_with_provenance(
        index,
        span.require_provenance(context).ok()?,
    ))
}

fn generated_expr_subscript(
    expr: Box<Expression>,
    span: Span,
    context: &'static str,
) -> Option<Subscript> {
    Some(Subscript::generated_expr_with_provenance(
        expr,
        span.require_provenance(context).ok()?,
    ))
}

fn flat_expr_dims(flat: &Model, expr: &Expression) -> Option<Vec<i64>> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let dims = &flat.variables.get(name.var_name())?.dims;
            Some(crate::scalar_inference::apply_subscripts_to_dims(
                dims, subscripts,
            ))
        }
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            if *is_matrix {
                let rows = i64::try_from(elements.len()).ok()?;
                let first = elements.first()?;
                let Expression::Array {
                    elements: inner,
                    is_matrix: false,
                    ..
                } = first
                else {
                    return None;
                };
                let cols = i64::try_from(inner.len()).ok()?;
                Some(vec![rows, cols])
            } else {
                Some(vec![i64::try_from(elements.len()).ok()?])
            }
        }
        _ => None,
    }
}

fn expand_supported_slice_read(flat: &Model, expr: &Expression, span: Span) -> Expression {
    let Expression::Index {
        base, subscripts, ..
    } = expr
    else {
        return expr.clone();
    };
    let Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base.as_ref()
    else {
        return expr.clone();
    };
    if !base_subscripts.is_empty() {
        return expr.clone();
    }

    let Some(dims) = flat_expr_dims(flat, base.as_ref()) else {
        return expr.clone();
    };
    let Some(plan) = supported_slice_plan(subscripts, &dims, flat) else {
        return expr.clone();
    };

    let mut elements = Vec::with_capacity(plan.pass_dim.max(0) as usize);
    for pass_value in 1..=plan.pass_dim {
        let Some(subscripts) = materialize_scalar_subscripts(&plan, pass_value) else {
            return expr.clone();
        };
        elements.push(Expression::VarRef {
            name: name.clone(),
            subscripts,
            span,
        });
    }
    Expression::Array {
        elements,
        is_matrix: false,
        span,
    }
}

fn lower_supported_slice_assignment(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Result<Option<Vec<AlgorithmAssignment>>, String> {
    let Some((base_target, subscripts)) = algorithm_assignment_base_with_subscripts(comp) else {
        return Ok(None);
    };
    let Some(dims) = flat.variables.get(&base_target).map(|var| var.dims.clone()) else {
        return Ok(None);
    };
    let Some(plan) = supported_slice_plan(subscripts, &dims, flat) else {
        return Ok(None);
    };

    let value = expand_supported_slice_read(flat, value, span);
    let Some(value_dims) = flat_expr_dims(flat, &value) else {
        return Ok(None);
    };
    if value_dims.len() != 1 || value_dims[0] != plan.pass_dim {
        return Ok(None);
    }

    let mut lowered = Vec::new();
    for pass_value in 1..=plan.pass_dim {
        let Some(scalar_value) = index_vector_expr(&value, pass_value, span) else {
            return Ok(None);
        };
        let Some(targets) = materialize_assignment_targets(&plan, &dims, pass_value, span) else {
            return Ok(None);
        };
        for (subscripts, guard) in targets {
            let scalar_target = varname_with_subscripts(&base_target, &subscripts);
            let scalar_value = match guard {
                Some(condition) => guarded_dynamic_assignment(
                    &base_target,
                    &subscripts,
                    condition,
                    &scalar_value,
                    span,
                )?,
                None => scalar_value.clone(),
            };
            lowered.push((
                scalar_target,
                scalar_value,
                span,
                "algorithm assignment".to_string(),
            ));
        }
    }
    Ok(Some(lowered))
}

fn materialize_assignment_targets(
    plan: &SupportedSlicePlan,
    dims: &[i64],
    pass_value: i64,
    span: Span,
) -> Option<Vec<(Vec<Subscript>, Option<Expression>)>> {
    let mut targets = vec![(Vec::new(), None)];
    for (axis, selector) in plan.fixed_selectors.iter().enumerate() {
        let options = assignment_axis_options(axis, selector.as_ref(), dims, pass_value, plan)?;
        targets = extend_assignment_targets(targets, options, span);
    }
    Some(targets)
}

fn assignment_axis_options(
    axis: usize,
    selector: Option<&(Expression, Span)>,
    dims: &[i64],
    pass_value: i64,
    plan: &SupportedSlicePlan,
) -> Option<Vec<(Subscript, Option<Expression>)>> {
    if axis == plan.pass_axis {
        return Some(vec![(
            generated_index_subscript(
                pass_value,
                plan.pass_span,
                "algorithm assignment pass-axis subscript",
            )?,
            None,
        )]);
    }

    let (selector, selector_span) = selector?;
    if let Some(index) = literal_integer_expr(selector) {
        return Some(vec![(
            generated_index_subscript(
                index,
                *selector_span,
                "algorithm assignment selector subscript",
            )?,
            None,
        )]);
    }

    let dim = *dims.get(axis)?;
    if dim <= 0 {
        return None;
    }
    (1..=dim)
        .map(|candidate| {
            Some((
                generated_index_subscript(
                    candidate,
                    *selector_span,
                    "algorithm assignment dynamic selector subscript",
                )?,
                Some(dynamic_selector_guard(selector, candidate, *selector_span)),
            ))
        })
        .collect::<Option<Vec<_>>>()
}

fn extend_assignment_targets(
    targets: Vec<(Vec<Subscript>, Option<Expression>)>,
    options: Vec<(Subscript, Option<Expression>)>,
    span: Span,
) -> Vec<(Vec<Subscript>, Option<Expression>)> {
    let mut extended = Vec::with_capacity(targets.len() * options.len());
    for (prefix, prefix_guard) in targets {
        for (subscript, guard) in &options {
            let mut subscripts = prefix.clone();
            subscripts.push(subscript.clone());
            extended.push((
                subscripts,
                combine_guards(prefix_guard.clone(), guard.clone(), span),
            ));
        }
    }
    extended
}

fn literal_integer_expr(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn dynamic_selector_guard(selector: &Expression, candidate: i64, span: Span) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(selector.clone()),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(candidate),
            span,
        }),
        span,
    }
}

fn combine_guards(
    lhs: Option<Expression>,
    rhs: Option<Expression>,
    span: Span,
) -> Option<Expression> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => Some(Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }),
        (Some(expr), None) | (None, Some(expr)) => Some(expr),
        (None, None) => None,
    }
}

fn guarded_dynamic_assignment(
    base_target: &VarName,
    subscripts: &[Subscript],
    condition: Expression,
    value: &Expression,
    span: Span,
) -> Result<Expression, String> {
    Ok(Expression::If {
        branches: vec![(condition, value.clone())],
        else_branch: Box::new(Expression::VarRef {
            name: super::structured_target_reference(base_target, span)
                .map_err(|err| err.to_string())?,
            subscripts: subscripts.to_vec(),
            span,
        }),
        span,
    })
}

fn index_vector_expr(expr: &Expression, index: i64, span: Span) -> Option<Expression> {
    let owner_span = expr.span().unwrap_or(span);
    let subscript =
        generated_index_subscript(index, owner_span, "algorithm vector index expression")?;
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut indexed = subscripts.clone();
            indexed.push(subscript);
            Some(Expression::VarRef {
                name: name.clone(),
                subscripts: indexed,
                span,
            })
        }
        _ => Some(Expression::Index {
            base: Box::new(expr.clone()),
            subscripts: vec![subscript],
            span,
        }),
    }
}

fn lower_index_list_assignment(
    comp: &ComponentReference,
    value: &Expression,
    assignment_span: Span,
) -> Option<Vec<AlgorithmAssignment>> {
    let (base_target, subscripts) = algorithm_assignment_base_with_subscripts(comp)?;
    if subscripts.len() != 1 {
        return None;
    }

    let Subscript::Expr {
        expr: index_expr,
        span: index_span,
    } = &subscripts[0]
    else {
        return None;
    };
    let Expression::Array {
        elements: index_elements,
        is_matrix: false,
        ..
    } = index_expr.as_ref()
    else {
        return None;
    };
    let Expression::Array {
        elements: value_elements,
        is_matrix: false,
        ..
    } = value
    else {
        return None;
    };

    if index_elements.is_empty() || index_elements.len() != value_elements.len() {
        return None;
    }

    index_elements
        .iter()
        .zip(value_elements.iter())
        .map(|(index_element, rhs)| {
            let subscript_span = index_element
                .span()
                .or_else(|| (!index_span.is_dummy()).then_some(*index_span))?;
            Some((
                varname_with_subscripts(
                    &base_target,
                    &[generated_expr_subscript(
                        Box::new(index_element.clone()),
                        subscript_span,
                        "algorithm index-list assignment subscript",
                    )?],
                ),
                rhs.clone(),
                assignment_span,
                "algorithm assignment (index-list lowering)".to_string(),
            ))
        })
        .collect::<Option<Vec<_>>>()
}

fn dynamic_scalar_assignment_targets(
    subscripts: &[Subscript],
    dims: &[i64],
    span: Span,
) -> Option<Vec<(Vec<Subscript>, Option<Expression>)>> {
    if subscripts.len() != dims.len() || subscripts.is_empty() {
        return None;
    }

    let mut has_dynamic_selector = false;
    let mut targets = vec![(Vec::new(), None)];
    for (subscript, dim) in subscripts.iter().zip(dims.iter()) {
        let selector = scalar_subscript_expr(subscript)?;
        let options = dynamic_scalar_axis_options(&selector.0, selector.1, *dim)?;
        has_dynamic_selector |= options.iter().any(|(_, guard)| guard.is_some());
        targets = extend_assignment_targets(targets, options, span);
    }

    has_dynamic_selector.then_some(targets)
}

fn dynamic_scalar_axis_options(
    selector: &Expression,
    selector_span: Span,
    dim: i64,
) -> Option<Vec<(Subscript, Option<Expression>)>> {
    if let Some(index) = literal_integer_expr(selector) {
        return Some(vec![(
            generated_index_subscript(
                index,
                selector_span,
                "algorithm dynamic scalar selector subscript",
            )?,
            None,
        )]);
    }
    if dim <= 0 {
        return None;
    }
    (1..=dim)
        .map(|candidate| {
            Some((
                generated_index_subscript(
                    candidate,
                    selector_span,
                    "algorithm dynamic scalar candidate subscript",
                )?,
                Some(dynamic_selector_guard(selector, candidate, selector_span)),
            ))
        })
        .collect::<Option<Vec<_>>>()
}

fn lower_dynamic_scalar_assignment(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Result<Option<Vec<AlgorithmAssignment>>, String> {
    if flat_expr_dims(flat, value).is_some_and(|dims| !dims.is_empty()) {
        return Ok(None);
    }

    let Some((base_target, subscripts)) = algorithm_assignment_base_with_subscripts(comp) else {
        return Ok(None);
    };
    let Some(dims) = flat.variables.get(&base_target).map(|var| var.dims.clone()) else {
        return Ok(None);
    };
    let Some(targets) = dynamic_scalar_assignment_targets(subscripts, &dims, span) else {
        return Ok(None);
    };
    let mut lowered = Vec::with_capacity(targets.len());
    for (subscripts, guard) in targets {
        let scalar_target = varname_with_subscripts(&base_target, &subscripts);
        let scalar_value = match guard {
            Some(condition) => {
                guarded_dynamic_assignment(&base_target, &subscripts, condition, value, span)?
            }
            None => value.clone(),
        };
        lowered.push((
            scalar_target,
            scalar_value,
            span,
            "algorithm assignment (dynamic scalar index lowering)".to_string(),
        ));
    }
    Ok(Some(lowered))
}

fn primitive_record_children(flat: &Model, target: &VarName) -> Vec<VarName> {
    flat.variables
        .iter()
        .filter(|(name, var)| {
            var.is_primitive
                && name
                    .structural_ancestors()
                    .iter()
                    .any(|ancestor| ancestor == target)
        })
        .map(|(name, _)| name.clone())
        .collect()
}

fn corresponding_record_field_value(
    flat: &Model,
    source: &rumoca_core::Reference,
    target: &VarName,
    target_child: &VarName,
    span: Span,
) -> Option<Expression> {
    let suffix = target_child.as_str().strip_prefix(target.as_str())?;
    if !(suffix.starts_with('.') || suffix.starts_with('[')) {
        return None;
    }

    let source_child = VarName::new(format!("{}{}", source.var_name().as_str(), suffix));
    let source_var = flat.variables.get(&source_child)?;
    if !source_var.is_primitive {
        return None;
    }

    Some(Expression::VarRef {
        name: source_child.into(),
        subscripts: vec![],
        span,
    })
}

fn lower_record_assignment(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Option<Vec<AlgorithmAssignment>> {
    let target = algorithm_assignment_target_name(comp)?;
    let Expression::VarRef {
        name: source,
        subscripts,
        ..
    } = value
    else {
        return None;
    };
    if flat
        .variables
        .get(&target)
        .is_some_and(|var| var.is_primitive)
    {
        return None;
    }
    if !subscripts.is_empty() {
        return None;
    }

    let target_children = primitive_record_children(flat, &target);
    if target_children.is_empty() {
        return None;
    }

    target_children
        .into_iter()
        .map(|target_child| {
            Some((
                target_child.clone(),
                corresponding_record_field_value(flat, source, &target, &target_child, span)?,
                span,
                "algorithm record assignment".to_string(),
            ))
        })
        .collect()
}

pub(super) fn lower_assignment_statement(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Result<Vec<AlgorithmAssignment>, String> {
    if let Some(lowered) = lower_index_list_assignment(comp, value, span) {
        return Ok(lowered);
    }

    if let Some(lowered) = lower_supported_slice_assignment(flat, comp, value, span)? {
        return Ok(lowered);
    }

    if let Some(lowered) = lower_dynamic_scalar_assignment(flat, comp, value, span)? {
        return Ok(lowered);
    }

    if let Some(lowered) = lower_record_assignment(flat, comp, value, span) {
        return Ok(lowered);
    }

    let Some(target) = algorithm_assignment_target_name(comp) else {
        return Err(format!(
            "Assignment Assignment {{ comp: {comp:?}, value: {value:?} }}"
        ));
    };
    Ok(vec![(
        target,
        expand_supported_slice_read(flat, value, span),
        span,
        "algorithm assignment".to_string(),
    )])
}
