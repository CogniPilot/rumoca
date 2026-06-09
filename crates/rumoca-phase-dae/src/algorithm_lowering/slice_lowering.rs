use super::*;

struct SupportedSlicePlan {
    fixed_selectors: Vec<Option<Expression>>,
    pass_axis: usize,
    pass_dim: i64,
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
    for (axis, (subscript, dim)) in subscripts.iter().zip(dims.iter()).enumerate() {
        if is_full_dimension_subscript(subscript, *dim, flat) {
            if pass_axis.is_some() {
                return None;
            }
            pass_axis = Some(axis);
            fixed_selectors.push(None);
            continue;
        }
        fixed_selectors.push(Some(scalar_subscript_expr(subscript)?));
    }

    let pass_axis = pass_axis?;
    Some(SupportedSlicePlan {
        fixed_selectors,
        pass_axis,
        pass_dim: dims[pass_axis],
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

fn scalar_subscript_expr(subscript: &Subscript) -> Option<Expression> {
    match subscript {
        Subscript::Index { value, .. } => Some(Expression::Literal {
            value: Literal::Integer(*value),
            span: rumoca_core::Span::DUMMY,
        }),
        Subscript::Expr { expr, .. } => Some((**expr).clone()),
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
                return Some(Subscript::generated_index(
                    pass_value,
                    rumoca_core::Span::DUMMY,
                ));
            }
            selector.as_ref().map(expr_to_subscript)
        })
        .collect()
}

fn expr_to_subscript(expr: &Expression) -> Subscript {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Subscript::generated_index(*value, rumoca_core::Span::DUMMY),
        _ => Subscript::generated_expr(Box::new(expr.clone())),
    }
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

fn expand_supported_slice_read(flat: &Model, expr: &Expression) -> Expression {
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
            span: rumoca_core::Span::DUMMY,
        });
    }
    Expression::Array {
        elements,
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn lower_supported_slice_assignment(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Option<Vec<AlgorithmAssignment>> {
    let (base_target, subscripts) = algorithm_assignment_base_with_subscripts(comp)?;
    let dims = flat.variables.get(&base_target)?.dims.clone();
    let plan = supported_slice_plan(subscripts, &dims, flat)?;

    let value = expand_supported_slice_read(flat, value);
    let value_dims = flat_expr_dims(flat, &value)?;
    if value_dims.len() != 1 || value_dims[0] != plan.pass_dim {
        return None;
    }

    let mut lowered = Vec::new();
    for pass_value in 1..=plan.pass_dim {
        let scalar_value = index_vector_expr(&value, pass_value);
        let targets = materialize_assignment_targets(&plan, &dims, pass_value)?;
        for (subscripts, guard) in targets {
            let scalar_target = varname_with_subscripts(&base_target, &subscripts);
            let scalar_value = guard
                .map(|condition| {
                    guarded_dynamic_assignment(&base_target, &subscripts, condition, &scalar_value)
                })
                .unwrap_or_else(|| scalar_value.clone());
            lowered.push((
                scalar_target,
                scalar_value,
                span,
                "algorithm assignment".to_string(),
            ));
        }
    }
    Some(lowered)
}

fn materialize_assignment_targets(
    plan: &SupportedSlicePlan,
    dims: &[i64],
    pass_value: i64,
) -> Option<Vec<(Vec<Subscript>, Option<Expression>)>> {
    let mut targets = vec![(Vec::new(), None)];
    for (axis, selector) in plan.fixed_selectors.iter().enumerate() {
        let options = assignment_axis_options(axis, selector.as_ref(), dims, pass_value, plan)?;
        targets = extend_assignment_targets(targets, options);
    }
    Some(targets)
}

fn assignment_axis_options(
    axis: usize,
    selector: Option<&Expression>,
    dims: &[i64],
    pass_value: i64,
    plan: &SupportedSlicePlan,
) -> Option<Vec<(Subscript, Option<Expression>)>> {
    if axis == plan.pass_axis {
        return Some(vec![(
            Subscript::generated_index(pass_value, rumoca_core::Span::DUMMY),
            None,
        )]);
    }

    let selector = selector?;
    if let Some(index) = literal_integer_expr(selector) {
        return Some(vec![(
            Subscript::generated_index(index, rumoca_core::Span::DUMMY),
            None,
        )]);
    }

    let dim = *dims.get(axis)?;
    if dim <= 0 {
        return None;
    }
    Some(
        (1..=dim)
            .map(|candidate| {
                (
                    Subscript::generated_index(candidate, rumoca_core::Span::DUMMY),
                    Some(dynamic_selector_guard(selector, candidate)),
                )
            })
            .collect(),
    )
}

fn extend_assignment_targets(
    targets: Vec<(Vec<Subscript>, Option<Expression>)>,
    options: Vec<(Subscript, Option<Expression>)>,
) -> Vec<(Vec<Subscript>, Option<Expression>)> {
    let mut extended = Vec::with_capacity(targets.len() * options.len());
    for (prefix, prefix_guard) in targets {
        for (subscript, guard) in &options {
            let mut subscripts = prefix.clone();
            subscripts.push(subscript.clone());
            extended.push((
                subscripts,
                combine_guards(prefix_guard.clone(), guard.clone()),
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

fn dynamic_selector_guard(selector: &Expression, candidate: i64) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(selector.clone()),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(candidate),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn combine_guards(lhs: Option<Expression>, rhs: Option<Expression>) -> Option<Expression> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => Some(Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
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
) -> Expression {
    Expression::If {
        branches: vec![(condition, value.clone())],
        else_branch: Box::new(Expression::VarRef {
            name: super::structured_target_reference(base_target),
            subscripts: subscripts.to_vec(),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn index_vector_expr(expr: &Expression, index: i64) -> Expression {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut indexed = subscripts.clone();
            indexed.push(Subscript::generated_index(index, rumoca_core::Span::DUMMY));
            Expression::VarRef {
                name: name.clone(),
                subscripts: indexed,
                span: rumoca_core::Span::DUMMY,
            }
        }
        _ => Expression::Index {
            base: Box::new(expr.clone()),
            subscripts: vec![Subscript::generated_index(index, rumoca_core::Span::DUMMY)],
            span: rumoca_core::Span::DUMMY,
        },
    }
}

fn lower_index_list_assignment(
    comp: &ComponentReference,
    value: &Expression,
) -> Option<Vec<AlgorithmAssignment>> {
    let (base_target, subscripts) = algorithm_assignment_base_with_subscripts(comp)?;
    if subscripts.len() != 1 {
        return None;
    }

    let Subscript::Expr {
        expr: index_expr, ..
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

    Some(
        index_elements
            .iter()
            .zip(value_elements.iter())
            .map(|(index_element, rhs)| {
                (
                    varname_with_subscripts(
                        &base_target,
                        &[Subscript::generated_expr(Box::new(index_element.clone()))],
                    ),
                    rhs.clone(),
                    Span::DUMMY,
                    "algorithm assignment (index-list lowering)".to_string(),
                )
            })
            .collect(),
    )
}

fn dynamic_scalar_assignment_targets(
    subscripts: &[Subscript],
    dims: &[i64],
) -> Option<Vec<(Vec<Subscript>, Option<Expression>)>> {
    if subscripts.len() != dims.len() || subscripts.is_empty() {
        return None;
    }

    let mut has_dynamic_selector = false;
    let mut targets = vec![(Vec::new(), None)];
    for (subscript, dim) in subscripts.iter().zip(dims.iter()) {
        let selector = scalar_subscript_expr(subscript)?;
        let options = dynamic_scalar_axis_options(&selector, *dim)?;
        has_dynamic_selector |= options.iter().any(|(_, guard)| guard.is_some());
        targets = extend_assignment_targets(targets, options);
    }

    has_dynamic_selector.then_some(targets)
}

fn dynamic_scalar_axis_options(
    selector: &Expression,
    dim: i64,
) -> Option<Vec<(Subscript, Option<Expression>)>> {
    if let Some(index) = literal_integer_expr(selector) {
        return Some(vec![(
            Subscript::generated_index(index, rumoca_core::Span::DUMMY),
            None,
        )]);
    }
    if dim <= 0 {
        return None;
    }
    Some(
        (1..=dim)
            .map(|candidate| {
                (
                    Subscript::generated_index(candidate, rumoca_core::Span::DUMMY),
                    Some(dynamic_selector_guard(selector, candidate)),
                )
            })
            .collect(),
    )
}

fn lower_dynamic_scalar_assignment(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Option<Vec<AlgorithmAssignment>> {
    if flat_expr_dims(flat, value).is_some_and(|dims| !dims.is_empty()) {
        return None;
    }

    let (base_target, subscripts) = algorithm_assignment_base_with_subscripts(comp)?;
    let dims = flat.variables.get(&base_target)?.dims.clone();
    let targets = dynamic_scalar_assignment_targets(subscripts, &dims)?;
    Some(
        targets
            .into_iter()
            .map(|(subscripts, guard)| {
                let scalar_target = varname_with_subscripts(&base_target, &subscripts);
                let scalar_value = guard
                    .map(|condition| {
                        guarded_dynamic_assignment(&base_target, &subscripts, condition, value)
                    })
                    .unwrap_or_else(|| value.clone());
                (
                    scalar_target,
                    scalar_value,
                    span,
                    "algorithm assignment (dynamic scalar index lowering)".to_string(),
                )
            })
            .collect(),
    )
}

pub(super) fn lower_assignment_statement(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
    span: Span,
) -> Result<Vec<AlgorithmAssignment>, String> {
    if let Some(lowered) = lower_index_list_assignment(comp, value) {
        return Ok(lowered);
    }

    if let Some(lowered) = lower_supported_slice_assignment(flat, comp, value, span) {
        return Ok(lowered);
    }

    if let Some(lowered) = lower_dynamic_scalar_assignment(flat, comp, value, span) {
        return Ok(lowered);
    }

    let Some(target) = algorithm_assignment_target_name(comp) else {
        return Err(format!(
            "Assignment Assignment {{ comp: {comp:?}, value: {value:?} }}"
        ));
    };
    Ok(vec![(
        target,
        expand_supported_slice_read(flat, value),
        span,
        "algorithm assignment".to_string(),
    )])
}
