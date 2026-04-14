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
        Subscript::Colon => true,
        Subscript::Expr(expr) => match expr.as_ref() {
            Expression::Range { start, step, end } => {
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
        Subscript::Index(_) => false,
    }
}

fn scalar_subscript_expr(subscript: &Subscript) -> Option<Expression> {
    match subscript {
        Subscript::Index(value) => Some(Expression::Literal(Literal::Integer(*value))),
        Subscript::Expr(expr) => Some((**expr).clone()),
        Subscript::Colon => None,
    }
}

fn materialize_scalar_subscripts(plan: &SupportedSlicePlan, pass_value: i64) -> Vec<Subscript> {
    plan.fixed_selectors
        .iter()
        .enumerate()
        .map(|(axis, selector)| {
            if axis == plan.pass_axis {
                return Subscript::Index(pass_value);
            }
            expr_to_subscript(selector.as_ref().expect("fixed selector missing"))
        })
        .collect()
}

fn expr_to_subscript(expr: &Expression) -> Subscript {
    match expr {
        Expression::Literal(Literal::Integer(value)) => Subscript::Index(*value),
        _ => Subscript::Expr(Box::new(expr.clone())),
    }
}

fn flat_expr_dims(flat: &Model, expr: &Expression) -> Option<Vec<i64>> {
    match expr {
        Expression::VarRef { name, subscripts } => {
            let dims = &flat.variables.get(name)?.dims;
            Some(crate::scalar_inference::apply_subscripts_to_dims(
                dims, subscripts,
            ))
        }
        Expression::Array {
            elements,
            is_matrix,
        } => {
            if *is_matrix {
                let rows = i64::try_from(elements.len()).ok()?;
                let first = elements.first()?;
                let Expression::Array {
                    elements: inner,
                    is_matrix: false,
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
    let Expression::Index { base, subscripts } = expr else {
        return expr.clone();
    };
    let Expression::VarRef {
        name,
        subscripts: base_subscripts,
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
        elements.push(Expression::VarRef {
            name: name.clone(),
            subscripts: materialize_scalar_subscripts(&plan, pass_value),
        });
    }
    Expression::Array {
        elements,
        is_matrix: false,
    }
}

fn lower_supported_slice_assignment(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
) -> Option<Vec<AlgorithmAssignment>> {
    let (base_target, subscripts) = algorithm_assignment_base_with_subscripts(comp)?;
    let dims = flat.variables.get(&base_target)?.dims.clone();
    let plan = supported_slice_plan(subscripts, &dims, flat)?;

    let value = expand_supported_slice_read(flat, value);
    let value_dims = flat_expr_dims(flat, &value)?;
    if value_dims.len() != 1 || value_dims[0] != plan.pass_dim {
        return None;
    }

    let mut lowered = Vec::with_capacity(plan.pass_dim.max(0) as usize);
    for pass_value in 1..=plan.pass_dim {
        let scalar_target = varref_with_subscripts(
            &base_target,
            &materialize_scalar_subscripts(&plan, pass_value),
        );
        let scalar_value = index_vector_expr(&value, pass_value);
        lowered.push((
            scalar_target,
            scalar_value,
            Span::DUMMY,
            "algorithm assignment".to_string(),
        ));
    }
    Some(lowered)
}

fn index_vector_expr(expr: &Expression, index: i64) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts } => {
            let mut indexed = subscripts.clone();
            indexed.push(Subscript::Index(index));
            Expression::VarRef {
                name: name.clone(),
                subscripts: indexed,
            }
        }
        _ => Expression::Index {
            base: Box::new(expr.clone()),
            subscripts: vec![Subscript::Index(index)],
        },
    }
}

pub(super) fn lower_assignment_statement(
    flat: &Model,
    comp: &ComponentReference,
    value: &Expression,
) -> Result<Vec<AlgorithmAssignment>, String> {
    if let Some(lowered) = lower_supported_slice_assignment(flat, comp, value) {
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
        Span::DUMMY,
        "algorithm assignment".to_string(),
    )])
}
