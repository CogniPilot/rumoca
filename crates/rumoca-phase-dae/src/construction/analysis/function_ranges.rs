use super::*;

pub(super) fn immutable_integer_defaults(
    function: &rumoca_core::Function,
    shapes: &ShapeEnvironment,
) -> Result<HashMap<VarName, i64>, ToDaeError> {
    let mut assigned = HashSet::new();
    collect_function_assignment_targets(&function.body, &mut assigned);
    let candidates = function
        .locals
        .iter()
        .filter(|local| local.type_name == "Integer" && !assigned.contains(&local.name))
        .collect::<Vec<_>>();
    let mut values = HashMap::with_capacity(candidates.len());
    loop {
        let mut progress = false;
        for local in &candidates {
            let name = VarName::new(&local.name);
            if values.contains_key(&name) {
                continue;
            }
            let Some(default) = &local.default else {
                continue;
            };
            let Some(value) = static_shape_integer_expression(default, &values, shapes)? else {
                continue;
            };
            values.insert(name, value);
            progress = true;
        }
        if !progress {
            break;
        }
    }
    Ok(values)
}

fn collect_function_assignment_targets(
    statements: &[rumoca_core::Statement],
    assigned: &mut HashSet<String>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } => {
                if let Some(target) = comp.parts.last() {
                    assigned.insert(target.ident.clone());
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_function_assignment_targets(equations, assigned);
            }
            _ => {}
        }
    }
}

pub(super) fn static_function_range(
    expression: &Expression,
    values: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<(i64, i64, i64)>, ToDaeError> {
    let Expression::Range {
        start, step, end, ..
    } = expression
    else {
        return Ok(None);
    };
    let evaluate = |expression| static_shape_integer_expression(expression, values, shapes);
    let Some(lower) = evaluate(start)? else {
        return Ok(None);
    };
    let Some(step) = step
        .as_deref()
        .map(evaluate)
        .transpose()?
        .unwrap_or(Some(1))
    else {
        return Ok(None);
    };
    let Some(upper) = evaluate(end)? else {
        return Ok(None);
    };
    Ok((step != 0).then_some((lower, step, upper)))
}

fn static_shape_integer_expression(
    expression: &Expression,
    values: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<i64>, ToDaeError> {
    if let Some(value) = static_integer_expression(expression, values) {
        return Ok(Some(value));
    }
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Size,
            ..
        } => evaluate_shape_integer(expression, shapes).map(Some),
        Expression::Unary { op, rhs, .. }
            if matches!(
                op,
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Minus | OpUnary::DotMinus
            ) =>
        {
            let Some(value) = static_shape_integer_expression(rhs, values, shapes)? else {
                return Ok(None);
            };
            Ok(match op {
                OpUnary::Minus | OpUnary::DotMinus => value.checked_neg(),
                _ => Some(value),
            })
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let Some(lhs) = static_shape_integer_expression(lhs, values, shapes)? else {
                return Ok(None);
            };
            let Some(rhs) = static_shape_integer_expression(rhs, values, shapes)? else {
                return Ok(None);
            };
            Ok(checked_static_integer_binary(op, lhs, rhs))
        }
        _ => Ok(None),
    }
}

pub(super) fn validate_function_range_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    let Expression::Range {
        start, step, end, ..
    } = expression
    else {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a checked function loop requires an explicit range expression",
            expression_span(expression)?,
        ));
    };
    validate_function_expression_with_roles(start, roles, flat)?;
    if let Some(step) = step {
        validate_function_expression_with_roles(step, roles, flat)?;
    }
    validate_function_expression_with_roles(end, roles, flat)
}

pub(in crate::construction) fn static_integer_expression(
    expression: &Expression,
    values: &HashMap<VarName, i64>,
) -> Option<i64> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => values.get(name.var_name()).copied(),
        Expression::Unary {
            op: OpUnary::Plus | OpUnary::DotPlus,
            rhs,
            ..
        } => static_integer_expression(rhs, values),
        Expression::Unary {
            op: OpUnary::Minus | OpUnary::DotMinus,
            rhs,
            ..
        } => static_integer_expression(rhs, values)?.checked_neg(),
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = static_integer_expression(lhs, values)?;
            let rhs = static_integer_expression(rhs, values)?;
            checked_static_integer_binary(op, lhs, rhs)
        }
        _ => None,
    }
}

fn checked_static_integer_binary(operator: &OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    match operator {
        OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
        OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
        OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
        OpBinary::Div | OpBinary::DivElem if rhs != 0 && lhs % rhs == 0 => Some(lhs / rhs),
        _ => None,
    }
}
