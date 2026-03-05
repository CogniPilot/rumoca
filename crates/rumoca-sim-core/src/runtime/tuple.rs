use crate::runtime::assignment::canonical_var_ref_key;
use rumoca_eval_flat::eval::VarEnv;
use rumoca_ir_dae as dae;

#[derive(Clone, Debug)]
pub struct TupleAssignmentTarget {
    pub key: String,
    pub base_name: String,
    pub suffix: String,
}

#[derive(Clone, Debug)]
pub struct TupleFunctionAssignment<'a> {
    pub targets: Vec<TupleAssignmentTarget>,
    pub solution: &'a dae::Expression,
}

fn canonical_subscript_suffix(subscripts: &[dae::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(String::new());
    }
    let mut index_parts = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let idx = match sub {
            dae::Subscript::Index(i) => *i,
            dae::Subscript::Expr(expr) => match expr.as_ref() {
                dae::Expression::Literal(dae::Literal::Integer(i)) => *i,
                dae::Expression::Literal(dae::Literal::Real(v))
                    if v.is_finite() && v.fract() == 0.0 =>
                {
                    *v as i64
                }
                _ => return None,
            },
            _ => return None,
        };
        index_parts.push(idx.to_string());
    }
    Some(format!("[{}]", index_parts.join(",")))
}

fn tuple_assignment_target_from_expr(expr: &dae::Expression) -> Option<TupleAssignmentTarget> {
    let dae::Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    Some(TupleAssignmentTarget {
        key: canonical_var_ref_key(name, subscripts)?,
        base_name: name.as_str().to_string(),
        suffix: canonical_subscript_suffix(subscripts)?,
    })
}

fn tuple_function_assignment_from_terms<'a>(
    tuple_expr: &'a dae::Expression,
    solution_expr: &'a dae::Expression,
) -> Option<TupleFunctionAssignment<'a>> {
    let dae::Expression::Tuple { elements } = tuple_expr else {
        return None;
    };
    if !matches!(solution_expr, dae::Expression::FunctionCall { .. }) {
        return None;
    }

    let mut targets = Vec::with_capacity(elements.len());
    for element in elements {
        targets.push(tuple_assignment_target_from_expr(element)?);
    }
    Some(TupleFunctionAssignment {
        targets,
        solution: solution_expr,
    })
}

pub fn extract_direct_tuple_function_assignment(
    rhs: &dae::Expression,
) -> Option<TupleFunctionAssignment<'_>> {
    match rhs {
        dae::Expression::Binary {
            op: dae::OpBinary::Sub(_),
            lhs,
            rhs,
        } => tuple_function_assignment_from_terms(lhs, rhs)
            .or_else(|| tuple_function_assignment_from_terms(rhs, lhs)),
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_),
            rhs,
        } => extract_direct_tuple_function_assignment(rhs),
        _ => None,
    }
}

fn extract_active_tuple_assignment_from_expr<'a, FEvalCondition>(
    expr: &'a dae::Expression,
    env: &VarEnv<f64>,
    eval_condition: FEvalCondition,
) -> Option<TupleFunctionAssignment<'a>>
where
    FEvalCondition: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> bool,
{
    if let Some(assignment) = extract_direct_tuple_function_assignment(expr) {
        return Some(assignment);
    }
    match expr {
        dae::Expression::Unary {
            op: dae::OpUnary::Minus(_),
            rhs,
        } => extract_active_tuple_assignment_from_expr(rhs, env, eval_condition),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                if eval_condition(condition, env) {
                    return extract_active_tuple_assignment_from_expr(value, env, eval_condition);
                }
            }
            extract_active_tuple_assignment_from_expr(else_branch, env, eval_condition)
        }
        _ => None,
    }
}

fn extract_active_discrete_tuple_function_assignment<'a, FEvalCondition, FIsZeroLiteral>(
    residual: &'a dae::Expression,
    env: &VarEnv<f64>,
    eval_condition: FEvalCondition,
    is_zero_literal: FIsZeroLiteral,
) -> Option<TupleFunctionAssignment<'a>>
where
    FEvalCondition: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> bool,
    FIsZeroLiteral: Copy + Fn(&dae::Expression) -> bool,
{
    if let Some(assignment) = extract_direct_tuple_function_assignment(residual) {
        return Some(assignment);
    }
    if let Some(assignment) =
        extract_active_tuple_assignment_from_expr(residual, env, eval_condition)
    {
        return Some(assignment);
    }
    let dae::Expression::Binary {
        op: dae::OpBinary::Sub(_),
        lhs,
        rhs,
    } = residual
    else {
        return None;
    };
    if is_zero_literal(lhs.as_ref()) {
        return extract_active_tuple_assignment_from_expr(rhs, env, eval_condition);
    }
    if is_zero_literal(rhs.as_ref()) {
        return extract_active_tuple_assignment_from_expr(lhs, env, eval_condition);
    }
    None
}

pub fn discrete_tuple_function_assignment_from_equation<'a, FEvalCondition, FIsZeroLiteral>(
    eq: &'a dae::Equation,
    env: &VarEnv<f64>,
    eval_condition: FEvalCondition,
    is_zero_literal: FIsZeroLiteral,
) -> Option<TupleFunctionAssignment<'a>>
where
    FEvalCondition: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> bool,
    FIsZeroLiteral: Copy + Fn(&dae::Expression) -> bool,
{
    if eq.lhs.is_some() {
        return None;
    }
    extract_active_discrete_tuple_function_assignment(&eq.rhs, env, eval_condition, is_zero_literal)
}

fn projected_output_name(resolved_name: &dae::VarName, output_name: &str, suffix: &str) -> String {
    if suffix.is_empty() {
        return format!("{}.{}", resolved_name.as_str(), output_name);
    }
    format!("{}.{}{}", resolved_name.as_str(), output_name, suffix)
}

fn dims_total_size(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    let mut total = 1usize;
    for dim in dims {
        if *dim <= 0 {
            return None;
        }
        let Ok(dim_usize) = usize::try_from(*dim) else {
            return None;
        };
        total = total.checked_mul(dim_usize)?;
    }
    Some(total)
}

fn evaluate_projected_function_output(
    resolved_name: &dae::VarName,
    output_name: &str,
    suffix: &str,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
) -> f64 {
    let projected = projected_output_name(resolved_name, output_name, suffix);
    let value =
        rumoca_eval_flat::eval::eval_function_call_pub(&dae::VarName::new(projected), args, env);
    if value.is_finite() { value } else { 0.0 }
}

fn apply_tuple_scalar_output_target(
    resolved_name: &dae::VarName,
    output_name: &str,
    suffix: &str,
    target: &str,
    args: &[dae::Expression],
    env: &mut VarEnv<f64>,
    set_target_value: &mut impl FnMut(&mut VarEnv<f64>, &str, f64) -> bool,
) -> bool {
    let new_value =
        evaluate_projected_function_output(resolved_name, output_name, suffix, args, env);
    set_target_value(env, target, new_value)
}

fn apply_tuple_array_output_target(
    resolved_name: &dae::VarName,
    output_name: &str,
    target_base: &str,
    args: &[dae::Expression],
    dims: &[i64],
    env: &mut VarEnv<f64>,
    set_target_value: &mut impl FnMut(&mut VarEnv<f64>, &str, f64) -> bool,
) -> bool {
    let Some(size) = dims_total_size(dims) else {
        return false;
    };
    if size <= 1 {
        return false;
    }

    let mut changed = false;
    let mut values = Vec::with_capacity(size);
    for index in 1..=size {
        let suffix = format!("[{index}]");
        let value =
            evaluate_projected_function_output(resolved_name, output_name, &suffix, args, env);
        let key = format!("{target_base}[{index}]");
        changed |= set_target_value(env, key.as_str(), value);
        values.push(value);
    }

    if let Some(first_value) = values.first().copied() {
        changed |= set_target_value(env, target_base, first_value);
    }

    if !values.is_empty() {
        rumoca_eval_flat::eval::set_array_entries(env, target_base, dims, &values);
    }
    changed
}

pub fn apply_discrete_tuple_function_assignment(
    tuple_assignment: &TupleFunctionAssignment<'_>,
    env: &mut VarEnv<f64>,
    implicit_clock_active: bool,
    expr_uses_previous: impl Fn(&dae::Expression) -> bool,
    mut set_target_value: impl FnMut(&mut VarEnv<f64>, &str, f64) -> bool,
    mut on_unresolved_function_outputs: impl FnMut(&dae::VarName),
) -> bool {
    let dae::Expression::FunctionCall { name, args, .. } = tuple_assignment.solution else {
        return false;
    };

    if !implicit_clock_active && expr_uses_previous(tuple_assignment.solution) {
        return false;
    }

    let Some((resolved_name, output_names)) =
        rumoca_eval_flat::eval::resolve_function_call_outputs_pub(name, env)
    else {
        on_unresolved_function_outputs(name);
        return false;
    };

    let mut changed_any = false;
    for (idx, target) in tuple_assignment.targets.iter().enumerate() {
        let Some(output_name) = output_names.get(idx) else {
            break;
        };

        if !target.suffix.is_empty() {
            changed_any |= apply_tuple_scalar_output_target(
                &resolved_name,
                output_name,
                target.suffix.as_str(),
                target.key.as_str(),
                args,
                env,
                &mut set_target_value,
            );
            continue;
        }

        let dims = env
            .dims
            .get(target.base_name.as_str())
            .cloned()
            .unwrap_or_default();
        if dims_total_size(&dims).is_some_and(|size| size > 1) {
            changed_any |= apply_tuple_array_output_target(
                &resolved_name,
                output_name,
                target.base_name.as_str(),
                args,
                &dims,
                env,
                &mut set_target_value,
            );
            continue;
        }

        changed_any |= apply_tuple_scalar_output_target(
            &resolved_name,
            output_name,
            "",
            target.base_name.as_str(),
            args,
            env,
            &mut set_target_value,
        );
    }

    changed_any
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_eval_flat::sim_float::SimFloat;

    #[test]
    fn extract_direct_tuple_function_assignment_detects_tuple_call_form() {
        let lhs = dae::Expression::Tuple {
            elements: vec![
                dae::Expression::VarRef {
                    name: dae::VarName::new("a"),
                    subscripts: vec![],
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("b"),
                    subscripts: vec![],
                },
            ],
        };
        let rhs = dae::Expression::FunctionCall {
            name: dae::VarName::new("f"),
            args: vec![],
            is_constructor: false,
        };
        let residual = dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };
        let assignment =
            extract_direct_tuple_function_assignment(&residual).expect("tuple function assignment");
        assert_eq!(assignment.targets.len(), 2);
    }

    #[test]
    fn discrete_tuple_function_assignment_from_equation_selects_active_if_branch() {
        let branch_true = dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::Tuple {
                elements: vec![dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }],
            }),
            rhs: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("f"),
                args: vec![],
                is_constructor: false,
            }),
        };
        let branch_false = dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::Tuple {
                elements: vec![dae::Expression::VarRef {
                    name: dae::VarName::new("y"),
                    subscripts: vec![],
                }],
            }),
            rhs: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("g"),
                args: vec![],
                is_constructor: false,
            }),
        };
        let eq = dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::Literal(dae::Literal::Boolean(true)),
                    branch_true,
                )],
                else_branch: Box::new(branch_false),
            },
            Span::DUMMY,
            "tuple_if",
        );
        let env = VarEnv::<f64>::new();
        let assignment = discrete_tuple_function_assignment_from_equation(
            &eq,
            &env,
            |cond, env| rumoca_eval_flat::eval::eval_expr::<f64>(cond, env).to_bool(),
            |expr| matches!(expr, dae::Expression::Literal(dae::Literal::Integer(0))),
        )
        .expect("active tuple assignment");
        assert_eq!(assignment.targets[0].key, "x");
    }
}
