use super::*;
use rumoca_core::ExpressionRewriter;

pub(super) fn rewrite_algorithm_current_refs(
    dae: &Dae,
    expr: &Expression,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    AlgorithmCurrentRewriter {
        dae,
        current_values,
        known_targets,
    }
    .rewrite_expression(expr)
}

struct AlgorithmCurrentRewriter<'a> {
    dae: &'a Dae,
    current_values: &'a IndexMap<VarName, Expression>,
    known_targets: &'a HashSet<VarName>,
}

impl ExpressionRewriter for AlgorithmCurrentRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => rewrite_algorithm_current_var_ref(
                self.dae,
                name,
                subscripts,
                *span,
                self.current_values,
                self.known_targets,
            ),
            Expression::BuiltinCall {
                function,
                args,
                span,
            } if matches!(function, BuiltinFunction::Pre) => Expression::BuiltinCall {
                function: *function,
                args: args.clone(),
                span: *span,
            },
            _ => self.walk_expression(expr),
        }
    }
}

fn rewrite_algorithm_current_var_ref(
    dae: &Dae,
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    span: Span,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Expression {
    let target = varref_with_subscripts(name, subscripts);
    if let Some(value) = current_values.get(&target) {
        return value.clone();
    }
    if !subscripts.is_empty()
        && let Some(value) = current_values.get(name.var_name())
    {
        return current_value_subscript_expr(value, subscripts);
    }
    {
        if let Some(value) =
            dynamic_current_var_ref(dae, name, subscripts, span, current_values, known_targets)
        {
            return value;
        }
        if let Some(value) = static_parameter_array_element(dae, name, subscripts) {
            return value;
        }
        if known_targets.contains(&target) {
            algorithm_if_fallback_expr(dae, &target)
        } else {
            Expression::VarRef {
                name: name.clone(),
                subscripts: subscripts.to_vec(),
                span,
            }
        }
    }
}

pub(super) fn current_value_subscript_expr(
    value: &Expression,
    subscripts: &[Subscript],
) -> Expression {
    if let Some(element) = static_array_element(value, subscripts) {
        return element;
    }
    let span = subscripts
        .first()
        .map(Subscript::span)
        .or_else(|| value.span())
        .unwrap_or(rumoca_core::Span::DUMMY);
    Expression::Index {
        base: Box::new(value.clone()),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn dynamic_current_var_ref(
    dae: &Dae,
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    span: Span,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Option<Expression> {
    if subscripts.is_empty() || !subscripts.iter().any(is_dynamic_scalar_subscript) {
        return None;
    }
    let base_name = VarName::new(name.as_str());
    let dims = algorithm_variable_dims(dae, &base_name)?;
    if dims.len() != subscripts.len() {
        return None;
    }

    let mut cases = vec![(Vec::new(), None)];
    for (subscript, dim) in subscripts.iter().zip(dims.iter()) {
        let options = dynamic_current_axis_options(subscript, *dim)?;
        cases = extend_dynamic_current_cases(cases, options);
    }

    let fallback = Expression::VarRef {
        name: name.clone(),
        subscripts: subscripts.to_vec(),
        span,
    };
    let mut merged = fallback;
    let mut used_current_value = false;
    for (static_subscripts, guard) in cases.into_iter().rev() {
        let target = varref_with_subscripts(name, &static_subscripts);
        let value = current_values.get(&target).cloned().or_else(|| {
            known_targets
                .contains(&target)
                .then(|| algorithm_if_fallback_expr(dae, &target))
        });
        let Some(value) = value else {
            continue;
        };
        let Some(guard) = guard else {
            return Some(value);
        };
        used_current_value = true;
        merged = Expression::If {
            branches: vec![(guard, value)],
            else_branch: Box::new(merged),
            span,
        };
    }
    used_current_value.then_some(merged)
}

fn is_dynamic_scalar_subscript(subscript: &Subscript) -> bool {
    matches!(subscript, Subscript::Expr { expr, .. } if static_subscript_index(&Subscript::generated_expr(expr.clone())).is_none())
}

pub(super) fn algorithm_variable_dims(dae: &Dae, name: &VarName) -> Option<Vec<i64>> {
    let key = flat_to_dae_var_name(name);
    dae.variables
        .states
        .get(&key)
        .or_else(|| dae.variables.algebraics.get(&key))
        .or_else(|| dae.variables.outputs.get(&key))
        .or_else(|| dae.variables.inputs.get(&key))
        .or_else(|| dae.variables.discrete_reals.get(&key))
        .or_else(|| dae.variables.discrete_valued.get(&key))
        .or_else(|| dae.variables.parameters.get(&key))
        .or_else(|| dae.variables.constants.get(&key))
        .map(|var| var.dims.clone())
}

fn dynamic_current_axis_options(
    subscript: &Subscript,
    dim: i64,
) -> Option<Vec<(Subscript, Option<Expression>)>> {
    if dim <= 0 {
        return None;
    }
    if let Some(index) = static_subscript_index(subscript) {
        return Some(vec![(
            Subscript::generated_index(index, subscript.span()),
            None,
        )]);
    }
    let Subscript::Expr { expr: selector, .. } = subscript else {
        return None;
    };
    Some(
        (1..=dim)
            .map(|candidate| {
                (
                    Subscript::generated_index(
                        candidate,
                        selector.span().unwrap_or(rumoca_core::Span::DUMMY),
                    ),
                    Some(dynamic_current_guard(selector, candidate)),
                )
            })
            .collect(),
    )
}

fn extend_dynamic_current_cases(
    cases: Vec<(Vec<Subscript>, Option<Expression>)>,
    options: Vec<(Subscript, Option<Expression>)>,
) -> Vec<(Vec<Subscript>, Option<Expression>)> {
    let mut extended = Vec::with_capacity(cases.len() * options.len());
    for (prefix, prefix_guard) in cases {
        for (subscript, guard) in &options {
            let mut subscripts = prefix.clone();
            subscripts.push(subscript.clone());
            extended.push((
                subscripts,
                and_optional_expr(prefix_guard.clone(), guard.clone()),
            ));
        }
    }
    extended
}

fn dynamic_current_guard(selector: &Expression, candidate: i64) -> Expression {
    let span = selector.span().unwrap_or(rumoca_core::Span::DUMMY);
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

fn and_optional_expr(lhs: Option<Expression>, rhs: Option<Expression>) -> Option<Expression> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => {
            let span = lhs
                .span()
                .or_else(|| rhs.span())
                .unwrap_or(rumoca_core::Span::DUMMY);
            Some(Expression::Binary {
                op: rumoca_core::OpBinary::And,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            })
        }
        (Some(expr), None) | (None, Some(expr)) => Some(expr),
        (None, None) => None,
    }
}

fn static_parameter_array_element(
    dae: &Dae,
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
) -> Option<Expression> {
    let var = dae.variables.parameters.get(name.var_name())?;
    let start = var.start.as_ref()?;
    static_array_element(start, subscripts)
}

fn static_array_element(expr: &Expression, subscripts: &[Subscript]) -> Option<Expression> {
    let (first, rest) = subscripts.split_first()?;
    let index = static_subscript_index(first)?;
    let Expression::Array { elements, .. } = expr else {
        return None;
    };
    let element = elements.get(index.checked_sub(1)? as usize)?;
    if rest.is_empty() {
        Some(element.clone())
    } else {
        static_array_element(element, rest)
    }
}

fn static_subscript_index(subscript: &Subscript) -> Option<i64> {
    match subscript {
        Subscript::Index { value: index, .. } => Some(*index),
        Subscript::Expr { expr, .. } => match &**expr {
            Expression::Literal {
                value: Literal::Integer(index),
                ..
            } => Some(*index),
            _ => None,
        },
        Subscript::Colon { .. } => None,
    }
}
