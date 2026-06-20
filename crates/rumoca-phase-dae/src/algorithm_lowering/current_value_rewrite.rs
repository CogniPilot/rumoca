use super::*;
use rumoca_core::ExpressionRewriter;

pub(super) fn rewrite_algorithm_current_refs(
    dae: &Dae,
    expr: &Expression,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Result<Expression, String> {
    let mut rewriter = AlgorithmCurrentRewriter {
        dae,
        current_values,
        known_targets,
        error: None,
    };
    let rewritten = rewriter.rewrite_expression(expr);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(rewritten),
    }
}

struct AlgorithmCurrentRewriter<'a> {
    dae: &'a Dae,
    current_values: &'a IndexMap<VarName, Expression>,
    known_targets: &'a HashSet<VarName>,
    error: Option<String>,
}

type DynamicCurrentCase = (Vec<Subscript>, Option<Expression>);
type DynamicCurrentAxisOption = (Subscript, Option<Expression>);

impl AlgorithmCurrentRewriter<'_> {
    fn record_error(&mut self, error: String) {
        if self.error.is_none() {
            self.error = Some(error);
        }
    }
}

impl ExpressionRewriter for AlgorithmCurrentRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        match expr {
            Expression::VarRef {
                name,
                subscripts,
                span,
            } => match rewrite_algorithm_current_var_ref(
                self.dae,
                name,
                subscripts,
                *span,
                self.current_values,
                self.known_targets,
            ) {
                Ok(expr) => expr,
                Err(error) => {
                    self.record_error(error);
                    expr.clone()
                }
            },
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
) -> Result<Expression, String> {
    let target = varref_with_subscripts(name, subscripts);
    if let Some(value) = current_values.get(&target) {
        return Ok(value.clone());
    }
    if !subscripts.is_empty()
        && let Some(value) = current_values.get(name.var_name())
    {
        return current_value_subscript_expr(value, subscripts);
    }
    {
        if let Some(value) =
            dynamic_current_var_ref(dae, name, subscripts, span, current_values, known_targets)?
        {
            return Ok(value);
        }
        if let Some(value) = static_parameter_array_element(dae, name, subscripts) {
            return Ok(value);
        }
        if known_targets.contains(&target) {
            algorithm_if_fallback_expr(dae, &target, span)
        } else {
            Ok(Expression::VarRef {
                name: name.clone(),
                subscripts: subscripts.to_vec(),
                span,
            })
        }
    }
}

pub(super) fn current_value_subscript_expr(
    value: &Expression,
    subscripts: &[Subscript],
) -> Result<Expression, String> {
    if let Some(element) = static_array_element(value, subscripts) {
        return Ok(element);
    }
    let span = required_current_value_span(
        "algorithm current-value subscript expression",
        subscripts
            .iter()
            .map(Subscript::span)
            .find(|span| !span.is_dummy())
            .or_else(|| value.span()),
    )?;
    Ok(Expression::Index {
        base: Box::new(value.clone()),
        subscripts: subscripts.to_vec(),
        span,
    })
}

fn dynamic_current_var_ref(
    dae: &Dae,
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    span: Span,
    current_values: &IndexMap<VarName, Expression>,
    known_targets: &HashSet<VarName>,
) -> Result<Option<Expression>, String> {
    if subscripts.is_empty() || !subscripts.iter().any(is_dynamic_scalar_subscript) {
        return Ok(None);
    }
    let base_name = VarName::new(name.as_str());
    let Some(dims) = algorithm_variable_dims(dae, &base_name) else {
        return Ok(None);
    };
    if dims.len() != subscripts.len() {
        return Ok(None);
    }

    let mut cases = vec![(Vec::new(), None)];
    for (subscript, dim) in subscripts.iter().zip(dims.iter()) {
        let Some(options) = dynamic_current_axis_options(subscript, *dim)? else {
            return Ok(None);
        };
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
        let value = match current_values.get(&target) {
            Some(value) => Some(value.clone()),
            None if known_targets.contains(&target) => {
                Some(algorithm_if_fallback_expr(dae, &target, span)?)
            }
            None => None,
        };
        let Some(value) = value else {
            continue;
        };
        let Some(guard) = guard else {
            return Ok(Some(value));
        };
        used_current_value = true;
        merged = Expression::If {
            branches: vec![(guard, value)],
            else_branch: Box::new(merged),
            span,
        };
    }
    Ok(used_current_value.then_some(merged))
}

fn is_dynamic_scalar_subscript(subscript: &Subscript) -> bool {
    matches!(
        subscript,
        Subscript::Expr { expr, .. }
            if !matches!(
                expr.as_ref(),
                Expression::Literal {
                    value: Literal::Integer(_),
                    ..
                }
            )
    )
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
) -> Result<Option<Vec<DynamicCurrentAxisOption>>, String> {
    if dim <= 0 {
        return Ok(None);
    }
    if let Some(index) = static_subscript_index(subscript) {
        return Ok(Some(vec![(
            Subscript::try_generated_index(
                index,
                subscript.span(),
                "algorithm current static subscript",
            )
            .map_err(|err| err.to_string())?,
            None,
        )]));
    }
    let Subscript::Expr {
        expr: selector,
        span: selector_span,
    } = subscript
    else {
        return Ok(None);
    };
    let selector_span = required_current_value_span(
        "algorithm current dynamic selector",
        selector
            .span()
            .or_else(|| (!selector_span.is_dummy()).then_some(*selector_span)),
    )?;
    Ok(Some(
        (1..=dim)
            .map(|candidate| {
                Ok((
                    Subscript::try_generated_index(
                        candidate,
                        selector_span,
                        "algorithm current dynamic candidate subscript",
                    )
                    .map_err(|err| err.to_string())?,
                    Some(dynamic_current_guard(selector, candidate, selector_span)),
                ))
            })
            .collect::<Result<Vec<_>, String>>()?,
    ))
}

fn extend_dynamic_current_cases(
    cases: Vec<DynamicCurrentCase>,
    options: Vec<DynamicCurrentAxisOption>,
) -> Vec<DynamicCurrentCase> {
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

fn dynamic_current_guard(
    selector: &Expression,
    candidate: i64,
    span: rumoca_core::Span,
) -> Expression {
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
            let span = lhs.span().or_else(|| rhs.span())?;
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

fn required_current_value_span(
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<rumoca_core::Span, String> {
    let Some(span) = span else {
        return Err(format!("missing source provenance for {context}"));
    };
    span.require_provenance(context)
        .map(|span| span.span())
        .map_err(|err| err.to_string())
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(rumoca_core::SourceId(97), 11, 23)
    }

    fn literal(value: f64, span: Span) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span,
        }
    }

    fn selector(span: Span) -> Subscript {
        Subscript::expr(
            Box::new(Expression::VarRef {
                name: rumoca_core::Reference::new("i"),
                subscripts: Vec::new(),
                span,
            }),
            span,
        )
    }

    fn array_dae() -> Dae {
        let mut dae = Dae::new();
        let name = VarName::new("x");
        let mut var = dae::Variable::new(
            flat_to_dae_var_name(&name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.dims = vec![2];
        dae.variables.discrete_valued.insert(var.name.clone(), var);
        dae
    }

    fn dynamic_x_ref(subscript: Subscript, span: Span) -> Expression {
        Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![subscript],
            span,
        }
    }

    #[test]
    fn dynamic_current_rewrite_preserves_selector_span() -> Result<(), String> {
        let span = test_span();
        let dae = array_dae();
        let current_values = IndexMap::from([
            (VarName::new("x[1]"), literal(1.0, span)),
            (VarName::new("x[2]"), literal(2.0, span)),
        ]);

        let rewritten = rewrite_algorithm_current_refs(
            &dae,
            &dynamic_x_ref(selector(span), span),
            &current_values,
            &HashSet::new(),
        )?;

        assert_eq!(rewritten.span(), Some(span));
        Ok(())
    }

    #[test]
    fn dynamic_current_rewrite_rejects_unspanned_selector() {
        let dae = array_dae();
        let current_values = IndexMap::from([
            (VarName::new("x[1]"), literal(1.0, test_span())),
            (VarName::new("x[2]"), literal(2.0, test_span())),
        ]);

        let err = rewrite_algorithm_current_refs(
            &dae,
            &dynamic_x_ref(selector(Span::DUMMY), Span::DUMMY),
            &current_values,
            &HashSet::new(),
        )
        .expect_err("dynamic algorithm-current selector should require provenance");

        assert!(
            err.contains("algorithm current dynamic selector"),
            "unexpected error: {err}"
        );
    }
}
