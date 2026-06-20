use super::*;

fn discrete_valued_name_exists(dae: &Dae, name: &rumoca_core::Reference) -> bool {
    dae.variables
        .discrete_valued
        .contains_key(&flat_to_dae_var_name(name.var_name()))
        || subscript_fallback_chain(name.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .discrete_valued
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
}

fn find_condition_definition_rhs(dae: &Dae, name: &rumoca_core::Reference) -> Option<Expression> {
    dae.discrete
        .valued_updates
        .iter()
        .chain(dae.discrete.real_updates.iter())
        .chain(dae.continuous.equations.iter())
        .find_map(|equation| {
            let lhs = equation.lhs.as_ref()?;
            let lhs = dae_to_flat_var_name(lhs.var_name());
            if lhs.as_str() != name.as_str() {
                return None;
            }
            if equation.origin.starts_with("guarded ") {
                return None;
            }
            Some(dae_to_flat_expression(&equation.rhs))
        })
}

fn unfold_boolean_aliases_in_expr(dae: &Dae, expr: &Expression) -> Expression {
    let mut rewriter = BooleanAliasUnfolder {
        dae,
        visiting: HashSet::new(),
        depth: 0,
    };
    rumoca_core::ExpressionRewriter::rewrite_expression(&mut rewriter, expr)
}

struct BooleanAliasUnfolder<'a> {
    dae: &'a Dae,
    visiting: HashSet<VarName>,
    depth: usize,
}

impl BooleanAliasUnfolder<'_> {
    fn unfold_alias_condition(&mut self, expr: &Expression) -> Option<Expression> {
        if self.depth > 8 {
            return None;
        }
        let Expression::VarRef {
            name, subscripts, ..
        } = expr
        else {
            return None;
        };
        if !subscripts.is_empty() || !discrete_valued_name_exists(self.dae, name) {
            return None;
        }
        if !self.visiting.insert(name.var_name().clone()) {
            return None;
        }
        let result = find_condition_definition_rhs(self.dae, name).and_then(|rhs| {
            let unfolded = self.rewrite_child(&rhs);
            if unfolded.contains_relational_operator() {
                Some(unfolded)
            } else {
                None
            }
        });
        self.visiting.remove(name.var_name());
        result
    }

    fn rewrite_child(&mut self, expr: &Expression) -> Expression {
        self.depth += 1;
        let rewritten = rumoca_core::ExpressionRewriter::rewrite_expression(self, expr);
        self.depth -= 1;
        rewritten
    }
}

impl rumoca_core::ExpressionRewriter for BooleanAliasUnfolder<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if let Some(unfolded) = self.unfold_alias_condition(expr) {
            return unfolded;
        }
        self.depth += 1;
        let rewritten = self.walk_expression(expr);
        self.depth -= 1;
        rewritten
    }
}

fn when_guard_scalar_activation_expr(
    dae: &Dae,
    when_condition: &Expression,
    context_span: Option<rumoca_core::Span>,
) -> Result<Expression, ToDaeError> {
    if is_direct_initial_condition(when_condition) {
        return Ok(when_condition.clone());
    }
    let unfolded = unfold_boolean_aliases_in_expr(dae, when_condition);
    if is_clock_constructor_condition(&unfolded) {
        return Ok(unfolded);
    }
    let span = guard_span(
        when_condition
            .span()
            .or_else(|| expression_span(&unfolded))
            .or(context_span),
    )?;
    // MLS §8.3.5.1: when-equation assignments fire on the false->true edge
    // of the condition, not while the condition remains true. Keep the
    // canonical relational subexpressions visible under edge(...) so the
    // relation analysis still exposes the underlying condition roots.
    match when_condition {
        Expression::VarRef { .. } | Expression::Index { .. } | Expression::FieldAccess { .. }
            if !unfolded.contains_relational_operator() =>
        {
            Ok(Expression::BuiltinCall {
                function: BuiltinFunction::Edge,
                args: vec![when_condition.clone()],
                span,
            })
        }
        _ => Ok(Expression::BuiltinCall {
            function: BuiltinFunction::Edge,
            args: vec![unfolded],
            span,
        }),
    }
}

fn is_direct_initial_condition(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            args,
            ..
        } if args.is_empty()
    )
}

fn is_clock_constructor_condition(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::FunctionCall { name, .. } if name.last_segment() == "Clock"
    )
}

fn bool_expr(value: bool, span: rumoca_core::Span) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span,
    }
}

fn or_expr(
    lhs: Expression,
    rhs: Expression,
    context_span: Option<rumoca_core::Span>,
) -> Result<Expression, ToDaeError> {
    match (literal_bool(&lhs), literal_bool(&rhs)) {
        (Some(true), _) => Ok(lhs),
        (_, Some(true)) => Ok(rhs),
        (Some(false), _) => Ok(rhs),
        (_, Some(false)) => Ok(lhs),
        _ => {
            let span = guard_span(
                expression_span(&lhs)
                    .or_else(|| expression_span(&rhs))
                    .or(context_span),
            )?;
            Ok(Expression::Binary {
                op: rumoca_core::OpBinary::Or,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            })
        }
    }
}

fn literal_bool(expr: &Expression) -> Option<bool> {
    match expr {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn expression_span(expr: &Expression) -> Option<rumoca_core::Span> {
    expr.span()
}

fn guard_span(span: Option<rumoca_core::Span>) -> Result<rumoca_core::Span, ToDaeError> {
    match span {
        Some(span) if !span.is_dummy() => Ok(span),
        Some(_) | None => Err(ToDaeError::runtime_metadata_violation(
            "when guard activation expression is missing source provenance",
        )),
    }
}

fn vector_when_guard_activation_expr<'a>(
    dae: &Dae,
    span: rumoca_core::Span,
    context_span: Option<rumoca_core::Span>,
    elements: impl IntoIterator<Item = &'a Expression>,
) -> Result<Expression, ToDaeError> {
    let context_span = if span.is_dummy() {
        context_span
    } else {
        Some(span)
    };
    let span = guard_span(context_span)?;
    let mut guards = elements
        .into_iter()
        .map(|element| when_guard_activation_expr_with_context(dae, element, context_span));
    let Some(first) = guards.next() else {
        return Ok(bool_expr(false, span));
    };
    guards.try_fold(first?, |lhs, rhs| or_expr(lhs, rhs?, context_span))
}

pub(super) fn when_guard_activation_expr(
    dae: &Dae,
    when_condition: &Expression,
    owner_span: rumoca_core::Span,
) -> Result<Expression, ToDaeError> {
    let context_span = (!owner_span.is_dummy()).then_some(owner_span);
    when_guard_activation_expr_with_context(dae, when_condition, context_span)
}

fn when_guard_activation_expr_with_context(
    dae: &Dae,
    when_condition: &Expression,
    context_span: Option<rumoca_core::Span>,
) -> Result<Expression, ToDaeError> {
    match when_condition {
        // MLS §8.3.5: vectorized when-conditions trigger when any listed
        // condition becomes true, so each scalar Boolean guard needs its own
        // activation edge and the aggregate guard is the scalar OR of those
        // edges. `edge(a or b)` would miss an event when `a` is already true
        // and `b` becomes true.
        Expression::Array { elements, span, .. } => {
            vector_when_guard_activation_expr(dae, *span, context_span, elements)
        }
        Expression::Tuple { elements, span, .. } => {
            vector_when_guard_activation_expr(dae, *span, context_span, elements)
        }
        _ => when_guard_scalar_activation_expr(dae, when_condition, context_span),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId(93), start, end)
    }

    fn bool_var(name: &str, span: rumoca_core::Span) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: Vec::new(),
            span,
        }
    }

    #[test]
    fn ownerless_when_guard_activation_fails_without_dummy_span() {
        let dae = Dae::default();
        let err = when_guard_activation_expr(
            &dae,
            &bool_var("trigger", rumoca_core::Span::DUMMY),
            rumoca_core::Span::DUMMY,
        )
        .expect_err("ownerless guard activation must not fabricate provenance");

        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }

    #[test]
    fn when_guard_activation_uses_enclosing_owner_span() {
        let dae = Dae::default();
        let owner_span = test_span(10, 20);
        let guard = when_guard_activation_expr(
            &dae,
            &bool_var("trigger", rumoca_core::Span::DUMMY),
            owner_span,
        )
        .expect("owner span should support generated edge guard");

        assert_eq!(guard.span(), Some(owner_span));
    }
}
