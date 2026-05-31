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
            let lhs = dae_to_flat_var_name(lhs);
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

fn when_guard_scalar_activation_expr(dae: &Dae, when_condition: &Expression) -> Expression {
    let unfolded = unfold_boolean_aliases_in_expr(dae, when_condition);
    if is_clock_constructor_condition(&unfolded) {
        return unfolded;
    }
    // MLS §8.3.5.1: when-equation assignments fire on the false->true edge
    // of the condition, not while the condition remains true. Keep the
    // canonical relational subexpressions visible under edge(...) so the
    // relation analysis still exposes the underlying condition roots.
    match when_condition {
        Expression::VarRef { .. } | Expression::Index { .. } | Expression::FieldAccess { .. }
            if !unfolded.contains_relational_operator() =>
        {
            Expression::BuiltinCall {
                function: BuiltinFunction::Edge,
                args: vec![when_condition.clone()],
                span: rumoca_core::Span::DUMMY,
            }
        }
        _ => Expression::BuiltinCall {
            function: BuiltinFunction::Edge,
            args: vec![unfolded],
            span: rumoca_core::Span::DUMMY,
        },
    }
}

fn is_clock_constructor_condition(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::FunctionCall { name, .. } if name.last_segment() == "Clock"
    )
}

fn bool_expr(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn or_expr(lhs: Expression, rhs: Expression) -> Expression {
    match (&lhs, &rhs) {
        (
            Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            _,
        )
        | (
            _,
            Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
        ) => bool_expr(true),
        (
            Expression::Literal {
                value: Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            },
            _,
        ) => rhs,
        (
            _,
            Expression::Literal {
                value: Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            },
        ) => lhs,
        _ => Expression::Binary {
            op: rumoca_core::OpBinary::Or,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        },
    }
}

fn vector_when_guard_activation_expr<'a>(
    dae: &Dae,
    elements: impl IntoIterator<Item = &'a Expression>,
) -> Expression {
    elements
        .into_iter()
        .map(|element| when_guard_activation_expr(dae, element))
        .reduce(or_expr)
        .unwrap_or_else(|| bool_expr(false))
}

pub(super) fn when_guard_activation_expr(dae: &Dae, when_condition: &Expression) -> Expression {
    match when_condition {
        // MLS §8.3.5: vectorized when-conditions trigger when any listed
        // condition becomes true, so each scalar Boolean guard needs its own
        // activation edge and the aggregate guard is the scalar OR of those
        // edges. `edge(a or b)` would miss an event when `a` is already true
        // and `b` becomes true.
        Expression::Array { elements, .. } => vector_when_guard_activation_expr(dae, elements),
        Expression::Tuple { elements, .. } => vector_when_guard_activation_expr(dae, elements),
        _ => when_guard_scalar_activation_expr(dae, when_condition),
    }
}
