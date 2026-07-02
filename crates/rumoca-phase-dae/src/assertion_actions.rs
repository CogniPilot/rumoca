use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Reference, Span, Subscript, VarName,
};
use rumoca_eval_dae::constant::{ConstValue, eval_const_expr_with};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::{ToDaeError, flat_to_dae_expression_with_refs};

pub(crate) fn lower_assert_equations_to_event_actions(
    dae_model: &mut dae::Dae,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    lower_assertion_set_to_event_actions(
        dae_model,
        flat,
        &flat.assert_equations,
        AssertionScope::Runtime,
    )?;
    lower_assertion_set_to_event_actions(
        dae_model,
        flat,
        &flat.initial_assert_equations,
        AssertionScope::Initial,
    )
}

#[derive(Clone, Copy)]
enum AssertionScope {
    Runtime,
    Initial,
}

impl AssertionScope {
    fn origin(self) -> &'static str {
        match self {
            Self::Runtime => "assert equation",
            Self::Initial => "initial assert equation",
        }
    }
}

fn lower_assertion_set_to_event_actions(
    dae_model: &mut dae::Dae,
    flat: &flat::Model,
    assertions: &[flat::AssertEquation],
    scope: AssertionScope,
) -> Result<(), ToDaeError> {
    dae_model
        .events
        .event_actions
        .try_reserve(assertions.len())
        .map_err(|_| {
            ToDaeError::runtime_metadata_violation(
                "assert event action capacity exceeds host memory limits",
            )
        })?;
    for assertion in assertions {
        dae_model
            .events
            .event_actions
            .push(lower_assertion_event_action(
                dae_model, flat, assertion, scope,
            )?);
    }
    Ok(())
}

fn lower_assertion_event_action(
    dae_model: &dae::Dae,
    flat: &flat::Model,
    assertion: &flat::AssertEquation,
    scope: AssertionScope,
) -> Result<dae::DaeEventAction, ToDaeError> {
    let condition = assertion_action_condition(assertion, scope);
    let condition = flat_to_dae_expression_with_refs(&condition, flat)?;
    let condition = fold_assertion_condition_constants(&condition, dae_model, assertion.span);
    Ok(dae::DaeEventAction {
        condition,
        kind: dae::DaeEventActionKind::Assert {
            message: flat_to_dae_expression_with_refs(&assertion.message, flat)?,
        },
        span: assertion.span,
        origin: scope.origin().to_string(),
    })
}

fn assertion_action_condition(
    assertion: &flat::AssertEquation,
    scope: AssertionScope,
) -> Expression {
    let failing = assertion_failure_condition(assertion);
    match scope {
        AssertionScope::Runtime => failing,
        AssertionScope::Initial => and_expr(initial_expr(assertion.span), failing, assertion.span),
    }
}

fn assertion_failure_condition(assertion: &flat::AssertEquation) -> Expression {
    let failing = not_expr(assertion.condition.clone(), assertion.span);
    let Some(level) = assertion.level.as_ref() else {
        return failing;
    };
    and_expr(
        failing,
        assertion_level_error_condition(level.clone(), assertion.span),
        assertion.span,
    )
}

fn assertion_level_error_condition(level: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Eq,
        lhs: Box::new(level),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("AssertionLevel.error").into(),
            subscripts: Vec::new(),
            span,
        }),
        span,
    }
}

fn initial_expr(span: Span) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Initial,
        args: Vec::new(),
        span,
    }
}

fn not_expr(expr: Expression, span: Span) -> Expression {
    if let Expression::Literal {
        value: Literal::Boolean(value),
        span,
    } = expr
    {
        return Expression::Literal {
            value: Literal::Boolean(!value),
            span,
        };
    }
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(expr),
        span,
    }
}

fn and_expr(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    if is_bool(&lhs, false) || is_bool(&rhs, false) {
        return bool_expr(false, span);
    }
    if is_bool(&lhs, true) {
        return rhs;
    }
    if is_bool(&rhs, true) {
        return lhs;
    }
    Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn bool_expr(value: bool, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span,
    }
}

fn is_bool(expr: &Expression, expected: bool) -> bool {
    matches!(expr, Expression::Literal { value: Literal::Boolean(value), .. } if *value == expected)
}

fn fold_assertion_condition_constants(
    expr: &Expression,
    dae_model: &dae::Dae,
    owner_span: Span,
) -> Expression {
    if let Some(value) = eval_assertion_condition_const(expr, dae_model) {
        return const_value_expr(value, expression_span_or_owner(expr, owner_span));
    }
    match expr {
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            span,
        } => {
            let rhs = fold_assertion_condition_constants(rhs, dae_model, owner_span);
            not_expr(rhs, *span)
        }
        Expression::Binary { op, lhs, rhs, span } => {
            let lhs = fold_assertion_condition_constants(lhs, dae_model, owner_span);
            let rhs = fold_assertion_condition_constants(rhs, dae_model, owner_span);
            fold_assertion_binary(op.clone(), lhs, rhs, *span)
        }
        _ => expr.clone(),
    }
}

fn expression_span_or_owner(expr: &Expression, owner_span: Span) -> Span {
    expr.span()
        .filter(|span| !span.is_dummy())
        .unwrap_or(owner_span)
}

fn fold_assertion_binary(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    match op {
        OpBinary::Or => {
            if is_bool(&lhs, true) || is_bool(&rhs, true) {
                return bool_expr(true, span);
            }
            if is_bool(&lhs, false) {
                return rhs;
            }
            if is_bool(&rhs, false) {
                return lhs;
            }
        }
        OpBinary::And => {
            if is_bool(&lhs, false) || is_bool(&rhs, false) {
                return bool_expr(false, span);
            }
            if is_bool(&lhs, true) {
                return rhs;
            }
            if is_bool(&rhs, true) {
                return lhs;
            }
        }
        _ => {}
    }
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn eval_assertion_condition_const(expr: &Expression, dae_model: &dae::Dae) -> Option<ConstValue> {
    eval_const_expr_with(expr, &|name, subscripts| {
        lookup_dae_start_const(dae_model, name, subscripts)
    })
}

fn lookup_dae_start_const(
    dae_model: &dae::Dae,
    name: &Reference,
    subscripts: &[Subscript],
) -> Option<ConstValue> {
    if !subscripts.is_empty() {
        return None;
    }
    let key = name.as_str();
    let var_name = VarName::new(key);
    let variable = dae_model
        .variables
        .parameters
        .get(&var_name)
        .or_else(|| dae_model.variables.constants.get(&var_name))?;
    let start = variable.start.as_ref()?;
    eval_const_expr_with(start, &|_, _| None)
}

fn const_value_expr(value: ConstValue, span: Span) -> Expression {
    Expression::Literal {
        value: value.into_literal(),
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("assertion_string_guard.mo"),
            10,
            60,
        )
    }

    fn string_literal(value: &str, span: Span) -> Expression {
        Expression::Literal {
            value: Literal::String(value.to_string()),
            span,
        }
    }

    fn var_ref(name: &str, span: Span) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: Vec::new(),
            span,
        }
    }

    #[test]
    fn fold_assertion_condition_resolves_constant_string_guard() {
        let span = test_span();
        let mut dae_model = dae::Dae::default();
        let mut table_name = dae::Variable::new(VarName::new("tableName"), span);
        table_name.start = Some(string_literal("a", span));
        dae_model
            .variables
            .parameters
            .insert(table_name.name.clone(), table_name);

        let table_name_is_named = Expression::Binary {
            op: OpBinary::Neq,
            lhs: Box::new(var_ref("tableName", span)),
            rhs: Box::new(string_literal("NoName", span)),
            span,
        };
        let assertion_condition = Expression::Unary {
            op: OpUnary::Not,
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Or,
                lhs: Box::new(table_name_is_named),
                rhs: Box::new(var_ref("isCsvExt", span)),
                span,
            }),
            span,
        };

        let folded = fold_assertion_condition_constants(&assertion_condition, &dae_model, span);

        assert!(is_bool(&folded, false));
    }
}
