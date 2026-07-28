use rumoca_core::{BuiltinFunction, Expression, OpBinary, OpUnary, VarName};
use rumoca_ir_dae as dae;

pub(super) fn is_event_constant_time_threshold_relation(
    dae_model: &dae::Dae,
    expr: &Expression,
) -> bool {
    let Expression::Binary { op, lhs, rhs, .. } = expr else {
        return false;
    };
    if matches!(op, OpBinary::Ge | OpBinary::Gt)
        && expr_is_bare_time(lhs)
        && !expr_is_bare_time(rhs)
    {
        return is_event_constant_threshold(dae_model, rhs);
    }
    if matches!(op, OpBinary::Le | OpBinary::Lt)
        && expr_is_bare_time(rhs)
        && !expr_is_bare_time(lhs)
    {
        return is_event_constant_threshold(dae_model, lhs);
    }
    false
}

fn is_event_constant_threshold(dae_model: &dae::Dae, expr: &Expression) -> bool {
    match expr {
        Expression::Literal { .. } => true,
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
            ..
        } => matches!(args.as_slice(), [arg] if pre_arg_is_component_reference(arg)),
        Expression::Unary { op, rhs, .. } => {
            threshold_unary_op_is_arithmetic(op) && is_event_constant_threshold(dae_model, rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            threshold_binary_op_is_arithmetic(op)
                && is_event_constant_threshold(dae_model, lhs)
                && is_event_constant_threshold(dae_model, rhs)
        }
        Expression::VarRef {
            name, subscripts, ..
        } => {
            subscripts_are_evaluable(subscripts)
                && var_ref_is_event_constant(dae_model, name.as_str())
        }
        Expression::Index {
            base, subscripts, ..
        } => subscripts_are_evaluable(subscripts) && is_event_constant_threshold(dae_model, base),
        Expression::FieldAccess { base, field, .. } => {
            field_access_is_event_constant(dae_model, base, field)
        }
        _ => false,
    }
}

fn expr_is_bare_time(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "time" && subscripts.is_empty()
    )
}

fn threshold_unary_op_is_arithmetic(op: &OpUnary) -> bool {
    matches!(
        op,
        OpUnary::Empty | OpUnary::Minus | OpUnary::Plus | OpUnary::DotMinus | OpUnary::DotPlus
    )
}

fn threshold_binary_op_is_arithmetic(op: &OpBinary) -> bool {
    matches!(
        op,
        OpBinary::Add
            | OpBinary::Sub
            | OpBinary::Mul
            | OpBinary::Div
            | OpBinary::Exp
            | OpBinary::AddElem
            | OpBinary::SubElem
            | OpBinary::MulElem
            | OpBinary::DivElem
            | OpBinary::ExpElem
    )
}

fn pre_arg_is_component_reference(expr: &Expression) -> bool {
    match expr {
        Expression::VarRef { subscripts, .. } => subscripts_are_evaluable(subscripts),
        Expression::Index {
            base, subscripts, ..
        } => pre_arg_is_component_reference(base) && subscripts_are_evaluable(subscripts),
        Expression::FieldAccess { base, .. } => pre_arg_is_component_reference(base),
        _ => false,
    }
}

fn field_access_is_event_constant(dae_model: &dae::Dae, base: &Expression, field: &str) -> bool {
    if let Expression::VarRef {
        name, subscripts, ..
    } = base
        && subscripts_are_evaluable(subscripts)
    {
        let field_name = format!("{}.{}", name.as_str(), field);
        return var_ref_is_event_constant(dae_model, &field_name)
            || var_ref_is_event_constant(dae_model, name.as_str());
    }
    is_event_constant_threshold(dae_model, base)
}

fn subscripts_are_evaluable(subscripts: &[rumoca_core::Subscript]) -> bool {
    subscripts.iter().all(subscript_is_evaluable)
}

fn subscript_is_evaluable(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Index { .. } => true,
        rumoca_core::Subscript::Expr { expr, .. } => expr_is_evaluable_subscript(expr),
        rumoca_core::Subscript::Colon { .. } => false,
    }
}

fn expr_is_evaluable_subscript(expr: &Expression) -> bool {
    match expr {
        Expression::Literal { .. } => true,
        Expression::Unary { op, rhs, .. } => {
            threshold_unary_op_is_arithmetic(op) && expr_is_evaluable_subscript(rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            threshold_binary_op_is_arithmetic(op)
                && expr_is_evaluable_subscript(lhs)
                && expr_is_evaluable_subscript(rhs)
        }
        _ => false,
    }
}

fn var_ref_is_event_constant(dae_model: &dae::Dae, name: &str) -> bool {
    if dae_model.symbols.enum_literal_ordinals.contains_key(name) {
        return true;
    }
    let is_const = |candidate: &VarName| {
        dae_model.variables.parameters.contains_key(candidate)
            || dae_model.variables.constants.contains_key(candidate)
    };
    is_const(&VarName::new(name))
        || dae::component_base_name(name)
            .map(|base| is_const(&VarName::new(base)))
            .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BuiltinFunction, Literal, Reference, SourceId, Span, Subscript};

    use super::*;
    fn test_span() -> Span {
        Span::from_offsets(SourceId::from_source_name("event_threshold_test.mo"), 1, 2)
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: test_span(),
        }
    }

    fn pre(expr: Expression) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![expr],
            span: test_span(),
        }
    }

    fn ge(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Ge,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: test_span(),
        }
    }

    fn eq(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Eq,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: test_span(),
        }
    }

    #[test]
    fn time_threshold_accepts_event_constant_expressions() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.parameters.insert(
            VarName::new("deadline"),
            dae::Variable {
                name: VarName::new("deadline"),
                ..dae::Variable::empty_with_span(test_span())
            },
        );
        dae_model.variables.parameters.insert(
            VarName::new("p"),
            dae::Variable {
                name: VarName::new("p"),
                ..dae::Variable::empty_with_span(test_span())
            },
        );
        let indexed_parameter = Expression::VarRef {
            name: Reference::new("p"),
            subscripts: vec![Subscript::generated_index(1, test_span())],
            span: test_span(),
        };

        assert!(is_event_constant_time_threshold_relation(
            &dae_model,
            &ge(var("time"), var("deadline"))
        ));
        assert!(is_event_constant_time_threshold_relation(
            &dae_model,
            &ge(var("time"), pre(var("x")))
        ));
        assert!(is_event_constant_time_threshold_relation(
            &dae_model,
            &ge(var("time"), indexed_parameter)
        ));
    }

    #[test]
    fn time_threshold_rejects_live_or_boolean_thresholds() {
        let dae_model = dae::Dae::default();
        let live_threshold = ge(var("time"), var("x"));
        let boolean_threshold = ge(
            var("time"),
            Expression::Unary {
                op: OpUnary::Not,
                rhs: Box::new(var("x")),
                span: test_span(),
            },
        );
        let indexed_parameter = Expression::VarRef {
            name: Reference::new("p"),
            subscripts: vec![Subscript::generated_index(1, test_span())],
            span: test_span(),
        };

        assert!(!is_event_constant_time_threshold_relation(
            &dae_model,
            &live_threshold
        ));
        assert!(!is_event_constant_time_threshold_relation(
            &dae_model,
            &boolean_threshold
        ));
        assert!(!is_event_constant_time_threshold_relation(
            &dae_model,
            &ge(var("time"), indexed_parameter)
        ));
        assert!(!is_event_constant_time_threshold_relation(
            &dae_model,
            &eq(var("time"), real(1.0))
        ));
        assert!(!is_event_constant_time_threshold_relation(
            &dae_model,
            &ge(var("time"), ge(var("x"), real(0.0)))
        ));
        assert!(!is_event_constant_time_threshold_relation(
            &dae_model,
            &ge(var("time"), pre(ge(var("x"), real(0.0))))
        ));
    }
}
