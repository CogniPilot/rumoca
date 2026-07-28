use std::collections::HashMap;

use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, OpUnary};
use rumoca_ir_dae::Dae;

pub(crate) fn structural_scalar_bindings(dae: &Dae) -> HashMap<String, f64> {
    let mut bindings = HashMap::new();
    for _ in 0..dae.variables.parameters.len().max(1) {
        let before = bindings.len();
        for (name, var) in dae.variables.constants.iter().chain(
            dae.variables
                .parameters
                .iter()
                .filter(|(_, var)| !var.is_tunable),
        ) {
            let Some(start) = var.start.as_ref() else {
                continue;
            };
            let Some(value) = eval_static_number(start, &bindings) else {
                continue;
            };
            bindings.insert(name.as_str().to_string(), value);
        }
        if bindings.len() == before {
            break;
        }
    }
    bindings
}

pub(crate) fn eval_static_bool(expr: &Expression, bindings: &HashMap<String, f64>) -> Option<bool> {
    match expr {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(*value),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => Some(!eval_static_bool(rhs, bindings)?),
        Expression::BuiltinCall { function, args, .. }
            if matches!(*function, BuiltinFunction::NoEvent) =>
        {
            eval_static_bool(args.first()?, bindings)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_static_bool(condition, bindings)? {
                    return eval_static_bool(value, bindings);
                }
            }
            eval_static_bool(else_branch, bindings)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            match op {
                OpBinary::And => {
                    let lhs = eval_static_bool(lhs, bindings)?;
                    return if lhs {
                        eval_static_bool(rhs, bindings)
                    } else {
                        Some(false)
                    };
                }
                OpBinary::Or => {
                    let lhs = eval_static_bool(lhs, bindings)?;
                    return if lhs {
                        Some(true)
                    } else {
                        eval_static_bool(rhs, bindings)
                    };
                }
                _ => {}
            }
            let lhs = eval_static_number(lhs, bindings)?;
            let rhs = eval_static_number(rhs, bindings)?;
            match op {
                OpBinary::Lt => Some(lhs < rhs),
                OpBinary::Le => Some(lhs <= rhs),
                OpBinary::Gt => Some(lhs > rhs),
                OpBinary::Ge => Some(lhs >= rhs),
                OpBinary::Eq => Some(lhs == rhs),
                OpBinary::Neq => Some(lhs != rhs),
                _ => None,
            }
        }
        _ => eval_static_number(expr, bindings).map(|value| value != 0.0),
    }
}

pub(crate) fn eval_static_number(
    expr: &Expression,
    bindings: &HashMap<String, f64>,
) -> Option<f64> {
    match expr {
        Expression::Literal { value, .. } => match value {
            Literal::Integer(value) => Some(*value as f64),
            Literal::Real(value) => Some(*value),
            Literal::Boolean(value) => Some(if *value { 1.0 } else { 0.0 }),
            _ => None,
        },
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => bindings.get(name.as_str()).copied(),
        Expression::Unary { op, rhs, .. } => {
            let value = eval_static_number(rhs, bindings)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Empty => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                OpUnary::Not => Some(if value == 0.0 { 1.0 } else { 0.0 }),
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_static_bool(condition, bindings)? {
                    return eval_static_number(value, bindings);
                }
            }
            eval_static_number(else_branch, bindings)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_static_number(lhs, bindings)?;
            let rhs = eval_static_number(rhs, bindings)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
                OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
                OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
                OpBinary::Div | OpBinary::DivElem => Some(lhs / rhs),
                OpBinary::Exp | OpBinary::ExpElem => Some(lhs.powf(rhs)),
                OpBinary::Lt => Some(if lhs < rhs { 1.0 } else { 0.0 }),
                OpBinary::Le => Some(if lhs <= rhs { 1.0 } else { 0.0 }),
                OpBinary::Gt => Some(if lhs > rhs { 1.0 } else { 0.0 }),
                OpBinary::Ge => Some(if lhs >= rhs { 1.0 } else { 0.0 }),
                OpBinary::Eq => Some(if lhs == rhs { 1.0 } else { 0.0 }),
                OpBinary::Neq => Some(if lhs != rhs { 1.0 } else { 0.0 }),
                _ => None,
            }
        }
        Expression::BuiltinCall { function, args, .. }
            if matches!(*function, BuiltinFunction::NoEvent) =>
        {
            eval_static_number(args.first()?, bindings)
        }
        Expression::BuiltinCall { function, args, .. }
            if matches!(*function, BuiltinFunction::Floor | BuiltinFunction::Integer) =>
        {
            Some(eval_static_number(args.first()?, bindings)?.floor())
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, Reference, Span, VarName};
    use rumoca_ir_dae::{Dae, Variable};

    use super::{eval_static_bool, eval_static_number, structural_scalar_bindings};

    #[test]
    fn fixed_parameter_start_is_available_to_structural_evaluation() {
        let mut dae = Dae::new();
        let mut parameter = Variable::new(VarName::new("p"), Span::DUMMY);
        parameter.start = Some(Expression::Literal {
            value: Literal::Real(3.0),
            span: Span::DUMMY,
        });
        parameter.is_tunable = false;
        dae.variables
            .parameters
            .insert(VarName::new("p"), parameter);
        let bindings = structural_scalar_bindings(&dae);
        let reference = Expression::VarRef {
            name: Reference::new("p"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        };

        assert_eq!(bindings.get("p"), Some(&3.0));
        assert_eq!(eval_static_number(&reference, &bindings), Some(3.0));
    }

    #[test]
    fn fixed_point_resolves_conditional_integer_parameter() {
        let mut dae = Dae::new();
        for (name, value) in [("kind", 2.0), ("order", 3.0)] {
            let mut parameter = Variable::new(VarName::new(name), Span::DUMMY);
            parameter.start = Some(Expression::Literal {
                value: Literal::Real(value),
                span: Span::DUMMY,
            });
            parameter.is_tunable = false;
            dae.variables
                .parameters
                .insert(VarName::new(name), parameter);
        }
        let reference = |name| Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        };
        let literal = |value| Expression::Literal {
            value: Literal::Integer(value),
            span: Span::DUMMY,
        };
        let mut derived = Variable::new(VarName::new("count"), Span::DUMMY);
        derived.start = Some(Expression::If {
            branches: vec![(
                Expression::Binary {
                    op: OpBinary::Or,
                    lhs: Box::new(Expression::Binary {
                        op: OpBinary::Eq,
                        lhs: Box::new(reference("kind")),
                        rhs: Box::new(literal(1)),
                        span: Span::DUMMY,
                    }),
                    rhs: Box::new(Expression::Binary {
                        op: OpBinary::Eq,
                        lhs: Box::new(reference("kind")),
                        rhs: Box::new(literal(2)),
                        span: Span::DUMMY,
                    }),
                    span: Span::DUMMY,
                },
                reference("order"),
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Integer,
                args: vec![Expression::Binary {
                    op: OpBinary::Div,
                    lhs: Box::new(reference("order")),
                    rhs: Box::new(literal(2)),
                    span: Span::DUMMY,
                }],
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        });
        derived.is_tunable = false;
        dae.variables
            .parameters
            .insert(VarName::new("count"), derived);

        let bindings = structural_scalar_bindings(&dae);

        assert_eq!(bindings.get("count"), Some(&3.0));
    }

    #[test]
    fn static_boolean_short_circuit_does_not_require_unreachable_operand() {
        let unknown = Expression::VarRef {
            name: Reference::new("unknown"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        };
        let bool_literal = |value| Expression::Literal {
            value: Literal::Boolean(value),
            span: Span::DUMMY,
        };
        let binary = |op, lhs, rhs| Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: Span::DUMMY,
        };

        assert_eq!(
            eval_static_bool(
                &binary(OpBinary::And, bool_literal(false), unknown.clone()),
                &HashMap::new(),
            ),
            Some(false)
        );
        assert_eq!(
            eval_static_bool(
                &binary(OpBinary::Or, bool_literal(true), unknown),
                &HashMap::new(),
            ),
            Some(true)
        );
    }
}
