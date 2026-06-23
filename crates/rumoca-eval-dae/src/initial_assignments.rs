//! DAE-level recognition of direct initial-equation assignments.
//!
//! This module identifies the solved-form subset used to seed runtime vectors.
//! It deliberately returns DAE/core terms only; solve-vector layout and storage
//! remain the responsibility of the Solve IR lowering boundary.

use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Subscript};
use rumoca_ir_dae as dae;

#[derive(Clone)]
pub struct InitialAssignment<'a> {
    pub target: String,
    pub solution: &'a Expression,
    pub is_pre_target: bool,
}

struct AssignmentTarget {
    name: String,
    is_pre_target: bool,
}

pub fn initial_assignment_from_equation(eq: &dae::Equation) -> Option<InitialAssignment<'_>> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return Some(InitialAssignment {
            target: lhs.as_str().to_string(),
            solution: &eq.rhs,
            is_pre_target: false,
        });
    }
    residual_initial_assignment(&eq.rhs)
}

fn residual_initial_assignment(expr: &Expression) -> Option<InitialAssignment<'_>> {
    match expr {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => target_expr_name(lhs).map_or_else(
            || {
                target_expr_name(rhs).map(|target| InitialAssignment {
                    target: target.name,
                    solution: lhs,
                    is_pre_target: target.is_pre_target,
                })
            },
            |target| {
                Some(InitialAssignment {
                    target: target.name,
                    solution: rhs,
                    is_pre_target: target.is_pre_target,
                })
            },
        ),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => residual_initial_assignment(rhs),
        _ => None,
    }
}

fn target_expr_name(expr: &Expression) -> Option<AssignmentTarget> {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
            ..
        } => {
            let target = target_expr_name(args.first()?)?;
            Some(AssignmentTarget {
                name: target.name,
                is_pre_target: true,
            })
        }
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let name = indexed_target_name(name.as_str(), subscripts)?;
            Some(AssignmentTarget {
                name,
                is_pre_target: false,
            })
        }
        _ => None,
    }
}

fn indexed_target_name(name: &str, subscripts: &[Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let index = match subscript {
            Subscript::Index { value: index, .. } => *index,
            Subscript::Expr { expr, .. } => literal_integer_expr(expr)?,
            Subscript::Colon { .. } => return None,
        };
        if index <= 0 {
            return None;
        }
        indices.push(usize::try_from(index).ok()?);
    }
    Some(dae::format_subscript_key(name, &indices))
}

fn literal_integer_expr(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::initial_assignment_from_equation;
    use rumoca_core::{Expression, Literal, OpBinary, Reference, Span, Subscript};
    use rumoca_ir_dae as dae;

    #[test]
    fn recognizes_subscripted_residual_initial_assignment() {
        let eq = dae::Equation::residual(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: Reference::new("state"),
                    subscripts: vec![Subscript::generated_expr(
                        Box::new(Expression::Literal {
                            value: Literal::Integer(2),
                            span: Span::DUMMY,
                        }),
                        rumoca_core::Span::DUMMY,
                    )],
                    span: Span::DUMMY,
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(7),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            "state[2] = 7",
        );

        let assignment =
            initial_assignment_from_equation(&eq).expect("subscripted residual assignment");

        assert_eq!(assignment.target, "state[2]");
        assert!(!assignment.is_pre_target);
    }
}
