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
    pub solution_output_index: Option<usize>,
}

struct AssignmentTarget {
    name: String,
    is_pre_target: bool,
}

pub fn initial_assignment_from_equation(eq: &dae::Equation) -> Option<InitialAssignment<'_>> {
    initial_assignments_from_equation(eq).into_iter().next()
}

pub fn initial_assignments_from_equation(eq: &dae::Equation) -> Vec<InitialAssignment<'_>> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return vec![InitialAssignment {
            target: lhs.as_str().to_string(),
            solution: &eq.rhs,
            is_pre_target: false,
            solution_output_index: None,
        }];
    }
    residual_initial_assignments(&eq.rhs)
}

fn residual_initial_assignments(expr: &Expression) -> Vec<InitialAssignment<'_>> {
    match expr {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => residual_sub_initial_assignments(lhs, rhs),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => residual_initial_assignments(rhs),
        _ => Vec::new(),
    }
}

fn residual_sub_initial_assignments<'a>(
    lhs: &'a Expression,
    rhs: &'a Expression,
) -> Vec<InitialAssignment<'a>> {
    if let Some(assignments) = tuple_target_assignments(lhs, rhs) {
        return assignments;
    }
    if let Some(assignments) = tuple_target_assignments(rhs, lhs) {
        return assignments;
    }
    target_expr_name(lhs).map_or_else(
        || {
            target_expr_name(rhs).map_or_else(Vec::new, |target| {
                vec![InitialAssignment {
                    target: target.name,
                    solution: lhs,
                    is_pre_target: target.is_pre_target,
                    solution_output_index: None,
                }]
            })
        },
        |target| {
            vec![InitialAssignment {
                target: target.name,
                solution: rhs,
                is_pre_target: target.is_pre_target,
                solution_output_index: None,
            }]
        },
    )
}

fn tuple_target_assignments<'a>(
    target_expr: &'a Expression,
    solution: &'a Expression,
) -> Option<Vec<InitialAssignment<'a>>> {
    let Expression::Tuple { elements, .. } = target_expr else {
        return None;
    };
    elements
        .iter()
        .enumerate()
        .map(|(idx, element)| {
            let target = target_expr_name(element)?;
            Some(InitialAssignment {
                target: target.name,
                solution,
                is_pre_target: target.is_pre_target,
                solution_output_index: Some(idx),
            })
        })
        .collect()
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
                    subscripts: vec![Subscript::generated_expr(Box::new(Expression::Literal {
                        value: Literal::Integer(2),
                        span: Span::DUMMY,
                    }))],
                    span: Span::DUMMY,
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(7),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            },
            Default::default(),
            "state[2] = 7",
        );

        let assignment =
            initial_assignment_from_equation(&eq).expect("subscripted residual assignment");

        assert_eq!(assignment.target, "state[2]");
        assert!(!assignment.is_pre_target);
        assert_eq!(assignment.solution_output_index, None);
    }

    #[test]
    fn recognizes_tuple_residual_initial_assignments() {
        let call = Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.multi"),
            args: Vec::new(),
            is_constructor: false,
            span: Span::DUMMY,
        };
        let eq = dae::Equation::residual(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(Expression::Tuple {
                    elements: vec![
                        Expression::VarRef {
                            name: Reference::new("a"),
                            subscripts: Vec::new(),
                            span: Span::DUMMY,
                        },
                        Expression::VarRef {
                            name: Reference::new("b"),
                            subscripts: Vec::new(),
                            span: Span::DUMMY,
                        },
                    ],
                    span: Span::DUMMY,
                }),
                rhs: Box::new(call),
                span: Span::DUMMY,
            },
            Default::default(),
            "(a, b) = Pkg.multi()",
        );

        let assignments = super::initial_assignments_from_equation(&eq);

        assert_eq!(assignments.len(), 2);
        assert_eq!(assignments[0].target, "a");
        assert_eq!(assignments[0].solution_output_index, Some(0));
        assert_eq!(assignments[1].target, "b");
        assert_eq!(assignments[1].solution_output_index, Some(1));
    }
}
