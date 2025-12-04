//! Symbolic Differentiation for DAE Index Reduction
//!
//! This module provides symbolic differentiation of expressions with respect to time,
//! which is needed for Pantelides index reduction algorithm.
//!
//! ## Differentiation Rules
//!
//! - `d/dt(x) = der(x)` for any variable x
//! - `d/dt(constant) = 0`
//! - `d/dt(a + b) = d/dt(a) + d/dt(b)` (sum rule)
//! - `d/dt(a - b) = d/dt(a) - d/dt(b)` (difference rule)
//! - `d/dt(a * b) = a' * b + a * b'` (product rule)
//! - `d/dt(der(x)) = der(der(x))` (higher derivatives)
//!
//! ## References
//!
//! - Pantelides, C. (1988). "The Consistent Initialization of Differential-Algebraic Systems"
//! - Mattsson, S.E. & Söderlind, G. (1993). "Index Reduction in Differential-Algebraic Equations"

use crate::ir::ast::{
    ComponentRefPart, ComponentReference, Equation, Expression, OpBinary, TerminalType, Token,
};

/// Symbolically differentiate an equation with respect to time
///
/// # Arguments
///
/// * `equation` - The equation to differentiate
///
/// # Returns
///
/// The differentiated equation, or `None` if the equation type is not supported
pub fn differentiate_equation(equation: &Equation) -> Option<Equation> {
    if let Equation::Simple { lhs, rhs, .. } = equation {
        let diff_lhs = differentiate_expression(lhs);
        let diff_rhs = differentiate_expression(rhs);

        Some(Equation::Simple {
            lhs: diff_lhs,
            rhs: diff_rhs,
        })
    } else {
        None
    }
}

/// Symbolically differentiate an expression with respect to time
///
/// # Arguments
///
/// * `expr` - The expression to differentiate
///
/// # Returns
///
/// The differentiated expression
pub fn differentiate_expression(expr: &Expression) -> Expression {
    match expr {
        Expression::ComponentReference(cref) => {
            // d/dt(x) = der(x)
            Expression::FunctionCall {
                comp: ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: "der".to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                },
                args: vec![Expression::ComponentReference(cref.clone())],
            }
        }
        Expression::Binary { lhs, op, rhs } => {
            match op {
                OpBinary::Add(_) | OpBinary::Sub(_) => {
                    // d/dt(a + b) = d/dt(a) + d/dt(b)
                    // d/dt(a - b) = d/dt(a) - d/dt(b)
                    Expression::Binary {
                        lhs: Box::new(differentiate_expression(lhs)),
                        op: op.clone(),
                        rhs: Box::new(differentiate_expression(rhs)),
                    }
                }
                OpBinary::Mul(_) => {
                    // Product rule: d/dt(a * b) = a' * b + a * b'
                    let da = differentiate_expression(lhs);
                    let db = differentiate_expression(rhs);
                    Expression::Binary {
                        lhs: Box::new(Expression::Binary {
                            lhs: Box::new(da),
                            op: op.clone(),
                            rhs: rhs.clone(),
                        }),
                        op: OpBinary::Add(Token::default()),
                        rhs: Box::new(Expression::Binary {
                            lhs: lhs.clone(),
                            op: op.clone(),
                            rhs: Box::new(db),
                        }),
                    }
                }
                OpBinary::Div(_) => {
                    // Quotient rule: d/dt(a / b) = (a' * b - a * b') / b^2
                    let da = differentiate_expression(lhs);
                    let db = differentiate_expression(rhs);
                    Expression::Binary {
                        lhs: Box::new(Expression::Binary {
                            lhs: Box::new(Expression::Binary {
                                lhs: Box::new(da),
                                op: OpBinary::Mul(Token::default()),
                                rhs: rhs.clone(),
                            }),
                            op: OpBinary::Sub(Token::default()),
                            rhs: Box::new(Expression::Binary {
                                lhs: lhs.clone(),
                                op: OpBinary::Mul(Token::default()),
                                rhs: Box::new(db),
                            }),
                        }),
                        op: OpBinary::Div(Token::default()),
                        rhs: Box::new(Expression::Binary {
                            lhs: rhs.clone(),
                            op: OpBinary::Mul(Token::default()),
                            rhs: rhs.clone(),
                        }),
                    }
                }
                _ => {
                    // For other operators, return der(expr) as placeholder
                    wrap_in_der(expr)
                }
            }
        }
        Expression::Terminal { terminal_type, .. } => {
            // Constants differentiate to 0
            match terminal_type {
                TerminalType::UnsignedInteger | TerminalType::UnsignedReal => {
                    Expression::Terminal {
                        terminal_type: TerminalType::UnsignedInteger,
                        token: Token {
                            text: "0".to_string(),
                            ..Default::default()
                        },
                    }
                }
                _ => wrap_in_der(expr),
            }
        }
        Expression::FunctionCall { comp, args } => {
            if comp.to_string() == "der" {
                // der(der(x)) = second derivative
                // Wrap in another der call
                Expression::FunctionCall {
                    comp: comp.clone(),
                    args: args.iter().map(differentiate_expression).collect(),
                }
            } else {
                // Chain rule for function calls (simplified - just wraps in der)
                // For proper chain rule, we would need to know the function's derivative
                wrap_in_der(expr)
            }
        }
        Expression::Unary { op, rhs } => {
            // d/dt(-x) = -d/dt(x)
            Expression::Unary {
                op: op.clone(),
                rhs: Box::new(differentiate_expression(rhs)),
            }
        }
        Expression::Array { elements } => {
            // Differentiate each element
            Expression::Array {
                elements: elements.iter().map(differentiate_expression).collect(),
            }
        }
        Expression::Tuple { elements } => {
            // Differentiate each element
            Expression::Tuple {
                elements: elements.iter().map(differentiate_expression).collect(),
            }
        }
        Expression::Range { .. } | Expression::If { .. } | Expression::Empty => {
            // For unsupported expressions, wrap in der()
            wrap_in_der(expr)
        }
        Expression::Parenthesized { inner } => {
            // Differentiate the inner expression and preserve parentheses
            Expression::Parenthesized {
                inner: Box::new(differentiate_expression(inner)),
            }
        }
        Expression::ArrayComprehension { expr, indices } => {
            // Differentiate the expression inside the comprehension
            Expression::ArrayComprehension {
                expr: Box::new(differentiate_expression(expr)),
                indices: indices.clone(),
            }
        }
    }
}

/// Wrap an expression in a der() call
fn wrap_in_der(expr: &Expression) -> Expression {
    Expression::FunctionCall {
        comp: ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: "der".to_string(),
                    ..Default::default()
                },
                subs: None,
            }],
        },
        args: vec![expr.clone()],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::OpUnary;

    fn make_var(name: &str) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: name.to_string(),
                    ..Default::default()
                },
                subs: None,
            }],
        })
    }

    fn make_der(var: Expression) -> Expression {
        Expression::FunctionCall {
            comp: ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: "der".to_string(),
                        ..Default::default()
                    },
                    subs: None,
                }],
            },
            args: vec![var],
        }
    }

    fn make_const(val: &str) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: val.to_string(),
                ..Default::default()
            },
        }
    }

    #[test]
    fn test_differentiate_variable() {
        // d/dt(x) should give der(x)
        let expr = make_var("x");
        let diff = differentiate_expression(&expr);

        if let Expression::FunctionCall { comp, args } = diff {
            assert_eq!(comp.to_string(), "der");
            assert_eq!(args.len(), 1);
        } else {
            panic!("Expected function call");
        }
    }

    #[test]
    fn test_differentiate_constant() {
        // d/dt(5) = 0
        let expr = make_const("5");
        let diff = differentiate_expression(&expr);

        if let Expression::Terminal { token, .. } = diff {
            assert_eq!(token.text, "0");
        } else {
            panic!("Expected terminal");
        }
    }

    #[test]
    fn test_differentiate_sum() {
        // d/dt(x + y) = der(x) + der(y)
        let expr = Expression::Binary {
            lhs: Box::new(make_var("x")),
            op: OpBinary::Add(Token::default()),
            rhs: Box::new(make_var("y")),
        };

        let diff = differentiate_expression(&expr);

        if let Expression::Binary {
            op: OpBinary::Add(_),
            ..
        } = diff
        {
            // Success - preserves addition structure
        } else {
            panic!("Expected binary addition");
        }
    }

    #[test]
    fn test_differentiate_product() {
        // d/dt(x * y) = der(x) * y + x * der(y)
        let expr = Expression::Binary {
            lhs: Box::new(make_var("x")),
            op: OpBinary::Mul(Token::default()),
            rhs: Box::new(make_var("y")),
        };

        let diff = differentiate_expression(&expr);

        // Result should be (der(x) * y) + (x * der(y))
        if let Expression::Binary {
            op: OpBinary::Add(_),
            ..
        } = diff
        {
            // Success - product rule applied
        } else {
            panic!("Expected binary addition from product rule");
        }
    }

    #[test]
    fn test_differentiate_negation() {
        // d/dt(-x) = -der(x)
        let expr = Expression::Unary {
            op: OpUnary::Minus(Token::default()),
            rhs: Box::new(make_var("x")),
        };

        let diff = differentiate_expression(&expr);

        if let Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } = diff
        {
            // Should be -der(x)
            if let Expression::FunctionCall { comp, .. } = *rhs {
                assert_eq!(comp.to_string(), "der");
            } else {
                panic!("Expected der() inside negation");
            }
        } else {
            panic!("Expected unary negation");
        }
    }

    #[test]
    fn test_differentiate_der() {
        // d/dt(der(x)) = der(der(x))
        let expr = make_der(make_var("x"));
        let diff = differentiate_expression(&expr);

        // Result should be der(der(x))
        if let Expression::FunctionCall { comp, args } = diff {
            assert_eq!(comp.to_string(), "der");
            if let Expression::FunctionCall {
                comp: inner_comp, ..
            } = &args[0]
            {
                assert_eq!(inner_comp.to_string(), "der");
            } else {
                panic!("Expected nested der()");
            }
        } else {
            panic!("Expected function call");
        }
    }

    #[test]
    fn test_differentiate_equation() {
        // d/dt(x = y) should give der(x) = der(y)
        let eq = Equation::Simple {
            lhs: make_var("x"),
            rhs: make_var("y"),
        };

        let diff_eq = differentiate_equation(&eq);
        assert!(diff_eq.is_some());

        if let Some(Equation::Simple { lhs, rhs }) = diff_eq {
            // Both sides should have der()
            assert!(matches!(lhs, Expression::FunctionCall { .. }));
            assert!(matches!(rhs, Expression::FunctionCall { .. }));
        }
    }
}
