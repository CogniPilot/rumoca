//! Equation Causalization
//!
//! This module implements causalization of equations - rewriting equations so that
//! a specific variable is isolated on the left-hand side.
//!
//! Causalization is essential for BLT transformation because once we know which
//! variable each equation should solve for (via matching), we need to algebraically
//! rearrange the equation to compute that variable.

use crate::ir::ast::{
    ComponentRefPart, ComponentReference, Equation, Expression, OpBinary, OpUnary, TerminalType,
    Token,
};
use crate::ir::visitor::{Visitable, Visitor};

/// Visitor to find der() calls in an expression
struct DerivativeFinder {
    derivatives: Vec<String>,
}

impl DerivativeFinder {
    fn new() -> Self {
        Self {
            derivatives: Vec::new(),
        }
    }
}

impl Visitor for DerivativeFinder {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node
            && comp.to_string() == "der"
            && !args.is_empty()
            && let Expression::ComponentReference(cref) = &args[0]
        {
            self.derivatives.push(cref.to_string());
        }
    }
}

/// Check if expression contains a der() call
pub fn has_der_call(expr: &Expression) -> bool {
    let mut finder = DerivativeFinder::new();
    expr.accept(&mut finder);
    !finder.derivatives.is_empty()
}

/// Check if equation needs LHS/RHS swap (der() on RHS but not on LHS)
pub(super) fn check_if_needs_swap(lhs: &Expression, rhs: &Expression) -> bool {
    let lhs_has_der = has_der_call(lhs);
    let rhs_has_der = has_der_call(rhs);

    // Swap if RHS has der() but LHS doesn't
    !lhs_has_der && rhs_has_der
}

/// Normalize derivative equations of the form `coeff * der(x) = expr` to `der(x) = expr / coeff`
///
/// This handles cases from component models like:
/// - `C * der(v) = i` (capacitor) -> `der(v) = i / C`
/// - `L * der(i) = v` (inductor) -> `der(i) = v / L`
///
/// Returns None if the equation is not of this form.
pub(super) fn normalize_derivative_equation(
    lhs: &Expression,
    rhs: &Expression,
) -> Option<Equation> {
    // Check if LHS is coeff * der(x) or der(x) * coeff
    if let Expression::Binary {
        op: OpBinary::Mul(_),
        lhs: mult_lhs,
        rhs: mult_rhs,
    } = lhs
    {
        // Case 1: coeff * der(x)
        if let Expression::FunctionCall { comp, args } = mult_rhs.as_ref()
            && comp.to_string() == "der"
            && args.len() == 1
        {
            // Extract der(x) and coefficient
            let der_expr = mult_rhs.as_ref().clone();
            let coeff = mult_lhs.as_ref().clone();
            return Some(Equation::Simple {
                lhs: der_expr,
                rhs: Expression::Binary {
                    op: OpBinary::Div(Token::default()),
                    lhs: Box::new(rhs.clone()),
                    rhs: Box::new(coeff),
                },
            });
        }

        // Case 2: der(x) * coeff
        if let Expression::FunctionCall { comp, args } = mult_lhs.as_ref()
            && comp.to_string() == "der"
            && args.len() == 1
        {
            // Extract der(x) and coefficient
            let der_expr = mult_lhs.as_ref().clone();
            let coeff = mult_rhs.as_ref().clone();
            return Some(Equation::Simple {
                lhs: der_expr,
                rhs: Expression::Binary {
                    op: OpBinary::Div(Token::default()),
                    lhs: Box::new(rhs.clone()),
                    rhs: Box::new(coeff),
                },
            });
        }
    }

    None
}

/// Causalize an equation by solving for a specific variable.
///
/// Given an equation `lhs = rhs` and a variable to solve for, this function
/// attempts to algebraically rewrite the equation so the variable is isolated
/// on the left-hand side.
///
/// # Limitations
///
/// This function only handles **linear equations** where the variable appears
/// with coefficient ±1 or as a simple multiplication factor. It does **not** handle:
/// - Nonlinear occurrences (e.g., `x^2`, `sin(x)`, `x * y` where solving for `x`)
/// - Multiple occurrences of the same variable (e.g., `x + 2*x = 3`)
/// - Variables inside function calls (e.g., `f(x) = y`)
///
/// For unsupported equation forms, returns `None` and the equation remains
/// in its original form (may require numerical solving in algebraic loops).
///
/// # Supported Forms
///
/// - `var = expr` → already causalized, returns None
/// - `expr = var` → `var = expr`
/// - `a + b = 0` → `a = -b`
/// - `a + b + c = 0` → `a = -(b + c)`
/// - `coeff * var = expr` → `var = expr / coeff`
/// - `a + b = c` → `a = c - b`
///
/// # Returns
///
/// - `Some(equation)` if the equation was successfully causalized
/// - `None` if the equation is already in correct form or cannot be causalized
pub(super) fn causalize_equation(
    lhs: &Expression,
    rhs: &Expression,
    solve_for: &str,
) -> Option<Equation> {
    // Check if LHS is already just the variable we're solving for
    if let Expression::ComponentReference(cref) = lhs
        && cref.to_string() == solve_for
    {
        return None; // Already in correct form
    }

    // Check if RHS is just the variable we're solving for - swap if so
    // E.g., expr = var => var = expr
    if let Expression::ComponentReference(cref) = rhs
        && cref.to_string() == solve_for
    {
        return Some(Equation::Simple {
            lhs: rhs.clone(),
            rhs: lhs.clone(),
        });
    }

    // Helper to check if an expression is zero
    let is_zero = |expr: &Expression| -> bool {
        match expr {
            Expression::Terminal { token, .. } => token.text == "0" || token.text == "0.0",
            _ => false,
        }
    };

    // Check if RHS is zero (common case for KCL equations: a + b + c = 0)
    let rhs_is_zero = is_zero(rhs);
    // Also check if LHS is zero (alternate form: 0 = a + b + c)
    let lhs_is_zero = is_zero(lhs);

    if rhs_is_zero {
        // Equation is: lhs = 0, where lhs is a sum
        // We need to solve for `solve_for`: solve_for = -(other terms)
        if let Some((coeff, other_terms)) = extract_linear_term(lhs, solve_for) {
            // If coeff is 1: solve_for = -other_terms
            // If coeff is -1: solve_for = other_terms
            let new_rhs = if coeff > 0.0 {
                // solve_for + other = 0 => solve_for = -other
                negate_expression(&other_terms)
            } else {
                // -solve_for + other = 0 => solve_for = other
                other_terms
            };

            return Some(Equation::Simple {
                lhs: Expression::ComponentReference(ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: solve_for.to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                }),
                rhs: new_rhs,
            });
        }
    }

    if lhs_is_zero {
        // Equation is: 0 = rhs, where rhs is a sum (alternate form of KCL equations)
        // We need to solve for `solve_for`: solve_for = -(other terms)
        if let Some((coeff, other_terms)) = extract_linear_term(rhs, solve_for) {
            // If coeff is 1: solve_for = -other_terms
            // If coeff is -1: solve_for = other_terms
            let new_rhs = if coeff > 0.0 {
                // 0 = solve_for + other => solve_for = -other
                negate_expression(&other_terms)
            } else {
                // 0 = -solve_for + other => solve_for = other
                other_terms
            };

            return Some(Equation::Simple {
                lhs: Expression::ComponentReference(ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: solve_for.to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                }),
                rhs: new_rhs,
            });
        }
    }

    // Handle case: coeff * var = expr (multiplication on LHS)
    // E.g., R * i = v => solving for i gives i = v / R
    if let Expression::Binary {
        op: OpBinary::Mul(_),
        lhs: mult_lhs,
        rhs: mult_rhs,
    } = lhs
    {
        // Check if solve_for is on the right side of multiplication: coeff * var
        if let Expression::ComponentReference(cref) = mult_rhs.as_ref()
            && cref.to_string() == solve_for
        {
            return Some(Equation::Simple {
                lhs: Expression::ComponentReference(ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: solve_for.to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                }),
                rhs: Expression::Binary {
                    op: OpBinary::Div(Token::default()),
                    lhs: Box::new(rhs.clone()),
                    rhs: mult_lhs.clone(),
                },
            });
        }
        // Check if solve_for is on the left side of multiplication: var * coeff
        if let Expression::ComponentReference(cref) = mult_lhs.as_ref()
            && cref.to_string() == solve_for
        {
            return Some(Equation::Simple {
                lhs: Expression::ComponentReference(ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: solve_for.to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                }),
                rhs: Expression::Binary {
                    op: OpBinary::Div(Token::default()),
                    lhs: Box::new(rhs.clone()),
                    rhs: mult_rhs.clone(),
                },
            });
        }
    }

    // Handle case: lhs = rhs where lhs contains solve_for
    // E.g., a + b = c => solving for a gives a = c - b
    if let Some((coeff, other_terms)) = extract_linear_term(lhs, solve_for) {
        // lhs contains solve_for: coeff*solve_for + other_terms = rhs
        // => solve_for = (rhs - other_terms) / coeff
        let rhs_minus_other = if is_zero_expression(&other_terms) {
            rhs.clone()
        } else {
            Expression::Binary {
                op: OpBinary::Sub(Token::default()),
                lhs: Box::new(rhs.clone()),
                rhs: Box::new(other_terms),
            }
        };

        let new_rhs = if (coeff - 1.0).abs() < 1e-10 {
            rhs_minus_other
        } else if (coeff + 1.0).abs() < 1e-10 {
            negate_expression(&rhs_minus_other)
        } else {
            // General case: divide by coefficient
            Expression::Binary {
                op: OpBinary::Div(Token::default()),
                lhs: Box::new(rhs_minus_other),
                rhs: Box::new(Expression::Terminal {
                    terminal_type: TerminalType::UnsignedReal,
                    token: Token {
                        text: coeff.to_string(),
                        ..Default::default()
                    },
                }),
            }
        };

        return Some(Equation::Simple {
            lhs: Expression::ComponentReference(ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: solve_for.to_string(),
                        ..Default::default()
                    },
                    subs: None,
                }],
            }),
            rhs: new_rhs,
        });
    }

    // Handle case: lhs = rhs where rhs contains solve_for
    // E.g., a = b - c where solving for b gives b = a + c
    if let Some((coeff, other_terms)) = extract_linear_term(rhs, solve_for) {
        // rhs contains solve_for: lhs = coeff*solve_for + other_terms
        // => solve_for = (lhs - other_terms) / coeff
        let lhs_minus_other = if is_zero_expression(&other_terms) {
            lhs.clone()
        } else {
            Expression::Binary {
                op: OpBinary::Sub(Token::default()),
                lhs: Box::new(lhs.clone()),
                rhs: Box::new(other_terms),
            }
        };

        let new_rhs = if (coeff - 1.0).abs() < 1e-10 {
            lhs_minus_other
        } else if (coeff + 1.0).abs() < 1e-10 {
            negate_expression(&lhs_minus_other)
        } else {
            // General case: divide by coefficient
            Expression::Binary {
                op: OpBinary::Div(Token::default()),
                lhs: Box::new(lhs_minus_other),
                rhs: Box::new(Expression::Terminal {
                    terminal_type: TerminalType::UnsignedReal,
                    token: Token {
                        text: coeff.to_string(),
                        ..Default::default()
                    },
                }),
            }
        };

        return Some(Equation::Simple {
            lhs: Expression::ComponentReference(ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: solve_for.to_string(),
                        ..Default::default()
                    },
                    subs: None,
                }],
            }),
            rhs: new_rhs,
        });
    }

    None
}

/// Check if an expression is effectively zero
fn is_zero_expression(expr: &Expression) -> bool {
    match expr {
        Expression::Terminal { token, .. } => token.text == "0" || token.text == "0.0",
        _ => false,
    }
}

/// Negate an expression: expr -> -expr or -(expr)
fn negate_expression(expr: &Expression) -> Expression {
    // Handle simple cases to produce cleaner output
    match expr {
        // -(-x) = x
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => (**rhs).clone(),
        // -(a - b) = b - a
        Expression::Binary {
            op: OpBinary::Sub(_),
            lhs,
            rhs,
        } => Expression::Binary {
            op: OpBinary::Sub(Token::default()),
            lhs: rhs.clone(),
            rhs: lhs.clone(),
        },
        // For other expressions, just negate
        _ => Expression::Unary {
            op: OpUnary::Minus(Token::default()),
            rhs: Box::new(expr.clone()),
        },
    }
}

/// Extract the coefficient and remaining terms for a variable in a linear expression.
///
/// Given an expression like `a + b + c` and variable `a`, returns `(1.0, b + c)`.
/// Given an expression like `-a + b` and variable `a`, returns `(-1.0, b)`.
///
/// Returns None if the variable is not found or appears nonlinearly.
fn extract_linear_term(expr: &Expression, var_name: &str) -> Option<(f64, Expression)> {
    match expr {
        Expression::ComponentReference(cref) => {
            if cref.to_string() == var_name {
                // Just the variable itself: coefficient is 1, no other terms
                Some((
                    1.0,
                    Expression::Terminal {
                        terminal_type: TerminalType::UnsignedReal,
                        token: Token {
                            text: "0".to_string(),
                            ..Default::default()
                        },
                    },
                ))
            } else {
                None // Variable not found in this expression
            }
        }
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => {
            // -expr: check if rhs is the variable
            if let Expression::ComponentReference(cref) = rhs.as_ref()
                && cref.to_string() == var_name
            {
                return Some((
                    -1.0,
                    Expression::Terminal {
                        terminal_type: TerminalType::UnsignedReal,
                        token: Token {
                            text: "0".to_string(),
                            ..Default::default()
                        },
                    },
                ));
            }
            // Recursively check inside the negation
            if let Some((coeff, other)) = extract_linear_term(rhs, var_name) {
                Some((-coeff, negate_expression(&other)))
            } else {
                None
            }
        }
        Expression::Binary {
            op: OpBinary::Add(_),
            lhs,
            rhs,
        } => {
            // a + b: check both sides
            if let Some((coeff, other_from_lhs)) = extract_linear_term(lhs, var_name) {
                // Variable found in lhs
                let combined_other = if is_zero_expression(&other_from_lhs) {
                    (**rhs).clone()
                } else {
                    Expression::Binary {
                        op: OpBinary::Add(Token::default()),
                        lhs: Box::new(other_from_lhs),
                        rhs: rhs.clone(),
                    }
                };
                Some((coeff, combined_other))
            } else if let Some((coeff, other_from_rhs)) = extract_linear_term(rhs, var_name) {
                // Variable found in rhs
                let combined_other = if is_zero_expression(&other_from_rhs) {
                    (**lhs).clone()
                } else {
                    Expression::Binary {
                        op: OpBinary::Add(Token::default()),
                        lhs: lhs.clone(),
                        rhs: Box::new(other_from_rhs),
                    }
                };
                Some((coeff, combined_other))
            } else {
                None
            }
        }
        Expression::Binary {
            op: OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            // a - b: check both sides
            if let Some((coeff, other_from_lhs)) = extract_linear_term(lhs, var_name) {
                // Variable found in lhs: (coeff*var + other) - rhs
                let combined_other = if is_zero_expression(&other_from_lhs) {
                    negate_expression(rhs)
                } else {
                    Expression::Binary {
                        op: OpBinary::Sub(Token::default()),
                        lhs: Box::new(other_from_lhs),
                        rhs: rhs.clone(),
                    }
                };
                Some((coeff, combined_other))
            } else if let Some((coeff, other_from_rhs)) = extract_linear_term(rhs, var_name) {
                // Variable found in rhs: lhs - (coeff*var + other)
                // = lhs - coeff*var - other
                // = -coeff*var + (lhs - other)
                let combined_other = if is_zero_expression(&other_from_rhs) {
                    (**lhs).clone()
                } else {
                    Expression::Binary {
                        op: OpBinary::Sub(Token::default()),
                        lhs: lhs.clone(),
                        rhs: Box::new(other_from_rhs),
                    }
                };
                Some((-coeff, combined_other))
            } else {
                None
            }
        }
        _ => None, // Other expression types not handled
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    fn make_zero() -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token: Token {
                text: "0".to_string(),
                ..Default::default()
            },
        }
    }

    #[test]
    fn test_causalize_already_causal() {
        // x = y (already in correct form for solving for x)
        let lhs = make_var("x");
        let rhs = make_var("y");

        let result = causalize_equation(&lhs, &rhs, "x");
        assert!(result.is_none(), "Already causal, should return None");
    }

    #[test]
    fn test_causalize_sum_to_zero() {
        // a + b = 0 => solving for a gives a = -b
        let lhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };
        let rhs = make_zero();

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(result.is_some());

        if let Some(Equation::Simple { lhs, rhs: _ }) = result {
            // LHS should be just "a"
            assert!(
                matches!(lhs, Expression::ComponentReference(_)),
                "LHS should be a simple variable"
            );
        } else {
            panic!("Expected Simple equation");
        }
    }

    #[test]
    fn test_causalize_three_term_sum() {
        // a + b + c = 0 => solving for a gives a = -(b + c)
        let inner_sum = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };
        let lhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(inner_sum),
            rhs: Box::new(make_var("c")),
        };
        let rhs = make_zero();

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(result.is_some(), "Should be able to solve for 'a'");
    }

    #[test]
    fn test_causalize_zero_on_lhs() {
        // 0 = a + b => solving for a gives a = -b
        let lhs = make_zero();
        let rhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(result.is_some(), "Should handle zero on LHS");
    }
}
