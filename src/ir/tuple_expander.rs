//! Tuple equation expander
//!
//! This visitor expands tuple equations like `(a, b) = (expr1, expr2)` into
//! separate equations `a = expr1` and `b = expr2`.

use crate::ir::ast::{ClassDefinition, Equation, Expression};

/// Expand all tuple equations in a class definition into individual equations.
///
/// Given an equation like `(a, b) = func(x)` where the RHS has been inlined to
/// a tuple `(sin(x), cos(x))`, this expands it into two equations:
/// - `a = sin(x)`
/// - `b = cos(x)`
pub fn expand_tuple_equations(class: &mut ClassDefinition) {
    let mut new_equations = Vec::new();

    for eq in &class.equations {
        if let Equation::Simple { lhs, rhs } = eq {
            if let (
                Expression::Tuple {
                    elements: lhs_elems,
                },
                Expression::Tuple {
                    elements: rhs_elems,
                },
            ) = (lhs, rhs)
            {
                // Both sides are tuples - expand into individual equations
                if lhs_elems.len() == rhs_elems.len() {
                    for (l, r) in lhs_elems.iter().zip(rhs_elems.iter()) {
                        new_equations.push(Equation::Simple {
                            lhs: l.clone(),
                            rhs: r.clone(),
                        });
                    }
                } else {
                    // Mismatched tuple sizes - keep the original equation
                    // (will be caught as an error later)
                    new_equations.push(eq.clone());
                }
            } else {
                // Not a tuple equation - keep as is
                new_equations.push(eq.clone());
            }
        } else {
            // Not a Simple equation - keep as is
            new_equations.push(eq.clone());
        }
    }

    class.equations = new_equations;

    // Also process initial equations
    let mut new_initial_equations = Vec::new();
    for eq in &class.initial_equations {
        if let Equation::Simple { lhs, rhs } = eq {
            if let (
                Expression::Tuple {
                    elements: lhs_elems,
                },
                Expression::Tuple {
                    elements: rhs_elems,
                },
            ) = (lhs, rhs)
            {
                if lhs_elems.len() == rhs_elems.len() {
                    for (l, r) in lhs_elems.iter().zip(rhs_elems.iter()) {
                        new_initial_equations.push(Equation::Simple {
                            lhs: l.clone(),
                            rhs: r.clone(),
                        });
                    }
                } else {
                    new_initial_equations.push(eq.clone());
                }
            } else {
                new_initial_equations.push(eq.clone());
            }
        } else {
            new_initial_equations.push(eq.clone());
        }
    }

    class.initial_equations = new_initial_equations;
}
