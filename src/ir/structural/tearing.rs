//! Tearing Algorithm for Algebraic Loops
//!
//! Large algebraic loops (SCCs with multiple equations) can be expensive to solve
//! as coupled nonlinear systems. Tearing selects a subset of variables ("tearing
//! variables") that, when guessed, allow the remaining equations to be solved
//! sequentially.
//!
//! ## Benefits
//!
//! - Reduces size of nonlinear system to solve
//! - Improves convergence in Newton iterations
//! - Can exploit sparsity in large systems
//!
//! ## Algorithm
//!
//! This implements a greedy heuristic:
//! 1. Build incidence matrix for equations in the loop
//! 2. Iteratively find equations with single unknown variable (can be solved directly)
//! 3. When stuck, select tearing variable (appears in most unsolved equations)
//! 4. Repeat until all equations are processed
//!
//! ## References
//!
//! - Elmqvist, H. & Otter, M. (1994). "Methods for Tearing Systems of Equations"
//! - Cellier, F.E. & Kofman, E. (2006). "Continuous System Simulation", Chapter 9

use super::AlgebraicLoop;
use crate::ir::ast::{ComponentReference, Equation, Expression};
use crate::ir::visitor::{Visitable, Visitor};
use std::collections::{HashMap, HashSet};

/// Visitor to find all variables referenced in an expression.
struct VariableFinder {
    variables: HashSet<String>,
    skip_next_cref: bool,
}

impl VariableFinder {
    fn new() -> Self {
        Self {
            variables: HashSet::new(),
            skip_next_cref: false,
        }
    }
}

impl Visitor for VariableFinder {
    fn enter_expression(&mut self, node: &Expression) {
        if matches!(node, Expression::FunctionCall { .. }) {
            self.skip_next_cref = true;
        }
    }

    fn enter_component_reference(&mut self, node: &ComponentReference) {
        if self.skip_next_cref {
            self.skip_next_cref = false;
        } else {
            self.variables.insert(node.to_string());
        }
    }
}

/// Extract variables from an equation
fn get_equation_variables(equation: &Equation) -> HashSet<String> {
    let mut variables = HashSet::new();

    if let Equation::Simple { lhs, rhs, .. } = equation {
        let mut finder = VariableFinder::new();
        lhs.accept(&mut finder);
        rhs.accept(&mut finder);
        variables = finder.variables;
    }

    variables
}

/// Apply tearing to an algebraic loop to reduce the size of the nonlinear system
///
/// Tearing selects a subset of variables (tearing variables) such that:
/// 1. If tearing variables are known, remaining equations can be solved sequentially
/// 2. The number of tearing variables is minimized (greedy heuristic)
///
/// # Arguments
///
/// * `equations` - All equations in the system
/// * `eq_indices` - Indices of equations in this algebraic loop
/// * `variables` - Variables involved in this loop
///
/// # Returns
///
/// An `AlgebraicLoop` struct with tearing information
pub fn tear_algebraic_loop(
    equations: &[Equation],
    eq_indices: &[usize],
    variables: &HashSet<String>,
) -> AlgebraicLoop {
    let n = eq_indices.len();

    if n <= 1 {
        // Single equation or empty - no tearing needed
        return AlgebraicLoop {
            equation_indices: eq_indices.to_vec(),
            variables: variables.clone(),
            tearing_variables: Vec::new(),
            residual_variables: variables.iter().cloned().collect(),
            size: n,
        };
    }

    // Build incidence matrix for the loop
    let vars: Vec<String> = variables.iter().cloned().collect();
    let var_to_idx: HashMap<&String, usize> =
        vars.iter().enumerate().map(|(i, v)| (v, i)).collect();

    let mut incidence: Vec<HashSet<usize>> = Vec::new();
    for &eq_idx in eq_indices {
        if eq_idx < equations.len() {
            let eq_vars = get_equation_variables(&equations[eq_idx]);
            let var_indices: HashSet<usize> = eq_vars
                .iter()
                .filter_map(|v| var_to_idx.get(v).copied())
                .collect();
            incidence.push(var_indices);
        }
    }

    // Greedy tearing: iteratively select tearing variables
    let mut tearing_vars: Vec<usize> = Vec::new();
    let mut solved_eqs: HashSet<usize> = HashSet::new();
    let mut solved_vars: HashSet<usize> = HashSet::new();

    // Repeat until all equations are solved
    while solved_eqs.len() < n {
        // Find equations that can be solved (have exactly one unknown variable)
        let mut made_progress = false;

        for (local_idx, var_set) in incidence.iter().enumerate() {
            if solved_eqs.contains(&local_idx) {
                continue;
            }

            // Count unsolved variables in this equation
            let unsolved: Vec<usize> = var_set
                .iter()
                .filter(|v| !solved_vars.contains(v))
                .copied()
                .collect();

            if unsolved.len() == 1 {
                // Can solve for this variable
                solved_eqs.insert(local_idx);
                solved_vars.insert(unsolved[0]);
                made_progress = true;
            }
        }

        if !made_progress {
            // Need to select a tearing variable
            // Heuristic: pick variable that appears in most unsolved equations
            let mut var_counts: HashMap<usize, usize> = HashMap::new();
            for (local_idx, var_set) in incidence.iter().enumerate() {
                if !solved_eqs.contains(&local_idx) {
                    for &var_idx in var_set {
                        if !solved_vars.contains(&var_idx) {
                            *var_counts.entry(var_idx).or_insert(0) += 1;
                        }
                    }
                }
            }

            if let Some((&best_var, _)) = var_counts.iter().max_by_key(|&(_, count)| *count) {
                tearing_vars.push(best_var);
                solved_vars.insert(best_var);
            } else {
                // No progress possible - structurally singular
                break;
            }
        }
    }

    // Convert indices back to variable names
    let tearing_variables: Vec<String> = tearing_vars
        .iter()
        .filter_map(|&idx| vars.get(idx).cloned())
        .collect();

    let residual_variables: Vec<String> = (0..vars.len())
        .filter(|idx| !tearing_vars.contains(idx))
        .filter_map(|idx| vars.get(idx).cloned())
        .collect();

    AlgebraicLoop {
        equation_indices: eq_indices.to_vec(),
        variables: variables.clone(),
        tearing_variables,
        residual_variables,
        size: n,
    }
}

/// Analyze algebraic loops in a BLT-ordered equation set
///
/// After BLT transformation, equations are ordered and grouped into SCCs.
/// This function identifies loops (SCCs with size > 1) and applies tearing.
///
/// # Arguments
///
/// * `equations` - All equations in the system
/// * `sccs` - Strongly connected components from Tarjan's algorithm
///
/// # Returns
///
/// A vector of `AlgebraicLoop` structs for each loop found
pub fn analyze_algebraic_loops(equations: &[Equation], sccs: &[Vec<usize>]) -> Vec<AlgebraicLoop> {
    let mut loops = Vec::new();

    for scc in sccs {
        if scc.len() > 1 {
            // This is an algebraic loop
            let mut loop_vars = HashSet::new();

            for &eq_idx in scc {
                if eq_idx < equations.len() {
                    let eq_vars = get_equation_variables(&equations[eq_idx]);
                    loop_vars.extend(eq_vars);
                }
            }

            let torn_loop = tear_algebraic_loop(equations, scc, &loop_vars);
            loops.push(torn_loop);
        }
    }

    loops
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, OpBinary, TerminalType, Token};

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

    fn make_const(val: &str) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: val.to_string(),
                ..Default::default()
            },
        }
    }

    fn make_mul(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op: OpBinary::Mul(Token::default()),
            rhs: Box::new(rhs),
        }
    }

    fn make_add(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op: OpBinary::Add(Token::default()),
            rhs: Box::new(rhs),
        }
    }

    fn make_sub(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op: OpBinary::Sub(Token::default()),
            rhs: Box::new(rhs),
        }
    }

    #[test]
    fn test_tearing_simple_loop() {
        // Two equations, two variables: x = y + 1, y = x + 1
        // These form an algebraic loop
        let equations = vec![
            Equation::Simple {
                lhs: make_var("x"),
                rhs: make_add(make_var("y"), make_const("1")),
            },
            Equation::Simple {
                lhs: make_var("y"),
                rhs: make_add(make_var("x"), make_const("1")),
            },
        ];

        let variables: HashSet<String> = ["x".to_string(), "y".to_string()].into_iter().collect();

        let loop_info = tear_algebraic_loop(&equations, &[0, 1], &variables);

        assert_eq!(loop_info.size, 2);
        // Should identify one tearing variable
        assert!(!loop_info.tearing_variables.is_empty());
    }

    #[test]
    fn test_tearing_single_equation() {
        // Single equation - no tearing needed
        let equations = vec![Equation::Simple {
            lhs: make_var("x"),
            rhs: make_const("1"),
        }];

        let variables: HashSet<String> = ["x".to_string()].into_iter().collect();

        let loop_info = tear_algebraic_loop(&equations, &[0], &variables);

        assert_eq!(loop_info.size, 1);
        assert!(loop_info.tearing_variables.is_empty());
    }

    #[test]
    fn test_algebraic_loop_in_pendulum() {
        // The pendulum forms an algebraic loop between constraint derivatives
        // and the Lagrange multiplier lambda

        // Simplified loop: three equations, three unknowns
        // ax = -lambda * x
        // ay = -lambda * y - g
        // ax*x + ay*y + vx*vx + vy*vy = 0 (second derivative of constraint)

        let equations = vec![
            // ax = -lambda * x
            Equation::Simple {
                lhs: make_var("ax"),
                rhs: Expression::Unary {
                    op: crate::ir::ast::OpUnary::Minus(Token::default()),
                    rhs: Box::new(make_mul(make_var("lambda"), make_var("x"))),
                },
            },
            // ay = -lambda * y - g
            Equation::Simple {
                lhs: make_var("ay"),
                rhs: make_sub(
                    Expression::Unary {
                        op: crate::ir::ast::OpUnary::Minus(Token::default()),
                        rhs: Box::new(make_mul(make_var("lambda"), make_var("y"))),
                    },
                    make_var("g"),
                ),
            },
            // ax*x + ay*y + vx*vx + vy*vy = 0 (second derivative of constraint)
            Equation::Simple {
                lhs: make_add(
                    make_add(
                        make_mul(make_var("ax"), make_var("x")),
                        make_mul(make_var("ay"), make_var("y")),
                    ),
                    make_add(
                        make_mul(make_var("vx"), make_var("vx")),
                        make_mul(make_var("vy"), make_var("vy")),
                    ),
                ),
                rhs: make_const("0"),
            },
        ];

        // These form an algebraic system for ax, ay, lambda
        let variables: HashSet<String> = ["ax", "ay", "lambda"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        let loop_info = tear_algebraic_loop(&equations, &[0, 1, 2], &variables);

        assert_eq!(loop_info.size, 3);
        // With tearing, we should be able to solve sequentially after guessing one variable
        // Typically lambda is a good tearing variable
    }

    #[test]
    fn test_analyze_algebraic_loops() {
        // Two equations forming a loop
        let equations = vec![
            Equation::Simple {
                lhs: make_var("x"),
                rhs: make_add(make_var("y"), make_const("1")),
            },
            Equation::Simple {
                lhs: make_var("y"),
                rhs: make_add(make_var("x"), make_const("1")),
            },
        ];

        // Both equations in same SCC (algebraic loop)
        let sccs = vec![vec![0, 1]];

        let loops = analyze_algebraic_loops(&equations, &sccs);

        assert_eq!(loops.len(), 1);
        assert_eq!(loops[0].size, 2);
    }

    #[test]
    fn test_analyze_no_loops() {
        // Chain of equations - no loops
        let equations = vec![
            Equation::Simple {
                lhs: make_var("x"),
                rhs: make_const("1"),
            },
            Equation::Simple {
                lhs: make_var("y"),
                rhs: make_var("x"),
            },
        ];

        // Each equation in its own SCC (no loops)
        let sccs = vec![vec![0], vec![1]];

        let loops = analyze_algebraic_loops(&equations, &sccs);

        // No algebraic loops detected
        assert!(loops.is_empty());
    }
}
