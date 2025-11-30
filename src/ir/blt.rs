//! Block Lower Triangular (BLT) decomposition for equation ordering.
//!
//! This module implements BLT transformation to reorder equations so that:
//! 1. Each equation can be solved for one variable
//! 2. Variables are computed in dependency order
//! 3. Derivative equations (der(x) = ...) are in proper form
//!
//! The algorithm uses **Tarjan's strongly connected components (SCC)** method:
//! 1. Build dependency graph: equation i depends on equation j if
//!    equation i uses a variable that equation j defines
//! 2. Apply Tarjan's algorithm to find strongly connected components (SCCs)
//!    - SCCs represent blocks of mutually dependent equations (algebraic loops)
//! 3. Tarjan's algorithm produces SCCs in reverse topological order
//! 4. Process equations in dependency order
//! 5. Normalize derivative equations: swap if der() on RHS but not on LHS
//!
//! References:
//! - Tarjan, R. (1972). "Depth-first search and linear graph algorithms"
//! - Pantelides, C. (1988). "The consistent initialization of differential-algebraic systems"

use crate::ir::ast::{Equation, Expression};
use crate::ir::visitors::expression_visitor::ExpressionVisitor;
use std::collections::{HashMap, HashSet};

/// Visitor to find all variables referenced in an expression
struct VariableFinder {
    variables: HashSet<String>,
}

impl VariableFinder {
    fn new() -> Self {
        Self {
            variables: HashSet::new(),
        }
    }
}

impl ExpressionVisitor for VariableFinder {
    fn visit_component_reference(&mut self, comp: &crate::ir::ast::ComponentReference) {
        self.variables.insert(comp.to_string());
    }
}

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

impl ExpressionVisitor for DerivativeFinder {
    fn visit_function_call(
        &mut self,
        comp: &crate::ir::ast::ComponentReference,
        args: &[Expression],
    ) {
        if comp.to_string() == "der" && !args.is_empty() {
            if let Expression::ComponentReference(cref) = &args[0] {
                self.derivatives.push(cref.to_string());
            }
        }
    }
}

/// Information about an equation in the BLT graph
#[derive(Debug, Clone)]
struct EquationInfo {
    equation: Equation,
    /// Variables that appear in this equation
    variables: HashSet<String>,
    /// Variable on LHS (if in form: var = expr or der(var) = expr)
    lhs_variable: Option<String>,
    /// True if this is a derivative equation: der(x) = expr
    is_derivative: bool,
}

/// Perform BLT transformation on a set of equations
pub fn blt_transform(equations: Vec<Equation>) -> Vec<Equation> {
    // Parse equations and build graph
    let mut eq_infos: Vec<EquationInfo> = Vec::new();

    for eq in equations.iter() {
        if let Equation::Simple { lhs, rhs, .. } = eq {
            let mut info = EquationInfo {
                equation: eq.clone(),
                variables: HashSet::new(),
                lhs_variable: None,
                is_derivative: false,
            };

            // Find LHS variable
            match lhs {
                Expression::ComponentReference(cref) => {
                    info.lhs_variable = Some(cref.to_string());
                }
                Expression::FunctionCall { comp, args } => {
                    if comp.to_string() == "der" && !args.is_empty() {
                        if let Expression::ComponentReference(cref) = &args[0] {
                            info.lhs_variable = Some(format!("der({})", cref));
                            info.is_derivative = true;
                        }
                    }
                }
                _ => {}
            }

            // Find all variables in RHS
            let mut var_finder = VariableFinder::new();
            visit_expression(&mut var_finder, rhs);
            info.variables = var_finder.variables;

            // Also check for der() calls in RHS
            let mut der_finder = DerivativeFinder::new();
            visit_expression(&mut der_finder, rhs);
            for der_var in &der_finder.derivatives {
                info.variables.insert(format!("der({})", der_var));
            }

            eq_infos.push(info);
        } else {
            // Non-simple equations (If, When, etc.) - keep as-is
            eq_infos.push(EquationInfo {
                equation: eq.clone(),
                variables: HashSet::new(),
                lhs_variable: None,
                is_derivative: false,
            });
        }
    }

    // Build dependency graph and find ordering using Tarjan's SCC algorithm
    let ordered_indices = tarjan_scc(&eq_infos);

    // Reorder and normalize equations
    let mut result = Vec::new();
    for idx in ordered_indices {
        let info = &eq_infos[idx];

        // Normalize derivative equations: if der(x) appears on RHS, swap sides
        if let Equation::Simple { lhs, rhs, .. } = &info.equation {
            let needs_swap = check_if_needs_swap(lhs, rhs);

            if needs_swap {
                // Swap LHS and RHS
                result.push(Equation::Simple {
                    lhs: rhs.clone(),
                    rhs: lhs.clone(),
                });
            } else {
                result.push(info.equation.clone());
            }
        } else {
            result.push(info.equation.clone());
        }
    }

    result
}

/// Check if equation needs LHS/RHS swap (der() on RHS but not on LHS)
fn check_if_needs_swap(lhs: &Expression, rhs: &Expression) -> bool {
    let lhs_has_der = has_der_call(lhs);
    let rhs_has_der = has_der_call(rhs);

    // Swap if RHS has der() but LHS doesn't
    !lhs_has_der && rhs_has_der
}

/// Check if expression contains a der() call
fn has_der_call(expr: &Expression) -> bool {
    let mut finder = DerivativeFinder::new();
    visit_expression(&mut finder, expr);
    !finder.derivatives.is_empty()
}

/// Tarjan's algorithm state for finding strongly connected components
struct TarjanState {
    index: usize,
    stack: Vec<usize>,
    indices: Vec<Option<usize>>,
    lowlinks: Vec<usize>,
    on_stack: Vec<bool>,
    sccs: Vec<Vec<usize>>,
}

impl TarjanState {
    fn new(n: usize) -> Self {
        Self {
            index: 0,
            stack: Vec::new(),
            indices: vec![None; n],
            lowlinks: vec![0; n],
            on_stack: vec![false; n],
            sccs: Vec::new(),
        }
    }

    fn strongconnect(&mut self, v: usize, graph: &[Vec<usize>]) {
        // Set the depth index for v to the smallest unused index
        self.indices[v] = Some(self.index);
        self.lowlinks[v] = self.index;
        self.index += 1;
        self.stack.push(v);
        self.on_stack[v] = true;

        // Consider successors of v
        for &w in &graph[v] {
            if self.indices[w].is_none() {
                // Successor w has not yet been visited; recurse on it
                self.strongconnect(w, graph);
                self.lowlinks[v] = self.lowlinks[v].min(self.lowlinks[w]);
            } else if self.on_stack[w] {
                // Successor w is in stack and hence in the current SCC
                self.lowlinks[v] = self.lowlinks[v].min(self.indices[w].unwrap());
            }
        }

        // If v is a root node, pop the stack and create an SCC
        if self.lowlinks[v] == self.indices[v].unwrap() {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack[w] = false;
                scc.push(w);
                if w == v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }
}

/// Find strongly connected components using Tarjan's algorithm and return equations in topological order
///
/// Tarjan's algorithm finds SCCs in O(V + E) time using a single depth-first search.
/// The SCCs are produced in reverse topological order, so we reverse them at the end.
fn tarjan_scc(eq_infos: &[EquationInfo]) -> Vec<usize> {
    let n = eq_infos.len();

    // Build dependency graph: equation i depends on equation j if
    // equation i uses a variable that equation j defines
    let mut graph: Vec<Vec<usize>> = vec![Vec::new(); n];

    // Map: variable -> equation that defines it
    let mut var_to_eq: HashMap<String, usize> = HashMap::new();

    for (i, info) in eq_infos.iter().enumerate() {
        if let Some(ref var) = info.lhs_variable {
            var_to_eq.insert(var.clone(), i);
        }
    }

    // Build dependency edges: graph[j] contains i if equation i depends on equation j
    for (i, info) in eq_infos.iter().enumerate() {
        for var in &info.variables {
            if let Some(&j) = var_to_eq.get(var) {
                if i != j {
                    graph[j].push(i);
                }
            }
        }
    }

    // Run Tarjan's algorithm
    let mut state = TarjanState::new(n);
    for v in 0..n {
        if state.indices[v].is_none() {
            state.strongconnect(v, &graph);
        }
    }

    // Tarjan's algorithm produces SCCs in reverse topological order
    // We need to reverse to get proper dependency order
    state.sccs.reverse();

    // Flatten SCCs into equation order
    let mut result = Vec::new();
    for scc in state.sccs {
        // Within each SCC, keep original order
        // (for simple cases, SCC will have size 1; for algebraic loops, we keep them together)
        for eq_idx in scc {
            result.push(eq_idx);
        }
    }

    result
}

/// Helper to visit all nodes in an expression tree
fn visit_expression<V: ExpressionVisitor>(visitor: &mut V, expr: &Expression) {
    match expr {
        Expression::ComponentReference(cref) => {
            visitor.visit_component_reference(cref);
        }
        Expression::FunctionCall { comp, args } => {
            visitor.visit_function_call(comp, args);
            for arg in args {
                visit_expression(visitor, arg);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            visit_expression(visitor, lhs);
            visit_expression(visitor, rhs);
        }
        Expression::Unary { rhs, .. } => {
            visit_expression(visitor, rhs);
        }
        Expression::Array { elements } => {
            for e in elements {
                visit_expression(visitor, e);
            }
        }
        Expression::Range {
            start, step, end, ..
        } => {
            visit_expression(visitor, start);
            if let Some(s) = step {
                visit_expression(visitor, s);
            }
            visit_expression(visitor, end);
        }
        Expression::Tuple { elements } => {
            for e in elements {
                visit_expression(visitor, e);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                visit_expression(visitor, cond);
                visit_expression(visitor, then_expr);
            }
            visit_expression(visitor, else_branch);
        }
        // Terminals - no recursion needed
        Expression::Terminal { .. } | Expression::Empty => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, ComponentReference, Token};

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

    #[test]
    fn test_swap_derivative_equation() {
        // v = der(h) should become der(h) = v
        let equations = vec![Equation::Simple {
            lhs: make_var("v"),
            rhs: make_der(make_var("h")),
        }];

        let result = blt_transform(equations);

        assert_eq!(result.len(), 1);
        if let Equation::Simple { lhs, rhs, .. } = &result[0] {
            assert!(has_der_call(lhs), "LHS should have der()");
            assert!(!has_der_call(rhs), "RHS should not have der()");
        } else {
            panic!("Expected Simple equation");
        }
    }
}
