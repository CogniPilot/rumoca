//! Pantelides Algorithm for DAE Index Reduction
//!
//! High-index DAEs cannot be solved directly by standard integrators. The Pantelides
//! algorithm detects structural singularities and identifies which equations need
//! to be differentiated to reduce the index to 1.
//!
//! ## Example: Index-3 DAE (Cartesian Pendulum)
//!
//! ```text
//! der(x) = vx
//! der(y) = vy
//! der(vx) = lambda * x / m
//! der(vy) = lambda * y / m - g
//! x^2 + y^2 = L^2  // Constraint equation (causes high index)
//! ```
//!
//! The constraint `x^2 + y^2 = L^2` contains no derivatives. It must be
//! differentiated twice before the system can be solved:
//! - First differentiation: `2*x*vx + 2*y*vy = 0` (velocity constraint)
//! - Second differentiation: acceleration constraint (involves lambda)
//!
//! ## Algorithm
//!
//! 1. Build a bipartite graph between equations and variables
//! 2. Find maximum matching using Hopcroft-Karp
//! 3. If matching is incomplete, differentiate unmatched constraint equations
//! 4. Repeat until all equations can be matched
//!
//! ## References
//!
//! - Pantelides, C. (1988). "The Consistent Initialization of Differential-Algebraic Systems"
//! - Mattsson, S.E. & Söderlind, G. (1993). "Index Reduction in Differential-Algebraic Equations"

use super::differentiate::differentiate_equation;
use super::{DummyDerivative, StructuralAnalysis};
use crate::ir::ast::{ComponentReference, Equation, Expression};
use crate::ir::visitor::{Visitable, Visitor};
use std::collections::{HashMap, HashSet, VecDeque};

/// Information about an equation for structural analysis
#[derive(Debug, Clone)]
struct EquationStructure {
    /// Original equation
    equation: Equation,
    /// All variables in this equation
    variables: HashSet<String>,
    /// Derivative variables (from der() calls)
    derivatives: HashSet<String>,
    /// Whether this is a constraint (no derivatives)
    is_constraint: bool,
    /// Differentiation level (0 = original, 1 = differentiated once, etc.)
    diff_level: usize,
}

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

/// Visitor to collect derivative expressions
struct DerivativeCollector {
    derivatives: HashSet<String>,
}

impl DerivativeCollector {
    fn new() -> Self {
        Self {
            derivatives: HashSet::new(),
        }
    }
}

impl Visitor for DerivativeCollector {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node
            && comp.to_string() == "der"
            && !args.is_empty()
            && let Expression::ComponentReference(cref) = &args[0]
        {
            self.derivatives.insert(cref.to_string());
        }
    }
}

/// Analyze the structure of a single equation
fn analyze_equation_structure(equation: &Equation) -> EquationStructure {
    let mut variables = HashSet::new();
    let mut derivatives = HashSet::new();

    if let Equation::Simple { lhs, rhs, .. } = equation {
        // Find all variables
        let mut var_finder = VariableFinder::new();
        lhs.accept(&mut var_finder);
        rhs.accept(&mut var_finder);
        variables = var_finder.variables;

        // Find derivatives (der() calls)
        let mut der_finder = DerivativeCollector::new();
        lhs.accept(&mut der_finder);
        rhs.accept(&mut der_finder);
        derivatives = der_finder.derivatives;

        // IMPORTANT: Also add der(x) as a variable for matching purposes
        // When der(x) appears in an equation, it should be considered as
        // a variable that the equation can be matched to.
        for der_var in &derivatives {
            variables.insert(format!("der({})", der_var));
        }
    }

    let is_constraint = derivatives.is_empty();

    EquationStructure {
        equation: equation.clone(),
        variables,
        derivatives,
        is_constraint,
        diff_level: 0,
    }
}

/// Pantelides algorithm for structural index reduction
///
/// The algorithm works by:
/// 1. Building a bipartite graph between equations and variables
/// 2. Finding maximum matching
/// 3. If matching is incomplete, differentiate unmatched constraint equations
/// 4. Repeat until all equations can be matched
///
/// # Arguments
///
/// * `equations` - The DAE equations
/// * `state_variables` - Set of state variable names (will have der() forms)
/// * `algebraic_variables` - Set of algebraic unknown names (like lambda)
///   If None, all non-state, non-derivative variables are treated as unknowns
///
/// # Returns
///
/// Structural analysis results including which equations need differentiation
pub fn pantelides_index_reduction(
    equations: &[Equation],
    state_variables: &HashSet<String>,
    algebraic_variables: Option<&HashSet<String>>,
) -> StructuralAnalysis {
    let mut analysis = StructuralAnalysis::default();

    // Parse equations to extract structure
    let mut eq_structures: Vec<EquationStructure> =
        equations.iter().map(analyze_equation_structure).collect();

    // Collect all variables that appear in equations
    let mut all_equation_vars: HashSet<String> = HashSet::new();
    for eq_struct in &eq_structures {
        all_equation_vars.extend(eq_struct.variables.clone());
    }

    // Build the set of UNKNOWN variables (what we solve for)
    // This should NOT include:
    // - Parameters/constants (like L, g)
    // - State variables (like x, y) - states are determined by INTEGRATION, not by equations
    // It SHOULD include:
    // - Derivative variables (der(x), der(y), etc.) - equations determine these
    // - Algebraic unknowns (lambda, etc.)
    let mut unknown_variables: HashSet<String> = HashSet::new();

    // Add derivative variables for states (NOT the states themselves!)
    for state in state_variables {
        unknown_variables.insert(format!("der({})", state));
    }

    // Add algebraic unknowns
    if let Some(alg_vars) = algebraic_variables {
        unknown_variables.extend(alg_vars.clone());
    } else {
        // If no explicit algebraic vars provided, assume all non-state, non-derivative vars are unknowns
        for var in &all_equation_vars {
            if !state_variables.contains(var) && !var.starts_with("der(") {
                unknown_variables.insert(var.clone());
            }
        }
    }

    // Iteratively apply Pantelides until we get a complete matching
    let mut iteration = 0;
    let max_iterations = 10; // Prevent infinite loops

    while iteration < max_iterations {
        iteration += 1;

        // Build bipartite graph and find matching
        let (matching, unmatched_eqs) =
            find_structural_matching(&eq_structures, &unknown_variables, state_variables);

        // Find the maximum differentiation level
        let max_level = eq_structures
            .iter()
            .map(|e| e.diff_level)
            .max()
            .unwrap_or(0);

        // Count equations at the highest level
        let highest_level_eqs: Vec<usize> = eq_structures
            .iter()
            .enumerate()
            .filter(|(_, e)| e.diff_level == max_level)
            .map(|(i, _)| i)
            .collect();

        // Check for convergence:
        // Success when all equations at the highest differentiation level are matched
        let unmatched_at_highest: Vec<usize> = unmatched_eqs
            .iter()
            .filter(|&&idx| idx < eq_structures.len() && eq_structures[idx].diff_level == max_level)
            .copied()
            .collect();

        if unmatched_at_highest.is_empty() && !highest_level_eqs.is_empty() {
            // All equations at highest level are matched - success!
            analysis.dae_index = max_level;
            break;
        }

        // Alternative convergence: we matched all unknowns
        if matching.len() >= unknown_variables.len() {
            analysis.dae_index = max_level;
            break;
        }

        // Find equations to differentiate using augmenting path approach
        // First, find unmatched equations at highest level
        let mut eqs_to_diff: Vec<usize> = if !unmatched_at_highest.is_empty() {
            unmatched_at_highest.clone()
        } else {
            find_equations_to_differentiate(&eq_structures, &unmatched_eqs)
        };

        if eqs_to_diff.is_empty() {
            // Structurally singular - cannot reduce index
            analysis.is_singular = true;
            analysis.diagnostics.push(
                "Structurally singular system: cannot find equations to differentiate".to_string(),
            );
            break;
        }

        // Augmenting path: also differentiate equations that are matched to variables
        // used by the unmatched equations. This breaks the "conflict" and allows
        // the differentiated unmatched equation to take that variable.
        let mut additional_eqs: Vec<usize> = Vec::new();
        for &unmatched_idx in &eqs_to_diff {
            if unmatched_idx < eq_structures.len() {
                let unmatched_eq = &eq_structures[unmatched_idx];
                // Find which unknowns this equation could potentially match to
                for var in &unmatched_eq.variables {
                    if unknown_variables.contains(var) {
                        // This variable is an unknown. Find which equation is currently matched to it.
                        for (&eq_idx, matched_var) in &matching {
                            if matched_var == var
                                && !eqs_to_diff.contains(&eq_idx)
                                && !additional_eqs.contains(&eq_idx)
                            {
                                // This equation is matched to a variable we want
                                // Differentiate it too to free up the variable
                                additional_eqs.push(eq_idx);
                            }
                        }
                    }
                }
            }
        }
        eqs_to_diff.extend(additional_eqs);

        // Differentiate the identified equations
        for eq_idx in eqs_to_diff {
            if eq_idx < eq_structures.len() {
                let eq_struct = &eq_structures[eq_idx];

                // Record that this equation needs differentiation
                *analysis
                    .equations_to_differentiate
                    .entry(eq_idx)
                    .or_insert(0) += 1;

                // Create differentiated version of the equation
                if let Some(diff_eq) = differentiate_equation(&eq_struct.equation) {
                    let mut diff_struct = analyze_equation_structure(&diff_eq);
                    diff_struct.diff_level = eq_struct.diff_level + 1;

                    // Add new derivative variables ONLY for STATE variables
                    for var in &diff_struct.derivatives {
                        if state_variables.contains(var) {
                            let der_var = format!("der({})", var);
                            if unknown_variables.insert(der_var.clone()) {
                                // Record dummy derivative
                                analysis.dummy_derivatives.push(DummyDerivative {
                                    name: der_var,
                                    base_variable: var.clone(),
                                    order: diff_struct.diff_level,
                                });
                            }
                        }
                    }

                    eq_structures.push(diff_struct);
                }
            }
        }

        analysis.dae_index = iteration;
    }

    if iteration >= max_iterations {
        analysis.is_singular = true;
        analysis
            .diagnostics
            .push("Index reduction did not converge".to_string());
    }

    analysis
}

/// Find structural matching between equations and variables
fn find_structural_matching(
    eq_structures: &[EquationStructure],
    all_variables: &HashSet<String>,
    state_variables: &HashSet<String>,
) -> (HashMap<usize, String>, Vec<usize>) {
    let n_equations = eq_structures.len();
    let vars: Vec<String> = all_variables.iter().cloned().collect();
    let n_variables = vars.len();

    let var_to_idx: HashMap<&String, usize> =
        vars.iter().enumerate().map(|(i, v)| (v, i)).collect();

    // Build adjacency list with proper causality constraints
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n_equations];
    for (eq_idx, eq_struct) in eq_structures.iter().enumerate() {
        for var in &eq_struct.variables {
            // Never allow matching to state variables
            if state_variables.contains(var) {
                continue;
            }
            if let Some(&var_idx) = var_to_idx.get(var) {
                adj[eq_idx].push(var_idx);
            }
        }
        // Add derivative variables
        for der_var in &eq_struct.derivatives {
            if state_variables.contains(der_var) {
                let der_var_name = format!("der({})", der_var);
                if let Some(&var_idx) = var_to_idx.get(&der_var_name) {
                    adj[eq_idx].push(var_idx);
                }
            }
        }
    }

    // Run Hopcroft-Karp
    let mut hk = HopcroftKarp::new(n_equations, n_variables, adj);
    hk.max_matching();

    // Extract matching
    let mut matching = HashMap::new();
    let mut unmatched = Vec::new();

    for (eq_idx, var_idx) in hk.pair_eq.iter().enumerate() {
        if *var_idx != NIL && *var_idx < vars.len() {
            matching.insert(eq_idx, vars[*var_idx].clone());
        } else {
            unmatched.push(eq_idx);
        }
    }

    (matching, unmatched)
}

/// Find which equations need to be differentiated
fn find_equations_to_differentiate(
    eq_structures: &[EquationStructure],
    unmatched_eqs: &[usize],
) -> Vec<usize> {
    // Find unmatched equations at the highest differentiation level
    let mut max_diff_level = 0;
    let mut to_diff = Vec::new();

    for &eq_idx in unmatched_eqs {
        if eq_idx < eq_structures.len() {
            let level = eq_structures[eq_idx].diff_level;
            if level > max_diff_level {
                max_diff_level = level;
                to_diff.clear();
                to_diff.push(eq_idx);
            } else if level == max_diff_level {
                to_diff.push(eq_idx);
            }
        }
    }

    // If all unmatched are at level 0, prefer constraints
    if max_diff_level == 0 && !to_diff.is_empty() {
        let constraints: Vec<usize> = to_diff
            .iter()
            .filter(|&&idx| eq_structures[idx].is_constraint)
            .copied()
            .collect();
        if !constraints.is_empty() {
            return constraints;
        }
    }

    to_diff
}

// ============================================================================
// Hopcroft-Karp Algorithm (local copy for Pantelides)
// ============================================================================

const NIL: usize = usize::MAX;

struct HopcroftKarp {
    n_equations: usize,
    adj: Vec<Vec<usize>>,
    pair_eq: Vec<usize>,
    pair_var: Vec<usize>,
    dist: Vec<usize>,
}

impl HopcroftKarp {
    fn new(n_equations: usize, n_variables: usize, adj: Vec<Vec<usize>>) -> Self {
        Self {
            n_equations,
            adj,
            pair_eq: vec![NIL; n_equations],
            pair_var: vec![NIL; n_variables],
            dist: vec![0; n_equations + 1],
        }
    }

    fn max_matching(&mut self) -> usize {
        let mut matching = 0;
        while self.bfs() {
            for eq in 0..self.n_equations {
                if self.pair_eq[eq] == NIL && self.dfs(eq) {
                    matching += 1;
                }
            }
        }
        matching
    }

    fn bfs(&mut self) -> bool {
        let mut queue = VecDeque::new();
        for eq in 0..self.n_equations {
            if self.pair_eq[eq] == NIL {
                self.dist[eq] = 0;
                queue.push_back(eq);
            } else {
                self.dist[eq] = usize::MAX;
            }
        }
        self.dist[self.n_equations] = usize::MAX;

        while let Some(eq) = queue.pop_front() {
            if self.dist[eq] < self.dist[self.n_equations] {
                for &var in &self.adj[eq] {
                    let next_eq = self.pair_var[var];
                    let next_idx = if next_eq == NIL {
                        self.n_equations
                    } else {
                        next_eq
                    };
                    if self.dist[next_idx] == usize::MAX {
                        self.dist[next_idx] = self.dist[eq] + 1;
                        if next_eq != NIL {
                            queue.push_back(next_eq);
                        }
                    }
                }
            }
        }
        self.dist[self.n_equations] != usize::MAX
    }

    fn dfs(&mut self, eq: usize) -> bool {
        if eq == NIL {
            return true;
        }
        for i in 0..self.adj[eq].len() {
            let var = self.adj[eq][i];
            let next_eq = self.pair_var[var];
            let next_idx = if next_eq == NIL {
                self.n_equations
            } else {
                next_eq
            };
            if self.dist[next_idx] == self.dist[eq] + 1 && self.dfs(next_eq) {
                self.pair_var[var] = eq;
                self.pair_eq[eq] = var;
                return true;
            }
        }
        self.dist[eq] = usize::MAX;
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, OpBinary, OpUnary, TerminalType, Token};

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
    fn test_equation_structure_analysis() {
        // der(x) = v
        let eq = Equation::Simple {
            lhs: make_der(make_var("x")),
            rhs: make_var("v"),
        };

        let structure = analyze_equation_structure(&eq);

        assert!(structure.variables.contains("x"));
        assert!(structure.variables.contains("v"));
        assert!(structure.derivatives.contains("x"));
        assert!(!structure.is_constraint);
    }

    #[test]
    fn test_constraint_detection() {
        // x^2 + y^2 = L^2 (constraint - no derivatives)
        let eq = Equation::Simple {
            lhs: make_var("x"),
            rhs: make_var("y"),
        };

        let structure = analyze_equation_structure(&eq);
        assert!(structure.is_constraint);
    }

    #[test]
    fn test_index_reduction_ode() {
        // Simple ODE: der(x) = -x (index 0)
        let equations = vec![Equation::Simple {
            lhs: make_der(make_var("x")),
            rhs: Expression::Unary {
                op: OpUnary::Minus(Token::default()),
                rhs: Box::new(make_var("x")),
            },
        }];

        let states: HashSet<String> = ["x".to_string()].into_iter().collect();

        let analysis = pantelides_index_reduction(&equations, &states, None);

        assert_eq!(analysis.dae_index, 0);
        assert!(analysis.equations_to_differentiate.is_empty());
        assert!(!analysis.is_singular);
    }

    #[test]
    fn test_pendulum_index3_dae() {
        // Cartesian pendulum - classic index-3 DAE
        let equations = vec![
            // der(x) = vx
            Equation::Simple {
                lhs: make_der(make_var("x")),
                rhs: make_var("vx"),
            },
            // der(y) = vy
            Equation::Simple {
                lhs: make_der(make_var("y")),
                rhs: make_var("vy"),
            },
            // der(vx) = -lambda * x
            Equation::Simple {
                lhs: make_der(make_var("vx")),
                rhs: Expression::Unary {
                    op: OpUnary::Minus(Token::default()),
                    rhs: Box::new(make_mul(make_var("lambda"), make_var("x"))),
                },
            },
            // der(vy) = -lambda * y - g
            Equation::Simple {
                lhs: make_der(make_var("vy")),
                rhs: make_sub(
                    Expression::Unary {
                        op: OpUnary::Minus(Token::default()),
                        rhs: Box::new(make_mul(make_var("lambda"), make_var("y"))),
                    },
                    make_var("g"),
                ),
            },
            // x^2 + y^2 = L^2 (constraint)
            Equation::Simple {
                lhs: make_add(
                    make_mul(make_var("x"), make_var("x")),
                    make_mul(make_var("y"), make_var("y")),
                ),
                rhs: make_mul(make_var("L"), make_var("L")),
            },
        ];

        let states: HashSet<String> = ["x", "y", "vx", "vy"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        let algebraic: HashSet<String> = ["lambda"].iter().map(|s| s.to_string()).collect();

        let analysis = pantelides_index_reduction(&equations, &states, Some(&algebraic));

        // The pendulum should be detected as a high-index DAE
        assert!(
            analysis.dae_index > 0,
            "Pendulum should be detected as high-index DAE (got index {})",
            analysis.dae_index
        );

        // The constraint equation should need differentiation
        assert!(
            !analysis.equations_to_differentiate.is_empty(),
            "Should identify constraint equation for differentiation"
        );

        // System should not be structurally singular
        assert!(
            !analysis.is_singular,
            "Pendulum should not be structurally singular"
        );
    }

    #[test]
    fn test_pendulum_constraint_is_detected() {
        // Just the constraint equation
        let constraint = Equation::Simple {
            lhs: make_add(
                make_mul(make_var("x"), make_var("x")),
                make_mul(make_var("y"), make_var("y")),
            ),
            rhs: make_mul(make_var("L"), make_var("L")),
        };

        let structure = analyze_equation_structure(&constraint);

        assert!(
            structure.is_constraint,
            "x^2 + y^2 = L^2 should be detected as constraint"
        );
        assert!(
            structure.derivatives.is_empty(),
            "Constraint should have no derivatives"
        );

        assert!(structure.variables.contains("x"));
        assert!(structure.variables.contains("y"));
        assert!(structure.variables.contains("L"));
    }

    #[test]
    fn test_index1_dae() {
        // Simple index-1 DAE
        // der(x) = -y
        // x + y = 1
        let equations = vec![
            Equation::Simple {
                lhs: make_der(make_var("x")),
                rhs: Expression::Unary {
                    op: OpUnary::Minus(Token::default()),
                    rhs: Box::new(make_var("y")),
                },
            },
            Equation::Simple {
                lhs: make_add(make_var("x"), make_var("y")),
                rhs: make_const("1"),
            },
        ];

        let states: HashSet<String> = ["x".to_string()].into_iter().collect();
        let algebraic: HashSet<String> = ["y".to_string()].into_iter().collect();

        let analysis = pantelides_index_reduction(&equations, &states, Some(&algebraic));

        // Index-1 DAE should have low index
        assert!(
            analysis.dae_index <= 1,
            "Simple index-1 DAE should have index <= 1"
        );
    }
}
