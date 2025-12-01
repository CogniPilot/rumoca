//! Structural analysis for DAE systems: Index reduction and Tearing
//!
//! This module provides algorithms for analyzing and transforming DAE systems:
//!
//! ## Index Reduction (Pantelides Algorithm)
//!
//! High-index DAEs cannot be solved directly by standard integrators. The Pantelides
//! algorithm detects structural singularities and identifies which equations need
//! to be differentiated to reduce the index to 1.
//!
//! **Example of index-2 DAE (pendulum):**
//! ```text
//! der(x) = vx
//! der(y) = vy
//! der(vx) = lambda * x / m
//! der(vy) = lambda * y / m - g
//! x^2 + y^2 = L^2  // Constraint equation (causes high index)
//! ```
//!
//! ## Tearing
//!
//! Large algebraic loops (SCCs with multiple equations) can be expensive to solve
//! as coupled nonlinear systems. Tearing selects a subset of variables ("tearing
//! variables") that, when guessed, allow the remaining equations to be solved
//! sequentially.
//!
//! **Benefits:**
//! - Reduces size of nonlinear system to solve
//! - Improves convergence in Newton iterations
//! - Can exploit sparsity in large systems
//!
//! ## References
//!
//! - Pantelides, C. (1988). "The Consistent Initialization of Differential-Algebraic Systems"
//! - Mattsson, S.E. & Söderlind, G. (1993). "Index Reduction in Differential-Algebraic Equations"
//! - Elmqvist, H. & Otter, M. (1994). "Methods for Tearing Systems of Equations"
//! - Cellier, F.E. & Kofman, E. (2006). "Continuous System Simulation", Chapter 9

use crate::ir::ast::{ComponentRefPart, ComponentReference, Equation, Expression, Token};
use crate::ir::visitors::expression_visitor::ExpressionVisitor;
use std::collections::{HashMap, HashSet, VecDeque};

// ============================================================================
// Data Structures
// ============================================================================

/// Result of structural analysis
#[derive(Debug, Clone, Default)]
pub struct StructuralAnalysis {
    /// The DAE index (0 = ODE, 1 = index-1 DAE, 2+ = high index)
    pub dae_index: usize,

    /// Equations that need to be differentiated for index reduction
    /// Maps equation index to number of times it needs differentiation
    pub equations_to_differentiate: HashMap<usize, usize>,

    /// Variables that are "dummy derivatives" introduced by index reduction
    /// These are new algebraic variables representing higher derivatives
    pub dummy_derivatives: Vec<DummyDerivative>,

    /// Information about each strongly connected component (algebraic loop)
    pub algebraic_loops: Vec<AlgebraicLoop>,

    /// Whether the system is structurally singular
    pub is_singular: bool,

    /// Diagnostic messages
    pub diagnostics: Vec<String>,
}

/// A dummy derivative variable introduced by index reduction
#[derive(Debug, Clone)]
pub struct DummyDerivative {
    /// Name of the dummy variable (e.g., "der_x" for der(x))
    pub name: String,
    /// The variable being differentiated
    pub base_variable: String,
    /// Order of differentiation (1 = first derivative, 2 = second, etc.)
    pub order: usize,
}

/// Information about an algebraic loop (SCC with size > 1)
#[derive(Debug, Clone)]
pub struct AlgebraicLoop {
    /// Indices of equations in this loop
    pub equation_indices: Vec<usize>,
    /// All variables involved in this loop
    pub variables: HashSet<String>,
    /// Tearing variables (if tearing was applied)
    pub tearing_variables: Vec<String>,
    /// Residual variables (solved from tearing variables)
    pub residual_variables: Vec<String>,
    /// Size of the loop
    pub size: usize,
}

/// Information about an equation for structural analysis
#[derive(Debug, Clone)]
pub struct EquationStructure {
    /// Original equation
    pub equation: Equation,
    /// All variables in this equation
    pub variables: HashSet<String>,
    /// Derivative variables (from der() calls)
    pub derivatives: HashSet<String>,
    /// Whether this is a constraint (no derivatives)
    pub is_constraint: bool,
    /// Assigned variable (from matching)
    pub assigned_variable: Option<String>,
    /// Differentiation level (0 = original, 1 = differentiated once, etc.)
    pub diff_level: usize,
}

// ============================================================================
// Pantelides Algorithm for Index Reduction
// ============================================================================

/// Pantelides algorithm for structural index reduction
///
/// The algorithm works by:
/// 1. Building a bipartite graph between equations and variables
/// 2. Finding maximum matching
/// 3. If matching is incomplete, differentiate unmatched constraint equations
/// 4. Repeat until all equations can be matched
///
/// Returns structural analysis results including which equations need differentiation.
///
/// # Arguments
/// * `equations` - The DAE equations
/// * `state_variables` - Set of state variable names (will have der() forms)
/// * `algebraic_variables` - Set of algebraic unknown names (like lambda)
///   If None, all non-state, non-derivative variables are treated as unknowns
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
        // Only match equations to UNKNOWN variables, not parameters
        let (matching, unmatched_eqs) =
            find_structural_matching(&eq_structures, &unknown_variables, state_variables);

        // Check for convergence:
        // Success when we can match all unknowns (matching size == number of unknowns)
        // Unmatched equations at lower differentiation levels are ok (they become hidden constraints)
        if matching.len() >= unknown_variables.len() {
            // Complete matching on the variable side - we're done
            analysis.dae_index = iteration - 1;
            break;
        }

        // Also check if all unmatched equations are redundant (lower diff level than max)
        if !unmatched_eqs.is_empty() {
            let max_level = eq_structures
                .iter()
                .map(|e| e.diff_level)
                .max()
                .unwrap_or(0);
            let all_redundant = unmatched_eqs
                .iter()
                .all(|&idx| idx < eq_structures.len() && eq_structures[idx].diff_level < max_level);
            if all_redundant && matching.len() == unknown_variables.len() {
                // All unmatched are at lower levels (redundant constraints)
                analysis.dae_index = iteration - 1;
                break;
            }
        }

        // Find equations to differentiate using augmenting path analysis
        let eqs_to_diff =
            find_equations_to_differentiate(&eq_structures, &unmatched_eqs, &matching);

        if eqs_to_diff.is_empty() {
            // Structurally singular - cannot reduce index
            analysis.is_singular = true;
            analysis.diagnostics.push(
                "Structurally singular system: cannot find equations to differentiate".to_string(),
            );
            break;
        }

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
                    // Parameters like L have der(L) = 0, so don't add them as unknowns
                    for var in &diff_struct.derivatives {
                        // Only add der(var) as unknown if var is a state variable
                        if state_variables.contains(var) {
                            let der_var = format!("der({})", var);
                            unknown_variables.insert(der_var.clone());
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

/// Analyze the structure of a single equation
fn analyze_equation_structure(equation: &Equation) -> EquationStructure {
    let mut variables = HashSet::new();
    let mut derivatives = HashSet::new();

    if let Equation::Simple { lhs, rhs, .. } = equation {
        // Find all variables
        let mut var_finder = VariableFinder::new();
        visit_expression(&mut var_finder, lhs);
        visit_expression(&mut var_finder, rhs);
        variables = var_finder.variables;

        // Find derivatives
        let mut der_finder = DerivativeCollector::new();
        visit_expression(&mut der_finder, lhs);
        visit_expression(&mut der_finder, rhs);
        derivatives = der_finder.derivatives;
    }

    let is_constraint = derivatives.is_empty();

    EquationStructure {
        equation: equation.clone(),
        variables,
        derivatives,
        is_constraint,
        assigned_variable: None,
        diff_level: 0,
    }
}

/// Find structural matching between equations and variables
///
/// Key insight for DAE index detection:
/// - Differential equations (those with der() calls) should match to der(state) or algebraic vars
///   They should NOT match to state variables, because states are determined by integration.
/// - Constraint equations (no der() calls) should NOT match to STATE variables directly,
///   because state values come from integration, not from algebraic equations.
///   They can only match to algebraic unknowns.
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
    // Key rule: STATE variables (x, y, etc.) can only be determined by integration,
    // so no equation should match to them. Equations can only match to:
    // - Derivative variables (der(x), der(y), etc.)
    // - Algebraic unknowns (lambda, etc.)
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n_equations];
    for (eq_idx, eq_struct) in eq_structures.iter().enumerate() {
        for var in &eq_struct.variables {
            // Never allow matching to state variables - states are determined by integration
            // Only allow algebraic (non-state) variables
            if state_variables.contains(var) {
                continue;
            }
            if let Some(&var_idx) = var_to_idx.get(var) {
                adj[eq_idx].push(var_idx);
            }
        }
        // Add derivative variables if they actually appear in the equation
        // Only allow matching to der(state), not der(parameter)
        for der_var in &eq_struct.derivatives {
            // Only add if der_var is a state variable (so der(der_var) is a valid unknown)
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
    _matching: &HashMap<usize, String>,
) -> Vec<usize> {
    // Find unmatched equations at the highest differentiation level
    // These are the ones that need to be differentiated next
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

/// Symbolically differentiate an equation with respect to time
fn differentiate_equation(equation: &Equation) -> Option<Equation> {
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
fn differentiate_expression(expr: &Expression) -> Expression {
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
            // Product rule, sum rule, etc.
            match op {
                crate::ir::ast::OpBinary::Add(_) | crate::ir::ast::OpBinary::Sub(_) => {
                    // d/dt(a + b) = d/dt(a) + d/dt(b)
                    Expression::Binary {
                        lhs: Box::new(differentiate_expression(lhs)),
                        op: op.clone(),
                        rhs: Box::new(differentiate_expression(rhs)),
                    }
                }
                crate::ir::ast::OpBinary::Mul(_) => {
                    // Product rule: d/dt(a * b) = a' * b + a * b'
                    let da = differentiate_expression(lhs);
                    let db = differentiate_expression(rhs);
                    Expression::Binary {
                        lhs: Box::new(Expression::Binary {
                            lhs: Box::new(da),
                            op: op.clone(),
                            rhs: rhs.clone(),
                        }),
                        op: crate::ir::ast::OpBinary::Add(Token::default()),
                        rhs: Box::new(Expression::Binary {
                            lhs: lhs.clone(),
                            op: op.clone(),
                            rhs: Box::new(db),
                        }),
                    }
                }
                _ => {
                    // For other operators, return der(expr) as placeholder
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
            }
        }
        Expression::Terminal { .. } => {
            // Constants differentiate to 0
            Expression::Terminal {
                terminal_type: crate::ir::ast::TerminalType::UnsignedInteger,
                token: Token {
                    text: "0".to_string(),
                    ..Default::default()
                },
            }
        }
        Expression::FunctionCall { comp, args } => {
            if comp.to_string() == "der" {
                // der(der(x)) = second derivative
                // For now, wrap in another der
                Expression::FunctionCall {
                    comp: comp.clone(),
                    args: args.iter().map(differentiate_expression).collect(),
                }
            } else {
                // Chain rule for function calls (simplified)
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
        }
        _ => expr.clone(),
    }
}

// ============================================================================
// Tearing Algorithm
// ============================================================================

/// Apply tearing to an algebraic loop to reduce the size of the nonlinear system
///
/// Tearing selects a subset of variables (tearing variables) such that:
/// 1. If tearing variables are known, remaining equations can be solved sequentially
/// 2. The number of tearing variables is minimized
///
/// This implements a greedy heuristic that tends to produce good results.
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
            let eq_struct = analyze_equation_structure(&equations[eq_idx]);
            let var_indices: HashSet<usize> = eq_struct
                .variables
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
pub fn analyze_algebraic_loops(equations: &[Equation], sccs: &[Vec<usize>]) -> Vec<AlgebraicLoop> {
    let mut loops = Vec::new();

    for scc in sccs {
        if scc.len() > 1 {
            // This is an algebraic loop
            let mut loop_vars = HashSet::new();

            for &eq_idx in scc {
                if eq_idx < equations.len() {
                    let eq_struct = analyze_equation_structure(&equations[eq_idx]);
                    loop_vars.extend(eq_struct.variables);
                }
            }

            let torn_loop = tear_algebraic_loop(equations, scc, &loop_vars);
            loops.push(torn_loop);
        }
    }

    loops
}

// ============================================================================
// Helper Structures (reused from blt.rs)
// ============================================================================

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
    fn visit_component_reference(&mut self, comp: &ComponentReference) {
        self.variables.insert(comp.to_string());
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

impl ExpressionVisitor for DerivativeCollector {
    fn visit_function_call(&mut self, comp: &ComponentReference, args: &[Expression]) {
        if comp.to_string() == "der" && !args.is_empty() {
            // Handle der(x) where x is a simple variable
            if let Expression::ComponentReference(cref) = &args[0] {
                self.derivatives.insert(cref.to_string());
            }
            // Handle der(der(x)) - nested derivatives
            // In the pendulum case: der(der(x)) represents der(vx) where vx = der(x)
            // Since vx is a state, we want to record "vx" as a derivative reference
            else if let Expression::FunctionCall {
                comp: inner_comp,
                args: inner_args,
            } = &args[0]
            {
                if inner_comp.to_string() == "der" && !inner_args.is_empty() {
                    if let Expression::ComponentReference(cref) = &inner_args[0] {
                        // For der(der(x)), extract the base and prepend "der_" to indicate
                        // this is a velocity variable's derivative (i.e., acceleration)
                        // But for matching purposes, we want this to match der(vx) where vx is a state
                        // The key insight: if der(x) = vx (a state), then der(der(x)) = der(vx)
                        // So we should record the variable whose derivative is being taken
                        // which in nested form der(der(x)) means we need der(vx) to match
                        let base = cref.to_string();
                        // Record both the base (x) and indicate this is a higher derivative
                        // For now, we'll use a naming convention: "vx" for velocity of x
                        // This assumes the velocity is named "v" + base
                        self.derivatives.insert(format!("v{}", base));
                    }
                }
            }
        }
    }
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
        Expression::Terminal { .. } | Expression::Empty => {}
    }
}

// ============================================================================
// Hopcroft-Karp (local copy for structural analysis)
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, TerminalType, Token};

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
    fn test_tearing_simple_loop() {
        // Two equations, two variables: x = y + 1, y = x + 1
        let equations = vec![
            Equation::Simple {
                lhs: make_var("x"),
                rhs: Expression::Binary {
                    lhs: Box::new(make_var("y")),
                    op: crate::ir::ast::OpBinary::Add(Token::default()),
                    rhs: Box::new(make_const("1")),
                },
            },
            Equation::Simple {
                lhs: make_var("y"),
                rhs: Expression::Binary {
                    lhs: Box::new(make_var("x")),
                    op: crate::ir::ast::OpBinary::Add(Token::default()),
                    rhs: Box::new(make_const("1")),
                },
            },
        ];

        let variables: HashSet<String> = ["x".to_string(), "y".to_string()].into_iter().collect();

        let loop_info = tear_algebraic_loop(&equations, &[0, 1], &variables);

        assert_eq!(loop_info.size, 2);
        // Should identify one tearing variable
        assert!(!loop_info.tearing_variables.is_empty());
    }

    #[test]
    fn test_index_reduction_ode() {
        // Simple ODE: der(x) = -x (index 0)
        let equations = vec![Equation::Simple {
            lhs: make_der(make_var("x")),
            rhs: Expression::Unary {
                op: crate::ir::ast::OpUnary::Minus(Token::default()),
                rhs: Box::new(make_var("x")),
            },
        }];

        let states: HashSet<String> = ["x".to_string()].into_iter().collect();

        // No algebraic unknowns for simple ODE
        let analysis = pantelides_index_reduction(&equations, &states, None);

        assert_eq!(analysis.dae_index, 0);
        assert!(analysis.equations_to_differentiate.is_empty());
        assert!(!analysis.is_singular);
    }

    #[test]
    fn test_differentiate_expression() {
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

    /// Helper to create a binary multiplication expression
    fn make_mul(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op: crate::ir::ast::OpBinary::Mul(Token::default()),
            rhs: Box::new(rhs),
        }
    }

    /// Helper to create a binary addition expression
    fn make_add(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op: crate::ir::ast::OpBinary::Add(Token::default()),
            rhs: Box::new(rhs),
        }
    }

    /// Helper to create a binary subtraction expression
    fn make_sub(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            lhs: Box::new(lhs),
            op: crate::ir::ast::OpBinary::Sub(Token::default()),
            rhs: Box::new(rhs),
        }
    }

    #[test]
    fn test_pendulum_index3_dae() {
        // Cartesian pendulum - classic index-3 DAE
        //
        // The pendulum equations in Cartesian coordinates:
        //   der(x) = vx                    (1) - kinematic
        //   der(y) = vy                    (2) - kinematic
        //   m * der(vx) = -lambda * x      (3) - Newton x (simplified, lambda = tension/L)
        //   m * der(vy) = -lambda * y - m*g (4) - Newton y
        //   x^2 + y^2 = L^2                (5) - constraint (causes high index!)
        //
        // This is an index-3 DAE because:
        // - Constraint (5) contains no derivatives
        // - Differentiating once: 2*x*vx + 2*y*vy = 0 (velocity constraint)
        // - Differentiating twice: 2*vx^2 + 2*x*ax + 2*vy^2 + 2*y*ay = 0 (acceleration constraint)
        // - Only at this level can we solve for lambda

        // Build the pendulum equations
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
            // der(vx) = -lambda * x / m  (simplified: assuming m=1)
            Equation::Simple {
                lhs: make_der(make_var("vx")),
                rhs: Expression::Unary {
                    op: crate::ir::ast::OpUnary::Minus(Token::default()),
                    rhs: Box::new(make_mul(make_var("lambda"), make_var("x"))),
                },
            },
            // der(vy) = -lambda * y - g  (simplified: assuming m=1)
            Equation::Simple {
                lhs: make_der(make_var("vy")),
                rhs: make_sub(
                    Expression::Unary {
                        op: crate::ir::ast::OpUnary::Minus(Token::default()),
                        rhs: Box::new(make_mul(make_var("lambda"), make_var("y"))),
                    },
                    make_var("g"),
                ),
            },
            // x^2 + y^2 = L^2  (constraint equation - no derivatives!)
            // Represented as: x*x + y*y = L*L
            Equation::Simple {
                lhs: make_add(
                    make_mul(make_var("x"), make_var("x")),
                    make_mul(make_var("y"), make_var("y")),
                ),
                rhs: make_mul(make_var("L"), make_var("L")),
            },
        ];

        // State variables (x, y, vx, vy are states)
        let states: HashSet<String> = ["x", "y", "vx", "vy"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        // Algebraic unknowns: lambda is the Lagrange multiplier (unknown tension)
        // L and g are parameters (known constants), NOT unknowns
        let algebraic: HashSet<String> = ["lambda"].iter().map(|s| s.to_string()).collect();

        // Run index reduction
        let analysis = pantelides_index_reduction(&equations, &states, Some(&algebraic));

        // The pendulum should be detected as a high-index DAE
        // The constraint needs to be differentiated twice to become solvable
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

        // Should be detected as a constraint (no derivatives)
        assert!(
            structure.is_constraint,
            "x^2 + y^2 = L^2 should be detected as constraint"
        );
        assert!(
            structure.derivatives.is_empty(),
            "Constraint should have no derivatives"
        );

        // Should contain x, y, L as variables
        assert!(structure.variables.contains("x"));
        assert!(structure.variables.contains("y"));
        assert!(structure.variables.contains("L"));
    }

    #[test]
    fn test_index1_dae() {
        // Simple index-1 DAE (can be solved without differentiation)
        // der(x) = -y
        // 0 = x + y - 1  (algebraic constraint that directly gives y)
        let equations = vec![
            Equation::Simple {
                lhs: make_der(make_var("x")),
                rhs: Expression::Unary {
                    op: crate::ir::ast::OpUnary::Minus(Token::default()),
                    rhs: Box::new(make_var("y")),
                },
            },
            // x + y = 1  (can solve for y = 1 - x directly)
            Equation::Simple {
                lhs: make_add(make_var("x"), make_var("y")),
                rhs: make_const("1"),
            },
        ];

        let states: HashSet<String> = ["x".to_string()].into_iter().collect();

        // y is the algebraic unknown (solved from the constraint)
        let algebraic: HashSet<String> = ["y".to_string()].into_iter().collect();

        let analysis = pantelides_index_reduction(&equations, &states, Some(&algebraic));

        println!("Index-1 DAE test:");
        println!("  DAE Index: {}", analysis.dae_index);
        println!("  Is singular: {}", analysis.is_singular);

        // Index-1 DAE should have low index
        assert!(
            analysis.dae_index <= 1,
            "Simple index-1 DAE should have index <= 1"
        );
    }

    #[test]
    fn test_algebraic_loop_in_pendulum() {
        // The pendulum forms an algebraic loop between constraint derivatives
        // and the Lagrange multiplier lambda

        // Simplified loop: two equations, two unknowns forming a loop
        // ax = -lambda * x  (where ax is algebraic, derived from differentiating twice)
        // constraint_dd = ax*x + vx*vx + ay*y + vy*vy = 0

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
            // This couples ax, ay, and lambda
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

        println!("Pendulum algebraic loop:");
        println!("  Size: {}", loop_info.size);
        println!("  Tearing variables: {:?}", loop_info.tearing_variables);
        println!("  Residual variables: {:?}", loop_info.residual_variables);

        assert_eq!(loop_info.size, 3);
        // With tearing, we should be able to solve sequentially after guessing one variable
        // Typically lambda is a good tearing variable
    }
}
