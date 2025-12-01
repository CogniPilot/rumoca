//! Block Lower Triangular (BLT) decomposition for equation ordering.
//!
//! This module implements BLT transformation to reorder equations so that:
//! 1. Each equation can be solved for one variable
//! 2. Variables are computed in dependency order
//! 3. Derivative equations (der(x) = ...) are in proper form
//!
//! The algorithm combines:
//! - **Hopcroft-Karp algorithm** for maximum bipartite matching (equation-variable assignment)
//! - **Tarjan's strongly connected components (SCC)** for topological ordering
//!
//! Steps:
//! 1. Build bipartite graph: equations on one side, variables on the other
//! 2. Use Hopcroft-Karp to find maximum matching (O(E√V) complexity)
//! 3. Build dependency graph from matching
//! 4. Apply Tarjan's algorithm to find strongly connected components (SCCs)
//!    - SCCs represent blocks of mutually dependent equations (algebraic loops)
//! 5. Process equations in dependency order
//! 6. Normalize derivative equations: swap if der() on RHS but not on LHS
//!
//! References:
//! - Hopcroft, J. & Karp, R. (1973). "An n^(5/2) Algorithm for Maximum Matchings in Bipartite Graphs"
//! - Tarjan, R. (1972). "Depth-first search and linear graph algorithms"
//! - Pantelides, C. (1988). "The consistent initialization of differential-algebraic systems"

use crate::ir::ast::{Equation, Expression};
use crate::ir::visitors::expression_visitor::ExpressionVisitor;
use std::collections::{HashMap, HashSet, VecDeque};

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
    /// All variables that appear in this equation (both LHS and RHS)
    all_variables: HashSet<String>,
    /// Variable on LHS (if in form: var = expr or der(var) = expr)
    lhs_variable: Option<String>,
    /// True if this is a derivative equation: der(x) = expr
    is_derivative: bool,
    /// Matched variable (assigned by Hopcroft-Karp)
    matched_variable: Option<String>,
}

// ============================================================================
// Hopcroft-Karp Maximum Bipartite Matching Algorithm
// ============================================================================

/// Sentinel value representing "no match" in Hopcroft-Karp
const NIL: usize = usize::MAX;

/// Hopcroft-Karp algorithm for maximum bipartite matching
///
/// Given a bipartite graph with equations on one side and variables on the other,
/// finds the maximum matching that assigns each equation to exactly one variable.
///
/// Time complexity: O(E * sqrt(V))
struct HopcroftKarp {
    /// Number of equations (left side of bipartite graph)
    n_equations: usize,
    /// Number of variables (right side of bipartite graph)
    #[allow(dead_code)]
    n_variables: usize,
    /// Adjacency list: adj[eq] = list of variable indices this equation can be matched to
    adj: Vec<Vec<usize>>,
    /// pair_eq[eq] = variable matched to equation eq (or NIL)
    pair_eq: Vec<usize>,
    /// pair_var[var] = equation matched to variable var (or NIL)
    pair_var: Vec<usize>,
    /// Distance labels for BFS layers
    dist: Vec<usize>,
}

impl HopcroftKarp {
    /// Create a new Hopcroft-Karp instance
    ///
    /// # Arguments
    /// * `n_equations` - Number of equations
    /// * `n_variables` - Number of variables
    /// * `adj` - Adjacency list where adj[eq] contains indices of variables that equation eq can solve for
    fn new(n_equations: usize, n_variables: usize, adj: Vec<Vec<usize>>) -> Self {
        Self {
            n_equations,
            n_variables,
            adj,
            pair_eq: vec![NIL; n_equations],
            pair_var: vec![NIL; n_variables],
            dist: vec![0; n_equations + 1],
        }
    }

    /// Run the Hopcroft-Karp algorithm and return the matching size
    fn max_matching(&mut self) -> usize {
        let mut matching = 0;

        // Keep finding augmenting paths until none exist
        while self.bfs() {
            for eq in 0..self.n_equations {
                if self.pair_eq[eq] == NIL && self.dfs(eq) {
                    matching += 1;
                }
            }
        }

        matching
    }

    /// BFS to build layers of the level graph
    /// Returns true if there's at least one augmenting path
    fn bfs(&mut self) -> bool {
        let mut queue = VecDeque::new();

        // Initialize distances
        for eq in 0..self.n_equations {
            if self.pair_eq[eq] == NIL {
                self.dist[eq] = 0;
                queue.push_back(eq);
            } else {
                self.dist[eq] = usize::MAX;
            }
        }

        // Distance to NIL (represents finding an augmenting path)
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

    /// DFS to find augmenting paths along the level graph
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

    /// Get the matching result: for each equation, which variable is it matched to
    fn get_equation_matching(&self) -> Vec<Option<usize>> {
        self.pair_eq
            .iter()
            .map(|&v| if v == NIL { None } else { Some(v) })
            .collect()
    }
}

/// Build bipartite graph and find maximum matching using Hopcroft-Karp
///
/// Returns a mapping from equation index to matched variable name
fn find_maximum_matching(
    eq_infos: &[EquationInfo],
    all_variables: &[String],
) -> HashMap<usize, String> {
    let n_equations = eq_infos.len();
    let n_variables = all_variables.len();

    // Build variable name to index mapping
    let var_to_idx: HashMap<&String, usize> = all_variables
        .iter()
        .enumerate()
        .map(|(i, v)| (v, i))
        .collect();

    // Build adjacency list: which variables can each equation solve for
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n_equations];

    for (eq_idx, info) in eq_infos.iter().enumerate() {
        // Prefer LHS variable if available (explicit assignment form)
        // But also include all variables the equation contains
        let mut candidates: Vec<usize> = Vec::new();

        // First priority: LHS variable (if equation is in assignment form)
        if let Some(ref lhs_var) = info.lhs_variable {
            if let Some(&var_idx) = var_to_idx.get(lhs_var) {
                candidates.push(var_idx);
            }
        }

        // Second priority: all other variables in the equation
        for var in &info.all_variables {
            if let Some(&var_idx) = var_to_idx.get(var) {
                if !candidates.contains(&var_idx) {
                    candidates.push(var_idx);
                }
            }
        }

        adj[eq_idx] = candidates;
    }

    // Run Hopcroft-Karp algorithm
    let mut hk = HopcroftKarp::new(n_equations, n_variables, adj);
    let _matching_size = hk.max_matching();

    // Convert matching to variable names
    let matching = hk.get_equation_matching();
    let mut result = HashMap::new();

    for (eq_idx, var_idx_opt) in matching.iter().enumerate() {
        if let Some(var_idx) = var_idx_opt {
            result.insert(eq_idx, all_variables[*var_idx].clone());
        }
    }

    result
}

/// Result of BLT transformation including structural information
#[derive(Debug, Clone, Default)]
pub struct BltResult {
    /// Transformed equations in topological order
    pub equations: Vec<Equation>,
    /// Strongly connected components (algebraic loops have size > 1)
    pub sccs: Vec<Vec<usize>>,
    /// Matching: equation index -> matched variable name
    pub matching: HashMap<usize, String>,
    /// Whether a perfect matching was found
    pub is_complete_matching: bool,
}

/// Perform BLT transformation on a set of equations
///
/// This function:
/// 1. Parses equations to extract variable information
/// 2. Uses Hopcroft-Karp algorithm to find maximum matching between equations and variables
/// 3. Uses Tarjan's SCC algorithm for topological ordering
/// 4. Normalizes derivative equations (der(x) on LHS)
pub fn blt_transform(equations: Vec<Equation>) -> Vec<Equation> {
    blt_transform_with_info(equations).equations
}

/// Perform BLT transformation and return detailed structural information
///
/// This function returns both the transformed equations and additional
/// structural information useful for:
/// - Index reduction (detecting high-index DAEs)
/// - Tearing (optimizing algebraic loop solving)
/// - Diagnostics (debugging structural issues)
pub fn blt_transform_with_info(equations: Vec<Equation>) -> BltResult {
    // Parse equations and extract variable information
    let mut eq_infos: Vec<EquationInfo> = Vec::new();
    let mut all_variables_set: HashSet<String> = HashSet::new();

    for eq in equations.iter() {
        if let Equation::Simple { lhs, rhs, .. } = eq {
            let mut info = EquationInfo {
                equation: eq.clone(),
                all_variables: HashSet::new(),
                lhs_variable: None,
                is_derivative: false,
                matched_variable: None,
            };

            // Find LHS variable and add to all_variables
            match lhs {
                Expression::ComponentReference(cref) => {
                    let var_name = cref.to_string();
                    info.lhs_variable = Some(var_name.clone());
                    info.all_variables.insert(var_name.clone());
                    all_variables_set.insert(var_name);
                }
                Expression::FunctionCall { comp, args } => {
                    if comp.to_string() == "der" && !args.is_empty() {
                        if let Expression::ComponentReference(cref) = &args[0] {
                            let var_name = format!("der({})", cref);
                            info.lhs_variable = Some(var_name.clone());
                            info.all_variables.insert(var_name.clone());
                            all_variables_set.insert(var_name);
                            info.is_derivative = true;
                        }
                    }
                }
                _ => {
                    // For other LHS types, try to extract variables
                    let mut lhs_finder = VariableFinder::new();
                    visit_expression(&mut lhs_finder, lhs);
                    for var in lhs_finder.variables {
                        info.all_variables.insert(var.clone());
                        all_variables_set.insert(var);
                    }
                }
            }

            // Find all variables in RHS
            let mut var_finder = VariableFinder::new();
            visit_expression(&mut var_finder, rhs);
            for var in var_finder.variables {
                info.all_variables.insert(var.clone());
                all_variables_set.insert(var);
            }

            // Also check for der() calls in both LHS and RHS
            let mut der_finder = DerivativeFinder::new();
            visit_expression(&mut der_finder, lhs);
            visit_expression(&mut der_finder, rhs);
            for der_var in &der_finder.derivatives {
                let var_name = format!("der({})", der_var);
                info.all_variables.insert(var_name.clone());
                all_variables_set.insert(var_name);
            }

            eq_infos.push(info);
        } else {
            // Non-simple equations (If, When, etc.) - keep as-is
            eq_infos.push(EquationInfo {
                equation: eq.clone(),
                all_variables: HashSet::new(),
                lhs_variable: None,
                is_derivative: false,
                matched_variable: None,
            });
        }
    }

    // Convert variable set to sorted vector for consistent ordering
    let all_variables: Vec<String> = {
        let mut vars: Vec<_> = all_variables_set.into_iter().collect();
        vars.sort();
        vars
    };

    // Use Hopcroft-Karp to find maximum matching
    let matching = find_maximum_matching(&eq_infos, &all_variables);

    // Update eq_infos with matched variables
    for (eq_idx, var_name) in &matching {
        eq_infos[*eq_idx].matched_variable = Some(var_name.clone());
    }

    // Build dependency graph and find ordering using Tarjan's SCC algorithm
    let tarjan_result = tarjan_scc(&eq_infos);

    // Reorder and normalize equations
    let mut result_equations = Vec::new();
    for idx in &tarjan_result.ordered_indices {
        let info = &eq_infos[*idx];

        // Normalize derivative equations: if der(x) appears on RHS, swap sides
        if let Equation::Simple { lhs, rhs, .. } = &info.equation {
            let needs_swap = check_if_needs_swap(lhs, rhs);

            if needs_swap {
                // Swap LHS and RHS
                result_equations.push(Equation::Simple {
                    lhs: rhs.clone(),
                    rhs: lhs.clone(),
                });
            } else {
                result_equations.push(info.equation.clone());
            }
        } else {
            result_equations.push(info.equation.clone());
        }
    }

    // Check if we have a complete matching
    let is_complete_matching = matching.len() == eq_infos.len();

    BltResult {
        equations: result_equations,
        sccs: tarjan_result.sccs,
        matching,
        is_complete_matching,
    }
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

/// Result of Tarjan's SCC algorithm
struct TarjanResult {
    /// Equation indices in topological order
    ordered_indices: Vec<usize>,
    /// Strongly connected components (each SCC is a Vec of equation indices)
    sccs: Vec<Vec<usize>>,
}

/// Find strongly connected components using Tarjan's algorithm and return equations in topological order
///
/// Tarjan's algorithm finds SCCs in O(V + E) time using a single depth-first search.
/// The SCCs are produced in reverse topological order, so we reverse them at the end.
///
/// Uses matched_variable from Hopcroft-Karp (if available) instead of just lhs_variable
/// for more robust equation-variable assignment.
fn tarjan_scc(eq_infos: &[EquationInfo]) -> TarjanResult {
    let n = eq_infos.len();

    // Build dependency graph: equation i depends on equation j if
    // equation i uses a variable that equation j defines (solves for)
    let mut graph: Vec<Vec<usize>> = vec![Vec::new(); n];

    // Map: variable -> equation that defines (solves for) it
    // Use matched_variable from Hopcroft-Karp if available, otherwise fall back to lhs_variable
    let mut var_to_eq: HashMap<String, usize> = HashMap::new();

    for (i, info) in eq_infos.iter().enumerate() {
        // Prefer matched_variable (from Hopcroft-Karp) over lhs_variable
        let defining_var = info
            .matched_variable
            .as_ref()
            .or(info.lhs_variable.as_ref());

        if let Some(var) = defining_var {
            var_to_eq.insert(var.clone(), i);
        }
    }

    // Build dependency edges: graph[j] contains i if equation i depends on equation j
    // Equation i depends on equation j if:
    //   - equation i uses a variable V
    //   - equation j is matched to (defines) variable V
    //   - V is not the variable that equation i is matched to
    for (i, info) in eq_infos.iter().enumerate() {
        let my_var = info
            .matched_variable
            .as_ref()
            .or(info.lhs_variable.as_ref());

        for var in &info.all_variables {
            // Skip the variable this equation is matched to (we're solving for it)
            if my_var.as_ref() == Some(&var) {
                continue;
            }

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
    let mut ordered_indices = Vec::new();
    for scc in &state.sccs {
        // Within each SCC, keep original order
        // (for simple cases, SCC will have size 1; for algebraic loops, we keep them together)
        for &eq_idx in scc {
            ordered_indices.push(eq_idx);
        }
    }

    TarjanResult {
        ordered_indices,
        sccs: state.sccs,
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

    // ========================================================================
    // Hopcroft-Karp Algorithm Tests
    // ========================================================================

    #[test]
    fn test_hopcroft_karp_simple_matching() {
        // Simple case: 3 equations, 3 variables, perfect matching exists
        // eq0 can match var0
        // eq1 can match var1
        // eq2 can match var2
        let adj = vec![vec![0], vec![1], vec![2]];

        let mut hk = HopcroftKarp::new(3, 3, adj);
        let matching_size = hk.max_matching();

        assert_eq!(matching_size, 3, "Should find perfect matching of size 3");

        let matching = hk.get_equation_matching();
        assert_eq!(matching[0], Some(0));
        assert_eq!(matching[1], Some(1));
        assert_eq!(matching[2], Some(2));
    }

    #[test]
    fn test_hopcroft_karp_requires_augmenting_path() {
        // Case where greedy matching fails but Hopcroft-Karp succeeds
        // eq0 can match var0 or var1
        // eq1 can match var0 only
        //
        // Greedy might match eq0->var0, leaving eq1 unmatched
        // Hopcroft-Karp should find eq0->var1, eq1->var0
        let adj = vec![vec![0, 1], vec![0]];

        let mut hk = HopcroftKarp::new(2, 2, adj);
        let matching_size = hk.max_matching();

        assert_eq!(matching_size, 2, "Should find perfect matching of size 2");

        let matching = hk.get_equation_matching();
        // eq0 should match var1, eq1 should match var0
        assert_eq!(matching[0], Some(1));
        assert_eq!(matching[1], Some(0));
    }

    #[test]
    fn test_hopcroft_karp_incomplete_matching() {
        // Case where no perfect matching exists
        // eq0 can match var0
        // eq1 can match var0 (conflict!)
        // eq2 can match var1
        let adj = vec![vec![0], vec![0], vec![1]];

        let mut hk = HopcroftKarp::new(3, 2, adj);
        let matching_size = hk.max_matching();

        // Only 2 equations can be matched (to 2 variables)
        assert_eq!(matching_size, 2, "Should find matching of size 2");
    }

    #[test]
    fn test_hopcroft_karp_complex_augmenting() {
        // More complex case requiring multiple augmenting paths
        // This tests the BFS layering properly
        //
        // eq0 -> var0, var1
        // eq1 -> var0, var2
        // eq2 -> var1, var2
        let adj = vec![vec![0, 1], vec![0, 2], vec![1, 2]];

        let mut hk = HopcroftKarp::new(3, 3, adj);
        let matching_size = hk.max_matching();

        assert_eq!(matching_size, 3, "Should find perfect matching of size 3");
    }

    #[test]
    fn test_hopcroft_karp_empty() {
        // Edge case: no equations
        let adj: Vec<Vec<usize>> = vec![];
        let mut hk = HopcroftKarp::new(0, 0, adj);
        let matching_size = hk.max_matching();

        assert_eq!(matching_size, 0);
    }

    #[test]
    fn test_hopcroft_karp_no_edges() {
        // Edge case: equations exist but no valid assignments
        let adj = vec![vec![], vec![], vec![]];

        let mut hk = HopcroftKarp::new(3, 3, adj);
        let matching_size = hk.max_matching();

        assert_eq!(matching_size, 0, "No matching possible without edges");
    }

    #[test]
    fn test_find_maximum_matching_integration() {
        // Integration test with EquationInfo structures
        let eq_infos = vec![
            EquationInfo {
                equation: Equation::Simple {
                    lhs: make_var("x"),
                    rhs: make_var("y"),
                },
                all_variables: ["x".to_string(), "y".to_string()].into_iter().collect(),
                lhs_variable: Some("x".to_string()),
                is_derivative: false,
                matched_variable: None,
            },
            EquationInfo {
                equation: Equation::Simple {
                    lhs: make_var("y"),
                    rhs: make_var("z"),
                },
                all_variables: ["y".to_string(), "z".to_string()].into_iter().collect(),
                lhs_variable: Some("y".to_string()),
                is_derivative: false,
                matched_variable: None,
            },
        ];

        let all_variables = vec!["x".to_string(), "y".to_string(), "z".to_string()];

        let matching = find_maximum_matching(&eq_infos, &all_variables);

        assert_eq!(matching.len(), 2, "Both equations should be matched");
        assert!(matching.contains_key(&0));
        assert!(matching.contains_key(&1));
    }

    #[test]
    fn test_blt_with_chain_dependencies() {
        // Test BLT ordering with chain: z = 1, y = z, x = y
        // Should be ordered as: z = 1, y = z, x = y
        use crate::ir::ast::{TerminalType, Token};

        let equations = vec![
            // x = y
            Equation::Simple {
                lhs: make_var("x"),
                rhs: make_var("y"),
            },
            // y = z
            Equation::Simple {
                lhs: make_var("y"),
                rhs: make_var("z"),
            },
            // z = 1
            Equation::Simple {
                lhs: make_var("z"),
                rhs: Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token: Token {
                        text: "1".to_string(),
                        ..Default::default()
                    },
                },
            },
        ];

        let result = blt_transform(equations);

        // After BLT, z=1 should come first, then y=z, then x=y
        assert_eq!(result.len(), 3);

        // Extract LHS variable names for checking order
        let order: Vec<String> = result
            .iter()
            .filter_map(|eq| {
                if let Equation::Simple { lhs: Expression::ComponentReference(cref), .. } = eq {
                    return Some(cref.to_string());
                }
                None
            })
            .collect();

        // z should come before y, y should come before x
        let z_pos = order.iter().position(|s| s == "z").unwrap();
        let y_pos = order.iter().position(|s| s == "y").unwrap();
        let x_pos = order.iter().position(|s| s == "x").unwrap();

        assert!(
            z_pos < y_pos,
            "z should be computed before y (z at {}, y at {})",
            z_pos,
            y_pos
        );
        assert!(
            y_pos < x_pos,
            "y should be computed before x (y at {}, x at {})",
            y_pos,
            x_pos
        );
    }

    #[test]
    fn test_blt_algebraic_loop_detection() {
        // Test that algebraic loops (SCCs) are kept together
        // x = y + 1
        // y = x + 1
        // These form an algebraic loop

        use crate::ir::ast::{OpBinary, TerminalType, Token};

        let one = Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: "1".to_string(),
                ..Default::default()
            },
        };

        let equations = vec![
            // x = y + 1
            Equation::Simple {
                lhs: make_var("x"),
                rhs: Expression::Binary {
                    lhs: Box::new(make_var("y")),
                    op: OpBinary::Add(Token::default()),
                    rhs: Box::new(one.clone()),
                },
            },
            // y = x + 1
            Equation::Simple {
                lhs: make_var("y"),
                rhs: Expression::Binary {
                    lhs: Box::new(make_var("x")),
                    op: OpBinary::Add(Token::default()),
                    rhs: Box::new(one),
                },
            },
        ];

        let result = blt_transform(equations);

        // Both equations should be present (algebraic loop)
        assert_eq!(result.len(), 2);
    }
}
