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

use crate::ir::ast::{
    ComponentRefPart, ComponentReference, Equation, Expression, OpBinary, OpUnary, TerminalType,
    Token,
};
use crate::ir::visitor::{Visitable, Visitor};
use std::collections::{HashMap, HashSet, VecDeque};

/// Visitor to find all variables referenced in an expression.
/// Excludes function names (like "der", "sin", etc.) from the variable list.
struct VariableFinder {
    variables: HashSet<String>,
    /// Track when entering a function call to skip the function name
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
        // When entering a function call, mark that we should skip the next component reference
        // (which is the function name, not a variable)
        if matches!(node, Expression::FunctionCall { .. }) {
            self.skip_next_cref = true;
        }
    }

    fn enter_component_reference(&mut self, comp: &ComponentReference) {
        // Skip function names, only collect actual variable references
        if self.skip_next_cref {
            self.skip_next_cref = false;
        } else {
            self.variables.insert(comp.to_string());
        }
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

impl Visitor for DerivativeFinder {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node {
            if comp.to_string() == "der" && !args.is_empty() {
                if let Expression::ComponentReference(cref) = &args[0] {
                    self.derivatives.push(cref.to_string());
                }
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
///
/// This implementation uses essential assignment preprocessing:
/// Variables that can only be defined by a single equation are force-matched first.
/// This ensures a correct assignment when there are multiple valid matchings,
/// but only one leads to a complete solution.
fn find_maximum_matching(
    eq_infos: &[EquationInfo],
    all_variables: &[String],
    exclude_from_matching: &HashSet<String>,
) -> HashMap<usize, String> {
    let n_equations = eq_infos.len();
    let n_variables = all_variables.len();

    // Build variable name to index mapping
    let var_to_idx: HashMap<&String, usize> = all_variables
        .iter()
        .enumerate()
        .map(|(i, v)| (v, i))
        .collect();

    // Build adjacency lists (equation -> variables it can solve for)
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n_equations];
    // Build reverse adjacency (variable -> equations that can solve for it)
    let mut reverse_adj: Vec<Vec<usize>> = vec![Vec::new(); n_variables];

    for (eq_idx, info) in eq_infos.iter().enumerate() {
        let mut candidates: Vec<usize> = Vec::new();

        // Collect all variables this equation can solve for
        for var in &info.all_variables {
            if !exclude_from_matching.contains(var) {
                if let Some(&var_idx) = var_to_idx.get(var) {
                    if !candidates.contains(&var_idx) {
                        candidates.push(var_idx);
                        reverse_adj[var_idx].push(eq_idx);
                    }
                }
            }
        }

        adj[eq_idx] = candidates;
    }

    // Find essential assignments: variables that can only be solved by one equation
    // These MUST be matched to that equation, otherwise the system is structurally singular
    let mut forced_eq_to_var: HashMap<usize, usize> = HashMap::new();
    let mut forced_var_to_eq: HashMap<usize, usize> = HashMap::new();

    // Iteratively find and propagate essential assignments
    // This handles chains: if var A is essential for eq1, and eq1 had another var B
    // that was essential for eq2, we need to update after removing eq1's other options
    let mut changed = true;
    while changed {
        changed = false;

        for (var_idx, var_eqs) in reverse_adj.iter().enumerate() {
            if forced_var_to_eq.contains_key(&var_idx) {
                continue; // Already assigned
            }

            // Count equations that can still solve for this variable
            let available_eqs: Vec<usize> = var_eqs
                .iter()
                .filter(|&&eq_idx| !forced_eq_to_var.contains_key(&eq_idx))
                .copied()
                .collect();

            if available_eqs.len() == 1 {
                // Essential assignment: only one equation can solve for this variable
                let eq_idx = available_eqs[0];
                forced_eq_to_var.insert(eq_idx, var_idx);
                forced_var_to_eq.insert(var_idx, eq_idx);
                changed = true;
            }
        }
    }

    // Build modified adjacency list that respects forced assignments
    // For forced equations, only allow the forced variable
    let mut adj_modified: Vec<Vec<usize>> = vec![Vec::new(); n_equations];

    for eq_idx in 0..n_equations {
        if let Some(&forced_var) = forced_eq_to_var.get(&eq_idx) {
            // This equation is forced to match this variable
            adj_modified[eq_idx] = vec![forced_var];
        } else {
            // Keep only non-forced variables, preferring LHS variable
            let info = &eq_infos[eq_idx];
            let mut candidates: Vec<usize> = Vec::new();

            // First priority: LHS variable (if not forced elsewhere)
            if let Some(ref lhs_var) = info.lhs_variable {
                if !exclude_from_matching.contains(lhs_var) {
                    if let Some(&var_idx) = var_to_idx.get(lhs_var) {
                        if !forced_var_to_eq.contains_key(&var_idx) {
                            candidates.push(var_idx);
                        }
                    }
                }
            }

            // Second priority: all other non-forced variables
            for var in &info.all_variables {
                if !exclude_from_matching.contains(var) {
                    if let Some(&var_idx) = var_to_idx.get(var) {
                        if !forced_var_to_eq.contains_key(&var_idx)
                            && !candidates.contains(&var_idx)
                        {
                            candidates.push(var_idx);
                        }
                    }
                }
            }

            adj_modified[eq_idx] = candidates;
        }
    }

    // Run Hopcroft-Karp algorithm with modified adjacency
    let mut hk = HopcroftKarp::new(n_equations, n_variables, adj_modified.clone());
    let _matching_size = hk.max_matching();

    // Convert matching to variable names
    let matching = hk.get_equation_matching();
    let mut result = HashMap::new();

    for (eq_idx, var_idx_opt) in matching.iter().enumerate() {
        if let Some(var_idx) = var_idx_opt {
            result.insert(eq_idx, all_variables[*var_idx].clone());
        }
    }

    // Check if all variables are matched; if not, try to fix by reassigning
    let matched_vars: HashSet<_> = result.values().cloned().collect();
    let all_vars_set: HashSet<_> = all_variables.iter().cloned().collect();
    let unmatched_vars: Vec<_> = all_vars_set.difference(&matched_vars).cloned().collect();

    #[cfg(debug_assertions)]
    {
        eprintln!(
            "BLT Debug: Initial matching has {} equations matched to {} unique variables",
            result.len(),
            matched_vars.len()
        );
        eprintln!(
            "BLT Debug: n_equations={}, n_variables={}",
            n_equations, n_variables
        );
        eprintln!("BLT Debug: all_variables={:?}", all_variables);

        // Show reverse_adj for unmatched variables
        for uv in &unmatched_vars {
            if let Some(&var_idx) = var_to_idx.get(uv) {
                let eqs = &reverse_adj[var_idx];
                eprintln!(
                    "BLT Debug: '{}' (idx {}) appears in equations: {:?}",
                    uv, var_idx, eqs
                );
                for &eq in eqs {
                    let eq_vars: Vec<_> = adj[eq].iter().map(|&v| &all_variables[v]).collect();
                    eprintln!("  eq {}: can solve for {:?}", eq, eq_vars);
                }
            }
        }
    }

    if !unmatched_vars.is_empty() {
        // Try to fix unmatched variables by reassigning equations
        result = fix_unmatched_variables(
            &result,
            &unmatched_vars,
            &adj,
            &reverse_adj,
            all_variables,
            eq_infos,
        );

        // Debug output if still unmatched after fix attempt
        #[cfg(debug_assertions)]
        {
            let matched_vars: HashSet<_> = result.values().cloned().collect();
            let still_unmatched: Vec<_> = all_vars_set.difference(&matched_vars).collect();
            if !still_unmatched.is_empty() {
                eprintln!(
                    "BLT Warning: Still unmatched variables after fix attempt: {:?}",
                    still_unmatched
                );
            }
        }
    }

    result
}

/// Try to fix unmatched variables by reassigning equations.
///
/// For each unmatched variable, find an equation that could solve for it,
/// and try to reassign that equation (moving its current assignment elsewhere).
fn fix_unmatched_variables(
    initial_matching: &HashMap<usize, String>,
    unmatched_vars: &[String],
    _adj: &[Vec<usize>],
    reverse_adj: &[Vec<usize>],
    all_variables: &[String],
    _eq_infos: &[EquationInfo],
) -> HashMap<usize, String> {
    let mut result = initial_matching.clone();

    // Build var_to_idx for quick lookup
    let var_to_idx: HashMap<&String, usize> = all_variables
        .iter()
        .enumerate()
        .map(|(i, v)| (v, i))
        .collect();

    // Build current var_to_eq mapping (which equation defines each variable)
    let mut var_to_eq: HashMap<usize, usize> = HashMap::new();
    for (&eq_idx, var_name) in &result {
        if let Some(&var_idx) = var_to_idx.get(var_name) {
            var_to_eq.insert(var_idx, eq_idx);
        }
    }

    // For each unmatched variable, try to find an equation to assign it
    for unmatched_var in unmatched_vars {
        let Some(&unmatched_var_idx) = var_to_idx.get(unmatched_var) else {
            continue;
        };

        // Find equations that can solve for this variable
        let candidate_eqs: Vec<usize> = reverse_adj[unmatched_var_idx].clone();

        #[cfg(debug_assertions)]
        eprintln!(
            "BLT Debug: Trying to fix unmatched var '{}' (idx {}), candidate eqs: {:?}",
            unmatched_var, unmatched_var_idx, candidate_eqs
        );

        #[cfg(debug_assertions)]
        let mut found_fix = false;
        for candidate_eq in candidate_eqs {
            // What variable is this equation currently assigned to?
            let current_var_name = match result.get(&candidate_eq) {
                None => {
                    // Equation not assigned - can directly assign to unmatched var
                    result.insert(candidate_eq, unmatched_var.clone());
                    var_to_eq.insert(unmatched_var_idx, candidate_eq);
                    #[cfg(debug_assertions)]
                    eprintln!(
                        "  -> Assigned eq {} directly to '{}'",
                        candidate_eq, unmatched_var
                    );
                    break;
                }
                Some(name) => name.clone(),
            };

            #[cfg(debug_assertions)]
            eprintln!(
                "  -> eq {} currently assigned to '{}', checking alternatives...",
                candidate_eq, current_var_name
            );

            let Some(&current_var_idx) = var_to_idx.get(&current_var_name) else {
                continue;
            };

            // Can the current variable be solved by another equation?
            // Note: we check if eq is NOT in result.values() indirectly by checking
            // if an equation is already matched to a different variable
            let matched_eqs: HashSet<usize> = result.keys().copied().collect();
            let other_eqs: Vec<usize> = reverse_adj[current_var_idx]
                .iter()
                .filter(|&&eq| eq != candidate_eq && !matched_eqs.contains(&eq))
                .copied()
                .collect();

            #[cfg(debug_assertions)]
            eprintln!(
                "     '{}' can also be solved by unassigned eqs: {:?}",
                current_var_name, other_eqs
            );

            if !other_eqs.is_empty() {
                // Yes! Reassign: candidate_eq -> unmatched_var, other_eq -> current_var
                let other_eq = other_eqs[0];
                result.insert(candidate_eq, unmatched_var.clone());
                result.insert(other_eq, current_var_name.clone());
                var_to_eq.insert(unmatched_var_idx, candidate_eq);
                var_to_eq.insert(current_var_idx, other_eq);
                #[cfg(debug_assertions)]
                {
                    eprintln!(
                        "  -> Reassigned: eq {} -> '{}', eq {} -> '{}'",
                        candidate_eq, unmatched_var, other_eq, current_var_name
                    );
                    found_fix = true;
                }
                break;
            }
        }

        #[cfg(debug_assertions)]
        if !found_fix {
            eprintln!("  -> Could not fix '{}'", unmatched_var);
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
///
/// The `exclude_from_matching` parameter specifies variables that should not
/// be matched to equations (e.g., parameters, constants, time). This ensures that
/// equations like `R * i = v` are solved for the algebraic variable `i` rather
/// than the parameter `R`.
pub fn blt_transform(
    equations: Vec<Equation>,
    exclude_from_matching: &HashSet<String>,
) -> Vec<Equation> {
    blt_transform_with_info(equations, exclude_from_matching).equations
}

/// Perform BLT transformation and return detailed structural information
///
/// This function returns both the transformed equations and additional
/// structural information useful for:
/// - Index reduction (detecting high-index DAEs)
/// - Tearing (optimizing algebraic loop solving)
/// - Diagnostics (debugging structural issues)
pub fn blt_transform_with_info(
    equations: Vec<Equation>,
    exclude_from_matching: &HashSet<String>,
) -> BltResult {
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
                    lhs.accept(&mut lhs_finder);
                    for var in lhs_finder.variables {
                        info.all_variables.insert(var.clone());
                        all_variables_set.insert(var);
                    }
                }
            }

            // Find all variables in RHS
            let mut var_finder = VariableFinder::new();
            rhs.accept(&mut var_finder);
            for var in var_finder.variables {
                info.all_variables.insert(var.clone());
                all_variables_set.insert(var);
            }

            // Also check for der() calls in both LHS and RHS
            let mut der_finder = DerivativeFinder::new();
            lhs.accept(&mut der_finder);
            rhs.accept(&mut der_finder);
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
    // Exclude specified variables from the matching (e.g., parameters, constants, time)
    let all_variables: Vec<String> = {
        let mut vars: Vec<_> = all_variables_set
            .into_iter()
            .filter(|v| !exclude_from_matching.contains(v))
            .collect();
        vars.sort();
        vars
    };

    // Use Hopcroft-Karp to find maximum matching
    let matching = find_maximum_matching(&eq_infos, &all_variables, exclude_from_matching);

    // Update eq_infos with matched variables
    for (eq_idx, var_name) in &matching {
        eq_infos[*eq_idx].matched_variable = Some(var_name.clone());
    }

    // Build dependency graph and find ordering using Tarjan's SCC algorithm
    let tarjan_result = tarjan_scc(&eq_infos);

    // Reorder, normalize, and causalize equations
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
            } else if let Some(normalized) = normalize_derivative_equation(lhs, rhs) {
                // Normalize derivative equations like C * der(x) = y to der(x) = y / C
                result_equations.push(normalized);
            } else {
                // Check if we need to causalize (solve for matched variable)
                if let Some(matched_var) = &info.matched_variable {
                    if let Some(causalized) = causalize_equation(lhs, rhs, matched_var) {
                        result_equations.push(causalized);
                    } else {
                        result_equations.push(info.equation.clone());
                    }
                } else {
                    result_equations.push(info.equation.clone());
                }
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

/// Normalize derivative equations of the form `coeff * der(x) = expr` to `der(x) = expr / coeff`
///
/// This handles cases from component models like:
/// - `C * der(v) = i` (capacitor) -> `der(v) = i / C`
/// - `L * der(i) = v` (inductor) -> `der(i) = v / L`
///
/// Returns None if the equation is not of this form.
fn normalize_derivative_equation(lhs: &Expression, rhs: &Expression) -> Option<Equation> {
    // Check if LHS is coeff * der(x) or der(x) * coeff
    if let Expression::Binary {
        op: OpBinary::Mul(_),
        lhs: mult_lhs,
        rhs: mult_rhs,
    } = lhs
    {
        // Case 1: coeff * der(x)
        if let Expression::FunctionCall { comp, args } = mult_rhs.as_ref() {
            if comp.to_string() == "der" && args.len() == 1 {
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
        }

        // Case 2: der(x) * coeff
        if let Expression::FunctionCall { comp, args } = mult_lhs.as_ref() {
            if comp.to_string() == "der" && args.len() == 1 {
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
    }

    None
}

/// Check if expression contains a der() call
fn has_der_call(expr: &Expression) -> bool {
    let mut finder = DerivativeFinder::new();
    expr.accept(&mut finder);
    !finder.derivatives.is_empty()
}

/// Causalize an equation by solving for a specific variable.
///
/// Given an equation `lhs = rhs` and a variable to solve for, this function
/// attempts to algebraically rewrite the equation so the variable is isolated
/// on the left-hand side.
///
/// Currently handles linear equations of the form:
/// - `var = expr` (already causalized, returns None)
/// - `a + b = 0` where solving for `a` gives `a = -b`
/// - `a + b + c = 0` where solving for `a` gives `a = -(b + c)`
///
/// Returns None if:
/// - The equation is already in the correct form
/// - The variable cannot be isolated (e.g., nonlinear in the variable)
fn causalize_equation(lhs: &Expression, rhs: &Expression, solve_for: &str) -> Option<Equation> {
    // Check if LHS is already just the variable we're solving for
    if let Expression::ComponentReference(cref) = lhs {
        if cref.to_string() == solve_for {
            return None; // Already in correct form
        }
    }

    // Check if RHS is just the variable we're solving for - swap if so
    // E.g., expr = var => var = expr
    if let Expression::ComponentReference(cref) = rhs {
        if cref.to_string() == solve_for {
            return Some(Equation::Simple {
                lhs: rhs.clone(),
                rhs: lhs.clone(),
            });
        }
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
        if let Expression::ComponentReference(cref) = mult_rhs.as_ref() {
            if cref.to_string() == solve_for {
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
        }
        // Check if solve_for is on the left side of multiplication: var * coeff
        if let Expression::ComponentReference(cref) = mult_lhs.as_ref() {
            if cref.to_string() == solve_for {
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
            if let Expression::ComponentReference(cref) = rhs.as_ref() {
                if cref.to_string() == var_name {
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

        let result = blt_transform(equations, &HashSet::new());

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

        let matching = find_maximum_matching(&eq_infos, &all_variables, &HashSet::new());

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

        let result = blt_transform(equations, &HashSet::new());

        // After BLT, z=1 should come first, then y=z, then x=y
        assert_eq!(result.len(), 3);

        // Extract LHS variable names for checking order
        let order: Vec<String> = result
            .iter()
            .filter_map(|eq| {
                if let Equation::Simple {
                    lhs: Expression::ComponentReference(cref),
                    ..
                } = eq
                {
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

        let result = blt_transform(equations, &HashSet::new());

        // Both equations should be present (algebraic loop)
        assert_eq!(result.len(), 2);
    }

    // ========================================================================
    // Causalization Tests
    // ========================================================================

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
    fn test_causalize_sum_to_zero() {
        // Test: a + b = 0 with matching to "a" should become a = -b
        let lhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };
        let rhs = make_zero();

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_some(),
            "Should be able to causalize a + b = 0 for a"
        );

        if let Some(Equation::Simple { lhs, rhs }) = result {
            // LHS should be "a"
            if let Expression::ComponentReference(cref) = lhs {
                assert_eq!(cref.to_string(), "a");
            } else {
                panic!("LHS should be ComponentReference");
            }

            // RHS should be -b (Unary minus of b)
            if let Expression::Unary {
                op: OpUnary::Minus(_),
                rhs,
            } = rhs
            {
                if let Expression::ComponentReference(cref) = *rhs {
                    assert_eq!(cref.to_string(), "b");
                } else {
                    panic!("RHS of negation should be ComponentReference");
                }
            } else {
                panic!("RHS should be Unary negation, got: {:?}", rhs);
            }
        }
    }

    #[test]
    fn test_causalize_three_term_sum() {
        // Test: a + b + c = 0 with matching to "a" should become a = -(b + c)
        let inner = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };
        let lhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(inner),
            rhs: Box::new(make_var("c")),
        };
        let rhs = make_zero();

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_some(),
            "Should be able to causalize a + b + c = 0 for a"
        );

        if let Some(Equation::Simple { lhs, .. }) = result {
            // LHS should be "a"
            if let Expression::ComponentReference(cref) = lhs {
                assert_eq!(cref.to_string(), "a");
            } else {
                panic!("LHS should be ComponentReference");
            }
        }
    }

    #[test]
    fn test_causalize_already_causal() {
        // Test: a = b should return None (already in causal form for "a")
        let lhs = make_var("a");
        let rhs = make_var("b");

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_none(),
            "Should return None for already causal equation"
        );
    }

    #[test]
    fn test_kcl_style_equation() {
        // Simulate KCL equation from circuit: R2_n_i + L1_p_i = 0
        // This should be causalized to R2_n_i = -L1_p_i
        let equations = vec![Equation::Simple {
            lhs: Expression::Binary {
                op: OpBinary::Add(Token::default()),
                lhs: Box::new(make_var("R2_n_i")),
                rhs: Box::new(make_var("L1_p_i")),
            },
            rhs: make_zero(),
        }];

        let result = blt_transform(equations, &HashSet::new());
        assert_eq!(result.len(), 1);

        // The equation should now be causalized
        if let Equation::Simple { lhs, .. } = &result[0] {
            // LHS should be a simple variable reference, not a binary expression
            assert!(
                matches!(lhs, Expression::ComponentReference(_)),
                "LHS should be a simple variable after causalization, got: {:?}",
                lhs
            );
        } else {
            panic!("Expected Simple equation");
        }
    }

    #[test]
    fn test_causalize_zero_on_lhs() {
        // Test: 0 = a + b (alternate form) with matching to "a" should become a = -b
        let lhs = make_zero();
        let rhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_some(),
            "Should be able to causalize 0 = a + b for a"
        );

        if let Some(Equation::Simple { lhs, rhs }) = result {
            // LHS should be "a"
            if let Expression::ComponentReference(cref) = lhs {
                assert_eq!(cref.to_string(), "a");
            } else {
                panic!("LHS should be ComponentReference");
            }

            // RHS should be -b (Unary minus of b)
            if let Expression::Unary {
                op: OpUnary::Minus(_),
                rhs,
            } = rhs
            {
                if let Expression::ComponentReference(cref) = *rhs {
                    assert_eq!(cref.to_string(), "b");
                } else {
                    panic!("RHS of negation should be ComponentReference");
                }
            } else {
                panic!("RHS should be Unary negation, got: {:?}", rhs);
            }
        }
    }

    #[test]
    fn test_kcl_style_equation_zero_on_lhs() {
        // Simulate KCL equation from circuit in the form: 0 = R2_n_i + L1_p_i
        // This should be causalized to one of the variables
        let equations = vec![Equation::Simple {
            lhs: make_zero(),
            rhs: Expression::Binary {
                op: OpBinary::Add(Token::default()),
                lhs: Box::new(make_var("R2_n_i")),
                rhs: Box::new(make_var("L1_p_i")),
            },
        }];

        let result = blt_transform(equations, &HashSet::new());
        assert_eq!(result.len(), 1);

        // The equation should now be causalized
        if let Equation::Simple { lhs, .. } = &result[0] {
            // LHS should be a simple variable reference, not zero
            assert!(
                matches!(lhs, Expression::ComponentReference(_)),
                "LHS should be a simple variable after causalization, got: {:?}",
                lhs
            );
        } else {
            panic!("Expected Simple equation");
        }
    }
}
