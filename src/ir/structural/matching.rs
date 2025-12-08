//! Hopcroft-Karp Maximum Bipartite Matching Algorithm
//!
//! This module implements the Hopcroft-Karp algorithm for finding maximum matchings
//! in bipartite graphs. In the context of BLT transformation, it matches equations
//! to variables.
//!
//! Reference: Hopcroft, J. & Karp, R. (1973). "An n^(5/2) Algorithm for Maximum
//! Matchings in Bipartite Graphs"

use super::EquationInfo;
use std::collections::{HashMap, HashSet, VecDeque};

/// Sentinel value representing "no match" in Hopcroft-Karp
const NIL: usize = usize::MAX;

/// Hopcroft-Karp algorithm for maximum bipartite matching
///
/// Given a bipartite graph with equations on one side and variables on the other,
/// finds the maximum matching that assigns each equation to exactly one variable.
///
/// Time complexity: O(E * sqrt(V))
pub(super) struct HopcroftKarp {
    /// Number of equations (left side of bipartite graph)
    n_equations: usize,
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
    pub fn new(n_equations: usize, n_variables: usize, adj: Vec<Vec<usize>>) -> Self {
        Self {
            n_equations,
            adj,
            pair_eq: vec![NIL; n_equations],
            pair_var: vec![NIL; n_variables],
            dist: vec![0; n_equations + 1],
        }
    }

    /// Run the Hopcroft-Karp algorithm and return the matching size
    pub fn max_matching(&mut self) -> usize {
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
    pub fn get_equation_matching(&self) -> Vec<Option<usize>> {
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
pub(super) fn find_maximum_matching(
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
            if !exclude_from_matching.contains(var)
                && let Some(&var_idx) = var_to_idx.get(var)
                && !candidates.contains(&var_idx)
            {
                candidates.push(var_idx);
                reverse_adj[var_idx].push(eq_idx);
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
            if let Some(ref lhs_var) = info.lhs_variable
                && !exclude_from_matching.contains(lhs_var)
                && let Some(&var_idx) = var_to_idx.get(lhs_var)
                && !forced_var_to_eq.contains_key(&var_idx)
            {
                candidates.push(var_idx);
            }

            // Second priority: all other non-forced variables
            for var in &info.all_variables {
                if !exclude_from_matching.contains(var)
                    && let Some(&var_idx) = var_to_idx.get(var)
                    && !forced_var_to_eq.contains_key(&var_idx)
                    && !candidates.contains(&var_idx)
                {
                    candidates.push(var_idx);
                }
            }

            adj_modified[eq_idx] = candidates;
        }
    }

    // Run Hopcroft-Karp algorithm with modified adjacency
    let mut hk = HopcroftKarp::new(n_equations, n_variables, adj_modified);
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

    if !unmatched_vars.is_empty() {
        // Try to fix unmatched variables by reassigning equations
        result = fix_unmatched_variables(&result, &unmatched_vars, &reverse_adj, all_variables);
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
    reverse_adj: &[Vec<usize>],
    all_variables: &[String],
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

        for candidate_eq in candidate_eqs {
            // What variable is this equation currently assigned to?
            let current_var_name = match result.get(&candidate_eq) {
                None => {
                    // Equation not assigned - can directly assign to unmatched var
                    result.insert(candidate_eq, unmatched_var.clone());
                    var_to_eq.insert(unmatched_var_idx, candidate_eq);
                    break;
                }
                Some(name) => name.clone(),
            };

            let Some(&current_var_idx) = var_to_idx.get(&current_var_name) else {
                continue;
            };

            // Can the current variable be solved by another equation?
            let matched_eqs: HashSet<usize> = result.keys().copied().collect();
            let other_eqs: Vec<usize> = reverse_adj[current_var_idx]
                .iter()
                .filter(|&&eq| eq != candidate_eq && !matched_eqs.contains(&eq))
                .copied()
                .collect();

            if !other_eqs.is_empty() {
                // Yes! Reassign: candidate_eq -> unmatched_var, other_eq -> current_var
                let other_eq = other_eqs[0];
                result.insert(candidate_eq, unmatched_var.clone());
                result.insert(other_eq, current_var_name.clone());
                var_to_eq.insert(unmatched_var_idx, candidate_eq);
                var_to_eq.insert(current_var_idx, other_eq);
                break;
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, ComponentReference, Equation, Expression, Token};

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
}
