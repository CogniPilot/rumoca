//! Tarjan's Strongly Connected Components Algorithm
//!
//! This module implements Tarjan's algorithm for finding strongly connected
//! components in a directed graph. In the context of BLT transformation, it
//! is used to find the topological ordering of equations and detect algebraic loops.
//!
//! Reference: Tarjan, R. (1972). "Depth-first search and linear graph algorithms"

use super::EquationInfo;
use std::collections::HashMap;

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
pub(super) struct TarjanResult {
    /// Equation indices in topological order
    pub ordered_indices: Vec<usize>,
    /// Strongly connected components (each SCC is a Vec of equation indices)
    pub sccs: Vec<Vec<usize>>,
}

/// Find strongly connected components using Tarjan's algorithm and return equations in topological order
///
/// Tarjan's algorithm finds SCCs in O(V + E) time using a single depth-first search.
/// The SCCs are produced in reverse topological order, so we reverse them at the end.
///
/// Uses matched_variable from Hopcroft-Karp (if available) instead of just lhs_variable
/// for more robust equation-variable assignment.
pub(super) fn tarjan_scc(eq_infos: &[EquationInfo]) -> TarjanResult {
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

            if let Some(&j) = var_to_eq.get(var)
                && i != j
            {
                graph[j].push(i);
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
