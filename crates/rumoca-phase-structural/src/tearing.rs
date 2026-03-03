//! Greedy Cellier-style tearing for algebraic loops.
//!
//! Converts an N-equation algebraic loop into K iteration (tear) variables
//! plus (N-K) causally ordered steps, reducing the nonlinear solve dimension.

use std::collections::{BTreeMap, BTreeSet, HashSet};

/// Result of tearing an algebraic loop.
#[derive(Debug, Clone)]
pub struct TearingResult {
    /// Indices of tear (iteration) variables within the block's unknown list.
    pub tear_var_local_indices: Vec<usize>,
    /// Indices of residual equations within the block's equation list.
    /// Same count as `tear_var_local_indices`.
    pub residual_eq_local_indices: Vec<usize>,
    /// Causal steps: (equation local index, variable local index) in solve order.
    pub causal_sequence: Vec<(usize, usize)>,
}

/// Repeatedly find equations with exactly 1 remaining unknown and solve them causally.
///
/// When multiple equations can solve for the same variable, prefer the equation
/// with fewer total unknowns (less coupling, more likely to be well-conditioned).
fn resolve_causal_equations(
    remaining_eqs: &mut BTreeSet<usize>,
    remaining_unknowns: &mut BTreeSet<usize>,
    causal_sequence: &mut Vec<(usize, usize)>,
    eq_unknowns: &[HashSet<usize>],
) {
    let mut changed = true;
    while changed {
        changed = false;
        // Build map: variable → list of (equation, eq_total_unknowns)
        // This lets us resolve conflicts deterministically
        let mut var_to_eqs: BTreeMap<usize, Vec<(usize, usize)>> = BTreeMap::new();
        for &eq in remaining_eqs.iter() {
            let live: Vec<usize> = eq_unknowns[eq]
                .iter()
                .copied()
                .filter(|v| remaining_unknowns.contains(v))
                .collect();
            if live.len() == 1 {
                let var = live[0];
                var_to_eqs
                    .entry(var)
                    .or_default()
                    .push((eq, eq_unknowns[eq].len()));
            }
        }

        // For each variable that can be solved, pick the best equation:
        // prefer fewer total unknowns (simpler equation), then lower index (deterministic)
        for (var, mut candidates) in var_to_eqs {
            if !remaining_unknowns.contains(&var) {
                continue;
            }
            candidates.sort_by_key(|&(eq, total)| (total, eq));
            let (best_eq, _) = candidates[0];
            causal_sequence.push((best_eq, var));
            remaining_eqs.remove(&best_eq);
            remaining_unknowns.remove(&var);
            changed = true;
        }
    }
}

/// Count how many remaining equations reference each remaining unknown.
fn count_var_appearances(
    remaining_eqs: &BTreeSet<usize>,
    eq_unknowns: &[HashSet<usize>],
    remaining_unknowns: &BTreeSet<usize>,
) -> BTreeMap<usize, usize> {
    let mut var_count: BTreeMap<usize, usize> = BTreeMap::new();
    for &eq in remaining_eqs {
        for &v in &eq_unknowns[eq] {
            if remaining_unknowns.contains(&v) {
                *var_count.entry(v).or_insert(0) += 1;
            }
        }
    }
    var_count
}

/// Apply greedy Cellier-style tearing to an algebraic loop.
///
/// Given equations `eq_indices` and unknowns `var_indices` of equal length N,
/// with `eq_unknowns[i]` giving the set of unknown local indices referenced
/// by equation i:
///
/// 1. Repeatedly find equations with exactly 1 remaining unknown → solve causally.
///    When multiple equations compete for the same variable, prefer the one
///    with fewer total unknowns (less coupling).
/// 2. When stuck, pick the unknown appearing in the most remaining equations
///    as a tear variable and remove it from the "remaining" set.
/// 3. Repeat until all equations are causal or assigned as residuals.
///
/// Returns `None` if tearing makes no progress (all equations reference all unknowns).
pub fn tear_algebraic_loop(n: usize, eq_unknowns: &[HashSet<usize>]) -> Option<TearingResult> {
    if n == 0 {
        return None;
    }

    let mut remaining_eqs: BTreeSet<usize> = (0..n).collect();
    let mut remaining_unknowns: BTreeSet<usize> = (0..n).collect();
    let mut causal_sequence: Vec<(usize, usize)> = Vec::new();
    let mut tear_vars: Vec<usize> = Vec::new();

    loop {
        // Phase 1: find equations with exactly 1 remaining unknown
        resolve_causal_equations(
            &mut remaining_eqs,
            &mut remaining_unknowns,
            &mut causal_sequence,
            eq_unknowns,
        );

        if remaining_eqs.is_empty() {
            break;
        }

        // Phase 2: select tear variable (most appearances in remaining equations,
        // break ties by lowest index for determinism)
        let var_count = count_var_appearances(&remaining_eqs, eq_unknowns, &remaining_unknowns);

        if var_count.is_empty() {
            // No progress possible
            break;
        }

        let &tear_var = var_count
            .iter()
            .max_by_key(|&(v, count)| (*count, std::cmp::Reverse(*v)))
            .map(|(v, _)| v)
            .unwrap();

        tear_vars.push(tear_var);
        remaining_unknowns.remove(&tear_var);
        // Don't remove any equation — they become potential causal or residual
    }

    // The remaining equations are the residual equations (driven by LM)
    let mut residual_eqs: Vec<usize> = remaining_eqs.into_iter().collect();
    residual_eqs.sort_unstable();

    // Only useful if we actually reduced the dimension
    if tear_vars.is_empty() || tear_vars.len() >= n {
        return None;
    }

    // Sanity: residual count should equal tear var count
    if residual_eqs.len() != tear_vars.len() {
        return None;
    }

    Some(TearingResult {
        tear_var_local_indices: tear_vars,
        residual_eq_local_indices: residual_eqs,
        causal_sequence,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tear_linear_chain() {
        // 3 equations: eq0 has {v0}, eq1 has {v0, v1}, eq2 has {v1, v2}
        // All can be solved causally: eq0→v0, eq1→v1, eq2→v2
        let eq_unknowns = vec![
            HashSet::from([0]),
            HashSet::from([0, 1]),
            HashSet::from([1, 2]),
        ];
        let result = tear_algebraic_loop(3, &eq_unknowns);
        // Fully causal — no tear vars needed, but our function returns None
        // when tear_vars is empty (meaning the block isn't really a loop)
        assert!(result.is_none() || result.as_ref().unwrap().tear_var_local_indices.is_empty());
    }

    #[test]
    fn test_tear_simple_2x2_loop() {
        // 2 equations forming a loop: eq0 has {v0, v1}, eq1 has {v0, v1}
        let eq_unknowns = vec![HashSet::from([0, 1]), HashSet::from([0, 1])];
        let result = tear_algebraic_loop(2, &eq_unknowns);
        assert!(result.is_some());
        let r = result.unwrap();
        assert_eq!(r.tear_var_local_indices.len(), 1);
        assert_eq!(r.residual_eq_local_indices.len(), 1);
        assert_eq!(r.causal_sequence.len(), 1);
    }

    #[test]
    fn test_tear_3x3_with_one_tear() {
        // 3-equation loop where tearing one var makes the rest causal
        // eq0: {v0, v1}, eq1: {v1, v2}, eq2: {v0, v2}
        let eq_unknowns = vec![
            HashSet::from([0, 1]),
            HashSet::from([1, 2]),
            HashSet::from([0, 2]),
        ];
        let result = tear_algebraic_loop(3, &eq_unknowns);
        assert!(result.is_some());
        let r = result.unwrap();
        // Should need only 1 tear variable
        assert_eq!(r.tear_var_local_indices.len(), 1);
        assert_eq!(r.causal_sequence.len(), 2);
        assert_eq!(r.residual_eq_local_indices.len(), 1);
    }
}
