//! Maximum matching via augmenting paths (Kuhn's algorithm).

use std::collections::HashSet;

/// Find maximum matching in a bipartite graph using augmenting paths.
///
/// Returns `(match_eq, match_var)` where:
/// - `match_eq[i] = Some(j)` means equation `i` is matched to variable `j`
/// - `match_var[j] = Some(i)` means variable `j` is matched to equation `i`
pub(crate) fn maximum_matching(
    n_eq: usize,
    n_var: usize,
    eq_vars: &[HashSet<usize>],
) -> (Vec<Option<usize>>, Vec<Option<usize>>) {
    let mut match_eq: Vec<Option<usize>> = vec![None; n_eq];
    let mut match_var: Vec<Option<usize>> = vec![None; n_var];

    for eq in 0..n_eq {
        let mut visited = vec![false; n_var];
        augment(eq, &mut match_eq, &mut match_var, eq_vars, &mut visited);
    }

    (match_eq, match_var)
}

/// Try to find an augmenting path starting from an unmatched equation.
fn augment(
    eq: usize,
    match_eq: &mut [Option<usize>],
    match_var: &mut [Option<usize>],
    eq_vars: &[HashSet<usize>],
    visited: &mut [bool],
) -> bool {
    // Deterministic traversal is critical for reproducible BLT/matching.
    // HashSet iteration order is process-random and can otherwise change
    // structural choices between runs.
    let mut vars: Vec<usize> = eq_vars[eq].iter().copied().collect();
    vars.sort_unstable();
    for var in vars {
        if !visited[var] {
            visited[var] = true;
            let can_augment = match match_var[var] {
                None => true,
                Some(matched_eq) => augment(matched_eq, match_eq, match_var, eq_vars, visited),
            };
            if can_augment {
                match_eq[eq] = Some(var);
                match_var[var] = Some(eq);
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_maximum_matching_perfect() {
        let eq_vars = vec![
            HashSet::from([0, 1]),
            HashSet::from([1, 2]),
            HashSet::from([0, 2]),
        ];
        let (match_eq, _match_var) = maximum_matching(3, 3, &eq_vars);
        let size = match_eq.iter().filter(|m| m.is_some()).count();
        assert_eq!(size, 3, "should find perfect matching");
    }

    #[test]
    fn test_maximum_matching_imperfect() {
        let eq_vars = vec![
            HashSet::from([0]),
            HashSet::from([0]),
            HashSet::from([1, 2]),
        ];
        let (match_eq, _match_var) = maximum_matching(3, 3, &eq_vars);
        let size = match_eq.iter().filter(|m| m.is_some()).count();
        assert_eq!(size, 2, "imperfect matching: two equations compete for v0");
    }

    #[test]
    fn test_maximum_matching_is_deterministic_under_ties() {
        let eq_vars = vec![HashSet::from([0, 1]), HashSet::from([0, 1])];
        let (match_eq, match_var) = maximum_matching(2, 2, &eq_vars);
        assert_eq!(match_eq, vec![Some(1), Some(0)]);
        assert_eq!(match_var, vec![Some(1), Some(0)]);
    }
}
