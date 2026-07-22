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
    preferred_vars: &[Option<usize>],
) -> (Vec<Option<usize>>, Vec<Option<usize>>) {
    let mut match_eq: Vec<Option<usize>> = vec![None; n_eq];
    let mut match_var: Vec<Option<usize>> = vec![None; n_var];

    // Match untargeted equations first.  Targeted equations then get the last
    // augmenting-path opportunity to reclaim their causal target while moving
    // an untargeted equation to an alternative unknown.  Processing purely in
    // source order lets a later untargeted equation displace an earlier
    // preferred edge even when an equal-cardinality matching preserves it.
    let equation_order = (0..n_eq)
        .filter(|&eq| preferred_vars.get(eq).copied().flatten().is_none())
        .chain((0..n_eq).filter(|&eq| preferred_vars.get(eq).copied().flatten().is_some()));
    for eq in equation_order {
        let mut visited = vec![false; n_var];
        augment(
            eq,
            &mut match_eq,
            &mut match_var,
            eq_vars,
            preferred_vars,
            &mut visited,
        );
    }

    maximize_preserved_preferences(&mut match_eq, &mut match_var, eq_vars, preferred_vars);

    (match_eq, match_var)
}

/// Improve causal-target preservation without changing matching cardinality.
///
/// A later augmenting path may retain maximum cardinality while displacing an
/// earlier equation from its unique direct-assignment target. Reclaim such a
/// target only when the displaced, currently-unsatisfied equations can be
/// relocated without moving any equation that already owns its preference.
fn maximize_preserved_preferences(
    match_eq: &mut [Option<usize>],
    match_var: &mut [Option<usize>],
    eq_vars: &[HashSet<usize>],
    preferred_vars: &[Option<usize>],
) {
    loop {
        let mut changed = false;
        for eq in 0..match_eq.len() {
            let Some(preferred) = preferred_vars
                .get(eq)
                .copied()
                .flatten()
                .filter(|var| eq_vars[eq].contains(var))
            else {
                continue;
            };
            if match_eq[eq].is_none() || match_eq[eq] == Some(preferred) {
                continue;
            }
            changed |=
                try_claim_preferred(eq, preferred, match_eq, match_var, eq_vars, preferred_vars);
        }
        if !changed {
            break;
        }
    }
}

fn try_claim_preferred(
    eq: usize,
    preferred: usize,
    match_eq: &mut [Option<usize>],
    match_var: &mut [Option<usize>],
    eq_vars: &[HashSet<usize>],
    preferred_vars: &[Option<usize>],
) -> bool {
    let Some(owner) = match_var[preferred] else {
        assign_match(eq, preferred, match_eq, match_var);
        return true;
    };
    if owner == eq {
        return false;
    }
    let mut visited_eq = vec![false; match_eq.len()];
    let mut visited_var = vec![false; match_var.len()];
    visited_var[preferred] = true;
    let relocated = RelocationSearch {
        root: eq,
        match_eq,
        match_var,
        eq_vars,
        preferred_vars,
        visited_eq: &mut visited_eq,
        visited_var: &mut visited_var,
    }
    .relocate(owner);
    if !relocated {
        return false;
    }
    assign_match(eq, preferred, match_eq, match_var);
    true
}

struct RelocationSearch<'a> {
    root: usize,
    match_eq: &'a mut [Option<usize>],
    match_var: &'a mut [Option<usize>],
    eq_vars: &'a [HashSet<usize>],
    preferred_vars: &'a [Option<usize>],
    visited_eq: &'a mut [bool],
    visited_var: &'a mut [bool],
}

impl RelocationSearch<'_> {
    fn relocate(&mut self, eq: usize) -> bool {
        if self.visited_eq[eq]
            || self.preferred_vars.get(eq).copied().flatten() == self.match_eq[eq]
        {
            return false;
        }
        self.visited_eq[eq] = true;
        let mut variables = self.eq_vars[eq].iter().copied().collect::<Vec<_>>();
        variables.sort_unstable();
        move_preferred_first(eq, &mut variables, self.eq_vars, self.preferred_vars);
        for variable in variables {
            if self.match_eq[eq] == Some(variable) || self.visited_var[variable] {
                continue;
            }
            self.visited_var[variable] = true;
            let can_relocate = match self.match_var[variable] {
                None => true,
                Some(owner) if owner == self.root => true,
                Some(owner) => self.relocate(owner),
            };
            if can_relocate {
                assign_match(eq, variable, self.match_eq, self.match_var);
                return true;
            }
        }
        false
    }
}

fn move_preferred_first(
    eq: usize,
    variables: &mut Vec<usize>,
    eq_vars: &[HashSet<usize>],
    preferred_vars: &[Option<usize>],
) {
    if let Some(preferred) = preferred_vars
        .get(eq)
        .copied()
        .flatten()
        .filter(|var| eq_vars[eq].contains(var))
        && let Ok(position) = variables.binary_search(&preferred)
    {
        variables.remove(position);
        variables.insert(0, preferred);
    }
}

fn assign_match(
    equation: usize,
    variable: usize,
    match_eq: &mut [Option<usize>],
    match_var: &mut [Option<usize>],
) {
    if let Some(previous) = match_eq[equation].replace(variable)
        && match_var[previous] == Some(equation)
    {
        match_var[previous] = None;
    }
    match_var[variable] = Some(equation);
}

/// Try to find an augmenting path starting from an unmatched equation.
fn augment(
    eq: usize,
    match_eq: &mut [Option<usize>],
    match_var: &mut [Option<usize>],
    eq_vars: &[HashSet<usize>],
    preferred_vars: &[Option<usize>],
    visited: &mut [bool],
) -> bool {
    // Deterministic traversal is critical for reproducible BLT/matching.
    // HashSet iteration order is process-random and can otherwise change
    // structural choices between runs.
    let mut vars: Vec<usize> = eq_vars[eq].iter().copied().collect();
    vars.sort_unstable();
    if let Some(preferred) = preferred_vars
        .get(eq)
        .copied()
        .flatten()
        .filter(|var| eq_vars[eq].contains(var))
        && let Ok(position) = vars.binary_search(&preferred)
    {
        vars.remove(position);
        vars.insert(0, preferred);
    }
    for var in vars {
        if !visited[var] {
            visited[var] = true;
            let can_augment = match match_var[var] {
                None => true,
                Some(matched_eq) => augment(
                    matched_eq,
                    match_eq,
                    match_var,
                    eq_vars,
                    preferred_vars,
                    visited,
                ),
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
        let (match_eq, _match_var) = maximum_matching(3, 3, &eq_vars, &[]);
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
        let (match_eq, _match_var) = maximum_matching(3, 3, &eq_vars, &[]);
        let size = match_eq.iter().filter(|m| m.is_some()).count();
        assert_eq!(size, 2, "imperfect matching: two equations compete for v0");
    }

    #[test]
    fn test_maximum_matching_is_deterministic_under_ties() {
        let eq_vars = vec![HashSet::from([0, 1]), HashSet::from([0, 1])];
        let (match_eq, match_var) = maximum_matching(2, 2, &eq_vars, &[]);
        assert_eq!(match_eq, vec![Some(1), Some(0)]);
        assert_eq!(match_var, vec![Some(1), Some(0)]);
    }

    #[test]
    fn test_maximum_matching_visits_preferred_edges_first() {
        let eq_vars = vec![HashSet::from([0, 1]), HashSet::from([0, 1])];
        let (match_eq, match_var) = maximum_matching(2, 2, &eq_vars, &[Some(0), Some(1)]);
        assert_eq!(match_eq, vec![Some(0), Some(1)]);
        assert_eq!(match_var, vec![Some(0), Some(1)]);
    }

    #[test]
    fn test_maximum_matching_keeps_cardinality_when_preferences_conflict() {
        let eq_vars = vec![HashSet::from([0, 1]), HashSet::from([0])];
        let (match_eq, _match_var) = maximum_matching(2, 2, &eq_vars, &[Some(0), Some(0)]);
        assert_eq!(match_eq, vec![Some(1), Some(0)]);
    }

    #[test]
    fn test_untargeted_equation_does_not_displace_preferred_edge() {
        let eq_vars = vec![HashSet::from([0, 1]), HashSet::from([0, 2])];
        let (match_eq, match_var) = maximum_matching(2, 3, &eq_vars, &[Some(0), None]);

        assert_eq!(match_eq[0], Some(0));
        assert_eq!(match_var[0], Some(0));
        assert_eq!(
            match_eq.iter().filter(|matched| matched.is_some()).count(),
            2
        );
    }

    #[test]
    fn post_matching_exchange_recovers_causal_targets_without_losing_cardinality() {
        let eq_vars = vec![
            HashSet::from([0, 3]),
            HashSet::from([0, 1]),
            HashSet::from([1, 2, 3]),
            HashSet::from([2, 3]),
        ];
        let preferred = [Some(0), Some(1), None, Some(2)];

        let (match_eq, match_var) = maximum_matching(4, 4, &eq_vars, &preferred);

        assert_eq!(match_eq.iter().flatten().count(), 4);
        assert_eq!(match_eq[0], Some(0));
        assert_eq!(match_eq[1], Some(1));
        assert_eq!(match_eq[3], Some(2));
        for (equation, variable) in match_eq.iter().copied().enumerate() {
            assert_eq!(variable.and_then(|index| match_var[index]), Some(equation));
        }
    }
}
