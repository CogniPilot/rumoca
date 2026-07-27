//! Matching regression tests.
//!
//! The first eight tests are the historical determinism/preference pins: they
//! must pass verbatim after the CSR + iterative-DFS migration, because
//! `sort_dae`/`eliminate_trivial`/`ic_plan` results feed MSL trace baselines.

use std::collections::HashSet;

use super::*;
use crate::incidence::StructuredMatchingFamily;
use crate::incidence::rows::IncidenceRowsBuilder;

fn rows(sets: Vec<HashSet<usize>>) -> IncidenceRows {
    IncidenceRows::from_sets(sets)
}

#[test]
fn test_maximum_matching_perfect() {
    let eq_vars = rows(vec![
        HashSet::from([0, 1]),
        HashSet::from([1, 2]),
        HashSet::from([0, 2]),
    ]);
    let (match_eq, _match_var) = maximum_matching(3, 3, &eq_vars, &[]);
    let size = match_eq.iter().filter(|m| m.is_some()).count();
    assert_eq!(size, 3, "should find perfect matching");
}

#[test]
fn test_maximum_matching_imperfect() {
    let eq_vars = rows(vec![
        HashSet::from([0]),
        HashSet::from([0]),
        HashSet::from([1, 2]),
    ]);
    let (match_eq, _match_var) = maximum_matching(3, 3, &eq_vars, &[]);
    let size = match_eq.iter().filter(|m| m.is_some()).count();
    assert_eq!(size, 2, "imperfect matching: two equations compete for v0");
}

#[test]
fn test_maximum_matching_is_deterministic_under_ties() {
    let eq_vars = rows(vec![HashSet::from([0, 1]), HashSet::from([0, 1])]);
    let (match_eq, match_var) = maximum_matching(2, 2, &eq_vars, &[]);
    assert_eq!(match_eq, vec![Some(1), Some(0)]);
    assert_eq!(match_var, vec![Some(1), Some(0)]);
}

#[test]
fn test_maximum_matching_visits_preferred_edges_first() {
    let eq_vars = rows(vec![HashSet::from([0, 1]), HashSet::from([0, 1])]);
    let (match_eq, match_var) = maximum_matching(2, 2, &eq_vars, &[Some(0), Some(1)]);
    assert_eq!(match_eq, vec![Some(0), Some(1)]);
    assert_eq!(match_var, vec![Some(0), Some(1)]);
}

#[test]
fn test_maximum_matching_keeps_cardinality_when_preferences_conflict() {
    let eq_vars = rows(vec![HashSet::from([0, 1]), HashSet::from([0])]);
    let (match_eq, _match_var) = maximum_matching(2, 2, &eq_vars, &[Some(0), Some(0)]);
    assert_eq!(match_eq, vec![Some(1), Some(0)]);
}

#[test]
fn test_untargeted_equation_does_not_displace_preferred_edge() {
    let eq_vars = rows(vec![HashSet::from([0, 1]), HashSet::from([0, 2])]);
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
    let eq_vars = rows(vec![
        HashSet::from([0, 3]),
        HashSet::from([0, 1]),
        HashSet::from([1, 2, 3]),
        HashSet::from([2, 3]),
    ]);
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

#[test]
fn earlier_preference_can_relocate_a_later_satisfied_preference() {
    let eq_vars = rows(vec![
        HashSet::from([0, 3]),
        HashSet::from([0, 1]),
        HashSet::from([1, 2]),
        HashSet::from([2, 3]),
    ]);
    let preferred = [Some(0), Some(1), Some(2), Some(2)];

    let (match_eq, match_var) = maximum_matching(4, 4, &eq_vars, &preferred);

    assert_eq!(match_eq, vec![Some(0), Some(1), Some(2), Some(3)]);
    assert_eq!(match_var, vec![Some(0), Some(1), Some(2), Some(3)]);
}

#[test]
fn row_order_visits_the_preferred_column_then_ascending_columns() {
    let eq_vars = IncidenceRows::from_sorted_rows(vec![vec![2, 5, 9, 11]]);
    let context = MatchContext {
        eq_vars: &eq_vars,
        preferred_vars: &[Some(9)],
    };
    let order = RowOrder::new(&context, 0);
    let visited: Vec<usize> = (0..5).filter_map(|cursor| order.get(cursor)).collect();
    assert_eq!(visited, vec![9, 2, 5, 11]);

    // An incompatible preference is ignored: plain ascending order.
    let plain_context = MatchContext {
        eq_vars: &eq_vars,
        preferred_vars: &[Some(7)],
    };
    let plain = RowOrder::new(&plain_context, 0);
    let visited: Vec<usize> = (0..5).filter_map(|cursor| plain.get(cursor)).collect();
    assert_eq!(visited, vec![2, 5, 9, 11]);
}

/// A 200_000-long alternating chain: `eq_i ~ {v_i, v_{i+1}}` for the chain plus
/// one trailing equation that can only take `v_0`. The trailing equation forces
/// a single augmenting path through every chain equation, which overflows the
/// recursive form's thread stack.
#[test]
fn augment_survives_a_200k_alternating_chain() {
    const CHAIN: usize = 200_000;
    let mut builder = IncidenceRowsBuilder::with_row_capacity(CHAIN + 1);
    for index in 0..CHAIN {
        builder.push_sorted(&[index, index + 1]);
    }
    builder.push_sorted(&[0]);
    let eq_vars = builder.finish();

    let (match_eq, match_var) = maximum_matching(CHAIN + 1, CHAIN + 1, &eq_vars, &[]);

    assert_eq!(
        match_eq.iter().flatten().count(),
        CHAIN + 1,
        "the chain admits a perfect matching"
    );
    assert_eq!(match_eq[CHAIN], Some(0));
    for (equation, variable) in match_eq.iter().copied().enumerate() {
        assert_eq!(variable.and_then(|index| match_var[index]), Some(equation));
    }
}

/// A 1-D family of `points` cells, one equation per cell, whose cell `p`
/// matches unknown `base + p`.
fn one_dim_family(points: usize, base: usize) -> StructuredMatchingFamily {
    StructuredMatchingFamily {
        span: crate::incidence::corners::tests::test_span(),
        first_equation_index: 0,
        equations_per_point: 1,
        point_count: points,
        extents: vec![points],
        cell_strides: vec![1],
        base_unknowns: vec![base],
        unknown_steps: vec![vec![1]],
    }
}

#[test]
fn structured_seeding_reports_fully_seeded_ranges() {
    const POINTS: usize = 4_096;
    let mut builder = IncidenceRowsBuilder::with_row_capacity(POINTS);
    for index in 0..POINTS {
        builder.push_sorted(&[index]);
    }
    let eq_vars = builder.finish();
    let family = one_dim_family(POINTS, 0);

    let mut match_eq = vec![None; POINTS];
    let mut match_var = vec![None; POINTS];
    let outcome = seed_structured_family_matches(
        &mut Matching {
            match_eq: &mut match_eq,
            match_var: &mut match_var,
        },
        &eq_vars,
        std::slice::from_ref(&family),
    );

    assert_eq!(outcome.fully_seeded_ranges, vec![0..POINTS]);
    assert!(
        kuhn_equation_order(POINTS, &match_eq, &[], &outcome).is_empty(),
        "a fully seeded family leaves Kuhn nothing to do"
    );
    assert!(match_eq.iter().all(Option::is_some));
}

#[test]
fn partially_seeded_family_reports_no_range_and_kuhn_sees_every_row() {
    // Row 2 references a different unknown than the descriptor predicts, so the
    // family is not fully seeded and none of its rows may be skipped.
    let eq_vars = IncidenceRows::from_sorted_rows(vec![vec![0], vec![1], vec![7], vec![3]]);
    let family = one_dim_family(4, 0);

    let mut match_eq = vec![None; 4];
    let mut match_var = vec![None; 8];
    let outcome = seed_structured_family_matches(
        &mut Matching {
            match_eq: &mut match_eq,
            match_var: &mut match_var,
        },
        &eq_vars,
        std::slice::from_ref(&family),
    );

    assert!(outcome.fully_seeded_ranges.is_empty());
    assert_eq!(kuhn_equation_order(4, &match_eq, &[], &outcome), vec![2]);
}

#[test]
fn seed_odometer_agrees_with_candidate() {
    // 3-D domain 2x3x4, two equations per point, distinct per-position steps.
    let family = StructuredMatchingFamily {
        span: crate::incidence::corners::tests::test_span(),
        first_equation_index: 5,
        equations_per_point: 2,
        point_count: 24,
        extents: vec![2, 3, 4],
        cell_strides: vec![12, 4, 1],
        base_unknowns: vec![100, 900],
        unknown_steps: vec![vec![12, 4, 1], vec![-6, 2, 3]],
    };

    let mut odometer = DomainOdometer::new(&family.extents);
    for point in 0..family.point_count {
        for position in 0..family.equations_per_point {
            assert_eq!(
                odometer.candidate(&family, point, position),
                family.candidate(point, position),
                "point {point} position {position}"
            );
        }
        odometer.advance();
    }
}

#[test]
fn unseeded_rows_returns_the_complement_of_the_seeded_ranges() {
    assert_eq!(unseeded_rows(10, &[]), (0..10).collect::<Vec<_>>());
    assert_eq!(unseeded_rows(10, &[0..4, 6..10]), vec![4, 5]);
    assert_eq!(
        unseeded_rows(10, std::slice::from_ref(&(2..5))),
        vec![0, 1, 5, 6, 7, 8, 9]
    );
    assert!(unseeded_rows(4, std::slice::from_ref(&(0..4))).is_empty());
    // Ranges reaching past the row count do not produce out-of-range rows.
    assert_eq!(unseeded_rows(3, std::slice::from_ref(&(1..9))), vec![0]);
}

#[test]
fn matching_is_reproducible_across_repeat_runs() {
    let eq_vars = rows(vec![
        HashSet::from([0, 3, 5]),
        HashSet::from([0, 1]),
        HashSet::from([1, 2, 3]),
        HashSet::from([2, 3, 4]),
        HashSet::from([4, 5]),
        HashSet::from([0, 5]),
    ]);
    let preferred = [Some(3), Some(1), None, Some(2), Some(4), None];
    let first = maximum_matching(6, 6, &eq_vars, &preferred);
    for _ in 0..8 {
        assert_eq!(maximum_matching(6, 6, &eq_vars, &preferred), first);
    }
}

// ---------------------------------------------------------------------------
// Equivalence with the pre-change recursive algorithm.
//
// The iterative `augment`/`relocate` rewrites are the real hazard in the CSR
// migration: they must visit variables in the same order and commit on the
// same first success, or matchings shift and MSL trace baselines move. The
// reference below is the recursive form verbatim (operating on `Vec<HashSet>`
// with the historical `collect + sort_unstable + move_preferred_first`
// ordering); the test drives both over pseudo-random bipartite graphs and
// asserts bit-identical results.
// ---------------------------------------------------------------------------

mod reference {
    // The reference is a VERBATIM copy of the pre-change recursive algorithm.
    // Its nesting is exactly what the rewrite removed; refactoring it here
    // would weaken the equivalence guarantee this module exists to provide.
    #![allow(
        clippy::excessive_nesting,
        reason = "verbatim copy of the pre-change recursive matching, kept unmodified so the equivalence assertion is meaningful"
    )]

    use std::collections::HashSet;

    pub(super) fn maximum_matching(
        n_eq: usize,
        n_var: usize,
        eq_vars: &[HashSet<usize>],
        preferred_vars: &[Option<usize>],
    ) -> (Vec<Option<usize>>, Vec<Option<usize>>) {
        let mut match_eq: Vec<Option<usize>> = vec![None; n_eq];
        let mut match_var: Vec<Option<usize>> = vec![None; n_var];
        let equation_order: Vec<_> = (0..n_eq)
            .filter(|&eq| {
                match_eq[eq].is_none() && preferred_vars.get(eq).copied().flatten().is_none()
            })
            .chain((0..n_eq).filter(|&eq| {
                match_eq[eq].is_none() && preferred_vars.get(eq).copied().flatten().is_some()
            }))
            .collect();
        let mut visited_epoch = vec![0usize; n_var];
        let mut epoch = 0usize;
        for eq in equation_order {
            epoch += 1;
            augment(
                eq,
                &mut match_eq,
                &mut match_var,
                eq_vars,
                preferred_vars,
                &mut visited_epoch,
                epoch,
            );
        }
        maximize_preserved_preferences(&mut match_eq, &mut match_var, eq_vars, preferred_vars);
        (match_eq, match_var)
    }

    fn ordered_vars(
        eq: usize,
        eq_vars: &[HashSet<usize>],
        preferred_vars: &[Option<usize>],
    ) -> Vec<usize> {
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
        vars
    }

    fn augment(
        eq: usize,
        match_eq: &mut [Option<usize>],
        match_var: &mut [Option<usize>],
        eq_vars: &[HashSet<usize>],
        preferred_vars: &[Option<usize>],
        visited_epoch: &mut [usize],
        epoch: usize,
    ) -> bool {
        for var in ordered_vars(eq, eq_vars, preferred_vars) {
            if visited_epoch[var] == epoch {
                continue;
            }
            visited_epoch[var] = epoch;
            let can_augment = match match_var[var] {
                None => true,
                Some(matched_eq) => augment(
                    matched_eq,
                    match_eq,
                    match_var,
                    eq_vars,
                    preferred_vars,
                    visited_epoch,
                    epoch,
                ),
            };
            if can_augment {
                match_eq[eq] = Some(var);
                match_var[var] = Some(eq);
                return true;
            }
        }
        false
    }

    fn maximize_preserved_preferences(
        match_eq: &mut [Option<usize>],
        match_var: &mut [Option<usize>],
        eq_vars: &[HashSet<usize>],
        preferred_vars: &[Option<usize>],
    ) {
        let mut locked_preferences = vec![false; match_eq.len()];
        for eq in 0..match_eq.len() {
            let Some(preferred) = preferred_vars
                .get(eq)
                .copied()
                .flatten()
                .filter(|var| eq_vars[eq].contains(var))
            else {
                continue;
            };
            if match_eq[eq] == Some(preferred) {
                locked_preferences[eq] = true;
                continue;
            }
            if try_claim_preferred(
                eq,
                preferred,
                match_eq,
                match_var,
                eq_vars,
                preferred_vars,
                &locked_preferences,
            ) {
                locked_preferences[eq] = true;
            }
        }
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "verbatim copy of the pre-change signature; changing it would weaken the equivalence guarantee"
    )]
    fn try_claim_preferred(
        eq: usize,
        preferred: usize,
        match_eq: &mut [Option<usize>],
        match_var: &mut [Option<usize>],
        eq_vars: &[HashSet<usize>],
        preferred_vars: &[Option<usize>],
        locked_preferences: &[bool],
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
            locked_preferences,
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
        locked_preferences: &'a [bool],
        visited_eq: &'a mut [bool],
        visited_var: &'a mut [bool],
    }

    impl RelocationSearch<'_> {
        fn relocate(&mut self, eq: usize) -> bool {
            if self.visited_eq[eq] || self.locked_preferences[eq] {
                return false;
            }
            self.visited_eq[eq] = true;
            for variable in ordered_vars(eq, self.eq_vars, self.preferred_vars) {
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
}

/// Deterministic xorshift so the generated graphs are reproducible.
fn next_random(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

#[test]
fn iterative_matching_reproduces_the_recursive_reference_on_random_graphs() {
    let mut state = 0x5DEE_CE66_D125_1337u64;
    for case in 0..600u32 {
        let n_eq = 1 + (next_random(&mut state) % 9) as usize;
        let n_var = 1 + (next_random(&mut state) % 9) as usize;
        let sets: Vec<HashSet<usize>> = (0..n_eq)
            .map(|_| {
                (0..n_var)
                    .filter(|_| !next_random(&mut state).is_multiple_of(3))
                    .collect()
            })
            .collect();
        let preferred: Vec<Option<usize>> = (0..n_eq)
            .map(|_| {
                let draw = next_random(&mut state) as usize;
                (draw.is_multiple_of(2)).then(|| draw / 2 % n_var)
            })
            .collect();

        let expected = reference::maximum_matching(n_eq, n_var, &sets, &preferred);
        let actual = maximum_matching(n_eq, n_var, &rows(sets.clone()), &preferred);
        assert_eq!(
            actual, expected,
            "case {case}: rows {sets:?} preferred {preferred:?}"
        );
    }
}

/// The structured-seeding path is the part of the matching rewrite that has no
/// counterpart in the recursive reference, so the equivalence test above cannot
/// cover it. Its own guard is the pair of properties seeding must not break:
/// the result is still a *valid maximum* matching, and it is reproducible.
///
/// Descriptors are drawn independently of the graph, so the draw covers fully
/// seeded families, partially seeded families (a predicted candidate that is not
/// an edge, or whose unknown is already taken) and descriptors that address rows
/// or unknowns outside the system.
#[test]
fn structured_seeding_preserves_maximum_cardinality_and_stays_reproducible() {
    let mut state = 0x2545_F491_4F6C_DD1Du64;
    for case in 0..600u32 {
        let n_eq = 1 + (next_random(&mut state) % 9) as usize;
        let n_var = 1 + (next_random(&mut state) % 9) as usize;
        let sets: Vec<HashSet<usize>> = (0..n_eq)
            .map(|_| {
                (0..n_var)
                    .filter(|_| !next_random(&mut state).is_multiple_of(3))
                    .collect()
            })
            .collect();
        let family = random_family(&mut state, n_eq, n_var);
        let eq_vars = rows(sets.clone());
        let families = std::slice::from_ref(&family);

        let (match_eq, match_var) =
            maximum_matching_with_structured(n_eq, n_var, &eq_vars, &[], families);

        let context = format!("case {case}: rows {sets:?} family {family:?}");
        assert_matching_is_consistent(&match_eq, &match_var, &sets, &context);
        let (unseeded, _) = maximum_matching(n_eq, n_var, &eq_vars, &[]);
        assert_eq!(
            match_eq.iter().flatten().count(),
            unseeded.iter().flatten().count(),
            "seeding must not cost cardinality -- {context}"
        );
        assert_eq!(
            maximum_matching_with_structured(n_eq, n_var, &eq_vars, &[], families),
            (match_eq, match_var),
            "seeding must be reproducible -- {context}"
        );
    }
}

/// A descriptor over the same index space as the generated graph, deliberately
/// allowed to overshoot it so out-of-range candidates are exercised too.
fn random_family(state: &mut u64, n_eq: usize, n_var: usize) -> StructuredMatchingFamily {
    let points = 1 + (next_random(state) % (n_eq as u64 + 2)) as usize;
    let base = (next_random(state) % (n_var as u64 + 1)) as usize;
    let step: i64 = match next_random(state) % 3 {
        0 => -1,
        1 => 0,
        _ => 1,
    };
    StructuredMatchingFamily {
        span: crate::incidence::corners::tests::test_span(),
        first_equation_index: (next_random(state) % 2) as usize,
        equations_per_point: 1,
        point_count: points,
        extents: vec![points],
        cell_strides: vec![1],
        base_unknowns: vec![base],
        unknown_steps: vec![vec![step]],
    }
}

/// Every matched pair is an edge of the graph and the two halves agree.
fn assert_matching_is_consistent(
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
    sets: &[HashSet<usize>],
    context: &str,
) {
    for (equation, variable) in match_eq.iter().copied().enumerate() {
        let Some(variable) = variable else {
            continue;
        };
        assert!(
            sets[equation].contains(&variable),
            "equation {equation} matched to a non-edge {variable} -- {context}"
        );
        assert_eq!(
            match_var[variable],
            Some(equation),
            "matching halves disagree at variable {variable} -- {context}"
        );
    }
    for (variable, equation) in match_var.iter().copied().enumerate() {
        let Some(equation) = equation else {
            continue;
        };
        assert_eq!(
            match_eq[equation],
            Some(variable),
            "matching halves disagree at equation {equation} -- {context}"
        );
    }
}
