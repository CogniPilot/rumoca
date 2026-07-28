//! Maximum matching via augmenting paths (Kuhn's algorithm).
//!
//! The compatibility graph arrives as [`IncidenceRows`]: compressed sparse rows
//! whose runs are already ascending. That is what makes the traversal both
//! allocation-free and deterministic -- the old code re-collected and re-sorted
//! every row on every visit precisely because `HashSet` iteration order is
//! process-random and must never leak into structural choices (`sort_dae`,
//! `eliminate_trivial` and `ic_plan` results feed MSL trace baselines).
//!
//! Both depth-first searches (`augment` and the preference relocation) run on
//! an explicit stack. The recursive forms could overflow the main thread stack
//! on long alternating chains, which array-shaped models produce routinely.

use crate::incidence::rows::IncidenceRows;

/// Find maximum matching in a bipartite graph using augmenting paths.
///
/// Returns `(match_eq, match_var)` where:
/// - `match_eq[i] = Some(j)` means equation `i` is matched to variable `j`
/// - `match_var[j] = Some(i)` means variable `j` is matched to equation `i`
pub(crate) fn maximum_matching(
    n_eq: usize,
    n_var: usize,
    eq_vars: &IncidenceRows,
    preferred_vars: &[Option<usize>],
) -> (Vec<Option<usize>>, Vec<Option<usize>>) {
    maximum_matching_with_structured(n_eq, n_var, eq_vars, preferred_vars, &[])
}

pub(crate) fn maximum_matching_with_structured(
    n_eq: usize,
    n_var: usize,
    eq_vars: &IncidenceRows,
    preferred_vars: &[Option<usize>],
    structured_families: &[crate::incidence::StructuredMatchingFamily],
) -> (Vec<Option<usize>>, Vec<Option<usize>>) {
    let mut match_eq: Vec<Option<usize>> = vec![None; n_eq];
    let mut match_var: Vec<Option<usize>> = vec![None; n_var];
    let seeded = seed_structured_family_matches(
        &mut Matching {
            match_eq: &mut match_eq,
            match_var: &mut match_var,
        },
        eq_vars,
        structured_families,
    );

    let context = MatchContext {
        eq_vars,
        preferred_vars,
    };
    let equation_order = kuhn_equation_order(n_eq, &match_eq, preferred_vars, &seeded);
    let mut scratch = AugmentScratch::new(n_var);
    for eq in equation_order {
        scratch.begin_search();
        augment(
            eq,
            &mut Matching {
                match_eq: &mut match_eq,
                match_var: &mut match_var,
            },
            &context,
            &mut scratch,
        );
    }

    maximize_preserved_preferences(
        &mut Matching {
            match_eq: &mut match_eq,
            match_var: &mut match_var,
        },
        &context,
    );

    (match_eq, match_var)
}

/// The two halves of a partial matching, bundled so the traversal helpers stay
/// under the argument budget.
struct Matching<'a> {
    match_eq: &'a mut [Option<usize>],
    match_var: &'a mut [Option<usize>],
}

/// The immutable inputs every traversal reads.
#[derive(Clone, Copy)]
struct MatchContext<'a> {
    eq_vars: &'a IncidenceRows,
    preferred_vars: &'a [Option<usize>],
}

/// Deterministic visit order of one equation's compatible variables: the
/// causal-target preference first (when it is actually compatible), then the
/// remaining columns in ascending index order.
///
/// This is a *view* over the CSR run -- no `Vec`, no `remove`/`insert` -- and
/// yields exactly the sequence the previous `sort_unstable` + `move_preferred_first`
/// produced.
#[derive(Clone, Copy)]
struct RowOrder<'a> {
    row: &'a [usize],
    preferred_position: Option<usize>,
}

impl<'a> RowOrder<'a> {
    fn new(context: &MatchContext<'a>, eq: usize) -> Self {
        let row = context.eq_vars.row(eq);
        let preferred_position = context
            .preferred_vars
            .get(eq)
            .copied()
            .flatten()
            .and_then(|preferred| row.binary_search(&preferred).ok());
        Self {
            row,
            preferred_position,
        }
    }

    fn get(&self, cursor: usize) -> Option<usize> {
        let Some(position) = self.preferred_position else {
            return self.row.get(cursor).copied();
        };
        let Some(remaining) = cursor.checked_sub(1) else {
            return self.row.get(position).copied();
        };
        let index = if remaining < position {
            remaining
        } else {
            remaining + 1
        };
        self.row.get(index).copied()
    }
}

/// One equation on the alternating-path stack.
#[derive(Debug, Clone, Copy)]
struct AugmentFrame {
    eq: usize,
    cursor: usize,
    chosen: Option<usize>,
}

impl AugmentFrame {
    const fn new(eq: usize) -> Self {
        Self {
            eq,
            cursor: 0,
            chosen: None,
        }
    }
}

/// Reusable buffers for the augmenting search: the explicit DFS stack and the
/// epoch-stamped visited marks (cleared in O(1) per start equation).
struct AugmentScratch {
    stack: Vec<AugmentFrame>,
    visited_epoch: Vec<usize>,
    epoch: usize,
}

impl AugmentScratch {
    fn new(n_var: usize) -> Self {
        Self {
            stack: Vec::new(),
            visited_epoch: vec![0usize; n_var],
            epoch: 0,
        }
    }

    fn begin_search(&mut self) {
        self.epoch = next_visit_epoch(self.epoch, &mut self.visited_epoch);
    }
}

/// Rows seeded straight from a structured family descriptor.
struct StructuredSeedOutcome {
    /// Row ranges in which *every* row was seeded, ascending and disjoint.
    /// Kuhn never needs to look at these rows again.
    fully_seeded_ranges: Vec<std::ops::Range<usize>>,
}

fn seed_structured_family_matches(
    matching: &mut Matching<'_>,
    eq_vars: &IncidenceRows,
    structured_families: &[crate::incidence::StructuredMatchingFamily],
) -> StructuredSeedOutcome {
    let mut fully_seeded_ranges = Vec::new();
    for family in structured_families {
        if seed_one_family(matching, eq_vars, family)
            && let Some(range) = family.row_range()
        {
            fully_seeded_ranges.push(range);
        }
    }
    fully_seeded_ranges.sort_by_key(|range| range.start);
    StructuredSeedOutcome {
        fully_seeded_ranges,
    }
}

/// Seed one family, returning whether every one of its rows got a match.
///
/// The per-point candidate is produced by an odometer over the compact domain
/// instead of `StructuredMatchingFamily::candidate`'s per-dimension divisions;
/// `seed_odometer_agrees_with_candidate` pins the two against each other.
fn seed_one_family(
    matching: &mut Matching<'_>,
    eq_vars: &IncidenceRows,
    family: &crate::incidence::StructuredMatchingFamily,
) -> bool {
    let mut odometer = DomainOdometer::new(&family.extents);
    let mut fully_seeded = true;
    for point in 0..family.point_count {
        for equation_position in 0..family.equations_per_point {
            let candidate = odometer.candidate(family, point, equation_position);
            fully_seeded &= seed_structured_candidate(matching, eq_vars, candidate);
        }
        odometer.advance();
    }
    fully_seeded
}

/// Row-major odometer over a compact domain's binder coordinates.
///
/// `ordinal_strides` are strictly row-major (last stride 1, descending), so
/// advancing one point increments the last coordinate with carry -- no
/// divisions, and the coordinates stay identical to
/// `cell_coordinate(point, stride, extent)`.
struct DomainOdometer {
    coordinates: Vec<usize>,
    extents: Vec<usize>,
}

impl DomainOdometer {
    fn new(extents: &[usize]) -> Self {
        Self {
            coordinates: vec![0usize; extents.len()],
            extents: extents.to_vec(),
        }
    }

    fn advance(&mut self) {
        for dimension in (0..self.coordinates.len()).rev() {
            let Some(&extent) = self.extents.get(dimension) else {
                continue;
            };
            self.coordinates[dimension] += 1;
            if self.coordinates[dimension] < extent {
                return;
            }
            self.coordinates[dimension] = 0;
        }
    }

    fn candidate(
        &self,
        family: &crate::incidence::StructuredMatchingFamily,
        point: usize,
        equation_position: usize,
    ) -> Option<(usize, usize)> {
        if point >= family.point_count || equation_position >= family.equations_per_point {
            return None;
        }
        let equation = point
            .checked_mul(family.equations_per_point)?
            .checked_add(equation_position)?
            .checked_add(family.first_equation_index)?;
        let mut unknown = i64::try_from(*family.base_unknowns.get(equation_position)?).ok()?;
        for (dimension, step) in family
            .unknown_steps
            .get(equation_position)?
            .iter()
            .enumerate()
        {
            let coordinate = i64::try_from(*self.coordinates.get(dimension)?).ok()?;
            unknown = unknown.checked_add(coordinate.checked_mul(*step)?)?;
        }
        Some((equation, usize::try_from(unknown).ok()?))
    }
}

/// Seed one `(equation, variable)` candidate, returning whether the row ended
/// up matched.
fn seed_structured_candidate(
    matching: &mut Matching<'_>,
    eq_vars: &IncidenceRows,
    candidate: Option<(usize, usize)>,
) -> bool {
    let Some((equation, variable)) = candidate else {
        return false;
    };
    if equation >= matching.match_eq.len() || variable >= matching.match_var.len() {
        return false;
    }
    if matching.match_eq[equation].is_some() || matching.match_var[variable].is_some() {
        return false;
    }
    if !eq_vars.contains(equation, variable) {
        return false;
    }
    matching.match_eq[equation] = Some(variable);
    matching.match_var[variable] = Some(equation);
    true
}

/// Equations Kuhn still has to process, in the historical order: untargeted
/// equations first so targeted ones get the last augmenting-path opportunity to
/// reclaim their causal target.
///
/// Rows inside a fully seeded family range are skipped entirely -- they are
/// already matched, so the old `(0..n_eq).filter(...)` passes only paid for
/// them.
fn kuhn_equation_order(
    n_eq: usize,
    match_eq: &[Option<usize>],
    preferred_vars: &[Option<usize>],
    seeded: &StructuredSeedOutcome,
) -> Vec<usize> {
    let residue = unseeded_rows(n_eq, &seeded.fully_seeded_ranges);
    let untargeted = residue.iter().copied().filter(|&eq| {
        match_eq[eq].is_none() && preferred_vars.get(eq).copied().flatten().is_none()
    });
    let targeted = residue.iter().copied().filter(|&eq| {
        match_eq[eq].is_none() && preferred_vars.get(eq).copied().flatten().is_some()
    });
    untargeted.chain(targeted).collect()
}

/// Rows outside every fully seeded family range, ascending.
fn unseeded_rows(n_eq: usize, seeded_ranges: &[std::ops::Range<usize>]) -> Vec<usize> {
    let mut rows = Vec::new();
    let mut row = 0usize;
    for range in seeded_ranges {
        if range.start > row {
            rows.extend(row..range.start.min(n_eq));
        }
        row = row.max(range.end);
        if row >= n_eq {
            return rows;
        }
    }
    rows.extend(row..n_eq);
    rows
}

fn next_visit_epoch(current: usize, visited_epoch: &mut [usize]) -> usize {
    if current == usize::MAX {
        visited_epoch.fill(0);
        1
    } else {
        current + 1
    }
}

/// Lexicographically preserve causal targets without changing cardinality.
///
/// Process equations in stable source order. Once an earlier equation owns its
/// preferred target, later exchanges may not displace it. A preference that is
/// already held by a later equation is not locked yet: an earlier equation may
/// move that equation along an alternating path when doing so preserves the
/// maximum matching cardinality. This is the incremental form of testing each
/// preference as a constrained maximum matching, but reuses the current
/// matching instead of recomputing it from scratch.
fn maximize_preserved_preferences(matching: &mut Matching<'_>, context: &MatchContext<'_>) {
    let n_eq = matching.match_eq.len();
    let n_var = matching.match_var.len();
    let mut locked_preferences = vec![false; n_eq];
    let mut visits = RelocationVisits::new(n_eq, n_var);
    for eq in 0..n_eq {
        let Some(preferred) = context
            .preferred_vars
            .get(eq)
            .copied()
            .flatten()
            .filter(|var| context.eq_vars.contains(eq, *var))
        else {
            continue;
        };
        if matching.match_eq[eq] == Some(preferred) {
            locked_preferences[eq] = true;
            continue;
        }
        if try_claim_preferred(
            ClaimRequest { eq, preferred },
            matching,
            context,
            &mut visits,
            &locked_preferences,
        ) {
            locked_preferences[eq] = true;
        }
    }
}

#[derive(Clone, Copy)]
struct ClaimRequest {
    eq: usize,
    preferred: usize,
}

/// Epoch-stamped visited marks for the relocation search, allocated once per
/// `maximize_preserved_preferences` call instead of once per preferred edge.
struct RelocationVisits {
    equations: Vec<usize>,
    variables: Vec<usize>,
    epoch: usize,
}

impl RelocationVisits {
    fn new(n_eq: usize, n_var: usize) -> Self {
        Self {
            equations: vec![0usize; n_eq],
            variables: vec![0usize; n_var],
            epoch: 0,
        }
    }

    fn begin_search(&mut self) {
        if self.epoch == usize::MAX {
            self.equations.fill(0);
            self.variables.fill(0);
            self.epoch = 1;
            return;
        }
        self.epoch += 1;
    }
}

fn try_claim_preferred(
    request: ClaimRequest,
    matching: &mut Matching<'_>,
    context: &MatchContext<'_>,
    visits: &mut RelocationVisits,
    locked_preferences: &[bool],
) -> bool {
    let Some(owner) = matching.match_var[request.preferred] else {
        assign_match(request.eq, request.preferred, matching);
        return true;
    };
    if owner == request.eq {
        return false;
    }
    visits.begin_search();
    visits.variables[request.preferred] = visits.epoch;
    let relocated = RelocationSearch {
        root: request.eq,
        matching,
        context,
        locked_preferences,
        visits,
    }
    .relocate(owner);
    if !relocated {
        return false;
    }
    assign_match(request.eq, request.preferred, matching);
    true
}

struct RelocationSearch<'a, 'm> {
    root: usize,
    matching: &'a mut Matching<'m>,
    context: &'a MatchContext<'a>,
    locked_preferences: &'a [bool],
    visits: &'a mut RelocationVisits,
}

/// What the relocation scan of one equation found.
enum RelocationStep {
    /// The equation can take `variable` immediately.
    Claim(usize),
    /// The equation wants `variable`, whose current owner must move first.
    Descend { variable: usize, owner: usize },
    /// No usable variable remains.
    Exhausted,
}

impl RelocationSearch<'_, '_> {
    /// Move `start` (and, transitively, the equations it displaces) so the root
    /// equation's preferred variable becomes free. Iterative: the recursive form
    /// nests once per displaced equation.
    fn relocate(&mut self, start: usize) -> bool {
        if self.visits.equations[start] == self.visits.epoch || self.locked_preferences[start] {
            return false;
        }
        self.visits.equations[start] = self.visits.epoch;
        let mut stack = vec![AugmentFrame::new(start)];
        while let Some(top) = stack.last().copied() {
            let mut cursor = top.cursor;
            let step = self.next_relocation_step(top.eq, &mut cursor);
            let Some(frame) = stack.last_mut() else {
                return false;
            };
            frame.cursor = cursor;
            match step {
                RelocationStep::Claim(variable) => {
                    frame.chosen = Some(variable);
                    self.commit(&stack);
                    return true;
                }
                RelocationStep::Descend { variable, owner } => {
                    frame.chosen = Some(variable);
                    stack.push(AugmentFrame::new(owner));
                }
                RelocationStep::Exhausted => {
                    stack.pop();
                }
            }
        }
        false
    }

    /// Advance `cursor` over `eq`'s preference-first variable order until a
    /// usable candidate is found.
    fn next_relocation_step(&mut self, eq: usize, cursor: &mut usize) -> RelocationStep {
        let order = RowOrder::new(self.context, eq);
        while let Some(variable) = order.get(*cursor) {
            *cursor += 1;
            if self.matching.match_eq[eq] == Some(variable)
                || self.visits.variables[variable] == self.visits.epoch
            {
                continue;
            }
            self.visits.variables[variable] = self.visits.epoch;
            let Some(owner) = self.matching.match_var[variable] else {
                return RelocationStep::Claim(variable);
            };
            if owner == self.root {
                return RelocationStep::Claim(variable);
            }
            if self.visits.equations[owner] == self.visits.epoch || self.locked_preferences[owner] {
                continue;
            }
            self.visits.equations[owner] = self.visits.epoch;
            return RelocationStep::Descend { variable, owner };
        }
        RelocationStep::Exhausted
    }

    /// Apply the alternating path innermost-first, which is exactly the order
    /// the recursive form assigned on unwind.
    fn commit(&mut self, stack: &[AugmentFrame]) {
        for frame in stack.iter().rev() {
            let Some(variable) = frame.chosen else {
                continue;
            };
            assign_match(frame.eq, variable, self.matching);
        }
    }
}

fn assign_match(equation: usize, variable: usize, matching: &mut Matching<'_>) {
    if let Some(previous) = matching.match_eq[equation].replace(variable)
        && matching.match_var[previous] == Some(equation)
    {
        matching.match_var[previous] = None;
    }
    matching.match_var[variable] = Some(equation);
}

/// What the augmenting scan of one equation found.
enum AugmentStep {
    /// `variable` is free; the alternating path ends here.
    Claim(usize),
    /// `variable` is held by `owner`, which must be re-matched first.
    Descend { variable: usize, owner: usize },
    /// No unvisited variable remains.
    Exhausted,
}

/// Try to find an augmenting path starting from an unmatched equation.
///
/// Iterative depth-first search over an explicit stack: an array-shaped model
/// can produce alternating chains hundreds of thousands of equations long, and
/// the recursive form overflows the thread stack on those.
fn augment(
    start: usize,
    matching: &mut Matching<'_>,
    context: &MatchContext<'_>,
    scratch: &mut AugmentScratch,
) -> bool {
    let mut stack = std::mem::take(&mut scratch.stack);
    stack.clear();
    stack.push(AugmentFrame::new(start));
    let found = run_augment_search(&mut stack, matching, context, scratch);
    scratch.stack = stack;
    found
}

fn run_augment_search(
    stack: &mut Vec<AugmentFrame>,
    matching: &mut Matching<'_>,
    context: &MatchContext<'_>,
    scratch: &mut AugmentScratch,
) -> bool {
    while let Some(top) = stack.last().copied() {
        let mut cursor = top.cursor;
        let step = next_augment_step(top.eq, &mut cursor, matching, context, scratch);
        let Some(frame) = stack.last_mut() else {
            return false;
        };
        frame.cursor = cursor;
        match step {
            AugmentStep::Claim(variable) => {
                frame.chosen = Some(variable);
                commit_augment(stack, matching);
                return true;
            }
            AugmentStep::Descend { variable, owner } => {
                frame.chosen = Some(variable);
                stack.push(AugmentFrame::new(owner));
            }
            AugmentStep::Exhausted => {
                stack.pop();
            }
        }
    }
    false
}

fn next_augment_step(
    eq: usize,
    cursor: &mut usize,
    matching: &Matching<'_>,
    context: &MatchContext<'_>,
    scratch: &mut AugmentScratch,
) -> AugmentStep {
    let order = RowOrder::new(context, eq);
    while let Some(variable) = order.get(*cursor) {
        *cursor += 1;
        if scratch.visited_epoch[variable] == scratch.epoch {
            continue;
        }
        scratch.visited_epoch[variable] = scratch.epoch;
        let Some(owner) = matching.match_var[variable] else {
            return AugmentStep::Claim(variable);
        };
        return AugmentStep::Descend { variable, owner };
    }
    AugmentStep::Exhausted
}

/// Assign the alternating path innermost-first, reproducing the assignment
/// order of the recursive form's unwind exactly.
fn commit_augment(stack: &[AugmentFrame], matching: &mut Matching<'_>) {
    for frame in stack.iter().rev() {
        let Some(variable) = frame.chosen else {
            continue;
        };
        matching.match_eq[frame.eq] = Some(variable);
        matching.match_var[variable] = Some(frame.eq);
    }
}

#[cfg(test)]
mod tests;
