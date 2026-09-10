//! Proven ranges of the scalar Integer locals of a lowered function body.
//!
//! A dynamic array index is admitted only when its range is proven inside the
//! subscripted extent, because the emitted code indexes without a runtime
//! check. Function locals earn a range here: an assignment records the range
//! of the value it stores, and the record is narrowed at every control-flow
//! join so that what a reader finds always bounds every value that reader can
//! observe.
//!
//! The join over a conditional is the union over its reaching definitions: an
//! arm that assigns the local contributes the range it stores, and an arm that
//! leaves the local alone contributes the range that held before the
//! conditional, because the prior value survives an untaken arm. A reaching
//! value with no proven range makes the union unproven, never optimistic: the
//! index is then refused, which costs an export but never a bad access.

use super::*;

/// Ranges proven at one point of a function body, keyed by local lexeme.
pub(super) type LocalIntegerBounds = HashMap<String, (i64, i64)>;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    /// Record the proven range of a scalar Integer local, or forget any range
    /// previously proven for it when the new value has none.
    pub(super) fn remember_local_integer_bounds(
        &mut self,
        name: gast::Name,
        value: &gast::Expression,
    ) {
        let lexeme = name.lexeme().to_owned();
        match self.integer_expression_bounds(value) {
            Some(bounds) => {
                self.local_integer_bounds.insert(lexeme, bounds);
            }
            None => {
                self.local_integer_bounds.remove(&lexeme);
            }
        }
    }

    /// Forget the proven ranges of the locals a nested block may assign.
    ///
    /// A range proven outside a loop does not survive into it if the body
    /// reassigns the local, because a reader would then see a value from a
    /// previous iteration; and a range proven inside does not survive out,
    /// because the loop may run zero times. Only the locals the block writes
    /// are affected: an index established before an inner loop and merely read
    /// inside it keeps its range, which is the ordinary shape of a back
    /// substitution walking its right-hand sides.
    ///
    /// This is the answer a loop keeps when [`LoopIntegerBounds`] cannot solve
    /// its entry ranges: dropping a range costs an export and never an access
    /// outside an extent, so it is the direction to fall back to.
    pub(super) fn forget_assigned_local_integer_bounds(&mut self, assigned: &HashSet<String>) {
        self.local_integer_bounds
            .retain(|name, _| !assigned.contains(name));
    }
}

/// Fold one arm's exit ranges into the union of the arms folded so far.
///
/// A local the union no longer bounds on every arm folded so far is dropped:
/// the union holds only what every reaching definition supports.
fn fold_reaching_arm(reaching: &mut Option<LocalIntegerBounds>, exit: &LocalIntegerBounds) {
    let Some(mut folded) = reaching.take() else {
        *reaching = Some(exit.clone());
        return;
    };
    folded.retain(|name, bounds| {
        let Some((minimum, maximum)) = exit.get(name) else {
            return false;
        };
        *bounds = (bounds.0.min(*minimum), bounds.1.max(*maximum));
        true
    });
    *reaching = Some(folded);
}

/// The union of the local ranges proven along each arm of one conditional.
///
/// Built by lowering every arm from the ranges that held at the conditional's
/// entry and folding the ranges each arm leaves proven into a running union.
/// Exactly one arm runs, so the union bounds every reaching value; a local no
/// arm writes carries its entry range through every arm and so survives the
/// union unchanged.
pub(super) struct ConditionalIntegerBounds {
    entry: LocalIntegerBounds,
    reaching: Option<LocalIntegerBounds>,
}

impl ConditionalIntegerBounds {
    /// Capture the ranges proven where the conditional begins.
    pub(super) fn enter(lowerer: &ExpressionLowerer<'_, '_>) -> Self {
        Self {
            entry: lowerer.local_integer_bounds.clone(),
            reaching: None,
        }
    }

    /// Restore the entry ranges before an arm is lowered.
    ///
    /// An arm's condition and body both run only when the preceding arms did
    /// not, so neither may read a range another arm established.
    pub(super) fn start_arm(&self, lowerer: &mut ExpressionLowerer<'_, '_>) {
        lowerer.local_integer_bounds.clone_from(&self.entry);
    }

    /// Fold the ranges the just-lowered arm leaves proven into the union.
    pub(super) fn finish_arm(&mut self, lowerer: &ExpressionLowerer<'_, '_>) {
        fold_reaching_arm(&mut self.reaching, &lowerer.local_integer_bounds);
    }

    /// Install the union as the ranges proven after the conditional.
    pub(super) fn commit(self, lowerer: &mut ExpressionLowerer<'_, '_>) {
        lowerer.local_integer_bounds = self.reaching.unwrap_or(self.entry);
    }
}

/// The ranges a loop leaves proven at its entry and at its exit.
///
/// A loop body reads what the previous iteration left, so the ranges holding
/// where the body begins are not the ranges that held before the loop: they are
/// the union of those with the ranges every iteration can leave behind. That
/// union is a fixpoint, and it is solved by lowering the body speculatively from
/// a candidate entry, joining what the body leaves into the candidate, and
/// repeating until the candidate stops moving.
///
/// The equation solved is `entry = before join body(entry)`, and it is checked
/// by equality rather than assumed: a candidate is accepted only when one more
/// pass reproduces it exactly. That check carries the whole soundness argument
/// and asks no monotonicity of the body's transfer. Every value reaching the
/// body is covered by induction over the iteration count: the first iteration
/// starts from the ranges before the loop, which the union covers, and an
/// iteration starting inside `entry` leaves a value inside `body(entry)`, which
/// the union covers too. The value observed after the loop is covered by those
/// same two cases, so `entry` bounds the exit as well.
///
/// Non-convergence is not an approximation to settle for but a reason to refuse
/// the ranges: a hull can widen without limit, since `k := k + 1` moves its
/// upper end on every pass. The solver therefore stops after a fixed number of
/// attempts and drops every range the body writes.
pub(super) struct LoopIntegerBounds {
    /// The ranges proven before the loop.
    before: LocalIntegerBounds,
    /// The solved entry ranges, or `None` when the loop keeps the drop rule.
    carried: Option<LocalIntegerBounds>,
    /// The names the body may assign, dropped whenever nothing is carried.
    assigned: HashSet<String>,
    /// Whether the emitted loop nest runs its body at least once.
    runs_body: bool,
}

/// How many speculative passes a loop gets before its ranges are dropped.
///
/// Every shape that matters here settles in two: one pass to learn what the body
/// leaves, one to confirm the union reproduces itself. The rest of the budget
/// covers a range that travels through a chain of locals before it settles, and
/// the budget exists at all because a widening hull need never settle.
const LOOP_BOUNDS_PASS_BUDGET: usize = 8;

/// Iterate `entry = before join body(entry)` until it reproduces itself.
///
/// `body_exit` answers the ranges one pass over the body leaves when it starts
/// from the ranges it is given, and `None` when that pass cannot be made, which
/// leaves the exit unknown and so admits no entry.
///
/// Answers `None` when the loop writes no range that held before it, since the
/// union carries only what both sides prove and dropping already gives that
/// answer; and `None` when the budget runs out, which is the fail-closed
/// direction for a hull that keeps widening.
fn solve_loop_entry(
    before: &LocalIntegerBounds,
    assigned: &HashSet<String>,
    mut body_exit: impl FnMut(&LocalIntegerBounds) -> Option<LocalIntegerBounds>,
) -> Option<LocalIntegerBounds> {
    if !assigned.iter().any(|name| before.contains_key(name)) {
        return None;
    }
    let mut entry = before.clone();
    for _ in 0..LOOP_BOUNDS_PASS_BUDGET {
        let exit = body_exit(&entry)?;
        let joined = join_bounds(before, &exit);
        if joined == entry {
            return Some(entry);
        }
        entry = joined;
    }
    None
}

/// The union of two sets of proven ranges, keeping only what both prove.
fn join_bounds(left: &LocalIntegerBounds, right: &LocalIntegerBounds) -> LocalIntegerBounds {
    let mut joined = Some(left.clone());
    fold_reaching_arm(&mut joined, right);
    joined.unwrap_or_default()
}

impl LoopIntegerBounds {
    /// Solve the loop's entry ranges and install them for the body's real pass.
    ///
    /// `trial` lowers the body into a throwaway buffer on a lowerer the solver
    /// clones from the real one, so a speculative pass emits nothing and caches
    /// nothing. A trial that fails to lower leaves the exit ranges unknown, so
    /// the loop keeps the drop rule and the real pass reports whatever
    /// diagnostic the body raises on its own.
    pub(super) fn enter<'a, 'dae>(
        lowerer: &mut ExpressionLowerer<'a, 'dae>,
        assigned: HashSet<String>,
        runs_body: bool,
        mut trial: impl FnMut(&mut ExpressionLowerer<'a, 'dae>) -> Result<(), GalecTargetError>,
    ) -> Self {
        let mut solved = Self {
            before: lowerer.local_integer_bounds.clone(),
            carried: None,
            assigned,
            runs_body,
        };
        solved.carried = solved.solve(lowerer, &mut trial);
        match &solved.carried {
            Some(entry) => lowerer.local_integer_bounds.clone_from(entry),
            None => lowerer.forget_assigned_local_integer_bounds(&solved.assigned),
        }
        solved
    }

    /// Solve the entry ranges, lowering each speculative pass on a clone.
    ///
    /// A pass that fails to lower leaves the exit ranges unknown, which stops
    /// the iteration and drops the loop's ranges.
    fn solve<'a, 'dae>(
        &self,
        lowerer: &ExpressionLowerer<'a, 'dae>,
        trial: &mut impl FnMut(&mut ExpressionLowerer<'a, 'dae>) -> Result<(), GalecTargetError>,
    ) -> Option<LocalIntegerBounds> {
        solve_loop_entry(&self.before, &self.assigned, |entry| {
            let mut speculative = lowerer.clone();
            speculative.local_integer_bounds.clone_from(entry);
            trial(&mut speculative).ok()?;
            Some(speculative.local_integer_bounds)
        })
    }

    /// Install the ranges that hold after the loop.
    ///
    /// A loop whose emitted nest always runs its body leaves exactly what the
    /// last iteration left, so the real pass's own exit ranges stand. A loop
    /// that may run zero times leaves the ranges from before it untouched, and
    /// the solved entry covers that case as well as the body's exit.
    ///
    /// The solved entry was verified against a speculative pass, so the real
    /// pass over the same body from the same ranges leaves the same ones.
    /// Checking that is cheap, and it is the one place where a silent divergence
    /// between the two passes would matter, so a mismatch refuses the model
    /// rather than emitting a body whose indices were proven against ranges the
    /// body does not actually maintain.
    pub(super) fn commit(
        mut self,
        lowerer: &mut ExpressionLowerer<'_, '_>,
        span: Span,
    ) -> Result<(), GalecTargetError> {
        let Some(entry) = self.carried.take() else {
            lowerer.forget_assigned_local_integer_bounds(&self.assigned);
            return Ok(());
        };
        if join_bounds(&self.before, &lowerer.local_integer_bounds) != entry {
            return Err(unsupported(
                "loop-integer-bounds",
                "the loop body left ranges its verified entry does not cover".to_owned(),
                span,
            ));
        }
        if !self.runs_body {
            lowerer.local_integer_bounds = entry;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::{LOOP_BOUNDS_PASS_BUDGET, LocalIntegerBounds, fold_reaching_arm, solve_loop_entry};

    fn bounds(entries: &[(&str, (i64, i64))]) -> LocalIntegerBounds {
        entries
            .iter()
            .map(|(name, range)| ((*name).to_owned(), *range))
            .collect()
    }

    fn written(names: &[&str]) -> HashSet<String> {
        names.iter().map(|name| (*name).to_owned()).collect()
    }

    /// The range a body leaves for `row`, whatever it was given.
    fn leaves(exit: (i64, i64)) -> impl FnMut(&LocalIntegerBounds) -> Option<LocalIntegerBounds> {
        move |_| Some(bounds(&[("row", exit)]))
    }

    fn join(arms: &[LocalIntegerBounds]) -> LocalIntegerBounds {
        let mut reaching = None;
        for exit in arms {
            fold_reaching_arm(&mut reaching, exit);
        }
        reaching.unwrap_or_default()
    }

    #[test]
    fn union_spans_every_arm() {
        let joined = join(&[
            bounds(&[("row", (1, 2))]),
            bounds(&[("row", (4, 4))]),
            bounds(&[("row", (3, 3))]),
        ]);
        assert_eq!(joined.get("row"), Some(&(1, 4)));
    }

    #[test]
    fn an_unproven_arm_leaves_the_union_unproven() {
        let joined = join(&[
            bounds(&[("row", (1, 2))]),
            LocalIntegerBounds::new(),
            bounds(&[("row", (3, 3))]),
        ]);
        assert_eq!(joined.get("row"), None);
    }

    #[test]
    fn an_unproven_first_arm_leaves_the_union_unproven() {
        let joined = join(&[
            LocalIntegerBounds::new(),
            bounds(&[("row", (1, 2))]),
            bounds(&[("row", (3, 3))]),
        ]);
        assert_eq!(joined.get("row"), None);
    }

    #[test]
    fn a_local_no_arm_writes_keeps_its_range() {
        let entry = bounds(&[("column", (1, 4))]);
        let mut assigned_arm = entry.clone();
        assigned_arm.insert("row".to_owned(), (2, 2));
        let joined = join(&[assigned_arm, entry]);
        assert_eq!(joined.get("column"), Some(&(1, 4)));
        assert_eq!(joined.get("row"), None);
    }

    #[test]
    fn the_entry_spans_the_range_before_the_loop_and_the_one_the_body_leaves() {
        let before = bounds(&[("row", (1, 1))]);
        let entry = solve_loop_entry(&before, &written(&["row"]), leaves((1, 3)));
        assert_eq!(entry, Some(bounds(&[("row", (1, 3))])));
    }

    #[test]
    fn an_entry_the_body_reproduces_settles_on_the_first_pass() {
        let before = bounds(&[("row", (1, 4))]);
        let mut passes = 0;
        let entry = solve_loop_entry(&before, &written(&["row"]), |given| {
            passes += 1;
            Some(given.clone())
        });
        assert_eq!(entry, Some(bounds(&[("row", (1, 4))])));
        assert_eq!(passes, 1);
    }

    #[test]
    fn a_range_absent_before_the_loop_is_never_carried_into_it() {
        let before = bounds(&[("column", (1, 4))]);
        let mut passes = 0;
        let entry = solve_loop_entry(&before, &written(&["row"]), |given| {
            passes += 1;
            Some(given.clone())
        });
        assert_eq!(entry, None);
        assert_eq!(passes, 0);
    }

    #[test]
    fn a_range_the_body_leaves_unproven_settles_on_dropping_it() {
        let before = bounds(&[("row", (1, 1))]);
        let entry = solve_loop_entry(&before, &written(&["row"]), |_| {
            Some(LocalIntegerBounds::new())
        });
        assert_eq!(entry, Some(LocalIntegerBounds::new()));
    }

    #[test]
    fn a_hull_that_keeps_widening_spends_its_budget_and_drops() {
        let before = bounds(&[("row", (0, 0))]);
        let mut passes = 0;
        let entry = solve_loop_entry(&before, &written(&["row"]), |given| {
            passes += 1;
            let (_, maximum) = *given.get("row")?;
            Some(bounds(&[("row", (maximum + 1, maximum + 1))]))
        });
        assert_eq!(entry, None);
        assert_eq!(passes, LOOP_BOUNDS_PASS_BUDGET);
    }

    #[test]
    fn a_pass_that_cannot_be_made_drops_the_ranges() {
        let before = bounds(&[("row", (1, 1))]);
        let entry = solve_loop_entry(&before, &written(&["row"]), |_| None);
        assert_eq!(entry, None);
    }

    #[test]
    fn a_settled_entry_covers_the_range_before_the_loop() {
        let before = bounds(&[("row", (7, 9))]);
        let entry = solve_loop_entry(&before, &written(&["row"]), leaves((2, 3)))
            .expect("a body leaving a constant range settles");
        let (minimum, maximum) = entry["row"];
        assert!(
            minimum <= 7 && maximum >= 9,
            "entry {entry:?} misses before"
        );
        assert!(minimum <= 2 && maximum >= 3, "entry {entry:?} misses exit");
    }
}
