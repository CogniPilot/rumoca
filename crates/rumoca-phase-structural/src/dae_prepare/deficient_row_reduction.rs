//! Equation-driven index reduction: differentiate the deficient constraint rows.
//!
//! The state-driven passes ([`super::constrained_dummy_derivative`] and
//! [`super::state_row_reduction`]) both nominate by *state*: they ask which
//! state lacks an assignable `der(state)` row and differentiate the row that
//! defines it. Pantelides asks the dual question — which *set of equations*
//! spans fewer unknowns than it has rows — and differentiates those equations.
//!
//! The two coincide on an open kinematic chain, where the deficient set is the
//! definition of one state. They diverge on a closed loop: the deficient set is
//! a group of connection equations (`b0.frame_b.r_0 = j2.frame_a.r_0`,
//! `zeros(3) = Frames.Orientation.equalityConstraint(...)`) that assigns no
//! state at all, so state-driven nomination never sees it and the loop-closure
//! rows survive undifferentiated.
//!
//! This pass closes that gap. It runs *before* scalarization, on a
//! scalar-width rank witness built for the purpose
//! ([`scalar_rank_view`]) — the symbolic derivative closure keys defining
//! expressions on aggregate variable names, and after scalarization no row
//! assigns `b0.frame_b.r_0` any more, so every nomination is rejected as an
//! unresolved `der` leaf. When the matching is perfect the pass is a no-op
//! beyond building that view once.
//!
//! # Minimal deficient subset, not the alternating-path closure
//!
//! The Dulmage-Mendelsohn over-determined block reported by `ES010` is the
//! alternating-path *closure* from the unmatched rows. On
//! `MultiBody.Examples.Loops.Fourbar1` that closure is 783 rows: it walks the
//! whole kinematic chain back to `world`, whose rows pin `frame_b.r_0` to
//! constants, and to `j1`, which is declared `stateSelect = StateSelect.always`
//! so no demotion of it is admissible. Differentiating the closure would
//! differentiate a third of the model.
//!
//! So the closure is walked in *shells* instead: shell 0 is the unmatched rows
//! themselves, shell `k + 1` the rows reached from shell `k` through one
//! matched column. The pass differentiates the first shell that contains any
//! admissible constraint row and stops there. That is the smallest subset of
//! the deficient block that can supply the missing rank, which is what
//! Pantelides differentiates — the rest of the closure is reachable, not
//! deficient.
//!
//! # One differentiation is not enough
//!
//! A loop closure is stated at *position* level, and the unknowns it leaves
//! undetermined are constraint forces and torques — which are determined at
//! *acceleration* level. Position to velocity to acceleration is two
//! differentiations, so the pass iterates: differentiate the deficient set,
//! re-derive the matching, repeat, and judge the accumulated work once at the
//! end ([`index_reduce_deficient_constraint_rows`]). Judging a single round
//! cannot work, because the first differentiation moves the closure to velocity
//! level and leaves the force exactly as undetermined as it was: the deficiency
//! plateaus, and the round that would pay off is never reached.
//!
//! The set carried into the next round is not re-derived from the new matching
//! alone: the rows the previous round rewrote are re-nominated with it (see
//! [`Nomination::carry_forward`]), because the matching is read off an
//! over-approximate view and a row can leave that view's unmatched set while the
//! real system still has no column for it.
//!
//! # Where `Fourbar1` stands
//!
//! The iteration reaches acceleration level and the *view* reaches a perfect
//! matching, which no configuration of this pass had done before. It is not
//! retained, and the model still reports `ES010` at 559 of 565: the residual
//! that buys the last of the rank is 75_866 nodes, above
//! [`MAX_DIFFERENTIATED_NODES`], and admitting it costs thirteen times the
//! compile and then fails in `EL004` instead. Both constants carry the
//! measurement.
//!
//! # Convention
//!
//! Like [`super::state_row_reduction`], and unlike
//! [`super::constrained_dummy_derivative`], the differentiated row *replaces*
//! the original and the original is retained in `initialization`. The naming
//! form is not available here: it funds a retained row with one new unknown per
//! demoted scalar state, and these rows demote no state at all, so there is
//! nothing to fund them with. Replacement keeps the row and column counts
//! unchanged while changing which columns the row spans, which is exactly the
//! rank the matching was missing.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae::DerivativeNameMatcher;

use crate::structural_trace;

use super::constrained_dummy_derivative::CONSTRAINED_DUMMY_ROW_ORIGIN;
use super::derivative_map::{
    RelaxedDerivativeMapOptions, build_relaxed_derivative_map_for_exprs_with_index,
};
use super::scalar_rank_view;
use super::state_row_reduction::expression_is_smooth_for_index_reduction;
use super::symbolic::symbolic_time_derivative;
use super::{
    Dae, Expression, VarName, collect_residual_defining_expr_index, derivative_states_in_eq,
    eq_contains_any_state_der_with_matcher,
};

/// Origin marker appended to a row this pass differentiated.
const DEFICIENT_ROW_ORIGIN: &str = "index_reduction:d_dt_deficient_constraint_row";

/// Maximum prolongation rounds in one iteration to a fixed point.
///
/// Each round differentiates the current deficient set once and re-derives the
/// matching, so a system of differentiation index `k` needs `k - 1` rounds. A
/// closed kinematic loop states its closure at *position* level while the
/// constraint force it leaves undetermined is fixed at *acceleration* level, so
/// index 3 — two rounds — is what MSL's mechanisms actually demand, and two is
/// what this allows.
///
/// A third round is not free headroom, it is the expensive one: differentiating
/// `Fourbar1`'s orientation closure a third time takes the compile from 32 s to
/// 246 s and the peak from 5.5 GB to 10.5 GB, all of it spent building a
/// residual the node budget then rejects. A cap that stops at the highest index
/// the library contains is both the correct bound and the affordable one.
///
/// Exhausting the cap is not silent recovery. The whole sequence is reverted, so
/// the deficiency the pass could not remove is still there for the real
/// scalarized matching to report as `ES010` — naming the scalar rows that stayed
/// unmatched, which is a strictly better diagnostic than this pass could span
/// from an over-approximate view whose rows are aggregates.
const MAX_ROUNDS: usize = 2;

/// Node budget for one differentiated row.
///
/// Differentiating `Frames.Orientation.equalityConstraint` expands a ~20-node
/// residual into a ~4600-node one, so a *ratio* bound cannot express the
/// intent. What must be bounded is the absolute size a single residual may
/// reach, because the compiled row is evaluated on every Jacobian column.
///
/// # Why `Fourbar1`'s acceleration-level orientation row does not fit
///
/// Raising this to admit it was measured end to end, and the measurement is the
/// reason the bound stayed where it is. `Fourbar1`'s orientation closure is
/// 4_640 nodes once differentiated and **75_866** twice — about 25_000 nodes per
/// scalar row. Admitting it does close the structural hole: the model goes from
/// 565 matched of 568 to a perfect matching and clears `ES010` for the first
/// time. It then costs 247 s and 10.5 GB to compile, against 19 s and 1.25 GB
/// for the same model failing early, and still does not simulate — it fails in
/// `EL004`, because the position-level row retained for initialization is a raw
/// `Frames.Orientation.equalityConstraint` call the initialization partition
/// cannot lower.
///
/// So the bound is not what blocks that model, and lifting it buys a different
/// failure at thirteen times the cost. It is lifted when the acceleration-level
/// residual can be shared instead of inlined per scalar row, and when
/// initialization can lower a function call.
const MAX_DIFFERENTIATED_NODES: usize = 32_768;

/// How many alternating-path shells may be nominated.
///
/// One: the unmatched rows themselves. A deeper shell is reached *through* a
/// matched column, which makes it the defining row of that column — and
/// differentiating a defining row destroys the value every other row reads from
/// it, which is why the state-driven passes exclude defining rows too. Walking
/// deeper was measured on `Fourbar1` to attempt 141 rows in shell 1 and 500+ in
/// shell 2, reject every one, and take longer than the whole rest of the
/// compile; there is no shell beyond the first whose rows are deficient rather
/// than merely reachable.
const NOMINATED_SHELL_DEPTH: usize = 1;

/// One source constraint retained by equation-driven index reduction.
///
/// `holonomic` is the position-level residual. `velocity` is present when the
/// same row was prolonged a second time to acceleration level. This is a
/// structural sidecar rather than a DAE field: the finalized DAE contains the
/// acceleration-level equation, while runtimes need the lower-order manifold
/// equations to control numerical drift.
#[derive(Debug, Clone)]
pub struct IndexReducedConstraint {
    pub source_row: usize,
    pub holonomic: rumoca_ir_dae::Equation,
    pub velocity: Option<rumoca_ir_dae::Equation>,
}

/// Result of equation-driven index reduction, including the manifold contract
/// that must accompany the rewritten DAE into simulation lowering.
#[derive(Debug, Clone, Default)]
pub struct IndexReductionResult {
    pub differentiated_rows: usize,
    pub constraints: Vec<IndexReducedConstraint>,
}

/// Differentiate the deficient constraint rows until the matching is perfect.
///
/// Returns the number of rows differentiated. A model whose scalarized view
/// already has a perfect matching is unchanged and pays one incidence build.
///
/// # Why this iterates instead of judging one round
///
/// Pantelides is a fixed-point iteration: differentiate the deficient set,
/// re-match, repeat. Judging each round on its own — accepting only a round
/// whose deficiency falls — cannot reduce a system of index 3, because the first
/// differentiation of a position-level loop closure moves it to velocity level
/// and leaves the constraint *force* exactly as undetermined as it was. The
/// deficiency plateaus, and the second differentiation, the one that determines
/// the force, never runs.
///
/// So the whole sequence is one transaction: rounds run to a fixed point and the
/// accumulated work is judged once, by [`Prolongation::is_accepted`]. Nothing is
/// retained unless the iteration ends holding a perfect matching that still
/// determines every column the matching it started from determined. An
/// iteration whose rank witness could not be rebuilt holds no such matching and
/// reverts like any other failure.
///
/// # Failure
///
/// Never fails: a model whose derivative closure cannot be built simply is not
/// index-reduced here, which is the behaviour that predates this pass.
pub fn index_reduce_deficient_constraint_rows(dae: &mut Dae) -> usize {
    index_reduce_deficient_constraint_rows_with_metadata(dae).differentiated_rows
}

/// Equation-driven index reduction with explicit lower-order constraint
/// metadata for solver manifold projection.
pub fn index_reduce_deficient_constraint_rows_with_metadata(dae: &mut Dae) -> IndexReductionResult {
    if dae.continuous.equations.is_empty() {
        return IndexReductionResult::default();
    }
    let RankOutcome::Deficient(first) = nominate_deficient_rows(dae) else {
        return IndexReductionResult::default();
    };
    let before = first.deficiency;
    let determined_before = first.determined_columns.clone();
    let mut undo: Vec<RoundSnapshot> = Vec::new();
    let prolongation = run_prolongation(dae, first, &mut undo);
    if prolongation.is_accepted(&determined_before) {
        return IndexReductionResult {
            differentiated_rows: prolongation.rows,
            constraints: retained_constraint_metadata(&undo),
        };
    }
    structural_trace!(
        "[sim-trace] deficient-row prolongation reverted rows={} deficiency_before={before} outcome={}",
        prolongation.rows,
        prolongation.outcome.describe()
    );
    for snapshot in undo.into_iter().rev() {
        snapshot.apply(dae);
    }
    IndexReductionResult::default()
}

fn retained_constraint_metadata(snapshots: &[RoundSnapshot]) -> Vec<IndexReducedConstraint> {
    let mut by_row = std::collections::BTreeMap::<usize, Vec<rumoca_ir_dae::Equation>>::new();
    for snapshot in snapshots {
        for (row, equation) in &snapshot.rows {
            by_row.entry(*row).or_default().push(equation.clone());
        }
    }
    by_row
        .into_iter()
        .filter_map(|(source_row, mut levels)| {
            let holonomic = levels.first().cloned()?;
            let velocity = (levels.len() > 1).then(|| levels.swap_remove(1));
            Some(IndexReducedConstraint {
                source_row,
                holonomic,
                velocity,
            })
        })
        .collect()
}

/// What one iteration to a fixed point cost and what it bought.
struct Prolongation {
    /// Rows differentiated across every round of the iteration.
    rows: usize,
    /// How the iteration ended.
    outcome: ProlongationOutcome,
}

/// The state of the rank witness when the iteration stopped.
///
/// The three cases are kept apart because only one of them is evidence of
/// anything. Collapsing "the view could not be built" into "the matching is
/// perfect" — which an `Option` return did — retains a prolongation on a model
/// whose rank witness declined, which is the opposite of what declining means.
enum ProlongationOutcome {
    /// Every row of the view is matched: the strong outcome, because a perfect
    /// matching is a witness that can be exhibited rather than a bound that
    /// moved.
    ///
    /// Carries the columns that matching determines, so acceptance can also ask
    /// whether the rewrites took a column's only defining row away (see
    /// [`Prolongation::is_accepted`]).
    Perfect { determined_columns: Vec<bool> },
    /// The view was built and still leaves this many rows unmatched.
    Deficient(usize),
    /// The view could not be built, or was degenerate. The iteration proved
    /// nothing, in either direction.
    Indeterminate,
}

impl ProlongationOutcome {
    /// Trace-friendly rendering; the column vector is a whole-model bitmap and
    /// is deliberately not printed.
    fn describe(&self) -> String {
        match self {
            Self::Perfect { .. } => "perfect_matching".to_string(),
            Self::Deficient(deficiency) => format!("deficiency={deficiency}"),
            Self::Indeterminate => "indeterminate_view".to_string(),
        }
    }
}

impl Prolongation {
    /// Should the accumulated rewrites be kept?
    ///
    /// Only when the iteration ended holding a *perfect matching* of the view
    /// that still determines every column the matching it started from
    /// determined.
    ///
    /// # Why a fall in the deficiency is not enough
    ///
    /// The view's deficiency is a *lower* bound on the real scalarized system's:
    /// it hands rows extra columns, never extra rows, so it can only over-match.
    /// `deficiency > 0` therefore *proves* the real system is deficient, which
    /// is what makes nominating an iteration sound at all. A *fall* in the same
    /// bound proves nothing of the kind — both readings are bounds, and the
    /// slack between a bound and the truth moves on its own when a round
    /// rewrites the rows the bound is computed from.
    ///
    /// That mattered nothing while every round was reverted. It matters now that
    /// rounds are retained, and it was measured: accepting a fall lets
    /// `Fourbar1` keep a partial reduction that takes it from 6 unmatched rows
    /// to 3 while more than doubling its compile time and tripling its peak
    /// memory, on a model that fails either way. A perfect matching is an
    /// exhibited assignment rather than a bound that moved, and it is the fixed
    /// point Pantelides terminates on.
    ///
    /// # Why the witness is not per row
    ///
    /// Requiring that every rewritten row have left the unmatched set was
    /// measured and is wrong: a row can carry new rank while the *maximum*
    /// matching still chooses to leave that particular row unmatched. It
    /// reverted the round that repairs
    /// `rumoca_sim::solve_lowering::tests::simulation_direct_lowering_falls_back_for_state_selection`
    /// and left that model structurally singular.
    ///
    /// # What this still does not prove
    ///
    /// A perfect matching of an over-approximation is not a perfect matching of
    /// the real system; the real scalarized matching downstream is the
    /// authority, and it still reports `ES010` when the view was optimistic. A
    /// criterion that is sufficient outright needs an *exact* scalar view, which
    /// is a change to where index reduction may run, not to this gate.
    ///
    /// # Why the columns are checked as well as the rows
    ///
    /// A perfect matching is a statement about *rows*: every row got a column.
    /// The second round of a prolongation re-differentiates rows the first round
    /// rewrote ([`Nomination::carry_forward`]), and a carried row is by
    /// construction matched — that is exactly why re-deriving the set from the
    /// matching alone drops it. Rewriting a matched row takes away the defining
    /// row of the column it was matched to, so "every row has a column" does not
    /// by itself say that column survived: in a view with more columns than rows
    /// every row can be matched while a column the pass orphaned sits free.
    ///
    /// So acceptance also requires that every column the *pre-prolongation*
    /// matching determined is still determined by the final one. That is the
    /// direct statement of what re-differentiation could break, it costs one
    /// bitmap comparison, and it is what [`row_admits_differentiation`]'s
    /// exemption rests on. Where the view is square it follows from the perfect
    /// matching already (a bijection covers every column); where it is not, it
    /// is the part that does the work.
    ///
    /// Every MSL model measured here has a square view — `Fourbar1` reports
    /// 2013 scalar rows over 2013 scalar columns and
    /// `Constraints.SphericalConstraint` 2231 over 2231 — so on those the check
    /// is implied and costs nothing but the comparison. It is not therefore
    /// decoration: squareness is a property of the model, not of this pass, and
    /// the exemption must not depend on one that is never asserted.
    fn is_accepted(&self, determined_before: &[bool]) -> bool {
        if self.rows == 0 {
            return false;
        }
        let ProlongationOutcome::Perfect { determined_columns } = &self.outcome else {
            return false;
        };
        if !retains_every_determined_column(determined_before, determined_columns) {
            structural_trace!(
                "[sim-trace] deficient-row prolongation rejected reason=column_lost_its_defining_row"
            );
            return false;
        }
        true
    }
}

/// Does `after` still determine every column `before` determined?
///
/// A length mismatch is not a comparison failure to shrug off: the column layout
/// is derived from the variable set, which this pass never changes, so differing
/// widths mean the two matchings are not about the same system and no conclusion
/// may be drawn from them.
fn retains_every_determined_column(before: &[bool], after: &[bool]) -> bool {
    before.len() == after.len()
        && before
            .iter()
            .zip(after)
            .all(|(before, after)| !*before || *after)
}

/// Differentiate, re-match, repeat — until the matching is perfect, no round
/// can differentiate anything, or the round cap is reached.
///
/// Every round that rewrote anything pushes its undo record onto `undo`, in
/// order, so the caller can roll the whole iteration back by applying them in
/// reverse.
fn run_prolongation(
    dae: &mut Dae,
    first: Nomination,
    undo: &mut Vec<RoundSnapshot>,
) -> Prolongation {
    let mut nomination = first;
    let mut rows = 0usize;
    let mut carried: Vec<usize> = Vec::new();
    for round in 0..MAX_ROUNDS {
        nomination.carry_forward(&carried);
        let snapshot = RoundSnapshot::take(dae, &nomination.shells);
        let rewritten = differentiate_shells(dae, &nomination.shells);
        let changed = rewritten.len();
        carried = rewritten;
        if changed == 0 {
            structural_trace!(
                "[sim-trace] deficient-row prolongation stalled round={round} rows={rows} deficiency={}",
                nomination.deficiency
            );
            return Prolongation {
                rows,
                outcome: ProlongationOutcome::Deficient(nomination.deficiency),
            };
        }
        undo.push(snapshot.only_rows(&carried));
        rows += changed;
        let next = match nominate_deficient_rows(dae) {
            RankOutcome::Deficient(next) => next,
            RankOutcome::Perfect { determined_columns } => {
                structural_trace!(
                    "[sim-trace] deficient-row prolongation converged rounds={} rows={rows}",
                    round + 1
                );
                return Prolongation {
                    rows,
                    outcome: ProlongationOutcome::Perfect { determined_columns },
                };
            }
            RankOutcome::Indeterminate => {
                structural_trace!(
                    "[sim-trace] deficient-row prolongation indeterminate rounds={} rows={rows}",
                    round + 1
                );
                return Prolongation {
                    rows,
                    outcome: ProlongationOutcome::Indeterminate,
                };
            }
        };
        structural_trace!(
            "[sim-trace] deficient-row prolongation round={round} rows={changed} deficiency={}",
            next.deficiency
        );
        nomination = next;
    }
    structural_trace!(
        "[sim-trace] deficient-row prolongation exhausted rounds={MAX_ROUNDS} rows={rows} deficiency={}",
        nomination.deficiency
    );
    Prolongation {
        rows,
        outcome: ProlongationOutcome::Deficient(nomination.deficiency),
    }
}

/// The rows a round may rewrite, kept so an iteration that buys no rank is
/// undone.
///
/// A differentiated constraint is a strictly larger residual than the one it
/// replaced — the orientation constraint of a multibody loop expands roughly
/// two hundredfold — and it moves a row into `initialization`. Paying that for
/// a matching that did not improve is a cost with no benefit, and it changes
/// the trajectory of a model that was already simulating. So the whole
/// iteration is provisional until its final re-match confirms it.
///
/// One record per round, applied newest-first, restores the DAE exactly:
/// rows are only ever replaced in place, never inserted or removed, so the
/// indices a record holds stay valid for the rounds that follow it, and
/// `initialization` only ever grows.
struct RoundSnapshot {
    rows: Vec<(usize, rumoca_ir_dae::Equation)>,
    initialization_len: usize,
}

impl RoundSnapshot {
    fn take(dae: &Dae, shells: &[Vec<usize>]) -> Self {
        let rows = shells
            .iter()
            .take(NOMINATED_SHELL_DEPTH)
            .flatten()
            .filter_map(|index| {
                dae.continuous
                    .equations
                    .get(*index)
                    .map(|equation| (*index, equation.clone()))
            })
            .collect();
        Self {
            rows,
            initialization_len: dae.initialization.equations.len(),
        }
    }

    fn apply(self, dae: &mut Dae) {
        for (index, equation) in self.rows {
            if let Some(slot) = dae.continuous.equations.get_mut(index) {
                *slot = equation;
            }
        }
        dae.initialization
            .equations
            .truncate(self.initialization_len);
    }

    fn only_rows(mut self, rewritten: &[usize]) -> Self {
        let rewritten = rewritten
            .iter()
            .copied()
            .collect::<std::collections::BTreeSet<_>>();
        self.rows.retain(|(row, _)| rewritten.contains(row));
        self
    }
}

/// The deficient block of the current matching, grouped into shells.
struct Nomination {
    /// Source equation indices, outermost index = alternating-path depth.
    shells: Vec<Vec<usize>>,
    /// Number of unmatched scalar rows: the rank the matching is missing.
    deficiency: usize,
    /// One flag per view column: was it matched, i.e. does some row define it?
    ///
    /// Kept so [`Prolongation::is_accepted`] can compare the columns determined
    /// before the iteration with the ones determined after it.
    determined_columns: Vec<bool>,
}

impl Nomination {
    /// Re-nominate the rows the previous round rewrote.
    ///
    /// Pantelides prolongs the deficient *set*, not whatever is left unmatched
    /// after each step. Re-deriving the set from the new matching every round is
    /// not the same thing here, because the matching is read off an
    /// over-approximate view ([`scalar_rank_view`]): a row can leave the view's
    /// unmatched set while the real system still has no column for it, and the
    /// iteration then stops one differentiation short.
    ///
    /// That is measurable on `Fourbar1`. Its position closure
    /// `b0.frame_b.r_0 = j2.frame_a.r_0` leaves the view's unmatched set after
    /// one differentiation, so the set re-derived from the matching drops it —
    /// but the real post-elimination system still reports those three rows
    /// unmatched, because a constraint force is determined at acceleration
    /// level, not velocity level.
    ///
    /// A carried row is therefore, in general, *matched*: leaving the unmatched
    /// set is what makes carrying it necessary. Two things downstream depend on
    /// that being said out loud rather than assumed away —
    /// [`row_admits_differentiation`]'s exemption for rows this pass produced,
    /// and [`ShellContext::der_map`]'s exclusion of nominated rows as
    /// definitions. Neither may claim that a nominated row defines no column.
    ///
    /// Carrying the previous round's rows forward is bounded by construction:
    /// the carried set is what the previous round rewrote, which is a subset of
    /// what it nominated, and the whole iteration is capped at [`MAX_ROUNDS`]
    /// rounds — so no row can be differentiated more than [`MAX_ROUNDS`] times.
    fn carry_forward(&mut self, carried: &[usize]) {
        if carried.is_empty() {
            return;
        }
        let shell = match self.shells.first_mut() {
            Some(shell) => shell,
            None => {
                self.shells.push(Vec::new());
                self.shells.first_mut().expect("a shell was just pushed")
            }
        };
        shell.extend_from_slice(carried);
        shell.sort_unstable();
        shell.dedup();
    }
}

/// What the rank witness says about the DAE as it stands.
///
/// # Why this is not an `Option`
///
/// Three unrelated things used to return `None` here: a view that could not be
/// built, a degenerate view, and a view whose matching is perfect. Only the last
/// is a result. Folding them together made "the rank witness declined" indis-
/// tinguishable from "the rank witness says we are done", so a round of
/// prolongation was retained — and traced as *converged* — on a model that had
/// exhibited no matching at all. A view that cannot be built is evidence in
/// neither direction and must revert.
enum RankOutcome {
    /// The matching leaves rows unmatched; here is the deficient block.
    Deficient(Nomination),
    /// Every row of the view is matched, and these are the columns that
    /// matching determines.
    Perfect { determined_columns: Vec<bool> },
    /// No view could be built, so the rank of the system is unknown here.
    Indeterminate,
}

/// Match the scalar-width view and report what it says.
fn nominate_deficient_rows(dae: &Dae) -> RankOutcome {
    let Some(view) = scalar_rank_view::build(dae) else {
        structural_trace!("[sim-trace] deficient-row view declined reason=indeterminate_row_shape");
        return RankOutcome::Indeterminate;
    };
    if view.n_eq == 0 || view.n_var == 0 {
        structural_trace!(
            "[sim-trace] deficient-row view declined reason=degenerate_view scalar_eqs={} scalar_vars={}",
            view.n_eq,
            view.n_var
        );
        return RankOutcome::Indeterminate;
    }
    let (match_eq, match_var) =
        crate::matching::maximum_matching(view.n_eq, view.n_var, &view.rows, &[]);
    let determined_columns: Vec<bool> = match_var.iter().map(Option::is_some).collect();
    let deficiency = match_eq.iter().filter(|matched| matched.is_none()).count();
    if deficiency == 0 {
        structural_trace!(
            "[sim-trace] deficient-row block scalar_eqs={} scalar_vars={} deficiency=0",
            view.n_eq,
            view.n_var
        );
        return RankOutcome::Perfect { determined_columns };
    }
    let shells = alternating_path_shells(&view, &match_eq, &match_var);
    structural_trace!(
        "[sim-trace] deficient-row block scalar_eqs={} scalar_vars={} deficiency={} shells={}",
        view.n_eq,
        view.n_var,
        deficiency,
        shells.len()
    );
    RankOutcome::Deficient(Nomination {
        shells,
        deficiency,
        determined_columns,
    })
}

/// Alternating-path reachability from the unmatched rows, grouped by depth.
///
/// Shell 0 is the unmatched rows. Shell `k + 1` holds the rows that own a
/// column first reached from shell `k`. Because the matching is maximum every
/// column reached this way is matched, so the traversal terminates and the
/// union of the shells is the Dulmage-Mendelsohn over-determined block.
///
/// Shells are reported as *source* equation indices: a wide row's scalar rows
/// are differentiated together or not at all, because the differentiation
/// rewrites the aggregate residual they all came from.
fn alternating_path_shells(
    view: &scalar_rank_view::ScalarRankView,
    match_eq: &[Option<usize>],
    match_var: &[Option<usize>],
) -> Vec<Vec<usize>> {
    let mut seen_equation = vec![false; view.n_eq];
    let mut seen_unknown = vec![false; view.n_var];
    let mut shells: Vec<Vec<usize>> = Vec::new();
    let mut frontier: Vec<usize> = match_eq
        .iter()
        .enumerate()
        .filter_map(|(equation, matched)| matched.is_none().then_some(equation))
        .collect();
    for &equation in &frontier {
        if let Some(flag) = seen_equation.get_mut(equation) {
            *flag = true;
        }
    }
    while !frontier.is_empty() {
        let mut next = Vec::new();
        for &equation in &frontier {
            collect_owner_rows(
                view,
                match_var,
                equation,
                &mut seen_unknown,
                &mut seen_equation,
                &mut next,
            );
        }
        shells.push(source_equations(view, &std::mem::take(&mut frontier)));
        frontier = next;
    }
    shells
}

/// Map a run of scalar rows to the source equations behind them, deduplicated.
fn source_equations(view: &scalar_rank_view::ScalarRankView, rows: &[usize]) -> Vec<usize> {
    let mut sources: Vec<usize> = rows
        .iter()
        .filter_map(|row| view.source_equation.get(*row).copied())
        .collect();
    sources.sort_unstable();
    sources.dedup();
    sources
}

/// Push the rows that own `equation`'s not-yet-seen columns onto `next`.
fn collect_owner_rows(
    view: &scalar_rank_view::ScalarRankView,
    match_var: &[Option<usize>],
    equation: usize,
    seen_unknown: &mut [bool],
    seen_equation: &mut [bool],
    next: &mut Vec<usize>,
) {
    for &unknown in view.rows.row(equation) {
        let Some(unknown_flag) = seen_unknown.get_mut(unknown) else {
            continue;
        };
        if *unknown_flag {
            continue;
        }
        *unknown_flag = true;
        let Some(Some(owner)) = match_var.get(unknown).copied() else {
            continue;
        };
        let Some(owner_flag) = seen_equation.get_mut(owner) else {
            continue;
        };
        if !*owner_flag {
            *owner_flag = true;
            next.push(owner);
        }
    }
}

/// Differentiate the first shell that admits any constraint row.
///
/// Returns the rows differentiated, empty when no shell admits one.
fn differentiate_shells(dae: &mut Dae, shells: &[Vec<usize>]) -> Vec<usize> {
    for (depth, shell) in shells.iter().take(NOMINATED_SHELL_DEPTH).enumerate() {
        let changed = differentiate_shell(dae, shell, depth);
        if !changed.is_empty() {
            return changed;
        }
    }
    Vec::new()
}

/// Whole-model tables a shell's differentiations all read.
///
/// Building these once per shell instead of once per row is what keeps the
/// pass affordable: `collect_residual_defining_expr_index` walks every
/// continuous row, and a deficient block can nominate a dozen of them.
struct ShellContext {
    state_names: Vec<VarName>,
    state_name_set: HashSet<String>,
    bindings: HashMap<String, f64>,
    /// One derivative closure covering every nominated row of the shell.
    ///
    /// The closure walk is a whole-model traversal, so building it per row cost
    /// more than the rest of the compile on `Fourbar1`. Seeding it with all the
    /// shell's residuals at once resolves the same names in one pass.
    ///
    /// The nominated rows are excluded from it as *definitions*, because letting
    /// the closure read back a row that is about to be rewritten is not a
    /// shortcut but a circularity: on `Fourbar1` the only definition the index
    /// offered for `j2.frame_a.r_0` was the loop-closure row itself, so
    /// `der(j2.frame_a.r_0)` came back as `der(b0.frame_b.r_0)` — world-fixed,
    /// hence zero — and the differentiated closure collapsed to `0 = 0`,
    /// deleting the constraint instead of prolonging it.
    ///
    /// Most nominated rows are unmatched and so define nothing, but a row
    /// carried over from the previous round ([`Nomination::carry_forward`]) can
    /// be matched, and excluding it does withhold a definition the closure could
    /// otherwise have used. That errs the safe way: the missing definition makes
    /// the closure block, the blocked name surfaces as an unresolved `der` leaf,
    /// and the candidate is declined. It can cost a prolongation; it cannot
    /// produce a wrong one.
    der_map: HashMap<String, Expression>,
}

impl ShellContext {
    fn build(dae: &Dae, candidates: &[usize]) -> Option<Self> {
        let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
        let state_name_set = state_names
            .iter()
            .map(|name| name.as_str().to_string())
            .collect();
        let seed_exprs: Vec<Expression> = candidates
            .iter()
            .filter_map(|index| dae.continuous.equations.get(*index))
            .map(|equation| equation.rhs.clone())
            .collect();
        // `der(x)` for states an earlier round demoted under the naming form.
        // Those states are no longer in `variables.states`, and the constraint
        // row that now defines them is not an explicit definition, so without
        // these values the closure blocks on the first algebraic that reads a
        // demoted state and every nomination is rejected as an unresolved leaf.
        let selected_derivatives =
            super::dummy_derivative_group::generated_dummy_derivative_values(dae);
        let der_map = build_relaxed_derivative_map_for_exprs_with_index(
            dae,
            &collect_residual_defining_expr_index(dae),
            &seed_exprs,
            RelaxedDerivativeMapOptions {
                canonical_state_derivative: None,
                rejected_state_derivative: None,
                excluded_equations: candidates,
                selected_derivatives: Some(&selected_derivatives),
            },
        )
        .map_err(|error| {
            structural_trace!(
                "[sim-trace] deficient-row shell declined reason=derivative_closure error={error}"
            );
        })
        .ok()?;
        Some(Self {
            state_names,
            state_name_set,
            bindings: crate::static_eval::structural_scalar_bindings(dae),
            der_map,
        })
    }
}

/// Differentiate every admissible constraint row of one shell.
fn differentiate_shell(dae: &mut Dae, shell: &[usize], depth: usize) -> Vec<usize> {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let matcher = DerivativeNameMatcher::from_var_names(&state_names);
    let candidates: Vec<usize> = shell
        .iter()
        .copied()
        .filter(|index| {
            dae.continuous
                .equations
                .get(*index)
                .is_some_and(|equation| row_admits_differentiation(equation, &matcher))
        })
        .collect();
    structural_trace!(
        "[sim-trace] deficient-row shell={depth} rows={} constraint_rows={}",
        shell.len(),
        candidates.len()
    );
    if candidates.is_empty() {
        return Vec::new();
    }
    let Some(context) = ShellContext::build(dae, &candidates) else {
        return Vec::new();
    };
    let mut changed = Vec::new();
    for index in candidates {
        let Some(new_rhs) = differentiated_row(dae, index, &context) else {
            continue;
        };
        structural_trace!(
            "[sim-trace] deficient-row differentiated row={index} shell={depth} origin='{}'",
            dae.continuous.equations[index].origin
        );
        commit_differentiated_row(dae, index, new_rhs);
        changed.push(index);
    }
    changed
}

/// May this nominated row be differentiated?
///
/// A row that already reads a state derivative is, in the general case, an ODE
/// row: differentiating it asks for a second-order derivative that no column
/// exists for, and destroys the value every other row reads from it.
///
/// The exception is a row *this pass* produced. The derivative of a position
/// constraint reads velocities, because that is what a velocity constraint is —
/// so the plain test locks the iteration at velocity level, exactly one
/// differentiation short of the acceleration level where a constraint force is
/// determined. That short fall is the whole reason an index-3 loop does not
/// reduce.
///
/// # What makes re-differentiating one of those rows safe
///
/// Not shell depth. Shell 0 is the unmatched rows, but a *carried* row
/// ([`Nomination::carry_forward`]) is added to shell 0 after the matching has
/// been read, and it is carried precisely because it left the unmatched set — so
/// a row reaching here through the exemption is, in general, matched, and it is
/// the defining row of the column `c` the view matched it to. Differentiating it
/// does take `c`'s definition away. The old claim that it could not was false.
///
/// Two things make the rewrite sound anyway.
///
/// First, what replaces the row is the *next* derivative of a constraint this
/// pass already differentiated, which is what index 3 asks for: the derivative
/// of a position closure is a velocity constraint, and its derivative is the
/// acceleration-level constraint that determines the constraint force. It is not
/// an ODE row being destroyed — [`commit_differentiated_row`] retains the form
/// it replaces in `initialization`, so both the position and the velocity form
/// of a twice-differentiated closure are still enforced at `t = 0`.
///
/// Second, `c` itself is checked rather than argued about. The whole iteration
/// is one transaction, and [`Prolongation::is_accepted`] retains it only if the
/// final matching still determines every column the pre-prolongation matching
/// determined. If the rewrite left `c` with no row able to define it, `c` is
/// undetermined in the final view and the entire sequence is reverted. The
/// exemption may therefore hand a matched row to the differentiator; it cannot
/// leave a column orphaned in a retained result.
fn row_admits_differentiation(
    equation: &rumoca_ir_dae::Equation,
    matcher: &DerivativeNameMatcher,
) -> bool {
    !eq_contains_any_state_der_with_matcher(&equation.rhs, matcher)
        || equation.origin.contains(DEFICIENT_ROW_ORIGIN)
}

/// The time derivative of row `index`, or `None` when it is inadmissible.
///
/// # Why a naming row is refused even when it is unmatched
///
/// [`super::constrained_dummy_derivative`] reduces under the *naming* form: it
/// adds a row and funds it with a new unknown, and the added row is that
/// unknown's only definition. This pass reduces by *replacement*, so
/// differentiating one of those rows leaves the unknown it named with no
/// defining row at all.
///
/// The tempting argument is that only shell 0 is nominated
/// ([`NOMINATED_SHELL_DEPTH`]) and shell 0 is the unmatched rows, so a row that
/// reaches here defines nothing whatever its origin says. That argument was
/// tested and it is wrong, because the matching it appeals to is the
/// *over-approximate* one ([`scalar_rank_view`]): letting `Fourbar1` prolong its
/// one unmatched naming row drove that view's deficiency from 3 to 0, and the
/// real post-elimination system then reported 568 equations over 571 unknowns —
/// exactly the width of the naming row, now under-determined rather than
/// over-determined. The view had matched the orphaned column through an edge the
/// real system does not have.
///
/// So the origin marker is not a stale fact here; it records funding this pass
/// cannot replace.
fn differentiated_row(dae: &Dae, index: usize, context: &ShellContext) -> Option<Expression> {
    let equation = dae.continuous.equations.get(index)?;
    if equation.origin.contains(CONSTRAINED_DUMMY_ROW_ORIGIN) {
        return reject(index, "defines_a_dummy_derivative");
    }
    if !expression_is_smooth_for_index_reduction(&equation.rhs, dae, &context.bindings) {
        return reject(index, "source_not_smooth");
    }
    let Some(differentiated) = symbolic_time_derivative(&equation.rhs, dae, &context.der_map)
    else {
        return reject(index, "not_differentiable");
    };
    let new_rhs = crate::eliminate::simplify_arithmetic_identities(differentiated);
    let unresolved = unresolved_der_leaves(&new_rhs, &context.state_name_set);
    if !unresolved.is_empty() {
        structural_trace!(
            "[sim-trace] deficient-row rejected row={index} reason=unresolved_der_leaf leaves={}",
            unresolved.join(",")
        );
        return None;
    }
    if !expression_is_smooth_for_index_reduction(&new_rhs, dae, &context.bindings) {
        return reject(index, "derivative_not_smooth");
    }
    let nodes = expression_node_count(&new_rhs);
    if nodes > MAX_DIFFERENTIATED_NODES {
        structural_trace!(
            "[sim-trace] deficient-row rejected row={index} reason=node_budget nodes={nodes}"
        );
        return None;
    }
    if row_is_vacuous(&new_rhs) {
        return reject(index, "derivative_is_constant");
    }
    if !row_gains_information(&equation.rhs, &new_rhs, &context.state_names) {
        return reject(index, "no_new_columns");
    }
    structural_trace!("[sim-trace] deficient-row accepted row={index} nodes={nodes}");
    Some(new_rhs)
}

/// Record why a nominated row was declined, and decline it.
fn reject(index: usize, reason: &str) -> Option<Expression> {
    structural_trace!("[sim-trace] deficient-row rejected row={index} reason={reason}");
    None
}

/// `der(...)` arguments the closure could not resolve to a retained state.
///
/// A differentiated row keeping one of these is not admissible: the residual
/// compiler lowers `der(x)` to zero outside a mass-matrix row, so the row would
/// silently evaluate to the wrong constraint. Naming the leaves is what makes a
/// declined nomination diagnosable — the difference between "index reduction
/// gave up" and "the closure is missing `der(b1.body.frame_a.R.T)`".
fn unresolved_der_leaves(expr: &Expression, state_name_set: &HashSet<String>) -> Vec<String> {
    let mut collector = UnresolvedDerLeaves {
        state_name_set,
        leaves: Vec::new(),
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
    collector.leaves.sort_unstable();
    collector.leaves.dedup();
    collector.leaves
}

struct UnresolvedDerLeaves<'a> {
    state_name_set: &'a HashSet<String>,
    leaves: Vec<String>,
}

impl rumoca_core::ExpressionVisitor for UnresolvedDerLeaves<'_> {
    fn visit_builtin_call(&mut self, function: &rumoca_core::BuiltinFunction, args: &[Expression]) {
        if *function != rumoca_core::BuiltinFunction::Der {
            for arg in args {
                self.visit_expression(arg);
            }
            return;
        }
        match args {
            [Expression::VarRef { name, .. }] if self.state_name_set.contains(name.as_str()) => {}
            [Expression::VarRef { name, .. }] => self.leaves.push(name.as_str().to_string()),
            _ => self.leaves.push("<non-variable der argument>".to_string()),
        }
    }
}

/// A residual over no variables at all — a constraint that was deleted, not
/// prolonged.
///
/// This pass *replaces* the row it differentiates, so a derivative that folds
/// away to a constant does not weaken the model, it removes an equation from it
/// while the row count says nothing changed. SPEC_0008 forbids exactly that kind
/// of silent recovery, and the failure it produces is the worst kind: a closed
/// kinematic loop whose closure has been deleted still integrates, and produces
/// a trajectory that looks plausible and drifts open.
///
/// It is a real shape, not a hypothetical. Before the derivative closure stopped
/// resolving a nominated row's variables through the nominated row itself
/// ([`ShellContext::der_map`]), `Fourbar1`'s loop closure differentiated to
/// exactly this and was accepted, because [`row_gains_information`] only asks
/// whether the column set *changed*.
fn row_is_vacuous(differentiated: &Expression) -> bool {
    let mut refs = Vec::new();
    differentiated.collect_var_refs(&mut refs);
    refs.is_empty()
}

/// Whether differentiating actually changed what the row constrains.
///
/// A row whose derivative reads exactly the same variables, and no state
/// derivative, occupies exactly the same columns: replacing it would trade a
/// constraint the initialization enforces for one that says the same thing to
/// the matcher. Any difference in the column set is admissible — whether the
/// difference *buys* rank is settled by re-matching, not guessed here.
fn row_gains_information(
    original: &Expression,
    differentiated: &Expression,
    state_names: &[VarName],
) -> bool {
    if !derivative_states_in_eq(differentiated, state_names).is_empty() {
        return true;
    }
    let mut original_refs = Vec::new();
    original.collect_var_refs(&mut original_refs);
    let original_refs: HashSet<VarName> = original_refs.into_iter().collect();
    let mut new_refs = Vec::new();
    differentiated.collect_var_refs(&mut new_refs);
    let new_refs: HashSet<VarName> = new_refs.into_iter().collect();
    new_refs != original_refs
}

/// Replace row `index` with `new_rhs`, retaining the original for initialization.
///
/// The differentiated constraint holds on the same manifold only if the
/// original held at `t = 0`, so the original row moves to the initialization
/// partition rather than being dropped — the same convention
/// [`super::state_row_reduction`] uses.
///
/// Because this runs once per round and the round before it left its own
/// original behind, a row the iteration differentiates *twice* retains both
/// forms: round one retains the position constraint, round two retains the
/// velocity constraint it produced. Both are needed. An acceleration-level
/// closure constrains only the second derivative of the loop error, so
/// initializing with the position form alone leaves the velocity error free and
/// the loop opens linearly in time; initializing with the velocity form alone
/// leaves a constant position error. Retaining both pins the constraint and its
/// first derivative at `t = 0`, which is what keeps a closed kinematic loop on
/// its manifold for the whole run.
fn commit_differentiated_row(dae: &mut Dae, index: usize, new_rhs: Expression) {
    let original = dae.continuous.equations[index].clone();
    dae.initialization.equations.push(original);
    let equation = &mut dae.continuous.equations[index];
    if equation.origin.is_empty() {
        equation.origin = DEFICIENT_ROW_ORIGIN.to_string();
    } else if !equation.origin.contains(DEFICIENT_ROW_ORIGIN) {
        equation.origin = format!("{}|{DEFICIENT_ROW_ORIGIN}", equation.origin);
    }
    equation.rhs = new_rhs;
}

/// Number of expression nodes, used as the size budget for a differentiated row.
fn expression_node_count(expr: &Expression) -> usize {
    let mut counter = NodeCounter { nodes: 0 };
    rumoca_core::ExpressionVisitor::visit_expression(&mut counter, expr);
    counter.nodes
}

struct NodeCounter {
    nodes: usize,
}

impl rumoca_core::ExpressionVisitor for NodeCounter {
    fn visit_expression(&mut self, expr: &Expression) {
        self.nodes += 1;
        self.walk_expression(expr);
    }
}

#[cfg(test)]
mod tests;
