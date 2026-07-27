//! Rank witness for the state-driven index-reduction passes.
//!
//! [`super::constrained_dummy_derivative`] already checks its own arithmetic
//! with a *counting* invariant: demoting `W` scalar states must add `W` scalar
//! rows and `W` scalar unknowns. That invariant is real and it is kept, but its
//! own documentation is explicit that it counts rows and columns and nothing
//! else — and the failure this module exists to observe keeps both counts
//! exactly balanced.
//!
//! The shape is: a pass consumes the row that defines a generated
//! `__dummyder__` unknown, replacing it with that row's time derivative. The
//! moved row's width and the derivative column it introduces cancel, so the
//! counts are undisturbed; but the dummy has lost its only continuous defining
//! row and the system's rank is one short per scalar. Only a matching sees it.
//!
//! So this module measures the *matching deficiency* of the continuous
//! partition before and after a pass. The measurement runs on
//! [`super::scalar_rank_view`], the same scalar-width rank witness the
//! equation-driven pass nominates from, because the aggregate incidence counts
//! one row per equation and one column per scalar unknown and so is not a rank
//! witness at all before scalarization.
//!
//! # Why this observes rather than enforces
//!
//! The view's deficiency is a *lower bound* on the real one: it hands rows
//! extra columns, never extra rows, so it can only over-match. A lower bound
//! rising does not prove the real deficiency rose — both readings are bounds,
//! and the slack between them can move on its own when a pass rewrites the
//! rows the bound is computed from.
//!
//! That is not a theoretical caveat. Enforcing `after <= before` as a contract
//! was measured to fail
//! `rumoca_sim::solve_lowering::tests::simulation_metadata_reports_constrained_state_as_unselected`
//! — a model that lowers, selects states, and reports metadata correctly — on a
//! bound moving from 1 to 2. Turning a bound into a contract stops a working
//! compile, so the transition is recorded and left for the matcher, which does
//! see the real system, to reject or accept.
//!
//! Making this an enforceable check needs a scalar view that is *exact* rather
//! than over-approximate, which is a change to where index reduction may run
//! rather than to this module.

use super::scalar_rank_view;
use super::{Dae, VarName};

/// Matching deficiency of the scalar-width rank witness, when one exists.
///
/// `None` means one of two things, and the caller treats both the same way — as
/// "no reading", never as "zero":
///
/// * the model does not admit the witness at all (an empty system, or a row
///   whose scalar width is not determinable), or
/// * nothing is listening, so the witness was not computed.
///
/// The second case is the reason this gate exists rather than being pushed into
/// the trace macro. The reading is produced by a full Kuhn maximum matching over
/// the whole continuous partition, and every pass takes two of them; in a build
/// where [`crate::structural_trace!`] expands to nothing, all of that work is
/// computed and discarded. This module observes — it never decides — so paying
/// for a reading no one can read is pure waste, and skipping it is
/// behaviourally identical (measured: byte-identical `DoublePendulum` output,
/// `Fourbar1` unchanged at 559/565/565).
///
/// The witness itself is worth keeping: under `--trace=structural` it shows
/// `DoublePendulum` 20 -> 3 -> 0 and `Fourbar1` 39 -> 12 -> 6, which is the
/// clearest evidence available that a demotion removed a rank defect rather
/// than merely rebalancing the counts.
pub(super) fn scalar_rank_deficiency(dae: &Dae) -> Option<usize> {
    if !crate::structural_trace_enabled() {
        return None;
    }
    scalar_rank_view::deficiency(dae)
}

/// Record what a pass did to the scalar-rank bound.
pub(super) fn trace_pass_rank_transition(pass: &str, before: Option<usize>, after: Option<usize>) {
    crate::structural_trace!(
        "[sim-trace] rank witness pass={pass} deficiency {:?} -> {:?}",
        before,
        after
    );
}

/// The rank reading one round of constrained-dummy demotion is judged against.
///
/// Unlike [`scalar_rank_deficiency`] the deficiency here is computed whether or
/// not anything is listening, because [`demotion_is_rank_justified`] decides on
/// it. It is taken once per round: the round's candidates are all evaluated
/// against the same DAE, and the first one that commits ends the round.
///
/// The reading is skipped altogether on a model the gate could not act on
/// anyway — one carrying a row wider than a single scalar — which is also where
/// it would cost the most, since it is a full Kuhn matching over every scalar
/// row of the model.
pub(super) struct RoundRank {
    deficiency: Option<usize>,
    columnless_rows: usize,
}

pub(super) fn round_rank(dae: &Dae) -> RoundRank {
    RoundRank {
        deficiency: scalar_rank_view::every_row_is_one_scalar_wide(dae)
            .then(|| scalar_rank_view::deficiency_over_distinct_rows(dae))
            .flatten(),
        columnless_rows: scalar_rank_view::columnless_row_count(dae),
    }
}

/// True when the rank witness allows a row-group naming reduction to commit.
///
/// The row-group form of [`super::dummy_row_group`] is a demotion: it appends
/// the differentiated source rows, names `der(state)` as a generated dummy and
/// moves the state into the algebraic partition.
///
/// The direction acted on is weaker than [`demotion_is_rank_justified`]'s,
/// because this form exists precisely to *avoid* the rank loss consumption
/// causes and refusing its flat commits would put the pass back on the consuming
/// path. Only a rise is refused: the appended row could not be matched, so the
/// group funded the state with a row the system had no room for. Measured on
/// `Rotational.Examples.CoupledClutches`, where naming `J4.w` from the row that
/// defines `__dummyder__.J3.phi` takes the witness `0 -> 1` and strands
/// `clutch3.flange_a.phi`.
///
/// `state_size` scopes it. The group's combined width equals the state's, so a
/// scalar state is funded by exactly one one-scalar-wide row: the appended row
/// stands for itself, and the demoted state's own columns are a single scalar
/// pair. A wider state is funded by a family, the view hands that family the
/// union of its columns, and a change in the aggregate says nothing about any
/// single row — the same restriction [`demotion_is_rank_justified`] documents,
/// applied to what this construction actually rewrites. Vector states are
/// therefore reduced exactly as before this gate existed.
pub(super) fn row_group_is_rank_justified(
    before: &Dae,
    staged: &Dae,
    state_name: &VarName,
    state_size: usize,
) -> bool {
    if state_size != 1 {
        return true;
    }
    let Some(before_deficiency) = scalar_rank_view::deficiency_over_distinct_rows(before) else {
        return true;
    };
    let Some(after) = scalar_rank_view::deficiency_over_distinct_rows(staged) else {
        return true;
    };
    if after <= before_deficiency {
        return true;
    }
    crate::structural_trace!(
        "[sim-trace] dummy row group declined state={} reason=rank_regressed deficiency {} -> {}",
        state_name.as_str(),
        before_deficiency,
        after
    );
    false
}

/// The reading [`consumption_is_rank_justified`] judges a candidate against.
///
/// Unlike [`round_rank`] this takes no whole-model scalar-width restriction: a
/// consumption rewrites one row and leaves the partitions and every other row
/// alone, so the scoping belongs on the row being rewritten and is applied
/// there. `None` means the model admits no witness at all, and the caller treats
/// that as "no reading".
pub(super) fn row_rank_reading(dae: &Dae) -> Option<usize> {
    scalar_rank_view::deficiency_over_distinct_rows(dae)
}

/// True when the rank witness allows this demotion to be committed — the
/// condition the constrained-dummy pass calls `rank_not_improved` when it fails.
///
/// # What the gate is protecting
///
/// A holonomic constraint costs the system exactly one degree of freedom, and
/// the Mattsson-Söderlind construction spends exactly one demotion recovering
/// it. So the number of legal demotions is fixed by the model: it is the rank
/// deficiency of the continuous partition, and each demotion must retire some of
/// it.
///
/// Nothing else in the pass measures that. The balance invariant in
/// [`super::constrained_dummy_derivative`] counts rows against columns, and the
/// construction keeps those equal by definition — it adds one row and one
/// unknown per demoted scalar whether or not a constraint was there to be
/// retired. A state whose only constraint an earlier demotion already consumed
/// therefore still passes every count, and demoting it silently removes a degree
/// of freedom the model has. Measured, with the gate switched off:
/// `CauerLowPassAnalog` has two capacitor-loop constraints among five capacitor
/// voltages and demotes four of them, integrating a third-order system in place
/// of the fifth-order one that was written; `PID_Controller` retires one defect
/// and then demotes two more states that retire nothing; `CauerLowPassSC`
/// demotes seven states against four constraints.
///
/// # Which readings may be acted on
///
/// A reading that **rises** is conclusive on any view: the demotion adds one row
/// and one column per demoted scalar and rewrites nothing else, so no amount of
/// approximation elsewhere can manufacture a rise. The appended row genuinely
/// has nothing left to match, and the demotion is refused.
///
/// A reading that **stays flat** says the demotion retired nothing the view can
/// see, and whether that is the same as retiring nothing at all depends on how
/// coarse the view is. [`scalar_rank_view`] expands an equation into the number
/// of scalar rows the flattener recorded on it and gives all of them one shared
/// column set, so on a model carrying vector equations its matching is an
/// aggregate over whole families and a flat reading carries no information.
/// [`round_rank`] therefore takes no reading at all on such a model, and every
/// demotion there is left to the matcher exactly as before this gate existed.
///
/// That split is measured on both sides. Rejecting flat demotions where rows are
/// wide regresses `DoublePendulum` (1272/1272 matching, three flat demotions) and
/// `DoublePendulumInitTip` (two flat demotions, and refusing them leaves a
/// `der()` reference the later partition cleanup cannot resolve). Accepting them
/// where rows are scalar is what loses the degrees of freedom listed above.
///
/// # The other way a demotion pays for itself
///
/// A constraint written between two states — `accelerate.s = mass.s - L/2` —
/// reads no unknown at all, so it is unmatchable whatever the rest of the model
/// does. Demoting either state gives it a column and retires that defect
/// outright, but the same demotion appends the differentiated constraint, and in
/// a chain of position/velocity constraints the appended row lands on a column
/// another row already wanted; the matching then comes out level even though a
/// real defect was removed. So a fall in
/// [`scalar_rank_view::columnless_row_count`] is accepted on its own terms,
/// before the matching is consulted at all.
///
/// # Why only a demotion that promotes nothing is judged
///
/// A plan that also promotes a state is a StateSelect *exchange*, not a plain
/// demotion, and the view cannot read one. Promoting `a` turns its value column
/// into a `der(a)` column, so a row that related `a` to another state — the very
/// row the exchange exists to differentiate — is left reading two states and no
/// column at all, and the reading rises for a reason that has nothing to do with
/// whether the exchange was right. Exchanges keep the eligibility test in
/// [`super::constrained_dummy_derivative::preferred_derivative_state_exchange`]
/// and are passed through here. Measured: every demotion this gate needs to
/// refuse — `CauerLowPassAnalog`'s `C2.v`/`C4.v`/`C5.v`, `Translational.Brake`'s
/// `brake.s`/`brake1.s`, `PID_Controller`'s `inertia1.phi`/`inertia2.phi` —
/// promotes nothing.
///
/// A missing reading means the model does not admit the witness at all (see
/// [`scalar_rank_view::build`]); an unmeasured demotion is accepted and left to
/// the matcher, exactly as before this gate existed.
pub(super) fn demotion_is_rank_justified(
    round: &RoundRank,
    staged: &Dae,
    state_name: &VarName,
    promotes_a_state: bool,
) -> bool {
    if promotes_a_state {
        return true;
    }
    if scalar_rank_view::columnless_row_count(staged) < round.columnless_rows {
        return true;
    }
    let Some(before) = round.deficiency else {
        return true;
    };
    let Some(after) = scalar_rank_view::deficiency_over_distinct_rows(staged) else {
        return true;
    };
    if after < before {
        return true;
    }
    crate::structural_trace!(
        "[sim-trace] constrained-dummy exchange rejected state={} reason=rank_not_improved deficiency {} -> {}",
        state_name.as_str(),
        before,
        after
    );
    false
}

/// True when the rank witness allows a row *consumption* to be committed.
///
/// # What this gate is protecting
///
/// Consumption is the state-driven pass's fallback: a state with no derivative
/// row keeps its place and one derivative-free row is replaced by its own time
/// derivative, so `der(state)` gains a row. Unlike a demotion it adds neither a
/// row nor a column, so the counting invariants say nothing about it at all —
/// and it is destructive in a way a demotion is not, because the row it rewrites
/// stops determining whatever it used to determine. The original is retained in
/// the initialization partition, which keeps the solution manifold right, but it
/// is gone from the continuous partition the matcher sees.
///
/// A consumption that pays for itself takes a row that no matching could use —
/// a constraint written purely between states — and turns it into a row that
/// determines `der(state)`; the deficiency falls. A consumption that does not
/// takes a row that was determining an algebraic and hands its column to nobody:
/// one orphan is traded for another and the deficiency rises. Measured on
/// `Modelica.Mechanics.Translational.Examples.Damper`, where the constrained-dummy
/// pass has already retired every defect it can (witness `3 -> 0`), the state-row
/// pass then consumes `mass1.flange_b.s = mass1.s + L/2` and its two siblings for
/// `damper1.s_rel`/`damper2.s_rel`/`springDamper3.s_rel`, which is `0 -> 3`: the
/// three demoted `mass*.s` are left as columns with no row, and the whole model
/// stops lowering.
///
/// The row is not caught by the pass's own algebraic-definition guard because
/// the algebraic it determines is offset — `mass1.s + L/2` is not an exact
/// unsliced reference — and the guard's other subject, `mass1.flange_b.s`, does
/// have an independent alias row. Widening that guard syntactically would be
/// guessing at which offsets count; what makes the consumption wrong is that a
/// column loses its last row, and only a matching sees that.
///
/// # Which readings may be acted on
///
/// `before` is the reading of the DAE the whole scan is judged against, taken
/// once; `staged` is that DAE with the candidate row already replaced by its
/// derivative. A *rise* is refused and everything else is accepted. A flat
/// reading is not evidence of waste here the way it is for a demotion — the pass
/// has its own termination measure and a consumption that changes no bound may
/// still be the step that lets a later round proceed — so only the direction
/// that demonstrably strands a column is acted on.
///
/// The scoping is narrower than
/// [`demotion_is_rank_justified`]'s and for a different reason. A demotion moves
/// a variable between partitions and appends rows, so its reading is only
/// meaningful where *every* row is one scalar wide; a consumption rewrites one
/// row and touches nothing else, so the before and after views differ in exactly
/// that row's column set. What it does need is for *that* row to be one scalar
/// wide, because a wide row's columns are the union over its whole family and a
/// change to the union says nothing about any single scalar row. Wide rows are
/// therefore consumed exactly as before this gate existed — which is what keeps
/// `MultiBody`'s vector constraints, the rows `DoublePendulum` and `Fourbar1`
/// reduce through, out of its reach.
///
/// A missing reading (a model that admits no witness at all) accepts the
/// consumption and leaves it to the matcher, again exactly as before.
pub(super) fn consumption_is_rank_justified(
    before: Option<usize>,
    staged: &Dae,
    state_name: &VarName,
    row: usize,
) -> bool {
    let Some(before) = before else {
        return true;
    };
    let Some(equation) = staged.continuous.equations.get(row) else {
        return true;
    };
    if !scalar_rank_view::row_is_one_scalar_wide(staged, equation) {
        return true;
    }
    let Some(after) = scalar_rank_view::deficiency_over_distinct_rows(staged) else {
        return true;
    };
    if after <= before {
        return true;
    }
    crate::structural_trace!(
        "[sim-trace] state-row reduction declined row={row} state={} \
         reason=rank_regressed deficiency {} -> {}",
        state_name.as_str(),
        before,
        after
    );
    false
}
