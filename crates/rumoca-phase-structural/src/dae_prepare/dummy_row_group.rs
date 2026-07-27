//! Coupled-group naming construction for the state-driven index reduction.
//!
//! [`super::dummy_derivative_group`] already carries a coupled-group form of
//! the Mattsson-Söderlind construction: it demotes a *group* of states at once
//! and pays for the differentiated constraint with one generated
//! `__dummyder__` unknown per demoted scalar. That form is keyed on an
//! aggregate algebraic whose components are the group's derivative values, so
//! it only reaches states that already share such an aggregate.
//!
//! This module extends the same construction to the shape
//! [`super::state_row_reduction`] meets: one state of scalar width `W`, and a
//! *group of rows* whose combined scalar width is also `W`. The exchange is the
//! same arithmetic read the other way round — a three-wide multibody residual
//! funds three scalar unknowns, three scalar rows fund one three-wide state —
//! and it is what lets the state-driven pass keep the rows it would otherwise
//! consume.
//!
//! # Why the state-driven pass needs it
//!
//! Without a naming form the state-driven pass reduces a state by *replacing*
//! the source row with its derivative and pushing the original into
//! `initialization`. When the source row is one
//! [`super::constrained_dummy_derivative`] wrote to define a generated
//! `__dummyder__` unknown, that unknown loses its only continuous defining row.
//! Rows and columns still balance — the moved row's width and the new
//! derivative column offset each other exactly — so a counting check cannot see
//! it, but the rank is gone and the matching reports the dummy as unmatched.
//!
//! The naming form removes the trade: the source rows stay where they are,
//! their derivatives are *appended*, and the state that is being reduced pays
//! for them by becoming an algebraic unknown whose derivative is named.
//!
//! # The exchange
//!
//! Demoting a state of scalar width `W`:
//!
//! * removes `W` derivative columns and adds `W` columns for the generated
//!   `__dummyder__` unknown that replaces them — no net change;
//! * adds `W` columns, because the demoted state's own value is an unknown the
//!   integrator no longer supplies;
//! * and the appended differentiated rows add `W` rows.
//!
//! So the group is admissible exactly when the rows' combined scalar width
//! equals the state's scalar width, which is what [`reduce_state_by_row_group`]
//! checks before it stages anything.
//!
//! That precondition reads the *source* rows' declared widths. What the model
//! actually gained is a separate question, and SPEC_0008 does not let a
//! demotion answer it by assumption: the staged DAE is measured against
//! [`verify_row_group_demotion_preserves_balance`] before the commit, and a
//! demotion that did not grow rows and variables by `W` each names the state
//! and stops. Note this is the model's own variable count (states plus
//! algebraics), not the residual-system count
//! [`super::constrained_dummy_derivative`] uses — this form is entered for a
//! state that has *no* `der(state)` row, so there is no derivative column to
//! exchange and the residual count legitimately moves by `2W`.

use super::constrained_dummy_derivative::{
    CONSTRAINED_DUMMY_ROW_ORIGIN, rewrite_state_derivative_as_dummy_everywhere, scalar_row_total,
};
use super::dummy_derivative_group::{
    declare_dummy_derivative, dummy_derivative_name, variable_name_is_taken,
};
use super::{Dae, Equation, Expression, StructuralError, VarName, expr_contains_der_of};

/// Origin stamped on a row appended by this construction.
pub(super) const DUMMY_ROW_GROUP_ORIGIN: &str = "index_reduction:d_dt_dummy_derivative_row_group";

/// True when this row was written to define a generated dummy derivative.
///
/// Such a row is the *only* continuous definition of its unknown, so a pass
/// that consumes it leaves the unknown undetermined. The equation-driven pass
/// declines these rows for the same reason
/// (`reason=defines_a_dummy_derivative`).
pub(super) fn row_defines_a_dummy_derivative(equation: &Equation) -> bool {
    equation.origin.contains(CONSTRAINED_DUMMY_ROW_ORIGIN)
}

/// True when index reduction may demote `state_name`.
///
/// `StateSelect.always` and `StateSelect.prefer` are the modeller's statement
/// that this coordinate is the one to integrate (MLS 4.4.2.2), so a reduction
/// that would take it away is not admissible. A state whose derivative is read
/// outside the continuous residual rows is equally out of reach: the discrete
/// and event partitions are not part of the system the appended rows balance.
pub(super) fn state_is_demotable(dae: &Dae, state_name: &VarName) -> bool {
    let Some(state) = dae.variables.states.get(state_name) else {
        return false;
    };
    if matches!(
        state.state_select,
        rumoca_core::StateSelect::Always | rumoca_core::StateSelect::Prefer
    ) {
        return false;
    }
    !super::dummy_derivative_group::state_derivative_escapes_continuous_rows(dae, state_name)
}

/// Origin an appended derivative carries, given the origin of its source row.
fn appended_origin(source_origin: &str) -> String {
    if source_origin.is_empty() {
        DUMMY_ROW_GROUP_ORIGIN.to_string()
    } else {
        format!("{source_origin}|{DUMMY_ROW_GROUP_ORIGIN}")
    }
}

/// Combined scalar width of a candidate row group, or `None` when a row index
/// no longer exists.
pub(super) fn group_scalar_width(dae: &Dae, rows: &[(usize, Expression)]) -> Option<usize> {
    rows.iter()
        .map(|(index, _)| Some(dae.continuous.equations.get(*index)?.scalar_count))
        .sum()
}

/// Commit the naming form for `state_name`, funded by `rows`.
///
/// `rows` pairs each source row index with the time derivative of that row,
/// already vetted by the caller. The source rows are left in place; their
/// derivatives are appended; every `der(state_name)` becomes a reference to the
/// generated `__dummyder__` unknown; and `state_name` is demoted to an
/// algebraic.
///
/// Returns `false` — leaving `dae` untouched — when the construction cannot be
/// completed: the group does not fund the state's width, the generated name is
/// taken, the state is no longer demotable, or a `der(state_name)` the rewriter
/// cannot express as a reference to the dummy survives. Every one of those
/// declines is recorded by name, so an operator diagnosing a state that stayed a
/// state can see which precondition turned the reduction away.
///
/// Returns `Err` when the staged demotion did not preserve the model's balance.
/// That is not a decline: the preconditions all passed and the exchange still
/// came out wrong, which SPEC_0008 forbids recovering from silently.
pub(super) fn reduce_state_by_row_group(
    dae: &mut Dae,
    state_name: &VarName,
    rows: &[(usize, Expression)],
) -> Result<bool, StructuralError> {
    if rows.is_empty() {
        return Ok(decline(state_name, "empty_group"));
    }
    if !state_is_demotable(dae, state_name) {
        return Ok(decline(state_name, "state_not_demotable"));
    }
    let Some(state) = dae.variables.states.get(state_name) else {
        return Ok(decline(state_name, "state_missing"));
    };
    let (size, span) = (state.size(), state.source_span);
    if size == 0 {
        return Ok(decline(state_name, "state_has_zero_scalar_width"));
    }
    if group_scalar_width(dae, rows) != Some(size) {
        return Ok(decline(state_name, "group_width_does_not_fund_state"));
    }
    let dummy_name = dummy_derivative_name(state_name);
    if variable_name_is_taken(dae, &dummy_name) {
        return Ok(decline(state_name, "generated_dummy_name_is_taken"));
    }
    let mut staged = super::copy_accounting::clone_dae(dae);
    append_differentiated_rows(&mut staged, rows);
    declare_dummy_derivative(&mut staged, state_name, &dummy_name)?;
    rewrite_state_derivative_as_dummy_everywhere(&mut staged, state_name, &dummy_name);
    if staged
        .continuous
        .equations
        .iter()
        .any(|equation| expr_contains_der_of(&equation.rhs, state_name))
    {
        return Ok(decline(state_name, "der_of_state_survived_rewrite"));
    }
    let Some(variable) = staged.variables.states.shift_remove(state_name) else {
        return Ok(decline(state_name, "state_vanished_while_staging"));
    };
    staged
        .variables
        .algebraics
        .insert(state_name.clone(), variable);
    // SPEC_0008: the static width precondition above reads the *source* rows'
    // declared widths; this reads the staged DAE's actual row and variable
    // growth.
    verify_row_group_demotion_preserves_balance(dae, &staged, state_name, size, span)?;
    if !super::demotion_rank_check::row_group_is_rank_justified(dae, &staged, state_name, size) {
        return Ok(false);
    }
    crate::structural_trace!(
        "[sim-trace] dummy row group reduced state={} width={size} rows={:?}",
        state_name.as_str(),
        rows.iter().map(|(index, _)| *index).collect::<Vec<_>>()
    );
    *dae = staged;
    Ok(true)
}

/// Record why the naming form was not committed, and report "not committed".
///
/// Every early return in [`reduce_state_by_row_group`] names its precondition,
/// so an operator diagnosing a state that stayed a state can see that the
/// reduction was attempted and which check turned it away.
fn decline(state_name: &VarName, reason: &str) -> bool {
    crate::structural_trace!(
        "[sim-trace] dummy row group declined state={} reason={reason}",
        state_name.as_str()
    );
    false
}

/// Scalar variables the model declares: every state plus every algebraic.
///
/// This is the count MLS 4.7 balances against the equation count, and it is the
/// one this construction has to preserve. The residual-system count used by
/// [`super::constrained_dummy_derivative`] — algebraics plus `der()` columns —
/// cannot be used here: this form is entered precisely for a state that has no
/// `der(state)` row, so `der(state)` occupies no column to exchange, and the
/// residual count legitimately rises by `2W` while the model count rises by `W`.
fn scalar_variable_total(dae: &Dae) -> usize {
    let states: usize = dae
        .variables
        .states
        .values()
        .map(rumoca_ir_dae::Variable::size)
        .sum();
    let algebraics: usize = dae
        .variables
        .algebraics
        .values()
        .map(rumoca_ir_dae::Variable::size)
        .sum();
    states + algebraics
}

/// Hard-fail when the naming form did not preserve the model's balance.
///
/// The exchange has a fixed arithmetic: the demoted state leaves the state
/// partition and re-enters as an algebraic, its derivative re-enters as the
/// generated `__dummyder__` algebraic, and the differentiated rows supply
/// exactly one new row per demoted scalar. So rows and declared variables both
/// grow by the state's scalar size and by nothing else.
///
/// SPEC_0008 forbids recovering silently from a violation of that arithmetic. A
/// demotion that adds a variable without adding a row leaves a system that can
/// still admit a perfect matching elsewhere and then integrates a different
/// model than the one that was written, so the pass names the state and stops.
fn verify_row_group_demotion_preserves_balance(
    before: &Dae,
    after: &Dae,
    state_name: &VarName,
    size: usize,
    span: rumoca_core::Span,
) -> Result<(), StructuralError> {
    let row_growth = scalar_row_total(after).wrapping_sub(scalar_row_total(before));
    let variable_growth = scalar_variable_total(after).wrapping_sub(scalar_variable_total(before));
    if row_growth == size && variable_growth == size {
        return Ok(());
    }
    Err(StructuralError::ContractViolation {
        reason: format!(
            "index reduction unbalanced the system while naming the derivative of state `{}`: \
             demoting {size} scalar state(s) added {row_growth} scalar row(s) and \
             {variable_growth} scalar variable(s), but the row-group naming form must add \
             exactly one row and one variable per demoted scalar state",
            state_name.as_str()
        ),
        span,
    })
}

/// Append the differentiated rows, each keeping its source row's shape.
fn append_differentiated_rows(dae: &mut Dae, rows: &[(usize, Expression)]) {
    let appended = rows
        .iter()
        .filter_map(|(index, differentiated)| {
            let source = dae.continuous.equations.get(*index)?;
            Some(Equation {
                lhs: None,
                rhs: differentiated.clone(),
                span: source.span,
                origin: appended_origin(&source.origin),
                scalar_count: source.scalar_count,
            })
        })
        .collect::<Vec<_>>();
    dae.continuous.equations.extend(appended);
}
