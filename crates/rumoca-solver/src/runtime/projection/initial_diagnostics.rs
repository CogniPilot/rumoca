//! What a failed MLS 3.6 §8.6 initialization says about the row that failed.
//!
//! A residual index is meaningless outside the lowered IR, and "no projection
//! block solves this row" is true of two situations a model author has to tell
//! apart:
//!
//! * a **surplus check** — §8.6 lets a coordinate be determined by a declaration
//!   and still be read by another initialization equation, so the extra equation
//!   is a consistency check. A failure of one means two declarations contradict
//!   each other, and the fix is in the declarations.
//! * a row over a coordinate the projection **never owned** — nothing solved that
//!   coordinate, so the residual is not a check that failed but a check that was
//!   never about the coordinate's value. For an algebraic it was read from a
//!   seeded `start`, because the algebraic refresh runs after this solve.
//!
//! The lowering decides which (`InitializationSolveSystem::row_roles`); this
//! module only reports it, and never guesses the friendlier reading when the role
//! is absent.

use super::{AlgebraicProjectionModel, RuntimeSolveError, residual_norm, solve};
use super::{initial_row_target_name, residual_sort_key};

pub(super) fn initial_projection_error<M: AlgebraicProjectionModel>(
    model: &M,
    message: &str,
    selected_rows: &[usize],
    residual: &[f64],
) -> RuntimeSolveError {
    let worst = residual
        .iter()
        .copied()
        .enumerate()
        .max_by(|(_, lhs), (_, rhs)| residual_sort_key(*lhs).total_cmp(&residual_sort_key(*rhs)));
    match worst {
        Some((row, value)) => {
            let original_row = selected_rows.get(row).copied().unwrap_or(row);
            RuntimeSolveError::solve_ir(format!(
                "{message}: max selected residual row={row} original_row={original_row}{} value={value:.6e} norm={:.6e}",
                initial_row_owner(model, original_row),
                residual_norm(residual)
            ))
        }
        None => RuntimeSolveError::solve_ir(message),
    }
}

/// How the initialization system answers one residual row.
///
/// Naming the row's target turns "row 51 is NaN" into the variable a model author
/// can act on. When no block owns the row, the recorded role decides which of the
/// two readings above is reported.
fn initial_row_owner<M: AlgebraicProjectionModel>(model: &M, row: usize) -> String {
    if let Some(name) = initial_row_target_name(model, row) {
        return format!(" target={name}");
    }
    if let Some(slot) = model.initial_target(row) {
        return format!(" target={slot:?}");
    }
    match model.initial_row_role(row) {
        Some(solve::InitializationRowRole::UnownedCoordinate(kind)) => format!(
            " owner=none(row reads a coordinate outside the planned initialization unknown \
             space: {})",
            unowned_coordinate_kind(kind)
        ),
        // A `Solved` role with no target is the lowering's own contract failure,
        // and a projection model that records no roles at all is a third-party
        // one. Neither may be reported as a check the model failed.
        Some(solve::InitializationRowRole::Solved) | None => {
            " owner=unknown(the lowered model records no role for this row)".to_string()
        }
        Some(solve::InitializationRowRole::SurplusCheck) => {
            " owner=surplus-check(every coordinate this row reads is determined elsewhere, so \
             the row is an MLS 3.6 §8.6 consistency check the model does not satisfy)"
                .to_string()
        }
    }
}

const fn unowned_coordinate_kind(kind: solve::InitializationCoordinateKind) -> &'static str {
    match kind {
        solve::InitializationCoordinateKind::Algebraic => {
            "a continuous algebraic/output, whose value the initialization residual reads from \
             its seeded `start` because the algebraic refresh runs after this solve"
        }
        solve::InitializationCoordinateKind::Discrete => "a discrete-time coordinate or its `pre`",
        solve::InitializationCoordinateKind::Unreadable => {
            "a coordinate the lowering cannot read per scalar (array element, multi-scalar row, \
             or structured family point)"
        }
        solve::InitializationCoordinateKind::Unmatched => {
            "an unknown the planner could not give a row of its own, so it kept its `start` guess"
        }
        solve::InitializationCoordinateKind::Other => {
            "an input, delay, previous, relation-memory, or terminal coordinate"
        }
    }
}
