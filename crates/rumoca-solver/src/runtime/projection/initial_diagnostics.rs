//! What a failed MLS 3.6 §8.6 initialization says about the row that failed.
//!
//! A residual index is meaningless outside the lowered IR, so a failure names
//! the coordinate a row was planned to determine whenever one exists. The only
//! row an executable product retains without an owner is the **stated-value
//! check**: two declarations state one coordinate's initial value, agreeing
//! exactly when their parameters do, so the restated equation is a consistency
//! check, and a failure of one means the declarations contradict each other.
//! Every other unowned reading (a coordinate outside the planned unknown
//! space, an overdetermined row) is refused during Solve construction and can
//! never reach this runtime.
//!
//! The lowering decides the roles (`InitializationSolveSystem::row_roles`);
//! this module only reports them, and never guesses the friendlier reading
//! when a role is absent.

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
/// can act on. When no block owns the row, the recorded role decides what is
/// reported.
fn initial_row_owner<M: AlgebraicProjectionModel>(model: &M, row: usize) -> String {
    if let Some(name) = initial_row_target_name(model, row) {
        return format!(" target={name}");
    }
    if let Some(slot) = model.initial_target(row) {
        return format!(" target={slot:?}");
    }
    match model.initial_row_role(row) {
        // A `Solved` role with no target is the lowering's own contract failure,
        // and a projection model that records no roles at all is a third-party
        // one. Neither may be reported as a check the model failed.
        Some(solve::InitializationRowRole::Solved) | None => {
            " owner=unknown(the lowered model records no role for this row)".to_string()
        }
        Some(solve::InitializationRowRole::StatedValueCheck) => {
            " owner=stated-value-check(two declarations state this coordinate's MLS 3.6 §8.6 \
             initial value, and at these parameter values they contradict each other)"
                .to_string()
        }
    }
}
