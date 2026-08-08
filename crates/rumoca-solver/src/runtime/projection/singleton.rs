//! Acceptance rule for a singleton initialization projection block.
//!
//! A singleton block is one row solved for one unknown, so the row can be
//! *isolated* rather than Newton-solved. Whether the isolated value is written
//! back decides whether a variable keeps its `0.0` seed, which is the
//! difference between a converged initialization and a `NaN` cascade.

use super::{AlgebraicProjectionModel, y_index_for_slot};

/// One candidate singleton assignment, measured before and after the write.
pub(super) struct SingletonAssignmentStep {
    /// Row residual at the seed.
    pub before: f64,
    /// Row residual once the isolated value is written.
    pub after: f64,
    /// How far the write moves the unknown (`previous - value`).
    pub step: f64,
    /// Convergence tolerance for the residual, scaled to the row.
    pub row_tol: f64,
    /// Convergence tolerance for the unknown, scaled to the variable.
    pub variable_tol: f64,
}

/// Accept an isolated singleton assignment when it strictly reduces the row
/// residual, or when it *settles* a row that was already within tolerance
/// without moving the unknown beyond its own tolerance.
///
/// The second case is the one a plain "must strictly improve" rule gets wrong.
/// `Modelica.Magnetic.FluxTubes.Basic.ElectroMagneticConverter` declares
/// `Real eps = 100*Modelica.Constants.eps` and then divides by it
/// (`L_stat = ... abs(Psi/eps)`). The residual of `eps`'s own assignment row at
/// the `0.0` seed is `2.2e-14`, far below any usable tolerance, so the write was
/// rejected as "no improvement", `eps` stayed `0.0`, and the dependent quotient
/// became `0/0` — poisoning the whole initialization with `NaN`.
///
/// The move guard is what keeps that from re-seeding unrelated variables: on a
/// nearly-singular row an already-converged residual can isolate to a value far
/// from the current one, and writing it would silently replace a legitimate
/// initial condition. Such a jump has to be earned by an actual residual
/// reduction.
pub(super) fn singleton_assignment_improves(step: SingletonAssignmentStep) -> bool {
    if !step.after.is_finite() {
        return false;
    }
    if step.after.abs() + step.row_tol < step.before.abs() {
        return true;
    }
    step.after.abs() <= step.row_tol
        && step.after.abs() < step.before.abs()
        && step.step.abs() <= step.variable_tol
}

/// Name of the variable an initialization residual row solves for, when the
/// lowered model carries one.
pub(super) fn initial_row_target_name<M: AlgebraicProjectionModel>(
    model: &M,
    row: usize,
) -> Option<&str> {
    model
        .initial_target(row)
        .and_then(y_index_for_slot)
        .and_then(|index| model.variable_name_for_y_index(index))
}
