//! Construction proofs that a causal step isolates its unknown exactly and
//! through a coefficient bounded away from zero (SPEC_0043 §4).

use rumoca_ir_solve as solve;

use super::target_catalog::ExactAssignmentAccess;
use crate::PreparedScalarProgramBlock;

/// Whether `row` is an admissible causal step for solver-Y unknown `y_index`:
/// an exact isolator whose coefficient has a construction proof.
pub fn causal_step_is_proven(
    implicit_scalar_rhs: &PreparedScalarProgramBlock,
    row: usize,
    y_index: usize,
) -> bool {
    causal_step_coefficient_proof(implicit_scalar_rhs, row, y_index)
        != solve::CausalCoefficient::Unproven
        && causal_step_certifies_exact_assignment(implicit_scalar_rhs, row, y_index)
}

/// The construction proof that `row` isolates solver-Y unknown `y_index`
/// through a coefficient bounded away from zero; a row without a program is
/// unproven.
pub fn causal_step_coefficient_proof(
    implicit_scalar_rhs: &PreparedScalarProgramBlock,
    row: usize,
    y_index: usize,
) -> solve::CausalCoefficient {
    implicit_scalar_rhs.coefficient_proof(row, y_index)
}

/// Whether evaluating `row`'s target isolator and writing its value satisfies
/// the scalar residual exactly for solver-Y unknown `y_index`. This mirrors the
/// runtime `implicit_target_assignment_is_exact` predicate exactly.
pub(super) fn causal_step_certifies_exact_assignment(
    implicit_scalar_rhs: &impl ExactAssignmentAccess,
    row: usize,
    y_index: usize,
) -> bool {
    implicit_scalar_rhs.is_exact_assignment(row, y_index)
}
