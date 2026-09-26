//! Construction proofs that a causal step isolates its unknown exactly and
//! through a coefficient bounded away from zero (SPEC_0043 §4).

use rumoca_ir_solve as solve;

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
    let program = implicit_scalar_rhs
        .row_output_position(row)
        .and_then(|(program, output)| {
            Some((implicit_scalar_rhs.block().program(program)?, output))
        });
    match program {
        Some((ops, output)) => solve::isolator_coefficient_proof(ops, output, y_index),
        None => solve::CausalCoefficient::Unproven,
    }
}

/// Whether evaluating `row`'s target isolator and writing its value satisfies
/// the scalar residual exactly for solver-Y unknown `y_index`. This mirrors the
/// runtime `implicit_target_assignment_is_exact` predicate exactly.
pub(super) fn causal_step_certifies_exact_assignment(
    implicit_scalar_rhs: &PreparedScalarProgramBlock,
    row: usize,
    y_index: usize,
) -> bool {
    implicit_scalar_rhs
        .row_output_position(row)
        .is_some_and(|(program_idx, output_offset)| {
            implicit_scalar_rhs.certifies_exact_target_assignment_output(
                program_idx,
                output_offset,
                y_index,
            )
        })
}
