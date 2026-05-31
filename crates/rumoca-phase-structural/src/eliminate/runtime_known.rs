use crate::StructuralError;

use super::{Dae, equation_analysis_expr, should_preserve_runtime_known_assignment};

pub(super) fn singular_rows_are_runtime_known_assignments(
    dae: &Dae,
    err: &StructuralError,
) -> bool {
    matches!(
        err,
        StructuralError::Singular {
            n_unknowns: 0,
            n_matched: 0,
            ..
        }
    ) && !dae.continuous.equations.is_empty()
        && dae.continuous.equations.iter().all(|eq| {
            let analysis_expr = equation_analysis_expr(eq);
            should_preserve_runtime_known_assignment(dae, &analysis_expr)
        })
}
