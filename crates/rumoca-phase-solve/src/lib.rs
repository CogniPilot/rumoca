//! Lower a valid-by-construction DAE into solver-facing register programs.
//!
//! This phase consumes only the immutable, branded view exposed by
//! [`rumoca_ir_dae::Dae::inspect`]. It does not validate, repair, or rewrite a
//! DAE. A continuous system that cannot be proved computable is rejected at
//! this boundary with the responsible source span.

mod artifacts;
mod error;
mod layout;
mod lower;

pub mod ad;
pub mod diagnostic_codes;

pub use ad::{
    lower_compute_block_full_jvp, lower_compute_block_jvp, lower_scalar_program_block_ad,
    lower_scalar_program_block_full_ad, lower_scalar_program_block_full_ad_with_spans,
};
pub use error::LowerError;
pub use layout::build_var_layout;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[cfg(test)]
mod tests;

/// Lower one immutable checked DAE into the canonical Solve problem.
pub fn lower_solve_problem(dae: &dae::Dae) -> Result<solve::SolveProblem, LowerError> {
    let prepared = rumoca_phase_structural::prepare_for_solve(dae).map_err(|error| {
        LowerError::Structural {
            reason: error.to_string(),
            span: error.source_span(),
        }
    })?;
    prepared.as_dae().inspect(lower::lower_solve_problem)
}

/// Materialize optional solver artifacts from an already-lowered problem.
pub fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
) -> Result<solve::SolveArtifacts, LowerError> {
    artifacts::lower_solve_artifacts(problem, solve::MassMatrix::Identity)
}

/// Materialize optional solver artifacts with an explicitly selected mass
/// matrix representation.
pub fn lower_solve_artifacts_with_mass_matrix(
    problem: &solve::SolveProblem,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, LowerError> {
    artifacts::lower_solve_artifacts(problem, mass_matrix)
}
