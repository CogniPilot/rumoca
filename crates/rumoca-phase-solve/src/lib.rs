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
mod model_values;
mod model_wire;

pub mod ad;
pub mod diagnostic_codes;
pub mod fmi;

pub use ad::{
    lower_compute_block_full_jvp, lower_compute_block_jvp, lower_scalar_program_block_ad,
    lower_scalar_program_block_full_ad, lower_scalar_program_block_full_ad_with_spans,
};
pub use error::LowerError;
pub use layout::build_var_layout;
pub use model_values::{
    LoweredSolveModel, SolveModelLoweringError, SolveModelLoweringStage, lower_solve_model,
};
pub use model_wire::{
    SOLVE_MODEL_SCHEMA_VERSION, SolveModelWireError, SolveModelWireRef, deserialize_solve_model,
    solve_model_wire,
};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[cfg(test)]
mod tests;

/// Lower one immutable checked DAE into the canonical Solve problem.
pub fn lower_solve_problem(dae: &dae::Dae) -> Result<solve::SolveProblem, LowerError> {
    lower_solve_package(dae).map(|package| package.problem)
}

/// One canonical numerical root plus its model-level typed call inventory.
pub struct LoweredSolvePackage {
    pub problem: solve::SolveProblem,
    pub pure_calls: solve::SolvePureCallTable,
}

/// Lower one immutable checked DAE and retain its model-level call owners.
pub fn lower_solve_package(dae: &dae::Dae) -> Result<LoweredSolvePackage, LowerError> {
    let prepared = rumoca_phase_structural::prepare_for_solve(dae).map_err(|error| {
        LowerError::Structural {
            reason: error.to_string(),
            span: error.source_span(),
        }
    })?;
    lower_prepared_solve_package(&prepared)
}

/// Lower the exact prepared DAE retained by complete-model construction.
fn lower_prepared_solve_package(
    prepared: &rumoca_phase_structural::PreparedDae<'_>,
) -> Result<LoweredSolvePackage, LowerError> {
    prepared
        .inspect(lower::lower_solve_problem)
        .map(|(problem, pure_calls)| LoweredSolvePackage {
            problem,
            pure_calls,
        })
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
