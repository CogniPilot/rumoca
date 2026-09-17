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
mod state_selection;

pub mod ad;
pub mod diagnostic_codes;
pub mod fmi;

pub use ad::{
    lower_compute_block_full_jvp, lower_compute_block_jvp, lower_scalar_program_block_ad,
    lower_scalar_program_block_full_ad_with_spans,
};
pub use error::LowerError;
pub use layout::build_var_layout;
pub use lower::typed_functions::formal_stages::{
    FormalDerivativePrograms, FormalResidualAssertion, FormalResidualProgram, FormalStageProgram,
    lower_formal_derivative_stages,
};
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
    let selection =
        state_selection::prepare(dae, &std::collections::HashMap::new()).map_err(|error| {
            LowerError::Structural {
                reason: error.to_string(),
                span: error.source_span(),
            }
        })?;
    lower_selection(&selection, &std::collections::HashMap::new())
}

/// Lower the primary basis and attach each alternate reduced chart's executable
/// plan, re-lowered from its own prepared DAE. The primary problem is unchanged;
/// a model with no alternate charts lowers exactly as before.
pub(crate) fn lower_selection(
    selection: &state_selection::PreparedSelection<'_>,
    overrides: &std::collections::HashMap<String, f64>,
) -> Result<LoweredSolvePackage, LowerError> {
    let mut package = lower_prepared_solve_package(&selection.primary, overrides)?;
    attach_alternate_chart_plans(
        &mut package.problem.continuous,
        &selection.alternates,
        overrides,
    )?;
    Ok(package)
}

/// Re-lower each alternate reduced chart's prepared DAE and carry its
/// reconstruction and derivative kernel on the matching chart. The alternates
/// align positionally with reduced chart index one and above; chart zero is the
/// primary basis and keeps no separate plan.
fn attach_alternate_chart_plans(
    continuous: &mut solve::ContinuousSolveSystem,
    alternates: &[rumoca_phase_structural::PreparedDae<'_>],
    overrides: &std::collections::HashMap<String, f64>,
) -> Result<(), LowerError> {
    if alternates.is_empty() {
        return Ok(());
    }
    if continuous.reduced_chart_set.charts.len() != alternates.len() + 1 {
        return Err(LowerError::unspanned_non_computable(
            "reduced chart count does not match the prepared alternate selections",
        ));
    }
    for (offset, alternate) in alternates.iter().enumerate() {
        let lowered = lower_prepared_solve_package(alternate, overrides)?;
        let alternate_continuous = lowered.problem.continuous;
        continuous.reduced_chart_set.charts[offset + 1].plan = Some(solve::ReducedChartPlan {
            implicit_rhs: alternate_continuous.implicit_rhs,
            implicit_row_targets: alternate_continuous.implicit_row_targets,
            algebraic_projection_plan: alternate_continuous.algebraic_projection_plan,
            residual: alternate_continuous.residual,
            derivative_rhs: alternate_continuous.derivative_rhs,
        });
    }
    Ok(())
}

/// Lower the exact prepared DAE retained by complete-model construction.
fn lower_prepared_solve_package(
    prepared: &rumoca_phase_structural::PreparedDae<'_>,
    overrides: &std::collections::HashMap<String, f64>,
) -> Result<LoweredSolvePackage, LowerError> {
    prepared
        .inspect(|system| lower::lower_solve_problem(system, overrides))
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
