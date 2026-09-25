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

/// Whether Solve lowering executes a reduced state selection instead of the
/// constrained state manifold that `prepared` retains. `model` is the DAE
/// `prepared` was prepared from (the alias quotient when there is one), so this
/// answers the same decision [`lower_solve_package`] takes for it.
pub fn executes_reduced_state_selection(
    model: &dae::Dae,
    prepared: &rumoca_phase_structural::PreparedDae<'_>,
) -> Result<bool, rumoca_phase_structural::StructuralError> {
    state_selection::executes_reduced_selection(model, prepared)
}

/// Lower the primary basis and attach each alternate reduced chart's executable
/// plan, re-lowered from its own prepared DAE. The primary problem is unchanged;
/// a model with no alternate charts lowers exactly as before.
pub(crate) fn lower_selection(
    selection: &state_selection::PreparedSelection<'_>,
    overrides: &std::collections::HashMap<String, f64>,
) -> Result<LoweredSolvePackage, LowerError> {
    let mut package = lower_prepared_solve_package(&selection.primary, overrides)?;
    package.problem.continuous.reduced_chart_set.exchanges = selection.exchanges.clone();
    attach_alternate_chart_plans(&mut package.problem, &selection.alternates, overrides)?;
    Ok(package)
}

/// Re-lower each alternate reduced chart's prepared DAE and carry its
/// reconstruction kernel, derivative kernel, refresh owners, and runtime-executable
/// solver artifacts on the matching chart. The alternates align positionally with
/// reduced chart index one and above; chart zero is the primary basis and keeps no
/// separate plan.
///
/// An exchange alternate is withheld rather than failing the model when its
/// checked construction failed, when it does not lower, or when it lowers to a
/// solver layout other than the primary's (SPEC_0040 STRUCT-T07 constraint-fold
/// chart rows): every chart must run in the one solver-Y space the transfer
/// relies on. A first-integral mirror keeps its existing failure semantics.
fn attach_alternate_chart_plans(
    problem: &mut solve::SolveProblem,
    alternates: &[Option<rumoca_phase_structural::PreparedDae<'_>>],
    overrides: &std::collections::HashMap<String, f64>,
) -> Result<(), LowerError> {
    if alternates.is_empty() {
        return Ok(());
    }
    if problem.continuous.reduced_chart_set.charts.len() != alternates.len() + 1 {
        return Err(LowerError::unspanned_non_computable(
            "reduced chart count does not match the prepared alternate selections",
        ));
    }
    let exchange = !problem.continuous.reduced_chart_set.exchanges.is_empty();
    let mut withheld = Vec::new();
    // The primary problem skeleton (its layout, solve layout, and initialization)
    // is shared by every alternate basis: an alternate re-selects the Independent
    // coordinates of the same first-integral group in the same solver-Y space. The
    // per-chart artifacts therefore lower against this skeleton with the chart's own
    // continuous kernel spliced in, which is the identical assembly the decode path
    // replays, so a constructed chart and a decoded one carry byte-identical
    // artifacts.
    let base = problem.clone();
    for (offset, alternate) in alternates.iter().enumerate() {
        let Some(alternate) = alternate else {
            withheld.push((
                offset + 1,
                solve::ChartExchangeStatus::WithheldByConstruction,
            ));
            continue;
        };
        let lowered = match lower_prepared_solve_package(alternate, overrides) {
            Ok(lowered) => lowered.problem,
            Err(_) if exchange => {
                withheld.push((
                    offset + 1,
                    solve::ChartExchangeStatus::WithheldByConstruction,
                ));
                continue;
            }
            Err(error) => return Err(error),
        };
        if exchange && !same_solver_layout(&lowered.solve_layout, &base.solve_layout) {
            withheld.push((offset + 1, solve::ChartExchangeStatus::WithheldByLayout));
            continue;
        }
        let alternate_continuous = lowered.continuous;
        let mut plan = solve::ReducedChartPlan {
            implicit_rhs: alternate_continuous.implicit_rhs,
            implicit_row_targets: alternate_continuous.implicit_row_targets,
            algebraic_projection_plan: alternate_continuous.algebraic_projection_plan,
            residual: alternate_continuous.residual,
            derivative_rhs: alternate_continuous.derivative_rhs,
            refresh_owners: alternate_continuous.refresh_owners,
            artifacts: solve::ContinuousSolveArtifacts::default(),
        };
        plan.artifacts = reduced_chart_continuous_artifacts(&base, &plan)?;
        problem.continuous.reduced_chart_set.charts[offset + 1].plan = Some(plan);
    }
    withhold_charts(&mut problem.continuous.reduced_chart_set, &withheld);
    Ok(())
}

/// Two lowered bases share one solver-Y space: the same scalar names in the
/// same slots, with the same state/algebraic split.
fn same_solver_layout(a: &solve::SolveLayout, b: &solve::SolveLayout) -> bool {
    a.solver_maps.names == b.solver_maps.names
        && a.state_scalar_count == b.state_scalar_count
        && a.algebraic_scalar_count == b.algebraic_scalar_count
}

/// Drop the withheld alternate charts, renumber the survivors, and record why
/// each dropped exchange was withheld. A set left with only its primary keeps
/// that partition-only chart; the runtime executes no switch for it.
fn withhold_charts(
    set: &mut solve::ReducedChartSet,
    withheld: &[(usize, solve::ChartExchangeStatus)],
) {
    if withheld.is_empty() {
        return;
    }
    let dropped = |chart: usize| withheld.iter().find(|(index, _)| *index == chart);
    for exchange in &mut set.exchanges {
        let solve::ChartExchangeStatus::Issued { chart } = exchange.status else {
            continue;
        };
        exchange.status = match dropped(chart) {
            Some(&(_, status)) => status,
            None => solve::ChartExchangeStatus::Issued {
                chart: chart - withheld.iter().filter(|(index, _)| *index < chart).count(),
            },
        };
    }
    let mut index = 0;
    set.charts.retain(|_| {
        let keep = dropped(index).is_none();
        index += 1;
        keep
    });
}

/// Materialize the runtime-executable continuous artifacts of one alternate
/// reduced chart. The chart's continuous kernel is spliced into a clone of the
/// primary problem skeleton and lowered through the same artifact path the primary
/// basis uses, so the alternate is a fully executable image in the shared solver-Y
/// space. A reduced first-integral chart retains no manifold, so the assembled
/// system carries an empty manifold and no nested chart set.
pub(crate) fn reduced_chart_continuous_artifacts(
    base: &solve::SolveProblem,
    plan: &solve::ReducedChartPlan,
) -> Result<solve::ContinuousSolveArtifacts, LowerError> {
    let mut assembled = base.clone();
    assembled.continuous = solve::ContinuousSolveSystem {
        implicit_rhs: plan.implicit_rhs.clone(),
        implicit_row_targets: plan.implicit_row_targets.clone(),
        algebraic_projection_plan: plan.algebraic_projection_plan.clone(),
        residual: plan.residual.clone(),
        manifold_residual: solve::ComputeBlock::default(),
        manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
        derivative_rhs: plan.derivative_rhs.clone(),
        refresh_owners: plan.refresh_owners.clone(),
        reduced_chart_set: solve::ReducedChartSet::default(),
    };
    Ok(lower_solve_artifacts(&assembled)?.continuous)
}

/// Rebuild the runtime-executable artifacts of every alternate reduced chart after
/// a model is decoded from the wire. Chart artifacts are derived data and are not
/// serialized; this replays the exact assembly used at construction so a decoded
/// model exposes the same executable chart image a freshly lowered one does.
pub(crate) fn replay_reduced_chart_artifacts(
    problem: &mut solve::SolveProblem,
) -> Result<(), LowerError> {
    if problem
        .continuous
        .reduced_chart_set
        .charts
        .iter()
        .all(|chart| chart.plan.is_none())
    {
        return Ok(());
    }
    let base = problem.clone();
    for chart in &mut problem.continuous.reduced_chart_set.charts {
        if let Some(plan) = chart.plan.as_mut() {
            let artifacts = reduced_chart_continuous_artifacts(&base, plan)?;
            plan.artifacts = artifacts;
        }
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
