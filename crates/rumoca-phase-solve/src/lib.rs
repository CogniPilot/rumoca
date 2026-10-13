//! Lower a valid-by-construction DAE into solver-facing register programs.
//!
//! This phase consumes only the immutable, branded view exposed by
//! [`rumoca_ir_dae::Dae::inspect`]. It does not validate, repair, or rewrite a
//! DAE. A continuous system that cannot be proved computable is rejected at
//! this boundary with the responsible source span.

mod error;
mod layout;
mod lower;
mod model_values;
mod numeric_values;
mod phasor_sources;
mod proven_tearing;
mod sensitivity_parameters;
mod state_selection;

pub mod diagnostic_codes;
pub mod fmi;

pub use error::LowerError;
pub use layout::build_var_layout;
pub use lower::typed_functions::FunctionAssertionMessage;
pub use lower::typed_functions::formal_stages::{
    FormalDerivativePrograms, FormalResidualAssertion, FormalResidualProgram, FormalStageProgram,
    lower_formal_derivative_stages,
};
pub use model_values::{
    LoweredSolveModel, SolveModelLoweringError, SolveModelLoweringStage, lower_solve_model,
    lower_solve_model_for_profile, lower_solve_model_with_input_policy,
};
pub use sensitivity_parameters::{independent_tunable_parameters, select_sensitivity_parameters};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_ir_solve::RefreshProfile;

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
    lower_selection(
        &selection,
        &std::collections::HashMap::new(),
        RefreshProfile::default(),
    )
}

/// The scalars Solve lowering integrates for `dae`, as source scalar names in
/// basis order: the candidate's state coordinate map when a formal selection chose
/// the basis (a formal derivative order `k` wraps the name in `k` `der`s), and
/// the prepared system's states otherwise.
pub fn integrated_state_names(dae: &dae::Dae) -> Result<Vec<String>, LowerError> {
    let structural = |error: rumoca_phase_structural::StructuralError| LowerError::Structural {
        reason: error.to_string(),
        span: error.source_span(),
    };
    state_selection::prepare(dae, &std::collections::HashMap::new())
        .and_then(|selection| selection.integrated_names())
        .map_err(structural)
}

/// Why Solve lowering kept the reducer's basis for `dae` although its
/// `StateSelect` preferences request another (MLS 3.7 §4.9.7.1): the refusal of
/// the requested basis's formal construction or checked selection.
pub fn withheld_state_preferences(dae: &dae::Dae) -> Result<Option<String>, LowerError> {
    state_selection::prepare(dae, &std::collections::HashMap::new())
        .map(|selection| selection.withheld_preferences)
        .map_err(|error| LowerError::Structural {
            reason: error.to_string(),
            span: error.source_span(),
        })
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
/// plan, lowered once against the primary from its own prepared DAE. The primary problem is unchanged;
/// a model with no alternate charts lowers exactly as before.
pub(crate) fn lower_selection(
    selection: &state_selection::PreparedSelection<'_>,
    overrides: &std::collections::HashMap<String, f64>,
    profile: RefreshProfile,
) -> Result<LoweredSolvePackage, LowerError> {
    let mut package = lower_prepared_solve_package(&selection.primary, overrides, None, profile)?;
    package.problem.continuous.reduced_chart_set.exchanges = selection.exchanges.clone();
    attach_alternate_chart_plans(
        &mut package.problem,
        &selection.alternates,
        overrides,
        profile,
    )?;
    Ok(package)
}

/// Lower each alternate reduced chart's prepared DAE against the primary (see
/// [`continuous_refresh_owners`]) and carry its
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
    profile: RefreshProfile,
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
        let lowered = match lower_prepared_solve_package(alternate, overrides, Some(&base), profile)
        {
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
        if exchange && !same_solver_layout(&lowered, &base) {
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
            delta: None,
        };
        // The wire carries each alternate as a delta against the primary; the
        // delta is issued only when patching the primary reproduces this plan.
        plan.delta = match solve::ChartPlanDelta::diff(&base.continuous, &plan) {
            Ok(delta) => Some(delta),
            Err(error) => {
                return Err(LowerError::unspanned_non_computable(format!(
                    "alternate reduced chart plan has no faithful delta: {error}"
                )));
            }
        };
        plan.artifacts =
            rumoca_eval_solve::artifacts::reduced_chart_continuous_artifacts(&base, &plan)
                .map_err(LowerError::from)?;
        problem.continuous.reduced_chart_set.charts[offset + 1].plan = Some(plan);
    }
    withhold_charts(&mut problem.continuous.reduced_chart_set, &withheld);
    Ok(())
}

/// The layout facts every chart of one exchange must share: the addressing of
/// every scalar (names, slots, shapes, indexed views), every variable's
/// storage run and declaration, and the state/algebraic/output split. The
/// source span a synthetic shape is declared at is provenance and differs
/// between the DAEs of two charts, so it is not part of the identity.
pub(crate) fn same_exchange_layout(
    (layout, solve_layout): (&solve::VarLayout, &solve::SolveLayout),
    (other_layout, other_solve): (&solve::VarLayout, &solve::SolveLayout),
) -> bool {
    layout.same_addressing(other_layout)
        && solve_layout.variable_storage_runs == other_solve.variable_storage_runs
        && solve_layout.variable_declarations == other_solve.variable_declarations
        && solve_layout.state_scalar_count == other_solve.state_scalar_count
        && solve_layout.algebraic_scalar_count == other_solve.algebraic_scalar_count
        && solve_layout.output_scalar_count == other_solve.output_scalar_count
}

fn same_solver_layout(a: &solve::SolveProblem, b: &solve::SolveProblem) -> bool {
    same_exchange_layout((&a.layout, &a.solve_layout), (&b.layout, &b.solve_layout))
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

/// Lower the exact prepared DAE retained by complete-model construction.
fn lower_prepared_solve_package(
    prepared: &rumoca_phase_structural::PreparedDae<'_>,
    overrides: &std::collections::HashMap<String, f64>,
    primary: Option<&solve::SolveProblem>,
    profile: RefreshProfile,
) -> Result<LoweredSolvePackage, LowerError> {
    prepared
        .inspect(|system| lower::lower_solve_problem(system, overrides, primary, profile))
        .map(|(problem, pure_calls)| LoweredSolvePackage {
            problem,
            pure_calls,
        })
}

/// The continuous refresh owners of `problem`: from scratch for a primary
/// basis, and from `primary`'s for an alternate reduced chart, which re-runs
/// the per-row analysis only where its rows differ (SPEC_0040 STRUCT-T07
/// constraint-fold chart rows). With debug assertions the from-scratch
/// construction checks every alternate's owners and projection plan.
pub(crate) fn continuous_refresh_owners(
    problem: &mut solve::SolveProblem,
    primary: Option<&solve::SolveProblem>,
    calls: &solve::SolvePureCallTable,
    profile: RefreshProfile,
) -> Result<solve::ContinuousRefreshOwners, rumoca_eval_solve::EvalSolveError> {
    use rumoca_eval_solve::refresh_plan::{
        build_continuous_refresh_owners_from_with_calls, build_continuous_refresh_owners_with_calls,
    };
    proven_tearing::retear_unproven_blocks(problem)?;
    let Some(primary) = primary else {
        return build_continuous_refresh_owners_with_calls(problem, calls, profile);
    };
    #[cfg(debug_assertions)]
    let mut checked = problem.clone();
    let owners = build_continuous_refresh_owners_from_with_calls(problem, primary, calls, profile)?;
    #[cfg(debug_assertions)]
    {
        let scratch = build_continuous_refresh_owners_with_calls(&mut checked, calls, profile);
        debug_assert!(
            scratch.is_ok_and(|scratch| owners.issues_same_plans(&scratch))
                && checked.continuous.algebraic_projection_plan
                    == problem.continuous.algebraic_projection_plan,
            "an alternate chart's refresh owners differ from their from-scratch construction"
        );
    }
    Ok(owners)
}
