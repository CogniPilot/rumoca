use rumoca_ir_solve as solve;

use crate::ad::SolveDerivationError;
use crate::ad::{
    lower_compute_block_full_jvp, lower_compute_block_jvp, lower_scalar_program_block_full_jvp,
};

fn lower_solve_artifacts_with_mass_matrix(
    problem: &solve::SolveProblem,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, SolveDerivationError> {
    lower_artifacts(ArtifactSources::Problem(problem), mass_matrix)
}

pub fn lower_solve_artifacts_with_calls(
    problem: &solve::SolveProblem,
    calls: &solve::SolvePureCallTable,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, SolveDerivationError> {
    lower_artifacts(ArtifactSources::Finalized(problem, calls), mass_matrix)
}

enum ArtifactSources<'source> {
    Problem(&'source solve::SolveProblem),
    Finalized(
        &'source solve::SolveProblem,
        &'source solve::SolvePureCallTable,
    ),
}

impl ArtifactSources<'_> {
    fn problem(&self) -> &solve::SolveProblem {
        match self {
            Self::Problem(problem) | Self::Finalized(problem, _) => problem,
        }
    }

    fn assignment(
        &self,
    ) -> Result<Option<solve::ContinuousStructuralArtifacts>, SolveDerivationError> {
        match self {
            Self::Problem(problem) => {
                solve::ContinuousStructuralArtifacts::checked_assignment_for_problem(problem)
            }
            Self::Finalized(problem, calls) => {
                solve::ContinuousStructuralArtifacts::checked_assignment_with_calls(problem, calls)
            }
        }
        .map_err(SolveDerivationError::unspanned_contract)
    }

    fn structural(
        &self,
        artifacts: &solve::SolveArtifacts,
    ) -> Result<
        (
            solve::ContinuousStructuralArtifacts,
            solve::InitializationStructuralArtifacts,
        ),
        crate::EvalSolveError,
    > {
        match self {
            Self::Problem(problem) => crate::derive_solve_structural_artifacts(problem, artifacts),
            Self::Finalized(problem, calls) => {
                crate::derive_solve_structural_artifacts_with_calls(problem, calls, artifacts)
            }
        }
    }
}

fn lower_artifacts(
    source: ArtifactSources<'_>,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, SolveDerivationError> {
    let problem = source.problem();
    let assignment = source.assignment()?;
    let (continuous, implicit_rhs) = if let Some(structural) = assignment {
        (
            solve::ContinuousSolveArtifacts {
                structural,
                mass_matrix,
                ..Default::default()
            },
            None,
        )
    } else {
        let implicit_rhs = crate::to_scalar_program_block(&problem.continuous.implicit_rhs)?;
        let implicit_jacobian_v_scalar =
            full_jvp_scalar_view(&problem.continuous.implicit_rhs, &problem.layout)?;
        let full_jacobian_v =
            full_jvp_scalar_view(&problem.continuous.derivative_rhs, &problem.layout)?;
        let implicit_jacobian_v = lower_compute_block_jvp(&problem.continuous.implicit_rhs)?;
        let manifold_jacobian_v = lower_compute_block_jvp(&problem.continuous.manifold_residual)?;
        (
            solve::ContinuousSolveArtifacts {
                structural: solve::ContinuousStructuralArtifacts::default(),
                mass_matrix,
                implicit_jacobian_v,
                implicit_jacobian_v_scalar,
                manifold_jacobian_v,
                full_jacobian_v,
            },
            Some(implicit_rhs),
        )
    };
    let initialization_jacobian_v = lower_compute_block_full_jvp(
        problem.initialization.residual(),
        problem.solve_layout.solver_scalar_count(),
    )?;
    // An update row without a derivative lowering leaves the settled
    // initialization view without a tangent; only a model whose initialization
    // rows read that view needs one, and it reports the missing derivative.
    let update_jacobian_v = lower_scalar_program_block_full_jvp(
        problem.initialization.update_rhs(),
        problem.solve_layout.solver_scalar_count(),
    )
    .ok();
    let discrete = lower_discrete_artifacts(problem);
    let mut artifacts = solve::SolveArtifacts {
        continuous,
        initialization: solve::InitializationSolveArtifacts {
            structural: solve::InitializationStructuralArtifacts::default(),
            residual_jacobian_v: initialization_jacobian_v,
            update_jacobian_v,
        },
        discrete,
    };
    let (continuous, initialization) =
        source
            .structural(&artifacts)
            .map_err(|error| match error.source_span() {
                Some(span) => SolveDerivationError::contract(error.to_string(), span),
                None => SolveDerivationError::unspanned_contract(error.to_string()),
            })?;
    artifacts.continuous.structural = continuous;
    artifacts.initialization.structural = initialization;
    if let Some(implicit_rhs) = implicit_rhs {
        specialize_algebraic_jacobians(
            problem,
            &implicit_rhs,
            &mut artifacts.continuous.structural,
        )?;
    }
    Ok(artifacts)
}

/// The scalar view of `block`'s full JVP, seeded over solver Y followed by P.
///
/// Forward AD is a per-operation rule that depends neither on a load's index
/// nor on a constant's value under this seed mode, so differentiating a tensor
/// node's base program once and taking the scalar view yields exactly the
/// rows that differentiating each scalar row of the primal view would
/// (SPEC_0032 §4); the AD cost stays proportional to the node, not its domain.
fn full_jvp_scalar_view(
    block: &solve::ComputeBlock,
    layout: &solve::VarLayout,
) -> Result<solve::ScalarProgramBlock, SolveDerivationError> {
    let jvp = lower_compute_block_full_jvp(block, layout.y_scalars())?;
    Ok(crate::to_scalar_program_block(&jvp)?)
}

/// The discrete event rows' JVPs; a family whose rows do not all lower is
/// left without one, which only the coupled event Newton would need.
fn lower_discrete_artifacts(problem: &solve::SolveProblem) -> solve::DiscreteSolveArtifacts {
    let offset = problem.solve_layout.solver_scalar_count();
    let discrete = &problem.discrete;
    let jvp =
        |block: &solve::ScalarProgramBlock| lower_scalar_program_block_full_jvp(block, offset).ok();
    let guarded = &discrete.guarded_assignments;
    let guarded_primal = solve::ScalarProgramBlock::with_output_indices(
        guarded
            .iter()
            .map(|owner| owner.program().to_vec())
            .collect(),
        guarded
            .iter()
            .map(solve::GuardedAssignmentProgram::span)
            .collect(),
        (0..guarded
            .iter()
            .map(solve::GuardedAssignmentProgram::output_count)
            .sum())
            .collect(),
    );
    solve::DiscreteSolveArtifacts {
        rhs_jacobian_v: jvp(&discrete.rhs),
        runtime_assignment_jacobian_v: jvp(&discrete.runtime_assignment_rhs),
        guarded_jacobian_v: guarded_primal.ok().as_ref().and_then(jvp),
        structured_jacobian_v: crate::to_scalar_program_block(&discrete.structured_rhs)
            .ok()
            .as_ref()
            .and_then(jvp),
    }
}

fn specialize_algebraic_jacobians(
    problem: &solve::SolveProblem,
    primal: &solve::ScalarProgramBlock,
    structures: &mut solve::ContinuousStructuralArtifacts,
) -> Result<(), SolveDerivationError> {
    let [solve::ComputeNode::ScalarPrograms(canonical)] =
        problem.continuous.implicit_rhs.nodes.as_slice()
    else {
        return Ok(());
    };
    if !canonical.shares_program_owner(primal) {
        return Ok(());
    }
    let domains = structures
        .algebraic_projection()
        .iter()
        .filter_map(|structure| structure.jacobian_application())
        .filter(|application| application.y_indices().len() > 1)
        .filter_map(|application| solve::ProjectionJacobianSeedDomain::derive(application, primal))
        .collect::<Vec<_>>();
    for domain in domains {
        let application = crate::ad::lower_projection_domain(domain)?;
        structures
            .bind_algebraic_jacobian_application(application)
            .map_err(SolveDerivationError::unspanned_contract)?;
    }
    Ok(())
}

/// Derive executable artifacts from an already issued Solve problem.
pub fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
) -> Result<solve::SolveArtifacts, SolveDerivationError> {
    lower_solve_artifacts_with_mass_matrix(problem, solve::MassMatrix::Identity)
}

/// Materialize the runtime-executable continuous artifacts of one alternate
/// reduced chart. The chart's continuous kernel is spliced into a clone of the
/// primary problem skeleton and lowered through the same artifact path the primary
/// basis uses, so the alternate is a fully executable image in the shared solver-Y
/// space. A reduced first-integral chart retains no manifold, so the assembled
/// system carries an empty manifold and no nested chart set.
pub fn reduced_chart_continuous_artifacts(
    base: &solve::SolveProblem,
    plan: &solve::ReducedChartPlan,
) -> Result<solve::ContinuousSolveArtifacts, SolveDerivationError> {
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
        unlocalizable_guards: base.continuous.unlocalizable_guards.clone(),
    };
    Ok(lower_solve_artifacts(&assembled)?.continuous)
}

/// Rebuild the runtime-executable artifacts of every alternate reduced chart after
/// a model is decoded from the wire. Chart artifacts are derived data and are not
/// serialized; this replays the exact assembly used at construction so a decoded
/// model exposes the same executable chart image a freshly lowered one does.
#[cfg(feature = "wire-replay")]
pub(crate) fn replay_reduced_chart_artifacts(
    problem: &mut solve::SolveProblem,
) -> Result<(), SolveDerivationError> {
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
