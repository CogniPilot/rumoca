use rumoca_ir_solve as solve;

use crate::LowerError;
use crate::ad::{
    lower_compute_block_full_jvp, lower_compute_block_jvp,
    lower_scalar_program_block_full_ad_with_spans,
};

pub(crate) fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, LowerError> {
    let implicit_rhs =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs)?;
    let derivative_rhs =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs)?;
    let implicit_jacobian_v_scalar = solve::ScalarProgramBlock::with_output_indices(
        lower_scalar_program_block_full_ad_with_spans(
            implicit_rhs.programs(),
            implicit_rhs.program_spans(),
            &problem.layout,
        )?,
        implicit_rhs.program_spans().to_vec(),
        implicit_rhs.output_indices().to_vec(),
    )?;
    let full_jacobian_v = solve::ScalarProgramBlock::with_output_indices(
        lower_scalar_program_block_full_ad_with_spans(
            derivative_rhs.programs(),
            derivative_rhs.program_spans(),
            &problem.layout,
        )?,
        derivative_rhs.program_spans().to_vec(),
        derivative_rhs.output_indices().to_vec(),
    )?;
    let implicit_jacobian_v = lower_compute_block_jvp(&problem.continuous.implicit_rhs)?;
    let manifold_jacobian_v = lower_compute_block_jvp(&problem.continuous.manifold_residual)?;
    let initialization_jacobian_v = lower_compute_block_full_jvp(
        problem.initialization.residual(),
        problem.solve_layout.solver_scalar_count(),
    )?;
    let mut artifacts = solve::SolveArtifacts {
        continuous: solve::ContinuousSolveArtifacts {
            structural: solve::ContinuousStructuralArtifacts::default(),
            mass_matrix,
            implicit_jacobian_v,
            implicit_jacobian_v_scalar,
            manifold_jacobian_v,
            full_jacobian_v,
        },
        initialization: solve::InitializationSolveArtifacts {
            structural: solve::InitializationStructuralArtifacts::default(),
            residual_jacobian_v: initialization_jacobian_v,
        },
    };
    let (continuous, initialization) = rumoca_eval_solve::derive_solve_structural_artifacts(
        problem, &artifacts,
    )
    .map_err(|error| match error.source_span() {
        Some(span) => LowerError::contract(error.to_string(), span),
        None => LowerError::unspanned_non_computable(error.to_string()),
    })?;
    artifacts.continuous.structural = continuous;
    artifacts.initialization.structural = initialization;
    specialize_algebraic_jacobians(problem, &implicit_rhs, &mut artifacts.continuous.structural)?;
    Ok(artifacts)
}

fn specialize_algebraic_jacobians(
    problem: &solve::SolveProblem,
    primal: &solve::ScalarProgramBlock,
    structures: &mut solve::ContinuousStructuralArtifacts,
) -> Result<(), LowerError> {
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
            .map_err(LowerError::unspanned_non_computable)?;
    }
    Ok(())
}
