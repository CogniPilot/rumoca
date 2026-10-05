use rumoca_ir_solve as solve;

use crate::LowerError;
use crate::ad::{
    lower_compute_block_full_jvp, lower_compute_block_jvp, lower_scalar_program_block_full_jvp,
};

pub(crate) fn lower_solve_artifacts(
    problem: &solve::SolveProblem,
    mass_matrix: solve::MassMatrix,
) -> Result<solve::SolveArtifacts, LowerError> {
    let implicit_rhs =
        rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs)?;
    let implicit_jacobian_v_scalar =
        full_jvp_scalar_view(&problem.continuous.implicit_rhs, &problem.layout)?;
    let full_jacobian_v =
        full_jvp_scalar_view(&problem.continuous.derivative_rhs, &problem.layout)?;
    let implicit_jacobian_v = lower_compute_block_jvp(&problem.continuous.implicit_rhs)?;
    let manifold_jacobian_v = lower_compute_block_jvp(&problem.continuous.manifold_residual)?;
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
            update_jacobian_v,
        },
        discrete,
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
) -> Result<solve::ScalarProgramBlock, LowerError> {
    let jvp = lower_compute_block_full_jvp(block, layout.y_scalars())?;
    Ok(rumoca_eval_solve::to_scalar_program_block(&jvp)?)
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
        structured_jacobian_v: rumoca_eval_solve::to_scalar_program_block(&discrete.structured_rhs)
            .ok()
            .as_ref()
            .and_then(jvp),
    }
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
