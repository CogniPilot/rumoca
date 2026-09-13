//! Evaluate a certified affine block independently of its incoming guess.

use super::*;

pub(super) fn project_affine_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    // Construction proves F(x) = A*x + b for this block. Evaluating b at
    // x=0 and solving A*x=-b computes the coordinate itself. Accepting an
    // incoming Newton iterate merely because its correction is below tol
    // can retain the wrong side of a relation after a discrete branch change.
    let mut candidate = y.to_vec();
    for &index in &block.y_indices {
        candidate[index] = 0.0;
    }
    let structure = model.algebraic_projection_block_structure(block_index);
    let jacobian = algebraic_block_jacobian(
        model,
        &candidate,
        p,
        t,
        &block.rows,
        &block.y_indices,
        structure,
    )?;
    let residual = implicit_selected_residuals(
        model,
        &candidate,
        p,
        t,
        &block.rows,
        "affine block constant term",
    )?;
    let pattern = structure.map(solve::JacobianStructure::pattern);
    let (row_scales, variable_scales) =
        algebraic_block_scales(model, &candidate, block, &jacobian, pattern);
    let solution = model.solve_algebraic_newton_delta(
        block_index,
        ScaledNewtonSystem {
            jacobian: &jacobian,
            residual: &residual,
            row_scales: &row_scales,
            variable_scales: &variable_scales,
            structure: pattern,
            tolerance: tol,
        },
    );
    let Some(solution) = solution.filter(|v| v.iter().all(|x| x.is_finite())) else {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    };
    for (&index, &value) in block.y_indices.iter().zip(solution.iter()) {
        candidate[index] = value;
    }
    let residual = implicit_selected_residuals(
        model,
        &candidate,
        p,
        t,
        &block.rows,
        "affine block solution",
    )?;
    if !scaled_residual_converged(&residual, &row_scales, tol) {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    }
    let mut changed = false;
    for &index in &block.y_indices {
        changed |= y[index] != candidate[index];
        y[index] = candidate[index];
    }
    Ok(ProjectionBlockUpdate {
        changed,
        settled: true,
    })
}
