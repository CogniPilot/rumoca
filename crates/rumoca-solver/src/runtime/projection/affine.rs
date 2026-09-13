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
    let system = AffineBlockSystem {
        model,
        parameters: p,
        time: t,
        block,
        block_index,
        jacobian,
        structure,
        tolerance: tol,
    };
    let residual = system.residual(&candidate)?;
    let Some(solution) = system.solve(&candidate, &residual) else {
        return Ok(ProjectionBlockUpdate {
            changed: false,
            settled: false,
        });
    };
    for (&index, &value) in block.y_indices.iter().zip(solution.iter()) {
        candidate[index] = value;
    }
    if !system.refine(&mut candidate)? {
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

struct AffineBlockSystem<'a, M> {
    model: &'a M,
    parameters: &'a [f64],
    time: f64,
    block: &'a solve::AlgebraicProjectionBlock,
    block_index: usize,
    jacobian: DMatrix<f64>,
    structure: Option<&'a solve::JacobianStructure>,
    tolerance: f64,
}

impl<M: ImplicitProjectionModel> AffineBlockSystem<'_, M> {
    fn residual(&self, y: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
        implicit_selected_residuals(
            self.model,
            y,
            self.parameters,
            self.time,
            &self.block.rows,
            "affine block residual",
        )
    }

    fn scales(&self, y: &[f64]) -> (Vec<f64>, Vec<f64>) {
        algebraic_block_scales(
            self.model,
            y,
            self.block,
            &self.jacobian,
            self.structure.map(solve::JacobianStructure::pattern),
        )
    }

    fn solve(&self, y: &[f64], residual: &[f64]) -> Option<DVector<f64>> {
        let (row_scales, variable_scales) = self.scales(y);
        self.model
            .solve_algebraic_newton_delta(
                self.block_index,
                ScaledNewtonSystem {
                    jacobian: &self.jacobian,
                    residual,
                    row_scales: &row_scales,
                    variable_scales: &variable_scales,
                    structure: self.structure.map(solve::JacobianStructure::pattern),
                    tolerance: self.tolerance,
                },
            )
            .filter(|v| v.len() == self.block.y_indices.len() && v.iter().all(|x| x.is_finite()))
    }

    fn refine(&self, y: &mut [f64]) -> Result<bool, RuntimeSolveError> {
        for iteration in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
            let residual = self.residual(y)?;
            // Zero was only the arithmetic origin used to extract b. The
            // residual certificate uses this candidate's coordinate scales.
            let row_scales = self.scales(y).0;
            let converged = scaled_residual_converged(&residual, &row_scales, self.tolerance);
            // One correction recovers small coordinates lost while solving
            // beside large offsets, even when the residual already fits tol.
            // Exact zero needs no correction; subsequent passes certify the
            // corrected coordinate under the unchanged convergence policy.
            if converged && (iteration > 0 || residual.iter().all(|&value| value == 0.0)) {
                return Ok(true);
            }
            // Factorization roundoff can leave small coordinates inaccurate
            // in a block containing much larger currents or forces. Refine
            // against the original residual with the same certified matrix.
            let Some(delta) = self.solve(y, &residual) else {
                return Ok(false);
            };
            let Some(changed) = self.apply_correction(y, delta.as_slice()) else {
                return Ok(false);
            };
            if !changed {
                return Ok(converged);
            }
        }
        Ok(false)
    }

    fn apply_correction(&self, y: &mut [f64], delta: &[f64]) -> Option<bool> {
        let mut changed = false;
        for (&index, &correction) in self.block.y_indices.iter().zip(delta) {
            let value = y[index] + correction;
            if !value.is_finite() {
                return None;
            }
            changed |= value != y[index];
            y[index] = value;
        }
        Some(changed)
    }
}
