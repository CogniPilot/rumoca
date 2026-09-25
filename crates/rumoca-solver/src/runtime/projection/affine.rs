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
    let (row_scales, variable_scales) = algebraic_block_scales(
        model,
        &candidate,
        block,
        &jacobian,
        structure.map(solve::JacobianStructure::pattern),
    );
    let row_magnitudes =
        jacobian_row_magnitudes(&jacobian, structure.map(solve::JacobianStructure::pattern));
    let row_derived = jacobian_row_derived(
        &jacobian,
        &variable_scales,
        structure.map(solve::JacobianStructure::pattern),
    );
    let mut system = AffineBlockSystem {
        model,
        parameters: p,
        time: t,
        block,
        block_index,
        jacobian,
        row_magnitudes,
        row_derived,
        certificate_scales: CertificateScales::new(model, block),
        row_scales,
        variable_scales,
        structure,
        tolerance: tol,
        prefer_torn: true,
        used_torn: std::cell::Cell::new(false),
    };
    let mut settled = system.project(&mut candidate)?;
    if !settled && system.used_torn.get() {
        system.prefer_torn = false;
        for &index in &block.y_indices {
            candidate[index] = 0.0;
        }
        settled = system.project(&mut candidate)?;
    }
    if !settled {
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
    /// [`jacobian_row_magnitudes`] of `jacobian`.
    row_magnitudes: Vec<f64>,
    /// [`jacobian_row_derived`] at the origin variable scales.
    row_derived: Vec<bool>,
    /// The model scales the refinement's certificate reads.
    certificate_scales: CertificateScales,
    row_scales: Vec<f64>,
    variable_scales: Vec<f64>,
    structure: Option<&'a solve::JacobianStructure>,
    tolerance: f64,
    prefer_torn: bool,
    used_torn: std::cell::Cell<bool>,
}

impl<M: ImplicitProjectionModel> AffineBlockSystem<'_, M> {
    fn project(&self, y: &mut [f64]) -> Result<bool, RuntimeSolveError> {
        let residual = self.residual(y)?;
        let Some(solution) = self.solve(&residual) else {
            return Ok(false);
        };
        for (&index, &value) in self.block.y_indices.iter().zip(solution.iter()) {
            y[index] = value;
        }
        self.refine(y)
    }

    fn residual(&self, y: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
        if let Some(selection) = self
            .structure
            .and_then(solve::JacobianStructure::residual_output_evaluation)
        {
            let mut residual = vec![0.0; self.block.rows.len()];
            if self.model.eval_implicit_residual_outputs(
                selection,
                y,
                self.parameters,
                self.time,
                &mut residual,
            )? {
                return Ok(residual);
            }
        }
        implicit_selected_residuals(
            self.model,
            y,
            self.parameters,
            self.time,
            &self.block.rows,
            "affine block residual",
        )
    }

    fn solve(&self, residual: &[f64]) -> Option<DVector<f64>> {
        // Conditioning belongs to this fixed matrix. Candidate-dependent
        // scales still certify the fresh source residual in `refine`.
        let system = ScaledNewtonSystem {
            jacobian: &self.jacobian,
            residual,
            row_scales: &self.row_scales,
            variable_scales: &self.variable_scales,
            structure: self.structure.map(solve::JacobianStructure::pattern),
            tolerance: self.tolerance,
        };
        let finite = |v: &DVector<f64>| {
            v.len() == self.block.y_indices.len() && v.iter().all(|x| x.is_finite())
        };
        if self.prefer_torn
            && let Some(delta) = self
                .model
                .solve_affine_torn_delta(self.block_index, system)
                .filter(finite)
        {
            self.used_torn.set(true);
            return Some(delta);
        }
        self.model
            .solve_algebraic_newton_delta(self.block_index, system)
            .filter(finite)
    }

    fn refine(&self, y: &mut [f64]) -> Result<bool, RuntimeSolveError> {
        for iteration in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
            let residual = self.residual(y)?;
            // Zero was only the arithmetic origin used to extract b. The
            // residual certificate uses this candidate's coordinate scales.
            let origin = OriginRowScales {
                jacobian: &self.jacobian,
                structure: self.structure.map(solve::JacobianStructure::pattern),
                scales: &self.row_scales,
                magnitudes: &self.row_magnitudes,
                derived: &self.row_derived,
            };
            let converged = origin_bounded_residual_converged(
                y,
                &self.certificate_scales,
                &origin,
                &residual,
                self.tolerance,
            );
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
            let Some(delta) = self.solve(&residual) else {
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
