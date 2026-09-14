//! One immutable block matrix shared by its directional right-hand sides.

use super::*;

pub(crate) struct SeedBlockLinearization {
    factor: nalgebra::linalg::LU<f64, nalgebra::Dyn, nalgebra::Dyn>,
    row_scales: Vec<f64>,
}

impl SeedBlockLinearization {
    pub(crate) fn build(
        model: &dyn ImplicitProjectionModel,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        y: &[f64],
        args: AlgebraicProjectionArgs<'_>,
    ) -> Result<Self, RuntimeSolveError> {
        let structure = model.algebraic_projection_block_structure(block_index);
        let jacobian = algebraic_block_jacobian(
            model,
            y,
            args.parameters,
            args.time,
            &block.rows,
            &block.y_indices,
            structure,
        )?;
        let row_scales = algebraic_block_scales(
            model,
            y,
            block,
            &jacobian,
            structure.map(solve::JacobianStructure::pattern),
        )
        .0;
        Ok(Self {
            factor: jacobian.lu(),
            row_scales,
        })
    }

    pub(super) fn row_scales(&self) -> &[f64] {
        &self.row_scales
    }

    pub(super) fn solve(&self, rhs: &DVector<f64>) -> Option<DVector<f64>> {
        self.factor.solve(rhs)
    }
}
