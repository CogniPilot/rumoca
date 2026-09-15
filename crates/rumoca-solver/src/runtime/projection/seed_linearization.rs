//! One immutable block matrix shared by its directional right-hand sides.

use super::*;

pub(crate) struct SeedBlockLinearization {
    factor: nalgebra::linalg::LU<f64, nalgebra::Dyn, nalgebra::Dyn>,
    jacobian: DMatrix<f64>,
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
        Ok(Self {
            factor: jacobian.clone().lu(),
            jacobian,
        })
    }

    pub(super) fn row_scales(
        &self,
        model: &dyn ImplicitProjectionModel,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        seed: &[f64],
    ) -> Vec<f64> {
        let structure = model.algebraic_projection_block_structure(block_index);
        algebraic_block_scales(
            model,
            seed,
            block,
            &self.jacobian,
            structure.map(solve::JacobianStructure::pattern),
        )
        .0
    }

    pub(super) fn solve(&self, rhs: &DVector<f64>) -> Option<DVector<f64>> {
        self.factor.solve(rhs)
    }
}
