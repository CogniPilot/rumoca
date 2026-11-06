mod dense;
#[cfg(test)]
mod tests;
mod torn;

use nalgebra::DVector;
use rumoca_ir_solve::StructuralPattern;

use super::scaling::valid_variable_scale;
use crate::sparse_lu::SparseLuSystem;

#[derive(Clone, Default)]
pub(crate) struct SparseNewtonCache {
    system: Option<PreparedSparseSystem>,
    torn: torn::TornNewtonCache,
    dense: dense::DenseNewtonFactor,
    /// The origin linearization of the last affine solve, its Jacobian zero
    /// outside the block's structural pattern, kept so the next solve reuses
    /// it under the same certified parameter snapshot or refills only its
    /// pattern.
    affine_linearization: Option<super::affine::AffineLinearization>,
}

impl SparseNewtonCache {
    /// The cache of one projection block. A block whose affine elimination
    /// layout is admitted (`projection_policy::affine_elimination_capacity`)
    /// and stated in the block's own pattern has its torn system issued here,
    /// once, rather than admitted and compared on every solve.
    pub(crate) fn for_block(structure: &rumoca_ir_solve::ColoredJacobianStructure) -> Self {
        let torn = structure
            .affine_elimination()
            .filter(|layout| {
                layout.pattern() == structure.pattern()
                    && rumoca_eval_solve::projection_policy::affine_elimination_capacity(layout)
                        .is_some()
            })
            .map_or_else(
                torn::TornNewtonCache::default,
                torn::TornNewtonCache::issued,
            );
        Self {
            torn,
            ..Self::default()
        }
    }

    pub(crate) fn has_issued_torn(&self) -> bool {
        self.torn.is_issued()
    }

    pub(super) fn solve_torn_issued(
        &mut self,
        source: &super::BlockJacobian,
        residual: &[f64],
        scales: (&[f64], &[f64]),
        revision: Option<u64>,
    ) -> Option<DVector<f64>> {
        self.torn.solve_issued(source, residual, scales, revision)
    }

    pub(crate) fn take_affine_linearization(
        &mut self,
    ) -> Option<super::affine::AffineLinearization> {
        self.affine_linearization.take()
    }

    pub(crate) fn retain_affine_linearization(
        &mut self,
        linearization: super::affine::AffineLinearization,
    ) {
        self.affine_linearization = Some(linearization);
    }

    /// Dense scaled Newton solve reusing this block's factorization while
    /// its Jacobian and scales are bitwise unchanged.
    pub(super) fn solve_dense_scaled(
        &mut self,
        source: &super::BlockJacobian,
        rhs: &DVector<f64>,
        scales: (&[f64], &[f64]),
        tolerance: f64,
        allow_rank_deficient_fallback: bool,
    ) -> Option<DVector<f64>> {
        self.dense.solve(
            source,
            rhs,
            scales,
            tolerance,
            allow_rank_deficient_fallback,
        )
    }

    pub(super) fn solve_torn_scaled(
        &mut self,
        source: &super::BlockJacobian,
        rhs: &DVector<f64>,
        scales: (&[f64], &[f64]),
        layout: &rumoca_ir_solve::AffineEliminationLayout,
        revision: Option<u64>,
    ) -> Option<DVector<f64>> {
        self.torn
            .solve_scaled(source, rhs, scales, layout, revision)
    }

    /// Size of the current ready torn reduced system: issued plus promoted
    /// tears.
    #[cfg(test)]
    pub(crate) fn torn_reduced_size(&self) -> Option<usize> {
        self.torn.reduced_size()
    }

    pub(super) fn solve_scaled(
        &mut self,
        source: &super::BlockJacobian,
        rhs: &DVector<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
        pattern: &StructuralPattern,
    ) -> Option<DVector<f64>> {
        if self
            .system
            .as_ref()
            .is_none_or(|system| system.pattern != *pattern)
        {
            self.system = PreparedSparseSystem::new(source.nrows(), pattern);
        }
        self.system
            .as_mut()?
            .solve_scaled(source, rhs, row_scales, variable_scales)
    }
}

#[derive(Clone)]
struct PreparedSparseSystem {
    pattern: StructuralPattern,
    system: SparseLuSystem,
}

impl PreparedSparseSystem {
    fn new(dimension: usize, pattern: &StructuralPattern) -> Option<Self> {
        let system = SparseLuSystem::new(dimension, pattern.nonzero_coordinates())?;
        Some(Self {
            pattern: pattern.clone(),
            system,
        })
    }

    fn solve_scaled(
        &mut self,
        source: &super::BlockJacobian,
        rhs: &DVector<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
    ) -> Option<DVector<f64>> {
        self.system.fill(|_, row, column| {
            source[(row, column)] * valid_variable_scale(variable_scales[column])
                / valid_variable_scale(row_scales[row])
        });
        if !self.system.refactor_if_changed() {
            return None;
        }
        let mut solution = rhs.clone();
        if !self.system.solve_in_place(solution.as_mut_slice()) {
            return None;
        }
        solution
            .iter()
            .all(|value| value.is_finite())
            .then_some(solution)
    }
}
