mod dense;
#[cfg(test)]
mod tests;
mod torn;

use faer::{
    Conj, MatMut, Par,
    dyn_stack::{MemBuffer, MemStack, StackReq},
    sparse::{
        SparseColMat, Triplet,
        linalg::lu::{LuRef, NumericLu, SymbolicLu, factorize_symbolic_lu},
    },
};
use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve::StructuralPattern;

use super::scaling::valid_variable_scale;

#[derive(Clone, Default)]
pub(crate) struct SparseNewtonCache {
    system: Option<PreparedSparseSystem>,
    torn: torn::TornNewtonCache,
    dense: dense::DenseNewtonFactor,
}

impl SparseNewtonCache {
    /// Dense scaled Newton solve reusing this block's factorization while
    /// its Jacobian and scales are bitwise unchanged.
    pub(super) fn solve_dense_scaled(
        &mut self,
        source: &DMatrix<f64>,
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
        source: &DMatrix<f64>,
        rhs: &DVector<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
        layout: &rumoca_ir_solve::AffineEliminationLayout,
    ) -> Option<DVector<f64>> {
        self.torn
            .solve_scaled(source, rhs, row_scales, variable_scales, layout)
    }

    /// Size of the current ready torn reduced system: issued plus promoted
    /// tears.
    #[cfg(test)]
    pub(crate) fn torn_reduced_size(&self) -> Option<usize> {
        self.torn.reduced_size()
    }

    pub(super) fn solve_scaled(
        &mut self,
        source: &DMatrix<f64>,
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
    matrix: SparseColMat<usize, f64>,
    coordinates: Box<[(usize, usize)]>,
    symbolic: SymbolicLu<usize>,
    factorization: NumericFactor,
    factor_values: Box<[u64]>,
    workspace: SparseWorkspace,
}

#[derive(Clone, Default)]
enum NumericFactor {
    #[default]
    Unfactored,
    Ready(NumericLu<usize, f64>),
    Rejected(NumericLu<usize, f64>),
}

impl NumericFactor {
    fn into_storage(self) -> NumericLu<usize, f64> {
        match self {
            Self::Unfactored => NumericLu::new(),
            Self::Ready(numeric) | Self::Rejected(numeric) => numeric,
        }
    }
}

impl PreparedSparseSystem {
    fn new(dimension: usize, pattern: &StructuralPattern) -> Option<Self> {
        let triplets = pattern
            .nonzero_coordinates()
            .into_iter()
            .map(|(row, column)| Triplet::new(row, column, 0.0))
            .collect::<Vec<_>>();
        let matrix = SparseColMat::try_new_from_triplets(dimension, dimension, &triplets).ok()?;
        let symbolic = factorize_symbolic_lu(matrix.symbolic(), Default::default()).ok()?;
        let workspace = SparseWorkspace::new(&symbolic, faer::get_global_parallelism())?;
        let coordinates = matrix
            .as_ref()
            .triplet_iter()
            .map(|entry| (entry.row, entry.col))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let factor_values = vec![0; coordinates.len()].into_boxed_slice();
        Some(Self {
            pattern: pattern.clone(),
            matrix,
            coordinates,
            symbolic,
            factorization: NumericFactor::Unfactored,
            factor_values,
            workspace,
        })
    }

    fn solve_scaled(
        &mut self,
        source: &DMatrix<f64>,
        rhs: &DVector<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
    ) -> Option<DVector<f64>> {
        for (value, &(row, column)) in self.matrix.val_mut().iter_mut().zip(&self.coordinates) {
            *value = source[(row, column)] * valid_variable_scale(variable_scales[column])
                / valid_variable_scale(row_scales[row]);
        }
        let parallelism = faer::get_global_parallelism();
        if parallelism != self.workspace.parallelism {
            self.workspace = SparseWorkspace::new(&self.symbolic, parallelism)?;
        }
        self.refactor_if_changed(parallelism);
        let NumericFactor::Ready(numeric) = &self.factorization else {
            return None;
        };
        let mut solution = rhs.clone();
        // Only checked factorization of this owner's immutable CSC structure
        // constructs Ready; cloning preserves that pair, and refactoring takes
        // it out of Ready before mutating numeric storage.
        let factor = LuRef::new_unchecked(&self.symbolic, numeric);
        factor.solve_in_place_with_conj(
            Conj::No,
            MatMut::from_column_major_slice_mut(solution.as_mut_slice(), rhs.len(), 1),
            parallelism,
            MemStack::new(&mut self.workspace.storage),
        );
        solution
            .iter()
            .all(|value| value.is_finite())
            .then_some(solution)
    }

    fn refactor_if_changed(&mut self, parallelism: Par) {
        if !matches!(self.factorization, NumericFactor::Unfactored)
            && self
                .matrix
                .val()
                .iter()
                .zip(&self.factor_values)
                .all(|(value, cached)| value.to_bits() == *cached)
        {
            return;
        }
        let mut numeric = std::mem::take(&mut self.factorization).into_storage();
        let succeeded = self
            .symbolic
            .factorize_numeric_lu(
                &mut numeric,
                self.matrix.as_ref(),
                parallelism,
                MemStack::new(&mut self.workspace.storage),
                Default::default(),
            )
            .is_ok();
        self.factorization = if succeeded {
            NumericFactor::Ready(numeric)
        } else {
            NumericFactor::Rejected(numeric)
        };
        for (cached, value) in self.factor_values.iter_mut().zip(self.matrix.val()) {
            *cached = value.to_bits();
        }
    }
}

struct SparseWorkspace {
    parallelism: Par,
    requirements: StackReq,
    storage: MemBuffer,
}

impl SparseWorkspace {
    fn new(symbolic: &SymbolicLu<usize>, parallelism: Par) -> Option<Self> {
        let requirements = StackReq::any_of(&[
            symbolic.factorize_numeric_lu_scratch::<f64>(parallelism, Default::default()),
            symbolic.solve_in_place_scratch::<f64>(1, parallelism),
        ]);
        Some(Self {
            parallelism,
            requirements,
            storage: MemBuffer::try_new(requirements).ok()?,
        })
    }
}

impl Clone for SparseWorkspace {
    fn clone(&self) -> Self {
        Self {
            parallelism: self.parallelism,
            requirements: self.requirements,
            storage: MemBuffer::new(self.requirements),
        }
    }
}
