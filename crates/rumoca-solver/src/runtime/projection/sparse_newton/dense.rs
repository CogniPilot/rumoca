//! Dense scaled Newton factorization reused while its system is unchanged.
//!
//! A block solve that keeps its Jacobian and scales fixed (the affine solve
//! and its original-residual refinement) previously rebuilt the scaled matrix
//! and refactored it on every right-hand side. The factorization is a pure
//! function of the Jacobian and the two scale vectors, so this owner keys it
//! on their exact bits and reuses it until any of them changes. Every solve
//! is therefore bit-identical to a fresh factorization.

use nalgebra::{DMatrix, DVector, Dyn, LU};

use super::super::scaling::scaled_jacobian;

#[derive(Clone, Default)]
pub(super) struct DenseNewtonFactor {
    key: Vec<u64>,
    factor: Option<Factor>,
}

#[derive(Clone)]
struct Factor {
    scaled: DMatrix<f64>,
    lu: LU<f64, Dyn, Dyn>,
}

impl DenseNewtonFactor {
    /// Solve the scaled square system for `rhs`, falling back to the
    /// rank-deficient least-squares solve exactly as a fresh factorization.
    pub(super) fn solve(
        &mut self,
        source: &DMatrix<f64>,
        rhs: &DVector<f64>,
        scales: (&[f64], &[f64]),
        tolerance: f64,
        allow_rank_deficient_fallback: bool,
    ) -> Option<DVector<f64>> {
        let (row_scales, variable_scales) = scales;
        if source.nrows() != source.ncols() {
            // A non-square system has no LU; only the least-squares fallback
            // can answer, exactly as for a fresh solve.
            return allow_rank_deficient_fallback
                .then(|| scaled_jacobian(source, row_scales, variable_scales))
                .and_then(|scaled| scaled.svd(true, true).solve(rhs, tolerance).ok());
        }
        let factor = self.prepare(source, row_scales, variable_scales)?;
        let direct = factor.lu.solve(rhs);
        if allow_rank_deficient_fallback {
            direct.or_else(|| {
                factor
                    .scaled
                    .clone()
                    .svd(true, true)
                    .solve(rhs, tolerance)
                    .ok()
            })
        } else {
            direct
        }
    }

    fn prepare(
        &mut self,
        source: &DMatrix<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
    ) -> Option<&Factor> {
        let unchanged = self.factor.is_some()
            && self.key.len() == 2 + source.len() + row_scales.len() + variable_scales.len()
            && self.key[0] == source.nrows() as u64
            && self.key[1] == source.ncols() as u64
            && source
                .iter()
                .chain(row_scales)
                .chain(variable_scales)
                .zip(&self.key[2..])
                .all(|(value, bits)| value.to_bits() == *bits);
        if unchanged {
            return self.factor.as_ref();
        }
        self.key.clear();
        self.key.push(source.nrows() as u64);
        self.key.push(source.ncols() as u64);
        self.key.extend(
            source
                .iter()
                .chain(row_scales)
                .chain(variable_scales)
                .map(|value| value.to_bits()),
        );
        let scaled = scaled_jacobian(source, row_scales, variable_scales);
        let lu = scaled.clone().lu();
        Some(&*self.factor.insert(Factor { scaled, lu }))
    }
}
