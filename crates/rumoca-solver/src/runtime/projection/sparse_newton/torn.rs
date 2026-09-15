use nalgebra::{DMatrix, DVector, Dyn, LU};
use rumoca_ir_solve as solve;

use super::valid_variable_scale;

#[derive(Clone, Default)]
pub(super) struct TornNewtonCache {
    system: Option<TornSystem>,
}

impl TornNewtonCache {
    pub(super) fn solve_scaled(
        &mut self,
        source: &DMatrix<f64>,
        rhs: &DVector<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
        layout: &solve::AffineEliminationLayout,
    ) -> Option<DVector<f64>> {
        let n = layout.pattern().rows() as usize;
        if source.shape() != (n, n)
            || rhs.len() != n
            || row_scales.len() != n
            || variable_scales.len() != n
        {
            return None;
        }
        if self.system.as_ref().is_none_or(|s| s.layout != *layout) {
            self.system = TornSystem::new(layout);
        }
        let system = self.system.as_mut()?;
        // Check unconditioned coefficients before factor reuse: conditioning
        // may underflow a nonzero future dependency to zero.
        if layout
            .zero_guards()
            .iter()
            .any(|&(row, column)| source[(row, column)] != 0.0)
        {
            system.factor = Factor::Unfactored;
            return None;
        }
        system.update(source, row_scales, variable_scales);
        system.solve(rhs)
    }
}

#[derive(Clone)]
struct TornSystem {
    layout: solve::AffineEliminationLayout,
    offsets: Box<[usize]>,
    values: Box<[f64]>,
    recovery: Box<[f64]>,
    factor: Factor,
    work: DVector<f64>,
    reduced_rhs: DVector<f64>,
}

#[derive(Clone, Default)]
enum Factor {
    #[default]
    Unfactored,
    Ready(LU<f64, Dyn, Dyn>),
    Rejected,
}

impl TornSystem {
    fn new(layout: &solve::AffineEliminationLayout) -> Option<Self> {
        let n = layout.pattern().rows() as usize;
        let k = layout.tears().len();
        let mut offsets = Vec::with_capacity(n + 1);
        offsets.push(0);
        for row in 0..n {
            offsets.push(offsets[row] + layout.row_columns(row).len());
        }
        Some(Self {
            layout: layout.clone(),
            values: vec![0.0; offsets[n]].into_boxed_slice(),
            offsets: offsets.into_boxed_slice(),
            recovery: vec![0.0; n.checked_mul(k)?].into_boxed_slice(),
            factor: Factor::Unfactored,
            work: DVector::zeros(n),
            reduced_rhs: DVector::zeros(k),
        })
    }

    fn update(&mut self, source: &DMatrix<f64>, rows: &[f64], columns: &[f64]) {
        let mut changed = matches!(self.factor, Factor::Unfactored);
        for (row, &row_scale) in rows.iter().enumerate() {
            let values = &mut self.values[self.offsets[row]..self.offsets[row + 1]];
            for (value, &column) in values.iter_mut().zip(self.layout.row_columns(row)) {
                let next = source[(row, column)] * valid_variable_scale(columns[column])
                    / valid_variable_scale(row_scale);
                changed |= value.to_bits() != next.to_bits();
                *value = next;
            }
        }
        if changed {
            // Revoke the previous factor before changing its recovery relation.
            self.factor = Factor::Rejected;
            if let Some(factor) = self.refactor() {
                self.factor = Factor::Ready(factor);
            }
        }
    }

    fn row_values(&self, row: usize) -> &[f64] {
        &self.values[self.offsets[row]..self.offsets[row + 1]]
    }

    fn pivot(&self, row: usize, target: usize) -> Option<f64> {
        let mut magnitude = 0.0_f64;
        let mut pivot = 0.0;
        for (&column, &value) in self
            .layout
            .row_columns(row)
            .iter()
            .zip(self.row_values(row))
        {
            if !value.is_finite() {
                return None;
            }
            magnitude = magnitude.max(value.abs());
            if column == target {
                pivot = value;
            }
        }
        (pivot.abs() > f64::EPSILON.sqrt() * magnitude).then_some(pivot)
    }

    fn refactor(&mut self) -> Option<LU<f64, Dyn, Dyn>> {
        let k = self.layout.tears().len();
        self.recovery.fill(0.0);
        for (column, &target) in self.layout.tears().iter().enumerate() {
            self.recovery[target * k + column] = 1.0;
        }
        for &(row, target) in self.layout.causal() {
            let pivot = self.pivot(row, target)?;
            for column in 0..k {
                self.recovery[target * k + column] =
                    self.recovery_numerator(row, target, column, k) / pivot;
            }
        }
        let reduced = DMatrix::from_fn(k, k, |row, column| {
            let source = self.layout.residuals()[row];
            self.layout
                .row_columns(source)
                .iter()
                .zip(self.row_values(source))
                .map(|(&dependency, &value)| value * self.recovery[dependency * k + column])
                .sum()
        });
        if !self
            .recovery
            .iter()
            .chain(reduced.iter())
            .all(|value| value.is_finite())
        {
            return None;
        }
        let factor = reduced.lu();
        factor.is_invertible().then_some(factor)
    }

    fn recovery_numerator(&self, row: usize, target: usize, column: usize, k: usize) -> f64 {
        let mut sum = 0.0;
        for (&dependency, &value) in self
            .layout
            .row_columns(row)
            .iter()
            .zip(self.row_values(row))
        {
            if dependency != target {
                sum -= value * self.recovery[dependency * k + column];
            }
        }
        sum
    }

    fn causal_rhs(&self, row: usize, target: usize, mut value: f64) -> f64 {
        let mut pivot = 0.0;
        for (&dependency, &coefficient) in self
            .layout
            .row_columns(row)
            .iter()
            .zip(self.row_values(row))
        {
            if dependency == target {
                pivot = coefficient;
            } else {
                value -= coefficient * self.work[dependency];
            }
        }
        value / pivot
    }

    fn solve(&mut self, rhs: &DVector<f64>) -> Option<DVector<f64>> {
        let Factor::Ready(factor) = &self.factor else {
            return None;
        };
        self.work.fill(0.0);
        for &(row, target) in self.layout.causal() {
            self.work[target] = self.causal_rhs(row, target, rhs[row]);
        }
        for (row, &source) in self.layout.residuals().iter().enumerate() {
            let mut value = rhs[source];
            for (&dependency, &coefficient) in self
                .layout
                .row_columns(source)
                .iter()
                .zip(self.row_values(source))
            {
                value -= coefficient * self.work[dependency];
            }
            self.reduced_rhs[row] = value;
        }
        if !factor.solve_mut(&mut self.reduced_rhs) {
            return None;
        }
        let k = self.reduced_rhs.len();
        for &(_, target) in self.layout.causal() {
            let correction: f64 = self.recovery[target * k..(target + 1) * k]
                .iter()
                .zip(self.reduced_rhs.iter())
                .map(|(a, b)| a * b)
                .sum();
            self.work[target] += correction;
        }
        for (&target, &value) in self.layout.tears().iter().zip(self.reduced_rhs.iter()) {
            self.work[target] = value;
        }
        self.work
            .iter()
            .all(|value| value.is_finite())
            .then(|| self.work.clone())
    }
}
