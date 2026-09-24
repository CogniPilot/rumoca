use nalgebra::{DMatrix, DVector, Dyn, LU};
use rumoca_eval_solve::projection_policy::torn_promotion_capacity;
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
        system.update(source, row_scales, variable_scales);
        system.solve(rhs)
    }

    #[cfg(test)]
    pub(super) fn reduced_size(&self) -> Option<usize> {
        let system = self.system.as_ref()?;
        matches!(system.factor, Factor::Ready(_)).then_some(system.tear_columns.len())
    }
}

/// The torn affine elimination of one layout with in-place tear promotion.
///
/// The recovery relation expresses every block coordinate as a particular
/// solution plus a linear combination of the tears. Its storage has one
/// column per tear the policy capacity allows, so a step promoted during
/// elimination appends a column without moving earlier ones. Promotion is
/// exact: an earlier step never depends on a promoted coordinate except
/// through an exactly zero guard, so its recovery row holds zero in every
/// later column. Promotions are re-derived from the current coefficients on
/// every factorization; none carries over from another call.
#[derive(Clone)]
struct TornSystem {
    layout: solve::AffineEliminationLayout,
    /// Tear capacity and recovery row stride.
    capacity: usize,
    offsets: Box<[usize]>,
    values: Box<[f64]>,
    recovery: Box<[f64]>,
    /// Causal positions promoted by a nonzero raw guard.
    guarded: Box<[bool]>,
    next_guarded: Box<[bool]>,
    /// Causal positions skipped by elimination because they are tears.
    promoted: Box<[bool]>,
    /// Residual rows of the reduced system: issued, then promoted in order.
    residual_rows: Vec<usize>,
    /// Tear coordinates in reduced-column order, matching `residual_rows`.
    tear_columns: Vec<usize>,
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

/// The elimination check of one causal step.
enum Pivot {
    Usable(f64),
    /// At or below `sqrt(eps)` of its row's largest conditioned coefficient.
    Weak,
    NonFinite,
}

impl TornSystem {
    fn new(layout: &solve::AffineEliminationLayout) -> Option<Self> {
        let n = layout.pattern().rows() as usize;
        let capacity = torn_promotion_capacity(layout.tears().len())?;
        let steps = layout.causal().len();
        let mut offsets = Vec::with_capacity(n + 1);
        offsets.push(0);
        for row in 0..n {
            offsets.push(offsets[row] + layout.row_columns(row).len());
        }
        Some(Self {
            layout: layout.clone(),
            capacity,
            values: vec![0.0; offsets[n]].into_boxed_slice(),
            offsets: offsets.into_boxed_slice(),
            recovery: vec![0.0; n.checked_mul(capacity)?].into_boxed_slice(),
            guarded: vec![false; steps].into_boxed_slice(),
            next_guarded: vec![false; steps].into_boxed_slice(),
            promoted: vec![false; steps].into_boxed_slice(),
            residual_rows: Vec::with_capacity(capacity),
            tear_columns: Vec::with_capacity(capacity),
            factor: Factor::Unfactored,
            work: DVector::zeros(n),
            reduced_rhs: DVector::zeros(layout.tears().len()),
        })
    }

    /// Promote each step whose column a nonzero guard reads. Guards are
    /// checked on unconditioned coefficients: conditioning may underflow a
    /// nonzero future dependency to zero. Returns whether the promoted set
    /// differs from the one the current factor was built with.
    fn preflight_guards(&mut self, source: &DMatrix<f64>) -> bool {
        self.next_guarded.fill(false);
        for (&(row, column), &(holder, solver)) in self
            .layout
            .zero_guards()
            .iter()
            .zip(self.layout.guard_steps())
        {
            if !self.next_guarded[holder] && source[(row, column)] != 0.0 {
                self.next_guarded[solver] = true;
            }
        }
        let changed = self.next_guarded != self.guarded;
        std::mem::swap(&mut self.guarded, &mut self.next_guarded);
        changed
    }

    fn update(&mut self, source: &DMatrix<f64>, rows: &[f64], columns: &[f64]) {
        let mut changed = matches!(self.factor, Factor::Unfactored);
        changed |= self.preflight_guards(source);
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

    fn pivot(&self, row: usize, target: usize) -> Pivot {
        let mut magnitude = 0.0_f64;
        let mut pivot = 0.0;
        for (&column, &value) in self
            .layout
            .row_columns(row)
            .iter()
            .zip(self.row_values(row))
        {
            if !value.is_finite() {
                return Pivot::NonFinite;
            }
            magnitude = magnitude.max(value.abs());
            if column == target {
                pivot = value;
            }
        }
        if pivot.abs() > f64::EPSILON.sqrt() * magnitude {
            Pivot::Usable(pivot)
        } else {
            Pivot::Weak
        }
    }

    /// Make causal step `position` a tear: its coordinate gets the next
    /// recovery column as a unit row and its row joins the reduced system.
    /// `None` once the capacity is exhausted.
    fn promote(&mut self, position: usize) -> Option<()> {
        let column = self.tear_columns.len();
        if column >= self.capacity {
            return None;
        }
        let (row, target) = self.layout.causal()[position];
        self.promoted[position] = true;
        self.recovery[target * self.capacity + column] = 1.0;
        self.tear_columns.push(target);
        self.residual_rows.push(row);
        Some(())
    }

    fn initialize_tears(&mut self) -> Option<()> {
        let stride = self.capacity;
        self.recovery.fill(0.0);
        self.promoted.fill(false);
        self.residual_rows.clear();
        self.residual_rows
            .extend_from_slice(self.layout.residuals());
        self.tear_columns.clear();
        self.tear_columns.extend_from_slice(self.layout.tears());
        for (column, &target) in self.layout.tears().iter().enumerate() {
            self.recovery[target * stride + column] = 1.0;
        }
        for position in 0..self.guarded.len() {
            if self.guarded[position] {
                self.promote(position)?;
            }
        }
        Some(())
    }

    fn refactor(&mut self) -> Option<LU<f64, Dyn, Dyn>> {
        self.initialize_tears()?;
        let stride = self.capacity;
        for position in 0..self.layout.causal().len() {
            if self.promoted[position] {
                continue;
            }
            let (row, target) = self.layout.causal()[position];
            let pivot = match self.pivot(row, target) {
                Pivot::Usable(pivot) => pivot,
                Pivot::Weak => {
                    self.promote(position)?;
                    continue;
                }
                Pivot::NonFinite => return None,
            };
            for column in 0..self.tear_columns.len() {
                self.recovery[target * stride + column] =
                    self.recovery_numerator(row, target, column) / pivot;
            }
        }
        let k = self.tear_columns.len();
        let reduced = DMatrix::from_fn(k, k, |row, column| {
            let source = self.residual_rows[row];
            self.layout
                .row_columns(source)
                .iter()
                .zip(self.row_values(source))
                .map(|(&dependency, &value)| value * self.recovery[dependency * stride + column])
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
        if self.reduced_rhs.len() != k {
            self.reduced_rhs = DVector::zeros(k);
        }
        let factor = reduced.lu();
        factor.is_invertible().then_some(factor)
    }

    fn recovery_numerator(&self, row: usize, target: usize, column: usize) -> f64 {
        let mut sum = 0.0;
        for (&dependency, &value) in self
            .layout
            .row_columns(row)
            .iter()
            .zip(self.row_values(row))
        {
            if dependency != target {
                sum -= value * self.recovery[dependency * self.capacity + column];
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
        for (position, &(row, target)) in self.layout.causal().iter().enumerate() {
            if !self.promoted[position] {
                self.work[target] = self.causal_rhs(row, target, rhs[row]);
            }
        }
        for (row, &source) in self.residual_rows.iter().enumerate() {
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
        for (position, &(_, target)) in self.layout.causal().iter().enumerate() {
            if self.promoted[position] {
                continue;
            }
            let start = target * self.capacity;
            let correction: f64 = self.recovery[start..start + k]
                .iter()
                .zip(self.reduced_rhs.iter())
                .map(|(a, b)| a * b)
                .sum();
            self.work[target] += correction;
        }
        for (&target, &value) in self.tear_columns.iter().zip(self.reduced_rhs.iter()) {
            self.work[target] = value;
        }
        self.work
            .iter()
            .all(|value| value.is_finite())
            .then(|| self.work.clone())
    }
}
