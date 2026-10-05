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
        source: &super::super::BlockJacobian,
        rhs: &DVector<f64>,
        (row_scales, variable_scales): (&[f64], &[f64]),
        layout: &solve::AffineEliminationLayout,
        revision: Option<u64>,
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
        system.update(source, (row_scales, variable_scales), revision);
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
    /// The unscaled source entry each of `values` was conditioned from, and
    /// the row and variable scales it was conditioned with, so an update
    /// recomputes only the entries whose inputs changed: an entry is a pure
    /// function of its source value and its two scales.
    sources: Box<[f64]>,
    row_scales: Box<[f64]>,
    variable_scales: Box<[f64]>,
    /// Per variable, whether its scale differs from the one held.
    rescaled: Box<[bool]>,
    /// The compact layout of the block pattern, whose slots are this system's
    /// entries in order: both list each row's pattern columns ascending.
    compact: rumoca_ir_solve::CompactPatternLayout,
    /// The last source layout found equal to `compact`, so a source stored in
    /// it is read by slot without comparing layouts again.
    matched: Option<std::sync::Arc<rumoca_ir_solve::CompactPatternLayout>>,
    /// Whether `values` holds a conditioning of held inputs at all; until the
    /// first update every entry is recomputed.
    conditioned: bool,
    /// The revision of the inputs `values` and the factor were last formed
    /// from: an update under the same revision holds bitwise those inputs.
    revision: Option<u64>,
    recovery: Box<[f64]>,
    /// For every block coordinate, the recovery columns its row can hold
    /// nonzero, ascending: a tear's own column, and for a causal target the
    /// union of its dependencies' columns. Every other entry of the row is an
    /// exact zero that is neither stored nor read.
    support: Vec<Vec<usize>>,
    /// Per recovery column, the recovery that last added it to a support
    /// union, numbered by `unions` (zero is never).
    union_marks: Box<[u64]>,
    /// Recoveries computed over this cache's lifetime, so a union mark is
    /// never mistaken for one left by an earlier factorization.
    unions: u64,
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
    /// One recovery accumulator per tear column.
    accumulator: Box<[f64]>,
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
            sources: vec![0.0; offsets[n]].into_boxed_slice(),
            row_scales: vec![0.0; n].into_boxed_slice(),
            variable_scales: vec![0.0; n].into_boxed_slice(),
            rescaled: vec![true; n].into_boxed_slice(),
            compact: rumoca_ir_solve::CompactPatternLayout::of(layout.pattern()),
            matched: None,
            conditioned: false,
            revision: None,
            offsets: offsets.into_boxed_slice(),
            recovery: vec![0.0; n.checked_mul(capacity)?].into_boxed_slice(),
            support: vec![Vec::new(); n],
            union_marks: vec![0; capacity].into_boxed_slice(),
            unions: 0,
            guarded: vec![false; steps].into_boxed_slice(),
            next_guarded: vec![false; steps].into_boxed_slice(),
            promoted: vec![false; steps].into_boxed_slice(),
            residual_rows: Vec::with_capacity(capacity),
            tear_columns: Vec::with_capacity(capacity),
            factor: Factor::Unfactored,
            accumulator: vec![0.0; capacity].into_boxed_slice(),
            work: DVector::zeros(n),
            reduced_rhs: DVector::zeros(layout.tears().len()),
        })
    }

    /// Promote each step whose column a nonzero guard reads. Guards are
    /// checked on unconditioned coefficients: conditioning may underflow a
    /// nonzero future dependency to zero. Returns whether the promoted set
    /// differs from the one the current factor was built with.
    fn preflight_guards(&mut self, source: &super::super::BlockJacobian) -> bool {
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

    fn update(
        &mut self,
        source: &super::super::BlockJacobian,
        (rows, columns): (&[f64], &[f64]),
        revision: Option<u64>,
    ) {
        if revision.is_some() && revision == self.revision {
            return;
        }
        self.revision = revision;
        let mut changed = matches!(self.factor, Factor::Unfactored);
        changed |= self.preflight_guards(source);
        let fresh = !self.conditioned;
        self.conditioned = true;
        for ((&scale, held), rescaled) in columns
            .iter()
            .zip(self.variable_scales.iter_mut())
            .zip(self.rescaled.iter_mut())
        {
            *rescaled = fresh || scale.to_bits() != held.to_bits();
            *held = scale;
        }
        let compact = source
            .compact_storage()
            .filter(|(layout, _)| self.matches(layout))
            .map(|(_, values)| values);
        for (row, &row_scale) in rows.iter().enumerate() {
            let row_scaled = fresh || row_scale.to_bits() != self.row_scales[row].to_bits();
            self.row_scales[row] = row_scale;
            let read = SourceRow { source, compact };
            changed |= self.condition_row(read, (row, row_scale, row_scaled), columns);
        }
        if changed {
            // Revoke the previous factor before changing its recovery relation.
            self.factor = Factor::Rejected;
            if let Some(factor) = self.refactor() {
                self.factor = Factor::Ready(factor);
            }
        }
    }

    /// Recondition the entries of `row` whose source value or scales changed;
    /// whether any conditioned value changed.
    fn condition_row(
        &mut self,
        read: SourceRow<'_>,
        (row, row_scale, row_scaled): (usize, f64, bool),
        columns: &[f64],
    ) -> bool {
        let mut changed = false;
        let entries = self.offsets[row]..self.offsets[row + 1];
        for (((value, held), &column), slot) in self.values[entries.clone()]
            .iter_mut()
            .zip(&mut self.sources[entries.clone()])
            .zip(self.layout.row_columns(row))
            .zip(entries)
        {
            let raw = read.value(row, column, slot);
            if row_scaled || self.rescaled[column] || raw.to_bits() != held.to_bits() {
                *held = raw;
                let next =
                    raw * valid_variable_scale(columns[column]) / valid_variable_scale(row_scale);
                changed |= value.to_bits() != next.to_bits();
                *value = next;
            }
        }
        changed
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
        self.set_unit(target, column);
        self.tear_columns.push(target);
        self.residual_rows.push(row);
        Some(())
    }

    fn initialize_tears(&mut self) -> Option<()> {
        self.promoted.fill(false);
        for support in &mut self.support {
            support.clear();
        }
        self.residual_rows.clear();
        self.residual_rows
            .extend_from_slice(self.layout.residuals());
        self.tear_columns.clear();
        self.tear_columns.extend_from_slice(self.layout.tears());
        for column in 0..self.layout.tears().len() {
            let target = self.layout.tears()[column];
            self.set_unit(target, column);
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
            self.recover((row, target), pivot);
        }
        let k = self.tear_columns.len();
        let mut reduced = DMatrix::from_element(k, k, -0.0);
        for (row, &source) in self.residual_rows.iter().enumerate() {
            for (&dependency, &value) in self
                .layout
                .row_columns(source)
                .iter()
                .zip(self.row_values(source))
            {
                self.add_recovered(&mut reduced, row, (dependency, value));
            }
        }
        let recovered_finite = self
            .support
            .iter()
            .enumerate()
            .all(|(coordinate, columns)| {
                columns
                    .iter()
                    .all(|&column| self.recovery[coordinate * stride + column].is_finite())
            });
        if !recovered_finite || !reduced.iter().all(|value| value.is_finite()) {
            return None;
        }
        if self.reduced_rhs.len() != k {
            self.reduced_rhs = DVector::zeros(k);
        }
        let factor = reduced.lu();
        factor.is_invertible().then_some(factor)
    }

    /// The recovery row of causal target `target` from its row `row`: for
    /// every column its dependencies can hold, minus the row's other entries
    /// against their recovery rows, over the pivot. Each column accumulates
    /// the entries in pattern order; a dependency without the column holds an
    /// exact zero there, whose product leaves the (never negative-zero) sum
    /// unchanged, so skipping it is exact.
    fn recover(&mut self, (row, target): (usize, usize), pivot: f64) {
        let stride = self.capacity;
        self.unions += 1;
        let mark = self.unions;
        let mut union = std::mem::take(&mut self.support[target]);
        union.clear();
        for index in 0..self.layout.row_columns(row).len() {
            let dependency = self.layout.row_columns(row)[index];
            if dependency != target {
                self.open_columns(dependency, mark, &mut union);
            }
        }
        union.sort_unstable();
        let entries = self.offsets[row]..self.offsets[row + 1];
        for (&dependency, &value) in self
            .layout
            .row_columns(row)
            .iter()
            .zip(&self.values[entries])
        {
            if dependency == target {
                continue;
            }
            for &column in &self.support[dependency] {
                self.accumulator[column] -= value * self.recovery[dependency * stride + column];
            }
        }
        for &column in &union {
            self.recovery[target * stride + column] = self.accumulator[column] / pivot;
        }
        self.support[target] = union;
    }

    /// Add `dependency`'s recovery columns not yet in the union marked `mark`,
    /// starting their accumulators at zero.
    fn open_columns(&mut self, dependency: usize, mark: u64, union: &mut Vec<usize>) {
        for &column in &self.support[dependency] {
            if self.union_marks[column] != mark {
                self.union_marks[column] = mark;
                union.push(column);
                self.accumulator[column] = 0.0;
            }
        }
    }

    /// Add `value` times `dependency`'s recovery row to row `row` of `reduced`.
    fn add_recovered(
        &self,
        reduced: &mut DMatrix<f64>,
        row: usize,
        (dependency, value): (usize, f64),
    ) {
        let stride = self.capacity;
        for &column in &self.support[dependency] {
            reduced[(row, column)] += value * self.recovery[dependency * stride + column];
        }
    }

    /// Make `target`'s recovery row the unit row of `column`.
    fn set_unit(&mut self, target: usize, column: usize) {
        self.recovery[target * self.capacity + column] = 1.0;
        let support = &mut self.support[target];
        support.clear();
        support.push(column);
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
        for (position, &(_, target)) in self.layout.causal().iter().enumerate() {
            if self.promoted[position] {
                continue;
            }
            let start = target * self.capacity;
            let correction: f64 = self.support[target]
                .iter()
                .map(|&column| self.recovery[start + column] * self.reduced_rhs[column])
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

/// Where a torn update reads source entries: by slot from compact storage in
/// the system's own layout, else by coordinate.
#[derive(Clone, Copy)]
struct SourceRow<'a> {
    source: &'a super::super::BlockJacobian,
    compact: Option<&'a [f64]>,
}

impl SourceRow<'_> {
    fn value(self, row: usize, column: usize, slot: usize) -> f64 {
        match self.compact {
            Some(values) => values[slot],
            None => self.source[(row, column)],
        }
    }
}

impl TornSystem {
    /// Whether `layout` is this system's compact layout.
    fn matches(&mut self, layout: &std::sync::Arc<rumoca_ir_solve::CompactPatternLayout>) -> bool {
        if self
            .matched
            .as_ref()
            .is_some_and(|matched| std::sync::Arc::ptr_eq(matched, layout))
        {
            return true;
        }
        let equal = **layout == self.compact;
        if equal {
            self.matched = Some(std::sync::Arc::clone(layout));
        }
        equal
    }
}
