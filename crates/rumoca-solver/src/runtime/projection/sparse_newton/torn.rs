use nalgebra::{DMatrix, DVector, Dyn, LU};
use rumoca_ir_solve as solve;

use super::super::jacobian_values::{JacobianMatrix, JacobianReadMap, JacobianReads};
use super::valid_variable_scale;

#[derive(Clone, Default)]
pub(super) struct TornNewtonCache {
    system: Option<TornSystem>,
    // Storage only: every invocation clears and completely rechecks it.
    pivot_storage: Vec<f64>,
    // The construction-issued guarded and primary attempts share one numeric
    // factor, but each retains its own exact prepared source coordinates.
    reads: [Option<TornReads>; 2],
}

impl TornNewtonCache {
    pub(super) fn solve_scaled(
        &mut self,
        source: &dyn JacobianMatrix,
        rhs: &DVector<f64>,
        row_scales: &[f64],
        variable_scales: &[f64],
        candidate: solve::TearingCandidate,
        layout: &solve::AffineEliminationLayout,
    ) -> Option<DVector<f64>> {
        if rhs.len() != layout.pattern().rows() as usize {
            return None;
        }
        let slot = match candidate {
            solve::TearingCandidate::Guarded => 0,
            solve::TearingCandidate::Primary => 1,
        };
        if self.reads[slot]
            .as_ref()
            .is_none_or(|reads| reads.layout != *layout || !reads.map.matches(source))
        {
            self.reads[slot] = Some(TornReads::prepare(source, layout)?);
        }
        let reads = self.reads[slot].as_ref()?;
        let checked = CheckedPivots::new(
            source,
            reads,
            row_scales,
            variable_scales,
            layout,
            &mut self.pivot_storage,
        )?;
        checked.bind(&mut self.system)?.update();
        self.system.as_mut()?.solve(rhs)
    }
}

#[derive(Clone)]
struct TornReads {
    layout: solve::AffineEliminationLayout,
    map: JacobianReadMap,
    row_offsets: Vec<usize>,
}
impl TornReads {
    fn prepare(
        source: &dyn JacobianMatrix,
        layout: &solve::AffineEliminationLayout,
    ) -> Option<Self> {
        if source
            .value_layout()
            .is_some_and(|owner| owner.pattern() != layout.pattern())
        {
            return None;
        }
        let mut coordinates = layout.zero_guards().to_vec();
        let mut row_offsets = Vec::new();
        for row in 0..layout.pattern().rows() as usize {
            row_offsets.push(coordinates.len());
            coordinates.extend(layout.row_columns(row).iter().map(|&column| (row, column)));
        }
        Some(Self {
            layout: layout.clone(),
            map: JacobianReadMap::prepare(source, coordinates)?,
            row_offsets,
        })
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

    fn row_values(&self, row: usize) -> &[f64] {
        &self.values[self.offsets[row]..self.offsets[row + 1]]
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

/// Complete fresh preflight, borrowing the exact inputs until update/refactor ends.
/// Construction never exposes a partial buffer as evidence.
struct CheckedPivots<'a> {
    source: JacobianReads<'a>,
    row_offsets: &'a [usize],
    row_scales: &'a [f64],
    variable_scales: &'a [f64],
    layout: &'a solve::AffineEliminationLayout,
    pivots: &'a [f64],
}

impl<'a> CheckedPivots<'a> {
    fn new(
        source: &'a dyn JacobianMatrix,
        reads: &'a TornReads,
        row_scales: &'a [f64],
        variable_scales: &'a [f64],
        layout: &'a solve::AffineEliminationLayout,
        storage: &'a mut Vec<f64>,
    ) -> Option<Self> {
        storage.clear();
        let n = layout.pattern().rows() as usize;
        if source.shape() != (n, n) || row_scales.len() != n || variable_scales.len() != n {
            return None;
        }
        let source = reads.map.bind(source)?;
        // Raw guards and all pivots precede any usable layout/factor mutation.
        if (0..layout.zero_guards().len()).any(|index| source.at(index) != 0.0) {
            return None;
        }
        storage.reserve(layout.causal().len());
        for &(row, target) in layout.causal() {
            storage.push(checked_pivot(
                layout
                    .row_columns(row)
                    .iter()
                    .enumerate()
                    .map(|(offset, &column)| {
                        (
                            column,
                            source.at(reads.row_offsets[row] + offset)
                                * valid_variable_scale(variable_scales[column])
                                / valid_variable_scale(row_scales[row]),
                        )
                    }),
                target,
            )?);
        }
        Some(Self {
            source,
            row_offsets: &reads.row_offsets,
            row_scales,
            variable_scales,
            layout,
            pivots: storage.as_slice(),
        })
    }

    fn bind(self, cache: &mut Option<TornSystem>) -> Option<CheckedUpdate<'_, 'a>> {
        if cache
            .as_ref()
            .is_none_or(|system| system.layout != *self.layout)
        {
            *cache = TornSystem::new(self.layout);
        }
        Some(CheckedUpdate {
            system: cache.as_mut()?,
            checked: self,
        })
    }
}

/// Only binding a complete invocation to its matching cache constructs this owner.
struct CheckedUpdate<'s, 'a> {
    system: &'s mut TornSystem,
    checked: CheckedPivots<'a>,
}

impl CheckedUpdate<'_, '_> {
    fn update(mut self) {
        let source = self.checked.source;
        let rows = self.checked.row_scales;
        let columns = self.checked.variable_scales;
        let mut changed = matches!(self.system.factor, Factor::Unfactored);
        for (row, &row_scale) in rows.iter().enumerate() {
            let values =
                &mut self.system.values[self.system.offsets[row]..self.system.offsets[row + 1]];
            for (offset, (value, &column)) in values
                .iter_mut()
                .zip(self.system.layout.row_columns(row))
                .enumerate()
            {
                let next = source.at(self.checked.row_offsets[row] + offset)
                    * valid_variable_scale(columns[column])
                    / valid_variable_scale(row_scale);
                changed |= value.to_bits() != next.to_bits();
                *value = next;
            }
        }
        if changed {
            // Revoke the previous factor before changing its recovery relation.
            self.system.factor = Factor::Rejected;
            if let Some(factor) = self.refactor() {
                self.system.factor = Factor::Ready(factor);
            }
        }
    }

    fn refactor(&mut self) -> Option<LU<f64, Dyn, Dyn>> {
        let system = &mut self.system;
        let k = system.layout.tears().len();
        system.recovery.fill(0.0);
        for (column, &target) in system.layout.tears().iter().enumerate() {
            system.recovery[target * k + column] = 1.0;
        }
        for (index, &(row, target)) in system.layout.causal().iter().enumerate() {
            let pivot = self.checked.pivots[index];
            let values = &system.values[system.offsets[row]..system.offsets[row + 1]];
            recover_causal_row(
                system.layout.row_columns(row),
                values,
                &mut system.recovery,
                target,
                pivot,
                k,
            );
        }
        let reduced = DMatrix::from_fn(k, k, |row, column| {
            let source = system.layout.residuals()[row];
            system
                .layout
                .row_columns(source)
                .iter()
                .zip(system.row_values(source))
                .map(|(&dependency, &value)| value * system.recovery[dependency * k + column])
                .sum()
        });
        if !system
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
}

/// Traverse immutable row metadata once while retaining each tear column's
/// original dependency accumulation and final division order.
fn recover_causal_row(
    columns: &[usize],
    values: &[f64],
    recovery: &mut [f64],
    target: usize,
    pivot: f64,
    tears: usize,
) {
    // The issued partition has distinct causal targets. `recovery.fill(0.0)`
    // therefore gives every target cell the former numerator's +0.0 start.
    let target_start = target * tears;
    for (&dependency, &value) in columns.iter().zip(values) {
        #[cfg(test)]
        RECOVERY_DEPENDENCY_SCANS.with(|count| count.set(count.get() + 1));
        if dependency != target {
            let dependency_start = dependency * tears;
            for column in 0..tears {
                let operand = recovery[dependency_start + column];
                recovery[target_start + column] -= value * operand;
            }
        }
    }
    for column in 0..tears {
        recovery[target_start + column] /= pivot;
    }
}

/// Shared numerical predicate for preflight and the recovery recurrence.
fn checked_pivot(values: impl Iterator<Item = (usize, f64)>, target: usize) -> Option<f64> {
    #[cfg(test)]
    PIVOT_CHECKS.with(|count| count.set(count.get() + 1));
    let mut magnitude = 0.0_f64;
    let mut pivot = 0.0;
    for (column, value) in values {
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

#[cfg(test)]
thread_local! { static PIVOT_CHECKS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) }; }

#[cfg(test)]
thread_local! { static RECOVERY_DEPENDENCY_SCANS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) }; }

#[cfg(test)]
pub(in crate::runtime::projection) fn pivot_check_count() -> usize {
    PIVOT_CHECKS.with(std::cell::Cell::get)
}

#[cfg(test)]
pub(in crate::runtime::projection) fn recovery_dependency_scan_count() -> usize {
    RECOVERY_DEPENDENCY_SCANS.with(std::cell::Cell::get)
}

#[cfg(test)]
mod recovery_tests {
    use super::*;

    fn previous_column_order(
        columns: &[usize],
        values: &[f64],
        recovery: &mut [f64],
        target: usize,
        pivot: f64,
        tears: usize,
    ) {
        for column in 0..tears {
            let sum = previous_numerator(columns, values, recovery, target, column, tears);
            recovery[target * tears + column] = sum / pivot;
        }
    }

    fn previous_numerator(
        columns: &[usize],
        values: &[f64],
        recovery: &[f64],
        target: usize,
        column: usize,
        tears: usize,
    ) -> f64 {
        let mut sum = 0.0;
        for (&dependency, &value) in columns.iter().zip(values) {
            if dependency != target {
                sum -= value * recovery[dependency * tears + column];
            }
        }
        sum
    }

    fn assert_same_recovery_bits(
        columns: &[usize],
        values: &[f64],
        initial: &[f64],
        target: usize,
        pivot: f64,
        tears: usize,
    ) {
        let mut previous = initial.to_vec();
        let mut current = initial.to_vec();
        previous_column_order(columns, values, &mut previous, target, pivot, tears);
        recover_causal_row(columns, values, &mut current, target, pivot, tears);
        for (&actual, &expected) in current.iter().zip(&previous) {
            assert!(
                (actual.is_nan() && expected.is_nan()) || actual.to_bits() == expected.to_bits(),
                "recovery operation changed: actual {actual:?}, expected {expected:?}"
            );
        }
    }

    #[test]
    fn recovery_interchange_keeps_each_scalar_operation_order() {
        let initial = [
            1.0, -0.0, 0.0, // first dependency
            0.125, -1.0, 3.5, // second dependency
            0.0, 0.0, 0.0, // causal target starts at +0.0
            2.0, -0.25, 1.0, // later dependency
        ];
        for values in [
            [0.75, -1.25, -2.0, 2.5],
            [-0.0, f64::from_bits(1.0_f64.to_bits() + 1), -2.0, 1e-300],
        ] {
            assert_same_recovery_bits(&[0, 1, 2, 3], &values, &initial, 2, -2.0, 3);
        }
        assert_same_recovery_bits(&[2], &[-1.0], &[0.0; 6], 2, -1.0, 2);
        let mut nonfinite = [0.0; 6];
        nonfinite[0] = f64::INFINITY;
        assert_same_recovery_bits(&[0, 2], &[0.0, -1.0], &nonfinite, 2, -1.0, 2);
        let threshold = f64::EPSILON.sqrt();
        assert!(checked_pivot([(0, 1.0), (2, threshold)].into_iter(), 2).is_none());
        let accepted = f64::from_bits(threshold.to_bits() + 1);
        assert_eq!(
            checked_pivot([(0, 1.0), (2, accepted)].into_iter(), 2),
            Some(accepted)
        );
        assert_same_recovery_bits(
            &[0, 2],
            &[1.0, accepted],
            &[1.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            2,
            accepted,
            2,
        );
    }
}
