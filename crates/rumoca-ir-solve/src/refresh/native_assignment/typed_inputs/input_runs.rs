//! Source-issued typed storage and checked read selections (SOLVE-C69).

use super::*;
use std::ops::Range;

/// One original external-input storage claim and its packed typed lane range.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeInputRun {
    storage_index: usize,
    role: SolveVariableStorageRole,
    p_start: usize,
    count: usize,
    lane: NativeOutputLane,
    lane_offset: usize,
    viewed: bool,
}

impl NativeInputRun {
    /// The storage role that owns the run: an external input, or a parameter
    /// or constant whose value is fixed before execution.
    #[must_use]
    pub fn role(&self) -> SolveVariableStorageRole {
        self.role
    }
    #[must_use]
    pub fn p_range(&self) -> Range<usize> {
        self.p_start..self.p_start + self.count
    }
    #[must_use]
    pub fn scalar_count(&self) -> usize {
        self.count
    }
    #[must_use]
    pub fn lane(&self) -> NativeOutputLane {
        self.lane
    }
    #[must_use]
    pub fn lane_offset(&self) -> usize {
        self.lane_offset
    }
    /// Whether any operation of the schedule may read the Real view of the
    /// run in P: a read that is not bound to the lane itself (SOLVE-C69). A run
    /// no operation reads that way needs no view written.
    #[must_use]
    pub fn view_is_read(&self) -> bool {
        self.viewed
    }
    pub(super) fn offset_at(&self, p_index: usize) -> Option<usize> {
        self.p_range()
            .contains(&p_index)
            .then(|| self.lane_offset + (p_index - self.p_start) * self.lane.width())
    }
}

/// An original validated read's arithmetic selection; sparse reads stay sparse.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeInputSelection {
    pub(super) start: usize,
    pub(super) count: usize,
    pub(super) stride: usize,
}

impl NativeInputSelection {
    #[must_use]
    pub fn start(&self) -> usize {
        self.start
    }
    #[must_use]
    pub fn count(&self) -> usize {
        self.count
    }
    #[must_use]
    pub fn stride(&self) -> usize {
        self.stride
    }
    #[must_use]
    pub fn contains(&self, index: usize) -> bool {
        let Some(offset) = index.checked_sub(self.start) else {
            return false;
        };
        self.count != 0
            && if self.stride == 0 {
                offset == 0
            } else {
                offset % self.stride == 0 && offset / self.stride < self.count
            }
    }
    fn index_box(self) -> Option<rumoca_core::IndexBox> {
        let first = i64::try_from(self.start).ok()?;
        let progression = if self.stride == 0 {
            rumoca_core::Progression::single(first)
        } else {
            let last = self
                .start
                .checked_add((self.count.checked_sub(1)?).checked_mul(self.stride)?)?;
            rumoca_core::Progression::range(
                first,
                i64::try_from(self.stride).ok()?,
                i64::try_from(last).ok()?,
            )?
        };
        Some(rumoca_core::IndexBox(vec![progression]))
    }

    pub(super) fn checked(start: usize, count: usize, stride: usize) -> Option<Self> {
        if count != 0 {
            start.checked_add((count - 1).checked_mul(stride)?)?;
        }
        Some(Self {
            start,
            count,
            stride,
        })
    }
    pub(super) fn clipped(self, bounds: Range<usize>) -> Option<Self> {
        if self.count == 0 || bounds.is_empty() {
            return None;
        }
        if self.stride == 0 {
            return bounds
                .contains(&self.start)
                .then_some(Self { count: 1, ..self });
        }
        let first = bounds
            .start
            .saturating_sub(self.start)
            .div_ceil(self.stride);
        let last = bounds.end.checked_sub(1)?.checked_sub(self.start)? / self.stride;
        let end = last.checked_add(1)?.min(self.count);
        if first >= end {
            return None;
        }
        Some(Self {
            start: self.start + first * self.stride,
            count: end - first,
            stride: self.stride,
        })
    }
}

/// One source read and its bound sinks. Several reads of one P slot remain
/// independent obligations; an exclusion belongs only to this source read.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeInputCheck {
    pub(super) selected: NativeInputSelection,
    pub(super) bound: Vec<NativeInputSelection>,
}
impl NativeInputCheck {
    #[must_use]
    pub fn selected(&self) -> NativeInputSelection {
        self.selected
    }
    #[must_use]
    pub fn bound(&self) -> &[NativeInputSelection] {
        &self.bound
    }
    /// Project this original read and its exclusions onto one exact leaf span.
    /// Failed set containment retains the check; it cannot grant read authority.
    #[must_use]
    pub fn in_range(&self, bounds: Range<usize>) -> Option<Self> {
        let selected = self.selected.clipped(bounds.clone())?;
        let bound = self
            .bound
            .iter()
            .filter_map(|part| part.clipped(bounds.clone()))
            .collect::<Vec<_>>();
        let boxes = bound
            .iter()
            .map(|part| part.index_box())
            .collect::<Option<Vec<_>>>();
        if let (Some(selected_box), Some(boxes)) = (selected.index_box(), boxes)
            && rumoca_core::IndexUnion::of(boxes).contains_box(&selected_box)
        {
            return None;
        }
        Some(Self { selected, bound })
    }

    #[must_use]
    pub fn contains(&self, p_index: usize) -> bool {
        self.selected.contains(p_index) && !self.bound.iter().any(|range| range.contains(p_index))
    }
}

/// P cells an initialization row assigns or solves for. Their value is not
/// fixed at construction, so they are never typed lanes.
fn initialization_owned_cells(problem: &SolveProblem) -> std::collections::BTreeSet<usize> {
    problem
        .initialization
        .update_targets()
        .iter()
        .chain(problem.initialization.projection_unknowns())
        .filter_map(|slot| match slot {
            crate::ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect()
}

/// Canonical input storage and source-read obligations. No scalar cache.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(in crate::refresh::native_assignment) struct NativeInputInventory {
    pub(in crate::refresh::native_assignment) runs: Vec<NativeInputRun>,
    pub(in crate::refresh::native_assignment) checks: Vec<NativeInputCheck>,
    /// Unbound reads of Boolean cells, which need no Integer check.
    pub(in crate::refresh::native_assignment) reads: Vec<NativeInputCheck>,
    pub(in crate::refresh::native_assignment) lane_bytes: usize,
}

impl NativeInputInventory {
    pub(in crate::refresh::native_assignment) fn classify(
        problem: &SolveProblem,
    ) -> Result<Self, NativeEvaluationRefusal> {
        let initialized = initialization_owned_cells(problem);
        let mut runs = Vec::new();
        for (storage_index, storage) in problem
            .solve_layout
            .variable_storage_runs
            .iter()
            .enumerate()
        {
            if !matches!(
                storage.role,
                SolveVariableStorageRole::ExternalInput
                    | SolveVariableStorageRole::Parameter
                    | SolveVariableStorageRole::Constant
            ) {
                continue;
            }
            let lane = match storage.value_kind {
                SolveVariableValueKind::Integer => NativeOutputLane::Integer,
                SolveVariableValueKind::Boolean => NativeOutputLane::Boolean,
                _ => continue,
            };
            let crate::ScalarSlot::P { index, .. } = storage.base else {
                return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
            };
            let end = index
                .checked_add(storage.scalar_count)
                .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
            if end > problem.layout.p_scalars() {
                return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
            }
            if storage.role != SolveVariableStorageRole::ExternalInput
                && (index..end).any(|cell| initialized.contains(&cell))
            {
                continue;
            }
            if storage.scalar_count != 0 {
                runs.push(NativeInputRun {
                    storage_index,
                    role: storage.role,
                    p_start: index,
                    count: storage.scalar_count,
                    lane,
                    lane_offset: 0,
                    viewed: true,
                });
            }
        }
        runs.sort_unstable_by_key(|run| run.p_start);
        if runs
            .windows(2)
            .any(|pair| pair[0].p_range().end > pair[1].p_start)
        {
            return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
        }
        let mut bytes = 0usize;
        for wide in [true, false] {
            for run in runs
                .iter_mut()
                .filter(|run| (run.lane.width() == 8) == wide)
            {
                run.lane_offset = bytes;
                bytes = run
                    .count
                    .checked_mul(run.lane.width())
                    .and_then(|count| bytes.checked_add(count))
                    .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
            }
        }
        let lane_bytes = bytes
            .checked_add(7)
            .map(|bytes| bytes / 8 * 8)
            .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
        Ok(Self {
            runs,
            checks: Vec::new(),
            reads: Vec::new(),
            lane_bytes,
        })
    }
    /// Whether `selected` reads a cell of a run of `lane`.
    pub(in crate::refresh::native_assignment) fn selects_lane(
        &self,
        lane: NativeOutputLane,
        selected: NativeInputSelection,
    ) -> bool {
        self.runs
            .iter()
            .any(|run| run.lane() == lane && selected.clipped(run.p_range()).is_some())
    }
    /// Settle which runs have a Real view some operation reads, from the reads
    /// every stage was analysed for.
    pub(in crate::refresh::native_assignment) fn settle_views(&mut self) {
        let (checks, reads) = (&self.checks, &self.reads);
        for run in &mut self.runs {
            let range = run.p_range();
            run.viewed = checks
                .iter()
                .chain(reads)
                .any(|read| read.in_range(range.clone()).is_some());
        }
    }
    pub(in crate::refresh::native_assignment) fn run_at(
        &self,
        p_index: usize,
    ) -> Option<&NativeInputRun> {
        let ordinal = self
            .runs
            .partition_point(|run| run.p_start <= p_index)
            .checked_sub(1)?;
        self.runs
            .get(ordinal)
            .filter(|run| run.p_range().contains(&p_index))
    }
    pub(in crate::refresh::native_assignment) fn integer_lane(
        &self,
        p_index: usize,
    ) -> Option<usize> {
        let run = self.run_at(p_index)?;
        (run.lane == NativeOutputLane::Integer)
            .then(|| run.offset_at(p_index))
            .flatten()
    }
    pub(in crate::refresh::native_assignment) fn lane_at(
        &self,
        p_index: usize,
    ) -> Option<NativeInputLane> {
        let run = self.run_at(p_index)?;
        Some(NativeInputLane {
            p_index,
            lane: run.lane,
            lane_offset: run.offset_at(p_index)?,
            checked: run.lane == NativeOutputLane::Integer
                && self.checks.iter().any(|check| check.contains(p_index)),
        })
    }
}

#[cfg(test)]
mod projection_tests {
    use super::*;

    fn selection(start: usize, count: usize, stride: usize) -> NativeInputSelection {
        NativeInputSelection::checked(start, count, stride).unwrap()
    }

    #[test]
    fn leaf_projection_preserves_original_obligations_and_exclusions() {
        let original = NativeInputCheck {
            selected: selection(2, 6, 2),
            bound: vec![selection(4, 2, 4)],
        };
        assert!(original.in_range(13..20).is_none());
        assert!(original.in_range(4..5).is_none());
        let projected = original.in_range(3..11).unwrap();
        for cell in 0..16 {
            assert_eq!(
                projected.contains(cell),
                (3..11).contains(&cell) && original.contains(cell)
            );
        }
        assert_eq!(projected.selected(), selection(4, 4, 2));
        assert_eq!(projected.bound(), [selection(4, 2, 4)]);
        assert!(original.in_range(5..5).is_none());
    }

    #[test]
    fn complete_bound_union_can_remove_only_the_selected_read() {
        let original = NativeInputCheck {
            selected: selection(0, 8, 1),
            bound: vec![selection(0, 4, 2), selection(1, 4, 2)],
        };
        assert!(original.in_range(0..8).is_none());
        let partial = NativeInputCheck {
            selected: original.selected,
            bound: vec![selection(0, 4, 2)],
        };
        let projected = partial.in_range(0..8).unwrap();
        assert!(!projected.contains(6));
        assert!(projected.contains(7));
        let repeated = NativeInputCheck {
            selected: selection(usize::MAX, 1, 0),
            bound: vec![],
        };
        assert!(repeated.in_range(0..usize::MAX).is_none());
        let extreme = NativeInputCheck {
            selected: selection(usize::MAX - 1, 1, 1),
            bound: vec![],
        };
        assert!(
            extreme
                .in_range(usize::MAX - 1..usize::MAX)
                .unwrap()
                .contains(usize::MAX - 1)
        );
        assert!(NativeInputSelection::checked(usize::MAX, 2, 1).is_none());
    }
}
