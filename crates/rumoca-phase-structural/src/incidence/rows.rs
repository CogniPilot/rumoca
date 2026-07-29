//! Compact CSR storage for the scalar compatibility graph.
//!
//! Structural matching, Tarjan and BLT used to consume `Vec<HashSet<usize>>`:
//! one hash allocation per scalar equation, plus a fresh `collect()` +
//! `sort_unstable()` on every visit because hash iteration order must never
//! leak into structural choices (SPEC_0021 §"Deterministic Public Collections":
//! a hash collection may back a phase-internal lookup only when its iteration
//! cannot reach the output). [`IncidenceRows`]
//! stores the same relation as a compressed sparse row matrix whose runs are
//! kept sorted and deduplicated at construction, so every consumer reads an
//! already-canonical `&[usize]` with no allocation and no re-sort.
//!
//! The sorted invariant is what makes the migration order-preserving: the slice
//! a consumer sees is bit-for-bit the sequence the old `sort_unstable()`
//! produced, so matching decisions -- and therefore MSL trace baselines --
//! are unchanged.

use std::collections::HashSet;

#[cfg(test)]
use crate::types::StructuralError;

/// Scalar incidence stored as compressed sparse rows.
///
/// Invariants (established by [`IncidenceRowsBuilder`] and every constructor):
/// - `offsets.len() == rows + 1`, `offsets` is non-decreasing and starts at 0;
/// - each run `columns[offsets[r]..offsets[r + 1]]` is sorted ascending and
///   contains no duplicates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncidenceRows {
    offsets: Vec<usize>,
    columns: Vec<usize>,
}

impl Default for IncidenceRows {
    fn default() -> Self {
        Self {
            offsets: vec![0],
            columns: Vec::new(),
        }
    }
}

impl IncidenceRows {
    /// Number of equation rows.
    #[must_use]
    pub fn len(&self) -> usize {
        self.offsets.len().saturating_sub(1)
    }

    /// Whether the store holds no rows at all.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The sorted, deduplicated unknown indices referenced by `equation`.
    ///
    /// Out-of-range rows read as the empty relation, which is the same answer
    /// the old `eq_unknowns.get(eq)` callers folded to.
    #[must_use]
    pub fn row(&self, equation: usize) -> &[usize] {
        let Some(&start) = self.offsets.get(equation) else {
            return &[];
        };
        let Some(&end) = self.offsets.get(equation.saturating_add(1)) else {
            return &[];
        };
        self.columns.get(start..end).unwrap_or(&[])
    }

    /// The row of `equation`, or `None` when `equation` is out of range.
    #[must_use]
    pub fn get(&self, equation: usize) -> Option<&[usize]> {
        (equation < self.len()).then(|| self.row(equation))
    }

    /// Whether `equation` references `variable`.
    ///
    /// `O(log n)` on the sorted run, replacing the old `HashSet::contains`.
    #[must_use]
    pub fn contains(&self, equation: usize, variable: usize) -> bool {
        self.row(equation).binary_search(&variable).is_ok()
    }

    /// Total number of stored `(equation, variable)` pairs.
    #[must_use]
    pub fn entry_count(&self) -> usize {
        self.columns.len()
    }

    /// Iterate the rows in equation order.
    #[must_use]
    pub fn iter(&self) -> IncidenceRowIter<'_> {
        IncidenceRowIter {
            rows: self,
            next: 0,
            end: self.len(),
        }
    }

    /// Build from the historical `Vec<HashSet<usize>>` representation.
    ///
    /// Retained so the IC-projection and Solve-projection builders, which
    /// assemble rows incrementally from unordered sources, keep working while
    /// they migrate.
    #[must_use]
    pub fn from_sets(rows: Vec<HashSet<usize>>) -> Self {
        let mut builder = IncidenceRowsBuilder::with_row_capacity(rows.len());
        for row in rows {
            builder.push_unsorted(row);
        }
        builder.finish()
    }

    /// Build from rows that are already in ascending order without duplicates.
    ///
    /// Test-only, and deliberately so: the ascending, duplicate-free
    /// precondition is checked only by `debug_assert!` in
    /// [`IncidenceRowsBuilder::push_sorted`], while [`IncidenceRows::contains`]
    /// answers with `binary_search`. Exposing it would let a caller hand in an
    /// unsorted row and get a silently wrong `contains` in release builds —
    /// where the `debug_assert!` is compiled out. Production callers use
    /// [`IncidenceRows::from_sets`], which sorts, or the builder's affine
    /// translation path, which restores canonical order.
    #[cfg(test)]
    #[must_use]
    pub(crate) fn from_sorted_rows<I>(rows: I) -> Self
    where
        I: IntoIterator<Item = Vec<usize>>,
    {
        let mut builder = IncidenceRowsBuilder::default();
        for row in rows {
            builder.push_sorted(&row);
        }
        builder.finish()
    }
}

/// Row iterator over [`IncidenceRows`].
#[derive(Debug, Clone)]
pub struct IncidenceRowIter<'a> {
    rows: &'a IncidenceRows,
    next: usize,
    end: usize,
}

impl<'a> Iterator for IncidenceRowIter<'a> {
    type Item = &'a [usize];

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.end {
            return None;
        }
        let row = self.rows.row(self.next);
        self.next += 1;
        Some(row)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.end.saturating_sub(self.next);
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for IncidenceRowIter<'_> {}

impl<'a> IntoIterator for &'a IncidenceRows {
    type Item = &'a [usize];
    type IntoIter = IncidenceRowIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Position in a partially built [`IncidenceRows`], for test coverage of
/// transactional family emission.
#[derive(Debug, Clone, Copy)]
#[cfg(test)]
pub(crate) struct RowCheckpoint {
    rows: usize,
    columns: usize,
}

/// Incremental builder that keeps the CSR invariants.
///
/// Crate-private test constructors have a sorted insertion path that trusts
/// their caller for the ascending, duplicate-free invariant that
/// [`IncidenceRows::contains`] then relies on.
#[derive(Debug)]
pub(crate) struct IncidenceRowsBuilder {
    offsets: Vec<usize>,
    columns: Vec<usize>,
    scratch: Vec<usize>,
}

impl Default for IncidenceRowsBuilder {
    fn default() -> Self {
        Self {
            offsets: vec![0],
            columns: Vec::new(),
            scratch: Vec::new(),
        }
    }
}

impl IncidenceRowsBuilder {
    /// Pre-size the offset vector for `rows` equations.
    #[must_use]
    pub(crate) fn with_row_capacity(rows: usize) -> Self {
        let mut offsets = Vec::with_capacity(rows.saturating_add(1));
        offsets.push(0);
        Self {
            offsets,
            columns: Vec::new(),
            scratch: Vec::new(),
        }
    }

    /// Number of rows pushed so far; also the equation index the next push
    /// will occupy.
    #[must_use]
    pub(crate) fn row_count(&self) -> usize {
        self.offsets.len().saturating_sub(1)
    }

    /// Read one completed row while later rows are still being assembled.
    pub(crate) fn row(&self, equation: usize) -> Option<&[usize]> {
        let start = *self.offsets.get(equation)?;
        let end = *self.offsets.get(equation.checked_add(1)?)?;
        self.columns.get(start..end)
    }

    /// Append a row from an unordered source, sorting and deduplicating it.
    pub(crate) fn push_unsorted<I>(&mut self, columns: I)
    where
        I: IntoIterator<Item = usize>,
    {
        let mut scratch = std::mem::take(&mut self.scratch);
        scratch.clear();
        scratch.extend(columns);
        scratch.sort_unstable();
        scratch.dedup();
        self.columns.extend_from_slice(&scratch);
        self.offsets.push(self.columns.len());
        self.scratch = scratch;
    }

    /// Append a row that is already ascending and duplicate-free.
    #[cfg(test)]
    pub(crate) fn push_sorted(&mut self, columns: &[usize]) {
        debug_assert!(
            columns.windows(2).all(|pair| pair[0] < pair[1]),
            "push_sorted requires a strictly ascending run"
        );
        self.columns.extend_from_slice(columns);
        self.offsets.push(self.columns.len());
    }

    /// Append unknown occurrences, canonicalizing them into one incidence row.
    pub(crate) fn push_occurrences(&mut self, occurrences: &[usize]) {
        self.push_unsorted(occurrences.iter().copied());
    }

    /// Affinely translate ordered unknown occurrences and append their
    /// canonical incidence set.
    ///
    /// Regular equation bodies may capture binder-invariant unknowns alongside
    /// indexed array elements. Such a row is affine even though it is not a
    /// uniform translation as a whole: the captured column has shift zero while
    /// the indexed column advances. Occurrence identity is retained until after
    /// translation; sorting earlier would pair the wrong accesses when two
    /// affine index expressions cross.
    #[cfg(test)]
    pub(crate) fn push_affine_occurrences(
        &mut self,
        occurrences: &[usize],
        shifts: &[i64],
    ) -> Result<(), StructuralError> {
        if occurrences.len() != shifts.len() {
            return Err(StructuralError::UnspannedContractViolation {
                reason: format!(
                    "incidence affine translation has {} shifts for {} source occurrences",
                    shifts.len(),
                    occurrences.len()
                ),
            });
        }

        let mut scratch = std::mem::take(&mut self.scratch);
        scratch.clear();
        scratch.reserve(occurrences.len());
        for (&source, &shift) in occurrences.iter().zip(shifts) {
            let Some(shifted) = shifted_column(source, shift) else {
                self.scratch = scratch;
                return Err(StructuralError::UnspannedContractViolation {
                    reason: format!(
                        "incidence affine translation of column {source} by {shift} leaves the unknown index range"
                    ),
                });
            };
            scratch.push(shifted);
        }

        scratch.sort_unstable();
        scratch.dedup();
        self.columns.extend_from_slice(&scratch);
        self.offsets.push(self.columns.len());
        self.scratch = scratch;
        Ok(())
    }

    /// Record the current position so a failed family emission can be undone.
    #[must_use]
    #[cfg(test)]
    pub(crate) fn checkpoint(&self) -> RowCheckpoint {
        RowCheckpoint {
            rows: self.offsets.len(),
            columns: self.columns.len(),
        }
    }

    /// Discard every row pushed after `checkpoint`.
    #[cfg(test)]
    pub(crate) fn rollback(&mut self, checkpoint: RowCheckpoint) {
        self.offsets.truncate(checkpoint.rows);
        self.columns.truncate(checkpoint.columns);
    }

    /// Finish the build.
    #[must_use]
    pub(crate) fn finish(self) -> IncidenceRows {
        IncidenceRows {
            offsets: self.offsets,
            columns: self.columns,
        }
    }
}

#[cfg(test)]
fn shifted_column(column: usize, shift: i64) -> Option<usize> {
    let shifted = i128::try_from(column)
        .ok()?
        .checked_add(i128::from(shift))?;
    usize::try_from(shifted).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_unsorted_sorts_and_dedups_row() {
        let mut builder = IncidenceRowsBuilder::with_row_capacity(2);
        builder.push_unsorted([7usize, 3, 7, 1]);
        builder.push_unsorted(std::iter::empty());
        let rows = builder.finish();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows.row(0), &[1, 3, 7]);
        assert_eq!(rows.row(1), &[] as &[usize]);
    }

    #[test]
    fn row_out_of_range_is_empty() {
        let rows = IncidenceRows::from_sorted_rows(vec![vec![1usize, 2]]);
        assert_eq!(rows.row(0), &[1, 2]);
        assert_eq!(rows.row(1), &[] as &[usize]);
        assert_eq!(rows.row(usize::MAX), &[] as &[usize]);
        assert_eq!(rows.get(1), None);
        assert_eq!(rows.get(0), Some(&[1usize, 2][..]));
    }

    #[test]
    fn contains_matches_hashset_semantics() {
        let source: Vec<HashSet<usize>> = vec![
            HashSet::from([4, 9, 1]),
            HashSet::new(),
            HashSet::from([0]),
            HashSet::from([2, 3, 5, 8, 13, 21]),
        ];
        let rows = IncidenceRows::from_sets(source.clone());
        for (index, set) in source.iter().enumerate() {
            for candidate in 0..25usize {
                assert_eq!(
                    rows.contains(index, candidate),
                    set.contains(&candidate),
                    "row {index} column {candidate}"
                );
            }
        }
        // Out-of-range rows contain nothing.
        assert!(!rows.contains(4, 0));
    }

    #[test]
    fn push_affine_occurrences_shifts_then_canonicalizes() {
        let mut builder = IncidenceRowsBuilder::default();
        builder
            .push_affine_occurrences(&[5, 6, 20], &[3, 0, 7])
            .expect("in-range affine shifts");
        let rows = builder.finish();
        assert_eq!(rows.row(0), &[6, 8, 27]);
        assert!(rows.row(0).windows(2).all(|pair| pair[0] < pair[1]));
    }

    #[test]
    fn push_affine_occurrences_rejects_invalid_shift_or_arity() {
        let mut builder = IncidenceRowsBuilder::default();
        let err = builder
            .push_affine_occurrences(&[0, 1], &[-1, 0])
            .expect_err("shifting column 0 below zero leaves the index range");
        assert!(matches!(
            err,
            StructuralError::UnspannedContractViolation { .. }
        ));
        assert_eq!(builder.row_count(), 0);

        let arity = builder
            .push_affine_occurrences(&[9], &[1, 2])
            .expect_err("occurrence and shift counts must match");
        assert!(matches!(
            arity,
            StructuralError::UnspannedContractViolation { .. }
        ));
        assert_eq!(builder.finish().len(), 0);
    }

    #[test]
    fn from_sets_matches_from_sorted_rows() {
        let sets = vec![
            HashSet::from([9usize, 2, 5]),
            HashSet::new(),
            HashSet::from([1usize]),
        ];
        let sorted = vec![vec![2usize, 5, 9], Vec::new(), vec![1usize]];
        assert_eq!(
            IncidenceRows::from_sets(sets),
            IncidenceRows::from_sorted_rows(sorted)
        );
    }

    #[test]
    fn rollback_discards_rows_pushed_after_the_checkpoint() {
        let mut builder = IncidenceRowsBuilder::default();
        builder.push_sorted(&[1, 2]);
        let checkpoint = builder.checkpoint();
        builder.push_sorted(&[3, 4]);
        builder.push_sorted(&[5]);
        assert_eq!(builder.row_count(), 3);
        builder.rollback(checkpoint);
        assert_eq!(builder.row_count(), 1);
        let rows = builder.finish();
        assert_eq!(rows.len(), 1);
        assert_eq!(rows.entry_count(), 2);
    }

    #[test]
    fn iteration_yields_every_row_in_equation_order() {
        let rows = IncidenceRows::from_sorted_rows(vec![vec![1usize], Vec::new(), vec![2usize, 4]]);
        let collected: Vec<&[usize]> = rows.iter().collect();
        assert_eq!(collected, vec![&[1usize][..], &[][..], &[2usize, 4][..]]);
        assert_eq!(rows.iter().len(), 3);
        assert_eq!((&rows).into_iter().count(), 3);
    }
}
