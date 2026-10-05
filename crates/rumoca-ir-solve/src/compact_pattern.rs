//! Row-major storage order of a structural pattern's entries.

use crate::StructuralPattern;

/// The storage order of a pattern's certified entries: row by row, columns
/// ascending within a row, exactly the order
/// [`StructuralPattern::visit_row_columns`] yields them. A value stored for
/// every entry in this order, and nothing else, is the compact form of a
/// matrix that is zero outside the pattern; its slot of `(row, column)` is
/// [`Self::slot`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompactPatternLayout {
    rows: usize,
    columns: usize,
    row_offsets: Box<[usize]>,
    row_columns: Box<[usize]>,
}

impl CompactPatternLayout {
    #[must_use]
    pub fn of(pattern: &StructuralPattern) -> Self {
        let rows = pattern.rows() as usize;
        let mut row_offsets = Vec::with_capacity(rows + 1);
        let mut row_columns = Vec::with_capacity(pattern.nonzero_upper_bound().unwrap_or(0));
        row_offsets.push(0);
        for row in 0..rows {
            pattern.visit_row_columns(row, &mut |column| row_columns.push(column));
            row_offsets.push(row_columns.len());
        }
        Self {
            rows,
            columns: pattern.columns() as usize,
            row_offsets: row_offsets.into_boxed_slice(),
            row_columns: row_columns.into_boxed_slice(),
        }
    }

    #[must_use]
    pub const fn rows(&self) -> usize {
        self.rows
    }

    #[must_use]
    pub const fn columns(&self) -> usize {
        self.columns
    }

    /// The number of stored entries.
    #[must_use]
    pub fn len(&self) -> usize {
        self.row_columns.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.row_columns.is_empty()
    }

    /// The slots of `row` and their columns, ascending.
    #[must_use]
    pub fn row(&self, row: usize) -> (std::ops::Range<usize>, &[usize]) {
        let slots = self.row_offsets[row]..self.row_offsets[row + 1];
        let columns = &self.row_columns[slots.clone()];
        (slots, columns)
    }

    /// The slot of `(row, column)`, or `None` outside the pattern.
    #[must_use]
    pub fn slot(&self, row: usize, column: usize) -> Option<usize> {
        if row >= self.rows {
            return None;
        }
        let (slots, columns) = self.row(row);
        columns
            .binary_search(&column)
            .ok()
            .map(|offset| slots.start + offset)
    }

    /// The slot of a column-major dense index `column * rows + row`.
    #[must_use]
    pub fn dense_slot(&self, dense: usize) -> Option<usize> {
        let rows = self.rows.max(1);
        self.slot(dense % rows, dense / rows)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{PatternDerivation, PatternProvenance};

    fn pattern() -> StructuralPattern {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compact_pattern.mo"),
            0,
            1,
        );
        let provenance =
            PatternProvenance::derived(PatternDerivation::DependencyPropagation, span).unwrap();
        StructuralPattern::from_row_dependencies(3, 3, &[vec![2, 0], vec![], vec![1]], provenance)
            .unwrap()
    }

    #[test]
    fn compact_slots_follow_rows_then_ascending_columns() {
        let layout = CompactPatternLayout::of(&pattern());
        assert_eq!(layout.len(), 3);
        assert!(!layout.is_empty());
        assert_eq!(layout.slot(0, 0), Some(0));
        assert_eq!(layout.slot(0, 2), Some(1));
        assert_eq!(layout.slot(2, 1), Some(2));
        assert_eq!(layout.slot(1, 1), None);
        assert_eq!(layout.slot(3, 0), None);
        assert_eq!(layout.dense_slot(2 * 3), Some(1));
        assert_eq!(layout.row(1).1, &[] as &[usize]);
    }
}
