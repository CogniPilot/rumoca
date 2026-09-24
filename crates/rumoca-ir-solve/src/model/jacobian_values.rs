//! Non-serialized execution storage derived from the authoritative full pattern.
use super::*;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JacobianValueLayout {
    pattern: StructuralPattern,
    storage: ValueSlots,
    len: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ValueSlots {
    Empty,
    Full,
    Diagonal,
    Rows {
        offsets: Box<[usize]>,
        coordinates: Box<[(usize, usize)]>,
    },
}

impl JacobianValueLayout {
    pub(super) fn derive(pattern: &StructuralPattern) -> Option<Self> {
        let rows = pattern.rows() as usize;
        let columns = pattern.columns() as usize;
        let (storage, len) = match pattern.view() {
            StructuralPatternView::Empty => (ValueSlots::Empty, 0),
            StructuralPatternView::Full => (ValueSlots::Full, rows.checked_mul(columns)?),
            StructuralPatternView::Diagonal => (ValueSlots::Diagonal, rows.min(columns)),
            _ => {
                let mut offsets = Vec::with_capacity(rows.checked_add(1)?);
                let mut coordinates = Vec::new();
                offsets.push(0);
                for row in 0..rows {
                    pattern.visit_row_columns(row, |column| coordinates.push((row, column)));
                    offsets.push(coordinates.len());
                }
                let len = coordinates.len();
                (
                    ValueSlots::Rows {
                        offsets: offsets.into(),
                        coordinates: coordinates.into(),
                    },
                    len,
                )
            }
        };
        Some(Self {
            pattern: pattern.clone(),
            storage,
            len,
        })
    }

    pub fn pattern(&self) -> &StructuralPattern {
        &self.pattern
    }
    pub fn shape(&self) -> (usize, usize) {
        (
            self.pattern.rows() as usize,
            self.pattern.columns() as usize,
        )
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn coordinate(&self, slot: usize) -> Option<(usize, usize)> {
        if slot >= self.len {
            return None;
        }
        Some(match &self.storage {
            ValueSlots::Empty => return None,
            ValueSlots::Full => (slot / self.shape().1, slot % self.shape().1),
            ValueSlots::Diagonal => (slot, slot),
            ValueSlots::Rows { coordinates, .. } => coordinates[slot],
        })
    }

    pub fn row_slots(&self, row: usize) -> Option<Range<usize>> {
        if row >= self.shape().0 {
            return None;
        }
        Some(match &self.storage {
            ValueSlots::Empty => 0..0,
            ValueSlots::Full => row * self.shape().1..(row + 1) * self.shape().1,
            ValueSlots::Diagonal => row.min(self.len)..(row + 1).min(self.len),
            ValueSlots::Rows { offsets, .. } => offsets[row]..offsets[row + 1],
        })
    }

    /// Preparation-only coordinate lookup. None is an out-of-range request;
    /// Some(None) is a structural zero, never a numerical-zero classification.
    pub fn locate(&self, row: usize, column: usize) -> Option<Option<usize>> {
        if row >= self.shape().0 || column >= self.shape().1 {
            return None;
        }
        Some(match &self.storage {
            ValueSlots::Empty => None,
            ValueSlots::Full => Some(row * self.shape().1 + column),
            ValueSlots::Diagonal => (row == column).then_some(row),
            ValueSlots::Rows {
                offsets,
                coordinates,
            } => {
                let start = offsets[row];
                coordinates[start..offsets[row + 1]]
                    .binary_search(&(row, column))
                    .ok()
                    .map(|i| start + i)
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    fn provenance() -> PatternProvenance {
        PatternProvenance::derived(
            PatternDerivation::DependencyPropagation,
            Span::from_offsets(SourceId::from_source_name("compact_jacobian.mo"), 0, 1),
        )
        .unwrap()
    }

    #[test]
    fn compact_layout_covers_rectangular_full_diagonal_empty_and_csr() {
        let cases = [
            (vec![vec![0, 1, 2], vec![0, 1, 2]], 2, 3),
            (vec![vec![0], vec![1], vec![], vec![]], 4, 3),
            (vec![vec![], vec![]], 2, 3),
            (vec![vec![0, 2], vec![1], vec![0, 2]], 3, 3),
        ];
        for (rows, row_count, columns) in cases {
            check_case(&rows, row_count, columns);
        }
    }

    fn check_case(rows: &[Vec<usize>], row_count: usize, columns: usize) {
        let pattern =
            StructuralPattern::from_row_dependencies(row_count, columns, rows, provenance())
                .unwrap();
        let layout = JacobianValueLayout::derive(&pattern).unwrap();
        let coordinates = pattern.nonzero_coordinates();
        assert_eq!(layout.shape(), (row_count, columns));
        assert_eq!(layout.len(), coordinates.len());
        for (slot, &coordinate) in coordinates.iter().enumerate() {
            assert_eq!(layout.coordinate(slot), Some(coordinate));
            assert_eq!(layout.locate(coordinate.0, coordinate.1), Some(Some(slot)));
        }
        for row in 0..row_count {
            let slots = layout.row_slots(row).unwrap();
            assert_eq!(
                slots
                    .map(|slot| layout.coordinate(slot).unwrap())
                    .collect::<Vec<_>>(),
                coordinates
                    .iter()
                    .copied()
                    .filter(|(r, _)| *r == row)
                    .collect::<Vec<_>>()
            );
            for column in 0..columns {
                assert_eq!(
                    layout.locate(row, column),
                    Some(
                        coordinates
                            .iter()
                            .position(|&coordinate| coordinate == (row, column))
                    )
                );
            }
        }
        assert_eq!(layout.coordinate(layout.len()), None);
        assert_eq!(layout.row_slots(row_count), None);
        assert_eq!(layout.locate(row_count, 0), None);
        assert_eq!(layout.locate(0, columns), None);
    }

    #[test]
    fn full_large_extent_remains_implicit_metadata() {
        let pattern = StructuralPattern::full(1_000_000, 1_000_000, provenance()).unwrap();
        let layout = JacobianValueLayout::derive(&pattern).unwrap();
        assert!(matches!(layout.storage, ValueSlots::Full));
        assert_eq!(layout.len(), 1_000_000_000_000);
        assert_eq!(
            layout.locate(999_999, 999_999),
            Some(Some(layout.len() - 1))
        );
        assert_eq!(
            layout.coordinate(layout.len() - 1),
            Some((999_999, 999_999))
        );
    }
}
