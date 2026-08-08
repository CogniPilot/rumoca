use std::cmp::Reverse;
use std::collections::BTreeSet;

use rumoca_core::Span;
use serde::{Deserialize, Deserializer, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PatternDerivation {
    ConservativeFull,
    DependencyPropagation,
    TensorOperand,
    AffineDomain,
    ComplexLaneExpansion,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatternProvenance {
    derivation: PatternDerivation,
    span: Span,
}

impl PatternProvenance {
    pub fn derived(
        derivation: PatternDerivation,
        span: Span,
    ) -> Result<Self, StructuralPatternError> {
        if span.is_dummy() {
            return Err(StructuralPatternError::MissingProvenance);
        }
        Ok(Self { derivation, span })
    }

    pub const fn derivation(self) -> PatternDerivation {
        self.derivation
    }

    pub const fn span(self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct StructuralPattern {
    rows: u32,
    columns: u32,
    representation: PatternRepresentation,
    provenance: PatternProvenance,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum PatternRepresentation {
    Empty,
    Full,
    Diagonal,
    Banded {
        lower_bandwidth: u32,
        upper_bandwidth: u32,
    },
    Csr {
        row_offsets: Box<[u32]>,
        column_indices: Box<[u32]>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StructuralPatternView<'pattern> {
    Empty,
    Full,
    Diagonal,
    Banded {
        lower_bandwidth: u32,
        upper_bandwidth: u32,
    },
    Csr {
        row_offsets: &'pattern [u32],
        column_indices: &'pattern [u32],
    },
}

/// A deterministic coloring certified against one structural pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnColoring {
    column_count: u32,
    groups: Box<[Box<[u32]>]>,
}

impl ColumnColoring {
    pub const fn column_count(&self) -> u32 {
        self.column_count
    }

    pub fn groups(&self) -> &[Box<[u32]>] {
        &self.groups
    }

    pub fn compressed_seed_count(&self) -> usize {
        self.groups.len()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StructuralPatternError {
    MissingProvenance,
    DimensionOverflow,
    DependencyRowCount {
        expected: usize,
        found: usize,
    },
    InvalidRowOffsetCount {
        expected: usize,
        found: usize,
    },
    FirstRowOffsetNotZero {
        found: u32,
    },
    NonMonotoneRowOffsets {
        row: usize,
    },
    FinalRowOffsetMismatch {
        expected: usize,
        found: u32,
    },
    ColumnOutOfBounds {
        row: usize,
        column: u32,
        columns: u32,
    },
    ColumnsNotStrictlyIncreasing {
        row: usize,
    },
}

impl std::fmt::Display for StructuralPatternError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingProvenance => {
                formatter.write_str("structural pattern requires source-backed provenance")
            }
            Self::DimensionOverflow => {
                formatter.write_str("structural pattern dimensions exceed u32")
            }
            Self::DependencyRowCount { expected, found } => write!(
                formatter,
                "structural dependency row count is {found}, expected {expected}"
            ),
            Self::InvalidRowOffsetCount { expected, found } => write!(
                formatter,
                "CSR row-offset count is {found}, expected {expected}"
            ),
            Self::FirstRowOffsetNotZero { found } => {
                write!(formatter, "CSR first row offset is {found}, expected zero")
            }
            Self::NonMonotoneRowOffsets { row } => {
                write!(formatter, "CSR row offsets decrease at row {row}")
            }
            Self::FinalRowOffsetMismatch { expected, found } => write!(
                formatter,
                "CSR final row offset is {found}, expected {expected}"
            ),
            Self::ColumnOutOfBounds {
                row,
                column,
                columns,
            } => write!(
                formatter,
                "CSR row {row} contains column {column} outside 0..{columns}"
            ),
            Self::ColumnsNotStrictlyIncreasing { row } => {
                write!(
                    formatter,
                    "CSR columns are not strictly increasing in row {row}"
                )
            }
        }
    }
}

impl std::error::Error for StructuralPatternError {}

impl StructuralPattern {
    fn empty(
        rows: usize,
        columns: usize,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(rows, columns, PatternRepresentation::Empty, provenance)
    }

    pub fn full(
        rows: usize,
        columns: usize,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(rows, columns, PatternRepresentation::Full, provenance)
    }

    fn diagonal(
        rows: usize,
        columns: usize,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(rows, columns, PatternRepresentation::Diagonal, provenance)
    }

    fn banded(
        rows: usize,
        columns: usize,
        lower_bandwidth: u32,
        upper_bandwidth: u32,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(
            rows,
            columns,
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            },
            provenance,
        )
    }

    fn csr(
        rows: usize,
        columns: usize,
        row_offsets: impl Into<Box<[u32]>>,
        column_indices: impl Into<Box<[u32]>>,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        let rows = checked_dimension(rows)?;
        let columns = checked_dimension(columns)?;
        let row_offsets = row_offsets.into();
        let column_indices = column_indices.into();
        validate_csr(rows, columns, &row_offsets, &column_indices)?;
        Ok(Self {
            rows,
            columns,
            representation: PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            },
            provenance,
        })
    }

    fn new(
        rows: usize,
        columns: usize,
        representation: PatternRepresentation,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Ok(Self {
            rows: checked_dimension(rows)?,
            columns: checked_dimension(columns)?,
            representation,
            provenance,
        })
    }

    pub const fn rows(&self) -> u32 {
        self.rows
    }

    pub const fn columns(&self) -> u32 {
        self.columns
    }

    pub const fn provenance(&self) -> PatternProvenance {
        self.provenance
    }

    pub fn view(&self) -> StructuralPatternView<'_> {
        match &self.representation {
            PatternRepresentation::Empty => StructuralPatternView::Empty,
            PatternRepresentation::Full => StructuralPatternView::Full,
            PatternRepresentation::Diagonal => StructuralPatternView::Diagonal,
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => StructuralPatternView::Banded {
                lower_bandwidth: *lower_bandwidth,
                upper_bandwidth: *upper_bandwidth,
            },
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => StructuralPatternView::Csr {
                row_offsets,
                column_indices,
            },
        }
    }

    pub fn contains(&self, row: u32, column: u32) -> bool {
        if row >= self.rows || column >= self.columns {
            return false;
        }
        match &self.representation {
            PatternRepresentation::Empty => false,
            PatternRepresentation::Full => true,
            PatternRepresentation::Diagonal => row == column,
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => {
                column.saturating_add(*lower_bandwidth) >= row
                    && row.saturating_add(*upper_bandwidth) >= column
            }
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => {
                let start = row_offsets[row as usize] as usize;
                let end = row_offsets[row as usize + 1] as usize;
                let row_columns = &column_indices[start..end];
                if row_columns.len() <= 8 {
                    row_columns.contains(&column)
                } else {
                    row_columns.binary_search(&column).is_ok()
                }
            }
        }
    }

    pub fn nonzero_upper_bound(&self) -> Option<usize> {
        let rows = self.rows as usize;
        let columns = self.columns as usize;
        match &self.representation {
            PatternRepresentation::Empty => Some(0),
            PatternRepresentation::Full => rows.checked_mul(columns),
            PatternRepresentation::Diagonal => Some(rows.min(columns)),
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => banded_nonzero_count(
                rows,
                columns,
                *lower_bandwidth as usize,
                *upper_bandwidth as usize,
            ),
            PatternRepresentation::Csr { column_indices, .. } => Some(column_indices.len()),
        }
    }

    /// Materialize the certified relation in deterministic row-major order.
    /// Runtime and backend storage policies may consume this view without
    /// rediscovering dependencies from programs or numerical values.
    pub fn nonzero_coordinates(&self) -> Vec<(usize, usize)> {
        let capacity = self.nonzero_upper_bound().unwrap_or(0);
        let mut coordinates = Vec::with_capacity(capacity);
        for row in 0..self.rows as usize {
            self.visit_row_columns(row, |column| coordinates.push((row, column)));
        }
        coordinates
    }

    /// Visit the certified columns in one row without materializing the
    /// complete sparse relation. Columns are yielded in ascending order.
    #[inline]
    pub fn visit_row_columns(&self, row: usize, mut visitor: impl FnMut(usize)) {
        debug_assert!(row < self.rows as usize);
        match &self.representation {
            PatternRepresentation::Empty => {}
            PatternRepresentation::Full => {
                (0..self.columns as usize).for_each(&mut visitor);
            }
            PatternRepresentation::Diagonal => {
                if row < self.columns as usize {
                    visitor(row);
                }
            }
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => {
                let start = row.saturating_sub(*lower_bandwidth as usize);
                let end = row
                    .saturating_add(*upper_bandwidth as usize)
                    .saturating_add(1)
                    .min(self.columns as usize);
                (start..end).for_each(&mut visitor);
            }
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => {
                let start = row_offsets[row] as usize;
                let end = row_offsets[row + 1] as usize;
                column_indices[start..end]
                    .iter()
                    .for_each(|column| visitor(*column as usize));
            }
        }
    }

    /// Derive the canonical pattern from an exact or conservative set of
    /// possible columns for every row.
    ///
    /// Callers provide dependency facts, not a representation claim. This
    /// operation checks and canonicalizes the facts before choosing a compact
    /// representation.
    pub fn from_row_dependencies(
        rows: usize,
        columns: usize,
        row_dependencies: &[Vec<usize>],
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        if row_dependencies.len() != rows {
            return Err(StructuralPatternError::DependencyRowCount {
                expected: rows,
                found: row_dependencies.len(),
            });
        }
        let checked_rows = checked_dimension(rows)?;
        let checked_columns = checked_dimension(columns)?;
        let mut row_offsets = Vec::with_capacity(rows.saturating_add(1));
        let mut column_indices = Vec::new();
        row_offsets.push(0);
        let mut diagonal = true;
        let mut full = true;
        for (row, dependencies) in row_dependencies.iter().enumerate() {
            let mut canonical = dependencies.clone();
            canonical.sort_unstable();
            canonical.dedup();
            diagonal &= canonical
                .iter()
                .all(|column| *column == row && row < columns);
            full &= canonical.len() == columns && canonical.iter().copied().eq(0..columns);
            append_checked_columns(row, canonical, checked_columns, &mut column_indices)?;
            row_offsets.push(
                u32::try_from(column_indices.len())
                    .map_err(|_| StructuralPatternError::DimensionOverflow)?,
            );
        }
        if column_indices.is_empty() {
            return Self::empty(rows, columns, provenance);
        }
        if diagonal {
            return Self::diagonal(rows, columns, provenance);
        }
        if full {
            return Self::full(rows, columns, provenance);
        }
        validate_csr(checked_rows, checked_columns, &row_offsets, &column_indices)?;
        Ok(Self {
            rows: checked_rows,
            columns: checked_columns,
            representation: PatternRepresentation::Csr {
                row_offsets: row_offsets.into_boxed_slice(),
                column_indices: column_indices.into_boxed_slice(),
            },
            provenance,
        })
    }

    pub fn column_rows(&self) -> Vec<Vec<usize>> {
        let mut columns = vec![Vec::new(); self.columns as usize];
        match &self.representation {
            PatternRepresentation::Empty => {}
            PatternRepresentation::Full => {
                for column in &mut columns {
                    column.extend(0..self.rows as usize);
                }
            }
            PatternRepresentation::Diagonal => {
                for index in 0..(self.rows.min(self.columns) as usize) {
                    columns[index].push(index);
                }
            }
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => append_banded_column_rows(
                &mut columns,
                self.rows as usize,
                *lower_bandwidth as usize,
                *upper_bandwidth as usize,
            ),
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => append_csr_column_rows(&mut columns, row_offsets, column_indices),
        }
        columns
    }

    pub fn column_coloring(&self) -> ColumnColoring {
        let column_rows = self.column_rows();
        let mut order: Vec<usize> = (0..column_rows.len()).collect();
        order.sort_by_key(|column| (Reverse(column_rows[*column].len()), *column));

        let mut groups: Vec<Vec<u32>> = Vec::new();
        let mut occupied_rows: Vec<BTreeSet<usize>> = Vec::new();
        for column in order {
            let rows = &column_rows[column];
            if let Some((group_index, occupied)) = occupied_rows
                .iter_mut()
                .enumerate()
                .find(|(_, occupied)| rows.iter().all(|row| !occupied.contains(row)))
            {
                groups[group_index].push(column as u32);
                occupied.extend(rows);
            } else {
                groups.push(vec![column as u32]);
                occupied_rows.push(rows.iter().copied().collect());
            }
        }
        for group in &mut groups {
            group.sort_unstable();
        }
        groups.sort_by_key(|group| group.first().copied().unwrap_or(u32::MAX));
        ColumnColoring {
            column_count: self.columns,
            groups: groups
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }
}

fn append_checked_columns(
    row: usize,
    canonical: Vec<usize>,
    columns: u32,
    output: &mut Vec<u32>,
) -> Result<(), StructuralPatternError> {
    for column in canonical {
        let column = checked_dimension(column)?;
        if column >= columns {
            return Err(StructuralPatternError::ColumnOutOfBounds {
                row,
                column,
                columns,
            });
        }
        output.push(column);
    }
    Ok(())
}

fn append_banded_column_rows(
    columns: &mut [Vec<usize>],
    rows: usize,
    lower_bandwidth: usize,
    upper_bandwidth: usize,
) {
    for row in 0..rows {
        let start = row.saturating_sub(lower_bandwidth);
        let end = row
            .saturating_add(upper_bandwidth)
            .saturating_add(1)
            .min(columns.len());
        columns[start..end]
            .iter_mut()
            .for_each(|column_rows| column_rows.push(row));
    }
}

fn append_csr_column_rows(columns: &mut [Vec<usize>], row_offsets: &[u32], column_indices: &[u32]) {
    for row in 0..row_offsets.len().saturating_sub(1) {
        let start = row_offsets[row] as usize;
        let end = row_offsets[row + 1] as usize;
        column_indices[start..end]
            .iter()
            .for_each(|column| columns[*column as usize].push(row));
    }
}

fn checked_dimension(value: usize) -> Result<u32, StructuralPatternError> {
    u32::try_from(value).map_err(|_| StructuralPatternError::DimensionOverflow)
}

fn validate_csr(
    rows: u32,
    columns: u32,
    row_offsets: &[u32],
    column_indices: &[u32],
) -> Result<(), StructuralPatternError> {
    let expected = rows as usize + 1;
    if row_offsets.len() != expected {
        return Err(StructuralPatternError::InvalidRowOffsetCount {
            expected,
            found: row_offsets.len(),
        });
    }
    if row_offsets.first().copied() != Some(0) {
        return Err(StructuralPatternError::FirstRowOffsetNotZero {
            found: row_offsets.first().copied().unwrap_or(u32::MAX),
        });
    }
    for row in 0..rows as usize {
        let start = row_offsets[row];
        let end = row_offsets[row + 1];
        if start > end {
            return Err(StructuralPatternError::NonMonotoneRowOffsets { row });
        }
        let entries = column_indices.get(start as usize..end as usize).ok_or(
            StructuralPatternError::FinalRowOffsetMismatch {
                expected: column_indices.len(),
                found: end,
            },
        )?;
        let mut previous = None;
        for &column in entries {
            if column >= columns {
                return Err(StructuralPatternError::ColumnOutOfBounds {
                    row,
                    column,
                    columns,
                });
            }
            if previous.is_some_and(|previous| previous >= column) {
                return Err(StructuralPatternError::ColumnsNotStrictlyIncreasing { row });
            }
            previous = Some(column);
        }
    }
    let found = row_offsets.last().copied().unwrap_or(u32::MAX);
    if found as usize != column_indices.len() {
        return Err(StructuralPatternError::FinalRowOffsetMismatch {
            expected: column_indices.len(),
            found,
        });
    }
    Ok(())
}

fn banded_nonzero_count(
    rows: usize,
    columns: usize,
    lower_bandwidth: usize,
    upper_bandwidth: usize,
) -> Option<usize> {
    (0..rows).try_fold(0usize, |count, row| {
        let start = row.saturating_sub(lower_bandwidth);
        let end = row
            .checked_add(upper_bandwidth)?
            .checked_add(1)?
            .min(columns);
        count.checked_add(end.saturating_sub(start))
    })
}

#[derive(Deserialize)]
struct StructuralPatternWire {
    rows: u32,
    columns: u32,
    representation: PatternRepresentation,
    provenance: PatternProvenance,
}

impl<'de> Deserialize<'de> for StructuralPattern {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = StructuralPatternWire::deserialize(deserializer)?;
        if wire.provenance.span().is_dummy() {
            return Err(serde::de::Error::custom(
                StructuralPatternError::MissingProvenance,
            ));
        }
        let rows = wire.rows as usize;
        let columns = wire.columns as usize;
        match wire.representation {
            PatternRepresentation::Empty => Self::empty(rows, columns, wire.provenance),
            PatternRepresentation::Full => Self::full(rows, columns, wire.provenance),
            PatternRepresentation::Diagonal => Self::diagonal(rows, columns, wire.provenance),
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => Self::banded(
                rows,
                columns,
                lower_bandwidth,
                upper_bandwidth,
                wire.provenance,
            ),
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => Self::csr(rows, columns, row_offsets, column_indices, wire.provenance),
        }
        .map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn provenance() -> PatternProvenance {
        PatternProvenance::derived(
            PatternDerivation::DependencyPropagation,
            Span::from_offsets(
                rumoca_core::SourceId::from_source_name("structural_pattern.mo"),
                0,
                1,
            ),
        )
        .expect("fixture provenance")
    }

    #[test]
    fn csr_construction_rejects_unsorted_and_out_of_bounds_columns() {
        assert!(matches!(
            StructuralPattern::csr(2, 3, [0, 2, 2], [2, 1], provenance()),
            Err(StructuralPatternError::ColumnsNotStrictlyIncreasing { row: 0 })
        ));
        assert!(matches!(
            StructuralPattern::csr(1, 3, [0, 1], [3], provenance()),
            Err(StructuralPatternError::ColumnOutOfBounds { row: 0, .. })
        ));
    }

    #[test]
    fn checked_pattern_round_trip_preserves_membership() {
        let pattern = StructuralPattern::csr(3, 4, [0, 2, 3, 3], [0, 3, 2], provenance()).unwrap();
        let json = serde_json::to_string(&pattern).unwrap();
        let decoded: StructuralPattern = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded, pattern);
        assert!(decoded.contains(0, 3));
        assert!(!decoded.contains(2, 0));
    }

    #[test]
    fn dummy_provenance_is_rejected() {
        assert_eq!(
            PatternProvenance::derived(PatternDerivation::ConservativeFull, Span::DUMMY),
            Err(StructuralPatternError::MissingProvenance)
        );
    }
}
