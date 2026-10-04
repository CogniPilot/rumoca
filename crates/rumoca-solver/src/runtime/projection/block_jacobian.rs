//! Storage of one projection block's Jacobian.

use std::borrow::Cow;
use std::sync::Arc;

use nalgebra::{DMatrix, DVector};
use rumoca_ir_solve::CompactPatternLayout;

/// A projection block's Jacobian, stored densely when the block carries no
/// structural pattern and compactly, one value per certified entry in the
/// pattern's [`CompactPatternLayout`], when it does. A compact matrix is zero
/// outside its pattern by construction: no entry outside the pattern has
/// storage, so forming one costs the pattern, never the dense block.
#[derive(Clone, Debug)]
pub(crate) struct BlockJacobian {
    storage: Storage,
}

#[derive(Clone, Debug)]
enum Storage {
    Dense(DMatrix<f64>),
    Compact {
        layout: Arc<CompactPatternLayout>,
        values: Vec<f64>,
    },
}

/// The value every entry outside a compact pattern reads.
static OUTSIDE: f64 = 0.0;

impl BlockJacobian {
    pub(crate) fn dense(matrix: DMatrix<f64>) -> Self {
        Self {
            storage: Storage::Dense(matrix),
        }
    }

    pub(crate) fn zeros(rows: usize, columns: usize) -> Self {
        Self::dense(DMatrix::zeros(rows, columns))
    }

    /// A zero matrix stored in `layout`.
    pub(crate) fn compact(layout: &Arc<CompactPatternLayout>) -> Self {
        Self {
            storage: Storage::Compact {
                values: vec![0.0; layout.len()],
                layout: Arc::clone(layout),
            },
        }
    }

    pub(crate) fn nrows(&self) -> usize {
        match &self.storage {
            Storage::Dense(matrix) => matrix.nrows(),
            Storage::Compact { layout, .. } => layout.rows(),
        }
    }

    pub(crate) fn ncols(&self) -> usize {
        match &self.storage {
            Storage::Dense(matrix) => matrix.ncols(),
            Storage::Compact { layout, .. } => layout.columns(),
        }
    }

    pub(crate) fn shape(&self) -> (usize, usize) {
        (self.nrows(), self.ncols())
    }

    pub(crate) fn is_square(&self) -> bool {
        self.nrows() == self.ncols()
    }

    /// Whether this matrix is stored in `layout`.
    pub(crate) fn is_stored_in(&self, layout: &CompactPatternLayout) -> bool {
        matches!(&self.storage, Storage::Compact { layout: own, .. } if **own == *layout)
    }

    /// Zero every stored entry, keeping the storage.
    pub(crate) fn clear(&mut self) {
        match &mut self.storage {
            Storage::Dense(matrix) => matrix.fill(0.0),
            Storage::Compact { values, .. } => values.fill(0.0),
        }
    }

    /// The stored values: column-major for dense storage, in layout order for
    /// compact storage.
    pub(crate) fn storage_mut(&mut self) -> &mut [f64] {
        match &mut self.storage {
            Storage::Dense(matrix) => matrix.as_mut_slice(),
            Storage::Compact { values, .. } => values,
        }
    }

    /// The dense form, borrowed when the storage is dense. Only a dense
    /// kernel (a small block's LU, a rank-deficient fallback) asks for it.
    pub(crate) fn as_dense(&self) -> Cow<'_, DMatrix<f64>> {
        match &self.storage {
            Storage::Dense(matrix) => Cow::Borrowed(matrix),
            Storage::Compact { layout, values } => Cow::Owned(compact_to_dense(layout, values)),
        }
    }

    /// Visit every stored entry of `row` that can be nonzero, as `(column,
    /// value)`: the compact entries of compact storage, else the columns of
    /// `structure` (every column without one). An entry not visited is zero.
    pub(crate) fn visit_row(
        &self,
        row: usize,
        structure: Option<&rumoca_ir_solve::StructuralPattern>,
        visit: &mut dyn FnMut(usize, f64),
    ) {
        match &self.storage {
            Storage::Compact { layout, values } => {
                let (slots, columns) = layout.row(row);
                for (&column, &value) in columns.iter().zip(&values[slots]) {
                    visit(column, value);
                }
            }
            Storage::Dense(matrix) => match structure {
                Some(pattern) => {
                    pattern
                        .visit_row_columns(row, &mut |column| visit(column, matrix[(row, column)]));
                }
                None => (0..matrix.ncols()).for_each(|column| visit(column, matrix[(row, column)])),
            },
        }
    }

    /// Set every entry of `row` in `structure` to `value(column)`. A compact
    /// matrix stores exactly its pattern's entries, so it writes its own row.
    pub(crate) fn set_row(
        &mut self,
        row: usize,
        structure: &rumoca_ir_solve::StructuralPattern,
        value: &mut dyn FnMut(usize) -> f64,
    ) {
        match &mut self.storage {
            Storage::Compact { layout, values } => {
                let (slots, columns) = layout.row(row);
                for (slot, &column) in slots.zip(columns) {
                    values[slot] = value(column);
                }
            }
            Storage::Dense(matrix) => {
                structure
                    .visit_row_columns(row, &mut |column| matrix[(row, column)] = value(column));
            }
        }
    }

    /// The compact layout and its values, when stored compactly.
    pub(crate) fn compact_storage(&self) -> Option<(&Arc<CompactPatternLayout>, &[f64])> {
        match &self.storage {
            Storage::Dense(_) => None,
            Storage::Compact { layout, values } => Some((layout, values)),
        }
    }

    /// `self * vector`.
    pub(crate) fn mul_vector(&self, vector: &DVector<f64>) -> DVector<f64> {
        match &self.storage {
            Storage::Dense(matrix) => matrix * vector,
            Storage::Compact { layout, values } => DVector::from_fn(layout.rows(), |row, _| {
                let (slots, columns) = layout.row(row);
                columns
                    .iter()
                    .zip(&values[slots])
                    .map(|(&column, &value)| value * vector[column])
                    .sum()
            }),
        }
    }
}

fn compact_to_dense(layout: &CompactPatternLayout, values: &[f64]) -> DMatrix<f64> {
    let mut dense = DMatrix::zeros(layout.rows(), layout.columns());
    for row in 0..layout.rows() {
        let (slots, columns) = layout.row(row);
        for (&column, &value) in columns.iter().zip(&values[slots]) {
            dense[(row, column)] = value;
        }
    }
    dense
}

impl std::ops::Index<(usize, usize)> for BlockJacobian {
    type Output = f64;

    fn index(&self, (row, column): (usize, usize)) -> &f64 {
        match &self.storage {
            Storage::Dense(matrix) => &matrix[(row, column)],
            Storage::Compact { layout, values } => layout
                .slot(row, column)
                .map_or(&OUTSIDE, |slot| &values[slot]),
        }
    }
}

impl std::ops::IndexMut<(usize, usize)> for BlockJacobian {
    /// A compact matrix has storage only inside its pattern, and every
    /// structured writer writes only pattern entries.
    fn index_mut(&mut self, (row, column): (usize, usize)) -> &mut f64 {
        match &mut self.storage {
            Storage::Dense(matrix) => &mut matrix[(row, column)],
            Storage::Compact { layout, values } => {
                let slot = layout
                    .slot(row, column)
                    .expect("a structured Jacobian writer writes only pattern entries");
                &mut values[slot]
            }
        }
    }
}

#[cfg(test)]
mod tests;
