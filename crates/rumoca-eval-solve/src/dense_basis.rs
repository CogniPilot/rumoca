//! Bounded numerical linear algebra for coordinate proposals, not rank proofs.

#[cfg(test)]
mod tests;

use nalgebra::{DMatrix, DVector, Dyn, linalg::SVD};

/// Column policy supplied by the semantic owner. Higher priorities remain free
/// when a lower-priority regular dependent basis exists.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColumnChoice {
    Dependent,
    Eligible(u8),
    Independent,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DenseBasisError {
    Shape,
    NonFinite,
    Decomposition,
    Rank,
}

/// Reciprocal conditioning of a set of dependent columns measured against the
/// full stage's pivot scale, paired with the singular threshold the rank test
/// rejects.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DependentConditioning {
    /// The smallest column-pivoted pivot of the dependent columns divided by
    /// the largest column-pivoted pivot of the whole stage. It falls to zero as
    /// a dependent reconstruction column collapses relative to the stage.
    pub rcond: f64,
    /// `max(rows, cols) * EPSILON`: the relative pivot boundary
    /// [`DenseStageMatrix::independent_columns`] applies. An `rcond` at or below
    /// this is the rank loss the column-pivoted QR test rejects.
    pub singular_threshold: f64,
}

/// Finite numerical payload, materialized only at the evaluation boundary.
pub struct DenseStageMatrix(DMatrix<f64>);

impl DenseStageMatrix {
    pub fn new(rows: usize, columns: usize, values: &[f64]) -> Result<Self, DenseBasisError> {
        if rows.checked_mul(columns) != Some(values.len()) {
            return Err(DenseBasisError::Shape);
        }
        if !values.iter().all(|x| x.is_finite()) {
            return Err(DenseBasisError::NonFinite);
        }
        Ok(Self(DMatrix::from_row_slice(rows, columns, values)))
    }

    /// Minimum-norm linear correction. Singular trial points are permitted here;
    /// they never constitute a successfully selected coordinate basis.
    pub fn correction(&self, residual: &[f64]) -> Result<Vec<f64>, DenseBasisError> {
        if residual.len() != self.0.nrows() {
            return Err(DenseBasisError::Shape);
        }
        if !residual.iter().all(|x| x.is_finite()) {
            return Err(DenseBasisError::NonFinite);
        }
        if self.0.nrows() == 0 || self.0.ncols() == 0 {
            return Ok(vec![0.; self.0.ncols()]);
        }
        let decomposition = decompose(self.0.clone())?;
        let threshold = self.threshold(decomposition.singular_values.amax());
        let step = decomposition
            .solve(&(-DVector::from_column_slice(residual)), threshold)
            .map_err(|_| DenseBasisError::Decomposition)?;
        if !step.iter().all(|x| x.is_finite()) {
            return Err(DenseBasisError::NonFinite);
        }
        Ok(step.as_slice().to_vec())
    }

    /// Choose a complete dependent basis and return its independent complement.
    /// Reorthogonalized pivoting respects required columns and preference groups.
    pub fn independent_columns(
        &self,
        choices: &[ColumnChoice],
    ) -> Result<Vec<usize>, DenseBasisError> {
        if choices.len() != self.0.ncols() || self.0.nrows() > choices.len() {
            return Err(DenseBasisError::Shape);
        }
        let mut basis = Vec::new();
        let mut selected = vec![false; choices.len()];
        let scale = (0..self.0.ncols())
            .map(|c| self.0.column(c).norm())
            .fold(0., f64::max);
        let threshold = self.threshold(scale);
        for (column, choice) in choices.iter().enumerate() {
            if *choice == ColumnChoice::Dependent {
                let vector = remainder(self.0.column(column).into_owned(), &basis);
                append_basis(&mut basis, vector, threshold)?;
                selected[column] = true;
            }
        }
        let priorities = choices
            .iter()
            .filter_map(|c| match c {
                ColumnChoice::Eligible(priority) => Some(*priority),
                _ => None,
            })
            .collect::<std::collections::BTreeSet<_>>();
        for priority in priorities {
            self.select_group(choices, priority, threshold, &mut basis, &mut selected)?;
        }
        if basis.len() != self.0.nrows() {
            return Err(DenseBasisError::Rank);
        }
        let dependent = (0..choices.len())
            .filter(|&c| selected[c])
            .collect::<Vec<_>>();
        if !dependent.is_empty() {
            let matrix = DMatrix::from_fn(self.0.nrows(), dependent.len(), |r, c| {
                self.0[(r, dependent[c])]
            });
            if !self.is_full_column_rank(matrix) {
                return Err(DenseBasisError::Rank);
            }
        }
        Ok((0..choices.len()).filter(|&c| !selected[c]).collect())
    }

    fn select_group(
        &self,
        choices: &[ColumnChoice],
        priority: u8,
        threshold: f64,
        basis: &mut Vec<DVector<f64>>,
        selected: &mut [bool],
    ) -> Result<(), DenseBasisError> {
        let mut remaining = choices
            .iter()
            .enumerate()
            .filter(|&(column, choice)| {
                !selected[column] && *choice == ColumnChoice::Eligible(priority)
            })
            .map(|(column, _)| (column, remainder(self.0.column(column).into_owned(), basis)))
            .collect::<Vec<_>>();
        while basis.len() < self.0.nrows() {
            let Some((index, norm)) = largest_remainder(&remaining) else {
                break;
            };
            if norm <= threshold {
                break;
            }
            let (column, vector) = remaining.swap_remove(index);
            append_basis(basis, remainder(vector, basis), threshold)?;
            selected[column] = true;
            let direction = basis.last().expect("appended basis direction");
            for (_, vector) in &mut remaining {
                subtract_direction(vector, direction);
            }
        }
        Ok(())
    }

    /// Confirm a square dependent block spans its own column count. Column-pivoted
    /// QR is rank revealing: the pivot ordering places the pivot magnitudes on the
    /// diagonal in descending order, so the leading entry is the block's largest
    /// and sets the scale. The block is full rank exactly when every pivot clears
    /// the leading-pivot-scaled threshold, the same acceptance the spectral-norm
    /// rank test applied, without computing a full singular value decomposition.
    fn is_full_column_rank(&self, matrix: DMatrix<f64>) -> bool {
        let columns = matrix.ncols();
        let r = matrix.col_piv_qr().r();
        let threshold = self.threshold(r[(0, 0)].abs());
        (0..columns).all(|index| r[(index, index)].abs() > threshold)
    }

    /// Reciprocal conditioning of the `dependent` columns of this stage,
    /// measured against the whole stage's pivot scale. The stage's
    /// column-pivoted QR sets the scale: its leading pivot `r[(0, 0)]` is the
    /// largest column magnitude across every column, dependent and independent.
    /// The dependent submatrix has its own column-pivoted QR whose smallest
    /// diagonal pivot is the reconstruction's weakest direction. Their ratio
    /// estimates `1/cond` in the same relative pivot scale
    /// [`Self::is_full_column_rank`] applies, so `rcond <= singular_threshold`
    /// is exactly the rank loss the acceptance rejects. Measuring the dependent
    /// pivot against the full stage rather than the submatrix's own leading
    /// pivot is what lets a single-column reconstruction chart register as
    /// folding: a lone dependent column that collapses toward zero has a
    /// unit within-block ratio but a vanishing ratio against the stage.
    pub fn dependent_conditioning(
        &self,
        dependent: &[usize],
    ) -> Result<DependentConditioning, DenseBasisError> {
        if self.0.nrows() == 0 || self.0.ncols() == 0 {
            return Err(DenseBasisError::Shape);
        }
        if dependent.is_empty() || dependent.len() > self.0.ncols() {
            return Err(DenseBasisError::Shape);
        }
        let mut seen = vec![false; self.0.ncols()];
        for &column in dependent {
            let slot = seen.get_mut(column).ok_or(DenseBasisError::Shape)?;
            if std::mem::replace(slot, true) {
                return Err(DenseBasisError::Shape);
            }
        }
        let stage_scale = self.0.clone().col_piv_qr().r()[(0, 0)].abs();
        let block = DMatrix::from_fn(self.0.nrows(), dependent.len(), |r, c| {
            self.0[(r, dependent[c])]
        });
        let pivots = block.nrows().min(block.ncols());
        let block_r = block.col_piv_qr().r();
        let smallest = (0..pivots)
            .map(|index| block_r[(index, index)].abs())
            .fold(f64::INFINITY, f64::min);
        let rcond = if stage_scale > 0.0 && smallest.is_finite() {
            smallest / stage_scale
        } else {
            0.0
        };
        Ok(DependentConditioning {
            rcond,
            singular_threshold: self.threshold(1.0),
        })
    }

    fn threshold(&self, scale: f64) -> f64 {
        self.0.nrows().max(self.0.ncols()) as f64 * f64::EPSILON * scale
    }
}

fn largest_remainder(remaining: &[(usize, DVector<f64>)]) -> Option<(usize, f64)> {
    let mut best = None;
    for (index, (_, vector)) in remaining.iter().enumerate() {
        let norm = vector.norm();
        if best.is_none_or(|(_, previous)| norm > previous) {
            best = Some((index, norm));
        }
    }
    best
}

fn subtract_direction(vector: &mut DVector<f64>, direction: &DVector<f64>) {
    for _ in 0..2 {
        vector.axpy(-direction.dot(vector), direction, 1.);
    }
}

fn decompose(matrix: DMatrix<f64>) -> Result<SVD<f64, Dyn, Dyn>, DenseBasisError> {
    SVD::try_new(matrix, true, true, f64::EPSILON, 4096).ok_or(DenseBasisError::Decomposition)
}

fn remainder(mut vector: DVector<f64>, basis: &[DVector<f64>]) -> DVector<f64> {
    for _ in 0..2 {
        for direction in basis {
            vector.axpy(-direction.dot(&vector), direction, 1.);
        }
    }
    vector
}

fn append_basis(
    basis: &mut Vec<DVector<f64>>,
    vector: DVector<f64>,
    threshold: f64,
) -> Result<(), DenseBasisError> {
    let norm = vector.norm();
    if !norm.is_finite() {
        return Err(DenseBasisError::NonFinite);
    }
    if norm <= threshold {
        return Err(DenseBasisError::Rank);
    }
    basis.push(vector / norm);
    Ok(())
}
