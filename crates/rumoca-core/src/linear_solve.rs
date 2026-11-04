//! Shared dense elimination for source initialization and executable evaluation.

#[derive(Debug)]
pub enum LinearSolveAllocationError {
    Shape { size: usize },
    Capacity { size: usize },
}

impl std::fmt::Display for LinearSolveAllocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shape { size } => write!(
                f,
                "linear solve augmented matrix shape for size {size} overflows"
            ),
            Self::Capacity { size } => {
                write!(f, "linear solve augmented matrix capacity {size} overflows")
            }
        }
    }
}

impl std::error::Error for LinearSolveAllocationError {}

/// Dense augmented `[A | b]` matrix used by the scalar Gaussian-elimination
/// linear solve.
pub struct AugmentedMatrix {
    values: Vec<f64>,
    n: usize,
}

impl AugmentedMatrix {
    pub fn zeroed(n: usize) -> Result<Self, LinearSolveAllocationError> {
        let len = checked_augmented_matrix_len(n)?;
        let mut values = Vec::new();
        values
            .try_reserve_exact(len)
            .map_err(|_| LinearSolveAllocationError::Capacity { size: len })?;
        values.resize(len, 0.0);
        Ok(Self { values, n })
    }

    fn index(&self, row: usize, col: usize) -> usize {
        row * self.stride() + col
    }

    fn stride(&self) -> usize {
        self.n + 1
    }

    pub fn get(&self, row: usize, col: usize) -> f64 {
        self.values[self.index(row, col)]
    }

    pub fn set(&mut self, row: usize, col: usize, value: f64) {
        let index = self.index(row, col);
        self.values[index] = value;
    }

    fn swap_rows(&mut self, lhs: usize, rhs: usize) {
        for col in 0..=self.n {
            let lhs_index = self.index(lhs, col);
            let rhs_index = self.index(rhs, col);
            self.values.swap(lhs_index, rhs_index);
        }
    }

    pub fn solution_component(&self, component: usize) -> f64 {
        self.get(component, self.n)
    }
}

/// Reduce the augmented matrix in place by Gaussian elimination with partial
/// pivoting, returning `None` when the pivot column is numerically singular.
///
/// Golub and Van Loan, "Matrix Computations", 4th ed., algorithm 3.4.1.
pub fn gaussian_eliminate(matrix: &mut AugmentedMatrix) -> Option<()> {
    let n = matrix.n;
    for col in 0..n {
        let pivot = (col..n).max_by(|&a, &b| {
            matrix
                .get(a, col)
                .abs()
                .total_cmp(&matrix.get(b, col).abs())
        })?;
        let pivot_value = matrix.get(pivot, col);
        if pivot_value == 0.0 || !pivot_value.is_finite() {
            return None;
        }
        matrix.swap_rows(col, pivot);
        normalize_pivot_row(matrix, col);
        eliminate_column(matrix, col);
    }
    (0..n)
        .all(|component| matrix.solution_component(component).is_finite())
        .then_some(())
}

fn normalize_pivot_row(matrix: &mut AugmentedMatrix, col: usize) {
    let pivot = matrix.get(col, col);
    for col_idx in col..=matrix.n {
        let index = matrix.index(col, col_idx);
        matrix.values[index] /= pivot;
    }
}

fn eliminate_column(matrix: &mut AugmentedMatrix, col: usize) {
    for row_idx in 0..matrix.n {
        if row_idx == col {
            continue;
        }
        let factor = matrix.get(row_idx, col);
        for col_idx in col..=matrix.n {
            let value_index = matrix.index(row_idx, col_idx);
            matrix.values[value_index] -= factor * matrix.get(col, col_idx);
        }
    }
}

fn checked_augmented_matrix_len(n: usize) -> Result<usize, LinearSolveAllocationError> {
    n.checked_add(1)
        .and_then(|stride| n.checked_mul(stride))
        .ok_or(LinearSolveAllocationError::Shape { size: n })
}
