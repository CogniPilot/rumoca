use super::solve_ops::RuntimeSolveError;

#[derive(Debug, Clone)]
pub struct PreparedMassMatrix {
    state_count: usize,
    kind: MassMatrixKind,
}

#[derive(Debug, Clone)]
enum MassMatrixKind {
    Identity,
    Diagonal(Vec<f64>),
    Dense {
        matrix: Vec<Vec<f64>>,
        inverse: Vec<Vec<f64>>,
    },
}

impl PreparedMassMatrix {
    pub fn new(mass: &[Vec<f64>], state_count: usize) -> Result<Self, RuntimeSolveError> {
        if state_count == 0 || mass.is_empty() {
            return Ok(Self {
                state_count,
                kind: MassMatrixKind::Identity,
            });
        }
        let matrix = normalized_matrix(mass, state_count);
        if is_identity(&matrix) {
            return Ok(Self {
                state_count,
                kind: MassMatrixKind::Identity,
            });
        }
        if let Some(diagonal) = diagonal_values(&matrix) {
            return Ok(Self {
                state_count,
                kind: MassMatrixKind::Diagonal(diagonal),
            });
        }
        let inverse =
            invert(matrix.clone()).ok_or_else(|| RuntimeSolveError::UnsupportedModel {
                reason: "singular state mass matrix".to_string(),
            })?;
        Ok(Self {
            state_count,
            kind: MassMatrixKind::Dense { matrix, inverse },
        })
    }

    pub fn solve(&self, rhs: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
        let n = self.state_count.min(rhs.len());
        match &self.kind {
            MassMatrixKind::Identity => Ok(rhs[..n].to_vec()),
            MassMatrixKind::Diagonal(diagonal) => diagonal
                .iter()
                .take(n)
                .zip(rhs.iter().copied())
                .map(|(coeff, value)| {
                    if coeff.abs() <= 1.0e-14 {
                        Err(RuntimeSolveError::UnsupportedModel {
                            reason: "singular state mass matrix".to_string(),
                        })
                    } else {
                        Ok(value / coeff)
                    }
                })
                .collect(),
            MassMatrixKind::Dense { inverse, .. } => Ok(mul_matrix_vector(inverse, &rhs[..n])),
        }
    }

    pub fn apply_solver_mass_with_beta(
        &self,
        v: &[f64],
        beta: f64,
        out: &mut [f64],
        solver_count: usize,
    ) {
        let n = self.state_count.min(v.len()).min(out.len());
        match &self.kind {
            MassMatrixKind::Identity => {
                for (slot, value) in out.iter_mut().take(n).zip(v.iter().copied()) {
                    *slot = value + beta * *slot;
                }
            }
            MassMatrixKind::Diagonal(diagonal) => {
                for ((slot, coeff), value) in out
                    .iter_mut()
                    .take(n)
                    .zip(diagonal.iter().copied())
                    .zip(v.iter().copied())
                {
                    *slot = coeff * value + beta * *slot;
                }
            }
            MassMatrixKind::Dense { matrix, .. } => {
                for row_idx in 0..n {
                    let acc = matrix[row_idx]
                        .iter()
                        .take(n)
                        .zip(v.iter().copied())
                        .map(|(coeff, value)| coeff * value)
                        .sum::<f64>();
                    out[row_idx] = acc + beta * out[row_idx];
                }
            }
        }
        for slot in out.iter_mut().take(solver_count).skip(n) {
            *slot *= beta;
        }
    }
}

pub fn solve_mass_matrix(mass: &[Vec<f64>], rhs: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
    PreparedMassMatrix::new(mass, rhs.len())?.solve(rhs)
}

fn normalized_matrix(mass: &[Vec<f64>], n: usize) -> Vec<Vec<f64>> {
    let mut matrix = vec![vec![0.0; n]; n];
    for (row_idx, row) in matrix.iter_mut().enumerate() {
        for (col_idx, slot) in row.iter_mut().enumerate() {
            *slot = mass
                .get(row_idx)
                .and_then(|mass_row| mass_row.get(col_idx))
                .copied()
                .unwrap_or((row_idx == col_idx) as u8 as f64);
        }
    }
    matrix
}

fn is_identity(matrix: &[Vec<f64>]) -> bool {
    matrix.iter().enumerate().all(|(row_idx, row)| {
        row.iter().enumerate().all(|(col_idx, value)| {
            let expected = (row_idx == col_idx) as u8 as f64;
            (*value - expected).abs() <= 1.0e-14
        })
    })
}

fn diagonal_values(matrix: &[Vec<f64>]) -> Option<Vec<f64>> {
    let mut diagonal = Vec::with_capacity(matrix.len());
    for (row_idx, row) in matrix.iter().enumerate() {
        for (col_idx, value) in row.iter().enumerate() {
            if row_idx == col_idx {
                diagonal.push(*value);
            } else if value.abs() > 1.0e-14 {
                return None;
            }
        }
    }
    Some(diagonal)
}

fn invert(matrix: Vec<Vec<f64>>) -> Option<Vec<Vec<f64>>> {
    let n = matrix.len();
    let mut augmented = vec![vec![0.0; n * 2]; n];
    for row_idx in 0..n {
        for col_idx in 0..n {
            augmented[row_idx][col_idx] = matrix[row_idx][col_idx];
        }
        augmented[row_idx][n + row_idx] = 1.0;
    }
    gaussian_eliminate(&mut augmented)?;
    Some(augmented.into_iter().map(|row| row[n..].to_vec()).collect())
}

fn gaussian_eliminate(matrix: &mut [Vec<f64>]) -> Option<()> {
    let n = matrix.len();
    for col in 0..n {
        let pivot =
            (col..n).max_by(|&a, &b| matrix[a][col].abs().total_cmp(&matrix[b][col].abs()))?;
        if matrix[pivot][col].abs() <= 1.0e-14 {
            return None;
        }
        matrix.swap(col, pivot);
        normalize_pivot_row(matrix, col);
        eliminate_column(matrix, col);
    }
    Some(())
}

fn normalize_pivot_row(matrix: &mut [Vec<f64>], col: usize) {
    let pivot = matrix[col][col];
    for value in &mut matrix[col][col..] {
        *value /= pivot;
    }
}

fn eliminate_column(matrix: &mut [Vec<f64>], col: usize) {
    let pivot_row = matrix[col].clone();
    for (row_idx, row) in matrix.iter_mut().enumerate() {
        if row_idx == col {
            continue;
        }
        let factor = row[col];
        for (value, pivot) in row[col..].iter_mut().zip(&pivot_row[col..]) {
            *value -= factor * pivot;
        }
    }
}

fn mul_matrix_vector(matrix: &[Vec<f64>], vector: &[f64]) -> Vec<f64> {
    matrix
        .iter()
        .map(|row| {
            row.iter()
                .zip(vector.iter().copied())
                .map(|(coeff, value)| coeff * value)
                .sum()
        })
        .collect()
}
