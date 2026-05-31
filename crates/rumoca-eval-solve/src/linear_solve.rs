use crate::{EvalSolveError, get};
use rumoca_solver::{
    LinearSolveKernel, matrix_is_diagonal, matrix_nonzeros, select_linear_solve_kernel,
};

pub(super) fn solve_component(
    regs: &[f64],
    initialized: &[bool],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    component: usize,
) -> Result<f64, EvalSolveError> {
    if n == 0 || component >= n {
        return Ok(0.0);
    }
    let mut matrix = build_augmented_matrix(regs, initialized, matrix_start, rhs_start, n)?;
    if gaussian_eliminate(&mut matrix).is_none() {
        return Ok(0.0);
    }
    Ok(matrix[component][n])
}

pub(super) fn solve_component_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    component: usize,
) -> f64 {
    if n == 0 || component >= n {
        return 0.0;
    }
    let mut matrix = build_augmented_matrix_unchecked(regs, matrix_start, rhs_start, n);
    if gaussian_eliminate(&mut matrix).is_none() {
        return 0.0;
    }
    matrix[component][n]
}

pub(crate) fn solve_all_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    out: &mut [f64],
) {
    let len = n.min(out.len());
    if len == 0 {
        return;
    }
    let start = matrix_start as usize;
    let matrix_len = n.saturating_mul(n);
    let matrix_values = &regs[start..start + matrix_len];
    let diagonal = matrix_is_diagonal(matrix_values, n, 1.0e-14);
    let nonzeros = matrix_nonzeros(matrix_values, 1.0e-14);
    match select_linear_solve_kernel(n, diagonal, nonzeros) {
        LinearSolveKernel::Diagonal => {
            solve_diagonal_unchecked(regs, matrix_start, rhs_start, n, out)
        }
        LinearSolveKernel::SmallDense
        | LinearSolveKernel::Dense
        | LinearSolveKernel::SparseCandidate => {
            let mut matrix = build_augmented_matrix_unchecked(regs, matrix_start, rhs_start, n);
            if gaussian_eliminate(&mut matrix).is_none() {
                out[..len].fill(0.0);
                return;
            }
            for (component, dst) in out.iter_mut().take(len).enumerate() {
                *dst = matrix[component][n];
            }
        }
    }
}

fn solve_diagonal_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    out: &mut [f64],
) {
    for (component, dst) in out.iter_mut().take(n).enumerate() {
        let coeff = regs[matrix_start as usize + component * n + component];
        if coeff.abs() <= 1.0e-14 {
            *dst = 0.0;
        } else {
            *dst = regs[rhs_start as usize + component] / coeff;
        }
    }
}

fn build_augmented_matrix(
    regs: &[f64],
    initialized: &[bool],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
) -> Result<Vec<Vec<f64>>, EvalSolveError> {
    let mut matrix = vec![vec![0.0; n + 1]; n];
    for (row_idx, row) in matrix.iter_mut().enumerate() {
        for (col_idx, slot) in row.iter_mut().take(n).enumerate() {
            *slot = get(
                regs,
                initialized,
                matrix_start + (row_idx * n + col_idx) as u32,
            )?;
        }
        row[n] = get(regs, initialized, rhs_start + row_idx as u32)?;
    }
    Ok(matrix)
}

fn build_augmented_matrix_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
) -> Vec<Vec<f64>> {
    let mut matrix = vec![vec![0.0; n + 1]; n];
    for (row_idx, row) in matrix.iter_mut().enumerate() {
        for (col_idx, slot) in row.iter_mut().take(n).enumerate() {
            *slot = regs[matrix_start as usize + row_idx * n + col_idx];
        }
        row[n] = regs[rhs_start as usize + row_idx];
    }
    matrix
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
