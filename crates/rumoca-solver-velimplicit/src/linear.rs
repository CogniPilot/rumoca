//! Dense LU with partial pivoting for the small iteration matrix `I - h J`.

/// Row-major LU factors of a square matrix and its row permutation.
#[derive(Debug, Default)]
pub(crate) struct DenseLu {
    width: usize,
    factors: Vec<f64>,
    pivots: Vec<usize>,
}

impl DenseLu {
    /// Factor `matrix` (row-major, `width` x `width`) in place of the previous
    /// factors. Returns `false` when a pivot is zero or not finite.
    pub(crate) fn factor(&mut self, matrix: &[f64], width: usize) -> bool {
        self.width = width;
        self.factors.clear();
        self.factors.extend_from_slice(matrix);
        self.pivots.clear();
        self.pivots.extend(0..width);
        for k in 0..width {
            let (pivot_row, pivot_abs) = largest_in_column(&self.factors, width, k);
            if !(pivot_abs.is_finite() && pivot_abs > 0.0) {
                return false;
            }
            if pivot_row != k {
                swap_rows(&mut self.factors, width, k, pivot_row);
                self.pivots.swap(k, pivot_row);
            }
            eliminate_below(&mut self.factors, width, k);
        }
        true
    }

    /// Solve `A x = b` with the stored factors, writing `x` into `out`.
    pub(crate) fn solve_into(&self, rhs: &[f64], out: &mut [f64]) {
        let width = self.width;
        let a = &self.factors;
        for (row, value) in out.iter_mut().enumerate() {
            *value = rhs[self.pivots[row]];
        }
        for row in 0..width {
            let mut sum = out[row];
            for col in 0..row {
                sum -= a[row * width + col] * out[col];
            }
            out[row] = sum;
        }
        for row in (0..width).rev() {
            let mut sum = out[row];
            for col in row + 1..width {
                sum -= a[row * width + col] * out[col];
            }
            out[row] = sum / a[row * width + row];
        }
    }
}

/// Row index and magnitude of the largest entry of column `k` at or below the
/// diagonal.
fn largest_in_column(a: &[f64], width: usize, k: usize) -> (usize, f64) {
    (k..width)
        .map(|row| (row, a[row * width + k].abs()))
        .fold((k, -1.0), |best, candidate| {
            if candidate.1 > best.1 {
                candidate
            } else {
                best
            }
        })
}

fn swap_rows(a: &mut [f64], width: usize, first: usize, second: usize) {
    for col in 0..width {
        a.swap(first * width + col, second * width + col);
    }
}

/// Store the multipliers of column `k` and update the trailing rows.
fn eliminate_below(a: &mut [f64], width: usize, k: usize) {
    let pivot = a[k * width + k];
    for row in k + 1..width {
        let factor = a[row * width + k] / pivot;
        a[row * width + k] = factor;
        for col in k + 1..width {
            a[row * width + col] -= factor * a[k * width + col];
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solves_a_system_that_needs_a_row_swap() {
        let matrix = [0.0, 2.0, 1.0, 3.0, 1.0, 0.0, 1.0, 0.0, 4.0];
        let expected = [1.0, -2.0, 0.5];
        let rhs: Vec<f64> = (0..3)
            .map(|row| {
                (0..3)
                    .map(|col| matrix[row * 3 + col] * expected[col])
                    .sum()
            })
            .collect();
        let mut lu = DenseLu::default();
        assert!(lu.factor(&matrix, 3));
        let mut out = [0.0; 3];
        lu.solve_into(&rhs, &mut out);
        for (got, want) in out.iter().zip(expected) {
            assert!((got - want).abs() < 1.0e-14);
        }
    }

    #[test]
    fn a_singular_matrix_is_reported() {
        let mut lu = DenseLu::default();
        assert!(!lu.factor(&[1.0, 2.0, 2.0, 4.0], 2));
    }
}
