use rumoca_ir_solve::SparsityPattern;

const DIAGONAL_LINEAR_SOLVE_LIMIT: usize = 64;
const SMALL_DENSE_LINEAR_SOLVE_LIMIT: usize = 16;
const SMALL_DENSE_MATMUL_OPS: usize = 4096;
const SPARSE_DENSITY_LIMIT: f64 = 0.20;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MatMulKernel {
    DiagonalLeft,
    DiagonalRight,
    SmallDense,
    Dense,
    SparseCandidate,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LinearSolveKernel {
    Diagonal,
    SmallDense,
    Dense,
    SparseCandidate,
}

pub fn select_matmul_kernel(
    m: usize,
    k: usize,
    n: usize,
    lhs_sparsity: &SparsityPattern,
    rhs_sparsity: &SparsityPattern,
) -> MatMulKernel {
    if matches!(lhs_sparsity, SparsityPattern::Diagonal) && m == k {
        return MatMulKernel::DiagonalLeft;
    }
    if matches!(rhs_sparsity, SparsityPattern::Diagonal) && k == n {
        return MatMulKernel::DiagonalRight;
    }
    let ops = m.saturating_mul(k).saturating_mul(n);
    if ops <= SMALL_DENSE_MATMUL_OPS {
        return MatMulKernel::SmallDense;
    }
    if sparse_operand_density(lhs_sparsity, m, k)
        .or_else(|| sparse_operand_density(rhs_sparsity, k, n))
        .is_some_and(|density| density <= SPARSE_DENSITY_LIMIT)
    {
        return MatMulKernel::SparseCandidate;
    }
    MatMulKernel::Dense
}

pub fn select_linear_solve_kernel(n: usize, diagonal: bool, nonzeros: usize) -> LinearSolveKernel {
    if diagonal && n <= DIAGONAL_LINEAR_SOLVE_LIMIT {
        return LinearSolveKernel::Diagonal;
    }
    if n <= SMALL_DENSE_LINEAR_SOLVE_LIMIT {
        return LinearSolveKernel::SmallDense;
    }
    let total = n.saturating_mul(n);
    if total > 0 && (nonzeros as f64 / total as f64) <= SPARSE_DENSITY_LIMIT {
        return LinearSolveKernel::SparseCandidate;
    }
    LinearSolveKernel::Dense
}

pub fn matrix_nonzeros(values: &[f64], eps: f64) -> usize {
    values.iter().filter(|value| value.abs() > eps).count()
}

pub fn matrix_is_diagonal(values: &[f64], n: usize, eps: f64) -> bool {
    values.len() >= n.saturating_mul(n)
        && values.iter().enumerate().all(|(idx, value)| {
            let row = idx / n;
            let col = idx % n;
            row == col || value.abs() <= eps
        })
}

fn sparse_operand_density(sparsity: &SparsityPattern, rows: usize, cols: usize) -> Option<f64> {
    let total = rows.saturating_mul(cols);
    if total == 0 {
        return Some(0.0);
    }
    match sparsity {
        SparsityPattern::Dense => None,
        SparsityPattern::Diagonal => Some(rows.min(cols) as f64 / total as f64),
        SparsityPattern::Explicit { nnz } => Some(nnz.len() as f64 / total as f64),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matmul_policy_prefers_diagonal_kernels_when_declared() {
        assert_eq!(
            select_matmul_kernel(3, 3, 2, &SparsityPattern::Diagonal, &SparsityPattern::Dense),
            MatMulKernel::DiagonalLeft
        );
        assert_eq!(
            select_matmul_kernel(2, 3, 3, &SparsityPattern::Dense, &SparsityPattern::Diagonal),
            MatMulKernel::DiagonalRight
        );
    }

    #[test]
    fn linear_solve_policy_uses_dynamic_matrix_shape() {
        let diagonal = [2.0, 0.0, 0.0, 3.0];
        assert!(matrix_is_diagonal(&diagonal, 2, 1.0e-14));
        assert_eq!(
            select_linear_solve_kernel(
                2,
                matrix_is_diagonal(&diagonal, 2, 1.0e-14),
                matrix_nonzeros(&diagonal, 1.0e-14),
            ),
            LinearSolveKernel::Diagonal
        );
    }
}
