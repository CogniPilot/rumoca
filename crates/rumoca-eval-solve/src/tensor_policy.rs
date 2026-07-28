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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TensorPolicyError {
    ShapeProductOverflow {
        context: &'static str,
        lhs: usize,
        rhs: usize,
    },
}

impl std::fmt::Display for TensorPolicyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ShapeProductOverflow { context, lhs, rhs } => {
                write!(f, "{context} shape product {lhs} * {rhs} overflows")
            }
        }
    }
}

impl std::error::Error for TensorPolicyError {}

pub fn select_matmul_kernel(
    m: usize,
    k: usize,
    n: usize,
    lhs_sparsity: &SparsityPattern,
    rhs_sparsity: &SparsityPattern,
) -> Result<MatMulKernel, TensorPolicyError> {
    if matches!(lhs_sparsity, SparsityPattern::Diagonal) && m == k {
        return Ok(MatMulKernel::DiagonalLeft);
    }
    if matches!(rhs_sparsity, SparsityPattern::Diagonal) && k == n {
        return Ok(MatMulKernel::DiagonalRight);
    }
    let lhs_ops = checked_policy_product(m, k, "MatMul")?;
    let ops = checked_policy_product(lhs_ops, n, "MatMul")?;
    if ops <= SMALL_DENSE_MATMUL_OPS {
        return Ok(MatMulKernel::SmallDense);
    }
    if sparse_operand_density(lhs_sparsity, m, k)?
        .or(sparse_operand_density(rhs_sparsity, k, n)?)
        .is_some_and(|density| density <= SPARSE_DENSITY_LIMIT)
    {
        return Ok(MatMulKernel::SparseCandidate);
    }
    Ok(MatMulKernel::Dense)
}

pub fn select_linear_solve_kernel(
    n: usize,
    diagonal: bool,
    nonzeros: usize,
) -> Result<LinearSolveKernel, TensorPolicyError> {
    if diagonal && n <= DIAGONAL_LINEAR_SOLVE_LIMIT {
        return Ok(LinearSolveKernel::Diagonal);
    }
    if n <= SMALL_DENSE_LINEAR_SOLVE_LIMIT {
        return Ok(LinearSolveKernel::SmallDense);
    }
    let total = checked_policy_product(n, n, "linear solve")?;
    if total > 0 && (nonzeros as f64 / total as f64) <= SPARSE_DENSITY_LIMIT {
        return Ok(LinearSolveKernel::SparseCandidate);
    }
    Ok(LinearSolveKernel::Dense)
}

pub fn matrix_nonzeros(values: &[f64], eps: f64) -> usize {
    values.iter().filter(|value| value.abs() > eps).count()
}

pub fn matrix_is_diagonal(values: &[f64], n: usize, eps: f64) -> Result<bool, TensorPolicyError> {
    if n == 0 {
        return Ok(values.is_empty());
    }
    let total = checked_policy_product(n, n, "matrix diagonal check")?;
    Ok(values.len() >= total
        && values.iter().enumerate().all(|(idx, value)| {
            let row = idx / n;
            let col = idx % n;
            row == col || value.abs() <= eps
        }))
}

fn sparse_operand_density(
    sparsity: &SparsityPattern,
    rows: usize,
    cols: usize,
) -> Result<Option<f64>, TensorPolicyError> {
    let total = checked_policy_product(rows, cols, "sparse operand density")?;
    if total == 0 {
        return Ok(Some(0.0));
    }
    Ok(match sparsity {
        SparsityPattern::Dense => None,
        SparsityPattern::Diagonal => Some(rows.min(cols) as f64 / total as f64),
        SparsityPattern::Explicit { nnz } => Some(nnz.len() as f64 / total as f64),
    })
}

fn checked_policy_product(
    lhs: usize,
    rhs: usize,
    context: &'static str,
) -> Result<usize, TensorPolicyError> {
    lhs.checked_mul(rhs)
        .ok_or(TensorPolicyError::ShapeProductOverflow { context, lhs, rhs })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matmul_policy_prefers_diagonal_kernels_when_declared() {
        assert_eq!(
            select_matmul_kernel(3, 3, 2, &SparsityPattern::Diagonal, &SparsityPattern::Dense)
                .expect("diagonal left policy should select"),
            MatMulKernel::DiagonalLeft
        );
        assert_eq!(
            select_matmul_kernel(2, 3, 3, &SparsityPattern::Dense, &SparsityPattern::Diagonal)
                .expect("diagonal right policy should select"),
            MatMulKernel::DiagonalRight
        );
    }

    #[test]
    fn linear_solve_policy_uses_dynamic_matrix_shape() {
        let diagonal = [2.0, 0.0, 0.0, 3.0];
        assert!(matrix_is_diagonal(&diagonal, 2, 1.0e-14).expect("diagonal check should fit"));
        assert_eq!(
            select_linear_solve_kernel(
                2,
                matrix_is_diagonal(&diagonal, 2, 1.0e-14).expect("diagonal check should fit"),
                matrix_nonzeros(&diagonal, 1.0e-14),
            )
            .expect("linear solve policy should select"),
            LinearSolveKernel::Diagonal
        );
    }

    #[test]
    fn tensor_policy_rejects_shape_product_overflow() {
        assert!(matches!(
            select_matmul_kernel(
                usize::MAX,
                2,
                1,
                &SparsityPattern::Dense,
                &SparsityPattern::Dense,
            ),
            Err(TensorPolicyError::ShapeProductOverflow { .. })
        ));
        assert!(matches!(
            matrix_is_diagonal(&[], usize::MAX, 1.0e-14),
            Err(TensorPolicyError::ShapeProductOverflow { .. })
        ));
        assert!(matrix_is_diagonal(&[], 0, 1.0e-14).expect("zero-size diagonal check should fit"));
    }
}
