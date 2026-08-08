use rumoca_ir_solve::{StructuralPattern, StructuralPatternView};

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
    lhs_pattern: &StructuralPattern,
    rhs_pattern: &StructuralPattern,
) -> Result<MatMulKernel, TensorPolicyError> {
    if matches!(lhs_pattern.view(), StructuralPatternView::Diagonal) && m == k {
        return Ok(MatMulKernel::DiagonalLeft);
    }
    if matches!(rhs_pattern.view(), StructuralPatternView::Diagonal) && k == n {
        return Ok(MatMulKernel::DiagonalRight);
    }
    let lhs_ops = checked_policy_product(m, k, "MatMul")?;
    let ops = checked_policy_product(lhs_ops, n, "MatMul")?;
    if ops <= SMALL_DENSE_MATMUL_OPS {
        return Ok(MatMulKernel::SmallDense);
    }
    if sparse_operand_density(lhs_pattern, m, k)?
        .or(sparse_operand_density(rhs_pattern, k, n)?)
        .is_some_and(|density| density <= SPARSE_DENSITY_LIMIT)
    {
        return Ok(MatMulKernel::SparseCandidate);
    }
    Ok(MatMulKernel::Dense)
}

pub fn select_linear_solve_kernel(
    n: usize,
    matrix_pattern: &StructuralPattern,
) -> Result<LinearSolveKernel, TensorPolicyError> {
    if matches!(matrix_pattern.view(), StructuralPatternView::Diagonal)
        && n <= DIAGONAL_LINEAR_SOLVE_LIMIT
    {
        return Ok(LinearSolveKernel::Diagonal);
    }
    if n <= SMALL_DENSE_LINEAR_SOLVE_LIMIT {
        return Ok(LinearSolveKernel::SmallDense);
    }
    let total = checked_policy_product(n, n, "linear solve")?;
    if total > 0
        && matrix_pattern
            .nonzero_upper_bound()
            .is_some_and(|nonzeros| (nonzeros as f64 / total as f64) <= SPARSE_DENSITY_LIMIT)
    {
        return Ok(LinearSolveKernel::SparseCandidate);
    }
    Ok(LinearSolveKernel::Dense)
}

fn sparse_operand_density(
    pattern: &StructuralPattern,
    rows: usize,
    cols: usize,
) -> Result<Option<f64>, TensorPolicyError> {
    let total = checked_policy_product(rows, cols, "sparse operand density")?;
    if total == 0 {
        return Ok(Some(0.0));
    }
    if pattern.rows() as usize != rows || pattern.columns() as usize != cols {
        return Ok(None);
    }
    Ok(match pattern.view() {
        StructuralPatternView::Full => None,
        _ => pattern
            .nonzero_upper_bound()
            .map(|nonzeros| nonzeros as f64 / total as f64),
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

    fn pattern(rows: usize, columns: usize, diagonal: bool) -> StructuralPattern {
        crate::fixture_pattern(rows, columns, diagonal)
    }

    #[test]
    fn matmul_policy_prefers_diagonal_kernels_when_declared() {
        assert_eq!(
            select_matmul_kernel(3, 3, 2, &pattern(3, 3, true), &pattern(3, 2, false))
                .expect("diagonal left policy should select"),
            MatMulKernel::DiagonalLeft
        );
        assert_eq!(
            select_matmul_kernel(2, 3, 3, &pattern(2, 3, false), &pattern(3, 3, true))
                .expect("diagonal right policy should select"),
            MatMulKernel::DiagonalRight
        );
    }

    #[test]
    fn linear_solve_policy_uses_dynamic_matrix_shape() {
        assert_eq!(
            select_linear_solve_kernel(2, &pattern(2, 2, true))
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
                &pattern(1, 1, false),
                &pattern(1, 1, false),
            ),
            Err(TensorPolicyError::ShapeProductOverflow { .. })
        ));
        assert!(matches!(
            select_linear_solve_kernel(usize::MAX, &pattern(1, 1, false)),
            Err(TensorPolicyError::ShapeProductOverflow { .. })
        ));
    }
}
