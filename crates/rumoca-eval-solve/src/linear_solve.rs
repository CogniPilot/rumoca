use crate::{EvalSolveError, get};
use rumoca_solver::{
    LinearSolveKernel, TensorPolicyError, matrix_is_diagonal, matrix_nonzeros,
    select_linear_solve_kernel,
};

/// Evaluate a [`LinearOp::LinearSolveComponent`] op directly, destructuring its
/// fields. Keeps the interpreter's `eval_op` match arm to a single line.
pub(super) fn solve_component_op(
    regs: &[f64],
    initialized: &[bool],
    op: rumoca_ir_solve::LinearOp,
) -> Result<f64, EvalSolveError> {
    if let rumoca_ir_solve::LinearOp::LinearSolveComponent {
        matrix_start,
        rhs_start,
        n,
        component,
        ..
    } = op
    {
        solve_component(regs, initialized, matrix_start, rhs_start, n, component)
    } else {
        Err(EvalSolveError::InvalidLinearOp {
            helper: "linear solve component",
            op: op.kind_name(),
        })
    }
}

pub(super) fn solve_component(
    regs: &[f64],
    initialized: &[bool],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    component: usize,
) -> Result<f64, EvalSolveError> {
    validate_component(n, component)?;
    let mut matrix = build_augmented_matrix(regs, initialized, matrix_start, rhs_start, n)?;
    if gaussian_eliminate(&mut matrix).is_none() {
        return Err(linear_solve_error(n, Some(component), "singular matrix"));
    }
    Ok(matrix.solution_component(component))
}

pub(super) fn solve_component_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    component: usize,
) -> Result<f64, EvalSolveError> {
    validate_component(n, component)?;
    let mut matrix = build_augmented_matrix_unchecked(regs, matrix_start, rhs_start, n)?;
    if gaussian_eliminate(&mut matrix).is_none() {
        return Err(linear_solve_error(n, Some(component), "singular matrix"));
    }
    Ok(matrix.solution_component(component))
}

pub(crate) fn solve_all_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    if n == 0 {
        return Ok(());
    }
    if out.len() < n {
        return Err(EvalSolveError::OutputTooSmall {
            required: n,
            len: out.len(),
            span: None,
        });
    }
    let start = matrix_start as usize;
    let matrix_len = checked_product(n, n, "linear solve matrix")?;
    let matrix_end = checked_register_end(matrix_start, matrix_len)?;
    let matrix_values = regs
        .get(start..matrix_end)
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: range_end_register(matrix_start, matrix_len),
            len: regs.len(),
            span: None,
        })?;
    ensure_register_range(regs, "read", rhs_start, n)?;
    let diagonal = matrix_is_diagonal(matrix_values, n, 1.0e-14).map_err(tensor_policy_error)?;
    let nonzeros = matrix_nonzeros(matrix_values, 1.0e-14);
    match select_linear_solve_kernel(n, diagonal, nonzeros).map_err(tensor_policy_error)? {
        LinearSolveKernel::Diagonal => {
            solve_diagonal_unchecked(regs, matrix_start, rhs_start, n, out)?;
        }
        LinearSolveKernel::SmallDense
        | LinearSolveKernel::Dense
        | LinearSolveKernel::SparseCandidate => {
            let mut matrix = build_augmented_matrix_unchecked(regs, matrix_start, rhs_start, n)?;
            if gaussian_eliminate(&mut matrix).is_none() {
                return Err(linear_solve_error(n, None, "singular matrix"));
            }
            for (component, dst) in out.iter_mut().take(n).enumerate() {
                *dst = matrix.solution_component(component);
            }
        }
    }
    Ok(())
}

pub(crate) fn tensor_policy_error(error: TensorPolicyError) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: format!("tensor policy failed: {error}"),
        span: None,
    }
}

fn solve_diagonal_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    for (component, dst) in out.iter_mut().take(n).enumerate() {
        let coeff = regs[matrix_start as usize + component * n + component];
        if coeff.abs() <= 1.0e-14 {
            return Err(linear_solve_error(n, Some(component), "singular diagonal"));
        }
        *dst = regs[rhs_start as usize + component] / coeff;
    }
    Ok(())
}

fn validate_component(n: usize, component: usize) -> Result<(), EvalSolveError> {
    if n == 0 {
        return Err(linear_solve_error(n, Some(component), "empty system"));
    }
    if component >= n {
        return Err(linear_solve_error(
            n,
            Some(component),
            "component is outside the solution vector",
        ));
    }
    Ok(())
}

fn linear_solve_error(
    size: usize,
    component: Option<usize>,
    reason: &'static str,
) -> EvalSolveError {
    EvalSolveError::LinearSolve {
        size,
        component,
        reason,
        span: None,
    }
}

fn build_augmented_matrix(
    regs: &[f64],
    initialized: &[bool],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
) -> Result<AugmentedMatrix, EvalSolveError> {
    let mut matrix = AugmentedMatrix::zeroed(n)?;
    for row_idx in 0..n {
        for col_idx in 0..n {
            let offset = checked_matrix_offset(row_idx, n, col_idx)?;
            let register = checked_register_at_offset(matrix_start, offset, regs.len())?;
            let value = get(regs, initialized, register, None)?;
            matrix.set(row_idx, col_idx, value);
        }
        let rhs_register = checked_register_at_offset(rhs_start, row_idx, regs.len())?;
        let rhs = get(regs, initialized, rhs_register, None)?;
        matrix.set(row_idx, n, rhs);
    }
    Ok(matrix)
}

fn checked_product(
    lhs: usize,
    rhs: usize,
    operation: &'static str,
) -> Result<usize, EvalSolveError> {
    lhs.checked_mul(rhs)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("{operation} shape product {lhs} * {rhs} overflows register range"),
            span: None,
        })
}

fn checked_register_end(start: u32, len: usize) -> Result<usize, EvalSolveError> {
    let start_index = start as usize;
    start_index
        .checked_add(len)
        .ok_or_else(|| EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: range_end_register(start, len),
            len: usize::MAX,
            span: None,
        })
}

fn ensure_register_range(
    regs: &[f64],
    access: &'static str,
    start: u32,
    len: usize,
) -> Result<(), EvalSolveError> {
    let end = checked_register_end(start, len)?;
    if end <= regs.len() {
        return Ok(());
    }
    Err(EvalSolveError::RegisterOutOfBounds {
        access,
        register: range_end_register(start, len),
        len: regs.len(),
        span: None,
    })
}

fn range_end_register(start: u32, len: usize) -> u32 {
    let Some(tail) = len.checked_sub(1) else {
        return start;
    };
    let Ok(tail) = u32::try_from(tail) else {
        return u32::MAX;
    };
    start.saturating_add(tail)
}

fn build_augmented_matrix_unchecked(
    regs: &[f64],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
) -> Result<AugmentedMatrix, EvalSolveError> {
    let mut matrix = AugmentedMatrix::zeroed(n)?;
    for row_idx in 0..n {
        for col_idx in 0..n {
            let offset = checked_matrix_offset(row_idx, n, col_idx)?;
            let index = checked_register_index_at_offset(matrix_start, offset, regs.len())?;
            matrix.set(row_idx, col_idx, regs[index]);
        }
        let rhs_index = checked_register_index_at_offset(rhs_start, row_idx, regs.len())?;
        matrix.set(row_idx, n, regs[rhs_index]);
    }
    Ok(matrix)
}

pub(crate) struct AugmentedMatrix {
    values: Vec<f64>,
    n: usize,
}

impl AugmentedMatrix {
    pub(crate) fn zeroed(n: usize) -> Result<Self, EvalSolveError> {
        let len = checked_augmented_matrix_len(n)?;
        let mut values = Vec::new();
        reserve_linear_solve_capacity(&mut values, len)?;
        values.resize(len, 0.0);
        Ok(Self { values, n })
    }

    fn index(&self, row: usize, col: usize) -> usize {
        row * self.stride() + col
    }

    fn stride(&self) -> usize {
        self.n + 1
    }

    pub(crate) fn get(&self, row: usize, col: usize) -> f64 {
        self.values[self.index(row, col)]
    }

    pub(crate) fn set(&mut self, row: usize, col: usize, value: f64) {
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

    fn solution_component(&self, component: usize) -> f64 {
        self.get(component, self.n)
    }
}

pub(crate) fn gaussian_eliminate(matrix: &mut AugmentedMatrix) -> Option<()> {
    let n = matrix.n;
    for col in 0..n {
        let pivot = (col..n).max_by(|&a, &b| {
            matrix
                .get(a, col)
                .abs()
                .total_cmp(&matrix.get(b, col).abs())
        })?;
        if matrix.get(pivot, col).abs() <= 1.0e-14 {
            return None;
        }
        matrix.swap_rows(col, pivot);
        normalize_pivot_row(matrix, col);
        eliminate_column(matrix, col);
    }
    Some(())
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

fn checked_augmented_matrix_len(n: usize) -> Result<usize, EvalSolveError> {
    let stride = n
        .checked_add(1)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("linear solve augmented matrix stride {n} + 1 overflows"),
            span: None,
        })?;
    n.checked_mul(stride)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!(
                "linear solve augmented matrix shape product {n} * {stride} overflows"
            ),
            span: None,
        })
}

fn checked_matrix_offset(row: usize, n: usize, col: usize) -> Result<usize, EvalSolveError> {
    let row_start = row
        .checked_mul(n)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("linear solve matrix row offset {row} * {n} overflows"),
            span: None,
        })?;
    row_start
        .checked_add(col)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("linear solve matrix offset {row_start} + {col} overflows"),
            span: None,
        })
}

fn checked_register_at_offset(
    start: u32,
    offset: usize,
    len: usize,
) -> Result<u32, EvalSolveError> {
    let offset = u32::try_from(offset).map_err(|_| EvalSolveError::RegisterOutOfBounds {
        access: "read",
        register: u32::MAX,
        len,
        span: None,
    })?;
    start
        .checked_add(offset)
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: u32::MAX,
            len,
            span: None,
        })
}

fn checked_register_index_at_offset(
    start: u32,
    offset: usize,
    len: usize,
) -> Result<usize, EvalSolveError> {
    let start = start as usize;
    let index = start
        .checked_add(offset)
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: u32::MAX,
            len,
            span: None,
        })?;
    if index < len {
        return Ok(index);
    }
    Err(EvalSolveError::RegisterOutOfBounds {
        access: "read",
        register: register_at_offset_for_error(start as u32, offset),
        len,
        span: None,
    })
}

fn register_at_offset_for_error(start: u32, offset: usize) -> u32 {
    let Ok(offset) = u32::try_from(offset) else {
        return u32::MAX;
    };
    start.saturating_add(offset)
}

fn reserve_linear_solve_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
) -> Result<(), EvalSolveError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| EvalSolveError::Scalarization {
            message: format!("linear solve augmented matrix capacity {capacity} overflows"),
            span: None,
        })
}
