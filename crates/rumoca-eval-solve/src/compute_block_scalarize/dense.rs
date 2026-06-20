use rumoca_ir_solve::{BinaryOp, LinearOp, Reg};

use super::{ScalarizeError, checked_product};

/// Scalarize a `LinSolve` node into a SINGLE self-contained program.
///
/// `setup_ops` (which compute the matrix `A` and rhs `b` into the register file)
/// are emitted ONCE, followed by one `LinearSolveComponent` + `StoreOutput` per
/// solution component. Each component's destination register is allocated
/// monotonically so every `dst` is unique within the program.
pub(super) fn scalarize_linsolve(
    setup_ops: &[LinearOp],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    next_reg: Reg,
    span: rumoca_core::Span,
) -> Result<Vec<LinearOp>, ScalarizeError> {
    let mut ops = cloned_linear_ops(setup_ops, "linsolve", span)?;
    let mut next = next_free_reg(&ops, "linsolve", span)?.max(next_reg);
    for component in 0..n {
        let dst = alloc_reg(&mut next, "linsolve", span)?;
        ops.push(LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            component,
        });
        ops.push(LinearOp::StoreOutput { src: dst });
    }
    Ok(ops)
}

pub(super) struct MatMulScalarizeInput<'a> {
    pub(super) lhs_ops: &'a [LinearOp],
    pub(super) lhs_start: Reg,
    pub(super) rhs_ops: &'a [LinearOp],
    pub(super) rhs_start: Reg,
    pub(super) m: usize,
    pub(super) k: usize,
    pub(super) n: usize,
    pub(super) span: rumoca_core::Span,
}

/// Scalarize a `MatMul` node into a SINGLE self-contained program.
///
/// Operand op-lists are emitted once, followed by one dot-product and
/// `StoreOutput` per result slot in row-major order.
pub(super) fn scalarize_matmul(
    input: MatMulScalarizeInput<'_>,
) -> Result<Vec<LinearOp>, ScalarizeError> {
    let MatMulScalarizeInput {
        lhs_ops,
        lhs_start,
        rhs_ops,
        rhs_start,
        m,
        k,
        n,
        span,
    } = input;
    let output_len = checked_product(m, n, "matmul", span)?;
    checked_product(m, k, "matmul lhs", span)?;
    checked_product(k, n, "matmul rhs", span)?;
    let mut ops = cloned_appended_linear_ops(lhs_ops, rhs_ops, "matmul", span)?;
    let mut next = next_free_reg(&ops, "matmul", span)?;
    if k == 0 {
        let zero = alloc_reg(&mut next, "matmul", span)?;
        ops.push(LinearOp::Const {
            dst: zero,
            value: 0.0,
        });
        for _ in 0..output_len {
            ops.push(LinearOp::StoreOutput { src: zero });
        }
        return Ok(ops);
    }

    for row in 0..m {
        for col in 0..n {
            let source_max = matmul_source_max(lhs_start, rhs_start, row, col, k, n, span)?;
            next = next.max(next_reg_after(source_max, "matmul", span)?);
            let first_lhs = checked_reg_add_usize(
                lhs_start,
                checked_product(row, k, "matmul lhs", span)?,
                "matmul lhs",
                span,
            )?;
            let first_rhs = checked_reg_add_usize(rhs_start, col, "matmul rhs", span)?;
            let mut acc = alloc_reg(&mut next, "matmul", span)?;
            ops.push(LinearOp::Binary {
                dst: acc,
                op: BinaryOp::Mul,
                lhs: first_lhs,
                rhs: first_rhs,
            });

            for ki in 1..k {
                let lhs_base = checked_product(row, k, "matmul lhs", span)?;
                let lhs_offset = checked_index_sum(lhs_base, ki, "matmul lhs", span)?;
                let rhs_base = checked_product(ki, n, "matmul rhs", span)?;
                let rhs_offset = checked_index_sum(rhs_base, col, "matmul rhs", span)?;
                let lhs = checked_reg_add_usize(lhs_start, lhs_offset, "matmul lhs", span)?;
                let rhs = checked_reg_add_usize(rhs_start, rhs_offset, "matmul rhs", span)?;
                let product = alloc_reg(&mut next, "matmul", span)?;
                ops.push(LinearOp::Binary {
                    dst: product,
                    op: BinaryOp::Mul,
                    lhs,
                    rhs,
                });
                let sum = alloc_reg(&mut next, "matmul", span)?;
                ops.push(LinearOp::Binary {
                    dst: sum,
                    op: BinaryOp::Add,
                    lhs: acc,
                    rhs: product,
                });
                acc = sum;
            }

            ops.push(LinearOp::StoreOutput { src: acc });
        }
    }
    Ok(ops)
}

fn cloned_linear_ops(
    ops: &[LinearOp],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<LinearOp>, ScalarizeError> {
    let mut cloned = scalarize_vec_with_capacity(ops.len(), kind, span)?;
    cloned.extend_from_slice(ops);
    Ok(cloned)
}

fn cloned_appended_linear_ops(
    first: &[LinearOp],
    second: &[LinearOp],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<LinearOp>, ScalarizeError> {
    let capacity = checked_index_sum(first.len(), second.len(), kind, span)?;
    let mut cloned = scalarize_vec_with_capacity(capacity, kind, span)?;
    cloned.extend_from_slice(first);
    cloned.extend_from_slice(second);
    Ok(cloned)
}

fn scalarize_vec_with_capacity<T>(
    capacity: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, ScalarizeError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| ScalarizeError::AllocationOverflow {
            kind,
            capacity,
            span,
        })?;
    Ok(values)
}

fn matmul_source_max(
    lhs_start: Reg,
    rhs_start: Reg,
    row: usize,
    col: usize,
    k: usize,
    n: usize,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    let lhs_base = checked_product(row, k, "matmul lhs", span)?;
    let lhs_offset = checked_index_sum(lhs_base, k - 1, "matmul lhs", span)?;
    let rhs_base = checked_product(k - 1, n, "matmul rhs", span)?;
    let rhs_offset = checked_index_sum(rhs_base, col, "matmul rhs", span)?;
    let lhs = checked_reg_add_usize(lhs_start, lhs_offset, "matmul lhs", span)?;
    let rhs = checked_reg_add_usize(rhs_start, rhs_offset, "matmul rhs", span)?;
    Ok(lhs.max(rhs))
}

fn checked_index_sum(
    lhs: usize,
    rhs: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, ScalarizeError> {
    lhs.checked_add(rhs).ok_or(ScalarizeError::IndexOverflow {
        kind,
        lhs,
        rhs,
        span,
    })
}

fn next_free_reg(
    ops: &[LinearOp],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    let max_reg = ops
        .iter()
        .map(|op| max_reg_in_op(op, kind, span))
        .try_fold(None, |current: Option<Reg>, reg| {
            let reg = reg?;
            Ok::<Option<Reg>, ScalarizeError>(Some(current.map_or(reg, |current| current.max(reg))))
        })?;
    match max_reg {
        Some(reg) => next_reg_after(reg, kind, span),
        None => Ok(0),
    }
}

fn max_reg_in_op(
    op: &LinearOp,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    Ok(match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::LoadIndexedP { dst, .. }
        | LinearOp::LoadIndexedSeed { dst, .. } => dst,
        LinearOp::Move { dst, src } | LinearOp::Unary { dst, arg: src, .. } => dst.max(src),
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            dst.max(lhs).max(rhs)
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => dst.max(cond).max(if_true).max(if_false),
        LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            ..
        } => dst
            .max(checked_reg_range_last(
                matrix_start,
                checked_product(n, n, kind, span)?,
                kind,
                span,
            )?)
            .max(checked_reg_range_last(rhs_start, n, kind, span)?),
        LinearOp::TableBounds { dst, table_id, .. } => dst.max(table_id),
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        }
        | LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => dst.max(table_id).max(column).max(input),
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => dst.max(table_id).max(time),
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => dst.max(local_seed).max(global_seed),
        LinearOp::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => dst.max(checked_reg_range_last(state_start, state_len, kind, span)?),
        LinearOp::ImpureRandomInit { dst, seed } => dst.max(seed),
        LinearOp::ImpureRandom { dst, id, .. } => dst.max(id),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => dst.max(id).max(imin).max(imax),
        LinearOp::StoreOutput { src } => src,
    })
}

fn checked_reg_add_usize(
    start: Reg,
    offset: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    let offset = Reg::try_from(offset).map_err(|_| ScalarizeError::RegisterIndexOverflow {
        kind,
        index: offset,
        span,
    })?;
    start
        .checked_add(offset)
        .ok_or(ScalarizeError::RegisterRangeOverflow {
            kind,
            start,
            offset: offset as usize,
            span,
        })
}

fn checked_reg_range_last(
    start: Reg,
    len: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    let Some(offset) = len.checked_sub(1) else {
        return Ok(start);
    };
    checked_reg_add_usize(start, offset, kind, span)
}

fn next_reg_after(
    reg: Reg,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    reg.checked_add(1)
        .ok_or(ScalarizeError::RegisterRangeOverflow {
            kind,
            start: reg,
            offset: 1,
            span,
        })
}

fn alloc_reg(
    next: &mut Reg,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Reg, ScalarizeError> {
    let reg = *next;
    *next = next_reg_after(reg, kind, span)?;
    Ok(reg)
}
