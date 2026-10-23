//! Shared compact tensor arithmetic and native snapshot commits.

mod copies;
#[cfg(test)]
mod tests;

use super::*;

/// The caller's checked register certificate proves every accessed range.
/// Staging is issued before any write; its storage is reused across evaluations.
pub(super) fn eval(
    regs: &mut [f64],
    results: &mut Vec<f64>,
    op: &LinearOp,
) -> Result<(), EvalSolveError> {
    let snapshot = tensor_snapshot::Snapshot::issue(op)
        .ok_or_else(|| invalid_row("native tensor snapshot geometry overflows"))?;
    let dst = op
        .dst_register()
        .expect("tensor operation has a destination") as usize;
    let count = snapshot.result_width();
    if matches!(
        op,
        LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorUpdate { .. }
            | LinearOp::TensorFill { .. }
    ) {
        return copies::eval(regs, results, op, snapshot);
    }
    if snapshot.width() == 0 {
        for offset in 0..count {
            regs[dst + offset] = value_at(
                op,
                &mut |reg| Ok::<_, EvalSolveError>(regs[reg as usize]),
                offset,
            )?;
        }
        return Ok(());
    }
    reserve_results(results, snapshot.width())?;
    for offset in 0..snapshot.width() {
        results.push(value_at(
            op,
            &mut |reg| Ok::<_, EvalSolveError>(regs[reg as usize]),
            offset,
        )?);
    }
    regs[dst..dst + snapshot.width()].copy_from_slice(results);
    Ok(())
}

fn reserve_results(results: &mut Vec<f64>, width: usize) -> Result<(), EvalSolveError> {
    results.clear();
    results
        .try_reserve_exact(width)
        .map_err(|error| invalid_row(format!("native tensor result allocation failed: {error}")))
}

/// One scalar of an aggregate result, read through the owner's register checks.
/// Both checked and prepared execution use these arithmetic and layout rules.
pub(super) fn value_at<E>(
    op: &LinearOp,
    read: &mut impl FnMut(Reg) -> Result<f64, E>,
    offset: usize,
) -> Result<f64, E> {
    match op {
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
            ..
        } => matrix_value(
            read,
            (*lhs_start, *rhs_start),
            MatrixProductShape {
                rows: *rows,
                inner: *inner,
                columns: *columns,
                lanes: *lanes,
            },
            offset,
        ),
        LinearOp::TensorBinary {
            op,
            lhs_start,
            rhs_start,
            lhs_stride,
            rhs_stride,
            lanes,
            ..
        } => {
            let element = offset / lanes;
            let lane = offset % lanes;
            let lhs = *lhs_start + (element * lhs_stride * lanes) as Reg;
            let rhs = *rhs_start + (element * rhs_stride * lanes) as Reg;
            let (lhs_re, rhs_re) = (read(lhs)?, read(rhs)?);
            if lane == 0 {
                Ok(eval_tensor_binary_primal(*op, lhs_re, rhs_re, *lanes))
            } else {
                Ok(eval_tensor_binary_tangent(
                    *op,
                    lhs_re,
                    read(lhs + lane as Reg)?,
                    rhs_re,
                    read(rhs + lane as Reg)?,
                ))
            }
        }
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes,
            ..
        } => {
            let width = element_width * lanes;
            let element = offset / width;
            let row = element / columns;
            let column = element % columns;
            read(*src_start + ((column * rows + row) * width + offset % width) as Reg)
        }
        _ => unreachable!("native result dispatch accepts only matrix, binary and transpose ops"),
    }
}

fn matrix_value<E>(
    read: &mut impl FnMut(Reg) -> Result<f64, E>,
    inputs: (Reg, Reg),
    shape: MatrixProductShape,
    offset: usize,
) -> Result<f64, E> {
    let MatrixProductShape {
        inner,
        columns,
        lanes,
        ..
    } = shape;
    let element = offset / lanes;
    let lane = offset % lanes;
    let row = element / columns;
    let column = element % columns;
    let mut product = |term: usize| {
        let lhs = inputs.0 + ((row * inner + term) * lanes) as Reg;
        let rhs = inputs.1 + ((term * columns + column) * lanes) as Reg;
        let (lhs_re, rhs_re) = (read(lhs)?, read(rhs)?);
        Ok(if lane == 0 {
            lhs_re * rhs_re
        } else {
            read(lhs + lane as Reg)? * rhs_re + lhs_re * read(rhs + lane as Reg)?
        })
    };
    // An empty dot is `+0.0`. Otherwise the first term is its own sum, as in
    // the scalar expansion, so an all `-0.0` dot keeps its sign.
    if inner == 0 {
        return Ok(0.0);
    }
    let mut value = product(0)?;
    for term in 1..inner {
        value += product(term)?;
    }
    Ok(value)
}
