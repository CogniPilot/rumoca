//! Shared compact tensor arithmetic and native snapshot commits.

mod copies;
#[cfg(test)]
mod integer_power_tests;
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
    if let LinearOp::TensorIntegerPower {
        src_start,
        count: elements,
        power,
        lanes,
        ..
    } = *op
    {
        eval_integer_power(regs, (dst, src_start as usize), elements, power, lanes);
        return Ok(());
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

/// Every element of an elementwise literal power, all lanes at once, written
/// as soon as it is computed: validation refuses a destination that overlaps
/// the source.
fn eval_integer_power(
    regs: &mut [f64],
    (dst, src): (usize, usize),
    elements: usize,
    power: IntegerPower,
    lanes: usize,
) {
    debug_assert!(dst + elements * lanes <= src || src + elements * lanes <= dst);
    let mut element = [0.0; MAX_TENSOR_LANES];
    for index in 0..elements {
        let source = src + index * lanes;
        integer_power_element(power, &regs[source..source + lanes], &mut element[..lanes]);
        let output = dst + index * lanes;
        regs[output..output + lanes].copy_from_slice(&element[..lanes]);
    }
}

/// The lanes of one element of an elementwise literal power: lane 0 the
/// primal plan, each tangent lane the `TensorBinary` division and product
/// tangents of its steps, in their order. Bit for bit the values of the
/// packed `TensorBinary` chain the plan expands to, with two live values
/// (the running base and result) of `source.len()` lanes each.
///
/// # Panics
///
/// When `source` is empty or wider than `MAX_TENSOR_LANES`, or when
/// `output` does not have the length of `source`.
pub fn integer_power_element(power: IntegerPower, source: &[f64], output: &mut [f64]) {
    assert!(
        (1..=MAX_TENSOR_LANES).contains(&source.len()) && output.len() == source.len(),
        "integer power element needs 1 to {MAX_TENSOR_LANES} source lanes and as many outputs, \
         got {} and {}",
        source.len(),
        output.len()
    );
    match source.len() {
        1 => power_lanes::<1>(power, source, output),
        2 => power_lanes::<2>(power, source, output),
        _ => power_lanes::<MAX_TENSOR_LANES>(power, source, output),
    }
}

/// [`integer_power_element`] on stack values of `N >= source.len()` lanes,
/// of which only the first `source.len()` are touched after their creation.
fn power_lanes<const N: usize>(power: IntegerPower, source: &[f64], output: &mut [f64]) {
    let lanes = source.len();
    let (mut base, mut result) = ([0.0; N], [0.0; N]);
    let (base, result) = (&mut base[..lanes], &mut result[..lanes]);
    if power.reciprocal() {
        let element = source[0];
        for lane in 1..lanes {
            base[lane] = eval_tensor_binary_tangent(BinaryOp::Div, 1.0, 0.0, element, source[lane]);
        }
        base[0] = eval_tensor_binary_primal(BinaryOp::Div, 1.0, element, lanes);
    } else {
        base.copy_from_slice(source);
    }
    for step in power.steps() {
        match step {
            IntegerPowerStep::Seed => result.copy_from_slice(base),
            IntegerPowerStep::Multiply => multiply_lanes(result, base),
            IntegerPowerStep::Square => square_lanes(base),
        }
    }
    output.copy_from_slice(result);
}

/// `target = target * factor` as a `TensorBinary` product element: each
/// tangent lane reads the primal lanes before the primal is replaced.
fn multiply_lanes(target: &mut [f64], factor: &[f64]) {
    for lane in 1..target.len() {
        target[lane] = eval_tensor_binary_tangent(
            BinaryOp::Mul,
            target[0],
            target[lane],
            factor[0],
            factor[lane],
        );
    }
    target[0] = eval_tensor_binary_primal(BinaryOp::Mul, target[0], factor[0], target.len());
}

/// `base = base * base`, the same product with both operands the base.
fn square_lanes(base: &mut [f64]) {
    for lane in 1..base.len() {
        base[lane] =
            eval_tensor_binary_tangent(BinaryOp::Mul, base[0], base[lane], base[0], base[lane]);
    }
    base[0] = eval_tensor_binary_primal(BinaryOp::Mul, base[0], base[0], base.len());
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
