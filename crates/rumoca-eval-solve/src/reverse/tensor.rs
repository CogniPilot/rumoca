//! Reverse rules for primal (single-lane) tensor operations.
//!
//! Each rule is the transpose of the operation's forward arithmetic, read from
//! the same register layout the canonical evaluator uses (`native_tensors`,
//! `eval_lazy_tensor_load`, the `DotProduct` row), so the recorded tape is the
//! canonical primal bit for bit. Interleaved dual lanes (`lanes != 1`) and
//! seeded loads belong to forward-mode products and stay refused here.
//!
//! | Operation | Adjoint contribution |
//! |---|---|
//! | `TensorLoad` y/p range | `cot[input_start + e] += adj[dst + e]` |
//! | `DotProduct` `d = Σ l·r` | `l̄ += d̄·r`, `r̄ += d̄·l` |
//! | `MatrixMultiply` `C = A·B` | `Ā += C̄·Bᵀ`, `B̄ += Aᵀ·C̄` |
//! | `TensorBinary` | the scalar `Binary` partials, per element (a zero stride accumulates the broadcast operand) |
//! | `TensorTranspose` | the adjoint transposed back to the source layout |
//! | `TensorFill` | the sum of the filled elements' adjoints onto the value |
//! | `TensorIdentity` | none: a constant |

use super::{
    ReverseCotangents, ReverseInputs, accumulate, add_adj, binary_partials, load, reg, set,
    skips_untaken, take_adj,
};
use crate::EvalSolveError;
use rumoca_ir_solve::{LinearOp, Reg, TensorInputKind};

#[cfg(test)]
mod tests;

/// Whether this module owns the reverse rule of `op`.
pub(super) fn supported(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::TensorLoad {
            lanes: 1,
            seed_start: None,
            ..
        } | LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { lanes: 1, .. }
            | LinearOp::TensorBinary { lanes: 1, .. }
            | LinearOp::TensorTranspose { lanes: 1, .. }
            | LinearOp::TensorFill { lanes: 1, .. }
            | LinearOp::TensorIdentity { lanes: 1, .. }
    )
}

/// Record the primal value of one supported tensor operation.
pub(super) fn forward(
    op: &LinearOp,
    inputs: &ReverseInputs<'_>,
    regs: &mut [f64],
) -> Result<(), EvalSolveError> {
    refuse_overlap(op)?;
    match op {
        LinearOp::TensorLoad { .. } => forward_load(op, inputs, regs)?,
        LinearOp::DotProduct { .. } => forward_dot(op, regs),
        LinearOp::MatrixMultiply { .. } => forward_matmul(op, regs),
        LinearOp::TensorBinary { .. } => forward_binary(op, regs),
        LinearOp::TensorTranspose { .. } => forward_transpose(op, regs),
        LinearOp::TensorFill { .. } => forward_fill(op, regs),
        LinearOp::TensorIdentity { .. } => forward_identity(op, regs),
        _ => unreachable!("tensor reverse dispatch receives only supported operations"),
    }
    Ok(())
}

/// Refuse a destination range that overlaps a source range. The IR admits
/// such an operation and the canonical evaluator buffers it; this tape
/// writes in place, so it would read overwritten operands.
fn refuse_overlap(op: &LinearOp) -> Result<(), EvalSolveError> {
    let overlap = |dst: (Reg, usize), src: (Reg, usize)| {
        let (d0, d1) = (dst.0 as usize, dst.0 as usize + dst.1);
        let (s0, s1) = (src.0 as usize, src.0 as usize + src.1);
        d0 < s1 && s0 < d1
    };
    let overlapping = match *op {
        LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            ..
        } => {
            let dst = (dst_start, rows * columns);
            overlap(dst, (lhs_start, rows * inner)) || overlap(dst, (rhs_start, inner * columns))
        }
        LinearOp::TensorBinary {
            dst_start,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            let extent = |stride: usize| {
                if stride == 0 {
                    1
                } else {
                    (count - 1) * stride + 1
                }
            };
            let dst = (dst_start, count);
            overlap(dst, (lhs_start, extent(lhs_stride)))
                || overlap(dst, (rhs_start, extent(rhs_stride)))
        }
        LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            ..
        } => overlap(
            (dst_start, rows * columns * element_width),
            (src_start, rows * columns * element_width),
        ),
        LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            ..
        } => overlap((dst_start, count), (value_start, 1)),
        _ => false,
    };
    if overlapping {
        return Err(super::unsupported(&format!(
            "reverse-mode AD refuses a `{}` whose destination overlaps a source range",
            op.kind_name()
        )));
    }
    Ok(())
}

fn forward_load(
    op: &LinearOp,
    inputs: &ReverseInputs<'_>,
    regs: &mut [f64],
) -> Result<(), EvalSolveError> {
    let LinearOp::TensorLoad {
        dst_start,
        input,
        input_start,
        count,
        ..
    } = *op
    else {
        unreachable!("forward_load receives a tensor load")
    };
    let (values, name) = input_space(inputs, input);
    for element in 0..count {
        let value = load(values, name, input_start + element)?;
        set(regs, offset(dst_start, element), value);
    }
    Ok(())
}

fn forward_dot(op: &LinearOp, regs: &mut [f64]) {
    let LinearOp::DotProduct {
        dst,
        lhs_start,
        rhs_start,
        count,
        lhs_stride,
        rhs_stride,
    } = *op
    else {
        unreachable!("forward_dot receives a dot product")
    };
    let mut value = 0.0;
    for term in 0..count {
        value += reg(regs, offset(lhs_start, term * lhs_stride))
            * reg(regs, offset(rhs_start, term * rhs_stride));
    }
    set(regs, dst, value);
}

fn forward_matmul(op: &LinearOp, regs: &mut [f64]) {
    let LinearOp::MatrixMultiply {
        dst_start,
        lhs_start,
        rhs_start,
        rows,
        inner,
        columns,
        ..
    } = *op
    else {
        unreachable!("forward_matmul receives a matrix product")
    };
    for row in 0..rows {
        for column in 0..columns {
            let mut value = 0.0;
            for term in 0..inner {
                value += reg(regs, offset(lhs_start, row * inner + term))
                    * reg(regs, offset(rhs_start, term * columns + column));
            }
            set(regs, offset(dst_start, row * columns + column), value);
        }
    }
}

fn forward_binary(op: &LinearOp, regs: &mut [f64]) {
    let LinearOp::TensorBinary {
        dst_start,
        op: binary,
        lhs_start,
        rhs_start,
        count,
        lhs_stride,
        rhs_stride,
        lanes,
    } = *op
    else {
        unreachable!("forward_binary receives a tensor binary")
    };
    for element in 0..count {
        let lhs = reg(regs, offset(lhs_start, element * lhs_stride));
        let rhs = reg(regs, offset(rhs_start, element * rhs_stride));
        let value = crate::eval_tensor_binary_primal(binary, lhs, rhs, lanes);
        set(regs, offset(dst_start, element), value);
    }
}

fn forward_transpose(op: &LinearOp, regs: &mut [f64]) {
    for (dst, src) in transpose_pairs(op) {
        set(regs, dst, reg(regs, src));
    }
}

fn forward_fill(op: &LinearOp, regs: &mut [f64]) {
    let LinearOp::TensorFill {
        dst_start,
        value_start,
        count,
        ..
    } = *op
    else {
        unreachable!("forward_fill receives a tensor fill")
    };
    let value = reg(regs, value_start);
    for element in 0..count {
        set(regs, offset(dst_start, element), value);
    }
}

fn forward_identity(op: &LinearOp, regs: &mut [f64]) {
    let LinearOp::TensorIdentity {
        dst_start, size, ..
    } = *op
    else {
        unreachable!("forward_identity receives an identity")
    };
    for row in 0..size {
        for column in 0..size {
            let value = if row == column { 1.0 } else { 0.0 };
            set(regs, offset(dst_start, row * size + column), value);
        }
    }
}

/// Mutable adjoint state one reverse rule reads and writes.
struct Adjoints<'a> {
    adj: &'a mut [f64],
    untaken: &'a mut [bool],
}

/// Consume the destination adjoints of one supported tensor operation and
/// redistribute them to its operands or to the input cotangents.
pub(super) fn reverse(
    op: &LinearOp,
    regs: &[f64],
    adj: &mut [f64],
    untaken: &mut [bool],
    cot: &mut ReverseCotangents<'_>,
) {
    let mut state = Adjoints { adj, untaken };
    match op {
        LinearOp::TensorLoad { .. } => reverse_load(op, &mut state, cot),
        LinearOp::DotProduct { .. } => reverse_dot(op, regs, &mut state),
        LinearOp::MatrixMultiply { .. } => reverse_matmul(op, regs, &mut state),
        LinearOp::TensorBinary { .. } => reverse_binary(op, regs, &mut state),
        LinearOp::TensorTranspose { .. } => reverse_transpose(op, &mut state),
        LinearOp::TensorFill { .. } => reverse_fill(op, &mut state),
        LinearOp::TensorIdentity { .. } => reverse_identity(op, &mut state),
        _ => unreachable!("tensor reverse dispatch receives only supported operations"),
    }
}

fn reverse_load(op: &LinearOp, state: &mut Adjoints<'_>, cot: &mut ReverseCotangents<'_>) {
    let LinearOp::TensorLoad {
        dst_start,
        input,
        input_start,
        count,
        ..
    } = *op
    else {
        unreachable!("reverse_load receives a tensor load")
    };
    let target = match input {
        TensorInputKind::Y => &mut *cot.y,
        TensorInputKind::P => &mut *cot.p,
    };
    for element in 0..count {
        let dst = offset(dst_start, element);
        state.untaken[dst as usize] = false;
        accumulate(target, input_start + element, take_adj(state.adj, dst));
    }
}

fn reverse_dot(op: &LinearOp, regs: &[f64], state: &mut Adjoints<'_>) {
    let LinearOp::DotProduct {
        dst,
        lhs_start,
        rhs_start,
        count,
        lhs_stride,
        rhs_stride,
    } = *op
    else {
        unreachable!("reverse_dot receives a dot product")
    };
    let dst_adj = take_adj(state.adj, dst);
    let terms: Vec<(Reg, Reg)> = (0..count)
        .map(|term| {
            (
                offset(lhs_start, term * lhs_stride),
                offset(rhs_start, term * rhs_stride),
            )
        })
        .collect();
    let operands: Vec<Reg> = terms.iter().flat_map(|&(l, r)| [l, r]).collect();
    if skips_untaken(state.untaken, state.adj, dst, dst_adj, &operands) {
        return;
    }
    for (lhs, rhs) in terms {
        add_adj(state.adj, lhs, dst_adj * reg(regs, rhs));
        add_adj(state.adj, rhs, dst_adj * reg(regs, lhs));
    }
}

fn reverse_matmul(op: &LinearOp, regs: &[f64], state: &mut Adjoints<'_>) {
    let LinearOp::MatrixMultiply {
        dst_start,
        lhs_start,
        rhs_start,
        rows,
        inner,
        columns,
        ..
    } = *op
    else {
        unreachable!("reverse_matmul receives a matrix product")
    };
    let dst_adj = take_range(state.adj, dst_start, rows * columns);
    for (output, &g) in dst_adj.iter().enumerate() {
        let (row, column) = (output / columns, output % columns);
        let terms: Vec<(Reg, Reg)> = (0..inner)
            .map(|term| {
                (
                    offset(lhs_start, row * inner + term),
                    offset(rhs_start, term * columns + column),
                )
            })
            .collect();
        // Each output is its own dot product: an unselected output passes
        // nothing back and marks exactly its own row and column, as `Binary`.
        let operands: Vec<Reg> = terms.iter().flat_map(|&(l, r)| [l, r]).collect();
        if skips_untaken(
            state.untaken,
            state.adj,
            offset(dst_start, output),
            g,
            &operands,
        ) {
            continue;
        }
        for (lhs, rhs) in terms {
            add_adj(state.adj, lhs, g * reg(regs, rhs));
            add_adj(state.adj, rhs, g * reg(regs, lhs));
        }
    }
}

fn reverse_binary(op: &LinearOp, regs: &[f64], state: &mut Adjoints<'_>) {
    let LinearOp::TensorBinary {
        dst_start,
        op: binary,
        lhs_start,
        rhs_start,
        count,
        lhs_stride,
        rhs_stride,
        ..
    } = *op
    else {
        unreachable!("reverse_binary receives a tensor binary")
    };
    let dst_adj = take_range(state.adj, dst_start, count);
    for (element, &g) in dst_adj.iter().enumerate() {
        let dst = offset(dst_start, element);
        let lhs = offset(lhs_start, element * lhs_stride);
        let rhs = offset(rhs_start, element * rhs_stride);
        if skips_untaken(state.untaken, state.adj, dst, g, &[lhs, rhs]) {
            continue;
        }
        let (dl, dr) = binary_partials(binary, reg(regs, lhs), reg(regs, rhs));
        add_adj(state.adj, lhs, g * dl);
        add_adj(state.adj, rhs, g * dr);
    }
}

fn reverse_transpose(op: &LinearOp, state: &mut Adjoints<'_>) {
    // Each destination element is a copy of one source element: `Move`'s rule.
    for (dst, src) in transpose_pairs(op) {
        let g = take_adj(state.adj, dst);
        if skips_untaken(state.untaken, state.adj, dst, g, &[src]) {
            continue;
        }
        add_adj(state.adj, src, g);
    }
}

fn reverse_fill(op: &LinearOp, state: &mut Adjoints<'_>) {
    let LinearOp::TensorFill {
        dst_start,
        value_start,
        count,
        ..
    } = *op
    else {
        unreachable!("reverse_fill receives a tensor fill")
    };
    // Each element is a copy of the one value: `Move`'s rule per element.
    for element in 0..count {
        let dst = offset(dst_start, element);
        let g = take_adj(state.adj, dst);
        if skips_untaken(state.untaken, state.adj, dst, g, &[value_start]) {
            continue;
        }
        add_adj(state.adj, value_start, g);
    }
}

fn reverse_identity(op: &LinearOp, state: &mut Adjoints<'_>) {
    let LinearOp::TensorIdentity {
        dst_start, size, ..
    } = *op
    else {
        unreachable!("reverse_identity receives an identity")
    };
    for index in 0..size * size {
        let dst = offset(dst_start, index);
        state.untaken[dst as usize] = false;
        take_adj(state.adj, dst);
    }
}

fn input_space<'a>(
    inputs: &ReverseInputs<'a>,
    input: TensorInputKind,
) -> (&'a [f64], &'static str) {
    match input {
        TensorInputKind::Y => (inputs.y, "y"),
        TensorInputKind::P => (inputs.p, "p"),
    }
}

fn offset(start: Reg, index: usize) -> Reg {
    start + index as Reg
}

/// Read and zero a destination range, so a later write to the same registers
/// going backward starts from zero (as `take_adj` does for one register).
fn take_range(adj: &mut [f64], start: Reg, count: usize) -> Vec<f64> {
    (0..count)
        .map(|index| take_adj(adj, offset(start, index)))
        .collect()
}

/// `(destination, source)` registers of a first-two-dimension transpose, in
/// destination order, as `native_tensors::value_at` lays it out.
fn transpose_pairs(op: &LinearOp) -> impl Iterator<Item = (Reg, Reg)> {
    let LinearOp::TensorTranspose {
        dst_start,
        src_start,
        rows,
        columns,
        element_width,
        ..
    } = *op
    else {
        unreachable!("transpose_pairs receives a transpose")
    };
    (0..rows * columns * element_width).map(move |index| {
        let element = index / element_width;
        let (row, column) = (element / columns, element % columns);
        let src = (column * rows + row) * element_width + index % element_width;
        (offset(dst_start, index), offset(src_start, src))
    })
}
