//! Preserve aggregate read-before-write semantics in bounded scalar views.

use super::{LinearOp, Lowering, Reg};

/// Intersect a contiguous destination with a compact strided source, without
/// enumerating registers or treating gaps in a strided source as reads.
fn intersects(dst: Reg, count: usize, src: Reg, reads: usize, stride: usize) -> Option<bool> {
    if count == 0 || reads == 0 {
        return Some(false);
    }
    let first = usize::try_from(src).ok()?;
    let last = first.checked_add(reads.checked_sub(1)?.checked_mul(stride)?)?;
    Reg::try_from(last).ok()?;
    let low = usize::try_from(dst).ok()?;
    let high = low.checked_add(count.checked_sub(1)?)?;
    Reg::try_from(high).ok()?;
    if last < low || first > high {
        return Some(false);
    }
    if stride == 0 || first >= low {
        return Some(true);
    }
    let distance = low.checked_sub(first)?;
    let index = distance / stride + usize::from(distance % stride != 0);
    Some(index < reads && first.checked_add(index.checked_mul(stride)?)? <= high)
}

/// Number of temporary results needed by an overlapping aggregate. Shapes
/// remain compact; this is admission work, not a scalar register inventory.
pub(super) fn result_count(op: &LinearOp) -> Option<usize> {
    let Some(dst) = op.dst_register() else {
        return Some(0);
    };
    let count = op.dst_register_count();
    if count == 0 {
        return Some(0);
    }
    let overlap = match op {
        LinearOp::TensorBinary {
            lanes: 1,
            lhs_start,
            rhs_start,
            count: reads,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            intersects(dst, count, *lhs_start, *reads, *lhs_stride)?
                | intersects(dst, count, *rhs_start, *reads, *rhs_stride)?
        }
        LinearOp::MatrixMultiply {
            lanes: 1,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            ..
        } => {
            intersects(dst, count, *lhs_start, rows.checked_mul(*inner)?, 1)?
                | intersects(dst, count, *rhs_start, inner.checked_mul(*columns)?, 1)?
        }
        LinearOp::TensorTranspose {
            lanes: 1,
            src_start,
            ..
        } => intersects(dst, count, *src_start, count, 1)?,
        LinearOp::TensorCross {
            lanes: 1,
            lhs_start,
            rhs_start,
            ..
        } => intersects(dst, count, *lhs_start, 3, 1)? | intersects(dst, count, *rhs_start, 3, 1)?,
        _ => false,
    };
    Some(if overlap { count } else { 0 })
}

impl Lowering<'_> {
    /// Compute all aggregate results before committing any destination write.
    /// Admission has already charged both temporary storage and commit moves.
    pub(super) fn lower_with_snapshot(&mut self, op: &LinearOp) -> Option<()> {
        let count = result_count(op)?;
        if count == 0 {
            return self.lower(op);
        }
        let dst = op.dst_register()?;
        let temporary = self.next;
        self.next = Self::reg(temporary, count)?;
        let first = self.out.len();
        self.lower(op)?;
        let end = Self::reg(dst, count)?;
        for scalar in &mut self.out[first..] {
            // These are exactly the result-writing scalar forms issued by the
            // supported aggregate projections; other forms fail closed.
            let scalar_dst = match scalar {
                LinearOp::Const { dst, .. }
                | LinearOp::Move { dst, .. }
                | LinearOp::Binary { dst, .. } => dst,
                _ => return None,
            };
            if *scalar_dst >= dst && *scalar_dst < end {
                *scalar_dst = Self::reg(temporary, usize::try_from(*scalar_dst - dst).ok()?)?;
            }
        }
        for offset in 0..count {
            self.out.push(LinearOp::Move {
                dst: Self::reg(dst, offset)?,
                src: Self::reg(temporary, offset)?,
            });
        }
        Some(())
    }
}
