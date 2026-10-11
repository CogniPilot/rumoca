//! Preserve aggregate read-before-write semantics in bounded scalar views.

use super::{LinearOp, Lowering};

/// Number of staged values, from the shared compact read-before-write witness.
pub(super) fn result_count(op: &LinearOp) -> Option<usize> {
    Some(crate::tensor_snapshot::Snapshot::issue(op)?.width())
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
