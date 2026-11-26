//! Exact register-independent operation identity for shared-value proofs.

use super::super::*;
use super::registers::{Role, read_registers, records_operand_offsets, renamable, visit_registers};

/// Shapes of compiler operations and the builder's solver-slot reads occupy
/// distinct domains. Operation identity uses the complete typed IR encoding,
/// including floating-point bits.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum ShapeKey {
    Slot,
    Slots,
    Conditional,
    Operation(Box<[u8]>),
}

impl ShapeKey {
    fn operation(op: &LinearOp) -> Option<Self> {
        bincode::serialize(op)
            .ok()
            .map(|bytes| Self::Operation(bytes.into_boxed_slice()))
    }
}

/// The register-independent form of one value operation, derived once: its
/// shape with register fields normalized, and each operand as (field or
/// register offset, register). An operation whose sources are single
/// registers keys each operand in field order; any other keys every register
/// it reads by its offset from the lowest one, so a uniform shift of its
/// registers keeps the key.
#[derive(Clone, Debug)]
pub(super) struct ValueShape {
    pub(super) shape: ShapeKey,
    pub(super) operands: Vec<(usize, Reg)>,
    /// Every register the operation reads, ascending.
    pub(super) reads: Vec<Reg>,
    /// Whether a source is a register range, which a segment gathers into
    /// the operation's own layout.
    pub(super) ranged: bool,
}

/// The shape of `op`; `None` when its operands or encoding cannot be issued.
pub(super) fn value_shape(op: &LinearOp) -> Option<ValueShape> {
    let reads = read_registers(op)?;
    if !renamable(op) {
        let lowest = reads.first().copied().unwrap_or(0);
        let operands = reads.iter().map(|&r| ((r - lowest) as usize, r)).collect();
        return Some(ValueShape {
            shape: ShapeKey::operation(op)?,
            operands,
            reads,
            ranged: true,
        });
    }
    let ranged = records_operand_offsets(op);
    let mut scalar = Vec::new();
    let lowest = reads.first().copied().unwrap_or(0);
    let mut normalized = op.clone();
    visit_registers(&mut normalized, &mut |role, register| {
        if role == Role::Scalar {
            scalar.push(*register);
        }
        *register = match role {
            Role::Destination => 0,
            Role::Scalar if !ranged => 0,
            _ => *register - lowest,
        };
    });
    let operands = if ranged {
        reads.iter().map(|&r| ((r - lowest) as usize, r)).collect()
    } else {
        scalar.into_iter().enumerate().collect()
    };
    Some(ValueShape {
        shape: ShapeKey::operation(&normalized)?,
        operands,
        reads,
        ranged,
    })
}
