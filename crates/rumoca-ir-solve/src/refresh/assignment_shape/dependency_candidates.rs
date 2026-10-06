use std::collections::BTreeSet;

use super::{ProgramPrefix, tensor_affine::operation::ProjectionOperation};
use crate::{LinearOp, Reg, TensorInputKind};

/// The candidate targets of one output, and whether every operation reachable
/// from it is linear (a sum, difference, negation, or copy). Only a tensor
/// affine certificate, which needs a product step, keeps targets the output
/// itself does not depend on.
pub(super) struct Candidates {
    pub(super) targets: BTreeSet<usize>,
    pub(super) linear: bool,
}

/// Candidate targets of the complete SSA operations reachable from this output.
/// Tensor affine certificates retain whole operations, including coordinates
/// whose isolated coefficient can be zero; scalar-only dependencies would
/// therefore change the canonical certificate inventory.
pub(super) fn derive(program: ProgramPrefix<'_>, output: Reg) -> Option<Candidates> {
    let mut pending = BTreeSet::from([program.producer_position(output)?]);
    let mut visited = BTreeSet::new();
    let mut targets = BTreeSet::new();
    let mut linear = true;
    while let Some(position) = pending.pop_last() {
        if !visited.insert(position) {
            continue;
        }
        let operation = program.operation(position)?;
        match operation {
            LinearOp::LoadY { index, .. } => {
                targets.insert(*index);
            }
            LinearOp::TensorLoad {
                input: TensorInputKind::Y,
                input_start,
                count,
                ..
            } => {
                targets.extend(*input_start..input_start.checked_add(*count)?);
            }
            LinearOp::Const { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::TensorLoad {
                input: TensorInputKind::P,
                ..
            }
            | LinearOp::TensorIdentity { .. } => {}
            _ => {
                linear &= enqueue_operands(program.before(position)?, operation, &mut pending)?;
            }
        }
    }
    Some(Candidates { targets, linear })
}

fn enqueue_operands(
    program: ProgramPrefix<'_>,
    operation: &LinearOp,
    pending: &mut BTreeSet<usize>,
) -> Option<bool> {
    // A tensor affine certificate passes an operation it cannot project only
    // when that operation is independent of the target, so such an
    // operation contributes no candidate and no product step.
    let Some(projection) = ProjectionOperation::new(operation) else {
        return Some(true);
    };
    for range in projection.operands().into_iter().flatten() {
        let mut register = range.start;
        let end = range.end()?;
        while register < end {
            let owner = program.producer_position(register)?;
            let source = program.operation(owner)?;
            pending.insert(owner);
            register = source
                .dst_register()?
                .checked_add(u32::try_from(source.dst_register_count()).ok()?)?
                .min(end);
        }
    }
    Some(projection.is_linear())
}
