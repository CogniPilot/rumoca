//! Conservative exact-program storage subset; no new alias certificate.
use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
use std::collections::BTreeMap;

pub(crate) fn disjoint_destinations(block: &ScalarProgramBlock) -> Result<(), String> {
    for program in block.programs() {
        let mut written = BTreeMap::<u64, u64>::new();
        for operation in program {
            let Some(start) = operation.dst_register() else {
                continue;
            };
            let count = u64::try_from(operation.dst_register_count())
                .map_err(|_| "WASM exact destination width overflow")?;
            if count == 0 {
                continue;
            }
            let start = u64::from(start);
            let end = start
                .checked_add(count)
                .ok_or("WASM exact destination interval overflow")?;
            let overlaps_predecessor = written
                .range(..=start)
                .next_back()
                .is_some_and(|(_, &previous_end)| previous_end > start);
            let overlaps_successor = written
                .range(start..)
                .next()
                .is_some_and(|(&next_start, _)| next_start < end);
            if overlaps_predecessor || overlaps_successor {
                return Err("WASM exact profile declines overlapping destination versions".into());
            }
            written.insert(start, end);
        }
    }
    Ok(())
}

pub(crate) fn primal_only(operation: &LinearOp) -> Result<(), String> {
    match operation {
        LinearOp::LoadSeed { .. }
        | LinearOp::LoadIndexedSeed { .. }
        | LinearOp::LoadIndexedP { .. }
        | LinearOp::PureCallDirectional { .. } => {
            Err("WASM exact profile declines directional/seed operations".into())
        }
        // Other unsupported operations fail before instantiation in the owning
        // complete-prefix emitter. Never skip a source operation to admit a row.
        _ => Ok(()),
    }
}
