//! Positive scalar home construction from canonical root register facts.
use super::Component;
use crate::{LinearOp, Reg, RegisterVersionProgram};
use std::{collections::BTreeMap, ops::Range};

#[derive(Debug, Default)]
pub(super) struct ScalarLocals {
    slots: BTreeMap<Reg, u32>,
    count: u32,
}

impl ScalarLocals {
    pub(super) fn derive(
        program: &[LinearOp],
        versions: &RegisterVersionProgram,
        components: &[Component],
        maximum: u32,
    ) -> Result<Self, String> {
        let mut eligible: Vec<_> = components
            .iter()
            .map(|component| component.virtual_range.len() == 1)
            .collect();
        for (index, operation) in versions.operations().iter().enumerate() {
            if let Some((start, storage)) = operation.destination() {
                let range = start as usize..start as usize + storage.len();
                mark(
                    components,
                    &mut eligible,
                    range,
                    scalar_definition(&program[index]),
                )?;
            }
            for piece in operation.operands() {
                mark(
                    components,
                    &mut eligible,
                    piece.registers(),
                    scalar_reader(&program[index]),
                )?;
            }
        }
        allocate(components, &eligible, maximum)
    }

    pub(super) fn count(&self) -> u32 {
        self.count
    }

    pub(super) fn slot(&self, register: Reg) -> Option<u32> {
        self.slots.get(&register).copied()
    }
}

fn mark(
    components: &[Component],
    eligible: &mut [bool],
    range: Range<usize>,
    scalar: bool,
) -> Result<(), String> {
    if range.is_empty() {
        return Ok(());
    }
    let index = components
        .partition_point(|component| component.virtual_range.start <= range.start)
        .checked_sub(1)
        .ok_or("diagnostic scalar source has no original component")?;
    let component = &components[index];
    if range.end > component.virtual_range.end {
        return Err("diagnostic scalar source crosses original components".into());
    }
    eligible[index] &= scalar && range.len() == 1;
    Ok(())
}

fn allocate(
    components: &[Component],
    eligible: &[bool],
    maximum: u32,
) -> Result<ScalarLocals, String> {
    let mut order: Vec<_> = components
        .iter()
        .enumerate()
        .filter(|(index, _)| eligible[*index])
        .map(|(_, component)| component)
        .collect();
    order.sort_by_key(|component| {
        (
            component.first,
            component.last,
            component.virtual_range.start,
        )
    });
    let mut active: Vec<(usize, u32)> = Vec::new();
    let mut slots = BTreeMap::new();
    let mut count = 0u32;
    for component in order {
        // Source reads and the destination remain live together at one operation.
        active.retain(|&(last, _)| last >= component.first);
        let slot = (0..count)
            .find(|slot| active.iter().all(|&(_, used)| used != *slot))
            .unwrap_or(count);
        if slot == count {
            count = count
                .checked_add(1)
                .ok_or("diagnostic scalar local count overflow")?;
        }
        if count > maximum {
            return Err("root scalar local capacity exceeded".into());
        }
        let register = Reg::try_from(component.virtual_range.start)
            .map_err(|_| "diagnostic scalar register overflows")?;
        slots.insert(register, slot);
        active.push((component.last, slot));
    }
    Ok(ScalarLocals { slots, count })
}

fn scalar_definition(operation: &LinearOp) -> bool {
    matches!(
        operation,
        LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
    )
}

fn scalar_reader(operation: &LinearOp) -> bool {
    matches!(
        operation,
        LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::StoreOutput { .. }
    )
}

#[cfg(test)]
mod tests;
