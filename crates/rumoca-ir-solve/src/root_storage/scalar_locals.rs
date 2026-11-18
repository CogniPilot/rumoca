//! Positive f64 local home construction from canonical root register facts.
use super::Component;
use crate::{BinaryOp, LinearOp, Reg, RegisterVersionProgram};
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
        extent: usize,
    ) -> Result<Self, String> {
        let mut eligible: Vec<_> = components
            .iter()
            .map(|component| (1..=extent).contains(&component.virtual_range.len()))
            .collect();
        for (index, operation) in versions.operations().iter().enumerate() {
            if let Some((start, storage)) = operation.destination() {
                let range = start as usize..start as usize + storage.len();
                mark(
                    components,
                    &mut eligible,
                    range,
                    definition(&program[index], extent),
                )?;
            }
            for piece in operation.operands() {
                mark(
                    components,
                    &mut eligible,
                    piece.registers(),
                    reader(&program[index], extent),
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
    access: Access,
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
    eligible[index] &= match access {
        Access::Scalar => range.len() == 1,
        Access::StaticTensor => true,
        Access::Addressed => false,
    };
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
    let mut active: Vec<(usize, Range<u32>)> = Vec::new();
    let mut slots = BTreeMap::new();
    let mut count = 0u32;
    for component in order {
        // Source reads and the destination remain live together at one operation.
        active.retain(|(last, _)| *last >= component.first);
        let width = u32::try_from(component.virtual_range.len())
            .map_err(|_| "diagnostic scalar local count overflow")?;
        let run = lowest_free_run(&active, width)?;
        count = count.max(run.end);
        if count > maximum {
            return Err("root scalar local capacity exceeded".into());
        }
        for (register, slot) in component.virtual_range.clone().zip(run.clone()) {
            let register =
                Reg::try_from(register).map_err(|_| "diagnostic scalar register overflows")?;
            slots.insert(register, slot);
        }
        active.push((component.last, run));
    }
    Ok(ScalarLocals { slots, count })
}

/// The lowest contiguous run of `width` slots that no live component holds.
fn lowest_free_run(active: &[(usize, Range<u32>)], width: u32) -> Result<Range<u32>, String> {
    let mut held: Vec<_> = active.iter().map(|(_, run)| run.clone()).collect();
    held.sort_by_key(|run| run.start);
    let mut start = 0u32;
    for run in held {
        let end = start
            .checked_add(width)
            .ok_or("diagnostic scalar local count overflow")?;
        if end <= run.start {
            break;
        }
        start = start.max(run.end);
    }
    let end = start
        .checked_add(width)
        .ok_or("diagnostic scalar local count overflow")?;
    Ok(start..end)
}

/// How one operation reaches the registers of a component.
#[derive(Clone, Copy)]
enum Access {
    /// One cell, read or written as a value.
    Scalar,
    /// A run whose every cell the operation names statically.
    StaticTensor,
    /// Runtime-indexed, bulk, call, conditional or unknown use.
    Addressed,
}

fn definition(operation: &LinearOp, extent: usize) -> Access {
    if scalar_definition(operation) {
        Access::Scalar
    } else if !matches!(operation, LinearOp::StoreOutputRange { .. })
        && within(static_tensor_extent(operation), extent)
    {
        Access::StaticTensor
    } else {
        Access::Addressed
    }
}

fn reader(operation: &LinearOp, extent: usize) -> Access {
    if scalar_reader(operation) {
        Access::Scalar
    } else if !matches!(operation, LinearOp::TensorLoad { .. })
        && within(static_tensor_extent(operation), extent)
    {
        Access::StaticTensor
    } else {
        Access::Addressed
    }
}

/// A static tensor operation is admitted only when the issued extent admits
/// more than one cell and bounds every run the operation names, so nothing
/// that touches a local home can denote more work than the target requested.
fn within(operation_extent: Option<usize>, extent: usize) -> bool {
    extent > 1 && operation_extent.is_some_and(|cells| cells <= extent)
}

/// The largest run of cells a single-lane static tensor operation names,
/// taken from the operation's own extents; `None` for every other operation.
fn static_tensor_extent(operation: &LinearOp) -> Option<usize> {
    match operation {
        LinearOp::TensorBinary {
            op: BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div,
            count,
            lanes: 1,
            ..
        }
        | LinearOp::TensorLoad {
            count,
            lanes: 1,
            seed_start: None,
            ..
        }
        | LinearOp::StoreOutputRange { count, .. } => Some(*count),
        LinearOp::MatrixMultiply {
            rows,
            inner,
            columns,
            lanes: 1,
            ..
        } => Some(
            rows.checked_mul(*columns)?
                .max(rows.checked_mul(*inner)?)
                .max(inner.checked_mul(*columns)?),
        ),
        LinearOp::TensorCross { lanes: 1, .. } => Some(3),
        LinearOp::TensorTranspose {
            rows,
            columns,
            element_width,
            lanes: 1,
            ..
        } => rows.checked_mul(*columns)?.checked_mul(*element_width),
        LinearOp::TensorConcatenate {
            dimensions,
            lanes: 1,
            ..
        } => dimensions
            .iter()
            .try_fold(1usize, |cells, extent| cells.checked_mul(*extent as usize)),
        _ => None,
    }
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
