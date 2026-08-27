//! Deterministic final-boundary storage allocation for typed registers.

use std::collections::HashMap;

use super::{CompileError, StorageBase, ValueLocation, checked_cells};

struct RegisterStorageGroup {
    original_cell: u32,
    width: u32,
    first_definition: usize,
    last_use: usize,
    registers: Vec<usize>,
}

#[derive(Clone, Copy)]
struct LiveTapeRange {
    last_use: usize,
    cell: u32,
    width: u32,
}

#[derive(Clone, Copy)]
struct FreeTapeRange {
    cell: u32,
    width: u32,
}

pub(super) fn compact_register_tape(
    register_base: u32,
    last_uses: &[Option<usize>],
    definitions: &[Option<usize>],
    registers: &mut [ValueLocation],
) -> Result<u32, CompileError> {
    let mut group_indices = HashMap::<u32, usize>::new();
    let mut groups = Vec::<RegisterStorageGroup>::new();
    for (index, location) in registers.iter().enumerate() {
        if location.base != StorageBase::Tape {
            continue;
        }
        let definition = definitions.get(index).copied().flatten().ok_or_else(|| {
            CompileError::Backend("checked typed register has no definition".into())
        })?;
        let last_use = last_uses
            .get(index)
            .copied()
            .flatten()
            .unwrap_or(definition);
        let width = location.value_type.scalar_count();
        if let Some(group) = group_indices.get(&location.cell).copied() {
            let group = &mut groups[group];
            if group.width != width {
                return Err(CompileError::Backend(
                    "aliased typed registers have different widths".into(),
                ));
            }
            group.first_definition = group.first_definition.min(definition);
            group.last_use = group.last_use.max(last_use);
            group.registers.push(index);
        } else {
            group_indices.insert(location.cell, groups.len());
            groups.push(RegisterStorageGroup {
                original_cell: location.cell,
                width,
                first_definition: definition,
                last_use,
                registers: vec![index],
            });
        }
    }
    groups.sort_by_key(|group| (group.first_definition, group.original_cell));
    let mut active = Vec::<LiveTapeRange>::new();
    let mut free = Vec::<FreeTapeRange>::new();
    let mut tape_end = register_base;
    for group in groups {
        let mut retained = Vec::with_capacity(active.len());
        for range in active.drain(..) {
            if range.last_use < group.first_definition {
                release_tape_range(&mut free, range.cell, range.width)?;
            } else {
                retained.push(range);
            }
        }
        active = retained;
        let cell = allocate_tape_range(&mut free, &mut tape_end, group.width)?;
        for register in group.registers {
            registers[register].cell = cell;
        }
        active.push(LiveTapeRange {
            last_use: group.last_use,
            cell,
            width: group.width,
        });
    }
    Ok(tape_end)
}

fn allocate_tape_range(
    free: &mut Vec<FreeTapeRange>,
    tape_end: &mut u32,
    width: u32,
) -> Result<u32, CompileError> {
    free.sort_by_key(|range| range.cell);
    if let Some(index) = free.iter().position(|range| range.width >= width) {
        let cell = free[index].cell;
        if free[index].width == width {
            free.remove(index);
        } else {
            free[index].cell = checked_cells(cell, width, "typed tape reuse")?;
            free[index].width -= width;
        }
        return Ok(cell);
    }
    let cell = *tape_end;
    *tape_end = checked_cells(*tape_end, width, "typed compact tape")?;
    Ok(cell)
}

fn release_tape_range(
    free: &mut Vec<FreeTapeRange>,
    cell: u32,
    width: u32,
) -> Result<(), CompileError> {
    free.push(FreeTapeRange { cell, width });
    free.sort_by_key(|range| range.cell);
    let mut merged = Vec::<FreeTapeRange>::with_capacity(free.len());
    for range in free.drain(..) {
        if let Some(previous) = merged.last_mut()
            && checked_cells(previous.cell, previous.width, "typed free tape")? == range.cell
        {
            previous.width = checked_cells(previous.width, range.width, "typed free tape")?;
        } else {
            merged.push(range);
        }
    }
    *free = merged;
    Ok(())
}
