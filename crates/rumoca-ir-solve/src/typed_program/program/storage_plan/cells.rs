//! Frame-cell reuse of one program body.
//!
//! Every register whose home is the body's own cells belongs to the group of
//! its root, which shares one range of the root's scalar count. A group lives
//! from its first definition to its last read; groups are placed in order of
//! first definition, each into the lowest free range it fits, and a range is
//! freed once its group's last read precedes the next group's definition,
//! merging with adjacent free ranges. This is the in-process compiled
//! target's own compaction (`emit/typed_program/storage.rs`), ported
//! unchanged; the plan now issues it. Address-free scalar homes do not
//! participate in these groups and receive no frame offset.

use std::collections::BTreeMap;

use serde::Serialize;

use super::super::{SolveSpannedOperation, SolveValueType};
use super::SolveRegisterHome;

/// The reused frame range of every register that lives in the body's own
/// cells, as a scalar offset from the start of the register frame.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveFrameCells {
    offsets: Box<[Option<u32>]>,
    extent: u32,
}

impl SolveFrameCells {
    /// `last_reads` are the fusion-extended last reads and `homes` the
    /// resolved alias homes. A layout whose extent would not fit a `u32` is
    /// not compacted: no register is given an offset.
    pub(in crate::typed_program) fn of(
        register_types: &[SolveValueType],
        operations: &[SolveSpannedOperation],
        last_reads: &[Option<usize>],
        homes: &[SolveRegisterHome],
    ) -> Self {
        compact(register_types, operations, last_reads, homes).unwrap_or_else(|| Self {
            offsets: vec![None; register_types.len()].into_boxed_slice(),
            extent: 0,
        })
    }

    /// The frame offset of each register, `None` for one that lives in a
    /// slot or scalar home (or for every register of an overflowing layout).
    #[must_use]
    pub fn offsets(&self) -> &[Option<u32>] {
        &self.offsets
    }

    /// The scalar cells the register frame needs.
    #[must_use]
    pub const fn extent(&self) -> u32 {
        self.extent
    }
}

/// One root's registers and the span they are live.
struct Group {
    root: usize,
    width: u32,
    first_definition: usize,
    last_read: usize,
    registers: Vec<usize>,
}

#[derive(Clone, Copy)]
struct Range {
    cell: u32,
    width: u32,
}

fn compact(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    last_reads: &[Option<usize>],
    homes: &[SolveRegisterHome],
) -> Option<SolveFrameCells> {
    let definitions = definitions(register_types.len(), operations);
    let mut groups = BTreeMap::<usize, Group>::new();
    for (register, home) in homes.iter().enumerate() {
        let root = match home {
            SolveRegisterHome::Own => register,
            SolveRegisterHome::Register { register } => *register,
            SolveRegisterHome::InputSlot { .. }
            | SolveRegisterHome::OutputSlot { .. }
            | SolveRegisterHome::Scalar { .. } => continue,
        };
        // A checked body defines every register before reading it.
        let definition = definitions.get(register).copied().flatten().unwrap_or(0);
        let last_read = last_reads
            .get(register)
            .copied()
            .flatten()
            .unwrap_or(definition);
        let width = register_types.get(root)?.scalar_count();
        let group = groups.entry(root).or_insert_with(|| Group {
            root,
            width,
            first_definition: definition,
            last_read,
            registers: Vec::new(),
        });
        group.first_definition = group.first_definition.min(definition);
        group.last_read = group.last_read.max(last_read);
        group.registers.push(register);
    }
    let mut groups = groups.into_values().collect::<Vec<_>>();
    // Roots are laid out in register order before compaction, so the root
    // breaks ties as the target's original cell did.
    groups.sort_by_key(|group| (group.first_definition, group.root));
    let mut offsets = vec![None; register_types.len()];
    let mut active = Vec::<(usize, Range)>::new();
    let mut free = Vec::<Range>::new();
    let mut extent = 0u32;
    for group in groups {
        let (released, live): (Vec<_>, Vec<_>) = active
            .into_iter()
            .partition(|(last_read, _)| *last_read < group.first_definition);
        active = live;
        for (_, range) in released {
            release(&mut free, range)?;
        }
        let cell = allocate(&mut free, &mut extent, group.width)?;
        for register in group.registers {
            offsets[register] = Some(cell);
        }
        active.push((
            group.last_read,
            Range {
                cell,
                width: group.width,
            },
        ));
    }
    Some(SolveFrameCells {
        offsets: offsets.into_boxed_slice(),
        extent,
    })
}

/// The operation defining each register.
fn definitions(registers: usize, operations: &[SolveSpannedOperation]) -> Vec<Option<usize>> {
    let mut definitions = vec![None; registers];
    for (index, issued) in operations.iter().enumerate() {
        issued.operation.visit_output_registers(|register| {
            if let Some(definition) = definitions.get_mut(register.index()) {
                *definition = Some(index);
            }
        });
    }
    definitions
}

/// The lowest free range `width` fits, split from the front, or a new range
/// at the end of the frame.
fn allocate(free: &mut Vec<Range>, extent: &mut u32, width: u32) -> Option<u32> {
    free.sort_by_key(|range| range.cell);
    if let Some(index) = free.iter().position(|range| range.width >= width) {
        let cell = free[index].cell;
        if free[index].width == width {
            free.remove(index);
        } else {
            free[index].cell = cell.checked_add(width)?;
            free[index].width -= width;
        }
        return Some(cell);
    }
    let cell = *extent;
    *extent = extent.checked_add(width)?;
    Some(cell)
}

/// Return `range` to the free list, merging adjacent ranges.
fn release(free: &mut Vec<Range>, range: Range) -> Option<()> {
    free.push(range);
    free.sort_by_key(|range| range.cell);
    let mut merged = Vec::<Range>::with_capacity(free.len());
    for range in free.drain(..) {
        if let Some(previous) = merged.last_mut()
            && previous.cell.checked_add(previous.width)? == range.cell
        {
            previous.width = previous.width.checked_add(range.width)?;
        } else {
            merged.push(range);
        }
    }
    *free = merged;
    Some(())
}
