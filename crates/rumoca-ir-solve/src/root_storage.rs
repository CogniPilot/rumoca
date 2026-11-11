//! Checked target placement of indivisible original register ranges.
mod scalar_locals;
use std::{collections::BTreeMap, ops::Range};

use crate::{LinearOp, Reg, RegisterVersionProgram};

/// Resource description for checked root-register storage construction.
#[derive(Clone, Copy, Debug)]
pub struct RootRegisterTarget {
    max_scalar_locals: u32,
}

impl RootRegisterTarget {
    /// Keep every root component in the issued memory layout.
    pub const fn memory_only() -> Self {
        Self {
            max_scalar_locals: 0,
        }
    }

    /// Request scalar f64 homes within the target's available local capacity.
    pub const fn scalar_locals(maximum: u32) -> Self {
        Self {
            max_scalar_locals: maximum,
        }
    }
}

#[derive(Clone, Debug)]
struct Component {
    virtual_range: Range<usize>,
    first: usize,
    last: usize,
    physical: usize,
}

/// Immutable target storage homes issued from one complete register program.
#[derive(Debug)]
pub struct RootRegisterLayout {
    components: Vec<Component>,
    extent: usize,
    scalar_locals: scalar_locals::ScalarLocals,
}

impl RootRegisterLayout {
    /// Derive memory placement without scalar local homes.
    pub fn derive(program: &[LinearOp]) -> Result<Self, String> {
        Self::for_target(program, RootRegisterTarget::memory_only())
    }

    /// Issue the complete immutable layout for one checked program and target.
    pub fn for_target(program: &[LinearOp], target: RootRegisterTarget) -> Result<Self, String> {
        let versions = RegisterVersionProgram::derive(program)
            .ok_or("native register layout requires canonical register versions")?;
        let mut constraints = Vec::new();
        for (index, operation) in versions.operations().iter().enumerate() {
            if let Some((start, storage)) = operation.destination() {
                let start = start as usize;
                constraints.push(component(start..start + storage.len(), index, index));
            }
            let mut selections: BTreeMap<_, (Range<usize>, usize)> = BTreeMap::new();
            for piece in operation.operands() {
                let range = piece.registers();
                selections
                    .entry(piece.selection())
                    .and_modify(|entry| extend_selection(entry, &range, piece.producer()))
                    .or_insert((range, piece.producer()));
            }
            constraints.extend(
                selections
                    .into_values()
                    .map(|(range, first)| component(range, first, index)),
            );
        }
        constraints.retain(|component| !component.virtual_range.is_empty());
        constraints
            .sort_by_key(|component| (component.virtual_range.start, component.virtual_range.end));
        let mut components: Vec<Component> = Vec::new();
        for constraint in constraints {
            if let Some(previous) = components.last_mut()
                && constraint.virtual_range.start < previous.virtual_range.end
            {
                previous.virtual_range.end =
                    previous.virtual_range.end.max(constraint.virtual_range.end);
                previous.first = previous.first.min(constraint.first);
                previous.last = previous.last.max(constraint.last);
            } else {
                components.push(constraint);
            }
        }
        let scalar_locals = if target.max_scalar_locals == 0 {
            scalar_locals::ScalarLocals::default()
        } else {
            scalar_locals::ScalarLocals::derive(
                program,
                &versions,
                &components,
                target.max_scalar_locals,
            )?
        };
        let extent = allocate(&mut components)?;
        Ok(Self {
            components,
            extent,
            scalar_locals,
        })
    }

    /// Number of target f64 local slots issued for this complete root.
    pub fn scalar_local_count(&self) -> u32 {
        self.scalar_locals.count()
    }

    /// Target local slot for an original register, or its memory disposition.
    pub fn scalar_local_slot(&self, register: Reg) -> Option<u32> {
        self.scalar_locals.slot(register)
    }

    /// Physical memory extent in f64 cells.
    pub fn extent(&self) -> usize {
        self.extent
    }

    /// Original virtual register extent before physical placement.
    pub fn virtual_extent(&self) -> usize {
        self.components
            .last()
            .map_or(0, |component| component.virtual_range.end)
    }

    /// Project an original register to its issued physical memory cell.
    pub fn physical(&self, register: Reg) -> Result<Reg, String> {
        let register = register as usize;
        let index = self
            .components
            .partition_point(|component| component.virtual_range.start <= register);
        let component = index
            .checked_sub(1)
            .and_then(|index| self.components.get(index))
            .filter(|component| component.virtual_range.contains(&register))
            .ok_or("native register has no original placement")?;
        component
            .physical
            .checked_add(register - component.virtual_range.start)
            .and_then(|physical| Reg::try_from(physical).ok())
            .ok_or_else(|| "native physical register overflows".into())
    }
}

fn extend_selection(entry: &mut (Range<usize>, usize), range: &Range<usize>, producer: usize) {
    entry.0.start = entry.0.start.min(range.start);
    entry.0.end = entry.0.end.max(range.end);
    entry.1 = entry.1.min(producer);
}

fn component(virtual_range: Range<usize>, first: usize, last: usize) -> Component {
    Component {
        virtual_range,
        first,
        last,
        physical: 0,
    }
}

fn allocate(components: &mut [Component]) -> Result<usize, String> {
    let mut order: Vec<_> = (0..components.len()).collect();
    order.sort_by_key(|&index| {
        let component = &components[index];
        (
            std::cmp::Reverse(component.virtual_range.len()),
            std::cmp::Reverse(component.last - component.first),
            component.virtual_range.start,
        )
    });
    let mut placed: Vec<usize> = Vec::new();
    let mut extent = 0usize;
    for index in order {
        let current = &components[index];
        let width = current.virtual_range.len();
        let mut occupied = Vec::new();
        for &other in &placed {
            let other = &components[other];
            if other.first <= current.last && current.first <= other.last {
                let end = other
                    .physical
                    .checked_add(other.virtual_range.len())
                    .ok_or("native register placement overflows")?;
                occupied.push(other.physical..end);
            }
        }
        occupied.sort_by_key(|range| (range.start, range.end));
        let mut physical = 0usize;
        for range in occupied {
            let end = physical
                .checked_add(width)
                .ok_or("native register placement overflows")?;
            if end <= range.start {
                break;
            }
            physical = physical.max(range.end);
        }
        let end = physical
            .checked_add(width)
            .ok_or("native register placement overflows")?;
        components[index].physical = physical;
        extent = extent.max(end);
        placed.push(index);
    }
    Ok(extent)
}
