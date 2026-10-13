//! Exact old-value reads bound reuse of target-owned synchronous region inputs.
use super::{CellRange, PointerCell, visit_reads};
use rumoca_ir_solve as solve;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Default)]
pub(super) struct Lifetimes {
    last_use: Vec<usize>,
    defined_at: Vec<usize>,
    input_reads: BTreeMap<(u32, u32, u32), usize>,
    owned: BTreeMap<(u32, u32, u32), usize>,
    pinned: BTreeSet<(u32, u32, u32)>,
    ends: Vec<Vec<CellRange>>,
    free: BTreeMap<u32, Vec<CellRange>>,
    /// Registers that name a parent value by address: the register, its
    /// pointer range and the scratch values it may name.
    references: Vec<(solve::SolveRegisterId, CellRange, Vec<CellRange>)>,
}

impl Lifetimes {
    pub(super) fn construct(
        program: &solve::TypedProgram,
        slots: &[CellRange],
        liveness: &super::Liveness,
    ) -> Self {
        let mut result = Self {
            last_use: last_uses(program, liveness),
            defined_at: vec![usize::MAX; program.register_types().len()],
            ends: vec![Vec::new(); program.operations().len()],
            ..Self::default()
        };
        for (index, operation) in program.operations().iter().enumerate() {
            operation
                .operation()
                .visit_output_registers(|register| result.defined_at[register.index()] = index);
        }
        for (index, operation) in program.operations().iter().enumerate() {
            if liveness.is_dead(index) {
                continue;
            }
            if let Some((slot, destination)) = input_load(program, operation.operation()) {
                // Every load of the immutable old input version matters,
                // including a later load or a duplicate captured input slot.
                let last = result.last_use[destination.index()].max(index);
                result
                    .input_reads
                    .entry(key(slots[slot.index()]))
                    .and_modify(|end| *end = (*end).max(last))
                    .or_insert(last);
            }
        }
        result
    }

    /// Only the allocator's newly issued private ranges enter this inventory.
    pub(super) fn own(&mut self, range: CellRange) {
        self.owned.insert(key(range), 0);
    }

    pub(super) fn take_free(&mut self, bytes: u32) -> Option<CellRange> {
        self.free.get_mut(&bytes)?.pop()
    }

    pub(super) fn issued(&mut self, register: solve::SolveRegisterId, range: CellRange) {
        let Some(end) = self.owned.get_mut(&key(range)) else {
            return;
        };
        *end = (*end).max(self.last_use[register.index()].max(self.defined_at[register.index()]));
        self.ends[*end].push(range);
    }

    /// Record that `register`, stored at pointer range `range`, may name the
    /// value in any of `sources` for as long as it is read.
    ///
    /// Each source is a parent-owned value the register does not copy, so it
    /// must outlive the register: its storage is not reissued, and no
    /// operation may rewrite it in place, while the register is live.
    pub(super) fn reference(
        &mut self,
        register: solve::SolveRegisterId,
        range: CellRange,
        sources: &[CellRange],
    ) {
        let targets = sources
            .iter()
            .flat_map(|source| self.named_by(*source))
            .collect::<Vec<_>>();
        let end = self.last_use[register.index()].max(self.defined_at[register.index()]);
        for target in &targets {
            self.extend(*target, end);
        }
        self.references.push((register, range, targets));
    }

    /// The scratch values a range may name: itself, or what the reference
    /// register at that range names.
    fn named_by(&self, source: CellRange) -> Vec<CellRange> {
        match source.pointer_cell() {
            Some(PointerCell::Scratch(_)) => self
                .references
                .iter()
                .filter(|(_, other, _)| *other == source)
                .flat_map(|(_, _, inner)| inner.iter().copied())
                .collect(),
            Some(PointerCell::Input(_)) => Vec::new(),
            None if source.base == 2 => vec![source],
            None => Vec::new(),
        }
    }

    /// Keep an owned allocation until operation `end` has completed.
    fn extend(&mut self, range: CellRange, end: usize) {
        let Some(until) = self.owned.get_mut(&key(range)) else {
            return;
        };
        if end > *until {
            *until = end;
            self.ends[end].push(range);
        }
    }

    /// A region return persists until the parent has copied its completed tuple.
    pub(super) fn pin(&mut self, range: CellRange) {
        if self.owned.contains_key(&key(range)) {
            self.pinned.insert(key(range));
        }
    }

    /// Runs only after the whole operation, including synchronous regions/calls.
    pub(super) fn complete(&mut self, index: usize) {
        for range in std::mem::take(&mut self.ends[index]) {
            let allocation = key(range);
            if self.owned.get(&allocation) == Some(&index) && !self.pinned.contains(&allocation) {
                self.owned.remove(&allocation);
                self.free.entry(range.bytes).or_default().push(range);
            }
        }
    }

    pub(super) fn can_consume(
        &self,
        index: usize,
        source: solve::SolveRegisterId,
        registers: &[CellRange],
        reusable: &[bool],
    ) -> bool {
        let range = registers[source.index()];
        if !reusable[source.index()] || range.base != 2 {
            return false;
        }
        if self
            .input_reads
            .get(&key(range))
            .is_some_and(|end| *end > index)
        {
            return false;
        }
        // A reference result names this range for as long as it is read.
        if self.references.iter().any(|(register, _, targets)| {
            targets.contains(&range)
                && self.defined_at[register.index()] < index
                && self.last_use[register.index()] > index
        }) {
            return false;
        }
        // The live-alias guard is the proof: no other register sharing the range
        // is read after `index`, and `source` itself is not read later.
        // Allocation/borrow construction makes whole register ranges equal or
        // disjoint. Equality here identifies that checked private allocation,
        // never a semantic owner or equality of different source values.
        !registers.iter().enumerate().any(|(register, other)| {
            *other == range && self.defined_at[register] < index && self.last_use[register] > index
        })
    }
}

/// The last operation that runs and reads each register.
fn last_uses(program: &solve::TypedProgram, liveness: &super::Liveness) -> Vec<usize> {
    if liveness.dead().is_empty() {
        return program
            .register_last_reads()
            .iter()
            .map(|last| last.unwrap_or(0))
            .collect();
    }
    let mut last = vec![0; program.register_types().len()];
    for (index, operation) in program.operations().iter().enumerate() {
        if !liveness.is_dead(index) {
            visit_reads(
                operation.operation(),
                liveness.unused_captures(index),
                |register| {
                    last[register.index()] = index;
                },
            );
        }
    }
    last
}

fn input_load(
    program: &solve::TypedProgram,
    operation: &solve::SolveOperation,
) -> Option<(solve::SolveSlotId, solve::SolveRegisterId)> {
    match operation {
        solve::SolveOperation::Load { slot, destination }
            if program.slots()[slot.index()].storage() == solve::SolveStorageClass::Input =>
        {
            Some((*slot, *destination))
        }
        _ => None,
    }
}

fn key(range: CellRange) -> (u32, u32, u32) {
    (range.base, range.offset, range.bytes)
}
