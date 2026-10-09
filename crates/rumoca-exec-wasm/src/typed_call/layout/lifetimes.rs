//! Exact old-value reads bound reuse of target-owned synchronous region inputs.
use super::CellRange;
use rumoca_ir_solve as solve;
use std::collections::BTreeMap;

#[derive(Default)]
pub(super) struct Lifetimes {
    last_use: Vec<usize>,
    defined_at: Vec<usize>,
    input_reads: BTreeMap<(u32, u32, u32), usize>,
}

impl Lifetimes {
    pub(super) fn construct(program: &solve::TypedProgram, slots: &[CellRange]) -> Self {
        let mut result = Self {
            last_use: program
                .register_last_reads()
                .iter()
                .map(|last| last.unwrap_or(0))
                .collect(),
            defined_at: vec![usize::MAX; program.register_types().len()],
            input_reads: BTreeMap::new(),
        };
        for (index, operation) in program.operations().iter().enumerate() {
            operation
                .operation()
                .visit_output_registers(|register| result.defined_at[register.index()] = index);
        }
        for (index, operation) in program.operations().iter().enumerate() {
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
