//! Last-read relation of one checked typed program.
//!
//! Registers are single-assignment and a region body owns its own register
//! namespace, so the operation that reads a register or a read-only slot last
//! is fixed by construction. Every executor derives its value lifetimes from
//! this one relation: the interpreter releases a shared payload at its last
//! read, which lets a functional update of an aggregate reuse the payload of
//! an aggregate nothing reads afterwards, and the compiled backends end a
//! register's storage there.

use super::{SolveOperation, SolveSlot, SolveSlotAccess, SolveSpannedOperation};

/// The operation index of the last read of every register and of the last
/// load of every read-only slot.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct ReadFlow {
    register_last_read: Box<[Option<usize>]>,
    /// Whether the last reader lists the register more than once.
    register_listed_twice: Box<[bool]>,
    slot_last_load: Box<[Option<usize>]>,
}

/// The last operation to read each register and how many times it lists the
/// register.
struct RegisterReads {
    last_read: Vec<Option<usize>>,
    occurrences: Vec<usize>,
}

impl RegisterReads {
    fn record(&mut self, register: usize, operation: usize) {
        let (Some(last), Some(count)) = (
            self.last_read.get_mut(register),
            self.occurrences.get_mut(register),
        ) else {
            return;
        };
        if *last == Some(operation) {
            *count += 1;
        } else {
            *last = Some(operation);
            *count = 1;
        }
    }
}

impl ReadFlow {
    pub(super) fn of(
        operations: &[SolveSpannedOperation],
        register_count: usize,
        slots: &[SolveSlot],
    ) -> Self {
        let mut reads = RegisterReads {
            last_read: vec![None; register_count],
            occurrences: vec![0; register_count],
        };
        let mut slot_last_load = vec![None; slots.len()];
        for (index, operation) in operations.iter().enumerate() {
            operation
                .operation()
                .visit_input_registers(|register| reads.record(register.index(), index));
            if let SolveOperation::Load { slot, .. } = operation.operation()
                && let Some(last) = slot_last_load.get_mut(slot.index())
            {
                *last = Some(index);
            }
        }
        // Only a slot nothing stores to can release its value at its last
        // load: a read-write slot is read again as the program's result.
        for (last, slot) in slot_last_load.iter_mut().zip(slots) {
            if slot.access() != SolveSlotAccess::ReadOnly {
                *last = None;
            }
        }
        Self {
            register_last_read: reads.last_read.into_boxed_slice(),
            register_listed_twice: reads
                .occurrences
                .into_iter()
                .map(|count| count > 1)
                .collect(),
            slot_last_load: slot_last_load.into_boxed_slice(),
        }
    }

    pub(super) fn register_last_reads(&self) -> &[Option<usize>] {
        &self.register_last_read
    }

    /// Whether `operation` reads `register` last and lists it once, so that it
    /// can move the value out: a second listing would find it gone.
    pub(super) fn register_moves_at(&self, register: usize, operation: usize) -> bool {
        self.register_last_read.get(register).copied().flatten() == Some(operation)
            && !self
                .register_listed_twice
                .get(register)
                .copied()
                .unwrap_or(true)
    }

    pub(super) fn slot_last_load_at(&self, slot: usize, operation: usize) -> bool {
        self.slot_last_load.get(slot).copied().flatten() == Some(operation)
    }
}
