//! Last-read relation of one checked typed program.
//!
//! Registers are single-assignment and a region body owns its own register
//! namespace, so the operation that reads a register or a read-only slot last
//! is fixed by construction. An executor that holds a value as shared payload
//! uses this relation to release it at its last read instead of keeping it
//! alive, which lets a functional update of an aggregate reuse the payload of
//! an aggregate nothing reads afterwards.

use super::{SolveOperation, SolveSlot, SolveSlotAccess, SolveSpannedOperation};

/// The operation index of the last single read of every register and of the last
/// load of every slot.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct ReadFlow {
    register_last_read: Box<[Option<usize>]>,
    slot_last_load: Box<[Option<usize>]>,
}

impl ReadFlow {
    pub(super) fn of(
        operations: &[SolveSpannedOperation],
        register_count: usize,
        slots: &[SolveSlot],
    ) -> Self {
        let mut register_last_read = vec![None; register_count];
        let mut occurrences = vec![0_usize; register_count];
        let mut slot_last_load = vec![None; slots.len()];
        for (index, operation) in operations.iter().enumerate() {
            operation.operation().visit_input_registers(|register| {
                let (Some(last), Some(count)) = (
                    register_last_read.get_mut(register.index()),
                    occurrences.get_mut(register.index()),
                ) else {
                    return;
                };
                if *last == Some(index) {
                    *count += 1;
                } else {
                    *last = Some(index);
                    *count = 1;
                }
            });
            if let SolveOperation::Load { slot, .. } = operation.operation()
                && let Some(last) = slot_last_load.get_mut(slot.index())
            {
                *last = Some(index);
            }
        }
        // A register an operation lists twice cannot move out of that operation:
        // its second read would find it gone.
        for (last, count) in register_last_read.iter_mut().zip(&occurrences) {
            if *count > 1 {
                *last = None;
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
            register_last_read: register_last_read.into_boxed_slice(),
            slot_last_load: slot_last_load.into_boxed_slice(),
        }
    }

    pub(super) fn register_last_read_at(&self, register: usize, operation: usize) -> bool {
        self.register_last_read.get(register).copied().flatten() == Some(operation)
    }

    pub(super) fn slot_last_load_at(&self, slot: usize, operation: usize) -> bool {
        self.slot_last_load.get(slot).copied().flatten() == Some(operation)
    }
}
