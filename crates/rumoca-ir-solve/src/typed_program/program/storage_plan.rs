//! Host storage plan of one checked typed program.
//!
//! Where each slot and register of a program body lives is decided once,
//! from the issued body, when the program is assembled: an emitter prints
//! the placement it is handed and decides nothing. A program replayed from
//! its wire form assembles its plan again, so no serialized plan is trusted.
//!
//! The host plan keeps every owned value in the function's own frame. An
//! input slot the body never stores is a read-only view of the caller's
//! array, and a register only a load of such a slot defines is a read-only
//! view of that slot. Everything else is an owned frame array of the
//! value's scalar count, which a checked value type keeps at least one.

use serde::Serialize;

use super::{SolveOperation, SolveSlot, SolveSpannedOperation, SolveStorageClass, SolveValueType};

/// Where one slot of a program body lives.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "placement", rename_all = "snake_case")]
pub enum SolveSlotPlacement {
    /// The caller's array for input `argument`, read in place.
    Argument { argument: usize },
    /// An owned frame array of `extent` elements, copied in from input
    /// `argument` when the slot is an input.
    Frame {
        extent: u32,
        argument: Option<usize>,
    },
}

/// Where one register of a program body lives.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "placement", rename_all = "snake_case")]
pub enum SolveRegisterPlacement {
    /// A read-only view of `slot`, itself the caller's array.
    View { slot: usize },
    /// An owned frame array of `extent` elements.
    Frame { extent: u32 },
}

/// The issued placement of every slot and register of one program body.
/// Only assembly issues one; a caller reads it from
/// [`TypedProgram::storage_plan`](super::TypedProgram::storage_plan).
///
/// ```
/// fn issued(program: &rumoca_ir_solve::TypedProgram) -> &rumoca_ir_solve::SolveStoragePlan {
///     program.storage_plan()
/// }
/// ```
///
/// ```compile_fail
/// let _: rumoca_ir_solve::SolveStoragePlan = Default::default();
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveStoragePlan {
    slots: Box<[SolveSlotPlacement]>,
    registers: Box<[SolveRegisterPlacement]>,
}

impl SolveStoragePlan {
    pub(super) fn host(
        slots: &[SolveSlot],
        register_types: &[SolveValueType],
        operations: &[SolveSpannedOperation],
    ) -> Self {
        let (stored, writes) = stores_and_writes(slots.len(), register_types.len(), operations);
        let mut inputs = 0;
        let slot_placements: Box<[_]> = slots
            .iter()
            .zip(&stored)
            .map(|(slot, &stored)| {
                let extent = slot.value_type().scalar_count();
                if slot.storage() != SolveStorageClass::Input {
                    return SolveSlotPlacement::Frame {
                        extent,
                        argument: None,
                    };
                }
                let argument = inputs;
                inputs += 1;
                if stored {
                    SolveSlotPlacement::Frame {
                        extent,
                        argument: Some(argument),
                    }
                } else {
                    SolveSlotPlacement::Argument { argument }
                }
            })
            .collect();
        let mut registers: Box<[_]> = register_types
            .iter()
            .map(|value_type| SolveRegisterPlacement::Frame {
                extent: value_type.scalar_count(),
            })
            .collect();
        for issued in operations {
            if let SolveOperation::Load { destination, slot } = &issued.operation
                && matches!(
                    slot_placements.get(slot.index()),
                    Some(SolveSlotPlacement::Argument { .. })
                )
                && writes.get(destination.index()) == Some(&1)
                && let Some(placement) = registers.get_mut(destination.index())
            {
                *placement = SolveRegisterPlacement::View { slot: slot.index() };
            }
        }
        Self {
            slots: slot_placements,
            registers,
        }
    }

    #[must_use]
    pub fn slots(&self) -> &[SolveSlotPlacement] {
        &self.slots
    }

    #[must_use]
    pub fn registers(&self) -> &[SolveRegisterPlacement] {
        &self.registers
    }
}

impl super::TypedProgram {
    /// Where each slot and register of this body lives, issued once at
    /// assembly; emitters print it.
    #[must_use]
    pub fn storage_plan(&self) -> &SolveStoragePlan {
        &self.storage
    }
}

/// Whether the body stores each slot, and how many operations define each
/// register.
fn stores_and_writes(
    slots: usize,
    registers: usize,
    operations: &[SolveSpannedOperation],
) -> (Vec<bool>, Vec<usize>) {
    let mut stored = vec![false; slots];
    let mut writes = vec![0_usize; registers];
    let mut count = |register: super::SolveRegisterId| {
        if let Some(count) = writes.get_mut(register.index()) {
            *count += 1;
        }
    };
    for issued in operations {
        if let SolveOperation::Store { slot, .. } = &issued.operation
            && let Some(flag) = stored.get_mut(slot.index())
        {
            *flag = true;
        }
        issued.operation.visit_output_registers(&mut count);
    }
    (stored, writes)
}
