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
//!
//! The plan also carries refinements a target may apply to those frame
//! registers (see [`SolveFusionPlan`] and [`SolveAliasPlan`]). They never
//! change a slot or register placement above, so a target that ignores them
//! stores exactly as before.

mod aliases;
mod cells;
mod fusion;

use serde::Serialize;

#[cfg(test)]
pub(in crate::typed_program) use aliases::observation_cascades;
pub use aliases::{SolveAliasPlan, SolveRegisterHome};
pub use cells::SolveFrameCells;
pub use fusion::{SOLVE_STATIC_MATRIX_WORK, SolveFusedRegister, SolveFusionPlan};

use crate::typed_program::types::SolveValueKind;

use super::{
    SolveOperation, SolveRegisterId, SolveSlot, SolveSpannedOperation, SolveStorageClass,
    SolveValueType,
};

/// The serde name of a value derived from content that is serialized beside
/// it. Wire formats see it as its value; the block digest leaves it out, so a
/// derived plan is not hashed a second time.
pub(in crate::typed_program) const DERIVED: &str = "rumoca.solve.derived";

/// Serialize `value` as derived content (see [`DERIVED`]).
pub(super) fn derived<T: Serialize, S: serde::Serializer>(
    value: &T,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    serializer.serialize_newtype_struct(DERIVED, value)
}

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
    element_offsets: Box<[Option<u32>]>,
    fusion: SolveFusionPlan,
    aliases: SolveAliasPlan,
    cells: SolveFrameCells,
}

impl SolveStoragePlan {
    pub(super) fn host(
        slots: &[SolveSlot],
        register_types: &[SolveValueType],
        operations: &[SolveSpannedOperation],
        last_reads: &[Option<usize>],
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
        let fusion = SolveFusionPlan::of(register_types, operations, last_reads);
        let aliases =
            SolveAliasPlan::of(slots, register_types, operations, fusion.last_reads(), &[]);
        let cells = SolveFrameCells::of(
            register_types,
            operations,
            fusion.last_reads(),
            aliases.registers(),
        );
        Self {
            slots: slot_placements,
            registers,
            element_offsets: element_offsets(register_types, operations),
            fusion,
            aliases,
            cells,
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

    /// The row-major scalar offset, inside its aggregate, of each element
    /// read or update whose bounds check cannot fail, by operation index;
    /// `None` for every other operation. Such an access indexes directly.
    #[must_use]
    pub fn element_offsets(&self) -> &[Option<u32>] {
        &self.element_offsets
    }

    /// The registers a target may evaluate inside a later loop instead of
    /// storing, and the last reads that follow.
    #[must_use]
    pub fn fusion(&self) -> &SolveFusionPlan {
        &self.fusion
    }

    /// Where each register lives once the alias rules are applied, for an
    /// entry whose output cells only the body's stores write. An observation
    /// entry uses its owner's
    /// [`observation_aliases`](crate::SolvePureCallOwner::observation_aliases).
    #[must_use]
    pub fn aliases(&self) -> &SolveAliasPlan {
        &self.aliases
    }

    /// The reused frame range of each register homed in the body's own
    /// cells under [`aliases`](Self::aliases).
    #[must_use]
    pub fn cells(&self) -> &SolveFrameCells {
        &self.cells
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

/// The static offset of each `ProjectElementDynamic` and `UpdateElement`
/// whose one-based indices are all Integer constants defined earlier in this
/// body and within the aggregate's extents (see
/// [`SolveStoragePlan::element_offsets`]).
fn element_offsets(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
) -> Box<[Option<u32>]> {
    let mut constants = vec![None; register_types.len()];
    operations
        .iter()
        .map(|issued| match &issued.operation {
            SolveOperation::Constant { destination, value } => {
                if let (SolveValueKind::Integer(value), Some(known)) =
                    (value.kind(), constants.get_mut(destination.index()))
                {
                    *known = Some(value);
                }
                None
            }
            SolveOperation::ProjectElementDynamic {
                aggregate, indices, ..
            }
            | SolveOperation::UpdateElement {
                aggregate, indices, ..
            } => static_offset(register_types, &constants, *aggregate, indices),
            _ => None,
        })
        .collect()
}

/// Row-major offset of constant one-based `indices` into `aggregate`, if
/// every index is a known constant within its extent.
fn static_offset(
    register_types: &[SolveValueType],
    constants: &[Option<i64>],
    aggregate: SolveRegisterId,
    indices: &[SolveRegisterId],
) -> Option<u32> {
    let dimensions = register_types.get(aggregate.index())?.dimensions();
    if indices.len() != dimensions.len() {
        return None;
    }
    indices
        .iter()
        .zip(dimensions)
        .try_fold(0_u32, |offset, (index, &extent)| {
            let value = u32::try_from((*constants.get(index.index())?)?).ok()?;
            (1..=extent)
                .contains(&value)
                .then(|| offset.checked_mul(extent)?.checked_add(value - 1))
                .flatten()
        })
}
