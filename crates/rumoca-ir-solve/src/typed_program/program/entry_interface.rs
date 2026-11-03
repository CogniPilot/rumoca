//! Argument/result facts issued incrementally by complete-entry construction.
use super::*;
use crate::typed_program::entry::EntryInterface;

#[derive(Debug, Default)]
pub(super) struct EntryConstruction {
    inputs: Vec<SolveSlotId>,
    outputs: Vec<SolveSlotId>,
    pending: Vec<Option<Span>>,
    first_pending: Option<(usize, Span)>,
    enabled: bool,
}

impl EntryConstruction {
    pub(super) fn enable(&mut self) {
        self.enabled = true;
    }

    pub(super) fn finish(self) -> Result<EntryInterface, SolveProgramConstructionError> {
        if let Some((_, provenance)) = self.first_pending {
            return Err(SolveProgramConstructionError::UninitializedSlot { provenance });
        }
        Ok(EntryInterface {
            inputs: self.inputs.into_boxed_slice(),
            outputs: self.outputs.into_boxed_slice(),
        })
    }
}

impl TypedProgramBuilder<'_> {
    pub(super) fn entry_assume_slot_initialized(
        &self,
        index: usize,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        if self.entry_interface.enabled && !self.slot_initialized[index] {
            return Err(SolveProgramConstructionError::UninitializedSlot { provenance });
        }
        Ok(())
    }

    pub(super) fn entry_declare_slot(
        &mut self,
        slot: &SolveSlot,
    ) -> Result<(), SolveProgramConstructionError> {
        let entry = &mut self.entry_interface;
        if !entry.enabled {
            return Ok(());
        }
        let initialized = slot.storage.initialized_externally();
        if initialized && slot.access != SolveSlotAccess::ReadOnly
            || slot.storage == SolveStorageClass::Output
                && slot.access != SolveSlotAccess::ReadWrite
        {
            return Err(SolveProgramConstructionError::InvalidEntryInterface {
                provenance: slot.provenance,
            });
        }
        let pending = if slot.storage == SolveStorageClass::Output {
            entry.outputs.push(slot.id);
            if entry.first_pending.is_none() {
                entry.first_pending = Some((slot.id.index(), slot.provenance));
            }
            Some(slot.provenance)
        } else {
            if initialized {
                entry.inputs.push(slot.id);
            }
            None
        };
        entry.pending.push(pending);
        Ok(())
    }

    pub(super) fn entry_store(
        &mut self,
        slot: SolveSlotId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let entry = &mut self.entry_interface;
        if !entry.enabled {
            return Ok(());
        }
        if self.slots[slot.index()].storage == SolveStorageClass::Output {
            if entry.pending[slot.index()].take().is_none() {
                return Err(SolveProgramConstructionError::InvalidEntryInterface { provenance });
            }
            if entry
                .first_pending
                .is_some_and(|(index, _)| index == slot.index())
            {
                // The cursor only moves forward through declared slots. Each cell
                // is visited at most once, independently of tensor element count.
                entry.first_pending = entry
                    .pending
                    .iter()
                    .enumerate()
                    .skip(slot.index() + 1)
                    .find_map(|(index, provenance)| provenance.map(|span| (index, span)));
            }
        }
        Ok(())
    }
}
