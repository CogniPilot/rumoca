//! Complete numerical entries retain their checked body and original helpers.
mod shared_helpers;
pub use shared_helpers::{
    EntryCallCatalog, EntryCallOccurrence, EntryCallOccurrenceId, EntryCallOccurrences,
    EntryCallTarget, SharedHelperProgramEntry,
};

#[cfg(test)]
mod tests;

use std::sync::Arc;

use super::{
    SolveProgramConstructionError, SolvePureCallTable, SolveSlotId, TypedProgram,
    TypedProgramBuilder,
};

#[derive(Debug)]
pub(super) struct EntryInterface {
    pub(super) inputs: Box<[SolveSlotId]>,
    pub(super) outputs: Box<[SolveSlotId]>,
}

/// A complete body and the exact immutable helper namespace used to build it.
/// There is no constructor from detached bodies or caller-supplied slot lists.
#[derive(Clone, Debug)]
pub struct TypedProgramEntry {
    body: Arc<TypedProgram>,
    calls: Arc<SolvePureCallTable>,
    interface: Arc<EntryInterface>,
}

impl TypedProgramEntry {
    pub fn construct(
        calls: Arc<SolvePureCallTable>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        let (body, interface) =
            TypedProgram::construct_complete_entry(calls.primal_view(), calls.arithmetic(), build)?;
        Ok(Self {
            body: Arc::new(body),
            calls,
            interface: Arc::new(interface),
        })
    }

    pub fn body(&self) -> &TypedProgram {
        &self.body
    }
    pub fn calls(&self) -> &SolvePureCallTable {
        &self.calls
    }
    pub fn input_slots(&self) -> &[SolveSlotId] {
        &self.interface.inputs
    }
    pub fn output_slots(&self) -> &[SolveSlotId] {
        &self.interface.outputs
    }

    /// Equal independent constructions never acquire the original entry identity.
    pub fn shares_entry_owner(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.body, &other.body) && Arc::ptr_eq(&self.interface, &other.interface)
    }
}
