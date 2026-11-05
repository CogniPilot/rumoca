//! Complete entries issue each helper call against one exact body catalog.
mod occurrences;
mod partitions;
#[cfg(test)]
mod tests;

pub use occurrences::{EntryCallOccurrence, EntryCallOccurrenceId, EntryCallOccurrences};
pub use partitions::{EntryCallPartition, EntryPartitionCall, SharedEntryCallPlan};

use super::*;
use crate::typed_program::call::SolvePureCallTableView;
use crate::{BoundSolvePureCallBody, ProgramCall, ProgramRegister, SolvePureCallOwnerId};
use rumoca_core::Span;

#[derive(Clone, Copy, Debug)]
enum CalleeChoice {
    Original(SolvePureCallOwnerId),
    Directional(SolvePureCallOwnerId),
}

/// Constructor-scoped helper choices, not an independently attachable body list.
struct CatalogStorage<'source> {
    bindings: Vec<BoundSolvePureCallBody<'source>>,
    choices: Vec<CalleeChoice>,
    original: Vec<SolvePureCallOwnerId>,
    directional: Vec<Option<SolvePureCallOwnerId>>,
}

/// Program-branded view of the exact immutable construction catalog.
pub struct EntryCallCatalog<'program> {
    bindings: &'program [BoundSolvePureCallBody<'program>],
    original: &'program [SolvePureCallOwnerId],
    directional: &'program [Option<SolvePureCallOwnerId>],
    marker: std::marker::PhantomData<&'program mut &'program ()>,
}

/// A selector issued only by this construction's exact helper catalog.
#[derive(Clone, Copy)]
pub struct EntryCallTarget<'program> {
    namespace: &'program [BoundSolvePureCallBody<'program>],
    selector: SolvePureCallOwnerId,
    marker: std::marker::PhantomData<&'program mut &'program ()>,
}

impl<'source> CatalogStorage<'source> {
    fn construct(
        calls: &'source SolvePureCallTable,
        provenance: Span,
    ) -> Result<Self, SolveProgramConstructionError> {
        let mut catalog = Self {
            bindings: Vec::new(),
            choices: Vec::new(),
            original: Vec::new(),
            directional: Vec::new(),
        };
        for owner in calls.owners() {
            let original = catalog.insert(
                calls.bind_original_body(owner.id(), provenance)?,
                CalleeChoice::Original(owner.id()),
                provenance,
            )?;
            catalog.original.push(original);
            let directional = if owner.directional().is_some() {
                Some(catalog.insert(
                    calls.bind_directional_body(owner.id(), provenance)?,
                    CalleeChoice::Directional(owner.id()),
                    provenance,
                )?)
            } else {
                None
            };
            catalog.directional.push(directional);
        }
        Ok(catalog)
    }

    fn insert(
        &mut self,
        body: BoundSolvePureCallBody<'source>,
        choice: CalleeChoice,
        provenance: Span,
    ) -> Result<SolvePureCallOwnerId, SolveProgramConstructionError> {
        let index = u32::try_from(self.bindings.len())
            .ok()
            .filter(|index| *index < u32::MAX)
            .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
        self.bindings.push(body);
        self.choices.push(choice);
        Ok(SolvePureCallOwnerId::from_index(index))
    }
}

impl<'program> EntryCallCatalog<'program> {
    pub fn original(
        &self,
        owner: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<EntryCallTarget<'program>, SolveProgramConstructionError> {
        let selector = self
            .original
            .get(owner.index() as usize)
            .copied()
            .ok_or(SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        Ok(EntryCallTarget {
            namespace: self.bindings,
            selector,
            marker: std::marker::PhantomData,
        })
    }

    pub fn directional(
        &self,
        owner: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<EntryCallTarget<'program>, SolveProgramConstructionError> {
        let selector = self
            .directional
            .get(owner.index() as usize)
            .ok_or(SolveProgramConstructionError::UnknownCallOwner { provenance })?
            .ok_or(SolveProgramConstructionError::UnavailableDirectionalCall { provenance })?;
        Ok(EntryCallTarget {
            namespace: self.bindings,
            selector,
            marker: std::marker::PhantomData,
        })
    }
}

impl<'program> TypedProgramBuilder<'program> {
    /// Reissue an immutable helper target for a nested program's register scope.
    /// Only this builder's exact inherited catalog can supply the new target.
    pub fn rebind_entry_call_target(
        &self,
        source: EntryCallTarget<'_>,
        provenance: Span,
    ) -> Result<EntryCallTarget<'program>, SolveProgramConstructionError> {
        if !self.shares_bound_call_namespace(source.namespace) {
            return Err(SolveProgramConstructionError::UnknownCallOwner { provenance });
        }
        let namespace = self
            .bound_call_namespace()
            .ok_or(SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        Ok(EntryCallTarget {
            namespace,
            selector: source.selector,
            marker: std::marker::PhantomData,
        })
    }

    pub fn emit_entry_call(
        &mut self,
        target: EntryCallTarget<'program>,
        arguments: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<ProgramCall<'program>, SolveProgramConstructionError> {
        if !self.shares_bound_call_namespace(target.namespace) {
            return Err(SolveProgramConstructionError::UnknownCallOwner { provenance });
        }
        self.emit_call(target.selector, arguments, provenance)
    }
}

/// A distinct complete entry whose selectors resolve only through its issued
/// helper catalog. Primal-only entry consumers cannot accept this product.
#[derive(Clone, Debug)]
pub struct SharedHelperProgramEntry {
    body: Arc<TypedProgram>,
    calls: Arc<SolvePureCallTable>,
    interface: Arc<EntryInterface>,
    choices: Arc<[CalleeChoice]>,
    provenance: Span,
}

impl SharedHelperProgramEntry {
    pub fn construct(
        calls: Arc<SolvePureCallTable>,
        provenance: Span,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
            &EntryCallCatalog<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        let catalog = CatalogStorage::construct(&calls, provenance)?;
        let (body, interface) = TypedProgram::construct_complete_entry(
            SolvePureCallTableView::bound(
                &catalog.bindings,
                &catalog.original,
                &catalog.directional,
            ),
            calls.arithmetic(),
            |builder| {
                let missing = || SolveProgramConstructionError::UnknownCallOwner { provenance };
                let bindings = builder.bound_call_namespace().ok_or_else(missing)?;
                let (original, directional) = builder.bound_call_indices().ok_or_else(missing)?;
                let scoped = EntryCallCatalog {
                    bindings,
                    original,
                    directional,
                    marker: std::marker::PhantomData,
                };
                build(builder, &scoped)
            },
        )?;
        let choices = catalog.choices.into();
        Ok(Self {
            body: Arc::new(body),
            calls,
            interface: Arc::new(interface),
            choices,
            provenance,
        })
    }

    /// Exact construction provenance, retained even for a slot-free entry.
    pub fn provenance(&self) -> Span {
        self.provenance
    }

    /// Original source namespace. Entry selectors still resolve through the
    /// issued call partitions, never directly through this table's owner IDs.
    pub fn calls(&self) -> &SolvePureCallTable {
        &self.calls
    }

    pub fn body(&self) -> &TypedProgram {
        &self.body
    }
    pub fn input_slots(&self) -> &[SolveSlotId] {
        &self.interface.inputs
    }
    pub fn output_slots(&self) -> &[SolveSlotId] {
        &self.interface.outputs
    }

    /// Resolve an entry selector using its issued immutable catalog. Helper
    /// body selectors instead resolve through BoundSolvePureCallBody.
    fn callee(
        &self,
        selector: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<BoundSolvePureCallBody<'_>, SolveProgramConstructionError> {
        match self.choices.get(selector.index() as usize) {
            Some(CalleeChoice::Original(owner)) => {
                self.calls.bind_original_body(*owner, provenance)
            }
            Some(CalleeChoice::Directional(owner)) => {
                self.calls.bind_directional_body(*owner, provenance)
            }
            None => Err(SolveProgramConstructionError::UnknownCallOwner { provenance }),
        }
    }

    /// Independent construction cannot acquire this entry's owner.
    pub fn shares_entry_owner(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.body, &other.body)
            && Arc::ptr_eq(&self.interface, &other.interface)
            && Arc::ptr_eq(&self.choices, &other.choices)
    }
}
