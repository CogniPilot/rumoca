//! Checked Algorithm Code execution root (SPEC_0007, SOLVE-C32–C38).
//!
//! A `SolveAlgorithmBlock` is the Solve-side refinement of one checked
//! Algorithm Code block. It is a distinct root, not a mode of `SolveProblem`
//! (SOLVE-C32): it carries one block slot table and exactly one Startup,
//! Recalibrate and DoStep method (SOLVE-C34), each a `TypedProgram` region over
//! that table plus its own method locals.
//!
//! Construction seeds every method builder with the block slots in table
//! order, so all three methods share one slot identity prefix, and then checks
//! that nothing after the prefix is anything but a method local (SOLVE-C35).
//! Read-before-definition inside a method is already rejected by the typed
//! program builder. Every block slot and method local carries a backend-neutral
//! reference to its checked Algorithm Code entity, and those references must be
//! injective (SOLVE-C38). Operation- and statement-level correlation arrives
//! with the refinement that produces method bodies.

use super::{
    ProgramSlot, SolveArithmeticProfile, SolveProgramConstructionError, SolvePureCallOwnerId,
    SolvePureCallTable, SolveScalarType, SolveSlotAccess, SolveSlotId, SolveStorageClass,
    SolveValueType, TypedProgram, TypedProgramBuilder,
};
use rumoca_core::Span;
use serde::Serialize;

/// What kind of checked Algorithm Code entity a Solve identity refines.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveAlgorithmCodeEntityKind {
    Interface,
    CompartmentMember,
    Protected,
    MethodLocal,
    Function,
    Statement,
    /// One bit of the block's implicit `ErrorSignalStatus`: derived from the
    /// block itself, not from a checked declaration.
    ErrorSignalStatus,
}

/// One checked Algorithm Code entity, named without depending on the GALEC IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct SolveAlgorithmCodeRef {
    kind: SolveAlgorithmCodeEntityKind,
    qualified_name: Box<str>,
    span: Span,
}

impl SolveAlgorithmCodeRef {
    #[must_use]
    pub fn construct(
        kind: SolveAlgorithmCodeEntityKind,
        qualified_name: impl Into<Box<str>>,
        span: Span,
    ) -> Self {
        Self {
            kind,
            qualified_name: qualified_name.into(),
            span,
        }
    }

    #[must_use]
    pub const fn kind(&self) -> SolveAlgorithmCodeEntityKind {
        self.kind
    }

    #[must_use]
    pub fn qualified_name(&self) -> &str {
        &self.qualified_name
    }

    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    /// The entity this reference names: its kind and qualified name.
    ///
    /// Provenance is deliberately not part of it, so one entity cannot gain a
    /// second Solve identity by being cited at another span. Injectivity and
    /// any crosswalk to the checked package key on this.
    #[must_use]
    pub fn identity(&self) -> (SolveAlgorithmCodeEntityKind, &str) {
        (self.kind, &self.qualified_name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveBlockMethodKind {
    Startup,
    Recalibrate,
    DoStep,
}

/// One persistent or interface slot of the block table.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveBlockSlotDeclaration {
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    source: SolveAlgorithmCodeRef,
}

impl SolveBlockSlotDeclaration {
    #[must_use]
    pub const fn construct(
        value_type: SolveValueType,
        storage: SolveStorageClass,
        access: SolveSlotAccess,
        source: SolveAlgorithmCodeRef,
    ) -> Self {
        Self {
            value_type,
            storage,
            access,
            source,
        }
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn storage(&self) -> SolveStorageClass {
        self.storage
    }

    #[must_use]
    pub const fn access(&self) -> SolveSlotAccess {
        self.access
    }

    #[must_use]
    pub const fn source(&self) -> &SolveAlgorithmCodeRef {
        &self.source
    }
}

/// One lifecycle method: its typed body, the sources of its locals in local
/// slot order, and the signal status slots it declares may escape.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveBlockMethod {
    kind: SolveBlockMethodKind,
    program: TypedProgram,
    local_sources: Box<[SolveAlgorithmCodeRef]>,
    escapes: Box<[usize]>,
}

impl SolveBlockMethod {
    #[must_use]
    pub const fn kind(&self) -> SolveBlockMethodKind {
        self.kind
    }

    #[must_use]
    pub const fn program(&self) -> &TypedProgram {
        &self.program
    }

    #[must_use]
    pub fn local_sources(&self) -> &[SolveAlgorithmCodeRef] {
        &self.local_sources
    }

    /// The `SignalStatus` block slots whose signal may escape this method
    /// (its declared escape set), ascending.
    #[must_use]
    pub fn escapes(&self) -> &[usize] {
        &self.escapes
    }
}

/// A block's functions: one pure-call owner per checked function, the
/// checked function each owner refines (in owner order), and each owner's
/// status channels.
#[derive(Debug, Clone)]
pub struct SolveBlockFunctions {
    pub table: SolvePureCallTable,
    pub sources: Vec<SolveAlgorithmCodeRef>,
    pub status_bindings: Vec<SolveStatusBinding>,
}

/// One function owner's status channels (#483 EL007): the body slots
/// holding the bits the function has raised so far, one per signal status
/// slot of the block, in block order. A fault inside the owner carries those
/// bits to its caller, which unions them into its own status before the
/// fault unwinds further. Only checked Algorithm Code functions have one.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveStatusBinding {
    owner: SolvePureCallOwnerId,
    slots: Box<[SolveSlotId]>,
}

impl SolveStatusBinding {
    #[must_use]
    pub fn new(owner: SolvePureCallOwnerId, slots: Vec<SolveSlotId>) -> Self {
        Self {
            owner,
            slots: slots.into_boxed_slice(),
        }
    }

    #[must_use]
    pub const fn owner(&self) -> SolvePureCallOwnerId {
        self.owner
    }

    #[must_use]
    pub fn slots(&self) -> &[SolveSlotId] {
        &self.slots
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveAlgorithmBlock {
    arithmetic: SolveArithmeticProfile,
    slots: Box<[SolveBlockSlotDeclaration]>,
    /// The block's functions as pure-call owners, which every method may call.
    functions: SolvePureCallTable,
    /// The checked function each owner refines, in owner order.
    function_sources: Box<[SolveAlgorithmCodeRef]>,
    /// Each function owner's status channels, in owner order.
    status_bindings: Box<[SolveStatusBinding]>,
    startup: SolveBlockMethod,
    recalibrate: SolveBlockMethod,
    do_step: SolveBlockMethod,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveAlgorithmBlockConstructionError {
    Program {
        method: SolveBlockMethodKind,
        error: SolveProgramConstructionError,
    },
    /// A function owner's status binding names an owner outside the block's
    /// table, repeats one, or does not name one Boolean method-local slot per
    /// signal status slot with matching trailing Boolean results.
    StatusBinding { provenance: Span },
    /// A method local or a `PreviousState` slot in the block table; neither
    /// is block storage that checked Algorithm Code can produce.
    UnsupportedBlockStorage { provenance: Span },
    /// A method body did not keep the block slots as its identity prefix.
    SlotPrefixMismatch { method: SolveBlockMethodKind },
    /// A method declared persistent or interface storage of its own.
    MethodOwnedBlockStorage {
        method: SolveBlockMethodKind,
        provenance: Span,
    },
    /// A method's local sources do not pair one-to-one with its locals.
    LocalSourceCount { method: SolveBlockMethodKind },
    /// A block slot names something other than a declaration, or a method local
    /// names block storage.
    SourceKindMismatch { provenance: Span },
    /// A method local's source does not carry the provenance of its own slot.
    LocalSourceProvenance {
        method: SolveBlockMethodKind,
        provenance: Span,
    },
    /// Two Solve identities refine the same Algorithm Code entity.
    DuplicateSource { provenance: Span },
    /// The function table's arithmetic is not the block's.
    FunctionTableProfile,
    /// Function sources do not pair one-to-one with the table's owners, or one
    /// names something other than a function.
    FunctionSources,
    /// A declared escape names a slot that is not a signal status slot.
    EscapeNotSignalStatus {
        method: SolveBlockMethodKind,
        slot: usize,
    },
}

impl SolveAlgorithmBlockConstructionError {
    #[must_use]
    pub const fn source_span(&self) -> Option<Span> {
        match self {
            Self::Program { error, .. } => error.source_span(),
            Self::SlotPrefixMismatch { .. }
            | Self::LocalSourceCount { .. }
            | Self::FunctionTableProfile
            | Self::FunctionSources
            | Self::EscapeNotSignalStatus { .. } => None,
            Self::UnsupportedBlockStorage { provenance }
            | Self::MethodOwnedBlockStorage { provenance, .. }
            | Self::SourceKindMismatch { provenance }
            | Self::LocalSourceProvenance { provenance, .. }
            | Self::DuplicateSource { provenance }
            | Self::StatusBinding { provenance } => Some(*provenance),
        }
    }
}

impl std::fmt::Display for SolveAlgorithmBlockConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program { method, error } => write!(formatter, "{method:?} method: {error}"),
            Self::UnsupportedBlockStorage { .. } => {
                formatter.write_str("storage class cannot be declared in the block slot table")
            }
            Self::SlotPrefixMismatch { method } => write!(
                formatter,
                "{method:?} method does not keep the block slots as its identity prefix"
            ),
            Self::MethodOwnedBlockStorage { method, .. } => write!(
                formatter,
                "{method:?} method declares block storage outside the block slot table"
            ),
            Self::LocalSourceCount { method } => write!(
                formatter,
                "{method:?} method local sources do not pair one-to-one with its locals"
            ),
            Self::SourceKindMismatch { .. } => {
                formatter.write_str("slot source kind does not match its storage class")
            }
            Self::LocalSourceProvenance { method, .. } => write!(
                formatter,
                "{method:?} method local source does not carry its slot's provenance"
            ),
            Self::DuplicateSource { .. } => {
                formatter.write_str("two Solve identities refine the same Algorithm Code entity")
            }
            Self::FunctionTableProfile => {
                formatter.write_str("function table arithmetic differs from the block's")
            }
            Self::StatusBinding { .. } => formatter
                .write_str("a function status binding does not match its owner's status channels"),
            Self::FunctionSources => formatter
                .write_str("function sources do not pair one-to-one with the function owners"),
            Self::EscapeNotSignalStatus { method, slot } => write!(
                formatter,
                "{method:?} method declares block slot {slot} as an escaping signal"
            ),
        }
    }
}

impl std::error::Error for SolveAlgorithmBlockConstructionError {}

impl SolveAlgorithmBlock {
    /// Build one checked block.
    ///
    /// Each method closure receives a builder already holding the block slots
    /// (handles in table order), may declare only `MethodLocal` slots after
    /// them, and returns the source of each local it declared, in order.
    pub fn construct<S, R, D>(
        arithmetic: SolveArithmeticProfile,
        slots: Vec<SolveBlockSlotDeclaration>,
        startup: S,
        recalibrate: R,
        do_step: D,
    ) -> Result<Self, SolveAlgorithmBlockConstructionError>
    where
        S: for<'p> FnOnce(
            &mut TypedProgramBuilder<'p>,
            &[ProgramSlot<'p>],
        )
            -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
        R: for<'p> FnOnce(
            &mut TypedProgramBuilder<'p>,
            &[ProgramSlot<'p>],
        )
            -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
        D: for<'p> FnOnce(
            &mut TypedProgramBuilder<'p>,
            &[ProgramSlot<'p>],
        )
            -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
    {
        Self::construct_with_functions(
            arithmetic,
            slots,
            SolveBlockFunctions {
                table: SolvePureCallTable::builder(arithmetic).finish(),
                sources: Vec::new(),
                status_bindings: Vec::new(),
            },
            startup,
            recalibrate,
            do_step,
        )
    }

    /// [`Self::construct`] with the block's functions: `functions` holds one
    /// pure-call owner per checked function, `function_sources` names them in
    /// owner order, and every method is built able to call them.
    pub fn construct_with_functions<S, R, D>(
        arithmetic: SolveArithmeticProfile,
        slots: Vec<SolveBlockSlotDeclaration>,
        functions: SolveBlockFunctions,
        startup: S,
        recalibrate: R,
        do_step: D,
    ) -> Result<Self, SolveAlgorithmBlockConstructionError>
    where
        S: for<'p> FnOnce(
            &mut TypedProgramBuilder<'p>,
            &[ProgramSlot<'p>],
        )
            -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
        R: for<'p> FnOnce(
            &mut TypedProgramBuilder<'p>,
            &[ProgramSlot<'p>],
        )
            -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
        D: for<'p> FnOnce(
            &mut TypedProgramBuilder<'p>,
            &[ProgramSlot<'p>],
        )
            -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
    {
        let SolveBlockFunctions {
            table: functions,
            sources: function_sources,
            status_bindings,
        } = functions;
        if functions.arithmetic() != arithmetic {
            return Err(SolveAlgorithmBlockConstructionError::FunctionTableProfile);
        }
        if function_sources.len() != functions.owners().len()
            || function_sources
                .iter()
                .any(|source| source.kind != SolveAlgorithmCodeEntityKind::Function)
        {
            return Err(SolveAlgorithmBlockConstructionError::FunctionSources);
        }
        let signals = slots
            .iter()
            .filter(|slot| slot.storage == SolveStorageClass::SignalStatus)
            .count();
        require_status_bindings(&functions, &status_bindings, signals)?;
        for slot in &slots {
            let provenance = slot.source.span;
            if matches!(
                slot.storage,
                SolveStorageClass::MethodLocal | SolveStorageClass::PreviousState
            ) {
                return Err(
                    SolveAlgorithmBlockConstructionError::UnsupportedBlockStorage { provenance },
                );
            }
            let signal_status = slot.storage == SolveStorageClass::SignalStatus;
            let declared = matches!(
                slot.source.kind,
                SolveAlgorithmCodeEntityKind::Interface
                    | SolveAlgorithmCodeEntityKind::CompartmentMember
                    | SolveAlgorithmCodeEntityKind::Protected
            );
            let derived = slot.source.kind == SolveAlgorithmCodeEntityKind::ErrorSignalStatus;
            if !(if signal_status { derived } else { declared }) {
                return Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
                    provenance,
                });
            }
        }
        let startup = build_method(
            arithmetic,
            &functions,
            &slots,
            SolveBlockMethodKind::Startup,
            startup,
        )?;
        let recalibrate = build_method(
            arithmetic,
            &functions,
            &slots,
            SolveBlockMethodKind::Recalibrate,
            recalibrate,
        )?;
        let do_step = build_method(
            arithmetic,
            &functions,
            &slots,
            SolveBlockMethodKind::DoStep,
            do_step,
        )?;
        let block = Self {
            arithmetic,
            slots: slots.into_boxed_slice(),
            functions,
            function_sources: function_sources.into_boxed_slice(),
            status_bindings: status_bindings.into_boxed_slice(),
            startup,
            recalibrate,
            do_step,
        };
        block.require_injective_sources()?;
        Ok(block)
    }

    #[must_use]
    pub const fn arithmetic(&self) -> SolveArithmeticProfile {
        self.arithmetic
    }

    #[must_use]
    pub fn slots(&self) -> &[SolveBlockSlotDeclaration] {
        &self.slots
    }

    /// The block's functions; every method was built able to call them.
    #[must_use]
    pub const fn functions(&self) -> &SolvePureCallTable {
        &self.functions
    }

    /// The status channels of function owner `owner`, if it has any.
    #[must_use]
    pub fn status_binding(&self, owner: SolvePureCallOwnerId) -> Option<&SolveStatusBinding> {
        self.status_bindings
            .iter()
            .find(|binding| binding.owner == owner)
    }

    /// The checked function each owner of [`Self::functions`] refines.
    #[must_use]
    pub fn function_sources(&self) -> &[SolveAlgorithmCodeRef] {
        &self.function_sources
    }

    #[must_use]
    pub const fn startup(&self) -> &SolveBlockMethod {
        &self.startup
    }

    #[must_use]
    pub const fn recalibrate(&self) -> &SolveBlockMethod {
        &self.recalibrate
    }

    #[must_use]
    pub const fn do_step(&self) -> &SolveBlockMethod {
        &self.do_step
    }

    /// Record the declared escape set of `method`: signal status slots,
    /// in any order.
    pub fn with_method_escapes(
        mut self,
        method: SolveBlockMethodKind,
        escapes: impl IntoIterator<Item = usize>,
    ) -> Result<Self, SolveAlgorithmBlockConstructionError> {
        let mut escapes = escapes.into_iter().collect::<Vec<_>>();
        if let Some(&slot) = escapes.iter().find(|&&slot| {
            self.slots
                .get(slot)
                .is_none_or(|declared| declared.storage != SolveStorageClass::SignalStatus)
        }) {
            return Err(
                SolveAlgorithmBlockConstructionError::EscapeNotSignalStatus { method, slot },
            );
        }
        escapes.sort_unstable();
        escapes.dedup();
        let target = match method {
            SolveBlockMethodKind::Startup => &mut self.startup,
            SolveBlockMethodKind::Recalibrate => &mut self.recalibrate,
            SolveBlockMethodKind::DoStep => &mut self.do_step,
        };
        target.escapes = escapes.into_boxed_slice();
        Ok(self)
    }

    /// The three methods in lifecycle order.
    #[must_use]
    pub const fn methods(&self) -> [&SolveBlockMethod; 3] {
        [&self.startup, &self.recalibrate, &self.do_step]
    }

    fn require_injective_sources(&self) -> Result<(), SolveAlgorithmBlockConstructionError> {
        let mut seen = std::collections::HashSet::new();
        let block_sources = self.slots.iter().map(SolveBlockSlotDeclaration::source);
        let local_sources = self
            .methods()
            .into_iter()
            .flat_map(|method| method.local_sources.iter());
        let function_sources = self.function_sources.iter();
        for source in block_sources.chain(local_sources).chain(function_sources) {
            if !seen.insert(source.identity()) {
                return Err(SolveAlgorithmBlockConstructionError::DuplicateSource {
                    provenance: source.span,
                });
            }
        }
        Ok(())
    }
}

fn build_method<M>(
    arithmetic: SolveArithmeticProfile,
    functions: &SolvePureCallTable,
    slots: &[SolveBlockSlotDeclaration],
    kind: SolveBlockMethodKind,
    body: M,
) -> Result<SolveBlockMethod, SolveAlgorithmBlockConstructionError>
where
    M: for<'p> FnOnce(
        &mut TypedProgramBuilder<'p>,
        &[ProgramSlot<'p>],
    ) -> Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>,
{
    let mut local_sources = Vec::new();
    let program =
        TypedProgram::construct_with_calls(arithmetic, functions.primal_view(), |builder| {
            let handles = slots
                .iter()
                .map(|slot| {
                    builder.declare_slot(
                        slot.value_type.clone(),
                        slot.storage,
                        slot.access,
                        slot.source.span,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            if kind != SolveBlockMethodKind::Startup {
                for (handle, slot) in handles.iter().zip(slots) {
                    builder.assume_slot_initialized(*handle, slot.source.span)?;
                }
            }
            local_sources = body(builder, &handles)?;
            Ok(())
        })
        .map_err(|error| SolveAlgorithmBlockConstructionError::Program {
            method: kind,
            error,
        })?;
    require_block_prefix(&program, slots, kind)?;
    let locals = &program.slots()[slots.len()..];
    if let Some(slot) = locals
        .iter()
        .find(|slot| slot.storage() != SolveStorageClass::MethodLocal)
    {
        return Err(
            SolveAlgorithmBlockConstructionError::MethodOwnedBlockStorage {
                method: kind,
                provenance: slot.provenance(),
            },
        );
    }
    if locals.len() != local_sources.len() {
        return Err(SolveAlgorithmBlockConstructionError::LocalSourceCount { method: kind });
    }
    if let Some(source) = local_sources
        .iter()
        .find(|source| source.kind != SolveAlgorithmCodeEntityKind::MethodLocal)
    {
        return Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
            provenance: source.span,
        });
    }
    if let Some(slot) = locals
        .iter()
        .zip(&local_sources)
        .find_map(|(slot, source)| (source.span != slot.provenance()).then_some(slot))
    {
        return Err(
            SolveAlgorithmBlockConstructionError::LocalSourceProvenance {
                method: kind,
                provenance: slot.provenance(),
            },
        );
    }
    Ok(SolveBlockMethod {
        kind,
        program,
        local_sources: local_sources.into_boxed_slice(),
        escapes: Box::default(),
    })
}

fn require_block_prefix(
    program: &TypedProgram,
    slots: &[SolveBlockSlotDeclaration],
    kind: SolveBlockMethodKind,
) -> Result<(), SolveAlgorithmBlockConstructionError> {
    let declared = program.slots();
    let matches = declared.len() >= slots.len()
        && declared
            .iter()
            .zip(slots)
            .enumerate()
            .all(|(index, (slot, expected))| {
                slot.id().index() == index
                    && slot.value_type() == &expected.value_type
                    && slot.storage() == expected.storage
                    && slot.access() == expected.access
                    && slot.provenance() == expected.source.span
            });
    if matches {
        Ok(())
    } else {
        Err(SolveAlgorithmBlockConstructionError::SlotPrefixMismatch { method: kind })
    }
}

#[cfg(test)]
mod tests;

/// Every owner of `functions` has exactly one binding, and it names the
/// owner's canonical status channels: the `signals` Boolean method-local
/// slots its body declares first after its input and output slots, in
/// signal order, matching its last `signals` Boolean results. Omitting an
/// owner, repeating or permuting a slot, or naming another local is refused.
fn require_status_bindings(
    functions: &SolvePureCallTable,
    bindings: &[SolveStatusBinding],
    signals: usize,
) -> Result<(), SolveAlgorithmBlockConstructionError> {
    let mut bound = vec![false; functions.owners().len()];
    for binding in bindings {
        let owner = functions.owner(binding.owner).ok_or(
            SolveAlgorithmBlockConstructionError::StatusBinding {
                provenance: Span::DUMMY,
            },
        )?;
        let first = !std::mem::replace(
            bound.get_mut(binding.owner.index() as usize).ok_or(
                SolveAlgorithmBlockConstructionError::StatusBinding {
                    provenance: owner.provenance(),
                },
            )?,
            true,
        );
        if !first || canonical_status_slots(owner, signals).as_deref() != Some(&binding.slots[..]) {
            return Err(SolveAlgorithmBlockConstructionError::StatusBinding {
                provenance: owner.provenance(),
            });
        }
    }
    if let Some(unbound) = functions
        .owners()
        .iter()
        .zip(&bound)
        .find_map(|(owner, &bound)| (!bound).then_some(owner))
    {
        return Err(SolveAlgorithmBlockConstructionError::StatusBinding {
            provenance: unbound.provenance(),
        });
    }
    Ok(())
}

/// The status channels `owner` must have, if its interface and body carry
/// them: its last `signals` results are Boolean scalars, and the `signals`
/// slots its body declares right after its input and output slots are
/// Boolean method locals.
fn canonical_status_slots(
    owner: &super::SolvePureCallOwner,
    signals: usize,
) -> Option<Vec<SolveSlotId>> {
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let outputs = owner.outputs();
    let trailing = outputs.get(outputs.len().checked_sub(signals)?..)?;
    if trailing
        .iter()
        .any(|output| *output.value_type() != boolean)
    {
        return None;
    }
    let start = owner.inputs().len() + outputs.len();
    let slots = owner.body().slots().get(start..start + signals)?;
    slots
        .iter()
        .all(|slot| {
            *slot.value_type() == boolean && slot.storage() == SolveStorageClass::MethodLocal
        })
        .then(|| slots.iter().map(super::SolveSlot::id).collect())
}
