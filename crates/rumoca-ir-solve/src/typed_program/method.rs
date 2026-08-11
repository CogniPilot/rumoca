//! Bounded GALEC method vocabulary: lexical storage, ordered actions, checked
//! call ABI, and explicit limit/signal effects.
//!
//! A method owns one lexical scope tree. Parameters, results, and bound block
//! storage are declared by its interface; every other cell is a lexical local
//! that cannot become persistent state and cannot enter the call ABI. Bodies
//! are ordered action blocks whose evaluation order is exactly the order in
//! which the constructors accepted them.

mod action;
mod builder;
mod escape;
pub(in crate::typed_program) mod wire;

use rumoca_core::Span;
use serde::Serialize;

use super::call::{SolvePureCallInterface, SolvePureCallOwner, SolvePureCallTable};
use super::effect::{SolveSignalSet, SolveValueRange};
use super::program::{SolveProgramConstructionError, SolveSlotAccess, SolveStorageClass};
use super::types::{SolveArithmeticProfile, SolveValueType};

pub use action::{
    SolveAction, SolveActionBlock, SolveBranchCondition, SolveBranchConditionSpec,
    SolveLimitTarget, SolveLocalDeclaration, SolveSignalCheck, SolveSignalTest, SolveSpannedAction,
    SolveValueProgram,
};
pub use builder::{
    MethodCell, MethodClosure, SolveLimitTargetSpec, SolveMethodBuilder, SolveMethodCells,
};

/// Construction-issued identity of one method within one table.
///
/// This is the only method identity Solve owns, and it is not forgeable: the
/// constructor is private to `typed_program`, the value is the table-local
/// ordinal the table itself issued, and the wire projects it as array position
/// rather than carrying it. Solve deliberately publishes no opaque integer
/// "Algorithm Code identity": a public integer any caller can mint proves
/// nothing about upstream origin. Cross-stage origin is carried by exact
/// provenance today and must become a typed capability issued by the upstream
/// Algorithm Code owner when that owner exists.
///
/// Non-forgeability is enforced, not documented: the type does not decode, so
/// the reviewer's probe cannot even be written.
///
/// ```compile_fail,E0277
/// # use rumoca_ir_solve::SolveMethodId;
/// let forged: SolveMethodId = serde_json::from_str("7").expect("no such decoder");
/// ```
///
/// ```compile_fail,E0277
/// fn only_decodable<T: serde::de::DeserializeOwned>() {}
/// only_decodable::<rumoca_ir_solve::SolveMethodId>();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveMethodId(u32);

impl SolveMethodId {
    pub(in crate::typed_program) const fn from_index(index: u32) -> Self {
        Self(index)
    }

    #[must_use]
    pub const fn index(self) -> u32 {
        self.0
    }
}

/// One lexical scope inside one method.
///
/// Scopes are issued when a block is opened and never cross the wire at all,
/// so the identity has no decoder to forge.
///
/// ```compile_fail,E0277
/// fn only_decodable<T: serde::de::DeserializeOwned>() {}
/// only_decodable::<rumoca_ir_solve::SolveScopeId>();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveScopeId(u32);

impl SolveScopeId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// One typed storage cell addressed by method actions.
///
/// The wire addresses a cell by wire-local ordinal, which the builder resolves
/// into the identity it issued, so no cell identity is decodable.
///
/// ```compile_fail,E0277
/// fn only_decodable<T: serde::de::DeserializeOwned>() {}
/// only_decodable::<rumoca_ir_solve::SolveCellId>();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveCellId(u32);

impl SolveCellId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// One signal closure bound by a catching signal check.
///
/// A closure identity is issued by the branch that caught, so it is likewise
/// addressed by ordinal and never decoded.
///
/// ```compile_fail,E0277
/// fn only_decodable<T: serde::de::DeserializeOwned>() {}
/// only_decodable::<rumoca_ir_solve::SolveSignalClosureId>();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveSignalClosureId(u32);

impl SolveSignalClosureId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveScope {
    id: SolveScopeId,
    parent: Option<SolveScopeId>,
    provenance: Span,
}

impl SolveScope {
    #[must_use]
    pub const fn id(&self) -> SolveScopeId {
        self.id
    }

    #[must_use]
    pub const fn parent(&self) -> Option<SolveScopeId> {
        self.parent
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveCell {
    id: SolveCellId,
    scope: SolveScopeId,
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    range: Option<SolveValueRange>,
    provenance: Span,
}

impl SolveCell {
    #[must_use]
    pub const fn id(&self) -> SolveCellId {
        self.id
    }

    #[must_use]
    pub const fn scope(&self) -> SolveScopeId {
        self.scope
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
    pub fn range(&self) -> Option<&SolveValueRange> {
        self.range.as_ref()
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }

    /// True when the cell holds block storage rather than lexical scratch.
    #[must_use]
    pub const fn is_block_storage(&self) -> bool {
        !matches!(self.storage, SolveStorageClass::MethodLocal)
    }

    /// True when the cell holds state that survives one method call.
    #[must_use]
    pub const fn is_persistent(&self) -> bool {
        matches!(
            self.storage,
            SolveStorageClass::PersistentState | SolveStorageClass::PreviousState
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveSignalClosure {
    id: SolveSignalClosureId,
    scope: SolveScopeId,
    caught: SolveSignalSet,
    provenance: Span,
}

impl SolveSignalClosure {
    #[must_use]
    pub const fn id(&self) -> SolveSignalClosureId {
        self.id
    }

    #[must_use]
    pub const fn scope(&self) -> SolveScopeId {
        self.scope
    }

    #[must_use]
    pub const fn caught(&self) -> SolveSignalSet {
        self.caught
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

/// GALEC distinguishes stateless functions from stateful methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveMethodKind {
    Stateless,
    Stateful,
}

/// One block-storage cell a method binds before its body is constructed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, serde::Deserialize)]
pub struct SolveMethodBinding {
    value_type: SolveValueType,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    range: Option<SolveValueRange>,
}

impl SolveMethodBinding {
    #[must_use]
    pub fn construct(
        value_type: SolveValueType,
        storage: SolveStorageClass,
        access: SolveSlotAccess,
        range: Option<SolveValueRange>,
    ) -> Self {
        Self {
            value_type,
            storage,
            access,
            range,
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
    pub fn range(&self) -> Option<&SolveValueRange> {
        self.range.as_ref()
    }
}

/// The complete typed interface of one method.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, serde::Deserialize)]
pub struct SolveMethodInterface {
    parameters: Box<[SolveValueType]>,
    results: Box<[SolveValueType]>,
    bindings: Box<[SolveMethodBinding]>,
    escapes: SolveSignalSet,
}

impl SolveMethodInterface {
    #[must_use]
    pub fn construct(
        parameters: Vec<SolveValueType>,
        results: Vec<SolveValueType>,
        bindings: Vec<SolveMethodBinding>,
        escapes: SolveSignalSet,
    ) -> Self {
        Self {
            parameters: parameters.into_boxed_slice(),
            results: results.into_boxed_slice(),
            bindings: bindings.into_boxed_slice(),
            escapes,
        }
    }

    #[must_use]
    pub const fn parameters(&self) -> &[SolveValueType] {
        &self.parameters
    }

    #[must_use]
    pub const fn results(&self) -> &[SolveValueType] {
        &self.results
    }

    #[must_use]
    pub const fn bindings(&self) -> &[SolveMethodBinding] {
        &self.bindings
    }

    #[must_use]
    pub const fn escapes(&self) -> SolveSignalSet {
        self.escapes
    }
}

/// How one parameter crosses the call boundary. Derived, never selected.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveParameterPassing {
    Value,
    ConstantReference,
}

/// How one result crosses the call boundary. Derived, never selected.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveResultPassing {
    ReturnValue,
    OutputBuffer,
}

/// Whether the call boundary returns an error-signal status word.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveStatusPassing {
    None,
    ErrorSignalReturn,
}

/// The complete checked call ABI of one method.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveCallAbiPlan {
    parameters: Box<[SolveParameterPassing]>,
    results: Box<[SolveResultPassing]>,
    status: SolveStatusPassing,
}

impl SolveCallAbiPlan {
    /// Derives the only admissible ABI for one checked interface.
    ///
    /// Scalars travel by value and aggregates by constant reference. A method
    /// that can escape a signal spends its return value on the status word, so
    /// its results always travel through output buffers.
    #[must_use]
    pub fn derive(interface: &SolveMethodInterface) -> Self {
        let status = if interface.escapes.is_empty() {
            SolveStatusPassing::None
        } else {
            SolveStatusPassing::ErrorSignalReturn
        };
        let parameters = interface
            .parameters
            .iter()
            .map(|value_type| {
                if value_type.dimensions().is_empty() {
                    SolveParameterPassing::Value
                } else {
                    SolveParameterPassing::ConstantReference
                }
            })
            .collect();
        let returns_one_scalar = status == SolveStatusPassing::None
            && interface.results.len() == 1
            && interface.results[0].dimensions().is_empty();
        let passing = if returns_one_scalar {
            SolveResultPassing::ReturnValue
        } else {
            SolveResultPassing::OutputBuffer
        };
        Self {
            parameters,
            results: vec![passing; interface.results.len()].into_boxed_slice(),
            status,
        }
    }

    #[must_use]
    pub const fn parameters(&self) -> &[SolveParameterPassing] {
        &self.parameters
    }

    #[must_use]
    pub const fn results(&self) -> &[SolveResultPassing] {
        &self.results
    }

    #[must_use]
    pub const fn status(&self) -> SolveStatusPassing {
        self.status
    }
}

/// One checked, bounded, acyclic GALEC method.
///
/// Every field the constructors issue — the identity, the derived call ABI, and
/// the scope, cell, and closure arenas — is absent from the wire and re-issued
/// by replay, so only `kind`, `interface`, `body`, and `provenance` are wire
/// inputs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveMethod {
    #[serde(skip)]
    id: SolveMethodId,
    kind: SolveMethodKind,
    interface: SolveMethodInterface,
    #[serde(skip)]
    abi: SolveCallAbiPlan,
    #[serde(skip)]
    scopes: Box<[SolveScope]>,
    #[serde(skip)]
    cells: Box<[SolveCell]>,
    #[serde(skip)]
    closures: Box<[SolveSignalClosure]>,
    body: SolveActionBlock,
    provenance: Span,
}

impl SolveMethod {
    #[must_use]
    pub const fn id(&self) -> SolveMethodId {
        self.id
    }

    #[must_use]
    pub const fn kind(&self) -> SolveMethodKind {
        self.kind
    }

    #[must_use]
    pub const fn interface(&self) -> &SolveMethodInterface {
        &self.interface
    }

    #[must_use]
    pub const fn abi(&self) -> &SolveCallAbiPlan {
        &self.abi
    }

    #[must_use]
    pub const fn scopes(&self) -> &[SolveScope] {
        &self.scopes
    }

    #[must_use]
    pub const fn cells(&self) -> &[SolveCell] {
        &self.cells
    }

    #[must_use]
    pub const fn closures(&self) -> &[SolveSignalClosure] {
        &self.closures
    }

    #[must_use]
    pub const fn body(&self) -> &SolveActionBlock {
        &self.body
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }

    fn signature(&self) -> SolveMethodSignature {
        SolveMethodSignature {
            id: self.id,
            kind: self.kind,
            interface: self.interface.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::typed_program) struct SolveMethodSignature {
    pub(in crate::typed_program) id: SolveMethodId,
    pub(in crate::typed_program) kind: SolveMethodKind,
    pub(in crate::typed_program) interface: SolveMethodInterface,
}

/// One bounded acyclic table of checked GALEC methods.
///
/// A method can only call methods issued before it, so the call graph is
/// acyclic by construction and every execution is bounded.
///
/// A table is a reusable method *vocabulary*, never a block lifecycle root. It
/// claims nothing about `Startup`, `DoStep`, or any other GALEC lifecycle role,
/// and it deliberately does not police root completeness: a table of one
/// arbitrary helper and a table of none are both valid vocabularies, and a
/// non-emptiness rule would prove neither role, cardinality, nor interface.
/// Only the future checked `SolveAlgorithmBlock::construct` may claim lifecycle
/// semantics, and it — not this type — must require the exact lifecycle roles,
/// their cardinality, and their interfaces, and own the completed root and its
/// wire replay.
///
/// The boundary is enforced, not merely documented: no lifecycle role is
/// readable from a table, so no consumer can mistake a vocabulary for a root.
///
/// ```compile_fail
/// # use rumoca_ir_solve::{SolveMethod, SolveMethodTable};
/// fn root_step(table: &SolveMethodTable) -> &SolveMethod {
///     table.do_step()
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveMethodTable {
    arithmetic: SolveArithmeticProfile,
    signals: SolveSignalSet,
    calls: SolvePureCallTable,
    methods: Box<[SolveMethod]>,
}

impl SolveMethodTable {
    pub fn construct(
        arithmetic: SolveArithmeticProfile,
        signals: SolveSignalSet,
        calls: SolvePureCallTable,
        build: impl FnOnce(&mut SolveMethodTableBuilder) -> Result<(), SolveActionConstructionError>,
    ) -> Result<Self, SolveActionConstructionError> {
        let mut builder = Self::builder(arithmetic, signals, calls)?;
        build(&mut builder)?;
        Ok(builder.finish())
    }

    pub fn builder(
        arithmetic: SolveArithmeticProfile,
        signals: SolveSignalSet,
        calls: SolvePureCallTable,
    ) -> Result<SolveMethodTableBuilder, SolveActionConstructionError> {
        if calls.arithmetic() != arithmetic {
            return Err(SolveActionConstructionError::ProfileMismatch {
                provenance: Span::DUMMY,
            });
        }
        let mut call_signals = Vec::with_capacity(calls.owners().len());
        for owner in calls.owners() {
            if escape::body_has_unnamed_signal(owner.body()) {
                return Err(SolveActionConstructionError::UnnamedSignalEffect {
                    provenance: owner.provenance(),
                });
            }
            call_signals.push(escape::body_signals(owner.body(), &call_signals));
        }
        Ok(SolveMethodTableBuilder {
            arithmetic,
            signals,
            calls,
            call_signals,
            methods: Vec::new(),
        })
    }

    #[must_use]
    pub const fn arithmetic(&self) -> SolveArithmeticProfile {
        self.arithmetic
    }

    /// Every error signal the enclosing controller declares.
    #[must_use]
    pub const fn signals(&self) -> SolveSignalSet {
        self.signals
    }

    #[must_use]
    pub const fn calls(&self) -> &SolvePureCallTable {
        &self.calls
    }

    #[must_use]
    pub const fn methods(&self) -> &[SolveMethod] {
        &self.methods
    }

    #[must_use]
    pub fn method(&self, id: SolveMethodId) -> Option<&SolveMethod> {
        self.methods
            .get(id.index() as usize)
            .filter(|method| method.id == id)
    }
}

pub struct SolveMethodTableBuilder {
    arithmetic: SolveArithmeticProfile,
    signals: SolveSignalSet,
    calls: SolvePureCallTable,
    call_signals: Vec<SolveSignalSet>,
    methods: Vec<SolveMethod>,
}

impl SolveMethodTableBuilder {
    /// Seals one reusable method vocabulary; it claims no lifecycle root.
    #[must_use]
    pub fn finish(self) -> SolveMethodTable {
        SolveMethodTable {
            arithmetic: self.arithmetic,
            signals: self.signals,
            calls: self.calls,
            methods: self.methods.into_boxed_slice(),
        }
    }

    pub fn add_method(
        &mut self,
        kind: SolveMethodKind,
        interface: SolveMethodInterface,
        provenance: Span,
        build: impl for<'method> FnOnce(
            &mut SolveMethodBuilder<'method>,
            &SolveMethodCells<'method>,
        ) -> Result<(), SolveActionConstructionError>,
    ) -> Result<SolveMethodId, SolveActionConstructionError> {
        self.require_interface(&interface, provenance)?;
        let index = u32::try_from(self.methods.len())
            .map_err(|_| SolveActionConstructionError::IdentityOverflow { provenance })?;
        let id = SolveMethodId::from_index(index);
        let method =
            builder::construct_method(id, kind, interface, provenance, self.context(), build)?;
        self.methods.push(method);
        Ok(id)
    }

    fn context(&self) -> SolveMethodContext {
        SolveMethodContext {
            arithmetic: self.arithmetic,
            signals: self.signals,
            calls: self
                .calls
                .owners()
                .iter()
                .map(SolvePureCallOwner::interface)
                .collect(),
            call_signals: self.call_signals.clone(),
            methods: self.methods.iter().map(SolveMethod::signature).collect(),
        }
    }

    fn require_interface(
        &self,
        interface: &SolveMethodInterface,
        provenance: Span,
    ) -> Result<(), SolveActionConstructionError> {
        require_provenance(provenance)?;
        let types_belong = interface
            .parameters
            .iter()
            .chain(interface.results.iter())
            .chain(
                interface
                    .bindings
                    .iter()
                    .map(SolveMethodBinding::value_type),
            )
            .all(|value_type| value_type.belongs_to(self.arithmetic));
        if !types_belong {
            return Err(SolveActionConstructionError::ProfileMismatch { provenance });
        }
        if !self.signals.contains_all(interface.escapes) {
            return Err(SolveActionConstructionError::UndeclaredSignal { provenance });
        }
        for binding in &interface.bindings {
            require_binding(binding, provenance)?;
        }
        Ok(())
    }
}

fn require_binding(
    binding: &SolveMethodBinding,
    provenance: Span,
) -> Result<(), SolveActionConstructionError> {
    if binding.storage == SolveStorageClass::MethodLocal {
        return Err(SolveActionConstructionError::LocalStorageEscape { provenance });
    }
    if binding.storage == SolveStorageClass::Constant && binding.access != SolveSlotAccess::ReadOnly
    {
        return Err(SolveActionConstructionError::InvalidInterface { provenance });
    }
    if let Some(range) = &binding.range
        && !range.limits(binding.value_type.element_type())
    {
        return Err(SolveActionConstructionError::InvalidRange { provenance });
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub(in crate::typed_program) struct SolveMethodContext {
    pub(in crate::typed_program) arithmetic: SolveArithmeticProfile,
    pub(in crate::typed_program) signals: SolveSignalSet,
    pub(in crate::typed_program) calls: Vec<SolvePureCallInterface>,
    /// The signals each issued pure-call owner raises, in owner order.
    pub(in crate::typed_program) call_signals: Vec<SolveSignalSet>,
    pub(in crate::typed_program) methods: Vec<SolveMethodSignature>,
}

pub(in crate::typed_program) fn require_provenance(
    provenance: Span,
) -> Result<(), SolveActionConstructionError> {
    if provenance.is_dummy() {
        return Err(SolveActionConstructionError::MissingProvenance);
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveActionConstructionError {
    Program(SolveProgramConstructionError),
    MissingProvenance,
    WireMismatch,
    IdentityOverflow { provenance: Span },
    ProfileMismatch { provenance: Span },
    UnclosedBlock { provenance: Span },
    UnnamedSignalEffect { provenance: Span },
    UnsettableSignalTest { provenance: Span },
    InvalidInterface { provenance: Span },
    InvalidProgramInterface { provenance: Span },
    InvalidRange { provenance: Span },
    UnknownMethod { provenance: Span },
    UnknownCell { provenance: Span },
    UnknownClosure { provenance: Span },
    CellOutOfScope { provenance: Span },
    ClosureOutOfScope { provenance: Span },
    UndefinedRead { provenance: Span },
    ReadOnlyTarget { provenance: Span },
    DuplicateTarget { provenance: Span },
    LocalStorageEscape { provenance: Span },
    DeclarationAfterAction { provenance: Span },
    InvalidCallInterface { provenance: Span },
    StatefulEffectInStatelessMethod { provenance: Span },
    InvalidLoopDomain { provenance: Span },
    EmptyBranch { provenance: Span },
    OrphanedConstruction { provenance: Span },
    InvalidCondition { provenance: Span },
    UndeclaredSignal { provenance: Span },
    EmptySignalEffect { provenance: Span },
    EmptyLimit { provenance: Span },
    UnrangedLimitTarget { provenance: Span },
    EscapeSetMismatch { provenance: Span },
    UndefinedResult { provenance: Span },
}

impl From<SolveProgramConstructionError> for SolveActionConstructionError {
    fn from(error: SolveProgramConstructionError) -> Self {
        Self::Program(error)
    }
}

impl SolveActionConstructionError {
    #[must_use]
    pub const fn provenance(&self) -> Span {
        match self {
            Self::Program(error) => error.provenance(),
            Self::MissingProvenance | Self::WireMismatch => Span::DUMMY,
            Self::IdentityOverflow { provenance }
            | Self::ProfileMismatch { provenance }
            | Self::UnclosedBlock { provenance }
            | Self::OrphanedConstruction { provenance }
            | Self::UnnamedSignalEffect { provenance }
            | Self::UnsettableSignalTest { provenance }
            | Self::InvalidInterface { provenance }
            | Self::InvalidProgramInterface { provenance }
            | Self::InvalidRange { provenance }
            | Self::UnknownMethod { provenance }
            | Self::UnknownCell { provenance }
            | Self::UnknownClosure { provenance }
            | Self::CellOutOfScope { provenance }
            | Self::ClosureOutOfScope { provenance }
            | Self::UndefinedRead { provenance }
            | Self::ReadOnlyTarget { provenance }
            | Self::DuplicateTarget { provenance }
            | Self::LocalStorageEscape { provenance }
            | Self::DeclarationAfterAction { provenance }
            | Self::InvalidCallInterface { provenance }
            | Self::StatefulEffectInStatelessMethod { provenance }
            | Self::InvalidLoopDomain { provenance }
            | Self::EmptyBranch { provenance }
            | Self::InvalidCondition { provenance }
            | Self::UndeclaredSignal { provenance }
            | Self::EmptySignalEffect { provenance }
            | Self::EmptyLimit { provenance }
            | Self::UnrangedLimitTarget { provenance }
            | Self::EscapeSetMismatch { provenance }
            | Self::UndefinedResult { provenance } => *provenance,
        }
    }
}

impl std::fmt::Display for SolveActionConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Self::Program(error) = self {
            return error.fmt(formatter);
        }
        let message = match self {
            Self::Program(_) => unreachable!("delegated above"),
            Self::MissingProvenance => "method owner is missing exact provenance",
            Self::WireMismatch => "method wire does not replay through checked construction",
            Self::IdentityOverflow { .. } => "method identity capacity exceeded",
            Self::ProfileMismatch { .. } => "method value type does not belong to the profile",
            Self::OrphanedConstruction { .. } => {
                "method construction issued scopes or closures the body does not reach"
            }
            Self::UnclosedBlock { .. } => {
                "a nested lexical block was abandoned by a swallowed construction error"
            }
            Self::UnnamedSignalEffect { .. } => {
                "operation signals an error whose signal row Solve cannot yet name"
            }
            Self::UnsettableSignalTest { .. } => {
                "signal check tests a signal that cannot be active here"
            }
            Self::InvalidInterface { .. } => "method interface declaration is invalid",
            Self::InvalidProgramInterface { .. } => {
                "value program interface does not match its cells"
            }
            Self::InvalidRange { .. } => "saturation range does not match its entity type",
            Self::UnknownMethod { .. } => "callee was not issued before this method",
            Self::UnknownCell { .. } => "cell is not owned by this method",
            Self::UnknownClosure { .. } => "signal closure is not owned by this method",
            Self::CellOutOfScope { .. } => "cell is not visible from the current lexical scope",
            Self::ClosureOutOfScope { .. } => {
                "signal closure is not visible from the current lexical scope"
            }
            Self::UndefinedRead { .. } => "read is not dominated by a prior definition",
            Self::ReadOnlyTarget { .. } => "action writes read-only storage",
            Self::DuplicateTarget { .. } => "action writes the same cell twice",
            Self::LocalStorageEscape { .. } => "lexical local cannot own block storage",
            Self::DeclarationAfterAction { .. } => {
                "lexical locals are declared before the actions of their block"
            }
            Self::InvalidCallInterface { .. } => "call arguments or results do not match the ABI",
            Self::StatefulEffectInStatelessMethod { .. } => {
                "stateless method writes state or calls a stateful method"
            }
            Self::InvalidLoopDomain { .. } => "loop domain is not finite, bounded, and non-empty",
            Self::EmptyBranch { .. } => "branch has no action in either arm",
            Self::InvalidCondition { .. } => "branch condition is not one checked Boolean test",
            Self::UndeclaredSignal { .. } => "error signal is not declared by this controller",
            Self::EmptySignalEffect { .. } => "signal effect raises nothing",
            Self::EmptyLimit { .. } => "limit effect saturates nothing",
            Self::UnrangedLimitTarget { .. } => "limit target has no declared saturation range",
            Self::EscapeSetMismatch { .. } => {
                "declared escape set differs from the constructed escape set"
            }
            Self::UndefinedResult { .. } => "method result is not defined on every path",
        };
        formatter.write_str(message)
    }
}

impl std::error::Error for SolveActionConstructionError {}

#[cfg(test)]
mod tests;
