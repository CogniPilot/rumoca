//! Ordered, bounded method actions.
//!
//! Actions are the only control-flow vocabulary of a checked method. Their
//! stored order is their evaluation order; nothing reassociates or reorders
//! them, and every structured action owns its own lexical scope.

use rumoca_core::{Span, StructuredIndexDomain};
use serde::{Serialize, Serializer};

use super::super::effect::{SolveSignalSet, SolveValueRange};
use super::super::program::SolveProgramRegion;
use super::super::types::SolveValueType;
use super::{
    SolveActionConstructionError, SolveCellId, SolveMethodId, SolveScopeId, SolveSignalClosureId,
};

/// One typed expression program evaluated over exact readable cells.
///
/// The region interface is the complete capture ABI: its inputs are the read
/// cells in order and its outputs are the values the enclosing action consumes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveValueProgram {
    reads: Box<[SolveCellId]>,
    region: SolveProgramRegion,
}

impl SolveValueProgram {
    pub(super) fn issued(reads: Vec<SolveCellId>, region: SolveProgramRegion) -> Self {
        Self {
            reads: reads.into_boxed_slice(),
            region,
        }
    }

    #[must_use]
    pub const fn reads(&self) -> &[SolveCellId] {
        &self.reads
    }

    #[must_use]
    pub const fn region(&self) -> &SolveProgramRegion {
        &self.region
    }
}

/// The listed restriction of one signal check.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveSignalTest {
    negated: bool,
    signals: SolveSignalSet,
}

impl SolveSignalTest {
    /// Constructs `in s1, …` or `not in s1, …`; the listed set is never empty.
    pub fn construct(
        negated: bool,
        signals: SolveSignalSet,
        provenance: Span,
    ) -> Result<Self, SolveActionConstructionError> {
        super::require_provenance(provenance)?;
        if signals.is_empty() {
            return Err(SolveActionConstructionError::InvalidCondition { provenance });
        }
        Ok(Self { negated, signals })
    }

    #[must_use]
    pub const fn negated(self) -> bool {
        self.negated
    }

    #[must_use]
    pub const fn signals(self) -> SolveSignalSet {
        self.signals
    }

    /// The signals this test catches out of one declared universe.
    #[must_use]
    pub fn caught(self, universe: SolveSignalSet) -> SolveSignalSet {
        if self.negated {
            universe.difference(self.signals)
        } else {
            self.signals
        }
    }
}

/// One catching error-signal check.
///
/// Checking is catching: a satisfied check removes its caught set from the
/// active status, and an optional closure captures exactly that set.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveSignalCheck {
    test: Option<SolveSignalTest>,
    /// Derived from `test` and the declared universe; never a wire input.
    #[serde(skip)]
    caught: SolveSignalSet,
    /// Issued by the branch that caught; the wire input is only the request.
    #[serde(rename = "capture_closure", serialize_with = "serialize_capture")]
    closure: Option<SolveSignalClosureId>,
    fallback: Option<SolveValueProgram>,
}

fn serialize_capture<S>(
    closure: &Option<SolveSignalClosureId>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_bool(closure.is_some())
}

impl SolveSignalCheck {
    pub(super) const fn issued(
        test: Option<SolveSignalTest>,
        caught: SolveSignalSet,
        closure: Option<SolveSignalClosureId>,
        fallback: Option<SolveValueProgram>,
    ) -> Self {
        Self {
            test,
            caught,
            closure,
            fallback,
        }
    }

    #[must_use]
    pub const fn test(&self) -> Option<SolveSignalTest> {
        self.test
    }

    #[must_use]
    pub const fn caught(&self) -> SolveSignalSet {
        self.caught
    }

    #[must_use]
    pub const fn closure(&self) -> Option<SolveSignalClosureId> {
        self.closure
    }

    #[must_use]
    pub const fn fallback(&self) -> Option<&SolveValueProgram> {
        self.fallback.as_ref()
    }
}

/// The complete branch condition vocabulary.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "condition", content = "check", rename_all = "snake_case")]
pub enum SolveBranchCondition {
    /// One typed Boolean scalar produced by a checked expression program.
    Value(SolveValueProgram),
    /// One catching error-signal check.
    Signal(SolveSignalCheck),
}

/// The builder-facing branch condition.
///
/// A closure is requested here and issued by the branch itself, so a signal
/// closure can only ever live in the lexical scope that caught it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveBranchConditionSpec {
    Value(SolveValueProgram),
    Signal {
        test: Option<SolveSignalTest>,
        capture_closure: bool,
        fallback: Option<SolveValueProgram>,
    },
}

/// One saturation target of an explicit limit effect.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "target", content = "cell", rename_all = "snake_case")]
pub enum SolveLimitTarget {
    /// `limit self` — every visible ranged persistent-state cell.
    RangedState,
    /// One explicitly named ranged cell.
    Cell(SolveCellId),
}

/// The complete method action vocabulary.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum SolveAction {
    /// Evaluate one typed program and commit its results to exact cells.
    Assign {
        program: SolveValueProgram,
        targets: Box<[SolveCellId]>,
    },
    /// One complete two-armed branch; both arms own their lexical scope.
    Branch {
        condition: SolveBranchCondition,
        if_true: SolveActionBlock,
        if_false: SolveActionBlock,
    },
    /// One statically bounded loop over a checked finite, non-empty domain.
    Loop {
        domain: StructuredIndexDomain,
        /// Issued by the loop, one per domain binder; never a wire input.
        #[serde(skip)]
        binders: Box<[SolveCellId]>,
        body: SolveActionBlock,
    },
    /// One checked call of a previously issued method.
    Invoke {
        method: SolveMethodId,
        arguments: Box<[SolveCellId]>,
        results: Box<[SolveCellId]>,
    },
    /// One explicit saturation effect.
    Limit { targets: Box<[SolveLimitTarget]> },
    /// One explicit error-signal set or closure re-raise.
    Signal {
        signals: SolveSignalSet,
        closures: Box<[SolveSignalClosureId]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveSpannedAction {
    action: SolveAction,
    provenance: Span,
}

impl SolveSpannedAction {
    pub(super) const fn issued(action: SolveAction, provenance: Span) -> Self {
        Self { action, provenance }
    }

    #[must_use]
    pub const fn action(&self) -> &SolveAction {
        &self.action
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

/// One lexical local declared at the head of one block.
///
/// The declaration is the constructor input; the cell it was issued is derived.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveLocalDeclaration {
    #[serde(skip)]
    cell: SolveCellId,
    value_type: SolveValueType,
    range: Option<SolveValueRange>,
    provenance: Span,
}

impl SolveLocalDeclaration {
    pub(super) fn issued(
        cell: SolveCellId,
        value_type: SolveValueType,
        range: Option<SolveValueRange>,
        provenance: Span,
    ) -> Self {
        Self {
            cell,
            value_type,
            range,
            provenance,
        }
    }

    #[must_use]
    pub const fn cell(&self) -> SolveCellId {
        self.cell
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub fn range(&self) -> Option<&SolveValueRange> {
        self.range.as_ref()
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }
}

/// One lexical block: the locals it declares and the actions it runs in order.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveActionBlock {
    /// Issued when the block was opened; never a wire input.
    #[serde(skip)]
    scope: SolveScopeId,
    locals: Box<[SolveLocalDeclaration]>,
    actions: Box<[SolveSpannedAction]>,
    /// The provenance of the owner that opened the block; never a wire input.
    #[serde(skip)]
    provenance: Span,
}

impl SolveActionBlock {
    pub(super) fn issued(
        scope: SolveScopeId,
        locals: Vec<SolveLocalDeclaration>,
        actions: Vec<SolveSpannedAction>,
        provenance: Span,
    ) -> Self {
        Self {
            scope,
            locals: locals.into_boxed_slice(),
            actions: actions.into_boxed_slice(),
            provenance,
        }
    }

    #[must_use]
    pub const fn scope(&self) -> SolveScopeId {
        self.scope
    }

    #[must_use]
    pub const fn locals(&self) -> &[SolveLocalDeclaration] {
        &self.locals
    }

    #[must_use]
    pub const fn actions(&self) -> &[SolveSpannedAction] {
        &self.actions
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.actions.is_empty()
    }
}
