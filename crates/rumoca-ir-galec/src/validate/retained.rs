//! Private construction-issued subject locators and retained validation facts.
//!
//! This module is intentionally not an Algorithm Code identity API. It closes
//! the internal prerequisite for SPEC_0034 GAL-041: semantic analyses write
//! facts against stable typed locators rather than expression addresses, and
//! root close refuses missing or duplicate fact installation. The temporary
//! address lookup exists only while one immutable input tree is borrowed and
//! is discarded before [`RetainedValidation`] enters a checked carrier.

#![deny(unused_must_use)]

mod builder;
mod capabilities;
mod shapes;
#[cfg(test)]
mod tests;
mod topology;

use rumoca_core::Span;
use rustc_hash::FxHashMap;
use std::num::NonZeroUsize;

use crate::ast::{
    Block, BlockMethod, BlockMethodKind, Condition, Dimension, Direction, Expression, ForLoop,
    FunctionCall, FunctionKind, IfStatement, LimitTarget, Name, Reference, Spanned, Statement,
    UserFunction, VariableDeclaration,
};
use crate::signal_effect::{StatusEffect, expression_effect, statement_effect};

use super::context::{Callee, Resolved, ResolvedCall, ResolvedRef, Ty};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct DeclarationLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MethodLoc(u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FunctionLoc(u32);

impl FunctionLoc {
    pub(super) const fn index(self) -> usize {
        self.0 as usize
    }

    pub(super) const fn ordinal(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct BinderLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StatementLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RealMatrixMultiplyOccurrenceLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExpressionLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ReferenceLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct CallLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct CallResultProjectionLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct BuiltinResultLoc(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SubjectLoc {
    Declaration(DeclarationLoc),
    Method(MethodLoc),
    Function(FunctionLoc),
    Binder(BinderLoc),
    Statement(StatementLoc),
    Expression(ExpressionLoc),
    Reference(ReferenceLoc),
    Call(CallLoc),
    CallResultProjection(CallResultProjectionLoc),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MethodOwner {
    Startup,
    Recalibrate,
    DoStep,
}

/// Declaration ownership fixed by the sole topology traversal.
///
/// This is deliberately more precise than declaration syntax.  In particular,
/// a later refinement never has to recover storage or mutability from a name,
/// a declaration ordinal, or its parent edge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeclarationClass {
    Input,
    Output,
    TunableParameter,
    DependentParameter,
    Constant,
    PersistentState,
    CompartmentDependentParameter,
    CompartmentConstant,
    CompartmentPersistentState,
    MethodLocal,
    FunctionInput,
    FunctionOutput,
    FunctionLocal,
}

impl DeclarationClass {
    const fn requires_start(self) -> bool {
        matches!(
            self,
            Self::Input
                | Self::Output
                | Self::TunableParameter
                | Self::DependentParameter
                | Self::Constant
                | Self::PersistentState
                | Self::CompartmentDependentParameter
                | Self::CompartmentConstant
                | Self::CompartmentPersistentState
        )
    }
}

impl MethodOwner {
    const fn loc(self) -> MethodLoc {
        MethodLoc(match self {
            Self::Startup => 0,
            Self::Recalibrate => 1,
            Self::DoStep => 2,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ChildRole {
    BlockDeclaration(u32),
    BlockMethod(MethodOwner),
    BlockFunction(u32),
    CompartmentMember(u32),
    DeclarationDimension(u32),
    DeclarationMinimum,
    DeclarationMaximum,
    DeclarationStart,
    MethodLocal(u32),
    MethodAction(u32),
    FunctionParameter { direction: Direction, index: u32 },
    FunctionLocal(u32),
    FunctionAction(u32),
    AssignmentTarget,
    AssignmentValue,
    MultiAssignmentTarget(u32),
    MultiAssignmentCall,
    CallStatement,
    IfCondition(u32),
    IfAction { branch: u32, index: u32 },
    ElseAction(u32),
    LoopBinder,
    LoopStart,
    LoopStep,
    LoopStop,
    LoopAction(u32),
    LimitTarget(u32),
    SignalFallback(u32),
    ReferenceSubscript { part: u32, index: u32 },
    CallArgument(u32),
    CallResult(u32),
    Parenthesized,
    NotOperand,
    NegatedReference,
    SizeArray,
    SizeDimension,
    IfExpressionCondition(u32),
    IfExpressionValue(u32),
    IfExpressionElse,
    ArrayElement(u32),
    BinaryLeft,
    BinaryRight,
    ExpressionReference,
    ExpressionCall,
    AggregateProjectionSource,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SubjectParent {
    Block(ChildRole),
    Subject { owner: SubjectLoc, role: ChildRole },
}

impl SubjectParent {
    const fn child(owner: SubjectLoc, role: ChildRole) -> Self {
        Self::Subject { owner, role }
    }
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MissingProvenance {
    FaultInjected,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum GeneratedOrigin {
    Declaration,
    LifecycleMethod,
    UserFunction,
    OrderedStatement,
    Expression,
    Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SubjectProvenance {
    Exact(Span),
    NearestStatement(Span),
    Generated(GeneratedOrigin),
    #[cfg(test)]
    Missing(MissingProvenance),
}

#[derive(Debug, Clone, Copy)]
struct ProvenanceContext {
    statement: Option<Span>,
}

impl ProvenanceContext {
    const NONE: Self = Self { statement: None };

    const fn for_statement(span: Span) -> Self {
        Self {
            statement: Some(span),
        }
    }

    fn expression(self) -> SubjectProvenance {
        match self.statement.filter(|span| !span.is_dummy()) {
            Some(span) => SubjectProvenance::NearestStatement(span),
            None => SubjectProvenance::Generated(GeneratedOrigin::Expression),
        }
    }

    fn name(self, name: &Name) -> SubjectProvenance {
        exact_or(name.span(), || {
            self.statement.filter(|span| !span.is_dummy()).map_or(
                SubjectProvenance::Generated(GeneratedOrigin::Name),
                SubjectProvenance::NearestStatement,
            )
        })
    }
}

fn exact_or(span: Span, fallback: impl FnOnce() -> SubjectProvenance) -> SubjectProvenance {
    if span.is_dummy() {
        fallback()
    } else {
        SubjectProvenance::Exact(span)
    }
}

/// Fault-injection marker for a fact a constructor deliberately refused.
/// Production installation has no such path; every subject starts pending and
/// must be filled before close.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CoverageGap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RequiredFact<T> {
    Pending,
    Checked(T),
    NotChecked(CoverageGap),
}

impl<T> RequiredFact<T> {
    fn checked(&self) -> Option<&T> {
        match self {
            Self::Checked(value) => Some(value),
            Self::Pending | Self::NotChecked(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
struct DeclarationSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    class: DeclarationClass,
    primitive: bool,
    fixed_shape: Option<FixedValueShape>,
    start: RequiredFact<DeclarationStartFact>,
    children: Vec<(SubjectLoc, ChildRole)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DeclarationStartFact {
    NotApplicable,
    /// The source is checked Algorithm Code syntax (SPEC_0034 GAL-014), whose
    /// declaration grammar has no manifest-bound start field. Generated
    /// package construction uses the private GAL-020 contract and can never
    /// mint this disposition.
    NotRepresentedInSyntax,
    Exact {
        expression: ExpressionLoc,
        fixed_shape: FixedValueShape,
    },
}

#[derive(Debug, Clone)]
struct MethodSubject {
    provenance: SubjectProvenance,
    children: Vec<(SubjectLoc, ChildRole)>,
}

#[derive(Debug, Clone)]
struct FunctionSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    kind: FunctionKind,
    results: Vec<DeclarationLoc>,
    children: Vec<(SubjectLoc, ChildRole)>,
}

#[derive(Debug, Clone)]
struct BinderSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
}

#[derive(Debug, Clone)]
struct StatementSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    kind: StatementKind,
    effect: RequiredFact<StatusEffect>,
    real_matrix_multiply: Option<RealMatrixMultiplyOccurrenceLoc>,
    children: Vec<(SubjectLoc, ChildRole)>,
}

#[derive(Debug, Clone)]
struct RealMatrixMultiplyOccurrenceSubject {
    #[cfg(test)]
    owner: StatementLoc,
    #[cfg(test)]
    provenance: SubjectProvenance,
    #[cfg(test)]
    target: DeclarationLoc,
    #[cfg(test)]
    source: DeclarationLoc,
    #[cfg(test)]
    contract: crate::ast::RealMatrixMultiplyOccurrenceContract,
}

/// Closed syntax family retained when the sole topology traversal installs a
/// statement subject.  This is the minimum operation fact an executable
/// refinement needs; it is not a second statement tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StatementKind {
    Assignment,
    MultiAssignment,
    Call,
    If,
    For,
    Limit,
    Signal,
}

#[derive(Debug, Clone)]
struct ExpressionSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    kind: ExpressionKind,
    effect: RequiredFact<StatusEffect>,
    ty: RequiredFact<Ty>,
    fixed_shape: RequiredFact<Option<FixedValueShape>>,
    evaluated_literal: RequiredFact<EvaluatedLiteral>,
    children: Vec<(SubjectLoc, ChildRole)>,
}

/// Closed expression operation retained beside the already-checked type,
/// shape, effect, provenance, and child edges. Real literals retain their bits
/// so equality and hashing never reinterpret NaNs or signed zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExpressionKind {
    Boolean(bool),
    Integer(i64),
    Real(u64),
    Reference,
    Size,
    Call,
    Parenthesized,
    If,
    Array,
    NegatedReference,
    Not,
    Binary(crate::ast::BinaryOp),
}

/// Construction-issued evaluation disposition for the literal subset used by
/// executable declaration initialization. Aggregate facts retain one scalar
/// payload regardless of extent; they never create coordinate subjects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EvaluatedLiteral {
    Scalar(EvaluatedScalar),
    UniformTensorFill(EvaluatedScalar),
    NonUniformTensor,
    Symbolic,
}

/// Exact scalar payload of one evaluated literal. Real values retain their
/// IEEE-754 bits so signed zero remains part of the construction proof.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EvaluatedScalar {
    RealBits(u64),
    Integer(i64),
    Boolean(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BlockDeclarationStartLiteral {
    NotApplicable,
    Exact(EvaluatedLiteral),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResolvedTarget {
    Declaration(DeclarationLoc),
    Binder(BinderLoc),
}

#[derive(Debug, Clone)]
struct ReferenceResolution {
    target: ResolvedTarget,
}

#[derive(Debug, Clone)]
struct ReferenceSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    resolution: RequiredFact<ReferenceResolution>,
    fixed_shape: RequiredFact<Option<FixedValueShape>>,
    children: Vec<(SubjectLoc, ChildRole)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct BuiltinLoc(u16);

impl BuiltinLoc {
    pub(super) const fn index(self) -> usize {
        self.0 as usize
    }

    pub(super) const fn ordinal(self) -> u32 {
        self.0 as u32
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CallTarget {
    Function(FunctionLoc),
    Builtin { base: BuiltinLoc, lifted_rank: u8 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct CallResolution {
    pub(super) target: CallTarget,
    inputs: Vec<Ty>,
    outputs: Vec<Ty>,
    results: Vec<CalleeResultLoc>,
    pub(super) stateful: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallResolutionFact {
    Known(CallResolution),
    /// Type analysis visited this exact occurrence and diagnosed EG015.
    /// This differs from `RequiredFact::Pending`, which means the owning
    /// analysis has not run and therefore forbids graph closure.
    Unknown,
}

#[derive(Debug, Clone)]
struct CallSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    use_site: CallUseLoc,
    effect: RequiredFact<StatusEffect>,
    resolution: RequiredFact<CallResolutionFact>,
    result_set: RequiredFact<ClosedCallResults>,
    children: Vec<(SubjectLoc, ChildRole)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FixedValueShape {
    pub(crate) scalar: crate::ast::ScalarType,
    pub(crate) extents: Box<[u32]>,
}

impl FixedValueShape {
    fn scalar(scalar: crate::ast::ScalarType) -> Self {
        Self {
            scalar,
            extents: Box::new([]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallUseLoc {
    ExpressionValue(ExpressionLoc),
    MultiAssignment {
        action: StatementLoc,
        targets: Box<[ReferenceLoc]>,
    },
    DiscardedBy(StatementLoc),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResultReceiverKind {
    Expression,
    MultiAssignment,
    Discarded,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CalleeResultLoc {
    UserOutput(DeclarationLoc),
    BuiltinOutput(BuiltinResultLoc),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BuiltinResultSubject {
    base: BuiltinLoc,
    lifted_rank: u8,
    output: u16,
}

/// Fully prepared but not yet visible call-resolution transaction.
///
/// Keeping the resolution and any builtin result subjects in one private value
/// prevents a caller from pairing a valid resolution with a foreign prepared
/// result bundle at the commit boundary.
#[derive(Debug)]
struct PreparedCallResolution {
    resolution: CallResolution,
    builtin_results: Vec<BuiltinResultSubject>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CallResultReceiverLoc {
    ExpressionValue(ExpressionLoc),
    MultiAssignmentDestination(ReferenceLoc),
    DiscardedBy(StatementLoc),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallResultProjectionSubject {
    parent: SubjectParent,
    provenance: SubjectProvenance,
    call: CallLoc,
    callee_result: CalleeResultLoc,
    receiver: CallResultReceiverLoc,
    value: Option<FixedValueShape>,
    order: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ClosedCallResults {
    count: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CallPathSegment {
    Statement(u32),
    Branch(u32),
    Else,
    Condition,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct UserCallEdge {
    pub(super) callee: FunctionLoc,
    pub(super) path: Box<[CallPathSegment]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct UserCallGraph {
    pub(super) methods: [Vec<UserCallEdge>; 3],
    pub(super) functions: Vec<Vec<UserCallEdge>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutableOwner {
    Method(MethodLoc),
    Function(FunctionLoc),
}

/// Internal retained result. It deliberately has no public projection API;
/// facts with a `NotChecked` state and missing provenance prevent a GAL-041
/// identity surface until the remaining AST/validator boundary is closed.
#[derive(Debug)]
pub(crate) struct RetainedValidation {
    declarations: Vec<DeclarationSubject>,
    methods: Vec<MethodSubject>,
    functions: Vec<FunctionSubject>,
    binders: Vec<BinderSubject>,
    statements: Vec<StatementSubject>,
    expressions: Vec<ExpressionSubject>,
    references: Vec<ReferenceSubject>,
    calls: Vec<CallSubject>,
    builtin_results: Vec<BuiltinResultSubject>,
    call_result_projections: Vec<CallResultProjectionSubject>,
    real_matrix_multiply_occurrences: Vec<RealMatrixMultiplyOccurrenceSubject>,
    root_children: Vec<(SubjectLoc, ChildRole)>,
    user_call_graph: RequiredFact<UserCallGraph>,
    fixed_shapes: FixedShapeClosure,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct UnprovenValueShape {
    pub(crate) subject: &'static str,
    pub(crate) provenance: SubjectProvenance,
}

#[derive(Debug, Clone, Copy)]
enum FixedShapeFamilyState {
    AllProven,
    Failures {
        count: NonZeroUsize,
        first: UnprovenValueShape,
    },
}

#[derive(Debug, Clone, Copy)]
struct FixedShapeClosure {
    call_results: FixedShapeFamilyState,
    declarations: FixedShapeFamilyState,
    expressions: FixedShapeFamilyState,
    references: FixedShapeFamilyState,
}

impl FixedShapeClosure {
    const ALL_PROVEN: Self = Self {
        call_results: FixedShapeFamilyState::AllProven,
        declarations: FixedShapeFamilyState::AllProven,
        expressions: FixedShapeFamilyState::AllProven,
        references: FixedShapeFamilyState::AllProven,
    };

    fn first_failure(self) -> Option<UnprovenValueShape> {
        [
            self.call_results,
            self.declarations,
            self.expressions,
            self.references,
        ]
        .into_iter()
        .find_map(|family| match family {
            FixedShapeFamilyState::AllProven => None,
            FixedShapeFamilyState::Failures { count, first } => {
                let _proven_nonzero_count = count;
                Some(first)
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub(crate) enum RetainedValidationError {
    #[error(
        "Integer literal {value} is outside the selected source representation domain [{minimum}, {maximum}] at {provenance:?}"
    )]
    IntegerLiteralOutOfDomain {
        value: i64,
        minimum: i64,
        maximum: i64,
        provenance: SubjectProvenance,
    },
    #[error("authored checked blocks cannot carry compiler matrix-multiply occurrences")]
    UnexpectedRealMatrixMultiplyOccurrence,
    #[error(
        "generated Real matrix-multiply occurrence failed to close at {provenance:?}: {detail}"
    )]
    InvalidRealMatrixMultiplyOccurrence {
        detail: String,
        provenance: SubjectProvenance,
    },
    #[error("semantic subject was installed more than once")]
    DuplicateSubject,
    #[error("semantic fact was installed more than once for {family} subject {index}")]
    DuplicateFact { family: &'static str, index: u32 },
    #[error("semantic fact is missing for {family} subject {index}")]
    MissingFact { family: &'static str, index: u32 },
    #[error("semantic fact was explicitly left unchecked for {family} subject {index}")]
    UncheckedFact { family: &'static str, index: u32 },
    #[error("resolved subject is absent from the construction-issued index")]
    MissingResolvedSubject,
    #[error("builtin target is absent from the normative catalog")]
    MissingBuiltin,
    #[error("semantic subject count exceeds the private locator domain")]
    LocatorOverflow,
    #[error("allocation failed while preparing retained {family} state")]
    AllocationFailed { family: &'static str },
    #[error("retained {family} fact is inconsistent for subject {index}")]
    InconsistentFact { family: &'static str, index: u32 },
}

#[derive(Debug)]
pub(super) enum ShapeRelationError {
    Mismatch { context: &'static str },
    Index(RetainedValidationError),
}

#[derive(Debug)]
pub(super) enum DeclarationStartRelationError {
    TypeMismatch { expected: Ty, found: Ty },
    ShapeMismatch,
    UnprovenShape,
    Index(RetainedValidationError),
}

impl From<RetainedValidationError> for DeclarationStartRelationError {
    fn from(error: RetainedValidationError) -> Self {
        Self::Index(error)
    }
}

impl From<RetainedValidationError> for ShapeRelationError {
    fn from(error: RetainedValidationError) -> Self {
        Self::Index(error)
    }
}

struct AddressLookup {
    declarations: FxHashMap<*const VariableDeclaration, DeclarationLoc>,
    functions: FxHashMap<*const UserFunction, FunctionLoc>,
    binders: FxHashMap<*const Name, BinderLoc>,
    statements: FxHashMap<*const Statement, StatementLoc>,
    expressions: FxHashMap<*const Expression, ExpressionLoc>,
    references: FxHashMap<*const Reference, ReferenceLoc>,
    calls: FxHashMap<*const FunctionCall, CallLoc>,
}

#[derive(Debug, Default)]
struct ReservationGate {
    #[cfg(test)]
    fail_at: Option<usize>,
    #[cfg(test)]
    visited: usize,
}

impl ReservationGate {
    #[cfg(not(test))]
    fn pass(&mut self, _family: &'static str) -> Result<(), RetainedValidationError> {
        Ok(())
    }

    #[cfg(test)]
    fn pass(&mut self, family: &'static str) -> Result<(), RetainedValidationError> {
        let current = self.visited;
        self.visited += 1;
        if self.fail_at == Some(current) {
            return Err(RetainedValidationError::AllocationFailed { family });
        }
        Ok(())
    }

    #[cfg(test)]
    fn fail_at(&mut self, reservation: usize) {
        self.fail_at = Some(reservation);
        self.visited = 0;
    }
}

impl AddressLookup {
    fn new() -> Self {
        Self {
            declarations: FxHashMap::default(),
            functions: FxHashMap::default(),
            binders: FxHashMap::default(),
            statements: FxHashMap::default(),
            expressions: FxHashMap::default(),
            references: FxHashMap::default(),
            calls: FxHashMap::default(),
        }
    }
}

/// Construction session shared by validators. Only stable typed locators enter
/// retained fact columns; raw addresses are lookup keys and never escape
/// [`finish`](Self::finish).
pub(super) struct RetainedValidationBuilder {
    retained: RetainedValidation,
    lookup: AddressLookup,
    capabilities: capabilities::ConstructionCapabilities,
    fixed_shapes: shapes::FixedShapeCapabilities,
    reservations: ReservationGate,
}

impl RetainedValidation {
    #[cfg(test)]
    pub(crate) fn real_matrix_multiply_occurrence_locators(
        &self,
    ) -> impl ExactSizeIterator<Item = RealMatrixMultiplyOccurrenceLoc> + '_ {
        (0..self.real_matrix_multiply_occurrences.len()).map(|index| {
            RealMatrixMultiplyOccurrenceLoc(
                u32::try_from(index)
                    .expect("closed matrix-multiply occurrence arena fits its locator domain"),
            )
        })
    }

    #[cfg(test)]
    pub(crate) fn real_matrix_multiply_occurrence_facts(
        &self,
        locator: RealMatrixMultiplyOccurrenceLoc,
    ) -> (
        StatementLoc,
        SubjectProvenance,
        DeclarationLoc,
        DeclarationLoc,
        &crate::ast::RealMatrixMultiplyOccurrenceContract,
    ) {
        let subject = &self.real_matrix_multiply_occurrences[locator.0 as usize];
        (
            subject.owner,
            subject.provenance,
            subject.target,
            subject.source,
            &subject.contract,
        )
    }

    pub(crate) fn call_result_projection_locators(
        &self,
    ) -> impl ExactSizeIterator<Item = CallResultProjectionLoc> + '_ {
        (0..self.call_result_projections.len()).map(|index| {
            CallResultProjectionLoc(
                u32::try_from(index)
                    .expect("closed call-result projection arena fits its locator domain"),
            )
        })
    }

    pub(crate) fn projection_call(&self, locator: CallResultProjectionLoc) -> CallLoc {
        self.call_result_projections[locator.0 as usize].call
    }

    pub(crate) fn projection_callee_result(
        &self,
        locator: CallResultProjectionLoc,
    ) -> CalleeResultLoc {
        self.call_result_projections[locator.0 as usize].callee_result
    }

    pub(crate) fn projection_receiver(
        &self,
        locator: CallResultProjectionLoc,
    ) -> CallResultReceiverLoc {
        self.call_result_projections[locator.0 as usize].receiver
    }

    pub(crate) fn projection_shape(&self, locator: CallResultProjectionLoc) -> &FixedValueShape {
        self.call_result_projections[locator.0 as usize]
            .value
            .as_ref()
            .expect("AlgorithmCodePackage construction requires every result shape")
    }

    pub(crate) fn projection_order(&self, locator: CallResultProjectionLoc) -> u32 {
        self.call_result_projections[locator.0 as usize].order
    }

    pub(crate) fn projection_provenance(
        &self,
        locator: CallResultProjectionLoc,
    ) -> SubjectProvenance {
        self.call_result_projections[locator.0 as usize].provenance
    }

    pub(crate) fn call_facts(&self, locator: CallLoc) -> (SubjectProvenance, StatusEffect) {
        let subject = &self.calls[locator.0 as usize];
        (
            subject.provenance,
            *subject
                .effect
                .checked()
                .expect("closed call retains its checked effect"),
        )
    }

    pub(crate) fn declaration_facts(
        &self,
        locator: DeclarationLoc,
    ) -> (
        SubjectProvenance,
        DeclarationClass,
        &FixedValueShape,
        Option<ExpressionLoc>,
    ) {
        let subject = &self.declarations[locator.0 as usize];
        let (value, start) = match subject
            .start
            .checked()
            .expect("closed declaration retains its start disposition")
        {
            DeclarationStartFact::NotApplicable | DeclarationStartFact::NotRepresentedInSyntax => (
                subject
                    .fixed_shape
                    .as_ref()
                    .expect("packaged primitive declaration has one fixed shape"),
                None,
            ),
            DeclarationStartFact::Exact {
                expression,
                fixed_shape,
            } => (fixed_shape, Some(*expression)),
        };
        (subject.provenance, subject.class, value, start)
    }

    pub(crate) fn expression_facts(
        &self,
        locator: ExpressionLoc,
    ) -> (
        SubjectProvenance,
        ExpressionKind,
        StatusEffect,
        &FixedValueShape,
    ) {
        let subject = &self.expressions[locator.0 as usize];
        (
            subject.provenance,
            subject.kind,
            *subject
                .effect
                .checked()
                .expect("closed expression retains its checked effect"),
            subject
                .fixed_shape
                .checked()
                .and_then(Option::as_ref)
                .expect("packaged expression has one fixed shape"),
        )
    }

    pub(crate) fn expression_evaluated_literal(&self, locator: ExpressionLoc) -> EvaluatedLiteral {
        *self.expressions[locator.0 as usize]
            .evaluated_literal
            .checked()
            .expect("packaged expression retains one literal-evaluation disposition")
    }

    pub(crate) fn block_declaration_start_literal(
        &self,
        block_index: u32,
    ) -> (BlockDeclarationStartLiteral, &FixedValueShape) {
        let subject = self
            .declarations
            .iter()
            .find(|subject| {
                subject.parent == SubjectParent::Block(ChildRole::BlockDeclaration(block_index))
            })
            .expect("the closed topology retains every block declaration index exactly once");
        match subject
            .start
            .checked()
            .expect("packaged declaration retains one start disposition")
        {
            DeclarationStartFact::NotApplicable | DeclarationStartFact::NotRepresentedInSyntax => (
                BlockDeclarationStartLiteral::NotApplicable,
                subject
                    .fixed_shape
                    .as_ref()
                    .expect("a block declaration retains its exact fixed shape"),
            ),
            DeclarationStartFact::Exact {
                expression,
                fixed_shape,
            } => (
                BlockDeclarationStartLiteral::Exact(self.expression_evaluated_literal(*expression)),
                fixed_shape,
            ),
        }
    }

    pub(crate) fn reference_facts(
        &self,
        locator: ReferenceLoc,
    ) -> (SubjectProvenance, ResolvedTarget, &FixedValueShape) {
        let subject = &self.references[locator.0 as usize];
        (
            subject.provenance,
            subject
                .resolution
                .checked()
                .expect("closed reference retains its resolved target")
                .target,
            subject
                .fixed_shape
                .checked()
                .and_then(Option::as_ref)
                .expect("packaged reference has one fixed shape"),
        )
    }

    pub(crate) fn statement_facts(
        &self,
        locator: StatementLoc,
    ) -> (
        SubjectProvenance,
        StatementKind,
        StatusEffect,
        Option<RealMatrixMultiplyOccurrenceLoc>,
    ) {
        let subject = &self.statements[locator.0 as usize];
        (
            subject.provenance,
            subject.kind,
            *subject
                .effect
                .checked()
                .expect("closed statement retains its checked effect"),
            subject.real_matrix_multiply,
        )
    }

    pub(crate) fn builtin_result_facts(
        &self,
        locator: BuiltinResultLoc,
    ) -> (
        &'static crate::builtins::Builtin,
        u8,
        &'static crate::builtins::BuiltinParam,
    ) {
        let subject = &self.builtin_results[locator.0 as usize];
        let builtin = &crate::builtins::BUILTINS[subject.base.index()];
        (
            builtin,
            subject.lifted_rank,
            &builtin.outputs[usize::from(subject.output)],
        )
    }

    pub(crate) fn require_fixed_value_shapes(&self) -> Result<(), UnprovenValueShape> {
        self.fixed_shapes.first_failure().map_or(Ok(()), Err)
    }

    pub(crate) fn method_locators(&self) -> impl ExactSizeIterator<Item = MethodLoc> + '_ {
        [MethodLoc(0), MethodLoc(1), MethodLoc(2)]
            .into_iter()
            .take(self.methods.len())
    }

    pub(crate) fn method_provenance(&self, locator: MethodLoc) -> SubjectProvenance {
        self.methods[locator.0 as usize].provenance
    }

    pub(crate) fn method_kind(&self, locator: MethodLoc) -> BlockMethodKind {
        match locator.0 {
            0 => BlockMethodKind::Startup,
            1 => BlockMethodKind::Recalibrate,
            2 => BlockMethodKind::DoStep,
            _ => unreachable!("closed method locator is outside the three-method arena"),
        }
    }

    pub(crate) fn subject_count(&self) -> usize {
        self.declarations.len()
            + self.methods.len()
            + self.functions.len()
            + self.binders.len()
            + self.statements.len()
            + self.expressions.len()
            + self.references.len()
            + self.calls.len()
            + self.builtin_results.len()
            + self.call_result_projections.len()
    }

    pub(crate) fn subject_locators(&self) -> std::vec::IntoIter<SubjectLoc> {
        let mut locators = Vec::with_capacity(
            self.subject_count()
                .checked_sub(self.builtin_results.len())
                .expect("builtin results are included in the subject census"),
        );
        locators.extend((0..self.declarations.len()).map(|index| {
            SubjectLoc::Declaration(DeclarationLoc(
                u32::try_from(index).expect("closed declaration arena fits its locator domain"),
            ))
        }));
        locators.extend(self.method_locators().map(SubjectLoc::Method));
        locators.extend((0..self.functions.len()).map(|index| {
            SubjectLoc::Function(FunctionLoc(
                u32::try_from(index).expect("closed function arena fits its locator domain"),
            ))
        }));
        locators.extend((0..self.binders.len()).map(|index| {
            SubjectLoc::Binder(BinderLoc(
                u32::try_from(index).expect("closed binder arena fits its locator domain"),
            ))
        }));
        locators.extend((0..self.statements.len()).map(|index| {
            SubjectLoc::Statement(StatementLoc(
                u32::try_from(index).expect("closed statement arena fits its locator domain"),
            ))
        }));
        locators.extend((0..self.expressions.len()).map(|index| {
            SubjectLoc::Expression(ExpressionLoc(
                u32::try_from(index).expect("closed expression arena fits its locator domain"),
            ))
        }));
        locators.extend((0..self.references.len()).map(|index| {
            SubjectLoc::Reference(ReferenceLoc(
                u32::try_from(index).expect("closed reference arena fits its locator domain"),
            ))
        }));
        locators.extend((0..self.calls.len()).map(|index| {
            SubjectLoc::Call(CallLoc(
                u32::try_from(index).expect("closed call arena fits its locator domain"),
            ))
        }));
        locators.extend(
            self.call_result_projection_locators()
                .map(SubjectLoc::CallResultProjection),
        );
        locators.into_iter()
    }

    pub(crate) fn root_children(&self) -> &[(SubjectLoc, ChildRole)] {
        &self.root_children
    }

    pub(crate) fn subject_children(&self, subject: SubjectLoc) -> &[(SubjectLoc, ChildRole)] {
        match subject {
            SubjectLoc::Declaration(loc) => &self.declarations[loc.0 as usize].children,
            SubjectLoc::Method(loc) => &self.methods[loc.0 as usize].children,
            SubjectLoc::Function(loc) => &self.functions[loc.0 as usize].children,
            SubjectLoc::Binder(_) | SubjectLoc::CallResultProjection(_) => &[],
            SubjectLoc::Statement(loc) => &self.statements[loc.0 as usize].children,
            SubjectLoc::Expression(loc) => &self.expressions[loc.0 as usize].children,
            SubjectLoc::Reference(loc) => &self.references[loc.0 as usize].children,
            SubjectLoc::Call(loc) => &self.calls[loc.0 as usize].children,
        }
    }

    pub(crate) fn subject_parent(
        &self,
        subject: SubjectLoc,
    ) -> Result<SubjectParent, RetainedValidationError> {
        match subject {
            SubjectLoc::Declaration(loc) => self
                .declarations
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
            SubjectLoc::Method(loc) => method_parent(loc),
            SubjectLoc::Function(loc) => self
                .functions
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
            SubjectLoc::Binder(loc) => self
                .binders
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
            SubjectLoc::Statement(loc) => self
                .statements
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
            SubjectLoc::Expression(loc) => self
                .expressions
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
            SubjectLoc::Reference(loc) => self
                .references
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
            SubjectLoc::Call(loc) => self.calls.get(loc.0 as usize).map(|subject| subject.parent),
            SubjectLoc::CallResultProjection(loc) => self
                .call_result_projections
                .get(loc.0 as usize)
                .map(|subject| subject.parent),
        }
        .ok_or(RetainedValidationError::MissingResolvedSubject)
    }

    pub(crate) fn subject_provenance(
        &self,
        subject: SubjectLoc,
    ) -> Result<SubjectProvenance, RetainedValidationError> {
        match subject {
            SubjectLoc::Declaration(loc) => self
                .declarations
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Method(loc) => self
                .methods
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Function(loc) => self
                .functions
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Binder(loc) => self
                .binders
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Statement(loc) => self
                .statements
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Expression(loc) => self
                .expressions
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Reference(loc) => self
                .references
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::Call(loc) => self
                .calls
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
            SubjectLoc::CallResultProjection(loc) => self
                .call_result_projections
                .get(loc.0 as usize)
                .map(|subject| subject.provenance),
        }
        .ok_or(RetainedValidationError::MissingResolvedSubject)
    }
}

fn method_parent(loc: MethodLoc) -> Option<SubjectParent> {
    let owner = match loc.0 {
        0 => MethodOwner::Startup,
        1 => MethodOwner::Recalibrate,
        2 => MethodOwner::DoStep,
        _ => return None,
    };
    Some(SubjectParent::Block(ChildRole::BlockMethod(owner)))
}

fn fixed_declaration_shape(declaration: &VariableDeclaration) -> Option<FixedValueShape> {
    let crate::ast::TypeRef::Primitive(scalar) = declaration.ty else {
        return None;
    };
    let extents = declaration
        .dimensions
        .iter()
        .map(|dimension| match dimension {
            Dimension::Expr(Expression::Integer(value)) if *value > 0 => {
                u32::try_from(*value).ok().filter(|extent| *extent > 0)
            }
            Dimension::Derived | Dimension::Expr(_) => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(FixedValueShape {
        scalar,
        extents: extents.into_boxed_slice(),
    })
}

fn fixed_reference_shape(resolved: &ResolvedRef<'_>) -> Option<FixedValueShape> {
    if matches!(resolved.target, Resolved::Iterator(_)) {
        return Some(FixedValueShape::scalar(crate::ast::ScalarType::Integer));
    }
    let declaration = match resolved.target {
        Resolved::Entity { decl, .. } | Resolved::Local(decl) => decl,
        Resolved::Parameter(parameter) => &parameter.decl,
        Resolved::Component { .. } | Resolved::Iterator(_) => return None,
    };
    let mut shape = fixed_declaration_shape(declaration)?;
    let final_part = resolved.parts.last()?;
    if !final_part.part.subscripts.is_empty() {
        if final_part.part.subscripts.len() != shape.extents.len() {
            return None;
        }
        shape.extents = Box::new([]);
    }
    Some(shape)
}

fn equal_fixed_shapes(
    shapes: Vec<Option<FixedValueShape>>,
    context: &'static str,
) -> Result<Option<FixedValueShape>, ShapeRelationError> {
    let mut shapes = shapes.into_iter();
    let Some(first) = shapes.next() else {
        return Ok(None);
    };
    let Some(first) = first else {
        return Ok(None);
    };
    for shape in shapes {
        let Some(shape) = shape else {
            return Ok(None);
        };
        if shape != first {
            return Err(ShapeRelationError::Mismatch { context });
        }
    }
    Ok(Some(first))
}

fn fixed_builtin_output_extents(
    name: &str,
    arguments: &[Option<FixedValueShape>],
    _index: usize,
) -> Result<Vec<Option<Box<[u32]>>>, ShapeRelationError> {
    let exact = |position: usize| arguments.get(position).and_then(Option::as_ref);
    match name {
        "solveLinearEquations" => {
            let (Some(matrix), Some(vector)) = (exact(0), exact(1)) else {
                return Ok(vec![None]);
            };
            if matrix.extents.len() != 2
                || vector.extents.len() != 1
                || matrix.extents[0] != matrix.extents[1]
                || matrix.extents[0] != vector.extents[0]
            {
                return Err(ShapeRelationError::Mismatch {
                    context: "solveLinearEquations fixed dimensions",
                });
            }
            Ok(vec![Some(vector.extents.clone())])
        }
        "luFactorize" => {
            let Some(matrix) = exact(0) else {
                return Ok(vec![None, None]);
            };
            if matrix.extents.len() != 2 || matrix.extents[0] != matrix.extents[1] {
                return Err(ShapeRelationError::Mismatch {
                    context: "luFactorize fixed dimensions",
                });
            }
            Ok(vec![
                Some(matrix.extents.clone()),
                Some(vec![matrix.extents[0]].into_boxed_slice()),
            ])
        }
        "luSolve" => {
            let (Some(matrix), Some(pivots), Some(vector)) = (exact(0), exact(1), exact(2)) else {
                return Ok(vec![None]);
            };
            if matrix.extents.len() != 2
                || pivots.extents.len() != 1
                || vector.extents.len() != 1
                || matrix.extents[0] != matrix.extents[1]
                || matrix.extents[0] != pivots.extents[0]
                || matrix.extents[0] != vector.extents[0]
            {
                return Err(ShapeRelationError::Mismatch {
                    context: "luSolve fixed dimensions",
                });
            }
            Ok(vec![Some(vector.extents.clone())])
        }
        _ => Ok(Vec::new()),
    }
}

fn require_provenance(provenance: SubjectProvenance) -> Result<(), RetainedValidationError> {
    match provenance {
        SubjectProvenance::Exact(span) | SubjectProvenance::NearestStatement(span) => {
            if span.is_dummy() {
                return inconsistent("provenance", 0);
            }
        }
        SubjectProvenance::Generated(origin) => match origin {
            GeneratedOrigin::Declaration
            | GeneratedOrigin::LifecycleMethod
            | GeneratedOrigin::UserFunction
            | GeneratedOrigin::OrderedStatement
            | GeneratedOrigin::Expression
            | GeneratedOrigin::Name => {}
        },
        #[cfg(test)]
        SubjectProvenance::Missing(MissingProvenance::FaultInjected) => {
            return inconsistent("provenance", 0);
        }
    }
    Ok(())
}

fn inconsistent<T>(family: &'static str, index: usize) -> Result<T, RetainedValidationError> {
    Err(RetainedValidationError::InconsistentFact {
        family,
        index: to_u32(index)?,
    })
}

fn inconsistent_shape<T>(family: &'static str, index: usize) -> Result<T, ShapeRelationError> {
    inconsistent(family, index).map_err(ShapeRelationError::Index)
}

#[cfg(test)]
fn install_fact<T>(
    slot: &mut RequiredFact<T>,
    value: T,
    family: &'static str,
    index: u32,
) -> Result<(), RetainedValidationError> {
    match slot {
        RequiredFact::Pending => {
            *slot = RequiredFact::Checked(value);
            Ok(())
        }
        RequiredFact::Checked(_) | RequiredFact::NotChecked(_) => {
            Err(RetainedValidationError::DuplicateFact { family, index })
        }
    }
}

fn user_call_signature(
    function: &UserFunction,
) -> Result<(Vec<Ty>, Vec<Ty>), RetainedValidationError> {
    let input_count = function
        .parameters
        .iter()
        .filter(|parameter| parameter.direction == Direction::Input)
        .count();
    let output_count = function.parameters.len() - input_count;
    let mut inputs = Vec::new();
    inputs
        .try_reserve(input_count)
        .map_err(|_| RetainedValidationError::AllocationFailed {
            family: "call-input-signature",
        })?;
    let mut outputs = Vec::new();
    outputs
        .try_reserve(output_count)
        .map_err(|_| RetainedValidationError::AllocationFailed {
            family: "call-output-signature",
        })?;
    for parameter in &function.parameters {
        let destination = match parameter.direction {
            Direction::Input => &mut inputs,
            Direction::Output => &mut outputs,
        };
        destination.push(Ty::of_decl(&parameter.decl));
    }
    Ok((inputs, outputs))
}

fn builtin_call_signature(
    builtin: &crate::builtins::Builtin,
    lifted_rank: u8,
) -> Result<(Vec<Ty>, Vec<Ty>), RetainedValidationError> {
    let mut inputs = Vec::new();
    inputs.try_reserve(builtin.inputs.len()).map_err(|_| {
        RetainedValidationError::AllocationFailed {
            family: "builtin-input-signature",
        }
    })?;
    inputs.extend(
        builtin
            .inputs
            .iter()
            .map(|parameter| builtin_parameter_type(parameter.ty, lifted_rank)),
    );
    let mut outputs = Vec::new();
    outputs.try_reserve(builtin.outputs.len()).map_err(|_| {
        RetainedValidationError::AllocationFailed {
            family: "builtin-output-signature",
        }
    })?;
    outputs.extend(
        builtin
            .outputs
            .iter()
            .map(|parameter| builtin_parameter_type(parameter.ty, lifted_rank)),
    );
    Ok((inputs, outputs))
}

fn builtin_parameter_type(ty: crate::builtins::BuiltinType, lifted_rank: u8) -> Ty {
    let ty = Ty::of_builtin(ty);
    if lifted_rank == 0 {
        return ty;
    }
    match ty {
        Ty::Scalar(scalar) => Ty::Array(scalar, usize::from(lifted_rank)),
        other => other,
    }
}

fn builtin_loc(
    builtin: &'static crate::builtins::Builtin,
) -> Result<BuiltinLoc, RetainedValidationError> {
    let index = crate::builtins::BUILTINS
        .iter()
        // `BUILTINS` is a `const` slice, so pointer equality is not stable
        // across promoted uses. Match the complete normative entry: a
        // same-spelled value with a different signature or signal set is not
        // the catalog target.
        .position(|candidate| candidate == builtin)
        .ok_or(RetainedValidationError::MissingBuiltin)?;
    Ok(BuiltinLoc(
        u16::try_from(index).map_err(|_| RetainedValidationError::LocatorOverflow)?,
    ))
}

fn to_u32(value: usize) -> Result<u32, RetainedValidationError> {
    u32::try_from(value).map_err(|_| RetainedValidationError::LocatorOverflow)
}
