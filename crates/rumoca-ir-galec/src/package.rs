//! Opaque checked Algorithm Code data.
//!
//! Raw syntax trees are accepted only at [`CheckedAlgorithmBlock::construct`],
//! which closes the whole-root language invariants. The projection package
//! adds only target-neutral semantic correlations that are not recoverable
//! from the block itself. Artifact identities, filenames, checksums, XML
//! hierarchy, target types, and representation structure belong to target
//! templates and the generic artifact graph.

mod errors;
pub(crate) mod matrix_multiply;
mod metadata;
#[cfg(test)]
mod tests;
mod variable_catalog;

use errors::validation_error;
use metadata::{validate_constant_folds, validate_variable_nominals};
use variable_catalog::{block_declarations, block_variable_starts, clock_ordinal};

pub use errors::{BlockDiagnostics, PackageError};

use crate::ast::ScalarType;
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;
use std::sync::Arc;

/// Opaque proof that one Algorithm Code block passed whole-root validation.
///
/// A checked root cannot be cloned without replaying construction, and its AST
/// has no mutable projection:
///
/// ```compile_fail
/// use rumoca_ir_galec::package::CheckedAlgorithmBlock;
///
/// fn clone_checked(block: &CheckedAlgorithmBlock) -> CheckedAlgorithmBlock {
///     block.clone()
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_ir_galec::package::CheckedAlgorithmBlock;
///
/// fn mutate_checked(block: &CheckedAlgorithmBlock) {
///     block.block().do_step.statements.clear();
/// }
/// ```
#[derive(Serialize)]
#[serde(transparent)]
pub struct CheckedAlgorithmBlock {
    block: crate::Block,
    #[serde(skip)]
    retained: crate::validate::RetainedValidation,
}

impl std::fmt::Debug for CheckedAlgorithmBlock {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("CheckedAlgorithmBlock(..)")
    }
}

/// A generative inspection borrow over one checked Algorithm Code root.
///
/// The `'id` lifetime is invariant and late-bound by
/// [`AlgorithmCodePackage::inspect`]. Subject identifiers issued through a
/// later typed projection therefore cannot escape this call or be mixed with
/// identifiers issued by another package.
pub struct AlgorithmCodeInspection<'a, 'id> {
    block: &'a crate::Block,
    retained: &'a crate::validate::RetainedValidation,
    block_variable_starts: &'a [AlgorithmCodeEvaluatedStart],
    package_brand: &'a Arc<AlgorithmCodePackageBrand>,
    _brand: PhantomData<fn(&'id mut ()) -> &'id mut ()>,
}

/// Source responsibility retained for a checked semantic subject.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticProvenance {
    Exact(rumoca_core::Span),
    NearestStatement(rumoca_core::Span),
    Generated(GeneratedSubjectKind),
}

impl std::fmt::Display for SemanticProvenance {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let span = |formatter: &mut std::fmt::Formatter<'_>, span: rumoca_core::Span| {
            write!(
                formatter,
                "source {:?} bytes {}..{}",
                span.source, span.start.0, span.end.0
            )
        };
        match self {
            Self::Exact(value) => {
                formatter.write_str("at ")?;
                span(formatter, *value)
            }
            Self::NearestStatement(value) => {
                formatter.write_str("in the statement at ")?;
                span(formatter, *value)
            }
            Self::Generated(kind) => write!(formatter, "at generated {kind:?}"),
        }
    }
}

/// Subject family responsible for a source-free generated subject.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GeneratedSubjectKind {
    Declaration,
    LifecycleMethod,
    UserFunction,
    OrderedStatement,
    Expression,
    Name,
}

/// A package-branded identity for one lifecycle method.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LifecycleMethodId<'id> {
    locator: crate::validate::MethodLoc,
    _brand: PhantomData<fn(&'id mut ()) -> &'id mut ()>,
}

impl std::fmt::Debug for LifecycleMethodId<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("LifecycleMethodId(..)")
    }
}

/// One identity paired with its immutable checked syntax subject and retained
/// provenance.
#[derive(Debug, Clone, Copy)]
pub struct LifecycleMethodSubject<'a, 'id> {
    id: LifecycleMethodId<'id>,
    kind: crate::BlockMethodKind,
    method: &'a crate::BlockMethod,
    signals: &'a [crate::ast::PredefinedSignal],
    provenance: SemanticProvenance,
}

macro_rules! branded_subject_id {
    ($name:ident, $locator:path) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name<'id> {
            locator: $locator,
            _brand: PhantomData<fn(&'id mut ()) -> &'id mut ()>,
        }

        impl std::fmt::Debug for $name<'_> {
            fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str(concat!(stringify!($name), "(..)"))
            }
        }
    };
}

branded_subject_id!(CallId, crate::validate::CallLoc);
branded_subject_id!(
    CallResultProjectionId,
    crate::validate::CallResultProjectionLoc
);
branded_subject_id!(DeclarationId, crate::validate::DeclarationLoc);
branded_subject_id!(ExpressionId, crate::validate::ExpressionLoc);
branded_subject_id!(ReferenceId, crate::validate::ReferenceLoc);
branded_subject_id!(StatementId, crate::validate::StatementLoc);
branded_subject_id!(BuiltinResultId, crate::validate::BuiltinResultLoc);
branded_subject_id!(UserFunctionId, crate::validate::FunctionLoc);
branded_subject_id!(LoopBinderId, crate::validate::BinderLoc);
branded_subject_id!(
    RealMatrixMultiplyOccurrenceId,
    crate::validate::RealMatrixMultiplyOccurrenceLoc
);

/// One construction-issued Algorithm Code subject identity.
///
/// The enum is closed over the subject families retained by package
/// construction. Consumers follow these identities and their child roles;
/// they never recover identity from a name, ordinal, or AST shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AlgorithmCodeSubjectId<'id> {
    Declaration(DeclarationId<'id>),
    LifecycleMethod(LifecycleMethodId<'id>),
    UserFunction(UserFunctionId<'id>),
    LoopBinder(LoopBinderId<'id>),
    Statement(StatementId<'id>),
    Expression(ExpressionId<'id>),
    Reference(ReferenceId<'id>),
    Call(CallId<'id>),
    CallResultProjection(CallResultProjectionId<'id>),
}

#[derive(Debug)]
struct AlgorithmCodePackageBrand;

/// Opaque owned evidence that one exact subject belongs to one exact package.
///
/// This is not a detached subject identity: it exposes neither its private
/// locator nor its package brand, has no equality/order/hash operation, and
/// cannot project a subject. It exists only so an owning refinement product
/// can retain the exact correlation after the generative inspection borrow
/// closes. The retained package is the sole projection authority.
///
/// ```compile_fail
/// use rumoca_ir_galec::package::AlgorithmCodeSubjectCorrelation;
///
/// fn duplicate(value: &AlgorithmCodeSubjectCorrelation) -> AlgorithmCodeSubjectCorrelation {
///     value.clone()
/// }
/// ```
///
/// ```compile_fail
/// let _ = rumoca_ir_galec::package::AlgorithmCodeSubjectCorrelation::default();
/// ```
pub struct AlgorithmCodeSubjectCorrelation {
    _package: Arc<AlgorithmCodePackageBrand>,
    _subject: crate::validate::SubjectLoc,
}

impl std::fmt::Debug for AlgorithmCodeSubjectCorrelation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("AlgorithmCodeSubjectCorrelation(..)")
    }
}

/// Exact ownership role of a child in the retained Algorithm Code graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeChildRole {
    BlockDeclaration(u32),
    BlockMethod(crate::BlockMethodKind),
    BlockFunction(u32),
    CompartmentMember(u32),
    DeclarationDimension(u32),
    DeclarationMinimum,
    DeclarationMaximum,
    DeclarationStart,
    MethodLocal(u32),
    MethodAction(u32),
    FunctionParameter {
        direction: crate::ast::Direction,
        index: u32,
    },
    FunctionLocal(u32),
    FunctionAction(u32),
    AssignmentTarget,
    AssignmentValue,
    MultiAssignmentTarget(u32),
    MultiAssignmentCall,
    CallStatement,
    IfCondition(u32),
    IfAction {
        branch: u32,
        index: u32,
    },
    ElseAction(u32),
    LoopBinder,
    LoopStart,
    LoopStep,
    LoopStop,
    LoopAction(u32),
    LimitTarget(u32),
    SignalFallback(u32),
    ReferenceSubscript {
        part: u32,
        index: u32,
    },
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

/// Exact parent edge retained for one Algorithm Code subject.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeSubjectParent<'id> {
    Block(AlgorithmCodeChildRole),
    Subject {
        owner: AlgorithmCodeSubjectId<'id>,
        role: AlgorithmCodeChildRole,
    },
}

/// One ordered child edge from a block root or retained subject.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AlgorithmCodeSubjectEdge<'id> {
    subject: AlgorithmCodeSubjectId<'id>,
    role: AlgorithmCodeChildRole,
}

impl<'id> AlgorithmCodeSubjectEdge<'id> {
    #[must_use]
    pub const fn subject(self) -> AlgorithmCodeSubjectId<'id> {
        self.subject
    }

    #[must_use]
    pub const fn role(self) -> AlgorithmCodeChildRole {
        self.role
    }
}

/// Exact fixed value type and compact tensor shape retained by package
/// construction. Scalars have an empty extent slice.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExactValueShape<'a> {
    scalar: ScalarType,
    extents: &'a [u32],
}

/// Construction-issued declaration ownership for executable refinement.
///
/// `PreviousState` is intentionally absent: Algorithm Code represents a
/// materialized pre-value as ordinary protected state.  A distinct previous
/// storage slot requires an upstream semantic correlation; it must never be
/// guessed from the declaration's quoted name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeDeclarationClass {
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

/// Exact scalar payload retained by one evaluated initialization fact.
/// Real values retain exact IEEE-754 bits, including the sign of zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum AlgorithmCodeEvaluatedScalar {
    RealBits(u64),
    Integer(i64),
    Boolean(bool),
}

/// Closed literal-evaluation fact issued for every checked expression.
/// A uniform tensor retains one scalar payload regardless of its extent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeEvaluatedLiteral {
    Scalar(AlgorithmCodeEvaluatedScalar),
    UniformTensorFill(AlgorithmCodeEvaluatedScalar),
    NonUniformTensor,
    Symbolic,
}

/// Evaluated start fact sealed into the manifest-visible variable catalog.
/// Unsupported tensor forms name the future initialization plan they require.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum AlgorithmCodeEvaluatedStart {
    Missing,
    Scalar(AlgorithmCodeEvaluatedScalar),
    UniformTensorFill(AlgorithmCodeEvaluatedScalar),
    UnsupportedScalarExpression,
    UnsupportedNonUniformTensor,
    UnsupportedSymbolicTensor,
}

/// Package-issued position of one manifest-visible block declaration.
///
/// This is presentation correlation, not semantic identity. Consumers may
/// use it to relate Algorithm Code and Production Code artifacts, but cannot
/// manufacture it from a name or a Solve declaration list position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AlgorithmCodeBlockDeclarationIndex(u32);

impl AlgorithmCodeBlockDeclarationIndex {
    #[must_use]
    pub const fn get(self) -> u32 {
        self.0
    }
}

/// Closed statement operation retained by Algorithm Code package construction.
///
/// The operation family is semantic input to executable refinement.  It is
/// deliberately separate from the owned syntax tree: consumers cannot recover
/// children or targets except through construction-issued subject identities.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeStatementKind {
    Assignment,
    MultiAssignment,
    Call,
    If,
    For,
    Limit,
    Signal,
}

/// Target-neutral binary operation retained by package construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeBinaryOperator {
    Power,
    Multiply,
    Divide,
    Add,
    Subtract,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

/// Closed expression operation retained by Algorithm Code package
/// construction.
///
/// Real literal payloads are represented by their IEEE-754 bits so this fact
/// preserves signed zero and NaN payloads without target spelling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeExpressionKind {
    Boolean(bool),
    Integer(i64),
    RealBits(u64),
    Reference,
    Size,
    Call,
    Parenthesized,
    If,
    Array,
    NegatedReference,
    Not,
    Binary(AlgorithmCodeBinaryOperator),
}

/// Construction-resolved target of one exact reference subject.
///
/// Both variants carry identities branded by the active inspection, so a
/// refinement cannot substitute a same-named declaration or loop binder from
/// another package.
#[derive(Debug, Clone, Copy)]
pub enum AlgorithmCodeReferenceTarget<'a, 'id> {
    Declaration(DeclarationSubject<'a, 'id>),
    LoopBinder(LoopBinderSubject<'a, 'id>),
}

impl<'a> ExactValueShape<'a> {
    #[must_use]
    pub const fn scalar(self) -> ScalarType {
        self.scalar
    }

    #[must_use]
    pub const fn extents(self) -> &'a [u32] {
        self.extents
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallSubject<'a, 'id> {
    id: CallId<'id>,
    provenance: SemanticProvenance,
    effect: crate::StatusEffect,
    _borrow: PhantomData<&'a ()>,
}

impl<'a, 'id> CallSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> CallId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn effect(self) -> crate::StatusEffect {
        self.effect
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DeclarationSubject<'a, 'id> {
    id: DeclarationId<'id>,
    provenance: SemanticProvenance,
    class: AlgorithmCodeDeclarationClass,
    block_index: Option<AlgorithmCodeBlockDeclarationIndex>,
    evaluated_start: Option<AlgorithmCodeEvaluatedStart>,
    start_expression: Option<ExpressionId<'id>>,
    value: ExactValueShape<'a>,
}

impl<'a, 'id> DeclarationSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> DeclarationId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn class(self) -> AlgorithmCodeDeclarationClass {
        self.class
    }

    /// Construction-issued zero-based position in the Algorithm Code block's
    /// manifest-visible declaration sequence. Scoped declarations have no
    /// block index and therefore cannot be mistaken for AC variables.
    #[must_use]
    pub const fn block_index(self) -> Option<AlgorithmCodeBlockDeclarationIndex> {
        self.block_index
    }

    /// Evaluated start from the sealed manifest-visible variable catalog.
    /// Scoped declarations are not members of that catalog and return `None`.
    #[must_use]
    pub const fn evaluated_start(self) -> Option<AlgorithmCodeEvaluatedStart> {
        self.evaluated_start
    }

    /// Exact construction-issued start-expression subject for a block
    /// declaration. Scoped declarations have no start owner.
    #[must_use]
    pub const fn start_expression(self) -> Option<ExpressionId<'id>> {
        self.start_expression
    }

    #[must_use]
    pub const fn value(self) -> ExactValueShape<'a> {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionSubject<'a, 'id> {
    id: ExpressionId<'id>,
    provenance: SemanticProvenance,
    kind: AlgorithmCodeExpressionKind,
    effect: crate::StatusEffect,
    value: ExactValueShape<'a>,
    evaluated_literal: AlgorithmCodeEvaluatedLiteral,
}

impl<'a, 'id> ExpressionSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> ExpressionId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn kind(self) -> AlgorithmCodeExpressionKind {
        self.kind
    }

    #[must_use]
    pub const fn effect(self) -> crate::StatusEffect {
        self.effect
    }

    #[must_use]
    pub const fn value(self) -> ExactValueShape<'a> {
        self.value
    }

    /// Construction-issued literal evaluation for this exact expression.
    #[must_use]
    pub const fn evaluated_literal(self) -> AlgorithmCodeEvaluatedLiteral {
        self.evaluated_literal
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ReferenceSubject<'a, 'id> {
    id: ReferenceId<'id>,
    provenance: SemanticProvenance,
    target: AlgorithmCodeReferenceTarget<'a, 'id>,
    value: ExactValueShape<'a>,
}

impl<'a, 'id> ReferenceSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> ReferenceId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn target(self) -> AlgorithmCodeReferenceTarget<'a, 'id> {
        self.target
    }

    #[must_use]
    pub const fn value(self) -> ExactValueShape<'a> {
        self.value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StatementSubject<'a, 'id> {
    id: StatementId<'id>,
    provenance: SemanticProvenance,
    kind: AlgorithmCodeStatementKind,
    effect: crate::StatusEffect,
    real_matrix_multiply: Option<RealMatrixMultiplyOccurrenceId<'id>>,
    _borrow: PhantomData<&'a ()>,
}

impl<'a, 'id> StatementSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> StatementId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn kind(self) -> AlgorithmCodeStatementKind {
        self.kind
    }

    #[must_use]
    pub const fn effect(self) -> crate::StatusEffect {
        self.effect
    }

    /// The exact package-issued matrix-product occurrence owned by this loop,
    /// if any. Its branded identity cannot be reconstructed from syntax.
    #[must_use]
    pub const fn real_matrix_multiply_occurrence(
        self,
    ) -> Option<RealMatrixMultiplyOccurrenceId<'id>> {
        self.real_matrix_multiply
    }
}

/// One construction-issued Real matrix-product occurrence. The owner and its
/// complete child topology were fixed in the same traversal that validated
/// and retained `contract`.
#[derive(Debug, Clone, Copy)]
pub struct RealMatrixMultiplyOccurrenceSubject<'a, 'id> {
    id: RealMatrixMultiplyOccurrenceId<'id>,
    owner: StatementSubject<'a, 'id>,
    provenance: SemanticProvenance,
    target: DeclarationSubject<'a, 'id>,
    source: DeclarationSubject<'a, 'id>,
    contract: &'a crate::ast::RealMatrixMultiplyOccurrenceContract,
}

impl<'a, 'id> RealMatrixMultiplyOccurrenceSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> RealMatrixMultiplyOccurrenceId<'id> {
        self.id
    }

    #[must_use]
    pub const fn owner(self) -> StatementSubject<'a, 'id> {
        self.owner
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }

    #[must_use]
    pub const fn target(self) -> DeclarationSubject<'a, 'id> {
        self.target
    }

    #[must_use]
    pub const fn source(self) -> DeclarationSubject<'a, 'id> {
        self.source
    }

    #[must_use]
    pub const fn contract(self) -> &'a crate::ast::RealMatrixMultiplyOccurrenceContract {
        self.contract
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UserFunctionSubject<'a, 'id> {
    id: UserFunctionId<'id>,
    provenance: SemanticProvenance,
    _borrow: PhantomData<&'a ()>,
}

impl<'a, 'id> UserFunctionSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> UserFunctionId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopBinderSubject<'a, 'id> {
    id: LoopBinderId<'id>,
    provenance: SemanticProvenance,
    _borrow: PhantomData<&'a ()>,
}

impl<'a, 'id> LoopBinderSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> LoopBinderId<'id> {
        self.id
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }
}

/// One borrowed subject in the complete package-issued semantic graph.
#[derive(Debug, Clone)]
pub enum AlgorithmCodeSubject<'a, 'id> {
    Declaration(DeclarationSubject<'a, 'id>),
    LifecycleMethod(LifecycleMethodSubject<'a, 'id>),
    UserFunction(UserFunctionSubject<'a, 'id>),
    LoopBinder(LoopBinderSubject<'a, 'id>),
    Statement(StatementSubject<'a, 'id>),
    Expression(ExpressionSubject<'a, 'id>),
    Reference(ReferenceSubject<'a, 'id>),
    Call(CallSubject<'a, 'id>),
    CallResultProjection(Box<CallResultProjectionSubject<'a, 'id>>),
}

impl<'a, 'id> AlgorithmCodeSubject<'a, 'id> {
    #[must_use]
    pub fn id(self) -> AlgorithmCodeSubjectId<'id> {
        match self {
            Self::Declaration(subject) => AlgorithmCodeSubjectId::Declaration(subject.id()),
            Self::LifecycleMethod(subject) => AlgorithmCodeSubjectId::LifecycleMethod(subject.id()),
            Self::UserFunction(subject) => AlgorithmCodeSubjectId::UserFunction(subject.id()),
            Self::LoopBinder(subject) => AlgorithmCodeSubjectId::LoopBinder(subject.id()),
            Self::Statement(subject) => AlgorithmCodeSubjectId::Statement(subject.id()),
            Self::Expression(subject) => AlgorithmCodeSubjectId::Expression(subject.id()),
            Self::Reference(subject) => AlgorithmCodeSubjectId::Reference(subject.id()),
            Self::Call(subject) => AlgorithmCodeSubjectId::Call(subject.id()),
            Self::CallResultProjection(subject) => {
                AlgorithmCodeSubjectId::CallResultProjection(subject.id())
            }
        }
    }

    #[must_use]
    pub fn provenance(self) -> SemanticProvenance {
        match self {
            Self::Declaration(subject) => subject.provenance(),
            Self::LifecycleMethod(subject) => subject.provenance(),
            Self::UserFunction(subject) => subject.provenance(),
            Self::LoopBinder(subject) => subject.provenance(),
            Self::Statement(subject) => subject.provenance(),
            Self::Expression(subject) => subject.provenance(),
            Self::Reference(subject) => subject.provenance(),
            Self::Call(subject) => subject.provenance(),
            Self::CallResultProjection(subject) => subject.provenance(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BuiltinResultSubject<'a, 'id> {
    id: BuiltinResultId<'id>,
    builtin: &'static crate::builtins::Builtin,
    output: &'static crate::builtins::BuiltinParam,
    lifted_rank: u8,
    _borrow: PhantomData<&'a ()>,
}

impl<'a, 'id> BuiltinResultSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> BuiltinResultId<'id> {
        self.id
    }

    #[must_use]
    pub const fn builtin(self) -> &'static crate::builtins::Builtin {
        self.builtin
    }

    #[must_use]
    pub const fn output(self) -> &'static crate::builtins::BuiltinParam {
        self.output
    }

    #[must_use]
    pub const fn lifted_rank(self) -> u8 {
        self.lifted_rank
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CalleeResultSubject<'a, 'id> {
    UserOutput(DeclarationSubject<'a, 'id>),
    BuiltinOutput(BuiltinResultSubject<'a, 'id>),
}

#[derive(Debug, Clone, Copy)]
pub enum CallResultReceiver<'a, 'id> {
    ExpressionValue(ExpressionSubject<'a, 'id>),
    MultiAssignmentDestination(ReferenceSubject<'a, 'id>),
    DiscardedBy(StatementSubject<'a, 'id>),
}

#[derive(Debug, Clone, Copy)]
pub struct CallResultProjectionSubject<'a, 'id> {
    id: CallResultProjectionId<'id>,
    call: CallSubject<'a, 'id>,
    callee_result: CalleeResultSubject<'a, 'id>,
    receiver: CallResultReceiver<'a, 'id>,
    value: ExactValueShape<'a>,
    order: u32,
    provenance: SemanticProvenance,
}

impl<'a, 'id> CallResultProjectionSubject<'a, 'id> {
    #[must_use]
    pub const fn id(self) -> CallResultProjectionId<'id> {
        self.id
    }

    #[must_use]
    pub const fn call(self) -> CallSubject<'a, 'id> {
        self.call
    }

    #[must_use]
    pub const fn callee_result(self) -> CalleeResultSubject<'a, 'id> {
        self.callee_result
    }

    #[must_use]
    pub const fn receiver(self) -> CallResultReceiver<'a, 'id> {
        self.receiver
    }

    #[must_use]
    pub const fn value(self) -> ExactValueShape<'a> {
        self.value
    }

    #[must_use]
    pub const fn order(self) -> u32 {
        self.order
    }

    #[must_use]
    pub const fn provenance(self) -> SemanticProvenance {
        self.provenance
    }
}

impl<'a, 'id> LifecycleMethodSubject<'a, 'id> {
    #[must_use]
    pub const fn id(&self) -> LifecycleMethodId<'id> {
        self.id
    }

    #[must_use]
    pub const fn kind(&self) -> crate::BlockMethodKind {
        self.kind
    }

    #[must_use]
    pub const fn method(&self) -> &'a crate::BlockMethod {
        self.method
    }

    /// Complete checked escape set declared by this lifecycle method.
    #[must_use]
    pub const fn signals(&self) -> &'a [crate::ast::PredefinedSignal] {
        self.signals
    }

    #[must_use]
    pub const fn provenance(&self) -> SemanticProvenance {
        self.provenance
    }
}

impl<'a, 'id> AlgorithmCodeInspection<'a, 'id> {
    /// The immutable syntax root owned by this inspection.
    #[must_use]
    pub const fn block(&self) -> &crate::Block {
        self.block
    }

    /// Number of construction-issued semantic subjects retained by this root.
    #[must_use]
    pub fn subject_count(&self) -> usize {
        self.subjects().len()
    }

    /// Every construction-issued subject that participates in the retained
    /// ownership graph. Builtin result descriptors remain reachable through
    /// their call-result projection and are not detached graph roots.
    pub fn subjects(&self) -> impl ExactSizeIterator<Item = AlgorithmCodeSubject<'a, 'id>> + '_ {
        self.retained
            .subject_locators()
            .map(|locator| self.subject(subject_id(locator)))
    }

    /// Ordered subject edges owned directly by the block root.
    pub fn root_children(
        &self,
    ) -> impl ExactSizeIterator<Item = AlgorithmCodeSubjectEdge<'id>> + '_ {
        self.retained
            .root_children()
            .iter()
            .copied()
            .map(subject_edge)
    }

    /// Ordered subject edges owned by `owner`.
    pub fn children(
        &self,
        owner: AlgorithmCodeSubjectId<'id>,
    ) -> impl ExactSizeIterator<Item = AlgorithmCodeSubjectEdge<'id>> + '_ {
        self.retained
            .subject_children(subject_locator(owner))
            .iter()
            .copied()
            .map(subject_edge)
    }

    /// Exact construction-issued parent of `subject`.
    #[must_use]
    pub fn parent(&self, subject: AlgorithmCodeSubjectId<'id>) -> AlgorithmCodeSubjectParent<'id> {
        subject_parent(
            self.retained
                .subject_parent(subject_locator(subject))
                .expect("a packaged subject retains exactly one parent"),
        )
    }

    /// Project an identity issued by this inspection to its typed subject.
    #[must_use]
    pub fn subject(&self, id: AlgorithmCodeSubjectId<'id>) -> AlgorithmCodeSubject<'a, 'id> {
        match id {
            AlgorithmCodeSubjectId::Declaration(id) => {
                AlgorithmCodeSubject::Declaration(self.declaration_subject(id.locator))
            }
            AlgorithmCodeSubjectId::LifecycleMethod(id) => {
                AlgorithmCodeSubject::LifecycleMethod(self.lifecycle_method_subject(id.locator))
            }
            AlgorithmCodeSubjectId::UserFunction(id) => {
                AlgorithmCodeSubject::UserFunction(UserFunctionSubject {
                    id,
                    provenance: semantic_provenance(
                        self.retained
                            .subject_provenance(crate::validate::SubjectLoc::Function(id.locator))
                            .expect("a packaged function retains provenance"),
                    ),
                    _borrow: PhantomData,
                })
            }
            AlgorithmCodeSubjectId::LoopBinder(id) => {
                AlgorithmCodeSubject::LoopBinder(self.loop_binder_subject(id.locator))
            }
            AlgorithmCodeSubjectId::Statement(id) => {
                AlgorithmCodeSubject::Statement(self.statement_subject(id.locator))
            }
            AlgorithmCodeSubjectId::Expression(id) => {
                AlgorithmCodeSubject::Expression(self.expression_subject(id.locator))
            }
            AlgorithmCodeSubjectId::Reference(id) => {
                AlgorithmCodeSubject::Reference(self.reference_subject(id.locator))
            }
            AlgorithmCodeSubjectId::Call(id) => {
                AlgorithmCodeSubject::Call(self.call_subject(id.locator))
            }
            AlgorithmCodeSubjectId::CallResultProjection(id) => {
                AlgorithmCodeSubject::CallResultProjection(Box::new(
                    self.call_result_projection(id.locator),
                ))
            }
        }
    }

    /// Retain an opaque package correlation for one exact branded subject.
    ///
    /// The generative identity proves which subject is being correlated; the
    /// package-owned brand prevents a certificate minted by another package
    /// from becoming evidence for this package. Consumers cannot inspect or
    /// compare the certificate and therefore cannot reconstruct identity from
    /// its representation.
    #[must_use]
    pub fn own_correlation(
        &self,
        subject: AlgorithmCodeSubjectId<'id>,
    ) -> AlgorithmCodeSubjectCorrelation {
        AlgorithmCodeSubjectCorrelation {
            _package: Arc::clone(self.package_brand),
            _subject: subject_locator(subject),
        }
    }

    /// The three construction-issued lifecycle method subjects.
    pub fn lifecycle_methods(
        &self,
    ) -> impl ExactSizeIterator<Item = LifecycleMethodSubject<'a, 'id>> + '_ {
        self.retained
            .method_locators()
            .map(|locator| self.lifecycle_method_subject(locator))
    }

    fn lifecycle_method_subject(
        &self,
        locator: crate::validate::MethodLoc,
    ) -> LifecycleMethodSubject<'a, 'id> {
        let kind = self.retained.method_kind(locator);
        let method = match kind {
            crate::BlockMethodKind::Startup => &self.block.startup,
            crate::BlockMethodKind::Recalibrate => &self.block.recalibrate,
            crate::BlockMethodKind::DoStep => &self.block.do_step,
        };
        LifecycleMethodSubject {
            id: LifecycleMethodId {
                locator,
                _brand: PhantomData,
            },
            kind,
            method,
            signals: &method.signals,
            provenance: semantic_provenance(self.retained.method_provenance(locator)),
        }
    }

    /// Every construction-issued call-result projection in package order.
    /// The order is presentation metadata; each identity is independently
    /// allocated and cannot be reconstructed from it.
    pub fn call_result_projections(
        &self,
    ) -> impl ExactSizeIterator<Item = CallResultProjectionSubject<'a, 'id>> + '_ {
        self.retained
            .call_result_projection_locators()
            .map(|locator| self.call_result_projection(locator))
    }

    fn call_result_projection(
        &self,
        locator: crate::validate::CallResultProjectionLoc,
    ) -> CallResultProjectionSubject<'a, 'id> {
        let callee_result = match self.retained.projection_callee_result(locator) {
            crate::validate::CalleeResultLoc::UserOutput(declaration) => {
                CalleeResultSubject::UserOutput(self.declaration_subject(declaration))
            }
            crate::validate::CalleeResultLoc::BuiltinOutput(result) => {
                let (builtin, lifted_rank, output) = self.retained.builtin_result_facts(result);
                CalleeResultSubject::BuiltinOutput(BuiltinResultSubject {
                    id: BuiltinResultId {
                        locator: result,
                        _brand: PhantomData,
                    },
                    builtin,
                    output,
                    lifted_rank,
                    _borrow: PhantomData,
                })
            }
        };
        let receiver = match self.retained.projection_receiver(locator) {
            crate::validate::CallResultReceiverLoc::ExpressionValue(expression) => {
                CallResultReceiver::ExpressionValue(self.expression_subject(expression))
            }
            crate::validate::CallResultReceiverLoc::MultiAssignmentDestination(reference) => {
                CallResultReceiver::MultiAssignmentDestination(self.reference_subject(reference))
            }
            crate::validate::CallResultReceiverLoc::DiscardedBy(statement) => {
                CallResultReceiver::DiscardedBy(self.statement_subject(statement))
            }
        };
        CallResultProjectionSubject {
            id: CallResultProjectionId {
                locator,
                _brand: PhantomData,
            },
            call: self.call_subject(self.retained.projection_call(locator)),
            callee_result,
            receiver,
            value: exact_value_shape(self.retained.projection_shape(locator)),
            order: self.retained.projection_order(locator),
            provenance: semantic_provenance(self.retained.projection_provenance(locator)),
        }
    }

    fn call_subject(&self, locator: crate::validate::CallLoc) -> CallSubject<'a, 'id> {
        let (provenance, effect) = self.retained.call_facts(locator);
        CallSubject {
            id: CallId {
                locator,
                _brand: PhantomData,
            },
            provenance: semantic_provenance(provenance),
            effect,
            _borrow: PhantomData,
        }
    }

    fn declaration_subject(
        &self,
        locator: crate::validate::DeclarationLoc,
    ) -> DeclarationSubject<'a, 'id> {
        let (provenance, class, value, start_expression) = self.retained.declaration_facts(locator);
        let block_index = match self
            .retained
            .subject_parent(crate::validate::SubjectLoc::Declaration(locator))
            .expect("a packaged declaration retains exactly one parent")
        {
            crate::validate::SubjectParent::Block(
                crate::validate::ChildRole::BlockDeclaration(index),
            ) => Some(AlgorithmCodeBlockDeclarationIndex(index)),
            _ => None,
        };
        let evaluated_start = block_index
            .and_then(|index| self.block_variable_starts.get(index.get() as usize))
            .copied();
        DeclarationSubject {
            id: DeclarationId {
                locator,
                _brand: PhantomData,
            },
            provenance: semantic_provenance(provenance),
            class: declaration_class(class),
            block_index,
            evaluated_start,
            start_expression: start_expression.map(|locator| ExpressionId {
                locator,
                _brand: PhantomData,
            }),
            value: exact_value_shape(value),
        }
    }

    fn loop_binder_subject(
        &self,
        locator: crate::validate::BinderLoc,
    ) -> LoopBinderSubject<'a, 'id> {
        LoopBinderSubject {
            id: LoopBinderId {
                locator,
                _brand: PhantomData,
            },
            provenance: semantic_provenance(
                self.retained
                    .subject_provenance(crate::validate::SubjectLoc::Binder(locator))
                    .expect("a packaged binder retains provenance"),
            ),
            _borrow: PhantomData,
        }
    }

    fn expression_subject(
        &self,
        locator: crate::validate::ExpressionLoc,
    ) -> ExpressionSubject<'a, 'id> {
        let (provenance, kind, effect, value) = self.retained.expression_facts(locator);
        ExpressionSubject {
            id: ExpressionId {
                locator,
                _brand: PhantomData,
            },
            provenance: semantic_provenance(provenance),
            kind: expression_kind(kind),
            effect,
            value: exact_value_shape(value),
            evaluated_literal: evaluated_literal(
                self.retained.expression_evaluated_literal(locator),
            ),
        }
    }

    fn reference_subject(
        &self,
        locator: crate::validate::ReferenceLoc,
    ) -> ReferenceSubject<'a, 'id> {
        let (provenance, target, value) = self.retained.reference_facts(locator);
        let target = match target {
            crate::validate::ResolvedTarget::Declaration(declaration) => {
                AlgorithmCodeReferenceTarget::Declaration(self.declaration_subject(declaration))
            }
            crate::validate::ResolvedTarget::Binder(binder) => {
                AlgorithmCodeReferenceTarget::LoopBinder(self.loop_binder_subject(binder))
            }
        };
        ReferenceSubject {
            id: ReferenceId {
                locator,
                _brand: PhantomData,
            },
            provenance: semantic_provenance(provenance),
            target,
            value: exact_value_shape(value),
        }
    }

    fn statement_subject(
        &self,
        locator: crate::validate::StatementLoc,
    ) -> StatementSubject<'a, 'id> {
        let (provenance, kind, effect, real_matrix_multiply) =
            self.retained.statement_facts(locator);
        StatementSubject {
            id: StatementId {
                locator,
                _brand: PhantomData,
            },
            provenance: semantic_provenance(provenance),
            kind: statement_kind(kind),
            effect,
            real_matrix_multiply: real_matrix_multiply.map(|locator| {
                RealMatrixMultiplyOccurrenceId {
                    locator,
                    _brand: PhantomData,
                }
            }),
            _borrow: PhantomData,
        }
    }
}

const fn statement_kind(kind: crate::validate::StatementKind) -> AlgorithmCodeStatementKind {
    use crate::validate::StatementKind as R;
    match kind {
        R::Assignment => AlgorithmCodeStatementKind::Assignment,
        R::MultiAssignment => AlgorithmCodeStatementKind::MultiAssignment,
        R::Call => AlgorithmCodeStatementKind::Call,
        R::If => AlgorithmCodeStatementKind::If,
        R::For => AlgorithmCodeStatementKind::For,
        R::Limit => AlgorithmCodeStatementKind::Limit,
        R::Signal => AlgorithmCodeStatementKind::Signal,
    }
}

const fn expression_kind(kind: crate::validate::ExpressionKind) -> AlgorithmCodeExpressionKind {
    use crate::validate::ExpressionKind as R;
    match kind {
        R::Boolean(value) => AlgorithmCodeExpressionKind::Boolean(value),
        R::Integer(value) => AlgorithmCodeExpressionKind::Integer(value),
        R::Real(bits) => AlgorithmCodeExpressionKind::RealBits(bits),
        R::Reference => AlgorithmCodeExpressionKind::Reference,
        R::Size => AlgorithmCodeExpressionKind::Size,
        R::Call => AlgorithmCodeExpressionKind::Call,
        R::Parenthesized => AlgorithmCodeExpressionKind::Parenthesized,
        R::If => AlgorithmCodeExpressionKind::If,
        R::Array => AlgorithmCodeExpressionKind::Array,
        R::NegatedReference => AlgorithmCodeExpressionKind::NegatedReference,
        R::Not => AlgorithmCodeExpressionKind::Not,
        R::Binary(operator) => AlgorithmCodeExpressionKind::Binary(binary_operator(operator)),
    }
}

const fn evaluated_scalar(
    scalar: crate::validate::EvaluatedScalar,
) -> AlgorithmCodeEvaluatedScalar {
    match scalar {
        crate::validate::EvaluatedScalar::RealBits(bits) => {
            AlgorithmCodeEvaluatedScalar::RealBits(bits)
        }
        crate::validate::EvaluatedScalar::Integer(value) => {
            AlgorithmCodeEvaluatedScalar::Integer(value)
        }
        crate::validate::EvaluatedScalar::Boolean(value) => {
            AlgorithmCodeEvaluatedScalar::Boolean(value)
        }
    }
}

const fn evaluated_literal(
    literal: crate::validate::EvaluatedLiteral,
) -> AlgorithmCodeEvaluatedLiteral {
    match literal {
        crate::validate::EvaluatedLiteral::Scalar(scalar) => {
            AlgorithmCodeEvaluatedLiteral::Scalar(evaluated_scalar(scalar))
        }
        crate::validate::EvaluatedLiteral::UniformTensorFill(scalar) => {
            AlgorithmCodeEvaluatedLiteral::UniformTensorFill(evaluated_scalar(scalar))
        }
        crate::validate::EvaluatedLiteral::NonUniformTensor => {
            AlgorithmCodeEvaluatedLiteral::NonUniformTensor
        }
        crate::validate::EvaluatedLiteral::Symbolic => AlgorithmCodeEvaluatedLiteral::Symbolic,
    }
}

const fn binary_operator(operator: crate::ast::BinaryOp) -> AlgorithmCodeBinaryOperator {
    use crate::ast::BinaryOp as R;
    match operator {
        R::Pow => AlgorithmCodeBinaryOperator::Power,
        R::Mul => AlgorithmCodeBinaryOperator::Multiply,
        R::Div => AlgorithmCodeBinaryOperator::Divide,
        R::Add => AlgorithmCodeBinaryOperator::Add,
        R::Sub => AlgorithmCodeBinaryOperator::Subtract,
        R::Lt => AlgorithmCodeBinaryOperator::Less,
        R::Gt => AlgorithmCodeBinaryOperator::Greater,
        R::Le => AlgorithmCodeBinaryOperator::LessEqual,
        R::Ge => AlgorithmCodeBinaryOperator::GreaterEqual,
        R::Eq => AlgorithmCodeBinaryOperator::Equal,
        R::Ne => AlgorithmCodeBinaryOperator::NotEqual,
        R::And => AlgorithmCodeBinaryOperator::And,
        R::Or => AlgorithmCodeBinaryOperator::Or,
    }
}

const fn declaration_class(
    class: crate::validate::DeclarationClass,
) -> AlgorithmCodeDeclarationClass {
    match class {
        crate::validate::DeclarationClass::Input => AlgorithmCodeDeclarationClass::Input,
        crate::validate::DeclarationClass::Output => AlgorithmCodeDeclarationClass::Output,
        crate::validate::DeclarationClass::TunableParameter => {
            AlgorithmCodeDeclarationClass::TunableParameter
        }
        crate::validate::DeclarationClass::DependentParameter => {
            AlgorithmCodeDeclarationClass::DependentParameter
        }
        crate::validate::DeclarationClass::Constant => AlgorithmCodeDeclarationClass::Constant,
        crate::validate::DeclarationClass::PersistentState => {
            AlgorithmCodeDeclarationClass::PersistentState
        }
        crate::validate::DeclarationClass::CompartmentDependentParameter => {
            AlgorithmCodeDeclarationClass::CompartmentDependentParameter
        }
        crate::validate::DeclarationClass::CompartmentConstant => {
            AlgorithmCodeDeclarationClass::CompartmentConstant
        }
        crate::validate::DeclarationClass::CompartmentPersistentState => {
            AlgorithmCodeDeclarationClass::CompartmentPersistentState
        }
        crate::validate::DeclarationClass::MethodLocal => {
            AlgorithmCodeDeclarationClass::MethodLocal
        }
        crate::validate::DeclarationClass::FunctionInput => {
            AlgorithmCodeDeclarationClass::FunctionInput
        }
        crate::validate::DeclarationClass::FunctionOutput => {
            AlgorithmCodeDeclarationClass::FunctionOutput
        }
        crate::validate::DeclarationClass::FunctionLocal => {
            AlgorithmCodeDeclarationClass::FunctionLocal
        }
    }
}

fn subject_id<'id>(locator: crate::validate::SubjectLoc) -> AlgorithmCodeSubjectId<'id> {
    match locator {
        crate::validate::SubjectLoc::Declaration(locator) => {
            AlgorithmCodeSubjectId::Declaration(DeclarationId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Method(locator) => {
            AlgorithmCodeSubjectId::LifecycleMethod(LifecycleMethodId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Function(locator) => {
            AlgorithmCodeSubjectId::UserFunction(UserFunctionId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Binder(locator) => {
            AlgorithmCodeSubjectId::LoopBinder(LoopBinderId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Statement(locator) => {
            AlgorithmCodeSubjectId::Statement(StatementId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Expression(locator) => {
            AlgorithmCodeSubjectId::Expression(ExpressionId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Reference(locator) => {
            AlgorithmCodeSubjectId::Reference(ReferenceId {
                locator,
                _brand: PhantomData,
            })
        }
        crate::validate::SubjectLoc::Call(locator) => AlgorithmCodeSubjectId::Call(CallId {
            locator,
            _brand: PhantomData,
        }),
        crate::validate::SubjectLoc::CallResultProjection(locator) => {
            AlgorithmCodeSubjectId::CallResultProjection(CallResultProjectionId {
                locator,
                _brand: PhantomData,
            })
        }
    }
}

fn subject_locator(id: AlgorithmCodeSubjectId<'_>) -> crate::validate::SubjectLoc {
    match id {
        AlgorithmCodeSubjectId::Declaration(id) => {
            crate::validate::SubjectLoc::Declaration(id.locator)
        }
        AlgorithmCodeSubjectId::LifecycleMethod(id) => {
            crate::validate::SubjectLoc::Method(id.locator)
        }
        AlgorithmCodeSubjectId::UserFunction(id) => {
            crate::validate::SubjectLoc::Function(id.locator)
        }
        AlgorithmCodeSubjectId::LoopBinder(id) => crate::validate::SubjectLoc::Binder(id.locator),
        AlgorithmCodeSubjectId::Statement(id) => crate::validate::SubjectLoc::Statement(id.locator),
        AlgorithmCodeSubjectId::Expression(id) => {
            crate::validate::SubjectLoc::Expression(id.locator)
        }
        AlgorithmCodeSubjectId::Reference(id) => crate::validate::SubjectLoc::Reference(id.locator),
        AlgorithmCodeSubjectId::Call(id) => crate::validate::SubjectLoc::Call(id.locator),
        AlgorithmCodeSubjectId::CallResultProjection(id) => {
            crate::validate::SubjectLoc::CallResultProjection(id.locator)
        }
    }
}

fn subject_edge<'id>(
    (subject, role): (crate::validate::SubjectLoc, crate::validate::ChildRole),
) -> AlgorithmCodeSubjectEdge<'id> {
    AlgorithmCodeSubjectEdge {
        subject: subject_id(subject),
        role: child_role(role),
    }
}

fn subject_parent<'id>(parent: crate::validate::SubjectParent) -> AlgorithmCodeSubjectParent<'id> {
    match parent {
        crate::validate::SubjectParent::Block(role) => {
            AlgorithmCodeSubjectParent::Block(child_role(role))
        }
        crate::validate::SubjectParent::Subject { owner, role } => {
            AlgorithmCodeSubjectParent::Subject {
                owner: subject_id(owner),
                role: child_role(role),
            }
        }
    }
}

fn child_role(role: crate::validate::ChildRole) -> AlgorithmCodeChildRole {
    use crate::validate::ChildRole as R;
    match role {
        R::BlockDeclaration(index) => AlgorithmCodeChildRole::BlockDeclaration(index),
        R::BlockMethod(owner) => AlgorithmCodeChildRole::BlockMethod(match owner {
            crate::validate::MethodOwner::Startup => crate::BlockMethodKind::Startup,
            crate::validate::MethodOwner::Recalibrate => crate::BlockMethodKind::Recalibrate,
            crate::validate::MethodOwner::DoStep => crate::BlockMethodKind::DoStep,
        }),
        R::BlockFunction(index) => AlgorithmCodeChildRole::BlockFunction(index),
        R::CompartmentMember(index) => AlgorithmCodeChildRole::CompartmentMember(index),
        R::DeclarationDimension(index) => AlgorithmCodeChildRole::DeclarationDimension(index),
        R::DeclarationMinimum => AlgorithmCodeChildRole::DeclarationMinimum,
        R::DeclarationMaximum => AlgorithmCodeChildRole::DeclarationMaximum,
        R::DeclarationStart => AlgorithmCodeChildRole::DeclarationStart,
        R::MethodLocal(index) => AlgorithmCodeChildRole::MethodLocal(index),
        R::MethodAction(index) => AlgorithmCodeChildRole::MethodAction(index),
        R::FunctionParameter { direction, index } => {
            AlgorithmCodeChildRole::FunctionParameter { direction, index }
        }
        R::FunctionLocal(index) => AlgorithmCodeChildRole::FunctionLocal(index),
        R::FunctionAction(index) => AlgorithmCodeChildRole::FunctionAction(index),
        R::AssignmentTarget => AlgorithmCodeChildRole::AssignmentTarget,
        R::AssignmentValue => AlgorithmCodeChildRole::AssignmentValue,
        R::MultiAssignmentTarget(index) => AlgorithmCodeChildRole::MultiAssignmentTarget(index),
        R::MultiAssignmentCall => AlgorithmCodeChildRole::MultiAssignmentCall,
        R::CallStatement => AlgorithmCodeChildRole::CallStatement,
        R::IfCondition(index) => AlgorithmCodeChildRole::IfCondition(index),
        R::IfAction { branch, index } => AlgorithmCodeChildRole::IfAction { branch, index },
        R::ElseAction(index) => AlgorithmCodeChildRole::ElseAction(index),
        R::LoopBinder => AlgorithmCodeChildRole::LoopBinder,
        R::LoopStart => AlgorithmCodeChildRole::LoopStart,
        R::LoopStep => AlgorithmCodeChildRole::LoopStep,
        R::LoopStop => AlgorithmCodeChildRole::LoopStop,
        R::LoopAction(index) => AlgorithmCodeChildRole::LoopAction(index),
        R::LimitTarget(index) => AlgorithmCodeChildRole::LimitTarget(index),
        R::SignalFallback(index) => AlgorithmCodeChildRole::SignalFallback(index),
        R::ReferenceSubscript { part, index } => {
            AlgorithmCodeChildRole::ReferenceSubscript { part, index }
        }
        R::CallArgument(index) => AlgorithmCodeChildRole::CallArgument(index),
        R::CallResult(index) => AlgorithmCodeChildRole::CallResult(index),
        R::Parenthesized => AlgorithmCodeChildRole::Parenthesized,
        R::NotOperand => AlgorithmCodeChildRole::NotOperand,
        R::NegatedReference => AlgorithmCodeChildRole::NegatedReference,
        R::SizeArray => AlgorithmCodeChildRole::SizeArray,
        R::SizeDimension => AlgorithmCodeChildRole::SizeDimension,
        R::IfExpressionCondition(index) => AlgorithmCodeChildRole::IfExpressionCondition(index),
        R::IfExpressionValue(index) => AlgorithmCodeChildRole::IfExpressionValue(index),
        R::IfExpressionElse => AlgorithmCodeChildRole::IfExpressionElse,
        R::ArrayElement(index) => AlgorithmCodeChildRole::ArrayElement(index),
        R::BinaryLeft => AlgorithmCodeChildRole::BinaryLeft,
        R::BinaryRight => AlgorithmCodeChildRole::BinaryRight,
        R::ExpressionReference => AlgorithmCodeChildRole::ExpressionReference,
        R::ExpressionCall => AlgorithmCodeChildRole::ExpressionCall,
        R::AggregateProjectionSource => AlgorithmCodeChildRole::AggregateProjectionSource,
    }
}

fn exact_value_shape(shape: &crate::validate::FixedValueShape) -> ExactValueShape<'_> {
    ExactValueShape {
        scalar: shape.scalar,
        extents: &shape.extents,
    }
}

fn semantic_provenance(provenance: crate::validate::SubjectProvenance) -> SemanticProvenance {
    match provenance {
        crate::validate::SubjectProvenance::Exact(span) => SemanticProvenance::Exact(span),
        crate::validate::SubjectProvenance::NearestStatement(span) => {
            SemanticProvenance::NearestStatement(span)
        }
        crate::validate::SubjectProvenance::Generated(origin) => {
            SemanticProvenance::Generated(match origin {
                crate::validate::GeneratedOrigin::Declaration => GeneratedSubjectKind::Declaration,
                crate::validate::GeneratedOrigin::LifecycleMethod => {
                    GeneratedSubjectKind::LifecycleMethod
                }
                crate::validate::GeneratedOrigin::UserFunction => {
                    GeneratedSubjectKind::UserFunction
                }
                crate::validate::GeneratedOrigin::OrderedStatement => {
                    GeneratedSubjectKind::OrderedStatement
                }
                crate::validate::GeneratedOrigin::Expression => GeneratedSubjectKind::Expression,
                crate::validate::GeneratedOrigin::Name => GeneratedSubjectKind::Name,
            })
        }
        #[cfg(test)]
        crate::validate::SubjectProvenance::Missing(_) => {
            unreachable!("a missing-provenance subject cannot enter a checked inspection")
        }
    }
}

impl CheckedAlgorithmBlock {
    pub fn construct(block: crate::Block) -> Result<Self, PackageError> {
        Self::construct_with_signal_policy(
            block,
            crate::validate::SignalClausePolicy::RetainAuthored,
        )
    }

    pub(crate) fn construct_generated(
        block: crate::Block,
        arithmetic: AlgorithmCodeArithmeticProfile,
    ) -> Result<Self, PackageError> {
        Self::construct_generated_with_profile(block, arithmetic)
    }

    fn construct_generated_with_profile(
        mut block: crate::Block,
        arithmetic: AlgorithmCodeArithmeticProfile,
    ) -> Result<Self, PackageError> {
        let retained = crate::validate::close_profiled(
            &mut block,
            crate::validate::SignalClausePolicy::DeriveGenerated,
            arithmetic,
        )
        .map_err(validation_error)?;
        Ok(Self { block, retained })
    }

    fn construct_with_signal_policy(
        mut block: crate::Block,
        signal_policy: crate::validate::SignalClausePolicy,
    ) -> Result<Self, PackageError> {
        let retained =
            crate::validate::close(&mut block, signal_policy).map_err(validation_error)?;
        Ok(Self { block, retained })
    }

    #[must_use]
    pub fn block(&self) -> &crate::Block {
        &self.block
    }
}

/// Complete normalized numeric profile selected before package construction.
///
/// Source Real and Integer representations specialize Modelica's width-neutral
/// types, while the matrix-product relation closes value-affecting reduction
/// semantics. The profile has no `Default`; target construction must supply all
/// three facts, and every later consumer reads this package-retained value.
///
/// ```compile_fail
/// fn require_default<T: Default>() {}
/// require_default::<rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile>();
/// ```
///
/// ```compile_fail
/// let _ = rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::default();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct AlgorithmCodeArithmeticProfile {
    source_real: AlgorithmCodeRealFormat,
    source_integer: AlgorithmCodeIntegerFormat,
    real_matrix_multiply: rumoca_core::RealMatrixMultiplySemantics,
}

/// Checked storage-and-arithmetic format for Algorithm Code `Real` values.
///
/// ```compile_fail
/// let _ = rumoca_ir_galec::package::AlgorithmCodeRealFormat::default();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AlgorithmCodeRealFormat {
    Binary32,
    Binary64,
}

/// Checked signed representation for source Modelica `Integer` values.
///
/// This is executable type identity, not a range inferred from a declaration.
/// Its legal domain is derived from the representation, so a package cannot
/// pair one representation with a separately supplied range.
///
/// ```compile_fail
/// let _ = rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::default();
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AlgorithmCodeIntegerFormat {
    I8,
    I16,
    I32,
    I64,
}

impl AlgorithmCodeArithmeticProfile {
    #[must_use]
    pub const fn construct(
        source_real: AlgorithmCodeRealFormat,
        source_integer: AlgorithmCodeIntegerFormat,
        real_matrix_multiply: rumoca_core::RealMatrixMultiplySemantics,
    ) -> Self {
        Self {
            source_real,
            source_integer,
            real_matrix_multiply,
        }
    }

    /// The checked format in which every primitive Real product and sum is rounded.
    #[must_use]
    pub const fn source_real(self) -> AlgorithmCodeRealFormat {
        self.source_real
    }

    /// The checked signed representation of source Modelica `Integer`.
    #[must_use]
    pub const fn source_integer(self) -> AlgorithmCodeIntegerFormat {
        self.source_integer
    }

    #[must_use]
    pub const fn real_matrix_multiply(self) -> rumoca_core::RealMatrixMultiplySemantics {
        self.real_matrix_multiply
    }
}

impl AlgorithmCodeIntegerFormat {
    /// Smallest value representable by this source Integer specialization.
    #[must_use]
    pub const fn minimum(self) -> i64 {
        match self {
            Self::I8 => i8::MIN as i64,
            Self::I16 => i16::MIN as i64,
            Self::I32 => i32::MIN as i64,
            Self::I64 => i64::MIN,
        }
    }

    /// Largest value representable by this source Integer specialization.
    #[must_use]
    pub const fn maximum(self) -> i64 {
        match self {
            Self::I8 => i8::MAX as i64,
            Self::I16 => i16::MAX as i64,
            Self::I32 => i32::MAX as i64,
            Self::I64 => i64::MAX,
        }
    }
}

/// One dependent parameter whose defining call was evaluated while the code
/// was generated and emitted as a literal.
///
/// SPEC_0034 GAL-017 lets `Startup` call builtins only, so a parameter bound
/// to a Modelica function call reaches the block as a value rather than as the
/// call that produced it. The call is then absent from every emitted artifact,
/// which is why the correlation is recorded here: the package states which
/// block variable was folded and which function it was folded from, without
/// any generated code having to carry a comment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ConstantFoldedParameter {
    /// Block variable the folded value is assigned to.
    pub variable: String,
    /// Modelica function whose call the value was folded from.
    pub folded_from: String,
    /// Scalars the folded value carries.
    pub scalars: usize,
}

/// Complete metadata supplied while closing one target-specialized Algorithm
/// Code package.
///
/// Keeping these inputs in one value is the migration boundary for
/// SPEC_0034 GAL-041 package construction: a caller using
/// [`AlgorithmCodePackage::construct`] cannot first expose a package and then
/// attach projection facts to it. This is deliberately only a
/// construction-input carrier. It is not a semantic-subject index and grants
/// no identity or refinement capability.
#[derive(Debug, Clone)]
pub struct AlgorithmCodePackageMetadata {
    variable_nominals: Vec<Option<f64>>,
    clock_variable_name: String,
    constant_folded_parameters: Vec<ConstantFoldedParameter>,
    arithmetic_profile: AlgorithmCodeArithmeticProfile,
}

impl AlgorithmCodePackageMetadata {
    /// Supply every target-neutral package field in one call.
    #[must_use]
    pub fn new(
        variable_nominals: Vec<Option<f64>>,
        clock_variable_name: impl Into<String>,
        constant_folded_parameters: Vec<ConstantFoldedParameter>,
        arithmetic_profile: AlgorithmCodeArithmeticProfile,
    ) -> Self {
        Self {
            variable_nominals,
            clock_variable_name: clock_variable_name.into(),
            constant_folded_parameters,
            arithmetic_profile,
        }
    }
}

/// Checked GALEC block plus projection correlations and one normalized target
/// numeric profile.
///
/// The package is intentionally non-cloneable. A future GAL-041 semantic
/// index will be generatively branded, so copying the carrier must replay the
/// whole-root constructor and issue a fresh index rather than duplicate an
/// identity domain.
///
/// ```compile_fail
/// use rumoca_ir_galec::package::AlgorithmCodePackage;
///
/// fn duplicate(package: &AlgorithmCodePackage) -> AlgorithmCodePackage {
///     <AlgorithmCodePackage as Clone>::clone(package)
/// }
/// ```
#[derive(Serialize)]
pub struct AlgorithmCodePackage {
    block: CheckedAlgorithmBlock,
    #[serde(skip)]
    correlation_brand: Arc<AlgorithmCodePackageBrand>,
    variable_nominals: Vec<Option<f64>>,
    block_variable_starts: Box<[AlgorithmCodeEvaluatedStart]>,
    /// One-based ordinal in the block declaration order.
    clock_variable_ordinal: usize,
    constant_folded_parameters: Vec<ConstantFoldedParameter>,
    /// The complete numeric profile fixed for this executable package.
    arithmetic_profile: AlgorithmCodeArithmeticProfile,
}

impl std::fmt::Debug for AlgorithmCodePackage {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("AlgorithmCodePackage(..)")
    }
}

impl AlgorithmCodePackage {
    /// Close a block and all package metadata atomically.
    ///
    /// No package is exposed unless generated signal closure, whole-block
    /// validation, metadata cardinality, and the clock correlation all close.
    /// The compiler projection additionally requires exact fixed shapes before
    /// it can expose its branded semantic inspection.
    pub fn construct(
        block: crate::Block,
        metadata: AlgorithmCodePackageMetadata,
    ) -> Result<Self, PackageError> {
        let AlgorithmCodePackageMetadata {
            variable_nominals,
            clock_variable_name,
            constant_folded_parameters,
            arithmetic_profile,
        } = metadata;
        let block = CheckedAlgorithmBlock::construct_generated(block, arithmetic_profile)?;
        block
            .retained
            .require_fixed_value_shapes()
            .map_err(|error| PackageError::UnprovenValueShape {
                subject: error.subject,
                provenance: semantic_provenance(error.provenance),
            })?;
        let declarations = block_declarations(block.block());
        let block_variable_starts =
            block_variable_starts(block.block(), &block.retained).into_boxed_slice();
        if declarations.len() != variable_nominals.len() {
            return Err(PackageError::VariableCount {
                block: declarations.len(),
                metadata: variable_nominals.len(),
            });
        }
        let clock_variable_ordinal = clock_ordinal(block.block(), &clock_variable_name)
            .ok_or(PackageError::InvalidClockReference(clock_variable_name))?;
        validate_variable_nominals(&declarations, &variable_nominals)?;
        validate_constant_folds(block.block(), &constant_folded_parameters)?;
        Ok(Self {
            block,
            correlation_brand: Arc::new(AlgorithmCodePackageBrand),
            variable_nominals,
            block_variable_starts,
            clock_variable_ordinal,
            constant_folded_parameters,
            arithmetic_profile,
        })
    }

    /// The complete normalized numeric profile issued with this package.
    #[must_use]
    pub const fn arithmetic_profile(&self) -> AlgorithmCodeArithmeticProfile {
        self.arithmetic_profile
    }

    #[must_use]
    pub fn block(&self) -> &crate::Block {
        self.block.block()
    }

    #[must_use]
    pub const fn checked_block(&self) -> &CheckedAlgorithmBlock {
        &self.block
    }

    /// Inspect this package under a fresh invariant semantic-identity brand.
    ///
    /// An identity cannot escape its inspection call:
    ///
    /// ```compile_fail
    /// use rumoca_ir_galec::package::{AlgorithmCodePackage, CallResultProjectionId};
    ///
    /// fn escape(package: &AlgorithmCodePackage) -> CallResultProjectionId<'static> {
    ///     package.inspect(|view| view.call_result_projections().next().unwrap().id())
    /// }
    /// ```
    ///
    /// An identity issued by one package cannot enter another package's
    /// inspection brand:
    ///
    /// ```compile_fail
    /// use rumoca_ir_galec::package::{
    ///     AlgorithmCodeInspection, AlgorithmCodePackage, CallResultProjectionId,
    /// };
    ///
    /// fn accept<'id>(
    ///     _view: &AlgorithmCodeInspection<'_, 'id>,
    ///     _id: CallResultProjectionId<'id>,
    /// ) {}
    ///
    /// fn mix(left: &AlgorithmCodePackage, right: &AlgorithmCodePackage) {
    ///     left.inspect(|left_view| {
    ///         let id = left_view.call_result_projections().next().unwrap().id();
    ///         right.inspect(|right_view| accept(&right_view, id));
    ///     });
    /// }
    /// ```
    pub fn inspect<R>(
        &self,
        inspect: impl for<'id> FnOnce(AlgorithmCodeInspection<'_, 'id>) -> R,
    ) -> R {
        inspect(AlgorithmCodeInspection {
            block: &self.block.block,
            retained: &self.block.retained,
            block_variable_starts: &self.block_variable_starts,
            package_brand: &self.correlation_brand,
            _brand: PhantomData,
        })
    }

    #[must_use]
    pub fn variable_nominals(&self) -> &[Option<f64>] {
        &self.variable_nominals
    }

    #[must_use]
    pub const fn clock_variable_ordinal(&self) -> usize {
        self.clock_variable_ordinal
    }

    #[must_use]
    pub fn constant_folded_parameters(&self) -> &[ConstantFoldedParameter] {
        &self.constant_folded_parameters
    }
}
