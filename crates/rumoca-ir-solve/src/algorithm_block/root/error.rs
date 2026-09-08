//! Typed construction refusals for the sealed Algorithm Code refinement.

use rumoca_ir_galec::package::{
    AlgorithmCodeDeclarationClass, AlgorithmCodeExpressionKind, AlgorithmCodeStatementKind,
    SemanticProvenance,
};

use crate::{
    CallTransferConstructionError, SolveProgramConstructionError, SolveStorageClass,
    SolveTypeConstructionError,
};

use super::{SolveAlgorithmMethodKind, UnsupportedTensorInitializationPlan};

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum SolveAlgorithmBlockConstructionError {
    #[error("{method:?} was mapped more than once")]
    DuplicateLifecycleMethod { method: SolveAlgorithmMethodKind },
    #[error("{method:?} contains Algorithm Code behavior without a Solve refinement {provenance}")]
    UnsupportedLifecycleSource {
        method: SolveAlgorithmMethodKind,
        provenance: SemanticProvenance,
    },
    #[error("{method:?} lifecycle action count exceeds u32 {provenance}")]
    LifecycleActionCountOverflow {
        method: SolveAlgorithmMethodKind,
        provenance: SemanticProvenance,
    },
    #[error("unsupported Algorithm Code statement {kind:?} {provenance}")]
    UnsupportedStatement {
        kind: AlgorithmCodeStatementKind,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code statement is not a direct lifecycle action {provenance}")]
    UnsupportedStatementContext { provenance: SemanticProvenance },
    #[error("Algorithm Code statement precedes its lifecycle owner {provenance}")]
    StatementBeforeLifecycleMethod { provenance: SemanticProvenance },
    #[error("Algorithm Code statement cites a foreign lifecycle owner {provenance}")]
    ForeignLifecycleStatement { provenance: SemanticProvenance },
    #[error("Algorithm Code lifecycle action {action} is out of bounds {provenance}")]
    LifecycleActionOutOfBounds {
        action: u32,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code statement was mapped more than once {provenance}")]
    DuplicateStatement { provenance: SemanticProvenance },
    #[error("Algorithm Code expression has no admitted executable role {provenance}")]
    UnsupportedExpressionContext { provenance: SemanticProvenance },
    #[error("unsupported Algorithm Code expression {kind:?} {provenance}")]
    UnsupportedExpression {
        kind: AlgorithmCodeExpressionKind,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code scalar-literal expression has aggregate shape {provenance}")]
    UnsupportedExpressionShape { provenance: SemanticProvenance },
    #[error("Algorithm Code scalar literal disagrees with its checked type {provenance}")]
    LiteralTypeMismatch { provenance: SemanticProvenance },
    #[error("Algorithm Code scalar literal is invalid for Solve: {source} {provenance}")]
    Literal {
        source: SolveTypeConstructionError,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code reference has no admitted executable role {provenance}")]
    UnsupportedReferenceContext { provenance: SemanticProvenance },
    #[error("Algorithm Code reference target is not a block declaration {provenance}")]
    UnsupportedReferenceTarget { provenance: SemanticProvenance },
    #[error("Algorithm Code method-local use needs a checked region operation {provenance}")]
    UnsupportedMethodLocalReference { provenance: SemanticProvenance },
    #[error(
        "Algorithm Code assignment target has no admitted scalar or Startup aggregate plan {provenance}"
    )]
    UnsupportedReferenceShape { provenance: SemanticProvenance },
    #[error("Algorithm Code assignment target cites an unmapped declaration {provenance}")]
    ForeignAssignmentTarget { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration has no sealed evaluated start {provenance}")]
    MissingEvaluatedDeclarationStart { provenance: SemanticProvenance },
    #[error(
        "Algorithm Code declaration start has no supported evaluated initialization plan {provenance}"
    )]
    UnsupportedEvaluatedDeclarationStart { provenance: SemanticProvenance },
    #[error("unsupported capability: {plan} {provenance}")]
    UnsupportedTensorInitializationPlan {
        plan: UnsupportedTensorInitializationPlan,
        provenance: SemanticProvenance,
    },
    #[error("unsupported capability: future general tensor assignment plan {provenance}")]
    UnsupportedTensorAssignmentPlan { provenance: SemanticProvenance },
    #[error("aggregate literal member precedes its checked root owner {provenance}")]
    AggregateLiteralMemberBeforeOwner { provenance: SemanticProvenance },
    #[error(
        "Algorithm Code declaration start disagrees with its sealed evaluated catalog value {provenance}"
    )]
    DeclarationInitializationCatalogMismatch { provenance: SemanticProvenance },
    #[error(
        "Startup initialization value disagrees with the sealed declaration start {provenance}"
    )]
    StartupInitializationValueMismatch { provenance: SemanticProvenance },
    #[error("declaration initialization occurs outside Startup {provenance}")]
    StartupInitializationOutsideStartup { provenance: SemanticProvenance },
    #[error("declaration has more than one Startup initialization {provenance}")]
    DuplicateStartupInitialization { provenance: SemanticProvenance },
    #[error("declaration has no Startup initialization {provenance}")]
    MissingStartupInitialization { provenance: SemanticProvenance },
    #[error("Startup initialization coverage counter is inconsistent")]
    StartupInitializationCoverageMismatch,
    #[error("{method:?} cannot write {storage:?} storage through this action {provenance}")]
    IllegalLifecycleWrite {
        storage: SolveStorageClass,
        method: SolveAlgorithmMethodKind,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code assignment target was mapped more than once {provenance}")]
    DuplicateAssignmentTarget { provenance: SemanticProvenance },
    #[error("Algorithm Code assignment value was mapped more than once {provenance}")]
    DuplicateAssignmentValue { provenance: SemanticProvenance },
    #[error("Algorithm Code assignment child cites an unmapped statement {provenance}")]
    ForeignAssignmentChild { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration initializer cites an unmapped declaration {provenance}")]
    ForeignDeclarationInitialization { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration initializer was mapped more than once {provenance}")]
    DuplicateDeclarationInitialization { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration has no executable initialization {provenance}")]
    MissingDeclarationInitialization { provenance: SemanticProvenance },
    #[error("{method:?} has an unmapped lifecycle action")]
    MissingLifecycleAction { method: SolveAlgorithmMethodKind },
    #[error("Algorithm Code lifecycle action has no issued execution disposition {provenance}")]
    MissingActionExecution { provenance: SemanticProvenance },
    #[error("Algorithm Code lifecycle action has the wrong execution disposition {provenance}")]
    ActionExecutionDispositionMismatch { provenance: SemanticProvenance },
    #[error("Algorithm Code assignment has no mapped target {provenance}")]
    MissingAssignmentTarget { provenance: SemanticProvenance },
    #[error("Algorithm Code assignment has no mapped value {provenance}")]
    MissingAssignmentValue { provenance: SemanticProvenance },
    #[error("Solve declaration index exceeds u32 {provenance}")]
    DeclarationIndexOverflow { provenance: SemanticProvenance },
    #[error("typed lifecycle program construction failed: {0}")]
    Program(SolveProgramConstructionError),
    #[error("typed lifecycle program slots and declaration bindings disagree")]
    ProgramStorageBindingMismatch,
    #[error("{method:?} was not mapped")]
    MissingLifecycleMethod { method: SolveAlgorithmMethodKind },
    #[error("Algorithm Code subject count overflowed during refinement")]
    SubjectCountOverflow,
    #[error("Algorithm Code refinement attempted to map a foreign or duplicate subject")]
    ForeignOrDuplicateSubject,
    #[error("Algorithm Code refinement changed construction-issued subject order")]
    ReorderedSubject,
    #[error("Algorithm Code refinement has no package-owned correlation for an issued subject")]
    MissingSubjectCorrelation,
    #[error("Algorithm Code declaration subject was mapped more than once {provenance}")]
    DuplicateDeclarationSubject { provenance: SemanticProvenance },
    #[error("Algorithm Code refinement mapped {mapped} of {expected} semantic subjects")]
    UnmappedSubjects { expected: usize, mapped: usize },
    #[error("Algorithm Code expression is not a declaration dimension {provenance}")]
    ExpressionIsNotDeclarationDimension { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration dimension has no mapped declaration {provenance}")]
    ForeignDeclarationDimension { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration dimension axis {axis} is out of bounds {provenance}")]
    DeclarationDimensionOutOfBounds {
        axis: u32,
        provenance: SemanticProvenance,
    },
    #[error(
        "Algorithm Code declaration dimension axis {axis} was mapped more than once {provenance}"
    )]
    DuplicateDeclarationDimension {
        axis: u32,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code declaration dimension axis count exceeds u32 {provenance}")]
    DeclarationDimensionAxisOverflow { provenance: SemanticProvenance },
    #[error("Algorithm Code declaration dimension axis {axis} was not mapped {provenance}")]
    MissingDeclarationDimension {
        axis: u32,
        provenance: SemanticProvenance,
    },
    #[error(
        "Algorithm Code declaration class {class:?} is not yet representable in Solve {provenance}"
    )]
    UnsupportedDeclarationClass {
        class: AlgorithmCodeDeclarationClass,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code MethodLocal has no lifecycle-method owner {provenance}")]
    UnsupportedMethodLocalOwner { provenance: SemanticProvenance },
    #[error("Algorithm Code MethodLocal index exceeds the Solve identity domain {provenance}")]
    MethodLocalIndexOverflow { provenance: SemanticProvenance },
    #[error("Algorithm Code MethodLocal order disagrees with its issued owner edge {provenance}")]
    MethodLocalOrderMismatch { provenance: SemanticProvenance },
    #[error("Algorithm Code MethodLocal dimension precedes its lifecycle owner {provenance}")]
    MethodLocalBeforeLifecycleMethod { provenance: SemanticProvenance },
    #[error("Algorithm Code MethodLocal logical storage overflowed {provenance}")]
    MethodLocalStorageOverflow { provenance: SemanticProvenance },
    #[error("one or more MethodLocal declarations have no mapped lifecycle owner")]
    UnownedMethodLocals,
    #[error(
        "Algorithm Code declaration has an invalid executable value type: {source} {provenance}"
    )]
    ValueType {
        source: SolveTypeConstructionError,
        provenance: SemanticProvenance,
    },
    #[error("Algorithm Code declaration overflows {storage:?} logical scalar storage {provenance}")]
    LogicalStorageOverflow {
        storage: SolveStorageClass,
        provenance: SemanticProvenance,
    },
    #[error("the complete call-transfer catalog was not issued")]
    MissingCallTransferCatalog,
    #[error("the call-transfer catalog was issued more than once")]
    DuplicateCallTransferCatalog,
    #[error("the call-transfer catalog is invalid: {0}")]
    CallTransfer(CallTransferConstructionError),
}

impl SolveAlgorithmBlockConstructionError {
    /// Exact source span responsible for a source-owned refinement failure.
    ///
    /// This match is intentionally exhaustive: adding an error must classify
    /// its diagnostic responsibility instead of silently falling back to a
    /// global diagnostic.
    #[must_use]
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        let provenance = match self {
            Self::UnsupportedLifecycleSource { provenance, .. }
            | Self::LifecycleActionCountOverflow { provenance, .. }
            | Self::UnsupportedStatement { provenance, .. }
            | Self::UnsupportedStatementContext { provenance }
            | Self::StatementBeforeLifecycleMethod { provenance }
            | Self::ForeignLifecycleStatement { provenance }
            | Self::LifecycleActionOutOfBounds { provenance, .. }
            | Self::DuplicateStatement { provenance }
            | Self::UnsupportedExpressionContext { provenance }
            | Self::UnsupportedExpression { provenance, .. }
            | Self::UnsupportedExpressionShape { provenance }
            | Self::LiteralTypeMismatch { provenance }
            | Self::Literal { provenance, .. }
            | Self::UnsupportedReferenceContext { provenance }
            | Self::UnsupportedReferenceTarget { provenance }
            | Self::UnsupportedMethodLocalReference { provenance }
            | Self::UnsupportedReferenceShape { provenance }
            | Self::ForeignAssignmentTarget { provenance }
            | Self::MissingEvaluatedDeclarationStart { provenance }
            | Self::UnsupportedEvaluatedDeclarationStart { provenance }
            | Self::UnsupportedTensorInitializationPlan { provenance, .. }
            | Self::UnsupportedTensorAssignmentPlan { provenance }
            | Self::AggregateLiteralMemberBeforeOwner { provenance }
            | Self::DeclarationInitializationCatalogMismatch { provenance }
            | Self::StartupInitializationValueMismatch { provenance }
            | Self::StartupInitializationOutsideStartup { provenance }
            | Self::DuplicateStartupInitialization { provenance }
            | Self::MissingStartupInitialization { provenance }
            | Self::IllegalLifecycleWrite { provenance, .. }
            | Self::DuplicateAssignmentTarget { provenance }
            | Self::DuplicateAssignmentValue { provenance }
            | Self::ForeignAssignmentChild { provenance }
            | Self::ForeignDeclarationInitialization { provenance }
            | Self::DuplicateDeclarationInitialization { provenance }
            | Self::MissingDeclarationInitialization { provenance }
            | Self::MissingActionExecution { provenance }
            | Self::ActionExecutionDispositionMismatch { provenance }
            | Self::MissingAssignmentTarget { provenance }
            | Self::MissingAssignmentValue { provenance }
            | Self::DeclarationIndexOverflow { provenance }
            | Self::DuplicateDeclarationSubject { provenance }
            | Self::ExpressionIsNotDeclarationDimension { provenance }
            | Self::ForeignDeclarationDimension { provenance }
            | Self::DeclarationDimensionOutOfBounds { provenance, .. }
            | Self::DuplicateDeclarationDimension { provenance, .. }
            | Self::DeclarationDimensionAxisOverflow { provenance }
            | Self::MissingDeclarationDimension { provenance, .. }
            | Self::UnsupportedDeclarationClass { provenance, .. }
            | Self::UnsupportedMethodLocalOwner { provenance }
            | Self::MethodLocalIndexOverflow { provenance }
            | Self::MethodLocalOrderMismatch { provenance }
            | Self::MethodLocalBeforeLifecycleMethod { provenance }
            | Self::MethodLocalStorageOverflow { provenance }
            | Self::ValueType { provenance, .. }
            | Self::LogicalStorageOverflow { provenance, .. } => *provenance,
            Self::Program(error) => return error.source_span(),
            Self::DuplicateLifecycleMethod { .. }
            | Self::MissingLifecycleAction { .. }
            | Self::MissingLifecycleMethod { .. }
            | Self::SubjectCountOverflow
            | Self::ForeignOrDuplicateSubject
            | Self::ReorderedSubject
            | Self::MissingSubjectCorrelation
            | Self::UnmappedSubjects { .. }
            | Self::StartupInitializationCoverageMismatch
            | Self::ProgramStorageBindingMismatch
            | Self::MissingCallTransferCatalog
            | Self::DuplicateCallTransferCatalog
            | Self::UnownedMethodLocals
            | Self::CallTransfer(_) => return None,
        };
        match provenance {
            SemanticProvenance::Exact(span) | SemanticProvenance::NearestStatement(span)
                if !span.is_dummy() =>
            {
                Some(span)
            }
            SemanticProvenance::Exact(_)
            | SemanticProvenance::NearestStatement(_)
            | SemanticProvenance::Generated(_) => None,
        }
    }
}

impl From<CallTransferConstructionError> for SolveAlgorithmBlockConstructionError {
    fn from(error: CallTransferConstructionError) -> Self {
        Self::CallTransfer(error)
    }
}
