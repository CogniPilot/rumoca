//! Presentation of the phase-local closed C61 diagnostic vocabulary.

use std::fmt;

/// Closed metadata identity in the scalar constant-derivative checker.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolveMetadataField {
    PureCallOwners,
    ImplicitRowTargets,
    AlgebraicProjectionBlocks,
    ManifoldProjectionBlocks,
    InitializationProjectionUnknowns,
    InitializationProjectionBlocks,
    InitializationUpdateTargets,
    ContinuousRefreshRows,
    ContinuousRefreshStaticParameters,
    StructuralImplicit,
    StructuralAlgebraicProjectionBlocks,
    StructuralManifold,
    StructuralManifoldProjectionBlocks,
    InitializationStructuralResidual,
    InitializationStructuralProjectionBlocks,
    DiscreteUpdateTargets,
    DiscreteEventIterationRuns,
    DiscreteRuntimeAssignmentTargets,
    DiscreteRuntimeAssignmentRoles,
    DiscretePostCommitTargets,
    DiscretePostCommitRuntimeRows,
    DiscreteRowRoles,
    DiscretePreModes,
    DiscreteObservationRefresh,
    DiscreteObservationRefreshReadsY,
    DiscreteIntegratorHistoryEffects,
    DiscreteClockOwners,
    DiscreteStructuredUpdates,
    DiscreteGuardedAssignments,
    DiscreteEventTransactions,
    DiscreteClockPartitionOrder,
    DiscreteClockIntermediateTargets,
    DiscreteClockIntermediateClocks,
    EventRootMemoryTargets,
    EventRootZeroDomains,
    EventRootRefreshRoles,
    EventConditionMemories,
    EventScheduledRoots,
    EventScheduledTimes,
    EventDynamicTimeNames,
    EventActions,
    EventHasTerminal,
    EventDelayTargets,
    EventDelayDiscreteFlags,
    ClockSchedules,
    ClockActivationParameters,
}

impl fmt::Display for SolveMetadataField {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::PureCallOwners => "pure call owners",
            Self::ImplicitRowTargets => "implicit row targets",
            Self::AlgebraicProjectionBlocks => "algebraic projection blocks",
            Self::ManifoldProjectionBlocks => "manifold projection blocks",
            Self::InitializationProjectionUnknowns => "initialization projection unknowns",
            Self::InitializationProjectionBlocks => "initialization projection blocks",
            Self::InitializationUpdateTargets => "initialization update targets",
            Self::ContinuousRefreshRows => "continuous refresh rows",
            Self::ContinuousRefreshStaticParameters => "continuous refresh static parameters",
            Self::StructuralImplicit => "structural implicit",
            Self::StructuralAlgebraicProjectionBlocks => "structural algebraic projection blocks",
            Self::StructuralManifold => "structural manifold",
            Self::StructuralManifoldProjectionBlocks => "structural manifold projection blocks",
            Self::InitializationStructuralResidual => "initialization structural residual",
            Self::InitializationStructuralProjectionBlocks => {
                "initialization structural projection blocks"
            }
            Self::DiscreteUpdateTargets => "discrete update targets",
            Self::DiscreteEventIterationRuns => "discrete event iteration runs",
            Self::DiscreteRuntimeAssignmentTargets => "discrete runtime assignment targets",
            Self::DiscreteRuntimeAssignmentRoles => "discrete runtime assignment roles",
            Self::DiscretePostCommitTargets => "discrete post-commit targets",
            Self::DiscretePostCommitRuntimeRows => "discrete post-commit runtime rows",
            Self::DiscreteRowRoles => "discrete row roles",
            Self::DiscretePreModes => "discrete pre modes",
            Self::DiscreteObservationRefresh => "discrete observation refresh",
            Self::DiscreteObservationRefreshReadsY => "discrete observation reads Y",
            Self::DiscreteIntegratorHistoryEffects => "discrete history effects",
            Self::DiscreteClockOwners => "discrete clock owners",
            Self::DiscreteStructuredUpdates => "discrete structured updates",
            Self::DiscreteGuardedAssignments => "discrete guarded assignments",
            Self::DiscreteEventTransactions => "discrete event transactions",
            Self::DiscreteClockPartitionOrder => "discrete clock partition order",
            Self::DiscreteClockIntermediateTargets => "discrete clock intermediate targets",
            Self::DiscreteClockIntermediateClocks => "discrete clock intermediate clocks",
            Self::EventRootMemoryTargets => "event root memory targets",
            Self::EventRootZeroDomains => "event root zero domains",
            Self::EventRootRefreshRoles => "event root refresh roles",
            Self::EventConditionMemories => "event condition memories",
            Self::EventScheduledRoots => "event scheduled roots",
            Self::EventScheduledTimes => "event scheduled times",
            Self::EventDynamicTimeNames => "event dynamic time names",
            Self::EventActions => "event actions",
            Self::EventHasTerminal => "event terminal flag",
            Self::EventDelayTargets => "event delay targets",
            Self::EventDelayDiscreteFlags => "event delay flags",
            Self::ClockSchedules => "clock schedules",
            Self::ClockActivationParameters => "clock activation parameters",
        })
    }
}

/// Closed executable-owner identity in the scalar constant-derivative checker.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SolveExecutableOwner {
    ContinuousImplicit,
    ContinuousResidual,
    ContinuousManifold,
    DerivativeKernel,
    InitializationResidual,
    StructuredDiscrete,
    ImplicitTensorJvp,
    ManifoldJvp,
    InitializationJvp,
    InitializationUpdates,
    RuntimeAssignments,
    PostCommitAssignments,
    DiscreteRows,
    ClockIntermediates,
    RootConditions,
    DynamicTimeEvents,
    ActionConditions,
    DelaySources,
    DelayTimes,
    DelayMaxima,
    ExactRefreshPrograms,
    FullDerivativeJvp,
    ImplicitScalarJvp,
    VisibleRows,
    GuardedAssignments,
    EventMessagePrograms,
    CompactTensorSetup,
    EventTransactionPrograms,
}

impl fmt::Display for SolveExecutableOwner {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::ContinuousImplicit => "continuous implicit",
            Self::ContinuousResidual => "continuous residual",
            Self::ContinuousManifold => "continuous manifold",
            Self::DerivativeKernel => "derivative kernel",
            Self::InitializationResidual => "initialization residual",
            Self::StructuredDiscrete => "structured discrete",
            Self::ImplicitTensorJvp => "implicit tensor JVP",
            Self::ManifoldJvp => "manifold JVP",
            Self::InitializationJvp => "initialization JVP",
            Self::InitializationUpdates => "initialization updates",
            Self::RuntimeAssignments => "runtime assignments",
            Self::PostCommitAssignments => "post-commit assignments",
            Self::DiscreteRows => "discrete rows",
            Self::ClockIntermediates => "clock intermediates",
            Self::RootConditions => "root conditions",
            Self::DynamicTimeEvents => "dynamic time events",
            Self::ActionConditions => "action conditions",
            Self::DelaySources => "delay sources",
            Self::DelayTimes => "delay times",
            Self::DelayMaxima => "delay maxima",
            Self::ExactRefreshPrograms => "exact refresh programs",
            Self::FullDerivativeJvp => "full derivative JVP",
            Self::ImplicitScalarJvp => "implicit scalar JVP",
            Self::VisibleRows => "visible rows",
            Self::GuardedAssignments => "guarded assignments",
            Self::EventMessagePrograms => "event message programs",
            Self::CompactTensorSetup => "compact tensor setup",
            Self::EventTransactionPrograms => "event transaction programs",
        })
    }
}

/// Mass-matrix representations refused by the identity-only profile.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NonIdentityMassMatrixKind {
    Diagonal,
    Sparse,
}

impl fmt::Display for NonIdentityMassMatrixKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Diagonal => "Diagonal",
            Self::Sparse => "Sparse",
        })
    }
}

/// Structural representations distinct from the dedicated Empty pattern.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NonemptyDerivativePatternKind {
    Full,
    Diagonal,
    Banded,
    Csr,
    Affine,
}

impl fmt::Display for NonemptyDerivativePatternKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Full => "full",
            Self::Diagonal => "diagonal",
            Self::Banded => "banded",
            Self::Csr => "csr",
            Self::Affine => "affine",
        })
    }
}

/// Structural pattern identity retained by a C61 mismatch.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DerivativePatternKind {
    Absent,
    Empty,
    Other(NonemptyDerivativePatternKind),
}

impl fmt::Display for DerivativePatternKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Absent => formatter.write_str("absent"),
            Self::Empty => formatter.write_str("empty"),
            Self::Other(kind) => kind.fmt(formatter),
        }
    }
}
