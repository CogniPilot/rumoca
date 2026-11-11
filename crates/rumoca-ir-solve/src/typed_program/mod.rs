//! Typed, storage-neutral executable Solve vocabulary.

mod algorithm_block;
mod call;
mod compact_placement;
mod demand;
mod entry;
mod integer_interval;
mod pass_independence;
mod program;
mod target;
mod target_specialization;
mod types;

pub use compact_placement::{CompactJacobianPlacement, CompactJacobianPlacementReceipt};
pub use target::{SolveAdmittedFunctionDomain, SolveFunctionTarget, SolveFunctionTargetRefusal};

pub use algorithm_block::{
    SolveAlgorithmBlock, SolveAlgorithmBlockConstructionError, SolveAlgorithmBlockDigest,
    SolveAlgorithmBlockDigestError, SolveAlgorithmCodeEntityKind, SolveAlgorithmCodeRef,
    SolveBlockCallLevels, SolveBlockFunctions, SolveBlockMethod, SolveBlockMethodKind,
    SolveBlockSlotDeclaration, SolveStatusBinding,
};
pub(crate) use call::dependency::CheckedCallDependencyProjection;
pub use call::dependency::SolveCallDependency;
pub use call::{
    ArticulatedJointReduction, AssertionRegionKind, AssertionRegionStep, AssertionSource,
    BodyQuaternionKinematics, BoundSolvePureCallBody, CheckedAssertionFlow,
    ScopedSolvePureCallTable, SolveAssertionLevel, SolveCallSiteCount, SolveCallValueRole,
    SolvePureCallDirectionalOwner, SolvePureCallDirectionalSite, SolvePureCallIdentity,
    SolvePureCallInputCoordinate, SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwner,
    SolvePureCallOwnerId, SolvePureCallSite, SolvePureCallTable, SolvePureCallTableBuilder,
    SolvePureCallWireError, SolvePureCallWireScope, SolveRecursionProfile, SolveRecursiveGroup,
    SolveRecursiveMember, SolveScalarBoundary, SolveScalarBoundaryError, SolveScalarCrossing,
    SolveScalarInput, SolveScalarOutput,
};
pub use demand::{SolveLiveness, SolveOutputDemand, visit_demanded_calls, visit_demanded_reads};
pub use entry::{
    EntryCallCatalog, EntryCallOccurrence, EntryCallOccurrenceId, EntryCallOccurrences,
    EntryCallPartition, EntryCallTarget, EntryPartitionCall, SharedEntryCallPlan,
    SharedHelperProgramEntry, TypedProgramEntry,
};
pub use integer_interval::{
    SolveIntegerInterval, fold_integer_compare, fold_scalar_binary, fold_scalar_unary,
};
pub use pass_independence::{IndependentPasses, LaneExactness, SolveIndexForm};
pub use program::parameter_stage::{
    SolveParameterCallStep, SolveParameterStageCandidate, SolveParameterStageError,
    SolveParameterStageValue,
};
pub use program::print_plan::{SolveElementAxis, SolveTensorKernel, SolveTensorPart};
pub use program::{
    MAX_UNROLLED_OPERATIONS, MAX_UNROLLED_PASSES, ProgramAssertion, ProgramAssignedState,
    ProgramCall, ProgramRegister, ProgramSlot, ProgramTensorViewAxis, SOLVE_STATIC_MATRIX_WORK,
    SolveAggregateBinaryDirectionalSource, SolveAggregateDirectionalColumns,
    SolveAggregateUnaryDirectionalSource, SolveAliasPlan, SolveAssertionForwarding,
    SolveAssertionMessage, SolveAssignedConditional, SolveAssignedStateId,
    SolveAssignedStateOperation, SolveBinaryDirectionalSource, SolveBinaryOperator,
    SolveBinaryToken, SolveBodyItem, SolveCheckedToken, SolveCompareOperator,
    SolveConversionOperator, SolveConversionToken, SolveDomainCheck, SolveElementOperand,
    SolveElementToken, SolveFiniteCheck, SolveFrameCells, SolveFrameItem, SolveFusedRegister,
    SolveFusionPlan, SolveIntegerLiteral, SolveKernel, SolveLiteral, SolveName, SolveNameSpace,
    SolveOperation, SolveOutputCopy, SolvePrintPlan, SolveProgramConstructionError,
    SolveProgramRegion, SolveReductionOperator, SolveRegisterFrame, SolveRegisterHome,
    SolveRegisterId, SolveRegisterPlacement, SolveScalarLifetime, SolveSlot, SolveSlotAccess,
    SolveSlotId, SolveSlotPlacement, SolveSpannedOperation, SolveStorageClass, SolveStoragePlan,
    SolveTensorViewAxis, SolveUnaryDirectionalSource, SolveUnaryOperator, SolveUnaryToken,
    TypedProgram, TypedProgramBuilder, index_forms, invariant_guard,
    promoted_concatenate_dimensions,
};
pub use types::{
    SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
    SolveTypeConstructionError, SolveValue, SolveValueKind, SolveValueType,
};

pub use target_specialization::{
    SolveMaterializedJacobian, SolveSpecializationIdentity, SolveTargetLayout, SolveTargetProfile,
    SolveTargetProfileError, SolveTargetSelection,
};
