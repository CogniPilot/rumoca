//! Typed, storage-neutral executable Solve vocabulary.

mod call;
mod demand;
mod integer_interval;
mod pass_independence;
mod program;
mod types;

pub(crate) use call::dependency::CheckedCallDependencyProjection;
pub use call::dependency::SolveCallDependency;
pub use call::{
    AssertionRegionKind, AssertionRegionStep, AssertionSource, CheckedAssertionFlow,
    ScopedSolvePureCallTable, SolveAssertionLevel, SolveCallSiteCount,
    SolvePureCallDirectionalOwner, SolvePureCallDirectionalSite, SolvePureCallIdentity,
    SolvePureCallInputCoordinate, SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwner,
    SolvePureCallOwnerId, SolvePureCallSite, SolvePureCallTable, SolvePureCallTableBuilder,
    SolvePureCallWireError, SolvePureCallWireScope, SolveRecursionProfile, SolveRecursiveGroup,
    SolveRecursiveMember,
};
pub use demand::{SolveLiveness, SolveOutputDemand, visit_demanded_calls, visit_demanded_reads};
pub use integer_interval::{
    SolveIntegerInterval, fold_integer_compare, fold_scalar_binary, fold_scalar_unary,
};
pub use pass_independence::{IndependentPasses, LaneExactness, SolveIndexForm};
pub use program::{
    MAX_UNROLLED_OPERATIONS, MAX_UNROLLED_PASSES, ProgramAssertion, ProgramAssignedState,
    ProgramCall, ProgramRegister, ProgramSlot, ProgramTensorViewAxis, SolveAssertionForwarding,
    SolveAssertionMessage, SolveAssignedConditional, SolveAssignedStateId,
    SolveAssignedStateOperation, SolveBinaryOperator, SolveCompareOperator,
    SolveConversionOperator, SolveOperation, SolveProgramConstructionError, SolveProgramRegion,
    SolveReductionOperator, SolveRegisterId, SolveSlot, SolveSlotAccess, SolveSlotId,
    SolveSpannedOperation, SolveStorageClass, SolveTensorViewAxis, SolveUnaryOperator,
    TypedProgram, TypedProgramBuilder, index_forms, invariant_guard,
    promoted_concatenate_dimensions,
};
pub use types::{
    SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
    SolveTypeConstructionError, SolveValue, SolveValueKind, SolveValueType,
};
