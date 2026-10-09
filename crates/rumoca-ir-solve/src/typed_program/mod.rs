//! Typed, storage-neutral executable Solve vocabulary.

mod call;
mod program;
mod types;

pub(crate) use call::dependency::CheckedCallDependencyProjection;
pub use call::dependency::SolveCallDependency;
pub use call::{
    AssertionRegionKind, AssertionRegionStep, AssertionSource, CheckedAssertionFlow,
    SolveAssertionLevel, SolveCallSiteCount, SolvePureCallDirectionalOwner,
    SolvePureCallDirectionalSite, SolvePureCallIdentity, SolvePureCallInputCoordinate,
    SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwner, SolvePureCallOwnerId,
    SolvePureCallSite, SolvePureCallTable, SolvePureCallTableBuilder, SolveRecursionProfile,
    SolveRecursiveGroup, SolveRecursiveMember,
};
pub use program::{
    ProgramAssertion, ProgramCall, ProgramRegister, ProgramSlot, ProgramTensorViewAxis,
    SolveAssertionForwarding, SolveAssertionMessage, SolveBinaryOperator, SolveCompareOperator,
    SolveConversionOperator, SolveOperation, SolveProgramConstructionError, SolveProgramRegion,
    SolveReductionOperator, SolveRegisterId, SolveSlot, SolveSlotAccess, SolveSlotId,
    SolveSpannedOperation, SolveStorageClass, SolveTensorViewAxis, SolveUnaryOperator,
    TypedProgram, TypedProgramBuilder, promoted_concatenate_dimensions,
};
pub use types::{
    SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
    SolveTypeConstructionError, SolveValue, SolveValueKind, SolveValueType,
};
