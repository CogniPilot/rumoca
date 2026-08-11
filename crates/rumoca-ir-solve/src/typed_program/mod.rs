//! Typed, storage-neutral executable Solve vocabulary.

mod call;
mod program;
mod types;

pub use call::{
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwner,
    SolvePureCallOwnerId, SolvePureCallSite, SolvePureCallTable, SolvePureCallTableBuilder,
};
pub use program::{
    ProgramRegister, ProgramSlot, ProgramTensorViewAxis, SolveBinaryOperator, SolveCompareOperator,
    SolveConversionOperator, SolveOperation, SolveProgramConstructionError, SolveProgramRegion,
    SolveReductionOperator, SolveRegisterId, SolveSlot, SolveSlotAccess, SolveSlotId,
    SolveSpannedOperation, SolveStorageClass, SolveTensorViewAxis, SolveUnaryOperator,
    TypedProgram, TypedProgramBuilder,
};
pub use types::{
    SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveRoundingMode,
    SolveScalarType, SolveTypeConstructionError, SolveValue, SolveValueKind, SolveValueType,
};
