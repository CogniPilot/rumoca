//! Typed, storage-neutral executable Solve vocabulary.

mod call;
mod program;
mod types;

pub use call::{
    SolvePureCallDirectionalOwner, SolvePureCallDirectionalSite, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwner, SolvePureCallOwnerId,
    SolvePureCallSite, SolvePureCallTable, SolvePureCallTableBuilder,
};
pub use program::{
    ProgramRegister, ProgramSlot, ProgramTensorViewAxis, SolveBinaryOperator, SolveCompareOperator,
    SolveConversionOperator, SolveOperation, SolveProgramConstructionError, SolveProgramRegion,
    SolveReductionOperator, SolveRegisterId, SolveSlot, SolveSlotAccess, SolveSlotId,
    SolveSpannedOperation, SolveStorageClass, SolveTensorViewAxis, SolveUnaryOperator,
    TypedProgram, TypedProgramBuilder, promoted_concatenate_dimensions,
};
pub use types::{
    SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
    SolveTypeConstructionError, SolveValue, SolveValueKind, SolveValueType,
};
