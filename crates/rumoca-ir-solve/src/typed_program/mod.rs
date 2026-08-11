//! Typed, storage-neutral executable Solve vocabulary.

mod call;
mod effect;
mod method;
mod program;
mod types;

pub use call::{
    SolvePureCallDirectionalOwner, SolvePureCallDirectionalSite, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallOutputKind, SolvePureCallOwner, SolvePureCallOwnerId,
    SolvePureCallSite, SolvePureCallTable, SolvePureCallTableBuilder,
};
pub use effect::{
    SolveEffectConstructionError, SolvePredefinedSignal, SolveSignal, SolveSignalSet,
    SolveUserSignal, SolveValueRange,
};
pub use method::{
    MethodCell, MethodClosure, SolveAction, SolveActionBlock, SolveActionConstructionError,
    SolveBranchCondition, SolveBranchConditionSpec, SolveCallAbiPlan, SolveCell, SolveCellId,
    SolveLimitTarget, SolveLimitTargetSpec, SolveLocalDeclaration, SolveMethod, SolveMethodBinding,
    SolveMethodBuilder, SolveMethodCells, SolveMethodId, SolveMethodInterface, SolveMethodKind,
    SolveMethodTable, SolveMethodTableBuilder, SolveParameterPassing, SolveResultPassing,
    SolveScope, SolveScopeId, SolveSignalCheck, SolveSignalClosure, SolveSignalClosureId,
    SolveSignalTest, SolveSpannedAction, SolveStatusPassing, SolveValueProgram,
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
