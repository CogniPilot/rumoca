use super::LinearOp;

pub(super) fn program_is_repeatable(program: &[LinearOp]) -> bool {
    // Inspect executed operations, including discarded values. Output-only
    // dependence cannot certify that an unused random draw has no effect.
    program.iter().all(operation_is_repeatable)
}

fn operation_is_repeatable(op: &LinearOp) -> bool {
    match op {
        LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. } => false,
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            program_is_repeatable(&program.update)
        }
        LinearOp::FunctionConditional { program, .. } => {
            program.arms.iter().all(|arm| {
                program_is_repeatable(&arm.condition) && program_is_repeatable(&arm.result)
            }) && program_is_repeatable(&program.fallback)
        }
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadIndexedP { .. }
        | LinearOp::LoadIndexedRegister { .. }
        | LinearOp::LoadIndexedFoldCarried { .. }
        | LinearOp::LoadIndexedFoldCapture { .. }
        | LinearOp::LoadSeed { .. }
        | LinearOp::LoadIndexedSeed { .. }
        | LinearOp::LoadFoldCarried { .. }
        | LinearOp::LoadFoldIndex { .. }
        | LinearOp::LoadFoldCapture { .. }
        | LinearOp::LoadFunctionConditionalCapture { .. }
        | LinearOp::LoadFunctionConditionalCaptureRange { .. }
        | LinearOp::Move { .. }
        | LinearOp::LinearSolveComponent { .. }
        | LinearOp::DotProduct { .. }
        | LinearOp::MatrixMultiply { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorCross { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorUpdate { .. }
        | LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. }
        | LinearOp::TableBounds { .. }
        | LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. }
        | LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::Unary { .. }
        | LinearOp::Binary { .. }
        | LinearOp::Compare { .. }
        | LinearOp::Select { .. }
        | LinearOp::PureCall { .. }
        | LinearOp::PureCallDirectional { .. }
        | LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutput { .. } => true,
    }
}
