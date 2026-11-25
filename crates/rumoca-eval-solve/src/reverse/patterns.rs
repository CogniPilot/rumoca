//! Closed groups used only where an entire family shares a handler. The owner
//! of each family matches its individual operations. Omitting an instruction
//! from its owning tape or pullback match is a Rust exhaustiveness error.

macro_rules! scalar_ops {
    () => {
        LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadIndexedRegister { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::LoadIndexedP { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
    };
}

macro_rules! tensor_ops {
    () => {
        LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorIntegerPower { .. }
            | LinearOp::TensorCross { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorUpdate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. }
            | LinearOp::TensorLoad { .. }
    };
}

macro_rules! region_ops {
    () => {
        LinearOp::FunctionFold { .. }
            | LinearOp::GuardedFunctionFold { .. }
            | LinearOp::FunctionConditional { .. }
    };
}

macro_rules! region_local_ops {
    () => {
        LinearOp::LoadIndexedFoldCarried { .. }
            | LinearOp::LoadIndexedFoldCapture { .. }
            | LinearOp::LoadFoldCarried { .. }
            | LinearOp::LoadFoldIndex { .. }
            | LinearOp::LoadFoldCapture { .. }
            | LinearOp::LoadFunctionConditionalCapture { .. }
            | LinearOp::LoadFunctionConditionalCaptureRange { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputFunctionFold { .. }
    };
}

macro_rules! outside_first_order {
    () => {
        LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }
            | LinearOp::PureCallObservation { .. }
            | LinearOp::PureCallDirectional { .. }
    };
}

macro_rules! non_tensor_ops {
    () => {
        scalar_ops!()
            | region_ops!()
            | region_local_ops!()
            | outside_first_order!()
            | LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::PureCall { .. }
    };
}
