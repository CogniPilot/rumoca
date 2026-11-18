//! Unactivated code product for one complete original causal FMI derivative.
#[cfg(feature = "precompiled-programs")]
mod artifact;
use crate::{CompiledNativeCallProgramWasm, WasmCompileError};
#[cfg(feature = "precompiled-programs")]
pub use artifact::{
    BoundFmiDerivativeOperation, FmiDerivativeCall, FmiDerivativeFailure, FmiKinematicCall,
    PrecompiledFmiDerivativeOperation,
};
use rumoca_ir_solve::{
    CheckedNativeWasmCapacityPolicy,
    fmi::{FmiDerivativeCausalOperation, FmiDerivativeWorkspace},
};

pub struct CompiledFmiDerivativeOperationWasm {
    #[cfg(feature = "precompiled-programs")]
    source_key: String,
    bytes: Vec<u8>,
    workspace: FmiDerivativeWorkspace,
    static_program: Option<CompiledNativeCallProgramWasm>,
    dynamic_program: Option<CompiledNativeCallProgramWasm>,
    values: CompiledNativeCallProgramWasm,
    kinematic: Option<CompiledNativeCallProgramWasm>,
}
impl CompiledFmiDerivativeOperationWasm {
    pub fn module_bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub fn workspace(&self) -> FmiDerivativeWorkspace {
        self.workspace
    }
    pub fn static_program(&self) -> Option<&CompiledNativeCallProgramWasm> {
        self.static_program.as_ref()
    }
    pub fn dynamic_program(&self) -> Option<&CompiledNativeCallProgramWasm> {
        self.dynamic_program.as_ref()
    }
    pub fn values(&self) -> &CompiledNativeCallProgramWasm {
        &self.values
    }
    pub fn kinematic(&self) -> Option<&CompiledNativeCallProgramWasm> {
        self.kinematic.as_ref()
    }
}

/// Emit code only. Runtime linking must retain the same original FMI owner;
/// this byte product does not admit an engine, foreign source or public call.
pub fn compile_fmi_derivative_operation_wasm(
    operation: &FmiDerivativeCausalOperation<'_>,
) -> Result<CompiledFmiDerivativeOperationWasm, WasmCompileError> {
    let source = operation.source();
    #[cfg(feature = "precompiled-programs")]
    let source_key =
        rumoca_ir_solve::PreparedKernelSource::new(&source.problem().layout, source.calls())
            .and_then(|key| key.fmi_derivative(operation))
            .map_err(WasmCompileError::Input)?;
    let problem = source.problem();
    let emit = |schedule| {
        crate::emit::emit_exact_assignment_module(
            &problem.continuous.implicit_rhs,
            &problem.continuous.refresh_owners,
            schedule,
            &problem.layout,
            source.calls(),
        )
        .map_err(WasmCompileError::Backend)
    };
    let static_program = operation.static_schedule().map(emit).transpose()?;
    let dynamic_program = operation.dynamic_schedule().map(emit).transpose()?;
    let values =
        crate::emit::emit_derivative_value_module(source).map_err(WasmCompileError::Backend)?;
    let kinematic = operation
        .kinematic()
        .map(|kinematic| crate::emit::emit_kinematic_value_module(source, kinematic))
        .transpose()
        .map_err(WasmCompileError::Backend)?;
    let control = operation
        .control_plan(
            static_program
                .iter()
                .chain(dynamic_program.iter())
                .chain(std::iter::once(&values))
                .chain(kinematic.iter())
                .map(|p| p.scratch_bytes()),
            CheckedNativeWasmCapacityPolicy::default(),
        )
        .map_err(|e| WasmCompileError::Input(e.to_string()))?;
    let workspace = control.workspace();
    let bytes = crate::emit::derivative_operation::emit(operation, &control);
    Ok(CompiledFmiDerivativeOperationWasm {
        #[cfg(feature = "precompiled-programs")]
        source_key,
        bytes,
        workspace,
        static_program,
        dynamic_program,
        values,
        kinematic,
    })
}
