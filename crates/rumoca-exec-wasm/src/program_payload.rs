//! Mechanical transport of an already emitted core program. No scheduling policy.
use crate::{
    CompiledNativeCallProgramWasm, NativeGatherFault, NativeLinearSolveFault, TypedCallFault,
    WasmCompileError,
};

/// Mechanical runtime payload of the module already emitted for this batch.
/// Scheduling, storage, operation order and budgets remain with the issuer.
#[derive(Clone)]
#[cfg_attr(
    feature = "precompiled-programs",
    derive(serde::Serialize, serde::Deserialize)
)]
#[cfg_attr(feature = "precompiled-programs", serde(deny_unknown_fields))]
pub(crate) struct ProgramPayload {
    #[cfg_attr(feature = "precompiled-programs", serde(with = "serde_bytes"))]
    bytes: Vec<u8>,
    scratch_bytes: u32,
    faults: Vec<TypedCallFault>,
    gather: Vec<NativeGatherFault>,
    linear: Vec<NativeLinearSolveFault>,
}

impl From<CompiledNativeCallProgramWasm> for ProgramPayload {
    fn from(module: CompiledNativeCallProgramWasm) -> Self {
        Self {
            bytes: module.bytes,
            scratch_bytes: module.scratch_bytes,
            faults: module.faults,
            gather: module.gather_faults,
            linear: module.linear_solve_faults,
        }
    }
}

impl From<&CompiledNativeCallProgramWasm> for ProgramPayload {
    fn from(module: &CompiledNativeCallProgramWasm) -> Self {
        Self {
            bytes: module.bytes.clone(),
            scratch_bytes: module.scratch_bytes,
            faults: module.faults.clone(),
            gather: module.gather_faults.clone(),
            linear: module.linear_solve_faults.clone(),
        }
    }
}

impl ProgramPayload {
    pub(crate) fn module_bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub(crate) fn scratch_bytes(&self) -> u32 {
        self.scratch_bytes
    }
    pub(crate) fn faults(&self) -> &[TypedCallFault] {
        &self.faults
    }
    /// Every fault table the payload decodes a failure status with.
    #[cfg(feature = "precompiled-programs")]
    pub(crate) fn fault_tables(
        &self,
    ) -> (
        &[TypedCallFault],
        &[NativeGatherFault],
        &[NativeLinearSolveFault],
    ) {
        (&self.faults, &self.gather, &self.linear)
    }
    /// Every persisted field that affects code, storage or fault decoding.
    #[cfg(feature = "precompiled-programs")]
    pub(crate) fn same_emission(&self, issued: &Self) -> bool {
        self.bytes == issued.bytes
            && self.scratch_bytes == issued.scratch_bytes
            && self.fault_tables() == issued.fault_tables()
    }
    pub(crate) fn status_error(&self, status: u32) -> WasmCompileError {
        self.entry_status_error(status, &self.gather, &self.linear)
    }
    pub(crate) fn entry_status_error(
        &self,
        status: u32,
        gather: &[NativeGatherFault],
        linear: &[NativeLinearSolveFault],
    ) -> WasmCompileError {
        if let Some(fault) = self.faults.iter().find(|fault| fault.status == status) {
            return WasmCompileError::TypedSource(fault.clone());
        }
        if let Some(fault) = gather.iter().find(|fault| fault.status == status) {
            return WasmCompileError::GatherSource(fault.clone());
        }
        if let Some(fault) = linear.iter().find(|fault| fault.status == status) {
            return WasmCompileError::LinearSolveSource(fault.clone());
        }
        WasmCompileError::Backend(format!("native entry failure status {status}"))
    }
}
