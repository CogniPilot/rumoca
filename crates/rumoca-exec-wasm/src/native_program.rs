//! Portable status-returning native assignments with checked typed call linkage.
use crate::{TypedCallFault, WasmCompileError};
use rumoca_ir_solve::{NativeRefreshAssignmentSchedule, SolvePureCallTable, VarLayout};

/// A checked address failure in a model scalar program, without a function owner.
/// Kernel/program ordinals refer to the emitted construction-issued value schedule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeGatherFault {
    pub status: u32,
    pub kind: crate::TypedCallFaultKind,
    pub kernel: usize,
    pub program: usize,
    pub operation: usize,
    /// Enclosing operation and ordered condition/result/fallback region ordinal.
    pub region_path: Vec<(usize, usize)>,
    pub provenance: rumoca_core::Span,
}

#[derive(Debug)]
pub struct CompiledNativeCallProgramWasm {
    pub(crate) bytes: Vec<u8>,
    pub(crate) scratch_bytes: u32,
    pub(crate) scratch_report: crate::ScratchReport,
    pub(crate) faults: Vec<TypedCallFault>,
    pub(crate) gather_faults: Vec<NativeGatherFault>,
    pub(crate) math_imports: Vec<&'static str>,
    pub(crate) pooled_arena_bytes: Option<u32>,
}

impl CompiledNativeCallProgramWasm {
    pub fn module_bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub fn scratch_bytes(&self) -> u32 {
        self.scratch_bytes
    }
    /// Per-owner frame, region and call-site scratch high-water marks.
    pub fn scratch_report(&self) -> &crate::ScratchReport {
        &self.scratch_report
    }
    /// Status 1 denotes invalid whole-program buffers; 2 invalid scalar-to-typed input.
    /// Higher statuses retain exact call-owner or model-program provenance.
    /// Model address faults are exposed by `gather_faults`.
    pub fn faults(&self) -> &[TypedCallFault] {
        &self.faults
    }
    pub fn gather_faults(&self) -> &[NativeGatherFault] {
        &self.gather_faults
    }
    #[cfg(target_arch = "wasm32")]
    pub(crate) fn status_error(&self, status: u32) -> WasmCompileError {
        if let Some(fault) = self.faults().iter().find(|fault| fault.status == status) {
            return WasmCompileError::TypedSource(fault.clone());
        }
        if let Some(fault) = self
            .gather_faults()
            .iter()
            .find(|fault| fault.status == status)
        {
            return WasmCompileError::GatherSource(fault.clone());
        }
        WasmCompileError::Backend(format!("native entry failure status {status}"))
    }

    pub fn math_imports(&self) -> &[&'static str] {
        &self.math_imports
    }
    /// Internal browser executors may import a disjoint private arena region.
    /// Standalone portable native programs keep their defined-memory ABI.
    pub const fn pooled_arena_bytes(&self) -> Option<u32> {
        self.pooled_arena_bytes
    }
}

/// Execute the construction-issued schedule, linking the model's exact call table.
/// ABI: eval_assignments(yPtr,pPtr,time,scratchPtr,reservedZero)->status.
/// Aligned Y/P/scratch spans must be disjoint and fully in unshared env.memory.
/// The complete Y tuple is published only on status zero; P is never changed.
pub fn compile_native_assignment_schedule_with_calls_wasm(
    schedule: &NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    calls: &SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, WasmCompileError> {
    crate::emit::emit_native_call_assignment_module(schedule, layout, calls)
        .map_err(WasmCompileError::Backend)
}
