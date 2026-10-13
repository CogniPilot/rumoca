//! Construction-issued ordered exact assignment execution.
pub(crate) mod plan;
pub(crate) mod profile;
use crate::{TypedCallFault, WasmCompileError};
use rumoca_ir_solve::{
    ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule, SolvePureCallTable,
    VarLayout,
};

pub struct CompiledExactAssignmentWasm {
    artifact: crate::CompiledNativeCallProgramWasm,
    y_count: usize,
    p_count: usize,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
}

impl CompiledExactAssignmentWasm {
    pub fn module_bytes(&self) -> &[u8] {
        self.artifact.module_bytes()
    }
    pub fn scratch_bytes(&self) -> u32 {
        self.artifact.scratch_bytes()
    }
    pub fn faults(&self) -> &[TypedCallFault] {
        self.artifact.faults()
    }
    pub fn gather_faults(&self) -> &[crate::NativeGatherFault] {
        self.artifact.gather_faults()
    }

    /// Direct schedule semantics: each complete source tuple commits in order.
    /// If a later source program faults, earlier complete commits remain in Y.
    /// The caller owns any whole-refresh rollback; there is no interpreter retry.
    pub fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        if y.len() != self.y_count
            || p.len() != self.p_count
            || scratch
                .len()
                .checked_mul(8)
                .is_none_or(|n| n < self.scratch_bytes() as usize)
        {
            return Err(WasmCompileError::Input(
                "exact schedule input lengths differ from issued layout".into(),
            ));
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let status = self.runtime.call_assignments(y, p, time, scratch)?;
            if status != 0 {
                return Err(self.artifact.status_error(status));
            }
            Ok(())
        }
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        {
            let _ = (y, p, time, scratch);
            Err(WasmCompileError::Input(
                "compiled WASM schedules have no admitted engine on this target".into(),
            ))
        }
    }
}

pub fn compile_exact_assignment_schedule_wasm(
    source: &ComputeBlock,
    owners: &ContinuousRefreshOwners,
    schedule: &ExactRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &SolvePureCallTable,
) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
    let artifact =
        crate::emit::emit_exact_assignment_module(source, owners, schedule, layout, table)
            .map_err(WasmCompileError::Backend)?;
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    let runtime = crate::WasmKernelRuntime::new_with_arena(
        artifact.module_bytes(),
        "eval_assignments",
        artifact.pooled_arena_bytes,
    )?;
    Ok(CompiledExactAssignmentWasm {
        artifact,
        y_count: layout.y_scalars(),
        p_count: layout.p_scalars(),
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        runtime,
    })
}
