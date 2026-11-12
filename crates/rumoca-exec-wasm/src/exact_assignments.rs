//! Construction-issued ordered exact assignment execution.
#[cfg(feature = "precompiled-programs")]
mod artifact;
pub(crate) mod plan;
pub(crate) mod profile;
use crate::{TypedCallFault, WasmCompileError};
#[cfg(feature = "precompiled-programs")]
pub use artifact::PrecompiledExactAssignments;
use rumoca_ir_solve::{
    ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule, IssuedAssignmentRows,
    SolvePureCallTable, VarLayout,
};

pub struct CompiledExactAssignmentWasm {
    artifact: crate::program_payload::ProgramPayload,
    gather_faults: Vec<crate::NativeGatherFault>,
    #[cfg(any(
        feature = "precompiled-programs",
        all(target_arch = "wasm32", target_os = "unknown")
    ))]
    linear_solve_faults: Vec<crate::NativeLinearSolveFault>,
    #[cfg(feature = "precompiled-programs")]
    source_key: String,
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
        &self.gather_faults
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
                return Err(self.artifact.entry_status_error(
                    status,
                    &self.gather_faults,
                    &self.linear_solve_faults,
                ));
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
    CompiledExactAssignmentWasm::from_artifact(artifact, layout, || {
        rumoca_ir_solve::PreparedKernelSource::new(layout, table)
            .and_then(|source_key| source_key.exact_assignment_schedule(source, owners, schedule))
    })
}

/// Issued ordered rows as one schedule call: each row commits its outputs to
/// its slot targets before the next row reads them, in issued order, with one
/// function per issued chunk. `layout` holds the issued slots as its Y and the
/// model's P; seeded rows read their seed from the issued seed slots.
pub fn compile_issued_assignment_rows_wasm(
    rows: &IssuedAssignmentRows,
    layout: &VarLayout,
    table: &SolvePureCallTable,
) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
    let artifact = crate::emit::emit_issued_assignment_module(rows, layout, table)
        .map_err(WasmCompileError::Backend)?;
    CompiledExactAssignmentWasm::from_artifact(artifact, layout, || {
        rumoca_ir_solve::PreparedKernelSource::new(layout, table)
            .and_then(|source_key| source_key.issued_assignment_rows(rows))
    })
}

impl CompiledExactAssignmentWasm {
    fn from_artifact(
        artifact: crate::native_program::CompiledNativeCallProgramWasm,
        layout: &VarLayout,
        source_key: impl FnOnce() -> Result<String, String>,
    ) -> Result<Self, WasmCompileError> {
        #[cfg(not(feature = "precompiled-programs"))]
        let _ = source_key;
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime =
            crate::WasmKernelRuntime::new_named(artifact.module_bytes(), "eval_assignments")?;
        Ok(Self {
            gather_faults: artifact.gather_faults.clone(),
            #[cfg(any(
                feature = "precompiled-programs",
                all(target_arch = "wasm32", target_os = "unknown")
            ))]
            linear_solve_faults: artifact.linear_solve_faults.clone(),
            artifact: artifact.into(),
            #[cfg(feature = "precompiled-programs")]
            source_key: source_key().map_err(WasmCompileError::Input)?,
            y_count: layout.y_scalars(),
            p_count: layout.p_scalars(),
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            runtime,
        })
    }
}
