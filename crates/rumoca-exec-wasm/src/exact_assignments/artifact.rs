//! Mechanical persisted form of the original emitted assignment module.
use super::*;
use serde::{Deserialize, Serialize};
#[cfg(all(test, feature = "host-engine"))]
mod tests;

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PrecompiledExactAssignments {
    version: u32,
    source_key: String,
    payload: crate::program_payload::ProgramPayload,
    y: usize,
    p: usize,
    gather: Vec<crate::NativeGatherFault>,
    linear: Vec<crate::NativeLinearSolveFault>,
}

impl PrecompiledExactAssignments {
    pub fn from_compiled(program: &CompiledExactAssignmentWasm) -> Self {
        Self {
            version: 1,
            source_key: program.source_key.clone(),
            payload: program.artifact.clone(),
            y: program.y_count,
            p: program.p_count,
            gather: program.gather_faults.clone(),
            linear: program.linear_solve_faults.clone(),
        }
    }

    pub fn bind(
        &self,
        key: &rumoca_ir_solve::PreparedKernelSource,
        source: &ComputeBlock,
        layout: &VarLayout,
    ) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
        let expected = key
            .exact_assignments(source)
            .map_err(WasmCompileError::Input)?;
        self.bind_expected(&expected, layout)
    }

    /// Bind captured issued rows to the same issued rows of the current build.
    pub fn bind_issued(
        &self,
        key: &rumoca_ir_solve::PreparedKernelSource,
        rows: &IssuedAssignmentRows,
        layout: &VarLayout,
    ) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
        let expected = key
            .issued_assignment_rows(rows)
            .map_err(WasmCompileError::Input)?;
        self.bind_expected(&expected, layout)
    }

    fn bind_expected(
        &self,
        expected: &str,
        layout: &VarLayout,
    ) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
        if self.version != 1
            || self.source_key != expected
            || self.y != layout.y_scalars()
            || self.p != layout.p_scalars()
            || self.payload.module_bytes().len() > (8 << 20)
            || self.payload.scratch_bytes()
                > crate::CheckedNativeWasmCapacityPolicy::default().public_scratch_bytes()
        {
            return Err(WasmCompileError::Input(
                "precompiled assignments belong to another checked source or extent".into(),
            ));
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime =
            crate::WasmKernelRuntime::new_named(self.payload.module_bytes(), "eval_assignments")?;
        Ok(CompiledExactAssignmentWasm {
            artifact: self.payload.clone(),
            gather_faults: self.gather.clone(),
            linear_solve_faults: self.linear.clone(),
            source_key: self.source_key.clone(),
            y_count: self.y,
            p_count: self.p,
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            runtime,
        })
    }
}
