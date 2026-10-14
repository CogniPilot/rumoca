//! Execute the emitted checked typed entry with its original fault inventory.
use super::*;

pub struct PreparedTypedCallWasm {
    artifact: CompiledTypedCallWasm,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
}

impl PreparedTypedCallWasm {
    pub fn compile(
        table: &solve::SolvePureCallTable,
        site: &solve::SolvePureCallSite,
    ) -> Result<Self, crate::WasmCompileError> {
        let artifact =
            compile_pure_call_wasm(table, site).map_err(crate::WasmCompileError::TypedAdmission)?;
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime =
            crate::WasmKernelRuntime::new_named(artifact.module_bytes(), "eval_typed_call")?;
        Ok(Self {
            artifact,
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            runtime,
        })
    }

    pub fn artifact(&self) -> &CompiledTypedCallWasm {
        &self.artifact
    }

    pub fn call(
        &self,
        input: &[u64],
        output: &mut [u64],
        scratch: &mut [u64],
    ) -> Result<(), crate::WasmCompileError> {
        let layout = self.artifact.layout();
        if input.len().checked_mul(8) != Some(layout.input_bytes as usize)
            || output.len().checked_mul(8) != Some(layout.output_bytes as usize)
            || scratch.len().checked_mul(8) != Some(layout.scratch_bytes as usize)
        {
            return Err(crate::WasmCompileError::Input(
                "typed invocation buffer footprint differs".into(),
            ));
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let status = self.runtime.call_typed(input, output, scratch)?;
            if status == 0 {
                return Ok(());
            }
            let fault = self
                .artifact
                .faults()
                .iter()
                .find(|fault| fault.status == status)
                .ok_or_else(|| {
                    crate::WasmCompileError::Backend(format!(
                        "unknown typed invocation status {status}"
                    ))
                })?;
            Err(crate::WasmCompileError::TypedSource(fault.clone()))
        }
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        Err(crate::WasmCompileError::Input(
            "compiled typed calls have no admitted engine on this target".into(),
        ))
    }
}
