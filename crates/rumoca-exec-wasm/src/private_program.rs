//! Execute one complete checked value program without publishing solver storage.
use crate::{CompiledNativeCallProgramWasm, TypedCallFault, WasmCompileError};
use rumoca_ir_solve::{ScalarProgramBlock, SolvePureCallTable, VarLayout};
mod directional;
pub use directional::compile_private_directional_program_wasm;

pub struct CompiledPrivateProgramWasm {
    artifact: CompiledNativeCallProgramWasm,
    y: usize,
    p: usize,
    outputs: usize,
    seed: Option<directional::SeedStorage>,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
    #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
    runtime: crate::host_runtime::HostKernelRuntime,
}

impl CompiledPrivateProgramWasm {
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
    pub const fn output_count(&self) -> usize {
        self.outputs
    }
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.call_with_seed(y, p, time, None, scratch)
    }

    pub fn call_directional(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        seed: &[f64],
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        self.call_with_seed(y, p, time, Some(seed), scratch)
    }

    fn call_with_seed(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        seed: Option<&[f64]>,
        scratch: &mut [f64],
    ) -> Result<(), WasmCompileError> {
        if y.len() != self.y
            || p.len() != self.p
            || scratch
                .len()
                .checked_mul(8)
                .is_none_or(|n| n < self.scratch_bytes() as usize)
        {
            return Err(WasmCompileError::Input(
                "private program lengths differ from issued layout".into(),
            ));
        }
        match (self.seed, seed) {
            (Some(storage), Some(seed)) if seed.len() >= storage.count => {
                scratch[storage.offset..storage.offset + storage.count]
                    .copy_from_slice(&seed[..storage.count]);
            }
            (None, None) => {}
            _ => {
                return Err(WasmCompileError::Input(
                    "private program seed differs from its issued coordinate storage".into(),
                ));
            }
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let status = self.runtime.call_private(y, p, time, scratch)?;
            if status != 0 {
                return Err(self.artifact.status_error(status));
            }
            Ok(())
        }
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        {
            let mut cells = scratch
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>();
            let status = self
                .runtime
                .call_native(&mut y.to_vec(), p, time, &mut cells, &mut [])?;
            if status != 0 {
                return Err(self.artifact.status_error(status));
            }
            for (value, bits) in scratch.iter_mut().zip(cells) {
                *value = f64::from_bits(bits);
            }
            Ok(())
        }
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        )))]
        {
            let _ = (y, p, time, scratch);
            Err(WasmCompileError::Input(
                "compiled private programs have no admitted engine on this target".into(),
            ))
        }
    }
}

pub fn compile_private_program_wasm(
    block: &ScalarProgramBlock,
    layout: &VarLayout,
    table: &SolvePureCallTable,
) -> Result<CompiledPrivateProgramWasm, WasmCompileError> {
    layout
        .validate_shape_contract()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    let artifact = crate::emit::emit_private_program_module(block, layout, table)
        .map_err(WasmCompileError::Backend)?;
    CompiledPrivateProgramWasm::from_artifact(artifact, block, layout, None)
}

impl CompiledPrivateProgramWasm {
    fn from_artifact(
        artifact: CompiledNativeCallProgramWasm,
        block: &ScalarProgramBlock,
        layout: &VarLayout,
        seed: Option<directional::SeedStorage>,
    ) -> Result<Self, WasmCompileError> {
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime = crate::WasmKernelRuntime::new_with_arena(
            artifact.module_bytes(),
            "eval_private",
            artifact.pooled_arena_bytes,
        )?;
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        let runtime = crate::host_runtime::HostKernelRuntime::new_named(
            artifact.module_bytes(),
            "eval_private",
        )?;
        Ok(CompiledPrivateProgramWasm {
            artifact,
            y: layout.y_scalars(),
            p: layout.p_scalars(),
            outputs: block.stored_output_count(),
            seed,
            #[cfg(any(
                all(target_arch = "wasm32", target_os = "unknown"),
                all(feature = "host-engine", not(target_arch = "wasm32"))
            ))]
            runtime,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{LinearOp, SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat};

    #[test]
    fn the_private_tuple_holds_every_stored_output_of_the_block() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("private_program.mo"),
            0,
            1,
        );
        let program = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::Const { dst: 2, value: 2. },
            LinearOp::StoreOutput { src: 2 },
        ];
        let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span]).unwrap();
        let layout = VarLayout::from_parts(Default::default(), 1, 1);
        let calls = SolvePureCallTable::builder(SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::FULL,
        ))
        .finish();
        let compiled = compile_private_program_wasm(&block, &layout, &calls).unwrap();
        assert_eq!(compiled.output_count(), block.stored_output_count());
        assert_eq!(compiled.output_count(), 3);
    }
}
