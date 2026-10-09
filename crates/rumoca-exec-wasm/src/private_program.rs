//! Execute one complete checked value program without publishing solver storage.
use crate::{CompiledNativeCallProgramWasm, TypedCallFault, WasmCompileError};
use rumoca_ir_solve::{ScalarProgramBlock, SolvePureCallTable, VarLayout};

pub struct CompiledPrivateProgramWasm {
    artifact: CompiledNativeCallProgramWasm,
    y: usize,
    p: usize,
    outputs: usize,
    #[cfg(target_arch = "wasm32")]
    runtime: crate::WasmKernelRuntime,
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
        #[cfg(target_arch = "wasm32")]
        {
            let status = self.runtime.call_private(y, p, time, scratch)?;
            if status != 0 {
                return Err(self.artifact.status_error(status));
            }
            Ok(())
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            let _ = (y, p, time, scratch);
            Err(WasmCompileError::Input(
                "compiled private programs execute only on wasm32".into(),
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
    #[cfg(target_arch = "wasm32")]
    let runtime = crate::WasmKernelRuntime::new_with_arena(
        artifact.module_bytes(),
        "eval_private",
        artifact.pooled_arena_bytes,
    )?;
    Ok(CompiledPrivateProgramWasm {
        artifact,
        y: layout.y_scalars(),
        p: layout.p_scalars(),
        outputs: block.stored_output_count(),
        #[cfg(target_arch = "wasm32")]
        runtime,
    })
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
