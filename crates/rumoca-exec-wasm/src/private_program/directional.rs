//! Compile the complete original directional source with private seed storage.
use super::*;

#[derive(Clone, Copy)]
pub(super) struct SeedStorage {
    pub offset: usize,
    pub count: usize,
}

pub fn compile_private_directional_program_wasm(
    block: &ScalarProgramBlock,
    layout: &VarLayout,
    table: &SolvePureCallTable,
    seed_len: usize,
) -> Result<CompiledPrivateProgramWasm, WasmCompileError> {
    layout
        .validate_shape_contract()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    let (artifact, seed_offset) =
        crate::emit::emit_private_directional_module(block, layout, table, seed_len)
            .map_err(WasmCompileError::Backend)?;
    CompiledPrivateProgramWasm::from_artifact(
        artifact,
        block,
        layout,
        Some(SeedStorage {
            offset: seed_offset as usize / 8,
            count: seed_len,
        }),
    )
}
