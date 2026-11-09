//! Compile the complete original directional source with private seed storage.
use super::*;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub(super) struct SeedStorage {
    pub offset: usize,
    pub count: usize,
    pub reads: Vec<std::ops::Range<usize>>,
}

pub fn compile_private_directional_program_wasm(
    block: &ScalarProgramBlock,
    layout: &VarLayout,
    table: &SolvePureCallTable,
    seed_len: usize,
) -> Result<CompiledPrivateProgramWasm, WasmCompileError> {
    single(compile_private_programs_wasm(
        &[(block, Some(seed_len))],
        layout,
        table,
    )?)
}
