//! Emit each complete issued tuple before committing its ordered Y targets.
mod ready;
#[cfg(test)]
mod tests;

use super::*;
use crate::exact_assignments::plan::Plan;
use crate::native_program::CompiledNativeCallProgramWasm;
use crate::typed_call::program::ProgramHelpers;
use call_program::layout::CallProgramPlan;
use ready::Ready;
use rumoca_ir_solve as solve;

pub(crate) fn emit_exact_assignment_module(
    source: &solve::ComputeBlock,
    owners: &solve::ContinuousRefreshOwners,
    schedule: &solve::ExactRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_module(
        Ready::new(source, owners, schedule, layout, table)?,
        layout,
        false,
    )
}

pub(crate) fn emit_private_program_module(
    block: &solve::ScalarProgramBlock,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_module(Ready::private(block, layout, table)?, layout, true)
}

fn emit_module(
    ready: Ready,
    layout: &VarLayout,
    private: bool,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_module_with_storage(ready, layout, private, arena::ArenaStorage::runtime())
}

fn emit_module_with_storage(
    mut ready: Ready,
    layout: &VarLayout,
    private: bool,
    storage: arena::ArenaStorage,
) -> Result<CompiledNativeCallProgramWasm, String> {
    ready.arena = ready.arena.with_storage(storage);
    let mut module = Module::new();
    let types = call_program::add_types(&mut module);
    let mut catalog =
        add_import_section_with_arena(&mut module, &ready.imports, &types, Some(ready.arena));
    catalog.eval_function_index = catalog
        .eval_function_index
        .checked_add(
            u32::try_from(ready.bodies.len()).map_err(|_| "WASM exact helper count overflow")?,
        )
        .ok_or("WASM exact entry index overflow")?;
    let mut functions = FunctionSection::new();
    for _ in &ready.bodies {
        functions.function(3);
    }
    functions.function(types.eval_type);
    module.section(&functions);
    ready.arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export(
        if private {
            "eval_private"
        } else {
            "eval_assignments"
        },
        ExportKind::Func,
        catalog.eval_function_index,
    );
    module.section(&exports);
    let (function, gather_faults) = emit_function(&catalog, &ready, layout, private)?;
    let mut code = CodeSection::new();
    for body in &ready.bodies {
        code.function(body);
    }
    code.function(&function);
    module.section(&code);
    Ok(CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        scratch_bytes: ready.calls.bytes,
        scratch_report: ready.scratch_report,
        faults: ready.faults,
        gather_faults,
        math_imports: ready.imports.iter().map(|import| import.symbol()).collect(),
        pooled_arena_bytes: ready.arena.pooled_bytes(),
    })
}

fn emit_function(
    catalog: &ImportCatalog,
    ready: &Ready,
    layout: &VarLayout,
    private: bool,
) -> Result<(Function, Vec<crate::NativeGatherFault>), String> {
    let mut locals = ready.arena.locals();
    locals.extend([(2, ValType::I32), (1, ValType::I64)]);
    let mut function = Function::new(locals);
    let mut emitter = BodyEmitter::new(catalog, &mut function);
    emitter.arena = Some(ready.arena);
    emitter.calls = Some(&ready.calls);
    emitter.gather_status_base = Some(
        u32::try_from(ready.faults.len())
            .ok()
            .and_then(|n| n.checked_add(3))
            .ok_or("exact gather status overflow")?,
    );
    emitter.guard_arena_region()?;
    emitter.begin_exact_schedule(layout)?;
    for (kernel, program) in ready.plan.programs.iter().enumerate() {
        emitter.kernel_ordinal = kernel;
        emitter.push(Instruction::LocalGet(SEED_PTR_PARAM));
        emitter.push(Instruction::LocalSet(OUT_PTR_PARAM));
        let outputs = (0..program.block.stored_output_count()).collect::<Vec<_>>();
        emitter.emit_mapped_programs(&program.block, &outputs)?;
        if !private {
            emit_commit(&mut emitter, &program.targets)?;
        }
    }
    emitter.push(Instruction::I32Const(0));
    let gather_faults = std::mem::take(&mut emitter.gather_faults);
    function.instruction(&Instruction::End);
    Ok((function, gather_faults))
}

fn emit_commit(emitter: &mut BodyEmitter<'_>, targets: &[usize]) -> Result<(), String> {
    // The complete prefix and tuple have succeeded. All addresses were checked
    // at admission and entry, so the ordered commit has no intermediate faults.
    for (output, &target) in targets.iter().enumerate() {
        emitter.push(Instruction::LocalGet(Y_PTR_PARAM));
        emitter.push(Instruction::LocalGet(SEED_PTR_PARAM));
        emitter.push(Instruction::F64Load(memarg_for_index(output)?));
        emitter.push(Instruction::F64Store(memarg_for_index(target)?));
    }
    Ok(())
}
