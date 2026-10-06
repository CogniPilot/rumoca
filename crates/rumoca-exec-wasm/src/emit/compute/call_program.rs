//! Checked call helpers share one status-returning, atomic assignment entrypoint.
mod buffers;
mod groups;
pub(in crate::emit) mod layout;
mod memo;
mod transfer;
use super::*;
use crate::native_program::CompiledNativeCallProgramWasm;
use crate::typed_call::program::ProgramHelpers;
use layout::CallProgramPlan;
use rumoca_ir_solve as solve;

pub(crate) fn emit_native_call_assignment_module(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_with_budget(schedule, layout, table, groups::FUNCTION_BYTES)
}

fn emit_with_budget(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    budget: usize,
) -> Result<CompiledNativeCallProgramWasm, String> {
    layout
        .validate_shape_contract()
        .map_err(|e| e.to_string())?;
    assignments::validate_targets(schedule, layout.y_scalars())?;
    let plans = assignments::checked_plans(schedule, layout)?;
    let programs = plans
        .iter()
        .flat_map(|plan| plan.programs.iter().cloned())
        .collect::<Vec<_>>();
    let sites = conditional::call_sites(&programs)?;
    if sites.is_empty() && !has_checked_model_operations(programs.iter().flatten()) {
        return Err("native checked program has no checked operations".into());
    }
    let mut helpers = ProgramHelpers::new(table, &sites).map_err(|e| e.to_string())?;
    let imports = collect_imports(&programs)?
        .into_iter()
        .chain(helpers.math_imports().iter().copied())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    helpers.relocate(&imports).map_err(|e| e.to_string())?;
    let (bodies, faults) = helpers.bodies(table).map_err(|e| e.to_string())?;
    let rank = plans.iter().map(|plan| plan.rank).max().unwrap_or(0);
    let arena =
        arena::ArenaPlan::new(&programs, rank)?.ok_or("typed calls require private registers")?;
    let call_plan =
        CallProgramPlan::new(&programs, &helpers, layout.y_scalars(), arena.inner_counter)?;
    let mut module = Module::new();
    let types = add_types(&mut module);
    let mut catalog = add_import_section(&mut module, &imports, &types);
    catalog.eval_function_index = catalog
        .eval_function_index
        .checked_add(u32::try_from(bodies.len()).map_err(|_| "native helper count overflow")?)
        .ok_or("native helper index overflow")?;
    let first_group = catalog.eval_function_index;
    let status_base = u32::try_from(faults.len())
        .ok()
        .and_then(|n| n.checked_add(3))
        .ok_or("native gather status overflow")?;
    let mut gather_faults = groups::GatherFaults {
        status_base,
        entries: Vec::new(),
    };
    let outlined = groups::outline(
        schedule,
        &plans,
        &catalog,
        arena,
        &call_plan,
        budget,
        &mut gather_faults,
    )?;
    catalog.eval_function_index = first_group
        .checked_add(u32::try_from(outlined.len()).map_err(|_| "native group count overflow")?)
        .ok_or("native entrypoint index overflow")?;
    let mut functions = FunctionSection::new();
    for _ in &bodies {
        functions.function(3);
    }
    for _ in &outlined {
        functions.function(types.eval_type);
    }
    functions.function(types.eval_type);
    module.section(&functions);
    arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export(
        "eval_assignments",
        ExportKind::Func,
        catalog.eval_function_index,
    );
    module.section(&exports);
    let mut function = Function::new(groups::locals(arena));
    let mut emitter = BodyEmitter::new(&catalog, &mut function);
    emitter.arena = Some(arena);
    emitter.calls = Some(&call_plan);
    emitter.begin_call_program(layout)?;
    groups::invoke(&mut emitter, first_group, outlined.len())?;
    emitter.finish_call_program();
    function.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    for body in bodies {
        groups::check_size(&body, groups::FUNCTION_BYTES, "native typed helper")?;
        code.function(&body);
    }
    for body in outlined {
        code.function(&body);
    }
    groups::check_size(&function, budget, "native assignment entrypoint")?;
    code.function(&function);
    module.section(&code);
    Ok(CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        scratch_bytes: call_plan.bytes,
        faults,
        gather_faults: gather_faults.entries,
        math_imports: imports.iter().map(|import| import.symbol()).collect(),
        pooled_arena_bytes: None,
    })
}

fn has_checked_model_operations<'a>(mut operations: impl Iterator<Item = &'a LinearOp>) -> bool {
    operations.any(|op| {
        matches!(
            op,
            LinearOp::LoadIndexedRegister { .. } | LinearOp::FunctionConditional { .. }
        )
    })
}

pub(super) fn add_types(module: &mut Module) -> TypeIds {
    let mut types = TypeSection::new();
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
        ],
        [],
    );
    types.ty().function([ValType::F64], [ValType::F64]);
    types.ty().function([ValType::F64; 2], [ValType::F64]);
    types.ty().function([ValType::I32; 3], [ValType::I32]);
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
        ],
        [ValType::I32],
    );
    module.section(&types);
    TypeIds {
        eval_type: 4,
        unary_type: 1,
        binary_type: 2,
    }
}
