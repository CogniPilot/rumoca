//! Checked call helpers share one status-returning, atomic assignment entrypoint.
mod buffers;
mod carried;
pub(in crate::emit) mod glue;
mod groups;
pub(in crate::emit) mod layout;
mod memo;
mod transfer;
use super::*;
use crate::native_program::CompiledNativeCallProgramWasm;
use crate::typed_call::program::{Demand, ProgramHelpers};
use layout::CallProgramPlan;
use rumoca_ir_solve as solve;

pub(crate) fn emit_native_call_assignment_module(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_with_budget(schedule, layout, table, groups::FUNCTION_BYTES)
}

pub(crate) fn emit_native_call_assignment_module_with_policy(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    policy: crate::CheckedNativeWasmCapacityPolicy,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_with_policy(schedule, layout, table, groups::FUNCTION_BYTES, policy)
}

fn emit_with_budget(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    budget: usize,
) -> Result<CompiledNativeCallProgramWasm, String> {
    emit_with_policy(
        schedule,
        layout,
        table,
        budget,
        crate::CheckedNativeWasmCapacityPolicy::default(),
    )
}

pub(crate) fn emit_with_policy(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
    table: &solve::SolvePureCallTable,
    budget: usize,
    policy: crate::CheckedNativeWasmCapacityPolicy,
) -> Result<CompiledNativeCallProgramWasm, String> {
    layout
        .validate_shape_contract()
        .map_err(|e| e.to_string())?;
    let work = work_layout(schedule, layout)?;
    assignments::validate_targets(schedule, work.y_scalars())?;
    let plans = assignments::checked_plans(schedule, work)?;
    let programs = plans
        .iter()
        .flat_map(|plan| plan.programs.iter().cloned())
        .collect::<Vec<_>>();
    let sites = conditional::call_sites(&programs)?;
    require_checked_entry(schedule, &sites, &programs)?;
    let mut helpers = link_helpers(table, schedule, &plans, &sites)?;
    let imports = linked_imports(&programs, &helpers)?;
    helpers.relocate(&imports).map_err(|e| e.to_string())?;
    let (bodies, faults) = helpers.bodies(table, budget).map_err(|e| e.to_string())?;
    let rank = plans.iter().map(|plan| plan.rank).max().unwrap_or(0);
    // The checked entry always evaluates stages in private registers.
    let arena = arena::ArenaPlan::required_with_policy(&programs, rank, policy)?;
    let call_plan = CallProgramPlan::for_schedule(
        &programs,
        &helpers,
        arena.last_integer_local(),
        schedule,
        layout,
        policy,
    )?;
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
        linear_solve: Vec::new(),
    };
    let outlined = groups::outline(
        schedule,
        &plans,
        &catalog,
        arena.clone(),
        &call_plan,
        budget,
        &mut gather_faults,
    )?;
    catalog.eval_function_index = first_group
        .checked_add(u32::try_from(outlined.len()).map_err(|_| "native group count overflow")?)
        .ok_or("native entrypoint index overflow")?;
    add_function_section(&mut module, bodies.len(), outlined.len(), types.eval_type);
    arena.add_memory(&mut module);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export(
        "eval_assignments",
        ExportKind::Func,
        catalog.eval_function_index + 1,
    );
    module.section(&exports);
    let mut function = Function::new(groups::locals(arena.clone()));
    let mut emitter = BodyEmitter::new(&catalog, &mut function);
    emitter.arena = Some(arena.clone());
    emitter.calls = Some(&call_plan);
    emitter.begin_call_program(schedule)?;
    groups::invoke(&mut emitter, first_group, outlined.len())?;
    emitter.finish_call_program()?;
    function.instruction(&Instruction::End);
    let mut entry = Function::new(groups::locals(arena.clone()));
    let mut emitter = BodyEmitter::new(&catalog, &mut entry);
    emitter.arena = Some(arena);
    emitter.calls = Some(&call_plan);
    emitter.emit_call_program_entry(layout, schedule, catalog.eval_function_index)?;
    entry.instruction(&Instruction::End);
    append_code(&mut module, bodies, outlined, [function, entry], budget)?;
    Ok(CompiledNativeCallProgramWasm {
        bytes: module.finish(),
        capacity_policy: policy,
        scratch_bytes: call_plan.view().bytes,
        scratch_report: call_plan.scratch_report(helpers.owner_scratch(table)),
        faults,
        gather_faults: gather_faults.entries,
        linear_solve_faults: gather_faults.linear_solve,
        math_imports: imports.iter().map(|import| import.symbol()).collect(),
        pooled_arena_bytes: None,
        carried: schedule.carried_state().clone(),
    })
}

fn add_function_section(module: &mut Module, helpers: usize, groups: usize, eval_type: u32) {
    let mut functions = FunctionSection::new();
    for _ in 0..helpers {
        functions.function(3);
    }
    for _ in 0..groups {
        functions.function(eval_type);
    }
    functions.function(eval_type);
    functions.function(eval_type);
    module.section(&functions);
}

/// Link the helpers each root owner of the program calls, built for the outputs
/// the stages read (SOLVE-C95).
fn link_helpers(
    table: &solve::SolvePureCallTable,
    schedule: &solve::NativeRefreshAssignmentSchedule,
    plans: &[KernelPlan<'_>],
    sites: &[solve::SolvePureCallSite],
) -> Result<ProgramHelpers, String> {
    let demands = glue::root_demands(schedule, plans, sites);
    ProgramHelpers::native_with_demands(table, sites, |site| {
        demands
            .get(&site.owner().index())
            .cloned()
            .unwrap_or_else(Demand::all)
    })
    .map_err(|e| e.to_string())
}

fn append_code(
    module: &mut Module,
    bodies: Vec<Function>,
    outlined: Vec<Function>,
    entry: [Function; 2],
    budget: usize,
) -> Result<(), String> {
    let mut code = CodeSection::new();
    for body in bodies {
        code.function(&body);
    }
    for body in outlined {
        code.function(&body);
    }
    for function in &entry {
        groups::check_size(function, budget, "native assignment entrypoint")?;
        code.function(function);
    }
    module.section(&code);
    Ok(())
}

fn linked_imports(
    programs: &[Vec<LinearOp>],
    helpers: &ProgramHelpers,
) -> Result<Vec<MathImport>, String> {
    Ok(collect_imports(programs)?
        .into_iter()
        .chain(helpers.math_imports().iter().copied())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect())
}

/// The layout stages address: the host Y scalars followed by one private slot
/// per derived output, issued with the schedule for this host layout.
fn work_layout<'a>(
    schedule: &'a solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
) -> Result<&'a VarLayout, String> {
    let work = schedule.work_layout();
    if work.p_scalars() != layout.p_scalars()
        || work.y_scalars() != layout.y_scalars() + schedule.derived_outputs().len()
    {
        return Err("native schedule was issued for a different layout".into());
    }
    Ok(work)
}

/// A checked entry is issued only for a program that needs one: an issued
/// call, a checked model operation, or typed input or output lanes.
fn require_checked_entry(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    sites: &[solve::SolvePureCallSite],
    programs: &[Vec<LinearOp>],
) -> Result<(), String> {
    if sites.is_empty()
        && schedule.derived_outputs().is_empty()
        && schedule.input_runs().is_empty()
        && !has_checked_model_operations(programs.iter().flatten())
    {
        return Err("native checked program has no checked operations".into());
    }
    Ok(())
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
