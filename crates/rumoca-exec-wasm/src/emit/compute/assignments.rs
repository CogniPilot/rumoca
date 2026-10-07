//! One direct-write entrypoint for a construction-issued native schedule.

use super::*;
use rumoca_ir_solve as solve;

pub(crate) fn emit_native_assignment_module(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
) -> Result<Vec<u8>, String> {
    if !schedule.derived_outputs().is_empty() {
        return Err(
            "derived discrete outputs are published only by the checked native program entry"
                .into(),
        );
    }
    if !schedule.input_lanes().is_empty() {
        return Err("typed input lanes are read only by the checked native program entry".into());
    }
    layout
        .validate_shape_contract()
        .map_err(|error| error.to_string())?;
    checked_slot_count(layout.y_scalars())?;
    checked_slot_count(layout.p_scalars())?;
    validate_targets(schedule, layout.y_scalars())?;
    let plans = checked_plans(schedule, layout)?;
    let programs = plans
        .iter()
        .flat_map(|plan| plan.programs.iter().cloned())
        .collect::<Vec<_>>();
    let rank = plans.iter().map(|plan| plan.rank).max().unwrap_or(0);
    let arena = arena::ArenaPlan::new(&programs, rank)?;
    let imports = collect_imports(&programs)?;
    let mut module = Module::new();
    let types = add_type_section(&mut module);
    let catalog = add_import_section(&mut module, &imports, &types);
    add_function_section(&mut module, types.eval_type);
    if let Some(arena) = arena {
        arena.add_memory(&mut module);
    }
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export(
        "eval_assignments",
        ExportKind::Func,
        catalog.eval_function_index,
    );
    module.section(&exports);
    let (locals, counter) = if let Some(arena) = arena {
        (arena.locals(), LOCAL_BASE + 2)
    } else {
        program_locals(max_registers(&programs)?, rank)?
    };
    let mut function = Function::new(locals);
    let mut emitter = BodyEmitter::new(&catalog, &mut function);
    emitter.arena = arena;
    guard_buffers(&mut emitter, layout)?;
    for (stage, plan) in schedule.stages().iter().zip(&plans) {
        let offset = stage
            .target_span()
            .start
            .checked_mul(8)
            .and_then(|bytes| u32::try_from(bytes).ok())
            .ok_or("native target byte offset overflow")?;
        // The target owner proved independent per-point assignment values and
        // disjoint exact progressions. Redirect final stores without changing
        // source arithmetic, source prefixes, domains, or issued stage order.
        emitter.push(Instruction::LocalGet(Y_PTR_PARAM));
        emitter.push(Instruction::I32Const(offset as i32));
        emitter.push(Instruction::I32Add);
        emitter.push(Instruction::LocalSet(OUT_PTR_PARAM));
        for node in &plan.nodes {
            match node {
                NodePlan::Scalar { block, outputs } => {
                    emitter.emit_mapped_programs(block, outputs)?
                }
                NodePlan::Affine(affine) => emitter.emit_affine(affine, counter)?,
                NodePlan::Matrix(_) => return Err("native matrix stage is not admitted".into()),
            }
        }
    }
    function.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    Ok(module.finish())
}

pub(super) fn checked_plans<'a>(
    schedule: &'a solve::NativeRefreshAssignmentSchedule,
    layout: &VarLayout,
) -> Result<Vec<KernelPlan<'a>>, String> {
    schedule
        .stages()
        .iter()
        .map(|stage| {
            let block = stage.value_kernel();
            block
                .validate_shape_contract("native direct-write stage")
                .map_err(|error| error.to_string())?;
            let plan = KernelPlan::new(block, layout)?;
            if plan.outputs != stage.target_span().len() {
                return Err(
                    "native stage output bound differs from its target storage span".into(),
                );
            }
            if plan
                .nodes
                .iter()
                .any(|node| matches!(node, NodePlan::Matrix(_)))
            {
                return Err(
                    "native assignment schedule contains an unsupported matrix stage".into(),
                );
            }
            Ok(plan)
        })
        .collect()
}

fn program_locals(registers: usize, rank: usize) -> Result<(Vec<(u32, ValType)>, u32), String> {
    let integer_count = u32::try_from(
        rank.checked_add(1)
            .ok_or("native loop rank overflow")?
            .max(2),
    )
    .map_err(|_| "native loop rank exceeds u32")?;
    let counter = u32::try_from(registers)
        .ok()
        .and_then(|count| LOCAL_BASE.checked_add(count))
        .ok_or("native loop local overflow")?;
    counter
        .checked_add(integer_count)
        .ok_or("native loop local overflow")?;
    let mut locals = locals_for_register_count(registers)?;
    locals.push((integer_count, ValType::I32));
    Ok((locals, counter))
}

pub(super) fn validate_targets(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    y_count: usize,
) -> Result<(), String> {
    let targets = schedule.stages();
    if targets.is_empty() {
        return Err("native assignment schedule is empty".into());
    }
    let mut count = 0usize;
    for (index, stage) in targets.iter().enumerate() {
        let span = stage.target_span();
        if span.end <= span.start || span.end > y_count || stage.target_count() == 0 {
            return Err("native targets do not uniquely cover the provided Y layout".into());
        }
        count = count
            .checked_add(stage.target_count())
            .ok_or("native target count overflow")?;
        for previous in &targets[..index] {
            if stage
                .targets_overlap(previous)
                .map_err(|error| error.to_string())?
            {
                return Err("native targets overlap in the provided Y layout".into());
            }
        }
    }
    if count != y_count {
        return Err("native targets do not cover the provided Y layout".into());
    }
    Ok(())
}

fn guard_buffers(emitter: &mut BodyEmitter<'_>, layout: &VarLayout) -> Result<(), String> {
    for (pointer, count) in [
        (Y_PTR_PARAM, layout.y_scalars()),
        (P_PTR_PARAM, layout.p_scalars()),
    ] {
        if count == 0 {
            continue;
        }
        emitter.push(Instruction::LocalGet(pointer));
        emitter.push(Instruction::I32Const(7));
        emitter.push(Instruction::I32And);
        trap_if(emitter);
        buffer_end(emitter, pointer, count)?;
        emitter.push(Instruction::MemorySize(0));
        emitter.push(Instruction::I64ExtendI32U);
        emitter.push(Instruction::I64Const(16));
        emitter.push(Instruction::I64Shl);
        emitter.push(Instruction::I64GtU);
        trap_if(emitter);
    }
    if layout.p_scalars() != 0 {
        emitter.push(Instruction::LocalGet(Y_PTR_PARAM));
        emitter.push(Instruction::I64ExtendI32U);
        buffer_end(emitter, P_PTR_PARAM, layout.p_scalars())?;
        emitter.push(Instruction::I64LtU);
        emitter.push(Instruction::LocalGet(P_PTR_PARAM));
        emitter.push(Instruction::I64ExtendI32U);
        buffer_end(emitter, Y_PTR_PARAM, layout.y_scalars())?;
        emitter.push(Instruction::I64LtU);
        emitter.push(Instruction::I32And);
        trap_if(emitter);
    }
    Ok(())
}

fn buffer_end(emitter: &mut BodyEmitter<'_>, pointer: u32, count: usize) -> Result<(), String> {
    let bytes = count
        .checked_mul(8)
        .and_then(|value| i64::try_from(value).ok())
        .ok_or("native buffer byte count overflow")?;
    emitter.push(Instruction::LocalGet(pointer));
    emitter.push(Instruction::I64ExtendI32U);
    emitter.push(Instruction::I64Const(bytes));
    emitter.push(Instruction::I64Add);
    Ok(())
}

fn trap_if(emitter: &mut BodyEmitter<'_>) {
    emitter.push(Instruction::If(BlockType::Empty));
    emitter.push(Instruction::Unreachable);
    emitter.push(Instruction::End);
}
