//! Target-local function outlining at complete certified stage boundaries.
//!
//! The wrapper alone guards/copies/publishes Y and resets invocation memos.
//! Groups keep the same arena, scratch offsets, helper indices and status ABI;
//! no expression, target, call coordinate or schedule edge is reconstructed.
#[cfg(test)]
mod tests;
use super::*;

pub(super) struct GatherFaults {
    pub status_base: u32,
    pub entries: Vec<crate::NativeGatherFault>,
}

pub(super) const FUNCTION_BYTES: usize = 1024 * 1024;
// i32.const 0 followed by the function's end, including local declarations in
// every actual byte-length check. This leaves no estimated opcode budget.
const SUCCESS_BYTES: usize = 3;

pub(super) fn locals(arena: arena::ArenaPlan) -> Vec<(u32, ValType)> {
    let mut locals = arena.locals();
    locals.extend([(2, ValType::I32), (1, ValType::I64)]);
    locals
}

pub(super) fn check_size(body: &Function, budget: usize, kind: &str) -> Result<(), String> {
    if body.byte_len() > budget {
        return Err(format!(
            "{kind} exceeds native function byte budget ({budget})"
        ));
    }
    Ok(())
}

pub(super) fn outline(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    plans: &[KernelPlan<'_>],
    catalog: &ImportCatalog,
    arena: arena::ArenaPlan,
    calls: &CallProgramPlan,
    budget: usize,
    faults: &mut GatherFaults,
) -> Result<Vec<Function>, String> {
    let locals = locals(arena);
    let prefix = Function::new(locals.clone()).byte_len();
    let mut current = Function::new(locals.clone());
    let mut groups = Vec::new();
    for (kernel, plan) in plans.iter().enumerate().take(schedule.stages().len()) {
        let encoded =
            stage_body(schedule, kernel, plan, catalog, arena, calls, faults)?.into_raw_body();
        let instructions = &encoded[prefix..];
        if prefix
            .checked_add(instructions.len())
            .and_then(|n| n.checked_add(SUCCESS_BYTES))
            .is_none_or(|n| n > budget)
        {
            return Err("single native assignment stage exceeds function byte budget".into());
        }
        if current
            .byte_len()
            .checked_add(instructions.len())
            .and_then(|n| n.checked_add(SUCCESS_BYTES))
            .is_none_or(|n| n > budget)
        {
            finish(&mut current);
            groups.push(current);
            current = Function::new(locals.clone());
        }
        current.raw(instructions.iter().copied());
    }
    finish(&mut current);
    groups.push(current);
    Ok(groups)
}

fn finish(function: &mut Function) {
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::End);
}

fn stage_body(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    kernel: usize,
    plan: &KernelPlan<'_>,
    catalog: &ImportCatalog,
    arena: arena::ArenaPlan,
    calls: &CallProgramPlan,
    faults: &mut GatherFaults,
) -> Result<Function, String> {
    let stage = &schedule.stages()[kernel];
    let capture = integer_capture(schedule, stage, calls)?;
    let offset = stage
        .target_span()
        .start
        .checked_mul(8)
        .and_then(|n| i32::try_from(n).ok())
        .ok_or("native target address overflow")?;
    let mut function = Function::new(locals(arena));
    let mut emitter = BodyEmitter::new(catalog, &mut function);
    emitter.arena = Some(arena);
    emitter.calls = Some(calls);
    emitter.kernel_ordinal = kernel;
    emitter.integer_capture = capture;
    emitter.native_stage = Some(stage);
    emitter.gather_status_base = Some(
        faults
            .status_base
            .checked_add(
                u32::try_from(faults.entries.len()).map_err(|_| "native gather status overflow")?,
            )
            .ok_or("native gather status overflow")?,
    );
    emitter.push(Instruction::LocalGet(Y_PTR_PARAM));
    emitter.push(Instruction::I32Const(offset));
    emitter.push(Instruction::I32Add);
    emitter.push(Instruction::LocalSet(OUT_PTR_PARAM));
    for node in &plan.nodes {
        match node {
            NodePlan::Scalar { block, outputs } => emitter.emit_mapped_programs(block, outputs)?,
            NodePlan::Affine(affine) => emitter.emit_affine(affine, LOCAL_BASE + 2)?,
            NodePlan::Matrix(_) => return Err("native matrix stage is not admitted".into()),
        }
    }
    faults.entries.append(&mut emitter.gather_faults);
    Ok(function)
}

/// The Integer call cell a derived-output stage publishes into its lane.
fn integer_capture(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    stage: &solve::NativeRefreshAssignmentStage,
    calls: &CallProgramPlan,
) -> Result<Option<crate::emit::IntegerCapture>, String> {
    let solve::NativeStageSource::Discrete { row } = stage.source() else {
        return Ok(None);
    };
    let output = schedule
        .derived_outputs()
        .iter()
        .find(|output| output.row() == row)
        .ok_or("native discrete stage has no derived output")?;
    let Some(solve::NativeIntegerSource::CallCell { operation, cell }) = output.integer_source()
    else {
        return Ok(None);
    };
    Ok(Some(crate::emit::IntegerCapture {
        operation,
        cell,
        lane: super::buffers::lane_address(calls, output)?,
    }))
}

pub(super) fn invoke(
    emitter: &mut BodyEmitter<'_>,
    first: u32,
    count: usize,
) -> Result<(), String> {
    let status = emitter.calls.ok_or("missing native call layout")?.status;
    for index in 0..count {
        let target = u32::try_from(index)
            .ok()
            .and_then(|n| first.checked_add(n))
            .ok_or("native group index overflow")?;
        for argument in 0..LOCAL_BASE {
            emitter.push(Instruction::LocalGet(argument));
        }
        emitter.push(Instruction::Call(target));
        emitter.push(Instruction::LocalTee(status));
        emitter.push(Instruction::If(BlockType::Empty));
        emitter.push(Instruction::LocalGet(status));
        emitter.push(Instruction::Return);
        emitter.push(Instruction::End);
    }
    Ok(())
}
