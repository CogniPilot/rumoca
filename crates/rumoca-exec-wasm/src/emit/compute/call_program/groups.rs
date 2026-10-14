//! Target-local function outlining at complete certified stage boundaries.
//!
//! The wrapper alone guards/copies/publishes Y.
//! Groups keep the same arena, scratch offsets, helper indices and status ABI;
//! no expression, target, call coordinate or schedule edge is reconstructed.
#[cfg(test)]
mod tests;
use super::*;

pub(super) struct GatherFaults {
    pub status_base: u32,
    pub entries: Vec<crate::NativeGatherFault>,
    pub linear_solve: Vec<crate::NativeLinearSolveFault>,
}

pub(super) use crate::outline::FUNCTION_BYTES;
use crate::outline::Outliner;

pub(super) fn locals(arena: arena::ArenaPlan) -> Vec<(u32, ValType)> {
    let mut locals = arena.locals();
    locals.extend([(2, ValType::I32), (1, ValType::I64)]);
    locals
}

pub(super) fn check_size(body: &Function, budget: usize, kind: &str) -> Result<(), String> {
    if body.byte_len() > budget {
        return Err(format!(
            "{kind} is {} bytes, exceeding the native function byte budget ({budget})",
            body.byte_len()
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
    let mut outliner = Outliner::new(locals(arena.clone()), budget);
    let transfer = Transfer {
        catalog,
        arena: &arena,
        calls,
    };
    // A run no operation reads the Real view of (SOLVE-C69) has none written.
    for input in schedule
        .input_runs()
        .iter()
        .filter(|run| run.view_is_read())
    {
        let chunk = transfer.encode(&outliner, |e| {
            e.write_input_run(input, schedule.input_checks())
        })?;
        push_chunk(&mut outliner, &chunk, "input run")?;
    }
    let mut program_start = 0usize;
    for (kernel, plan) in plans.iter().enumerate().take(schedule.stages().len()) {
        let encoded = outliner.instructions(stage_body(
            schedule,
            (kernel, program_start),
            plan,
            catalog,
            arena.clone(),
            calls,
            faults,
        )?);
        program_start = program_start
            .checked_add(plan.programs.len())
            .ok_or("native stage program ordinal overflows")?;
        push_chunk(&mut outliner, &encoded, "assignment stage")?;
    }
    for run in buffers::derived_runs(schedule.derived_outputs()) {
        let chunk = transfer.encode(&outliner, |e| e.publish_derived_run(run))?;
        push_chunk(&mut outliner, &chunk, "derived output publication")?;
    }
    Ok(outliner.finish())
}

/// Encodes the memory-only entry work (input packing, output
/// staging) as operations the one outliner places alongside the stages.
struct Transfer<'a> {
    catalog: &'a ImportCatalog,
    arena: &'a arena::ArenaPlan,
    calls: &'a CallProgramPlan,
}

impl Transfer<'_> {
    fn encode(
        &self,
        outliner: &Outliner,
        step: impl FnOnce(&mut BodyEmitter<'_>) -> Result<(), String>,
    ) -> Result<Vec<u8>, String> {
        let mut function = Function::new(locals(self.arena.clone()));
        let mut emitter = BodyEmitter::new(self.catalog, &mut function);
        emitter.arena = Some(self.arena.clone());
        emitter.calls = Some(self.calls);
        step(&mut emitter)?;
        Ok(outliner.instructions(function))
    }
}

fn push_chunk(outliner: &mut Outliner, chunk: &[u8], what: &str) -> Result<(), String> {
    outliner.push(chunk).map_err(|too_large| {
        format!(
            "single native {what} is {} bytes, exceeding the function byte budget ({})",
            too_large.bytes, too_large.budget
        )
    })
}

fn stage_body(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    binding: (usize, usize),
    plan: &KernelPlan<'_>,
    catalog: &ImportCatalog,
    arena: arena::ArenaPlan,
    calls: &CallProgramPlan,
    faults: &mut GatherFaults,
) -> Result<Function, String> {
    let (kernel, program_start) = binding;
    let stage = &schedule.stages()[kernel];
    let capture = integer_capture(schedule, stage, calls)?;
    let offset = stage
        .target_span()
        .start
        .checked_mul(8)
        .and_then(|n| i32::try_from(n).ok())
        .ok_or("native target address overflow")?;
    let mut function = Function::new(locals(arena.clone()));
    let mut emitter = BodyEmitter::new(catalog, &mut function);
    emitter.arena = Some(arena);
    emitter.calls = Some(calls);
    emitter.kernel_ordinal = kernel;
    emitter.root_program_cursor = program_start;
    emitter.integer_capture = capture;
    emitter.native_stage = Some(stage);
    emitter.gather_status_base = Some(
        faults
            .status_base
            .checked_add(
                faults
                    .entries
                    .len()
                    .checked_add(faults.linear_solve.len())
                    .and_then(|count| u32::try_from(count).ok())
                    .ok_or("native source status overflow")?,
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
    if emitter.root_program_cursor
        != program_start
            .checked_add(plan.programs.len())
            .ok_or("native stage program ordinal overflows")?
    {
        return Err("native stage did not consume its complete issued programs".into());
    }
    faults.entries.append(&mut emitter.gather_faults);
    faults.linear_solve.append(&mut emitter.linear_solve_faults);
    Ok(function)
}

/// Capture every exact Integer call cell before another call reuses scratch.
fn integer_capture(
    schedule: &solve::NativeRefreshAssignmentSchedule,
    stage: &solve::NativeRefreshAssignmentStage,
    calls: &CallProgramPlan,
) -> Result<Vec<crate::emit::IntegerCapture>, String> {
    if !matches!(stage.source(), solve::NativeStageSource::Discrete { .. }) {
        return Ok(Vec::new());
    }
    let mut captures = Vec::new();
    for output in schedule.derived_outputs_in(stage.target_span()) {
        if let Some(solve::NativeIntegerSource::CallCell { operation, cell }) =
            output.integer_source()
        {
            append_capture(
                &mut captures,
                operation,
                cell,
                super::buffers::lane_address(calls, output)?,
            )?;
        }
    }
    Ok(captures)
}

fn append_capture(
    captures: &mut Vec<crate::emit::IntegerCapture>,
    operation: usize,
    cell: u32,
    lane: u32,
) -> Result<(), String> {
    if let Some(last) = captures.last_mut()
        && last.operation == operation
        && last.cell.checked_add(last.count) == Some(cell)
        && last
            .count
            .checked_mul(8)
            .and_then(|bytes| last.lane.checked_add(bytes))
            == Some(lane)
    {
        last.count = last
            .count
            .checked_add(1)
            .ok_or("native Integer capture count overflow")?;
        return Ok(());
    }
    captures.push(crate::emit::IntegerCapture {
        operation,
        cell,
        lane,
        count: 1,
    });
    Ok(())
}

pub(super) fn invoke(
    emitter: &mut BodyEmitter<'_>,
    first: u32,
    count: usize,
) -> Result<(), String> {
    let status = emitter
        .calls
        .ok_or("missing native call layout")?
        .view()
        .status;
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
