use super::*;
use rumoca_ir_solve::RegisterVersionProgram;

#[derive(Clone, Copy)]
pub(crate) enum VersionTask {
    Evaluate(usize),
    Apply(usize),
    Choose(usize),
    FinishSelect { operation: usize, source: usize },
}

pub(crate) fn evaluate(
    input: PreparedRowEval<'_, '_>,
    versions: &RegisterVersionProgram,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    scratch.version_values.resize(versions.storage_count(), 0.0);
    scratch.version_computed.resize(input.row.len(), false);
    scratch.version_computed.fill(false);
    scratch.version_tasks.clear();
    scratch.lazy_outputs.clear();
    for (index, op) in input.row.iter().enumerate() {
        if let LinearOp::StoreOutput { src } = *op {
            let (producer, value) = source(versions, index, src);
            scratch.version_tasks.push(VersionTask::Evaluate(producer));
            while let Some(task) = scratch.version_tasks.pop() {
                execute(input, versions, scratch, task)?;
            }
            scratch.lazy_outputs.push(scratch.version_values[value]);
        }
    }
    for &value in &scratch.lazy_outputs {
        sink.store(value)?;
    }
    Ok(())
}

fn source(versions: &RegisterVersionProgram, operation: usize, register: Reg) -> (usize, usize) {
    versions.operations()[operation]
        .source(register)
        .expect("the checked source-point view covers this operand")
}

fn execute(
    input: PreparedRowEval<'_, '_>,
    versions: &RegisterVersionProgram,
    scratch: &mut RowEvalScratch,
    task: VersionTask,
) -> Result<(), EvalSolveError> {
    match task {
        VersionTask::Evaluate(index) => demand(input, versions, scratch, index),
        VersionTask::Apply(index) => apply(input, versions, scratch, index)?,
        VersionTask::Choose(index) => choose(input, versions, scratch, index),
        VersionTask::FinishSelect { operation, source } => {
            let (_, destination) = versions.operations()[operation].destination().unwrap();
            scratch.version_values[destination.start] = scratch.version_values[source];
            scratch.version_computed[operation] = true;
        }
    }
    Ok(())
}

fn demand(
    input: PreparedRowEval<'_, '_>,
    versions: &RegisterVersionProgram,
    scratch: &mut RowEvalScratch,
    index: usize,
) {
    if scratch.version_computed[index] {
        return;
    }
    if let LinearOp::Select { cond, .. } = input.row[index] {
        scratch.version_tasks.push(VersionTask::Choose(index));
        scratch
            .version_tasks
            .push(VersionTask::Evaluate(source(versions, index, cond).0));
        return;
    }
    scratch.version_tasks.push(VersionTask::Apply(index));
    // Preserve the existing evaluator's dependency order, including strided
    // dot-product interleaving and guarded-fold activation. The reusable task
    // buffer is runtime scratch; the prepared graph keeps compact range views.
    scratch.lazy_tasks.clear();
    push_lazy_dependencies(&mut scratch.lazy_tasks, input.row[index].clone());
    for task in &scratch.lazy_tasks {
        if let LazyEvalTask::Eval(register) = *task {
            scratch
                .version_tasks
                .push(VersionTask::Evaluate(source(versions, index, register).0));
        }
    }
}

fn choose(
    input: PreparedRowEval<'_, '_>,
    versions: &RegisterVersionProgram,
    scratch: &mut RowEvalScratch,
    index: usize,
) {
    let LinearOp::Select {
        cond,
        if_true,
        if_false,
        ..
    } = input.row[index]
    else {
        unreachable!("a select task comes from a checked Select operation")
    };
    let condition = source(versions, index, cond).1;
    let chosen = if scratch.version_values[condition] != 0.0 {
        if_true
    } else {
        if_false
    };
    let (producer, value) = source(versions, index, chosen);
    scratch.version_tasks.push(VersionTask::FinishSelect {
        operation: index,
        source: value,
    });
    scratch.version_tasks.push(VersionTask::Evaluate(producer));
}

fn apply(
    input: PreparedRowEval<'_, '_>,
    versions: &RegisterVersionProgram,
    scratch: &mut RowEvalScratch,
    index: usize,
) -> Result<(), EvalSolveError> {
    let operation = &versions.operations()[index];
    for operand in operation.operands() {
        let registers = operand.registers();
        let values = operand.values();
        for (register, value) in registers.zip(values) {
            if operand.selected(register) {
                scratch.regs[register] = scratch.version_values[value];
            }
        }
    }
    eval_lazy_pure_op(
        input,
        &mut scratch.regs,
        &mut scratch.tensor_results,
        input.row[index].clone(),
    )?;
    let (start, destination) = operation.destination().unwrap();
    let count = destination.len();
    scratch.version_values[destination]
        .copy_from_slice(&scratch.regs[start as usize..start as usize + count]);
    scratch.version_computed[index] = true;
    Ok(())
}
