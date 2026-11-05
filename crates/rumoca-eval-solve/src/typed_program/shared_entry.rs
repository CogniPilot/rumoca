//! Mechanical reference consumption of an already-issued mixed-entry call plan.
//! No derivative, sparsity, partition, storage or arithmetic policy lives here.

use rumoca_core::Span;
use rumoca_ir_solve::{SharedEntryCallPlan, SolvePureCallOwner, TypedProgram};

use super::{
    AssertionPolicy, Direction, EvalFrame, EvaluationMode, FrameStorage, InvocationCompletion,
    RecursionChain, SolvePureCallOwnerId, TypedProgramEvalError, TypedValue, invalid_error,
};

/// Evaluate one checked shared entry with source assertions in force.
/// The caller prepares `plan` once; evaluation neither constructs partitions
/// nor treats its lazy call inventory as an eager instruction schedule.
/// Original slot identities determine the input/output ABI, including when
/// inputs and outputs are interleaved in the source declaration order.
pub fn eval_shared_entry_checked<'model>(
    plan: &'model SharedEntryCallPlan<'model>,
    arguments: &[TypedValue],
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    let entry = plan.entry();
    let program = entry.body();
    let provenance = entry.provenance();
    if entry.input_slots().len() != arguments.len() {
        return Err(TypedProgramEvalError::InvalidArgument {
            index: entry.input_slots().len().min(arguments.len()),
            provenance,
        });
    }
    // Preflight the complete ABI before exposing any value to the interpreter.
    for (index, (slot, actual)) in entry.input_slots().iter().zip(arguments).enumerate() {
        let source = program
            .slots()
            .get(slot.index())
            .ok_or_else(|| invalid_error("bind shared-entry input slot", provenance))?;
        if source.value_type() != actual.value_type() {
            return Err(TypedProgramEvalError::InvalidArgument {
                index,
                provenance: source.provenance(),
            });
        }
    }
    let mut storage = FrameStorage::default();
    let mode = EvaluationMode {
        direction: Direction::Primal,
        policy: AssertionPolicy::Stop,
        entry_plan: Some(plan),
    };
    let mut frame = EvalFrame::new(
        entry.calls(),
        program,
        &mut storage,
        mode,
        RecursionChain::ROOT,
        SolvePureCallOwnerId::LIFECYCLE_METHOD,
    );
    for (slot, value) in entry.input_slots().iter().zip(arguments) {
        frame.storage.slots[slot.index()] = Some(value.clone());
    }
    frame.run();
    if let Some(error) = frame.fault.take() {
        return Err(error);
    }
    if let Some(stop) = frame.stopped.take() {
        return InvocationCompletion::Stopped(stop).into_complete();
    }
    entry
        .output_slots()
        .iter()
        .map(|slot| {
            frame
                .storage
                .slots
                .get_mut(slot.index())
                .and_then(Option::take)
                .ok_or_else(|| invalid_error("read shared-entry output slot", provenance))
        })
        .collect()
}

/// Only an actual nominal local operation may select a call partition. The
/// callee's issued body form determines which existing interpreter entry runs.
pub(super) fn resolve_entry_call<'model>(
    plan: &'model SharedEntryCallPlan<'model>,
    program: &'model TypedProgram,
    operation: usize,
    provenance: Span,
    policy: AssertionPolicy,
) -> Result<(&'model SolvePureCallOwner, EvaluationMode<'model>), TypedProgramEvalError> {
    let missing = || invalid_error("resolve source-bound shared-entry call", provenance);
    let call = plan.entry_call_at(program, operation).ok_or_else(missing)?;
    let callee = plan
        .resolve(call)
        .map_err(|_| missing())?
        .helper()
        .ok_or_else(missing)?;
    let mode = if callee.is_directional() {
        EvaluationMode::directional(policy)
    } else {
        EvaluationMode::primal(policy)
    };
    Ok((callee.original_owner(), mode))
}
