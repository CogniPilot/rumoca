//! Lifecycle evaluation of a Solve algorithm block (#483).
//!
//! A block's persistent state outlives each method: the caller keeps one
//! value per block slot (`None` while the slot was never written) and each
//! method runs over it, leaving the slots it stored updated. Method locals are
//! not part of that state; they start unwritten on every call.

use rumoca_ir_solve::{SolveAlgorithmBlock, SolveBlockMethodKind, SolveStorageClass};

use super::{
    AssertionPolicy, EvalFrame, EvaluationMode, FrameStorage, RecursionChain, SolvePureCallOwnerId,
    TypedProgramEvalError, TypedValue, invalid,
};

/// Run `method` of `block` over the persistent `state` (one entry per block
/// slot, in slot order), updating the slots the method stored. A Startup that
/// faults leaves `state` unchanged except for the signals it raised; a
/// Recalibrate or DoStep that faults keeps what it stored before the fault.
pub fn eval_algorithm_method(
    block: &SolveAlgorithmBlock,
    method: SolveBlockMethodKind,
    state: &mut [Option<TypedValue>],
) -> Result<(), TypedProgramEvalError> {
    let program = match method {
        SolveBlockMethodKind::Startup => block.startup(),
        SolveBlockMethodKind::Recalibrate => block.recalibrate(),
        SolveBlockMethodKind::DoStep => block.do_step(),
    }
    .program();
    let provenance = block
        .slots()
        .first()
        .map_or(rumoca_core::Span::DUMMY, |slot| slot.source().span());
    let bound = state.len() == block.slots().len()
        && block.slots().iter().zip(state.iter()).all(|(slot, value)| {
            value
                .as_ref()
                .is_none_or(|value| value.value_type() == slot.value_type())
        });
    if !bound {
        return invalid("bind algorithm block state", provenance);
    }
    // The caller initializes inputs and tunables before Startup (its
    // return clamp includes them). Later methods assume every block slot
    // initialized: a successful Startup defines the rest, as checked
    // when the block is lowered. Each method clears its own status bits.
    let required = |storage: SolveStorageClass| match method {
        SolveBlockMethodKind::Startup => matches!(
            storage,
            SolveStorageClass::Input | SolveStorageClass::TunableParameter
        ),
        _ => storage != SolveStorageClass::SignalStatus,
    };
    if let Some(slot) = block
        .slots()
        .iter()
        .zip(state.iter())
        .find_map(|(slot, value)| (value.is_none() && required(slot.storage())).then_some(slot))
    {
        return invalid(
            "run a lifecycle method before its block state is initialized",
            slot.source().span(),
        );
    }
    // The block's functions are the only pure-call owners a method calls;
    // the table also carries the block's arithmetic, which types fold
    // iterators.
    let mut storage = FrameStorage::default();
    let mut frame = EvalFrame::new(
        block.functions(),
        program,
        &mut storage,
        EvaluationMode::primal(AssertionPolicy::Value),
        RecursionChain::ROOT,
        SolvePureCallOwnerId::LIFECYCLE_METHOD,
    );
    frame.block = Some(block);
    frame.bound = true;
    for (slot, value) in frame.storage.slots.iter_mut().zip(state.iter()) {
        slot.clone_from(value);
    }
    frame.run();
    if let Some(error) = frame.fault.take() {
        // As in the GALEC interpreter: a failed Startup leaves the state as it
        // was; a failed Recalibrate or DoStep keeps every store (and raised
        // signal) completed before the fault. Lowering refuses code whose
        // regions could hold such an effect back.
        // A failed Startup still leaves raised signals visible: the
        // interpreter restores its state but not its active signals.
        for ((value, slot), declared) in state
            .iter_mut()
            .zip(frame.storage.slots.iter())
            .zip(block.slots())
        {
            if method != SolveBlockMethodKind::Startup
                || declared.storage() == SolveStorageClass::SignalStatus
            {
                value.clone_from(slot);
            }
        }
        return Err(error);
    }
    if frame.stopped.is_some() {
        return invalid("a lifecycle method stopped on an assertion", provenance);
    }
    for (value, slot) in state.iter_mut().zip(frame.storage.slots.iter()) {
        value.clone_from(slot);
    }
    Ok(())
}
