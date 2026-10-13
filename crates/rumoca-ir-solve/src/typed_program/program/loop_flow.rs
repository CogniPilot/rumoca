//! Unroll decision of every loop of one checked typed program (SPEC_0040
//! SOLVE-C100, SOLVE-C102).
//!
//! A loop over one binder with a proven constant trip count, no early-exit
//! predicate and a body within the cost bound below carries an `unroll`
//! property: the number of straight-line passes every executor may emit for it
//! instead of a counted loop. The decision is made once, from the program
//! alone, so each backend either honours it or runs the counted loop with
//! identical results.
//!
//! The bounds are a code-size budget, not a model fit. A pass costs the
//! operations of its body, so the unrolled passes of one loop, nested unrolled
//! loops included, cost at most `MAX_UNROLLED_OPERATIONS` operations (a
//! straight-line region of that size stays within one function body of the
//! wasm and Cranelift backends without a second-level code-size decision), and
//! at most `MAX_UNROLLED_PASSES` passes so that the per-pass binder constants
//! stay a small fixed set.

use super::{
    SolveAssertionMessage, SolveOperation, SolveProgramRegion, SolveSpannedOperation,
    SolveStorageClass,
};
use crate::fault::FaultClass;
use rumoca_core::StructuredIndexDomain;

/// Most passes a loop may be emitted as separate straight-line passes.
pub const MAX_UNROLLED_PASSES: usize = 32;
/// Most operations the straight-line passes of one loop may emit, nested
/// unrolled loops included.
pub const MAX_UNROLLED_OPERATIONS: usize = 1024;

/// The unroll property of each operation, the operations one pass of the whole
/// program emits, and whether a pass can record an observation.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct LoopFlow {
    /// Per operation: the pass count when it is a loop emitted unrolled.
    unrolled: Box<[Option<usize>]>,
    weight: usize,
    observes: bool,
}

/// The loop an operation runs: its domain, body and whether it has an
/// early-exit predicate.
fn loop_of(
    operation: &SolveOperation,
) -> Option<(&StructuredIndexDomain, &super::TypedProgram, bool)> {
    match operation {
        SolveOperation::Fold {
            domain, transition, ..
        } => Some((
            domain,
            transition.body(),
            invariant_guard(operation).is_none() && is_tested(operation),
        )),
        SolveOperation::Map { domain, body, .. } => Some((domain, body.body(), false)),
        _ => None,
    }
}

impl LoopFlow {
    pub(super) fn of(operations: &[SolveSpannedOperation]) -> Self {
        let unrolled = operations
            .iter()
            .map(|operation| {
                let (domain, body, continuation) = loop_of(operation.operation())?;
                passes(domain, body.emitted_weight(), continuation)
            })
            .collect::<Box<[_]>>();
        let weight = operations
            .iter()
            .zip(&unrolled)
            .map(|(operation, unrolled)| 1 + operation_weight(operation.operation(), *unrolled))
            .fold(0, usize::saturating_add);
        let observes = operations
            .iter()
            .any(|operation| operation_observes(operation.operation()));
        Self {
            unrolled,
            weight,
            observes,
        }
    }

    pub(super) const fn weight(&self) -> usize {
        self.weight
    }

    pub(super) const fn observes(&self) -> bool {
        self.observes
    }

    pub(super) fn unrolled(&self, operation: usize) -> Option<usize> {
        self.unrolled.get(operation).copied().flatten()
    }
}

/// The passes a loop of `domain` runs when emitted unrolled: one binder, no
/// early-exit predicate, a count and a body within the bounds above.
fn passes(domain: &StructuredIndexDomain, body_weight: usize, continuation: bool) -> Option<usize> {
    if domain.binders.len() != 1 || continuation {
        return None;
    }
    let count = domain.scalar_count().ok()?;
    (1..=MAX_UNROLLED_PASSES)
        .contains(&count)
        .then(|| body_weight.saturating_mul(count))
        .filter(|operations| *operations <= MAX_UNROLLED_OPERATIONS)
        .map(|_| count)
}

/// The operations an operation emits beyond itself, counting each unrolled
/// loop once per pass.
fn operation_weight(operation: &SolveOperation, unrolled: Option<usize>) -> usize {
    let times =
        |body: &super::TypedProgram| body.emitted_weight().saturating_mul(unrolled.unwrap_or(1));
    match operation {
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => times(transition.body()).saturating_add(
            continuation
                .as_ref()
                .map_or(0, |predicate| predicate.body().emitted_weight()),
        ),
        SolveOperation::Map { body, .. } => times(body.body()),
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => if_true
            .body()
            .emitted_weight()
            .saturating_add(if_false.body().emitted_weight()),
        SolveOperation::AssignedConditional { program } => program
            .if_true()
            .body()
            .emitted_weight()
            .saturating_add(program.if_false().body().emitted_weight()),
        SolveOperation::CheckAssertion {
            message: SolveAssertionMessage::Captures { program },
            ..
        } => program.body().emitted_weight(),
        _ => 0,
    }
}

/// Whether an operation records an observation, which names the enclosing
/// pass counters: an assertion check or a call, or a region containing one.
fn operation_observes(operation: &SolveOperation) -> bool {
    match operation {
        SolveOperation::CheckAssertion { .. } | SolveOperation::Call { .. } => true,
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => {
            transition.body().observes()
                || continuation
                    .as_ref()
                    .is_some_and(|predicate| predicate.body().observes())
        }
        SolveOperation::Map { body, .. } => body.body().observes(),
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => if_true.body().observes() || if_false.body().observes(),
        SolveOperation::AssignedConditional { program } => {
            program.if_true().body().observes() || program.if_false().body().observes()
        }
        _ => false,
    }
}

fn is_tested(operation: &SolveOperation) -> bool {
    matches!(
        operation,
        SolveOperation::Fold {
            continuation: Some(_),
            ..
        }
    )
}

/// The early-exit predicate of a fold when its value cannot change from one
/// pass to the next, so the fold is guarded once and its passes run without a
/// test: the loop keeps its unroll property and a backend emits
/// `if predicate { passes }`.
///
/// The predicate is invariant exactly when evaluating it more or fewer times
/// is unobservable and every value it reads is fixed for the whole loop. The
/// first holds when the predicate as a region is a pure computation that
/// cannot fault (`FaultClass::CannotFault`, SOLVE-C102: no call, assertion,
/// assigned-state write or loop, and no Integer operation that can leave its
/// domain). The second holds when every load that feeds the test reads a slot no pass writes: a
/// capture (an immutable snapshot of the enclosing invocation) or a constant.
/// A load of a carried value, of a binder (the pass about to run) or of state
/// the loop may write makes the predicate vary with the pass.
#[must_use]
pub fn invariant_guard(operation: &SolveOperation) -> Option<&SolveProgramRegion> {
    let SolveOperation::Fold {
        initial,
        captures,
        continuation: Some(predicate),
        ..
    } = operation
    else {
        return None;
    };
    let program = predicate.body();
    if program.region_fault_class() != FaultClass::CannotFault {
        return None;
    }
    let captured = initial.len()..initial.len() + captures.len();
    let mut inputs = 0;
    let fixed = program
        .slots()
        .iter()
        .map(|slot| match slot.storage() {
            SolveStorageClass::Input => {
                inputs += 1;
                captured.contains(&(inputs - 1))
            }
            SolveStorageClass::Constant => true,
            _ => false,
        })
        .collect::<Vec<_>>();
    // Only the loads that feed the stored test matter: a load nothing reads
    // cannot make the test vary.
    let mut needed = vec![false; program.register_types().len()];
    let mut varies = false;
    for operation in program.operations().iter().rev() {
        let operation = operation.operation();
        let feeds = match operation {
            SolveOperation::Store { .. } => true,
            other => {
                let mut feeds = false;
                other.visit_output_registers(|register| feeds |= needed[register.index()]);
                feeds
            }
        };
        if !feeds {
            continue;
        }
        if let SolveOperation::Load { slot, .. } = operation {
            varies |= !fixed[slot.index()];
        }
        operation.visit_input_registers(|register| needed[register.index()] = true);
    }
    (!varies).then_some(predicate)
}
