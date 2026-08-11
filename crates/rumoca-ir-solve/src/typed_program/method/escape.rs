//! Escape-set construction and signal-check settability for method bodies.
//!
//! The active error-signal status is one 32-bit lattice. Raising, callee
//! propagation, and signalling expression operators add signals, a satisfied
//! signal check removes exactly its caught set, branch arms join, and a loop is
//! closed under one monotone fixpoint. The result is the set that can still be
//! active when the method returns.
//!
//! Two SPEC_0042 trap rows are normative here. T9: a Real relational or
//! equality operator signals NAN on a qNaN operand, so every Real comparison is
//! an effect. T14: division by zero and Real overflow are silently IEEE-754, so
//! no arithmetic operator contributes; only `integer()` and the three
//! linear-solver builtins signal, and Solve owns neither a linear-solver
//! operation nor an adjudicated signal row for `integer()`, so a signalling
//! conversion fails closed instead of under-approximating the escape set.
//! T10: a signal check catches, so testing a signal that cannot be active where
//! the check is written is a construction error. Inside a loop the obligation
//! is proved against exactly the trips the domain declares, so a check whose
//! only raise follows it in a single-trip body is rejected rather than excused
//! by an iteration that never runs.

use rumoca_core::{Span, StructuredIndexDomain};

use super::super::effect::{SolvePredefinedSignal, SolveSignal, SolveSignalSet};
use super::super::program::{
    SolveConversionOperator, SolveOperation, SolveProgramRegion, TypedProgram,
};
use super::super::types::{SolveScalarType, SolveValueType};
use super::action::{
    SolveAction, SolveActionBlock, SolveBranchCondition, SolveSignalTest, SolveSpannedAction,
    SolveValueProgram,
};
use super::{SolveActionConstructionError, SolveSignalClosure, SolveSignalClosureId};

pub(super) struct EscapeContext<'method> {
    pub(super) closures: &'method [SolveSignalClosure],
    pub(super) callees: &'method [SolveSignalSet],
    pub(super) calls: &'method [SolveSignalSet],
}

fn nan() -> SolveSignalSet {
    SolveSignalSet::construct(&[SolveSignal::Predefined(SolvePredefinedSignal::Nan)])
}

/// The signals one method body can still have active when it returns.
///
/// The same walk proves T10 for every signal check it passes, so a method that
/// tests an unsettable or already-caught signal never reaches an escape set.
pub(super) fn escape_set(
    body: &SolveActionBlock,
    context: &EscapeContext<'_>,
) -> Result<SolveSignalSet, SolveActionConstructionError> {
    Walk {
        context,
        checked: true,
    }
    .block(body, SolveSignalSet::EMPTY)
}

/// The signals one checked value program raises when it is evaluated.
fn program_signals(program: &SolveValueProgram, calls: &[SolveSignalSet]) -> SolveSignalSet {
    body_signals(program.region().body(), calls)
}

/// Rejects a region whose signalling behaviour Solve cannot yet name.
pub(super) fn require_named_signals(
    region: &SolveProgramRegion,
    provenance: Span,
) -> Result<(), SolveActionConstructionError> {
    if body_has_unnamed_signal(region.body()) {
        return Err(SolveActionConstructionError::UnnamedSignalEffect { provenance });
    }
    Ok(())
}

struct Walk<'method> {
    context: &'method EscapeContext<'method>,
    checked: bool,
}

impl Walk<'_> {
    fn block(
        &self,
        block: &SolveActionBlock,
        entry: SolveSignalSet,
    ) -> Result<SolveSignalSet, SolveActionConstructionError> {
        let mut active = entry;
        for spanned in block.actions() {
            active = self.action(spanned, active)?;
        }
        Ok(active)
    }

    fn action(
        &self,
        spanned: &SolveSpannedAction,
        active: SolveSignalSet,
    ) -> Result<SolveSignalSet, SolveActionConstructionError> {
        let provenance = spanned.provenance();
        match spanned.action() {
            SolveAction::Limit { .. } => Ok(active),
            SolveAction::Assign { program, .. } => {
                Ok(active.union(program_signals(program, self.context.calls)))
            }
            SolveAction::Signal { signals, closures } => Ok(closures
                .iter()
                .fold(active.union(*signals), |set, closure| {
                    set.union(self.caught(*closure))
                })),
            SolveAction::Invoke { method, .. } => Ok(active.union(
                self.context
                    .callees
                    .get(method.index() as usize)
                    .copied()
                    .unwrap_or(SolveSignalSet::EMPTY),
            )),
            SolveAction::Branch {
                condition,
                if_true,
                if_false,
            } => self.branch(condition, if_true, if_false, active, provenance),
            SolveAction::Loop { domain, body, .. } => self.iterate(domain, body, active),
        }
    }

    fn branch(
        &self,
        condition: &SolveBranchCondition,
        if_true: &SolveActionBlock,
        if_false: &SolveActionBlock,
        active: SolveSignalSet,
        provenance: Span,
    ) -> Result<SolveSignalSet, SolveActionConstructionError> {
        let entry = match condition {
            SolveBranchCondition::Value(program) => {
                active.union(program_signals(program, self.context.calls))
            }
            SolveBranchCondition::Signal(check) => {
                if self.checked {
                    require_settable(check.test(), check.caught(), active, provenance)?;
                }
                let entry = active.difference(check.caught());
                match check.fallback() {
                    Some(program) => entry.union(program_signals(program, self.context.calls)),
                    None => entry,
                }
            }
        };
        Ok(self
            .block(if_true, entry)?
            .union(self.block(if_false, entry)?))
    }

    /// One monotone join over exactly the trips the loop domain declares.
    ///
    /// A checked loop runs a statically known number of times, so the states
    /// its body can start from are exactly `entry`, `body(entry)`, …,
    /// `body^(trips-1)(entry)`. One round of the join below admits one more
    /// trip, so `trips - 1` rounds join precisely that sequence — and a
    /// single-trip loop performs no round at all, because its body can only
    /// ever start from `entry`. SPEC_0042 T10 is then proved against that
    /// join, so a check whose only raise follows it inside a one-trip body is
    /// a construction error instead of a false acceptance excused by an
    /// iteration that never happens.
    ///
    /// The join is monotone and every round that does not terminate adds a
    /// status bit, so it converges within the width of the status word. A
    /// domain whose trip count is not statically available — or is wider than
    /// that height, where the join has converged anyway — falls back to the
    /// conservative fixpoint over the full lattice height.
    ///
    /// A loop the domain proves runs at least once leaves exactly what its
    /// last trip left, which is what the checked walk from the joined entry
    /// state returns: `body` is monotone, so applying it to the join covers
    /// `body^trips(entry)`. Re-joining `entry` there would instead report a
    /// signal the body catches as still active after the loop, and reject a
    /// method that honestly declares it does not escape. A body that a
    /// statically known count does not prove to run — an unreadable count, or
    /// the empty domain a checked loop can never carry — keeps the entry state
    /// alive instead.
    fn iterate(
        &self,
        domain: &StructuredIndexDomain,
        body: &SolveActionBlock,
        entry: SolveSignalSet,
    ) -> Result<SolveSignalSet, SolveActionConstructionError> {
        let trips = static_trips(domain);
        let quiet = Walk {
            context: self.context,
            checked: false,
        };
        let mut reach = entry;
        for _ in 0..join_rounds(trips) {
            let next = reach.union(quiet.block(body, reach)?);
            if next == reach {
                break;
            }
            reach = next;
        }
        // One checked walk discharges T10 against every state the body can
        // start from and yields what one further trip leaves active.
        let after_one_more_trip = self.block(body, reach)?;
        Ok(match trips {
            Some(1..) => after_one_more_trip,
            _ => reach.union(after_one_more_trip),
        })
    }

    fn caught(&self, closure: SolveSignalClosureId) -> SolveSignalSet {
        self.context
            .closures
            .get(closure.index())
            .map_or(SolveSignalSet::EMPTY, SolveSignalClosure::caught)
    }
}

/// The exact number of trips one loop domain declares, when it declares one.
///
/// A checked loop domain is finite, bounded, and non-empty, so this is the
/// static trip count itself rather than a bound chosen by the analysis.
fn static_trips(domain: &StructuredIndexDomain) -> Option<usize> {
    domain.validate().ok()
}

/// The rounds needed to join every state one loop body can start from.
///
/// `trips` states are reachable at the body entry, and each round admits one
/// more, so `trips - 1` rounds are exact. An unvalidated or very wide domain
/// falls back to the lattice height, where the monotone join has converged.
fn join_rounds(trips: Option<usize>) -> u32 {
    let Some(trips) = trips else {
        return u32::BITS;
    };
    u32::try_from(trips.saturating_sub(1))
        .unwrap_or(u32::BITS)
        .min(u32::BITS)
}

/// SPEC_0042 T10: testing an unsettable or already-caught signal is an error.
///
/// A listed test names the exact signals it discharges, so every one of them
/// must be able to be active here. A negated or unrestricted check discharges
/// whatever remains, so it must be able to discharge at least one signal.
fn require_settable(
    test: Option<SolveSignalTest>,
    caught: SolveSignalSet,
    active: SolveSignalSet,
    provenance: Span,
) -> Result<(), SolveActionConstructionError> {
    let settable = match test {
        Some(test) if !test.negated() => active.contains_all(test.signals()),
        _ => !caught.is_disjoint(active),
    };
    if settable {
        return Ok(());
    }
    Err(SolveActionConstructionError::UnsettableSignalTest { provenance })
}

fn region_signals(region: &SolveProgramRegion, calls: &[SolveSignalSet]) -> SolveSignalSet {
    body_signals(region.body(), calls)
}

/// The signals one checked program body raises when it is evaluated.
pub(super) fn body_signals(body: &TypedProgram, calls: &[SolveSignalSet]) -> SolveSignalSet {
    body.operations()
        .iter()
        .fold(SolveSignalSet::EMPTY, |set, spanned| {
            set.union(operation_signals(
                spanned.operation(),
                body.register_types(),
                calls,
            ))
        })
}

fn operation_signals(
    operation: &SolveOperation,
    registers: &[SolveValueType],
    calls: &[SolveSignalSet],
) -> SolveSignalSet {
    match operation {
        SolveOperation::Compare { lhs, .. } if is_real(registers, lhs.index()) => nan(),
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => region_signals(if_true, calls).union(region_signals(if_false, calls)),
        SolveOperation::Map { body, .. } => region_signals(body, calls),
        SolveOperation::Fold { transition, .. } => region_signals(transition, calls),
        SolveOperation::Call { owner, .. } => calls
            .get(owner.index() as usize)
            .copied()
            .unwrap_or(SolveSignalSet::EMPTY),
        _ => SolveSignalSet::EMPTY,
    }
}

/// True when the region evaluates an operation whose signal row is unnamed.
fn region_has_unnamed_signal(region: &SolveProgramRegion) -> bool {
    body_has_unnamed_signal(region.body())
}

pub(super) fn body_has_unnamed_signal(body: &TypedProgram) -> bool {
    body.operations()
        .iter()
        .any(|spanned| match spanned.operation() {
            SolveOperation::Convert { operator, .. } => matches!(
                operator,
                SolveConversionOperator::RealToIntegerTowardZero
                    | SolveConversionOperator::RealToIntegerTowardNegativeInfinity
            ),
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => region_has_unnamed_signal(if_true) || region_has_unnamed_signal(if_false),
            SolveOperation::Map { body, .. } => region_has_unnamed_signal(body),
            SolveOperation::Fold { transition, .. } => region_has_unnamed_signal(transition),
            _ => false,
        })
}

fn is_real(registers: &[SolveValueType], index: usize) -> bool {
    registers
        .get(index)
        .is_some_and(|value_type| matches!(value_type.element_type(), SolveScalarType::Real { .. }))
}
