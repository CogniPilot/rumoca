//! A target-neutral candidate partition of one canonical typed program.
//!
//! This is not an executable staged entry or a stable-parameter certificate.
//! Its proposed read-only slots are lifetime requirements: the runtime owner
//! must commit their canonical values and all stage results atomically before
//! a target-specialization policy can select this candidate. The ordinary
//! program remains authoritative. No checks move and no finite facts arise
//! from stage initialization.

use super::*;
use std::collections::BTreeSet;

mod construction;
#[cfg(test)]
mod tests;

/// A proposed parameter slot does not belong to this program's read-only
/// externally initialized interface. Storage class alone never proves lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveParameterStageError {
    UnknownSlot(SolveSlotId),
    MutableSlot(SolveSlotId),
    InternalSlot(SolveSlotId),
    DuplicateSlot(SolveSlotId),
}

/// One call-time action at the original operation ordinal. Stage loads have
/// no validation meaning; every original finite/assertion/publication action
/// remains a Keep, in precisely the original order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveParameterCallStep {
    Keep { operation: usize },
    StageLoad { operation: usize, stage_slot: usize },
}

/// One complete typed value crossing from the stage to the call. Tensor
/// values retain their compact type; this does not enumerate tensor cells.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveParameterStageValue {
    register: SolveRegisterId,
    operation: usize,
}

impl SolveParameterStageValue {
    #[must_use]
    pub const fn register(self) -> SolveRegisterId {
        self.register
    }

    #[must_use]
    pub const fn operation(self) -> usize {
        self.operation
    }
}

/// Constructor-issued scheduling data, borrowed from its exact source owner.
/// No deserialization or detached operation list can forge that association.
///
/// Selection still requires complete writer ownership of `required_slots`,
/// unobservable floating exception state, an atomic snapshot/stage commit and
/// target-bound whole-plan resource admission. No backend may execute this
/// candidate as if those obligations had already been discharged.
#[derive(Debug)]
pub struct SolveParameterStageCandidate<'program> {
    source: &'program TypedProgram,
    required_slots: Box<[SolveSlotId]>,
    stage: Box<[usize]>,
    values: Box<[SolveParameterStageValue]>,
    call: Box<[SolveParameterCallStep]>,
}

impl TypedProgram {
    /// Construct a conservative stage candidate from proposed stable slots.
    /// Read-only is checked here; stable-P writer ownership is deliberately
    /// not inferred from Input/TunableParameter/CalculatedParameter labels.
    ///
    /// Only the existing stored CannotFault classification and the closed
    /// supported vocabulary license speculation. Regions/calls/foreign bodies
    /// stay intact at call time; this query never scalarizes or lowers them.
    pub fn parameter_stage_candidate(
        &self,
        proposed_slots: &[SolveSlotId],
    ) -> Result<SolveParameterStageCandidate<'_>, SolveParameterStageError> {
        construction::construct(self, proposed_slots)
    }
}

impl<'program> SolveParameterStageCandidate<'program> {
    #[must_use]
    pub const fn source(&self) -> &'program TypedProgram {
        self.source
    }

    /// The actually used proposed slots, in source slot order. Every writer
    /// must participate in the runtime owner transaction before selection.
    #[must_use]
    pub fn required_slots(&self) -> &[SolveSlotId] {
        &self.required_slots
    }

    #[must_use]
    pub fn stage_operation_ordinals(&self) -> &[usize] {
        &self.stage
    }

    pub fn stage_operations(&self) -> impl Iterator<Item = &'program SolveSpannedOperation> + '_ {
        self.stage
            .iter()
            .map(|index| &self.source.operations[*index])
    }

    #[must_use]
    pub fn values(&self) -> &[SolveParameterStageValue] {
        &self.values
    }

    #[must_use]
    pub fn call_steps(&self) -> &[SolveParameterCallStep] {
        &self.call
    }

    /// Complete logical value cells, not bytes or a WCET/storage bound. A
    /// target must account for representations, alignment, scratch, snapshot
    /// and owner metadata as well as these stage-to-call values.
    #[must_use]
    pub fn retained_scalar_cells(&self) -> u128 {
        self.values
            .iter()
            .map(|value| {
                u128::from(self.source.register_types[value.register.index()].scalar_count())
            })
            .sum()
    }
}
