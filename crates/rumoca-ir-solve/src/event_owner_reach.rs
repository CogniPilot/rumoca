//! What each event owner of a problem reads ([`EventOwnerReach`]).
//!
//! An event instant evaluates a fixed list of owners: the root conditions,
//! the discrete rows, the runtime, post-commit and guarded assignments, the
//! action conditions and messages, the dynamic time-event rows, the delay
//! rows, the clock-partition intermediates and the `pre` copies. This module
//! is the one walk over that list. It records, per owner program, the solver
//! coordinates, parameter slots and time its execution reads, and whether it
//! evaluates an operation whose result is not a function of those reads.
//! Execution reads are what its outputs depend on together with what its
//! execution obligations read (a finite check, an assertion or an operation
//! that can fault, inside a call included), whether or not an output uses
//! them: a lane only a guard reads still decides whether the owner fails.
//!
//! Two questions are answered from the same records, so they cannot disagree:
//! whether a write to an input can reach any event owner in the same instant
//! (the input event reach), and which owners an event update may take from
//! their last evaluation because no lane they read changed bits since.

#[cfg(test)]
mod tests;

use crate::{
    IndexIntervals, LinearOp, PreParamSource, RefreshPlan, ScalarProgramBlock, SolveProblem,
    StructuralPattern,
};

/// One owner program an event instant evaluates, by its position in the
/// problem.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventOwner {
    /// A program of `events.root_conditions`.
    RootCondition(usize),
    /// A program of `discrete.rhs`.
    DiscreteRow(usize),
    /// A program of `discrete.runtime_assignment_rhs`.
    RuntimeAssignment(usize),
    /// A program of `discrete.post_commit_assignment_rhs`.
    PostCommitAssignment(usize),
    /// An entry of `discrete.guarded_assignments`.
    GuardedAssignment(usize),
    /// A program of `events.action_conditions`.
    ActionCondition(usize),
    /// The message conversions of an entry of `events.actions`.
    ActionMessage(usize),
    /// A program of `events.dynamic_time_event_rhs`.
    DynamicTimeEvent(usize),
    /// A program of one of the three delay row blocks.
    DelayRow(usize),
    /// A program of `discrete.clock_partition_intermediates`.
    ClockIntermediate(usize),
}

/// What one owner program's execution reads.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct EventOwnerReads {
    y: IndexIntervals,
    p: IndexIntervals,
    time: bool,
    effect: bool,
}

impl EventOwnerReads {
    /// The solver coordinates, states included.
    #[must_use]
    pub fn y(&self) -> &IndexIntervals {
        &self.y
    }

    /// The parameter slots.
    #[must_use]
    pub fn p(&self) -> &IndexIntervals {
        &self.p
    }

    #[must_use]
    pub fn reads_time(&self) -> bool {
        self.time
    }

    /// Whether execution evaluates an operation (table access, random state)
    /// whose result is not a function of the reads above.
    #[must_use]
    pub fn has_effect(&self) -> bool {
        self.effect
    }

    fn of_program(program: &[LinearOp], span: Option<rumoca_core::Span>) -> Option<Self> {
        Some(Self {
            y: std::sync::Arc::unwrap_or_clone(
                StructuralPattern::derive_execution_y_dependency_ranges(program, span).ok()?,
            ),
            p: std::sync::Arc::unwrap_or_clone(
                StructuralPattern::derive_execution_p_dependency_ranges(program, span).ok()?,
            ),
            time: StructuralPattern::execution_reads_time(program, span).ok()?,
            effect: StructuralPattern::execution_has_effect(program, span).ok()?,
        })
    }

    /// The reads of every conversion in `action`'s message.
    fn of_message(action: &crate::SolveEventAction) -> Option<Self> {
        let mut reads = Self::default();
        for part in &action.message.parts {
            if let crate::SolveEventMessagePart::Conversion { value, .. } = part {
                reads.extend(&Self::of_program(value, Some(action.span))?);
            }
        }
        Some(reads)
    }

    fn extend(&mut self, other: &Self) {
        self.y = self.y.union(&other.y);
        self.p = self.p.union(&other.p);
        self.time |= other.time;
        self.effect |= other.effect;
    }
}

/// Why a question about an event's reach cannot be answered from the owner
/// records: the caller takes the path that evaluates everything.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventReachRefusal {
    /// The problem carries no issued reach.
    NotIssued,
    /// The issued reach is not the one this problem derives.
    Mismatch,
    /// The dependency walk refused an owner program.
    Unanalysed,
    /// A structured update or an event transaction is an owner; their reads
    /// are not recorded per program.
    OpaqueOwner,
    /// A refresh the event instant runs is not an exact causal schedule: it
    /// settles a simultaneous block from a warm start.
    UncertifiedRefresh,
    /// A `pre` copy reads a solver coordinate, which every event exit latches
    /// whatever the inputs are.
    SolverPreCopy,
    /// The model has delay channels, whose event roots and rows read history.
    DelayChannels,
    /// A root condition or discrete row contains an effect.
    EffectfulOwner,
}

impl std::fmt::Display for EventReachRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::NotIssued => "the problem carries no issued event owner reach",
            Self::Mismatch => "the issued event owner reach does not match the problem",
            Self::Unanalysed => "the dependency walk refused an event owner program",
            Self::OpaqueOwner => "a structured update or event transaction is an event owner",
            Self::UncertifiedRefresh => "an event refresh is not an exact causal schedule",
            Self::SolverPreCopy => "a pre copy reads a solver coordinate",
            Self::DelayChannels => "the model has delay channels",
            Self::EffectfulOwner => "a root condition or discrete row contains an effect",
        })
    }
}

/// The event owners of one problem and what each reads.
///
/// The only way to obtain one is [`issue_event_owner_reach`] over a problem,
/// and the only way to read it back is [`IssuedEventOwnerReach::checked_for`]
/// with the problem it is used for, which derives the reach again and refuses
/// one that differs. No constructor takes caller-supplied reads, and a reach
/// issued for one problem is refused by any other. It is not serialized: a
/// replayed problem issues its own from its checked owners.
///
/// ```compile_fail
/// use rumoca_ir_solve::EventOwnerReach;
/// // A caller cannot forge a reach with no owners.
/// let forged = EventOwnerReach::default();
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EventOwnerReach {
    owners: Vec<(EventOwner, EventOwnerReads)>,
    pre_parameter_sources: IndexIntervals,
    solver_pre_copy: bool,
    opaque_owner: bool,
    unanalysed: bool,
    algebraic_certified: bool,
    event_refresh_certified: bool,
    delay_channels: bool,
}

/// The reads of every owner of one instant, as the input event reach takes
/// them.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct EventInstantReads {
    /// Solver coordinates, states included.
    pub y: IndexIntervals,
    /// Parameter slots, the `pre` copy sources included.
    pub p: IndexIntervals,
}

impl EventOwnerReach {
    fn derive(problem: &SolveProblem) -> Self {
        let mut reach = Self {
            owners: Vec::new(),
            pre_parameter_sources: IndexIntervals::default(),
            solver_pre_copy: false,
            opaque_owner: false,
            unanalysed: false,
            algebraic_certified: false,
            event_refresh_certified: false,
            delay_channels: false,
        };
        let discrete = &problem.discrete;
        let events = &problem.events;
        reach.extend_block(&events.root_conditions, EventOwner::RootCondition);
        reach.extend_block(&discrete.rhs, EventOwner::DiscreteRow);
        reach.extend_block(
            &discrete.runtime_assignment_rhs,
            EventOwner::RuntimeAssignment,
        );
        reach.extend_block(
            &discrete.post_commit_assignment_rhs,
            EventOwner::PostCommitAssignment,
        );
        for (index, owner) in discrete.guarded_assignments.iter().enumerate() {
            reach.extend_program(
                EventOwner::GuardedAssignment(index),
                owner.program(),
                Some(owner.span()),
            );
        }
        reach.extend_block(&events.action_conditions, EventOwner::ActionCondition);
        for (index, action) in events.actions.iter().enumerate() {
            let reads = EventOwnerReads::of_message(action);
            reach.unanalysed |= reads.is_none();
            reach
                .owners
                .push((EventOwner::ActionMessage(index), reads.unwrap_or_default()));
        }
        reach.extend_block(&events.dynamic_time_event_rhs, EventOwner::DynamicTimeEvent);
        let delays = &events.delays;
        let mut delay_row = 0;
        for block in [
            &delays.source_rhs,
            &delays.delay_time_rhs,
            &delays.delay_max_rhs,
        ] {
            for program in 0..block.programs().len() {
                reach.extend_program(
                    EventOwner::DelayRow(delay_row),
                    &block.programs()[program],
                    block.program_span(program),
                );
                delay_row += 1;
            }
        }
        reach.delay_channels = delay_row != 0 || !delays.value_parameter_indices.is_empty();
        reach.extend_block(
            &discrete.clock_partition_intermediates,
            EventOwner::ClockIntermediate,
        );
        reach.opaque_owner =
            !discrete.structured_updates.is_empty() || !discrete.event_transactions.is_empty();
        for binding in &problem.solve_layout.pre_param_bindings {
            match binding.source {
                PreParamSource::P { index } => reach.pre_parameter_sources.insert(index),
                PreParamSource::Y { .. } => reach.solver_pre_copy = true,
            }
        }
        let owners = &problem.continuous.refresh_owners;
        reach.algebraic_certified = owners.algebraic().is_exact_causal_schedule();
        reach.event_refresh_certified = std::iter::once(owners.event())
            .chain(owners.clock_events())
            .all(RefreshPlan::is_exact_causal_schedule);
        reach
    }

    fn extend_block(&mut self, block: &ScalarProgramBlock, owner: fn(usize) -> EventOwner) {
        for (program, ops) in block.programs().iter().enumerate() {
            self.extend_program(owner(program), ops, block.program_span(program));
        }
    }

    fn extend_program(
        &mut self,
        owner: EventOwner,
        program: &[LinearOp],
        span: Option<rumoca_core::Span>,
    ) {
        match EventOwnerReads::of_program(program, span) {
            Some(reads) => self.owners.push((owner, reads)),
            None => {
                self.unanalysed = true;
                self.owners.push((owner, EventOwnerReads::default()));
            }
        }
    }

    /// The owners in issued order with what each reads.
    pub fn owners(&self) -> impl Iterator<Item = (EventOwner, &EventOwnerReads)> + '_ {
        self.owners.iter().map(|(owner, reads)| (*owner, reads))
    }

    /// What every owner of one instant reads, for the input event reach.
    ///
    /// Refused when an owner cannot be analysed, when a structured update or
    /// event transaction is an owner, or when a `pre` copy reads a solver
    /// coordinate. Following the coordinates through their algebraic
    /// definitions is the caller's, which owns the refresh plans.
    pub fn instant_reads(&self) -> Result<EventInstantReads, EventReachRefusal> {
        if self.unanalysed {
            return Err(EventReachRefusal::Unanalysed);
        }
        if self.opaque_owner {
            return Err(EventReachRefusal::OpaqueOwner);
        }
        if self.solver_pre_copy {
            return Err(EventReachRefusal::SolverPreCopy);
        }
        let mut reads = EventInstantReads {
            y: IndexIntervals::default(),
            p: self.pre_parameter_sources.clone(),
        };
        for (_, owner) in &self.owners {
            reads.y = reads.y.union(&owner.y);
            reads.p = reads.p.union(&owner.p);
        }
        Ok(reads)
    }

    /// Whether an event update may take a root-condition or discrete-row
    /// program from its last evaluation in the same event when no lane it
    /// reads changed bits since.
    ///
    /// Refused when an owner cannot be analysed, when a structured update or
    /// event transaction is an owner, when the model has delay channels, when
    /// a refresh the instant runs is not an exact causal schedule, or when a
    /// root condition or discrete row contains an effect. The refusal is
    /// known before the event evaluates anything.
    pub fn restricted_event_update(&self) -> Result<(), EventReachRefusal> {
        if self.unanalysed {
            return Err(EventReachRefusal::Unanalysed);
        }
        if self.opaque_owner {
            return Err(EventReachRefusal::OpaqueOwner);
        }
        if self.delay_channels {
            return Err(EventReachRefusal::DelayChannels);
        }
        if !self.algebraic_certified || !self.event_refresh_certified {
            return Err(EventReachRefusal::UncertifiedRefresh);
        }
        if self.owners.iter().any(|(owner, reads)| {
            matches!(
                owner,
                EventOwner::RootCondition(_) | EventOwner::DiscreteRow(_)
            ) && reads.effect
        }) {
            return Err(EventReachRefusal::EffectfulOwner);
        }
        Ok(())
    }
}

/// The event owner reach a problem carries, once issued.
///
/// The default is not issued. It is derived data, absent from the wire: a
/// replayed problem issues its own.
#[derive(Clone, Debug, Default)]
pub struct IssuedEventOwnerReach(Option<EventOwnerReach>);

impl IssuedEventOwnerReach {
    /// The issued reach if it is the one `problem` derives.
    ///
    /// The reach is derived again and compared, so a reach issued before the
    /// problem's owners changed, or copied from another problem, is refused.
    pub fn checked_for(
        &self,
        problem: &SolveProblem,
    ) -> Result<&EventOwnerReach, EventReachRefusal> {
        let issued = self.0.as_ref().ok_or(EventReachRefusal::NotIssued)?;
        if *issued != EventOwnerReach::derive(problem) {
            return Err(EventReachRefusal::Mismatch);
        }
        Ok(issued)
    }
}

/// Issue the event owner reach of `problem` from its own owners.
pub fn issue_event_owner_reach(problem: &mut SolveProblem) {
    let reach = EventOwnerReach::derive(problem);
    problem.events.owner_reach = IssuedEventOwnerReach(Some(reach));
}
