//! Read-only structural presence queries over a checked [`SolveProblem`].
//!
//! These are SPEC_0029 §3 IR query helpers: they walk the problem's own
//! partitions, evaluate nothing, and reach no phase crate. They report only
//! what a checked problem *contains*.
//!
//! Admissibility is deliberately elsewhere. Deciding whether a declared target
//! capability may consume a problem that contains one of these classes is
//! backend policy and stays in `rumoca-compile`'s target-capability
//! validation; deciding whether a storage-backed FMI template may render one
//! stays in [`crate::fmi`]. Both consume the same facts from here, so a class
//! cannot be recognised by one consumer and missed by the other.

use crate::SolveProblem;

/// One class of semantic event ownership a checked Solve problem may carry.
///
/// The classes partition the event domain by who owns the instant: the
/// compiler's discrete/event partitions, the runtime's history-bearing and
/// terminal events, and the clock partition. A consumer that must refuse every
/// event-bearing problem matches on the presence of a class rather than on a
/// field, so a partition added later reaches it without an edit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveEventClass {
    /// Discrete and event owners the compiler lowered: root conditions and
    /// their memories, scheduled and dynamic time events, event actions, and
    /// the discrete assignment/iteration partitions.
    Discrete,
    /// Events the runtime owns across accepted steps: `delay` history and the
    /// terminal event.
    Runtime,
    /// The clock partition and the discrete owners scoped to it.
    Clock,
}

/// The class of semantic event ownership a checked problem carries, or `None`
/// when it carries none at all.
///
/// This is the one composition of [`solve_has_events`],
/// [`solve_has_runtime_events`], and [`solve_has_clocks`]. A problem bearing
/// several classes reports the first in the order above; a consumer that only
/// asks "is this problem event-free" reads `is_none` and is unaffected by that
/// order.
#[must_use]
pub fn solve_event_class(problem: &SolveProblem) -> Option<SolveEventClass> {
    if solve_has_events(problem) {
        Some(SolveEventClass::Discrete)
    } else if solve_has_runtime_events(problem) {
        Some(SolveEventClass::Runtime)
    } else if solve_has_clocks(problem) {
        Some(SolveEventClass::Clock)
    } else {
        None
    }
}

/// Whether the checked problem carries any compiler-lowered discrete or event
/// owner.
#[must_use]
pub fn solve_has_events(problem: &SolveProblem) -> bool {
    let discrete = &problem.discrete;
    let events = &problem.events;
    !discrete.event_iteration_plan.runs.is_empty()
        || !discrete.runtime_assignment_rhs.is_empty()
        || !discrete.post_commit_assignment_rhs.is_empty()
        || !discrete.rhs.is_empty()
        || !discrete.structured_rhs.is_empty()
        || !discrete.structured_updates.is_empty()
        || !events.root_conditions.is_empty()
        || !events.condition_memory_parameter_indices.is_empty()
        || !events.scheduled_root_conditions.is_empty()
        || !events.scheduled_time_events.is_empty()
        || !events.dynamic_time_event_names.is_empty()
        || !events.dynamic_time_event_rhs.is_empty()
        || !events.action_conditions.is_empty()
        || !events.actions.is_empty()
}

/// Whether the checked problem carries an event the runtime owns across
/// accepted steps: `delay` history or the terminal event.
#[must_use]
pub fn solve_has_runtime_events(problem: &SolveProblem) -> bool {
    let events = &problem.events;
    let delays = &events.delays;
    events.has_terminal_event
        || !delays.source_rhs.is_empty()
        || !delays.delay_time_rhs.is_empty()
        || !delays.delay_max_rhs.is_empty()
        || !delays.value_parameter_indices.is_empty()
}

/// Whether the checked problem carries a clock partition or a clock-scoped
/// discrete owner.
#[must_use]
pub fn solve_has_clocks(problem: &SolveProblem) -> bool {
    !problem.clocks.periodic_event_schedules.is_empty()
        || !problem.clocks.activation_parameter_indices.is_empty()
        || problem.discrete.clock_owners.iter().any(Option::is_some)
        || problem
            .discrete
            .structured_updates
            .iter()
            .any(|update| update.clock_owner.is_some())
}

/// Whether the checked problem carries an initialization owner.
#[must_use]
pub fn solve_has_initialization(problem: &SolveProblem) -> bool {
    let initialization = &problem.initialization;
    !initialization.residual.is_empty()
        || !initialization.projection_unknowns.is_empty()
        || !initialization.projection_plan.is_empty()
        || !initialization.update_rhs.is_empty()
        || !initialization.update_targets.is_empty()
}
