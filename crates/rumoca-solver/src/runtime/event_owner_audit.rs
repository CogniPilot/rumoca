//! Test-only audit of the owner evaluations inside one event update.
//!
//! An event update evaluates its root conditions, discrete rows and
//! condition-memory rows once per step of the issued schedule that needs
//! them. Tests count those evaluations on one thread, force the full path
//! that evaluates every owner at every step, and compare the bits both paths
//! leave behind. The surface exists only for this crate's tests and for the
//! dev-only `input-write-audit` feature.

use std::cell::Cell;

/// Owner evaluations on this thread since the counts were last read.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct EventOwnerCounts {
    /// Event updates entered (a held continuation is not one).
    pub events: usize,
    /// Outer passes of the event fixed point.
    pub outer_passes: usize,
    /// Relation-memory refreshes inside the event fixed point.
    pub relation_refreshes: usize,
    /// Relation-memory refreshes after the commit.
    pub post_commit_relation_refreshes: usize,
    /// Root-condition programs evaluated, at any step.
    pub root_evaluations: usize,
    /// Root-condition programs a refresh took from their last evaluation.
    pub root_evaluations_skipped: usize,
    /// Discrete-owner settle passes.
    pub discrete_passes: usize,
    /// Condition-memory advances.
    pub condition_memory_passes: usize,
    /// Discrete-row programs evaluated by a settle pass or a condition-memory
    /// advance.
    pub discrete_programs_evaluated: usize,
    /// Discrete-row programs those passes took from their last evaluation.
    pub discrete_programs_skipped: usize,
    /// Event-action evaluations.
    pub action_evaluations: usize,
}

impl std::ops::Sub for EventOwnerCounts {
    type Output = Self;

    fn sub(self, before: Self) -> Self {
        Self {
            events: self.events - before.events,
            outer_passes: self.outer_passes - before.outer_passes,
            relation_refreshes: self.relation_refreshes - before.relation_refreshes,
            post_commit_relation_refreshes: self.post_commit_relation_refreshes
                - before.post_commit_relation_refreshes,
            root_evaluations: self.root_evaluations - before.root_evaluations,
            root_evaluations_skipped: self.root_evaluations_skipped
                - before.root_evaluations_skipped,
            discrete_passes: self.discrete_passes - before.discrete_passes,
            condition_memory_passes: self.condition_memory_passes - before.condition_memory_passes,
            discrete_programs_evaluated: self.discrete_programs_evaluated
                - before.discrete_programs_evaluated,
            discrete_programs_skipped: self.discrete_programs_skipped
                - before.discrete_programs_skipped,
            action_evaluations: self.action_evaluations - before.action_evaluations,
        }
    }
}

thread_local! {
    static FORCE_FULL_EVENT_UPDATE: Cell<bool> = const { Cell::new(false) };
    static COUNTS: Cell<EventOwnerCounts> = const {
        Cell::new(EventOwnerCounts {
            events: 0,
            outer_passes: 0,
            relation_refreshes: 0,
            post_commit_relation_refreshes: 0,
            root_evaluations: 0,
            root_evaluations_skipped: 0,
            discrete_passes: 0,
            condition_memory_passes: 0,
            discrete_programs_evaluated: 0,
            discrete_programs_skipped: 0,
            action_evaluations: 0,
        })
    };
}

/// Evaluate every owner at every step of each event update on this thread,
/// whatever the issued event owner reach admits.
pub fn force_full_event_update(forced: bool) {
    FORCE_FULL_EVENT_UPDATE.with(|force| force.set(forced));
}

pub(crate) fn full_event_update_forced() -> bool {
    FORCE_FULL_EVENT_UPDATE.with(Cell::get)
}

/// This thread's owner evaluations so far.
#[must_use]
pub fn event_owner_counts() -> EventOwnerCounts {
    COUNTS.with(Cell::get)
}

pub(crate) fn count(update: impl FnOnce(&mut EventOwnerCounts)) {
    COUNTS.with(|counts| {
        let mut current = counts.get();
        update(&mut current);
        counts.set(current);
    });
}
