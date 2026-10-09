//! Which storage the events of a Solve problem can write.

use std::collections::BTreeSet;

use crate::{ScalarSlot, SolveProblem};

/// The storage events may write, over-approximated by what each event owner
/// names as a target.
///
/// A consumer asking "can an event change what this program reads" intersects
/// `parameters` with the program's reads and refuses on `solver_y` or `opaque`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EventWrites {
    /// An event writes a solver coordinate (a state reset or algebraic update).
    pub solver_y: bool,
    /// Parameter storage indices an event writes: discrete values, relation and
    /// condition memory, `pre` copies, and clock activation lanes.
    pub parameters: BTreeSet<usize>,
    /// An event owner whose targets are not enumerated here (an aggregate
    /// transaction or a structured update) exists, so no bound is claimed.
    pub opaque: bool,
}

impl EventWrites {
    fn write(&mut self, slot: ScalarSlot, count: usize) {
        match slot {
            ScalarSlot::Y { .. } => self.solver_y = true,
            ScalarSlot::P { index, .. } => self.parameters.extend(index..index + count),
            ScalarSlot::Time | ScalarSlot::Constant(_) => {}
        }
    }
}

/// Every storage index some event owner of `problem` writes.
#[must_use]
pub fn event_writes(problem: &SolveProblem) -> EventWrites {
    let discrete = &problem.discrete;
    let events = &problem.events;
    let mut writes = EventWrites {
        opaque: !discrete.event_transactions.is_empty() || !discrete.structured_updates.is_empty(),
        ..EventWrites::default()
    };
    let scalar_targets = discrete
        .update_targets
        .iter()
        .chain(&discrete.runtime_assignment_targets)
        .chain(&discrete.post_commit_assignment_targets)
        .chain(events.root_relation_memory_targets.iter().flatten());
    for slot in scalar_targets {
        writes.write(*slot, 1);
    }
    for owner in &discrete.guarded_assignments {
        for range in owner.target_ranges() {
            writes.write(range.base(), range.count());
        }
    }
    let indices = events
        .condition_memory_parameter_indices
        .iter()
        .chain(&problem.clocks.activation_parameter_indices)
        .copied()
        .chain(
            problem
                .solve_layout
                .pre_param_bindings
                .iter()
                .map(|binding| binding.dest_p_index),
        );
    writes.parameters.extend(indices);
    writes
}
