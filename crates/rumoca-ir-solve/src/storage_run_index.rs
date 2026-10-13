//! Slot-to-variable lookup over the variable storage runs of a layout.
//!
//! Event-producer validation asks, for every producer target, which variable
//! storage run holds the slot. The runs are contiguous slot intervals, so the
//! index keeps them sorted by start per slot space and answers one slot by
//! binary search instead of scanning every run.

use super::{ScalarSlot, SolveLayout, SolveVariableStorageRole, SolveVariableStorageRun};

#[derive(Debug, Clone, Copy)]
struct IndexedRun {
    start: usize,
    end: usize,
    variable: usize,
    /// The largest `end` among this run and every run sorted before it.
    reach: usize,
}

/// More than one run holds the queried slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SharedSlot;

#[derive(Debug, Default)]
pub(crate) struct StorageRunIndex {
    parameters: Vec<IndexedRun>,
    states: Vec<IndexedRun>,
}

impl StorageRunIndex {
    /// Index the runs `keep` accepts, numbering each by its position in `runs`.
    pub(crate) fn new(
        runs: &[SolveVariableStorageRun],
        keep: impl Fn(&SolveVariableStorageRun) -> bool,
    ) -> Self {
        let mut parameters = Vec::new();
        let mut states = Vec::new();
        for (variable, run) in runs.iter().enumerate().filter(|(_, run)| keep(run)) {
            let (space, start) = match run.base {
                ScalarSlot::P { index, .. } => (&mut parameters, index),
                ScalarSlot::Y { index, .. } => (&mut states, index),
                ScalarSlot::Time | ScalarSlot::Constant(_) => continue,
            };
            if let Some(end) = start.checked_add(run.scalar_count) {
                space.push(IndexedRun {
                    start,
                    end,
                    variable,
                    reach: end,
                });
            }
        }
        for space in [&mut parameters, &mut states] {
            space.sort_by_key(|run| (run.start, run.variable));
            let mut reach = 0;
            for run in space.iter_mut() {
                reach = reach.max(run.end);
                run.reach = reach;
            }
        }
        Self { parameters, states }
    }

    /// The one run holding `slot`, `None` when no run holds it.
    pub(crate) fn owner(&self, slot: ScalarSlot) -> Result<Option<usize>, SharedSlot> {
        let (space, position) = match slot {
            ScalarSlot::P { index, .. } => (&self.parameters, index),
            ScalarSlot::Y { index, .. } => (&self.states, index),
            ScalarSlot::Time | ScalarSlot::Constant(_) => return Ok(None),
        };
        let after = space.partition_point(|run| run.start <= position);
        let mut holders = space[..after]
            .iter()
            .rev()
            .take_while(|run| run.reach > position)
            .filter(|run| run.end > position);
        let owner = holders.next().map(|run| run.variable);
        if holders.next().is_some() {
            return Err(SharedSlot);
        }
        Ok(owner)
    }

    /// Whether any indexed run holds `slot`.
    pub(crate) fn holds(&self, slot: ScalarSlot) -> bool {
        !matches!(self.owner(slot), Ok(None))
    }
}

/// The storage runs a producer target is validated against: every run, and
/// the external-input runs alone, each indexed once per validation.
pub(crate) struct ProducerStorage {
    all: StorageRunIndex,
    external_inputs: StorageRunIndex,
}

impl ProducerStorage {
    pub(crate) fn new(layout: &SolveLayout) -> Self {
        let runs = &layout.variable_storage_runs;
        Self {
            all: StorageRunIndex::new(runs, |_| true),
            external_inputs: StorageRunIndex::new(runs, |run| {
                run.role == SolveVariableStorageRole::ExternalInput
            }),
        }
    }

    /// The variable whose storage run holds `slot`.
    pub(crate) fn variable(&self, slot: ScalarSlot) -> Result<Option<usize>, &'static str> {
        self.all
            .owner(slot)
            .map_err(|SharedSlot| "one producer slot belongs to multiple variable-storage runs")
    }

    pub(crate) fn external_input(&self, slot: ScalarSlot) -> bool {
        self.external_inputs.holds(slot)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SolveVariableValueKind;

    fn run(base: ScalarSlot, scalar_count: usize) -> SolveVariableStorageRun {
        SolveVariableStorageRun {
            base,
            scalar_count,
            role: SolveVariableStorageRole::DiscreteValue,
            value_kind: SolveVariableValueKind::Real,
        }
    }

    fn slot_y(index: usize) -> ScalarSlot {
        ScalarSlot::Y {
            index,
            byte_offset: 0,
        }
    }

    /// The scan the index replaces, kept as the oracle.
    fn scanned(
        runs: &[SolveVariableStorageRun],
        slot: ScalarSlot,
    ) -> Result<Option<usize>, SharedSlot> {
        let mut owners = runs.iter().enumerate().filter_map(|(variable, run)| {
            let contains = match (run.base, slot) {
                (ScalarSlot::P { index: base, .. }, ScalarSlot::P { index, .. })
                | (ScalarSlot::Y { index: base, .. }, ScalarSlot::Y { index, .. }) => base
                    .checked_add(run.scalar_count)
                    .is_some_and(|end| (base..end).contains(&index)),
                _ => false,
            };
            contains.then_some(variable)
        });
        let first = owners.next();
        if owners.next().is_some() {
            return Err(SharedSlot);
        }
        Ok(first)
    }

    #[test]
    fn index_answers_every_slot_as_the_scan_over_all_runs_does() {
        // Disjoint runs, a gap, a nested overlap and a run spanning a gap.
        let runs = vec![
            run(slot_y(10), 4),
            run(slot_y(0), 3),
            run(slot_y(3), 2),
            run(slot_y(20), 10),
            run(slot_y(22), 2),
            run(slot_y(40), 0),
            run(
                ScalarSlot::P {
                    index: 5,
                    byte_offset: 0,
                },
                3,
            ),
            run(ScalarSlot::Time, 1),
        ];
        let index = StorageRunIndex::new(&runs, |_| true);
        for position in 0..50 {
            for slot in [
                slot_y(position),
                ScalarSlot::P {
                    index: position,
                    byte_offset: 0,
                },
            ] {
                assert_eq!(index.owner(slot), scanned(&runs, slot), "slot {slot:?}");
            }
        }
        assert_eq!(index.owner(ScalarSlot::Time), Ok(None));
        assert_eq!(index.owner(slot_y(23)), Err(SharedSlot));
        assert!(index.holds(slot_y(23)));
        assert!(!index.holds(slot_y(7)));
    }

    #[test]
    fn index_keeps_only_the_runs_its_filter_accepts() {
        let mut external = run(slot_y(0), 2);
        external.role = SolveVariableStorageRole::ExternalInput;
        let runs = vec![external, run(slot_y(2), 2)];
        let index = StorageRunIndex::new(&runs, |run| {
            run.role == SolveVariableStorageRole::ExternalInput
        });
        assert!(index.holds(slot_y(1)));
        assert!(!index.holds(slot_y(2)));
    }
}
