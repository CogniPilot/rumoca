//! MLS §8.6: the outer initialization loop owns its update targets and the
//! relation memory; every other runtime binding holds during initialization.

use rumoca_ir_solve::ScalarSlot;
use std::collections::BTreeSet;

/// Runtime assignment rows that initialization binds before and after its
/// projection. A row is left to its owner when it writes an initialization
/// update target or a relation memory slot, which the initialization settle
/// refreshes from the relation evaluated at the solution itself.
pub(super) fn select_initial_binding_rows(
    runtime_targets: &[ScalarSlot],
    initialization_targets: &[ScalarSlot],
    relation_memory_parameter_indices: &[usize],
) -> Vec<usize> {
    let mut owned: BTreeSet<_> = initialization_targets
        .iter()
        .filter_map(storage_owner)
        .collect();
    owned.extend(
        relation_memory_parameter_indices
            .iter()
            .map(|&index| (true, index)),
    );
    runtime_targets
        .iter()
        .enumerate()
        .filter_map(|(row, target)| {
            storage_owner(target)
                .is_some_and(|owner| !owned.contains(&owner))
                .then_some(row)
        })
        .collect()
}

fn storage_owner(slot: &ScalarSlot) -> Option<(bool, usize)> {
    match *slot {
        ScalarSlot::Y { index, .. } => Some((false, index)),
        ScalarSlot::P { index, .. } => Some((true, index)),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(index: usize) -> ScalarSlot {
        ScalarSlot::P {
            index,
            byte_offset: index * 8,
        }
    }

    fn y(index: usize) -> ScalarSlot {
        ScalarSlot::Y {
            index,
            byte_offset: index * 8,
        }
    }

    #[test]
    fn initialization_owners_keep_their_targets_from_runtime_bindings() {
        // Rows 0 and 2 write initialization update targets and row 4 writes
        // relation memory; the independent bindings in rows 1 and 3 hold.
        let runtime_targets = [p(0), p(1), y(0), y(1), p(2)];
        assert_eq!(
            select_initial_binding_rows(&runtime_targets, &[p(0), y(0)], &[2]),
            vec![1, 3]
        );
        // Equal numeric indices in different storage lanes are distinct owners.
        assert_eq!(
            select_initial_binding_rows(&[p(0), y(0)], &[p(0)], &[]),
            vec![1]
        );
        assert_eq!(select_initial_binding_rows(&[y(0)], &[], &[0]), vec![0]);
    }
}
