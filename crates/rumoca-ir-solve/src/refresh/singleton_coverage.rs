//! Exact singleton block membership over one borrowed canonical row inventory.

#[cfg(test)]
mod tests;

use std::collections::BTreeSet;

use super::{AlgebraicRefreshRow, RefreshPlan, RefreshRowOwnerId};

struct SingletonRows<'plan>(Vec<&'plan AlgebraicRefreshRow>);

impl<'plan> SingletonRows<'plan> {
    fn new(rows: &'plan [AlgebraicRefreshRow]) -> Option<Self> {
        let mut indexed = Vec::new();
        indexed.try_reserve_exact(rows.len()).ok()?;
        indexed.extend(rows);
        indexed.sort_unstable_by_key(|row| key(row));
        Some(Self(indexed))
    }

    fn unique_owner(&self, equation: usize, target: usize) -> Option<RefreshRowOwnerId> {
        let requested = (equation, target);
        let position = self.0.partition_point(|row| key(row) < requested);
        let matches = self.0.get(position..)?;
        let row = matches.first()?;
        if key(row) != requested || matches.get(1).is_some_and(|next| key(next) == requested) {
            return None;
        }
        Some(row.owner_id)
    }
}

fn key(row: &AlgebraicRefreshRow) -> (usize, usize) {
    (row.equation_index, row.target_index)
}

pub(super) fn exact_singleton_row_owners(
    plan: &RefreshPlan,
) -> Option<BTreeSet<RefreshRowOwnerId>> {
    let rows = SingletonRows::new(&plan.rows)?;
    let mut owners = BTreeSet::new();
    for block in &plan.simultaneous_plan.blocks {
        let ([equation], [target]) = (block.rows.as_slice(), block.y_indices.as_slice()) else {
            return None;
        };
        if !owners.insert(rows.unique_owner(*equation, *target)?) {
            return None;
        }
    }
    (owners.len() == plan.rows.len()).then_some(owners)
}
