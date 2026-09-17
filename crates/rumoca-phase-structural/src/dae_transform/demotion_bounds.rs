//! Source-owned row bounds for direct-state candidate search.
//!
//! Checked function bodies reject model coordinates, so calls can expose state
//! reads only through their arguments. Counting the entire containing owner
//! also covers tensor projections without enumerating their scalar coordinates.

use rumoca_ir_dae as dae;

use super::ManifoldConstraint;

pub(super) struct DemotionRowBounds {
    state_rows: Vec<usize>,
    owner_rows: Vec<usize>,
    /// Continuous owners that reference each variable in any coordinate form,
    /// in ascending owner order. A direct-state demotion rewrites exactly the
    /// owners that mention the demoted variable, so this is the set of rows the
    /// incremental incidence reprojects; every other row is reused unchanged.
    variable_owners: Vec<Vec<usize>>,
}

impl DemotionRowBounds {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let mut bounds = Self {
            state_rows: vec![0; view.variable_count()],
            owner_rows: Vec::new(),
            variable_owners: vec![Vec::new(); view.variable_count()],
        };
        let mut traversal = dae::ExpressionTraversal::new();
        let mut seen = vec![0; view.variable_count()];
        let mut roots = Vec::new();
        for (owner_index, owner) in view.continuous_owners().enumerate() {
            roots.clear();
            let rows = match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    roots.push(equation.residual());
                    view.expression(equation.residual())
                        .expect("checked residual resolves")
                        .value_type()
                        .scalar_count()
                        .expect("checked continuous residual has finite shape")
                }
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    roots.extend(family.bodies().iter());
                    family.scalar_rows() as usize
                }
            };
            bounds.owner_rows.push(rows);
            let stamp = bounds.owner_rows.len();
            let mut accumulator = CoordinateAccumulator {
                seen: &mut seen,
                counts: &mut bounds.state_rows,
                variable_owners: &mut bounds.variable_owners,
            };
            traversal.visit_pruned(view, roots.iter().copied(), |_, node| {
                accumulator.record(
                    node,
                    OwnerRows {
                        stamp,
                        owner_index,
                        rows,
                    },
                );
                true
            });
        }
        bounds
    }

    /// Mask of the continuous owners a demotion of `variable` rewrites.
    ///
    /// Every owner referencing the variable in any coordinate is `true`, so the
    /// incremental incidence reprojects exactly those and reuses the rest.
    pub(super) fn touched_owners(&self, variable: u32) -> Vec<bool> {
        let mut mask = vec![false; self.owner_rows.len()];
        let owners = self.variable_owners.get(variable as usize);
        for &owner in owners.into_iter().flatten() {
            if let Some(entry) = mask.get_mut(owner) {
                *entry = true;
            }
        }
        mask
    }

    pub(super) fn cannot_sort(
        &self,
        state: u32,
        residue: usize,
        manifold: &[ManifoldConstraint],
    ) -> bool {
        let mut rows = self.state_rows[state as usize];
        for entry in manifold {
            if let Some(lifted) = entry.lifted
                && lifted.state == state
            {
                rows = rows.saturating_add(self.owner_rows[lifted.owner_ordinal]);
            }
        }
        // Removing changed rows from a new matching leaves a source matching.
        // E and U are unchanged, so E + U - 2*M can fall by at most twice rows.
        residue > rows.saturating_mul(2)
    }
}

/// The current owner's stamp, ordinal, and scalar-row count.
#[derive(Clone, Copy)]
struct OwnerRows {
    stamp: usize,
    owner_index: usize,
    rows: usize,
}

/// Per-variable accumulators shared across one collection pass.
struct CoordinateAccumulator<'a> {
    seen: &'a mut [usize],
    counts: &'a mut [usize],
    variable_owners: &'a mut [Vec<usize>],
}

impl CoordinateAccumulator<'_> {
    fn record(&mut self, node: dae::ExpressionView<'_>, owner: OwnerRows) {
        let Some(variable) = node.variable_coordinate() else {
            return;
        };
        let index = variable.index() as usize;
        if self.seen[index] != owner.stamp {
            self.seen[index] = owner.stamp;
            self.counts[index] = self.counts[index].saturating_add(owner.rows);
            self.variable_owners[index].push(owner.owner_index);
        }
    }
}
