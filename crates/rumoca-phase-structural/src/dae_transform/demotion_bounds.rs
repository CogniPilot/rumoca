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
}

impl DemotionRowBounds {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let mut bounds = Self {
            state_rows: vec![0; view.variable_count()],
            owner_rows: Vec::new(),
        };
        let mut traversal = dae::ExpressionTraversal::new();
        let mut seen = vec![0; view.variable_count()];
        let mut roots = Vec::new();
        for owner in view.continuous_owners() {
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
            traversal.visit_pruned(view, roots.iter().copied(), |_, node| {
                record_coordinate_rows(node, stamp, rows, &mut seen, &mut bounds.state_rows);
                true
            });
        }
        bounds
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

fn record_coordinate_rows(
    node: dae::ExpressionView<'_>,
    stamp: usize,
    rows: usize,
    seen: &mut [usize],
    counts: &mut [usize],
) {
    let Some(variable) = node.variable_coordinate() else {
        return;
    };
    let index = variable.index() as usize;
    if seen[index] != stamp {
        seen[index] = stamp;
        counts[index] = counts[index].saturating_add(rows);
    }
}
