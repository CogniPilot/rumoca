//! Construction-issued coverage and remainder identities.

use super::*;

impl RefreshPlan {
    pub(super) fn issue_value_remainder_after(&self, settled: &Self) -> RefreshRemainderRelation {
        let settled_stages = refresh_stage_coverage(settled);
        let value_stages = self
            .value_stages
            .iter()
            .filter_map(|stage| uncovered_refresh_stage(self, stage, &settled_stages))
            .collect();
        let mut remainder = self.clone();
        remainder.value_stages = value_stages;
        if self.causal_solution_certified && settled.causal_solution_certified {
            let settled_rows = settled
                .causal_rows()
                .iter()
                .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id))
                .collect::<Vec<_>>();
            remainder.causal_seed_rows =
                uncovered_refresh_rows(self, &self.causal_seed_rows, &settled_rows);
            remainder.static_causal_seed_rows =
                uncovered_refresh_rows(self, &self.static_causal_seed_rows, &settled_rows);
            remainder.dynamic_causal_seed_rows =
                uncovered_refresh_rows(self, &self.dynamic_causal_seed_rows, &settled_rows);
        } else {
            // A staged remainder cannot inherit the complete plan's causal
            // certificate. Doing so would select an unfiltered causal schedule
            // and discard the construction-issued uncovered stages.
            remainder.causal_solution_certified = false;
        }
        RefreshRemainderRelation { remainder }
    }
}

#[derive(PartialEq)]
enum RefreshStageIdentity {
    ExactAssignment(RefreshRowOwnerId),
    ProjectionBlock(usize),
}

fn refresh_stage_coverage(plan: &RefreshPlan) -> Vec<RefreshStageIdentity> {
    let mut identities = Vec::new();
    for stage in &plan.value_stages {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
                ..
            }
            | RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
                ..
            } => identities.extend(
                plan.selected_rows(static_rows)
                    .iter()
                    .chain(plan.selected_rows(dynamic_rows).iter())
                    .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id)),
            ),
            RefreshStage::ProjectionBlock { block_index, .. } => {
                identities.push(RefreshStageIdentity::ProjectionBlock(*block_index));
            }
        }
    }
    identities
}

fn uncovered_refresh_rows(
    plan: &RefreshPlan,
    rows: &RefreshRowSelection,
    settled: &[RefreshStageIdentity],
) -> RefreshRowSelection {
    RefreshRowSelection(
        rows.indices()
            .iter()
            .copied()
            .filter(|index| {
                let row = &plan.rows[usize::try_from(*index)
                    .expect("checked continuous refresh row index fits usize")];
                !settled.contains(&RefreshStageIdentity::ExactAssignment(row.owner_id))
            })
            .collect(),
    )
}

fn uncovered_refresh_stage(
    plan: &RefreshPlan,
    stage: &RefreshStage,
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    match stage {
        RefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(plan, static_rows, dynamic_rows, settled, true),
        RefreshStage::ExactAssignments {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(plan, static_rows, dynamic_rows, settled, false),
        RefreshStage::ProjectionBlock { block_index, .. } => (!settled
            .contains(&RefreshStageIdentity::ProjectionBlock(*block_index)))
        .then(|| stage.clone()),
    }
}

fn uncovered_row_stage(
    plan: &RefreshPlan,
    static_rows: &RefreshRowSelection,
    dynamic_rows: &RefreshRowSelection,
    settled: &[RefreshStageIdentity],
    causal: bool,
) -> Option<RefreshStage> {
    let static_rows = uncovered_refresh_rows(plan, static_rows, settled);
    let dynamic_rows = uncovered_refresh_rows(plan, dynamic_rows, settled);
    if static_rows.is_empty() && dynamic_rows.is_empty() {
        return None;
    }
    Some(if causal {
        RefreshStage::CausalSeedSweep {
            static_sequence: RefreshSequenceId::default(),
            dynamic_sequence: RefreshSequenceId::default(),
            static_rows,
            dynamic_rows,
        }
    } else {
        RefreshStage::ExactAssignments {
            static_sequence: RefreshSequenceId::default(),
            dynamic_sequence: RefreshSequenceId::default(),
            static_rows,
            dynamic_rows,
        }
    })
}
