use std::collections::BTreeMap;

use rumoca_ir_solve as solve;

use super::{AlgebraicRefreshRow, RefreshRowSelection, RefreshStage};

pub fn build_refresh_stages(
    plan: &solve::AlgebraicProjectionPlan,
    block_indices: &[usize],
    rows: &[AlgebraicRefreshRow],
    static_rows: &RefreshRowSelection,
) -> Result<Vec<RefreshStage>, solve::ContinuousRefreshConstructionError> {
    let exact_rows = rows
        .iter()
        .enumerate()
        .filter(|(_, row)| row.exact_assignment_certified())
        .map(|(index, row)| ((row.equation_index(), row.target_index()), index))
        .collect::<BTreeMap<_, _>>();
    let mut stages = Vec::new();
    let mut assignments = Vec::new();
    let static_targets = static_rows
        .indices()
        .iter()
        .filter_map(|index| usize::try_from(*index).ok())
        .filter_map(|index| rows.get(index))
        .map(AlgebraicRefreshRow::target_index)
        .collect::<std::collections::BTreeSet<_>>();
    push_causal_seed_sweep(&mut stages, rows, &static_targets)?;
    for (local_block_index, block) in plan.blocks.iter().enumerate() {
        if let Some(row) = exact_singleton_row(block, &exact_rows) {
            assignments.push(row);
            continue;
        }
        flush_assignments(&mut stages, &mut assignments, rows, &static_targets)?;
        stages.push(RefreshStage::ProjectionBlock {
            seed_sequence: Default::default(),
            block_index: block_indices
                .get(local_block_index)
                .copied()
                .unwrap_or(local_block_index),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![block.clone()],
            },
            seed_rows: projection_seed_rows(block, rows)?,
        });
    }
    flush_assignments(&mut stages, &mut assignments, rows, &static_targets)?;
    Ok(stages)
}

fn push_causal_seed_sweep(
    stages: &mut Vec<RefreshStage>,
    rows: &[AlgebraicRefreshRow],
    static_targets: &std::collections::BTreeSet<usize>,
) -> Result<(), solve::ContinuousRefreshConstructionError> {
    if rows.is_empty() {
        return Ok(());
    }
    let (static_rows, dynamic_rows) = rows
        .iter()
        .enumerate()
        .partition::<Vec<_>, _>(|(_, row)| static_targets.contains(&row.target_index()));
    stages.push(RefreshStage::CausalSeedSweep {
        static_sequence: Default::default(),
        dynamic_sequence: Default::default(),
        static_rows: RefreshRowSelection::checked(
            rows.len(),
            static_rows.into_iter().map(|(index, _)| index),
        )?,
        dynamic_rows: RefreshRowSelection::checked(
            rows.len(),
            dynamic_rows.into_iter().map(|(index, _)| index),
        )?,
    });
    Ok(())
}

fn projection_seed_rows(
    block: &solve::AlgebraicProjectionBlock,
    rows: &[AlgebraicRefreshRow],
) -> Result<RefreshRowSelection, solve::ContinuousRefreshConstructionError> {
    let block_targets = block
        .y_indices
        .iter()
        .copied()
        .collect::<std::collections::BTreeSet<_>>();
    RefreshRowSelection::checked(
        rows.len(),
        rows.iter()
            .enumerate()
            .filter(|(_, row)| block_targets.contains(&row.target_index()))
            .map(|(index, _)| index),
    )
}

fn exact_singleton_row(
    block: &solve::AlgebraicProjectionBlock,
    exact_rows: &BTreeMap<(usize, usize), usize>,
) -> Option<usize> {
    let [equation_index] = block.rows.as_slice() else {
        return None;
    };
    let [target_index] = block.y_indices.as_slice() else {
        return None;
    };
    exact_rows.get(&(*equation_index, *target_index)).copied()
}

fn flush_assignments(
    stages: &mut Vec<RefreshStage>,
    assignments: &mut Vec<usize>,
    rows: &[AlgebraicRefreshRow],
    static_targets: &std::collections::BTreeSet<usize>,
) -> Result<(), solve::ContinuousRefreshConstructionError> {
    if assignments.is_empty() {
        return Ok(());
    }
    let (static_rows, dynamic_rows) = std::mem::take(assignments)
        .into_iter()
        .partition::<Vec<_>, _>(|index| static_targets.contains(&rows[*index].target_index()));
    stages.push(RefreshStage::ExactAssignments {
        static_sequence: Default::default(),
        dynamic_sequence: Default::default(),
        static_rows: RefreshRowSelection::checked(rows.len(), static_rows)?,
        dynamic_rows: RefreshRowSelection::checked(rows.len(), dynamic_rows)?,
    });
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn exact_row(index: usize) -> AlgebraicRefreshRow {
        AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
            owner_id: super::super::RefreshRowOwnerId::checked(index).unwrap(),
            source: solve::RefreshScalarProgramSource::checked(0, index).unwrap(),
            equation_index: index,
            output_offset: 0,
            target_index: index,
            assignment_target: Some(index),
            assignment_shape: Some(solve::TargetAssignmentShape::Direct {
                target_y_index: index,
                expr_reg: 0,
                target_scale: 1.0,
                expr_eval_len: 1,
            }),
            direct_assignment_certified: true,
            exact_assignment_certified: true,
        })
        .unwrap()
    }

    fn numerical_seed_row(index: usize) -> AlgebraicRefreshRow {
        AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
            owner_id: super::super::RefreshRowOwnerId::checked(index).unwrap(),
            source: solve::RefreshScalarProgramSource::checked(0, index).unwrap(),
            equation_index: index,
            output_offset: 0,
            target_index: index,
            assignment_target: Some(index),
            assignment_shape: None,
            direct_assignment_certified: false,
            exact_assignment_certified: false,
        })
        .unwrap()
    }

    #[test]
    fn exact_runs_do_not_cross_coupled_projection_blocks() {
        let plan = solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![1, 2],
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![3],
                    y_indices: vec![3],
                },
            ],
        };
        let stages = build_refresh_stages(
            &plan,
            &[7, 11, 13],
            &[
                exact_row(0),
                numerical_seed_row(1),
                numerical_seed_row(2),
                exact_row(3),
            ],
            &RefreshRowSelection::default(),
        )
        .unwrap();

        assert!(matches!(
            stages.as_slice(),
            [
                RefreshStage::CausalSeedSweep { dynamic_rows, .. },
                RefreshStage::ExactAssignments { dynamic_rows: before, .. },
                RefreshStage::ProjectionBlock { block_index: 11, seed_rows, .. },
                RefreshStage::ExactAssignments { dynamic_rows: after, .. },
            ] if dynamic_rows.indices() == [0, 1, 2, 3]
                && before.indices() == [0]
                && seed_rows.len() == 2
                && seed_rows.indices() == [1, 2]
                && after.indices() == [3]
        ));
    }

    #[test]
    fn reversed_causal_seed_order_is_preserved_before_projection() {
        let plan = solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                },
            ],
        };

        let rows = [exact_row(1), exact_row(0)];
        let stages =
            build_refresh_stages(&plan, &[0, 1], &rows, &RefreshRowSelection::default()).unwrap();

        assert!(matches!(
            stages.as_slice(),
            [
                RefreshStage::CausalSeedSweep { dynamic_rows, .. },
                RefreshStage::ExactAssignments { .. },
            ]
                if dynamic_rows.indices() == [0, 1]
                    && rows[0].target_index() == 1
                    && rows[1].target_index() == 0
        ));
    }
}
