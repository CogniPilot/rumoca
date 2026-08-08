use std::collections::BTreeMap;

use rumoca_ir_solve as solve;

use super::AlgebraicRefreshRow;

/// One proof-preserving stage in an algebraic value refresh.
#[derive(Clone)]
pub enum RefreshStage {
    /// Complete compiler-issued causal warm-start in its certified order.
    CausalSeedSweep {
        static_rows: Box<[AlgebraicRefreshRow]>,
        dynamic_rows: Box<[AlgebraicRefreshRow]>,
    },
    /// Consecutive BLT singleton blocks whose target isolators are exact.
    ExactAssignments {
        static_rows: Box<[AlgebraicRefreshRow]>,
        dynamic_rows: Box<[AlgebraicRefreshRow]>,
    },
    /// A block that still requires numerical projection.
    ProjectionBlock {
        block_index: usize,
        plan: solve::AlgebraicProjectionPlan,
        seed_rows: Box<[AlgebraicRefreshRow]>,
    },
}

pub fn build_refresh_stages(
    plan: &solve::AlgebraicProjectionPlan,
    block_indices: &[usize],
    rows: &[AlgebraicRefreshRow],
    static_rows: &[AlgebraicRefreshRow],
) -> Vec<RefreshStage> {
    let exact_rows = rows
        .iter()
        .filter(|row| row.exact_assignment_certified)
        .map(|row| ((row.equation_index, row.target_index), row))
        .collect::<BTreeMap<_, _>>();
    let mut stages = Vec::new();
    let mut assignments = Vec::new();
    let static_targets = static_rows
        .iter()
        .map(|row| row.target_index)
        .collect::<std::collections::BTreeSet<_>>();
    push_causal_seed_sweep(&mut stages, rows, &static_targets);
    for (local_block_index, block) in plan.blocks.iter().enumerate() {
        if let Some(row) = exact_singleton_row(block, &exact_rows) {
            assignments.push(row.clone());
            continue;
        }
        flush_assignments(&mut stages, &mut assignments, &static_targets);
        stages.push(RefreshStage::ProjectionBlock {
            block_index: block_indices
                .get(local_block_index)
                .copied()
                .unwrap_or(local_block_index),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![block.clone()],
            },
            seed_rows: projection_seed_rows(block, rows),
        });
    }
    flush_assignments(&mut stages, &mut assignments, &static_targets);
    stages
}

fn push_causal_seed_sweep(
    stages: &mut Vec<RefreshStage>,
    rows: &[AlgebraicRefreshRow],
    static_targets: &std::collections::BTreeSet<usize>,
) {
    if rows.is_empty() {
        return;
    }
    let (static_rows, dynamic_rows) = rows
        .iter()
        .cloned()
        .partition::<Vec<_>, _>(|row| static_targets.contains(&row.target_index));
    stages.push(RefreshStage::CausalSeedSweep {
        static_rows: static_rows.into_boxed_slice(),
        dynamic_rows: dynamic_rows.into_boxed_slice(),
    });
}

fn projection_seed_rows(
    block: &solve::AlgebraicProjectionBlock,
    rows: &[AlgebraicRefreshRow],
) -> Box<[AlgebraicRefreshRow]> {
    let block_targets = block
        .y_indices
        .iter()
        .copied()
        .collect::<std::collections::BTreeSet<_>>();
    rows.iter()
        .filter(|row| block_targets.contains(&row.target_index))
        .cloned()
        .collect()
}

fn exact_singleton_row<'a>(
    block: &solve::AlgebraicProjectionBlock,
    exact_rows: &BTreeMap<(usize, usize), &'a AlgebraicRefreshRow>,
) -> Option<&'a AlgebraicRefreshRow> {
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
    assignments: &mut Vec<AlgebraicRefreshRow>,
    static_targets: &std::collections::BTreeSet<usize>,
) {
    if assignments.is_empty() {
        return;
    }
    let (static_rows, dynamic_rows) = std::mem::take(assignments)
        .into_iter()
        .partition::<Vec<_>, _>(|row| static_targets.contains(&row.target_index));
    stages.push(RefreshStage::ExactAssignments {
        static_rows: static_rows.into_boxed_slice(),
        dynamic_rows: dynamic_rows.into_boxed_slice(),
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::refresh_plan::RefreshPlan;
    use std::sync::Arc;

    fn exact_row(index: usize) -> AlgebraicRefreshRow {
        AlgebraicRefreshRow {
            equation_index: index,
            row_idx: index,
            output_offset: 0,
            target_index: index,
            assignment_target: Some(index),
            assignment_shape: None,
            direct_assignment_certified: true,
            exact_assignment_certified: true,
        }
    }

    fn numerical_seed_row(index: usize) -> AlgebraicRefreshRow {
        AlgebraicRefreshRow {
            exact_assignment_certified: false,
            ..exact_row(index)
        }
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
            &[],
        );

        assert!(matches!(
            stages.as_slice(),
            [
                RefreshStage::CausalSeedSweep { dynamic_rows, .. },
                RefreshStage::ExactAssignments { dynamic_rows: before, .. },
                RefreshStage::ProjectionBlock { block_index: 11, seed_rows, .. },
                RefreshStage::ExactAssignments { dynamic_rows: after, .. },
            ] if dynamic_rows.iter().map(|row| row.target_index).eq(0..4)
                && before[0].target_index == 0
                && seed_rows.len() == 2
                && seed_rows[0].target_index == 1
                && seed_rows[1].target_index == 2
                && after[0].target_index == 3
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

        let stages = build_refresh_stages(&plan, &[0, 1], &[exact_row(1), exact_row(0)], &[]);

        assert!(matches!(
            stages.as_slice(),
            [
                RefreshStage::CausalSeedSweep { dynamic_rows, .. },
                RefreshStage::ExactAssignments { .. },
            ]
                if dynamic_rows[0].target_index == 1 && dynamic_rows[1].target_index == 0
        ));
    }

    #[test]
    fn derivative_settle_remainder_keeps_only_uncovered_root_stages() {
        let source = Arc::new(
            solve::ScalarProgramBlock::with_program_spans(Vec::new(), Vec::new()).unwrap(),
        );
        let projection = RefreshStage::ProjectionBlock {
            block_index: 11,
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![1, 2],
                }],
            },
            seed_rows: Box::new([]),
        };
        let root = RefreshPlan {
            source_block: Arc::clone(&source),
            value_stages: vec![exact_stage(0), projection.clone(), exact_stage(3)],
            ..RefreshPlan::default()
        };
        let derivative = RefreshPlan {
            source_block: source,
            value_stages: vec![exact_stage(0), exact_stage(9), projection],
            ..RefreshPlan::default()
        };

        let remainder = root
            .certified_value_remainder_after(&derivative)
            .expect("plans share construction identity");
        assert!(matches!(
            remainder.value_stages.as_slice(),
            [RefreshStage::ExactAssignments { dynamic_rows, .. }]
                if dynamic_rows.len() == 1 && dynamic_rows[0].target_index == 3
        ));
    }

    fn exact_stage(index: usize) -> RefreshStage {
        RefreshStage::ExactAssignments {
            static_rows: Box::new([]),
            dynamic_rows: vec![exact_row(index)].into_boxed_slice(),
        }
    }
}
