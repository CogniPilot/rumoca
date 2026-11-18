use super::super::*;
use crate::{
    AlgebraicProjectionBlock, ProjectionBlocks, ProjectionStage, ScalarSlot,
    SolveVariableStorageRole, SolveVariableStorageRun, SolveVariableValueKind,
};

fn compact_plan(offset: usize) -> RefreshPlan {
    let block = AlgebraicProjectionBlock {
        rows: vec![10 + offset],
        y_indices: vec![20 + offset],
        tearing: None,
        alternate_charts: Vec::new(),
    };
    let blocks = ProjectionBlocks::from_stages(vec![ProjectionStage::SingletonDeclarationRun {
        source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
        first_row: 10,
        declaration: 0,
        storage: SolveVariableStorageRun {
            base: ScalarSlot::Y {
                index: 20,
                byte_offset: 20 * 8,
            },
            scalar_count: 3,
            role: SolveVariableStorageRole::Algebraic,
            value_kind: SolveVariableValueKind::Real,
        },
    }])
    .unwrap();
    RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan { blocks },
        simultaneous_block_indices: vec![30, 31, 32],
        value_stages: vec![RefreshStage::ProjectionBlock {
            seed_sequence: Default::default(),
            block_index: 30 + offset,
            plan: AlgebraicProjectionPlan {
                blocks: vec![block].into(),
            },
            seed_rows: RefreshRowSelection::default(),
        }],
        ..Default::default()
    }
}

#[test]
fn scalar_projection_replays_each_compact_singleton_without_expanding_the_owner() {
    for offset in 0..3 {
        let plan = compact_plan(offset);
        validate_refresh_plan("test", &plan).unwrap();
        assert_eq!(plan.simultaneous_plan.blocks.stages().len(), 1);
        assert!(
            plan.simultaneous_plan
                .blocks
                .explicit_block_at(offset)
                .is_none()
        );
    }
}

#[test]
fn compact_replay_rejects_a_different_row_target_or_coupled_block() {
    for difference in 0..3 {
        let mut plan = compact_plan(1);
        let RefreshStage::ProjectionBlock { plan: replay, .. } = &mut plan.value_stages[0] else {
            unreachable!();
        };
        let mut block = replay.blocks.explicit_block_at(0).unwrap().clone();
        match difference {
            0 => block.rows[0] += 1,
            1 => block.y_indices[0] += 1,
            _ => {
                block.rows.push(12);
                block.y_indices.push(22);
            }
        }
        replay.blocks = vec![block].into();
        assert!(validate_refresh_plan("test", &plan).is_err());
    }
}
