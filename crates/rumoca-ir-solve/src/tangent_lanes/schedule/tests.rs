//! The issued rows of a torn tangent: slot layout, commit order, and refusals.
use super::*;
use crate::{BlockTearing, CausalStep};

fn span(offset: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("torn_tangent_schedule.mo"),
        offset,
        offset + 1,
    )
}

/// Row 0 `d1 + d0` solves y1 and row 1 `d1 - d0` is the reduced row of the
/// tear y0; the block's solver Y has three values.
fn plan() -> (TornTangentPlan, ScalarProgramBlock) {
    let row = |op| {
        vec![
            LinearOp::LoadSeed { dst: 0, index: 1 },
            LinearOp::LoadSeed { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]
    };
    let jvp = ScalarProgramBlock::with_program_spans(
        vec![row(BinaryOp::Add), row(BinaryOp::Sub)],
        vec![span(0), span(1)],
    )
    .unwrap();
    let tearing = BlockTearing {
        tear_y_indices: vec![0],
        residual_rows: vec![1],
        causal_steps: vec![CausalStep {
            row: 0,
            y_index: 1,
            ..Default::default()
        }],
    };
    (
        TornTangentPlan::derive_directional(&tearing, &jvp).unwrap(),
        jvp,
    )
}

#[test]
fn rows_commit_the_per_row_sweep_over_the_issued_slots() {
    let (plan, jvp) = plan();
    let schedule = plan.issue_schedule(&jvp, 3).unwrap();
    let slots = schedule.slots();
    assert_eq!(
        (
            slots.seed,
            slots.coefficient,
            slots.recovered,
            slots.residual
        ),
        (3, 6, 7, 8)
    );
    assert_eq!((slots.progress, slots.total), (9, 10));
    let rows = schedule.rows();
    assert_eq!(rows.slots(), 10);
    assert_eq!(rows.seed(), Some(3..6));
    // Seed the step; its coefficient, unseeded, the tear seeded, one group;
    // the step's recovered tangent to its seed and recovered slots; the
    // reduced row, then the seeds reset.
    assert_eq!(rows.targets(), [4, 6, 4, 3, 9, 4, 7, 8, 3, 4]);
    let mut targets = rows.targets();
    for program in rows.rows().programs() {
        let (tuple, rest) = targets.split_at(ScalarProgramBlock::program_output_count(program));
        let mut unique = tuple.to_vec();
        unique.sort_unstable();
        unique.dedup();
        assert_eq!(unique.len(), tuple.len(), "a tuple writes each slot once");
        targets = rest;
    }
    assert_eq!(
        rows.chunks()
            .map(|chunk| (chunk.start, chunk.end))
            .collect::<Vec<_>>(),
        [(0, 4)]
    );
    assert_eq!(schedule.committed_steps(0.0), Some(0));
    assert_eq!(schedule.committed_steps(1.0), Some(1));
    assert_eq!(schedule.committed_steps(2.0), None);
    assert_eq!(schedule.committed_steps(0.5), None);
}

#[test]
fn rows_the_plan_names_but_the_block_lacks_refuse_the_schedule() {
    let (plan, _) = plan();
    assert!(matches!(
        plan.issue_schedule(&ScalarProgramBlock::default(), 3),
        Err(TornTangentScheduleRefusal::MissingProgram { .. })
    ));
}

/// The persisted-product key of issued rows is their source identity: the
/// same plan over a fresh owner of the same JVP rows keeps it, while the plan
/// issued over another solver Y, whose slots and targets move, does not.
#[test]
fn the_issued_rows_key_follows_the_issued_source() {
    let (plan, jvp) = plan();
    let key = |plan: &TornTangentPlan, jvp: &ScalarProgramBlock, y_len| {
        let schedule = plan.issue_schedule(jvp, y_len).unwrap();
        crate::PreparedKernelSource::new(
            &crate::VarLayout::default(),
            &crate::SolvePureCallTable::default(),
        )
        .unwrap()
        .issued_assignment_rows(schedule.rows())
        .unwrap()
    };
    let replay: ScalarProgramBlock =
        serde_json::from_slice(&serde_json::to_vec(&jvp).unwrap()).unwrap();
    assert!(!jvp.shares_program_owner(&replay));
    let original = key(&plan, &jvp, 3);
    assert_eq!(original, key(&plan, &replay, 3));
    assert_ne!(original, key(&plan, &jvp, 4));
}
