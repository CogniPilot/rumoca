use rumoca_ir_solve::{
    AlgebraicProjectionBlock, RefreshScalarProgramSource, SolveVariableStorageRole,
    SolveVariableStorageRun, SolveVariableValueKind,
};

use super::*;

/// One singleton block per `(row, target)`, in issued order.
fn singletons(targets: &[usize]) -> Vec<AlgebraicProjectionBlock> {
    targets
        .iter()
        .enumerate()
        .map(|(row, &target)| AlgebraicProjectionBlock {
            rows: vec![row],
            y_indices: vec![target],
            tearing: None,
            alternate_charts: Vec::new(),
        })
        .collect()
}

fn order(targets: &[usize], reads: &[&[usize]]) -> Option<Vec<usize>> {
    let blocks = singletons(targets);
    let reads = reads
        .iter()
        .map(|row| Some(Arc::new(IndexIntervals::of(row.iter().copied()))))
        .collect::<Vec<_>>();
    execution_order(
        &blocks
            .into_iter()
            .map(ProjectionStage::Explicit)
            .collect::<Vec<_>>(),
        |row| reads.get(row)?.as_deref(),
    )
}

/// `der(x[1])` (target 10) is issued first and loads `uu[2]` (target 11) with
/// its tensor program; `uu[2] = x[1]` reads neither.
#[test]
fn a_block_runs_after_the_later_block_its_execution_reads() {
    assert_eq!(order(&[10, 11], &[&[11], &[]]), Some(vec![1, 0]));
}

#[test]
fn an_order_that_already_produces_every_read_is_kept() {
    assert_eq!(order(&[10, 11, 12], &[&[], &[10], &[10, 11]]), None);
}

#[test]
fn blocks_no_read_moves_keep_their_issued_order() {
    assert_eq!(
        order(&[10, 11, 12, 13], &[&[12], &[], &[], &[11]]),
        Some(vec![2, 0, 1, 3])
    );
}

/// Each block reads the other's target: no order produces both reads, so the
/// issued order and its refusal stay.
#[test]
fn an_execution_cycle_keeps_the_issued_order() {
    assert_eq!(order(&[10, 11], &[&[11], &[10]]), None);
}

#[test]
fn a_cycle_does_not_stop_the_other_blocks_from_moving() {
    assert_eq!(
        order(&[10, 11, 12, 13], &[&[11], &[10], &[13], &[]]),
        Some(vec![0, 1, 3, 2])
    );
}

#[test]
fn a_read_of_a_value_no_block_produces_orders_nothing() {
    assert_eq!(order(&[10, 11], &[&[3, 99], &[10]]), None);
}

#[test]
fn a_reader_inside_a_range_moves_after_the_run_and_its_producer() {
    let mut stages: Vec<_> = singletons(&[3, 8])
        .into_iter()
        .map(ProjectionStage::Explicit)
        .collect();
    stages.insert(
        1,
        ProjectionStage::SingletonDeclarationRun {
            source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
            first_row: 2,
            declaration: 0,
            storage: SolveVariableStorageRun {
                base: ScalarSlot::Y {
                    index: 20,
                    byte_offset: 160,
                },
                scalar_count: 3,
                role: SolveVariableStorageRole::Algebraic,
                value_kind: SolveVariableValueKind::Real,
            },
        },
    );
    let reads = [vec![21], vec![], vec![8], vec![8], vec![8]]
        .map(|indices| Some(Arc::new(IndexIntervals::of(indices))));
    assert_eq!(
        execution_order(&stages, |row| reads.get(row)?.as_deref()),
        Some(vec![2, 1, 0])
    );
    let mut cycle = reads;
    cycle[1] = Some(Arc::new(IndexIntervals::singleton(22)));
    assert_eq!(
        execution_order(&stages, |row| cycle.get(row)?.as_deref()),
        None
    );
}

#[test]
fn overlapping_targets_or_rows_retain_the_existing_first_owner() {
    assert_eq!(order(&[10, 11, 11], &[&[11], &[], &[]]), None);
    let mut blocks = singletons(&[1, 2]);
    blocks[1].rows[0] = 0;
    let stages = blocks
        .into_iter()
        .map(ProjectionStage::Explicit)
        .collect::<Vec<_>>();
    assert!(producer_intervals(&stages).is_none());
}

#[test]
fn a_multioutput_program_and_a_declaration_run_order_by_their_actual_reads() {
    use rumoca_ir_solve::{ComputeBlock, LinearOp};

    let mut stages: Vec<_> = singletons(&[10, 11])
        .into_iter()
        .map(ProjectionStage::Explicit)
        .collect();
    let run = ProjectionStage::SingletonDeclarationRun {
        source: RefreshScalarProgramSource::checked(0, 1).unwrap(),
        declaration: 0,
        first_row: 2,
        storage: SolveVariableStorageRun {
            base: rumoca_ir_solve::scalar_slot_y(12),
            scalar_count: 2,
            role: SolveVariableStorageRole::Algebraic,
            value_kind: SolveVariableValueKind::Real,
        },
    };
    stages.push(run.clone());
    let source = ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 13 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::StoreOutput { src: 1 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 11 },
                LinearOp::LoadY { dst: 1, index: 11 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
        ],
        vec![rumoca_ir_solve::source_span_from_offsets(1, 0, 1); 2],
    )
    .unwrap();
    let mut problem = SolveProblem::default();
    let blocks = ProjectionBlocks::from_stages(stages.clone()).unwrap();
    problem.continuous.implicit_row_targets = ImplicitRowTargets::from_projection(&blocks);
    problem.continuous.algebraic_projection_plan.blocks = blocks;
    problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(source);

    order_algebraic_projection_by_execution_reads(&mut problem).unwrap();
    let ordered = &problem.continuous.algebraic_projection_plan.blocks;
    assert_eq!(
        ordered.stages(),
        &[stages[1].clone(), run, stages[0].clone()]
    );
    assert!(
        problem
            .continuous
            .implicit_row_targets
            .shares_projection_owner(ordered)
    );
}
