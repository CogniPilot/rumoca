use super::*;
use std::cell::Cell;

thread_local! {
    static COUNTS: Cell<(usize, usize)> = const { Cell::new((0, 0)) };
}

pub(super) fn record_walk() {
    COUNTS.with(|counts| {
        let (walks, operations) = counts.get();
        counts.set((walks + 1, operations));
    });
}

pub(super) fn record_operation() {
    COUNTS.with(|counts| {
        let (walks, operations) = counts.get();
        counts.set((walks, operations + 1));
    });
}

fn source(program: Vec<LinearOp>) -> ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("grouped_affinity.mo"),
        0,
        1,
    );
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(vec![program], vec![span]).unwrap(),
    )
}

fn plan(rows: Vec<usize>, targets: Vec<usize>) -> RefreshPlan {
    RefreshPlan {
        simultaneous_block_indices: vec![0],
        simultaneous_plan: crate::AlgebraicProjectionPlan {
            blocks: vec![crate::AlgebraicProjectionBlock {
                rows,
                y_indices: targets,
                tearing: None,
                alternate_charts: Vec::new(),
            }],
        },
        ..Default::default()
    }
}

#[test]
fn one_program_and_target_inventory_share_one_degree_walk() {
    for count in [32, 128, 256] {
        let program = (0..count)
            .map(|index| LinearOp::LoadY {
                dst: index as u32,
                index,
            })
            .chain((0..count).map(|index| LinearOp::StoreOutput { src: index as u32 }))
            .collect();
        let source = source(program);
        let plan = plan((0..count).rev().collect(), (0..count).collect());
        COUNTS.with(|counts| counts.set((0, 0)));
        assert_eq!(
            projection_affinities(&source, &plan),
            BTreeMap::from([(0, true)])
        );
        let (walks, operations) = COUNTS.with(Cell::get);
        assert_eq!(
            walks, 1,
            "{count} outputs of one issued program and target set"
        );
        assert!(
            operations <= 2 * count,
            "visited {operations} source operations"
        );
    }
}

#[test]
fn distinct_target_inventories_keep_independent_degree_proofs() {
    let mut program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
    ];
    program.extend((0..16).map(|_| LinearOp::StoreOutput { src: 2 }));
    let source = source(program);
    let mut plan = plan((0..16).collect(), vec![0]);
    let mut nonlinear = plan.simultaneous_plan.blocks[0].clone();
    nonlinear.y_indices = vec![0, 1];
    plan.simultaneous_plan.blocks.push(nonlinear);
    plan.simultaneous_block_indices.push(1);
    COUNTS.with(|counts| counts.set((0, 0)));
    assert_eq!(
        projection_affinities(&source, &plan),
        BTreeMap::from([(0, true), (1, false)]),
    );
    assert_eq!(COUNTS.with(Cell::get).0, 2);
}

fn degrees(program: &[LinearOp], outputs: &[usize]) -> Option<Vec<Degree>> {
    let mut values = Vec::new();
    visit_program_degrees(
        program,
        &BTreeSet::from([0]),
        &outputs.iter().copied().collect(),
        |degree| {
            values.push(degree);
            true
        },
    )?;
    Some(values)
}

#[test]
fn selected_stores_read_before_later_overwrites_and_refusals() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::Binary {
            dst: 0,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    assert_eq!(
        degrees(&program, &[1, 0, 0]),
        Some(vec![Degree::Affine, Degree::Nonlinear])
    );
    assert_eq!(degrees(&program, &[0]), Some(vec![Degree::Affine]));
    assert_eq!(degrees(&program, &[2]), None);
    assert_eq!(degrees(&program, &[0, 2]), None);
    let source = source(program);
    assert_eq!(
        projection_affinities(&source, &plan(vec![0, 0], vec![0])),
        BTreeMap::from([(0, true)])
    );
    assert_eq!(
        projection_affinities(&source, &plan(vec![2], vec![0])),
        BTreeMap::from([(0, false)])
    );
    assert_eq!(
        projection_affinities(&source, &plan(vec![], vec![0])),
        BTreeMap::from([(0, true)])
    );
    assert_eq!(
        projection_affinities(&source, &plan(vec![3], vec![0])),
        BTreeMap::from([(0, false)])
    );
}

#[test]
fn ranged_selected_stores_preserve_stride_empty_range_and_prefix_arithmetic() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 0,
            stride: usize::MAX,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 2,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    assert_eq!(
        degrees(&program, &[2, 0]),
        Some(vec![Degree::Affine, Degree::Independent])
    );
    assert_eq!(degrees(&program, &[1]), Some(vec![Degree::Nonlinear]));
    assert_eq!(degrees(&program, &[3]), None);
    let overflowing_suffix = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: usize::MAX,
            stride: 0,
        },
    ];
    assert_eq!(
        degrees(&overflowing_suffix, &[0, 1]),
        Some(vec![Degree::Independent; 2])
    );
}
