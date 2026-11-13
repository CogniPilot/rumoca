//! Execution reads checked against the order each refresh dispatch executes.

use super::*;

/// A residual whose checked call returns `y[read]` and guards that it is
/// finite; with `guard_only`, the call returns a constant and only the guard
/// reads `y[read]`.
fn checked_reader(read: usize, guard_only: bool) -> Vec<crate::LinearOp> {
    let value = if guard_only {
        crate::LinearOp::Const { dst: 0, value: 1.0 }
    } else {
        crate::LinearOp::LoadY {
            dst: 0,
            index: read,
        }
    };
    vec![
        value,
        crate::LinearOp::LoadY {
            dst: 1,
            index: read,
        },
        crate::LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: crate::structural_pattern::execution_reads_tests::guarded_site(),
        },
        crate::LinearOp::StoreOutput { src: 2 },
    ]
}

fn source(programs: Vec<Vec<crate::LinearOp>>) -> ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("refresh_execution_order_test.mo"),
        0,
        1,
    );
    ComputeBlock::from_scalar_program_block(
        crate::ScalarProgramBlock::with_source_span(
            programs,
            rumoca_core::ProvenanceSpan::new(span, "refresh execution order test").unwrap(),
        )
        .unwrap(),
    )
}

/// The residual row of program `equation` solved for `target`.
fn residual_row(target: usize, equation: usize) -> AlgebraicRefreshRow {
    AlgebraicRefreshRow {
        owner_id: RefreshRowOwnerId::checked(target).unwrap(),
        source: RefreshScalarProgramSource::checked(0, equation).unwrap(),
        equation_index: equation,
        output_offset: 0,
        target_index: target,
        assignment_target: None,
        assignment_shape: None,
        direct_assignment_certified: false,
        exact_assignment_certified: false,
    }
}

fn block(equations: Vec<usize>, targets: Vec<usize>) -> crate::AlgebraicProjectionBlock {
    crate::AlgebraicProjectionBlock {
        rows: equations,
        y_indices: targets,
        tearing: None,
        alternate_charts: Vec::new(),
    }
}

/// One value stage per block, each projecting its canonical block after
/// seeding it, as construction does, with the block's exact rows whose
/// coefficient is a constant.
fn projection_plan(
    rows: Vec<AlgebraicRefreshRow>,
    blocks: Vec<crate::AlgebraicProjectionBlock>,
) -> RefreshPlan {
    let seeds = |block: &crate::AlgebraicProjectionBlock| {
        let seeded = rows.iter().enumerate().filter(|(_, row)| {
            block.y_indices.contains(&row.target_index)
                && row.exact_assignment_certified
                && row
                    .assignment_shape
                    .as_ref()
                    .is_some_and(TargetAssignmentShape::constant_coefficient)
        });
        selection(rows.len(), seeded.map(|(index, _)| index))
    };
    let value_stages = blocks
        .iter()
        .enumerate()
        .map(|(block_index, block)| RefreshStage::ProjectionBlock {
            seed_sequence: Default::default(),
            block_index,
            plan: AlgebraicProjectionPlan {
                blocks: (vec![block.clone()]).into(),
            },
            seed_rows: seeds(block),
        })
        .collect();
    RefreshPlan {
        simultaneous_block_indices: (0..blocks.len()).collect(),
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: blocks.into(),
        },
        rows,
        value_stages,
        ..RefreshPlan::default()
    }
}

fn algebraic_owners(source: &ComputeBlock, plan: RefreshPlan) -> ContinuousRefreshOwners {
    ContinuousRefreshOwners::checked_for_source(
        source,
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect("the refresh owners are valid")
}

fn program_lookup<'a>(
    source: &'a ComputeBlock,
) -> impl FnMut(RefreshScalarProgramSource) -> Option<&'a [crate::LinearOp]> {
    |owner| {
        scalar_source_program(source, owner)
            .ok()
            .flatten()
            .map(|(program, _)| program)
    }
}

/// `y1` is produced at or after the row reading it in issued row order, but
/// both rows are residuals of one block its projection solves together.
fn simultaneous_value_reads() -> (ComputeBlock, RefreshPlan) {
    (
        source(vec![checked_reader(1, false), checked_reader(0, false)]),
        projection_plan(
            vec![residual_row(0, 0), residual_row(1, 1)],
            vec![block(vec![0, 1], vec![0, 1])],
        ),
    )
}

#[test]
fn a_projection_block_reads_the_targets_it_solves_in_any_row_order() {
    let (source, plan) = simultaneous_value_reads();
    let owners = algebraic_owners(&source, plan);
    let plan = owners.algebraic();
    assert_eq!(plan.execution_order_refusal, None);
    assert_eq!(
        plan.possible_stage_schedules(),
        &[
            RefreshStageSchedule::Certified,
            RefreshStageSchedule::Uncertified
        ]
    );
    let steps = owners
        .staged_refresh_steps(plan, RefreshStageSchedule::Certified)
        .expect("the block stage settles every target");
    assert!(matches!(
        steps.as_slice(),
        [StagedRefreshStep::Project { block_index: 0, .. }]
    ));
}

#[test]
fn an_assignment_stage_reads_targets_produced_earlier_in_that_stage() {
    let source = source(vec![checked_reader(1, false), checked_reader(5, false)]);
    let sweep = |order: [usize; 2]| RefreshPlan {
        rows: vec![residual_row(0, 0), residual_row(1, 1)],
        value_stages: vec![RefreshStage::CausalSeedSweep {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: RefreshRowSelection::default(),
            dynamic_rows: selection(2, order),
        }],
        ..RefreshPlan::default()
    };
    // The stage produces `y1` before the row reading it, whatever the
    // issued row order.
    let settled = algebraic_owners(&source, sweep([1, 0]));
    assert_eq!(settled.algebraic().execution_order_refusal, None);
    let stale = algebraic_owners(&source, sweep([0, 1]));
    assert_eq!(
        stale.algebraic().execution_order_refusal,
        Some(RefreshExecutionRefusal::ForwardRead {
            row: RefreshRowOwnerId::checked(0).unwrap(),
            target: 0,
            read: 1,
        })
    );
}

#[test]
fn an_assignment_row_reading_its_own_target_refuses_the_staged_schedule() {
    // An assignment stores its target after evaluating the prefix, so the
    // prefix would check the target's stale value.
    let source = source(vec![checked_reader(0, false)]);
    let plan = RefreshPlan {
        rows: vec![residual_row(0, 0)],
        value_stages: vec![RefreshStage::CausalSeedSweep {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: RefreshRowSelection::default(),
            dynamic_rows: selection(1, [0]),
        }],
        ..RefreshPlan::default()
    };
    assert_eq!(
        algebraic_owners(&source, plan)
            .algebraic()
            .execution_order_refusal,
        Some(RefreshExecutionRefusal::ForwardRead {
            row: RefreshRowOwnerId::checked(0).unwrap(),
            target: 0,
            read: 0,
        })
    );
}

#[test]
fn a_projection_block_reads_a_target_of_an_earlier_stage() {
    // Row order issues the reader first; the stages project `y1`'s block
    // before the block whose guard checks `y1`.
    let source = source(vec![checked_reader(1, true), checked_reader(5, false)]);
    let plan = projection_plan(
        vec![residual_row(0, 0), residual_row(1, 1)],
        vec![block(vec![1], vec![1]), block(vec![0], vec![0])],
    );
    let owners = algebraic_owners(&source, plan);
    assert_eq!(owners.algebraic().execution_order_refusal, None);
    assert_eq!(
        owners.algebraic().possible_stage_schedules(),
        &[
            RefreshStageSchedule::Certified,
            RefreshStageSchedule::Uncertified
        ]
    );
}

#[test]
fn a_projection_block_read_of_a_target_no_stage_produces_refuses() {
    // `y1` is a refresh target, but no stage produces it: the block's guard
    // would check whatever value the coordinate held.
    let source = source(vec![checked_reader(1, true), checked_reader(5, false)]);
    let plan = projection_plan(
        vec![residual_row(0, 0), residual_row(1, 1)],
        vec![block(vec![0], vec![0])],
    );
    assert_eq!(
        algebraic_owners(&source, plan)
            .algebraic()
            .execution_order_refusal,
        Some(RefreshExecutionRefusal::ForwardRead {
            row: RefreshRowOwnerId::checked(0).unwrap(),
            target: 0,
            read: 1,
        })
    );
}

/// The canonical certificate of `row` as its source issues it.
fn certified(source: &ComputeBlock, mut row: AlgebraicRefreshRow) -> AlgebraicRefreshRow {
    let (shape, causal) = SourceCertificateQueries::new(source)
        .derive(&row, "test")
        .unwrap();
    row.exact_assignment_certified = shape.is_some() && causal;
    row.direct_assignment_certified =
        row.exact_assignment_certified && shape.as_ref().is_some_and(|shape| shape.is_direct());
    row.assignment_target = shape.as_ref().map(TargetAssignmentShape::target_y_index);
    row.assignment_shape = shape;
    row
}

/// A coupled block solving `y0` and `y2` from `y2 - y0` (whose direct
/// assignment prefix ends before an unused call guarding `y[guarded]`) and
/// `y0*y2*y2 - 1`, and a block producing `y1`, projected in the given order.
fn coupled_shaped_projection(guarded: usize, y1_first: bool) -> (ComputeBlock, RefreshPlan) {
    let shaped = vec![
        crate::LinearOp::LoadY { dst: 0, index: 2 },
        crate::LinearOp::LoadY { dst: 1, index: 0 },
        crate::LinearOp::Binary {
            dst: 2,
            op: crate::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        crate::LinearOp::Const { dst: 3, value: 1.0 },
        crate::LinearOp::LoadY {
            dst: 4,
            index: guarded,
        },
        crate::LinearOp::PureCall {
            dst_start: 5,
            input_starts: vec![3, 4].into(),
            site: crate::structural_pattern::execution_reads_tests::guarded_site(),
        },
        crate::LinearOp::StoreOutput { src: 2 },
    ];
    let product = vec![
        crate::LinearOp::LoadY { dst: 0, index: 0 },
        crate::LinearOp::LoadY { dst: 1, index: 2 },
        crate::LinearOp::Binary {
            dst: 2,
            op: crate::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        crate::LinearOp::Binary {
            dst: 3,
            op: crate::BinaryOp::Mul,
            lhs: 2,
            rhs: 1,
        },
        crate::LinearOp::Const { dst: 4, value: 1.0 },
        crate::LinearOp::Binary {
            dst: 5,
            op: crate::BinaryOp::Sub,
            lhs: 3,
            rhs: 4,
        },
        crate::LinearOp::StoreOutput { src: 5 },
    ];
    let source = source(vec![shaped, product, checked_reader(5, false)]);
    let rows = [(0, 0), (2, 1), (1, 2)]
        .map(|(target, equation)| certified(&source, residual_row(target, equation)));
    assert!(
        rows[0].exact_assignment_certified
            && rows[0].assignment_shape.as_ref().is_some_and(|shape| {
                shape.is_direct() && shape.constant_coefficient() && shape.expr_eval_len() < 6
            }),
        "`y2 - y0` seeds the block through a direct prefix ending before its guarded call"
    );
    assert!(!rows[1].exact_assignment_certified && !rows[2].exact_assignment_certified);
    let coupled = block(vec![0, 1], vec![0, 2]);
    let y1 = block(vec![2], vec![1]);
    let blocks = if y1_first {
        vec![y1, coupled]
    } else {
        vec![coupled, y1]
    };
    (source, projection_plan(rows.to_vec(), blocks))
}

#[test]
fn a_projection_block_proves_the_full_residual_of_a_shaped_row() {
    // The block evaluates the whole residual, guard included, before the
    // later block produces `y1`; the direct prefix alone never reaches it.
    let (source, plan) = coupled_shaped_projection(1, false);
    let owners = algebraic_owners(&source, plan);
    assert_eq!(
        owners.algebraic().execution_order_refusal,
        Some(RefreshExecutionRefusal::ForwardRead {
            row: RefreshRowOwnerId::checked(0).unwrap(),
            target: 0,
            read: 1,
        })
    );
    assert_eq!(
        owners.algebraic().possible_stage_schedules(),
        &[RefreshStageSchedule::Uncertified]
    );
}

#[test]
fn a_shaped_projection_row_guards_an_earlier_stage_or_its_own_unknowns() {
    for (guarded, y1_first) in [(1, true), (2, false), (0, false)] {
        let (source, plan) = coupled_shaped_projection(guarded, y1_first);
        let owners = algebraic_owners(&source, plan);
        assert_eq!(
            owners.algebraic().execution_order_refusal,
            None,
            "guard on y{guarded}, y1 first: {y1_first}"
        );
    }
}

#[test]
fn a_causal_claim_keeps_the_positional_rule_apart_from_the_staged_one() {
    let (source, mut plan) = simultaneous_value_reads();
    plan.causal_solution_certified = true;
    plan.check_execution_order(program_lookup(&source));
    // Executed once in row order, row 0 would check `y1` before row 1
    // produces it; the staged projection solves both together.
    assert!(!plan.causal_solution_certified);
    assert_eq!(plan.execution_order_refusal, None);
    assert_eq!(
        plan.possible_stage_schedules(),
        &[
            RefreshStageSchedule::Certified,
            RefreshStageSchedule::Uncertified
        ]
    );
}

#[test]
fn a_guard_read_of_a_later_stage_target_refuses_the_staged_schedule() {
    // Row order issues `y1` first, but the stages project `y0`'s block, whose
    // guard checks `y1`, before the block that produces `y1`.
    let source = source(vec![checked_reader(1, true), checked_reader(5, false)]);
    let plan = projection_plan(
        vec![residual_row(1, 1), residual_row(0, 0)],
        vec![block(vec![0], vec![0]), block(vec![1], vec![1])],
    );
    let owners = algebraic_owners(&source, plan);
    let expected = Some(RefreshExecutionRefusal::ForwardRead {
        row: RefreshRowOwnerId::checked(0).unwrap(),
        target: 0,
        read: 1,
    });
    assert_eq!(owners.algebraic().execution_order_refusal, expected);
    assert_eq!(
        owners.algebraic().possible_stage_schedules(),
        &[RefreshStageSchedule::Uncertified]
    );
    // The refusal is never serialized: loading derives it again from the
    // canonical source and the final stages.
    let mut loaded: ContinuousRefreshOwners =
        serde_json::from_value(serde_json::to_value(&owners).unwrap()).unwrap();
    assert_eq!(loaded.algebraic().execution_order_refusal, None);
    loaded.rebuild_exact_assignment_programs(&source).unwrap();
    assert_eq!(loaded.algebraic().execution_order_refusal, expected);
    // A plan claiming the opposite fails validation against its source.
    let mut tampered = owners;
    tampered.algebraic.execution_order_refusal = None;
    let error = tampered.validate_sources_against(&source).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("refresh execution-order certificate differs from its canonical source")
    );
}
