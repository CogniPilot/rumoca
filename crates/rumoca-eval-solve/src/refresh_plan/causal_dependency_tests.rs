use super::*;
use std::cell::Cell;

thread_local! {
    static COUNTS: Cell<(usize, usize)> = const { Cell::new((0, 0)) };
}

pub(crate) fn record_input_range_walk() {
    COUNTS.with(|counts| {
        let (walks, candidates) = counts.get();
        counts.set((walks + 1, candidates));
    });
}

pub(super) fn record_bounds_candidate() {
    COUNTS.with(|counts| {
        let (walks, candidates) = counts.get();
        counts.set((walks, candidates + 1));
    });
}

fn fixture(count: usize, unique: bool) -> (solve::SolveProblem, PreparedScalarProgramBlock) {
    let operations = (0..count)
        .flat_map(|index| {
            let register = u32::try_from(if unique { index * 3 } else { 0 }).unwrap();
            [
                solve::LinearOp::LoadY {
                    dst: register,
                    index,
                },
                solve::LinearOp::Const {
                    dst: register + 1,
                    value: 1.0,
                },
                solve::LinearOp::Binary {
                    dst: register + 2,
                    op: solve::BinaryOp::Sub,
                    lhs: register,
                    rhs: register + 1,
                },
                solve::LinearOp::StoreOutput { src: register + 2 },
            ]
        })
        .collect();
    let block = PreparedScalarProgramBlock::new(
        solve::ScalarProgramBlock::with_program_spans(
            vec![operations],
            vec![solve::source_span_from_offsets(7, 0, 1)],
        )
        .unwrap(),
    )
    .unwrap();
    let mut problem = solve::SolveProblem::default();
    problem.solve_layout.algebraic_scalar_count = count;
    problem.solve_layout.solver_maps.names = (0..count).map(|index| format!("y{index}")).collect();
    problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(block.block().clone());
    problem.continuous.implicit_row_targets = (0..count)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect();
    problem.continuous.algebraic_projection_plan.blocks = (0..count)
        .map(|index| solve::AlgebraicProjectionBlock {
            rows: vec![index],
            y_indices: vec![index],
            tearing: None,
            alternate_charts: Vec::new(),
        })
        .collect();
    (problem, block)
}

#[test]
fn overwritten_fixture_records_certificate_refusal_with_valid_algebraic_coordinates() {
    let (problem, block) = fixture(8, false);
    assert_eq!(problem.solve_layout.state_scalar_count(), 0);
    assert_eq!(problem.solve_layout.solver_scalar_count(), 8);
    for index in 0..8 {
        assert_eq!(
            problem.continuous.implicit_row_targets[index],
            Some(solve::scalar_slot_y(index))
        );
        eprintln!(
            "overwritten output={index} evaluable={} exact={} shape={}",
            block.can_evaluate_declared_target_assignment(0, index, index),
            block.certifies_exact_target_assignment_output(0, index, index),
            block.assignment_shape_for_output(0, index, index).is_some()
        );
    }
    let plan = build_algebraic_refresh_plan(&problem, &block).unwrap();
    eprintln!(
        "overwritten issued_targets={:?}",
        plan.rows
            .iter()
            .map(|row| row.target_index())
            .collect::<Vec<_>>()
    );
    assert_eq!(plan.rows.len(), 7);
    assert!(!block.certifies_exact_target_assignment_output(0, 1, 1));
}

#[test]
fn complete_causal_check_shares_one_program_range_walk_and_keeps_bounds_compact() {
    for count in [8, 64, 256] {
        let (problem, block) = fixture(count, true);
        let plan = build_algebraic_refresh_plan(&problem, &block).unwrap();
        assert_eq!(
            plan.rows.len(),
            count,
            "all exact row certificates are issued"
        );
        COUNTS.with(|counts| counts.set((0, 0)));
        assert!(complete_causal_projection_is_certified(
            &problem,
            block.block(),
            &plan.rows
        ));
        let (walks, candidates) = COUNTS.with(Cell::get);
        eprintln!("rows={count} range_walks={walks} bounds_candidates={candidates}");
        assert_eq!(walks, 1, "one immutable operation owner supplies every row");
        assert!(
            candidates <= count,
            "one merged interval per row, no scalar expansion"
        );
    }
}

fn tensor_load(start: usize, count: usize) -> solve::LinearOp {
    solve::LinearOp::TensorLoad {
        dst_start: 0,
        input: solve::TensorInputKind::Y,
        input_start: start,
        count,
        seed_start: None,
        lanes: 1,
    }
}

#[test]
fn interval_bounds_match_original_scalar_query_including_empty_and_saturated_inputs() {
    for start in 0..8 {
        for count in 0..8 {
            let program = [tensor_load(start, count)];
            for solver_count in 0..16 {
                let expected = !row_y_input_ranges(&program)
                    .into_iter()
                    .flatten()
                    .any(|index| index >= solver_count);
                assert_eq!(
                    RowDependencyCache::default()
                        .solver_inputs_are_in_bounds(&program, solver_count),
                    expected,
                    "start={start} count={count} solver_count={solver_count}"
                );
            }
        }
    }
    for program in [[tensor_load(usize::MAX, 0)], [tensor_load(usize::MAX, 1)]] {
        assert!(RowDependencyCache::default().solver_inputs_are_in_bounds(&program, 0));
    }
    let program = [tensor_load(usize::MAX - 1, 2)];
    assert!(RowDependencyCache::default().solver_inputs_are_in_bounds(&program, usize::MAX));
    assert!(!RowDependencyCache::default().solver_inputs_are_in_bounds(&program, usize::MAX - 1));
}

#[test]
fn full_source_ranges_include_unused_loads_and_distinct_immutable_owners() {
    let source = [
        tensor_load(0, 4_000_000),
        solve::LinearOp::LoadY {
            dst: 0,
            index: 4_000_001,
        },
    ];
    let independent = source.to_vec();
    let mut dependencies = RowDependencyCache::default();
    COUNTS.with(|counts| counts.set((0, 0)));
    assert!(!dependencies.solver_inputs_are_in_bounds(&source, 4_000_000));
    assert!(!dependencies.solver_inputs_are_in_bounds(&source, 4_000_000));
    assert!(!dependencies.solver_inputs_are_in_bounds(&independent, 4_000_000));
    assert_eq!(COUNTS.with(Cell::get), (2, 6));
}
