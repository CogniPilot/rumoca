//! A checked remainder executes every uncovered owner, including causal rows.
use super::*;

fn causal_root_model() -> solve::SolveModel {
    use solve::LinearOp::{Binary, Const, LoadTime, LoadY, StoreOutput};
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: crate::fixture_layout::named_y_layout(
                solve::VarLayout::from_parts(Default::default(), 0, 0),
                vec!["a".to_owned(), "b".to_owned()],
            ),
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        vec![
                            LoadY { dst: 0, index: 0 },
                            Const { dst: 1, value: 2.0 },
                            LoadTime { dst: 2 },
                            Binary {
                                dst: 3,
                                op: solve::BinaryOp::Add,
                                lhs: 1,
                                rhs: 2,
                            },
                            Binary {
                                dst: 4,
                                op: solve::BinaryOp::Sub,
                                lhs: 0,
                                rhs: 3,
                            },
                            StoreOutput { src: 4 },
                        ],
                        vec![
                            LoadY { dst: 0, index: 1 },
                            LoadY { dst: 1, index: 0 },
                            Const { dst: 2, value: 3.0 },
                            Binary {
                                dst: 3,
                                op: solve::BinaryOp::Add,
                                lhs: 1,
                                rhs: 2,
                            },
                            Binary {
                                dst: 4,
                                op: solve::BinaryOp::Sub,
                                lhs: 0,
                                rhs: 3,
                            },
                            StoreOutput { src: 4 },
                        ],
                    ],
                    "uncovered_root_causal.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ]
                .into(),
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![vec![LoadY { dst: 0, index: 1 }, StoreOutput { src: 0 }]],
                    "uncovered_root_causal_root.mo",
                ),
                root_relation_memory_targets: vec![None],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                root_relation_refresh_roles: vec![
                    solve::RootRelationRefreshRole::AlgebraicDependent,
                ],
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                algebraic_scalar_count: 2,
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, -123.0].into(),
        solver_nominals: vec![1.0, 1.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![
            affine_seed_row(&[(0, 1.0)]),
            affine_seed_row(&[(1, 1.0), (0, -1.0)]),
        ],
        "uncovered_root_causal_jvp.mo",
    );
    set_causal_test_projection_plan(&mut model);
    derive_test_structural_artifacts(&mut model);
    model
}

#[test]
fn certified_root_remainder_executes_an_uncovered_causal_owner() {
    let mut model = causal_root_model();
    let owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .unwrap();
    let mut root = owners.root().clone();
    assert!(root.causal_solution_certified);
    root.value_stages.clear();
    let mut covered = root.clone();
    covered.causal_seed_rows =
        solve::RefreshRowSelection::checked(covered.rows.len(), [0]).unwrap();
    covered.static_causal_seed_rows = Default::default();
    covered.dynamic_causal_seed_rows = covered.causal_seed_rows.clone();
    covered.value_stages.clear();
    model.problem.continuous.refresh_owners = solve::ContinuousRefreshOwners::checked_for_source(
        &model.problem.continuous.implicit_rhs,
        covered,
        owners.derivative().clone(),
        root,
        owners.event().clone(),
        Vec::new(),
    )
    .unwrap();
    let remainder = model
        .problem
        .continuous
        .refresh_owners
        .root_after_algebraic()
        .unwrap()
        .remainder();
    assert!(remainder.value_stages.is_empty());
    assert_eq!(
        remainder
            .causal_rows()
            .iter()
            .map(solve::AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        [1]
    );
    let runtime = SolveRuntime::new(&model).unwrap();
    let mut y = model.initial_y.to_vec();
    runtime
        .refresh_algebraic_and_output_slots_certified(0.0, &mut y, &[], 1e-10, 4)
        .unwrap();
    assert_eq!(y, [2.0, -123.0]);
    let mut roots = [0.0];
    runtime
        .eval_root_search_conditions_after_algebraic_settle_into(
            0.0,
            &[],
            &mut y,
            1e-10,
            4,
            &mut roots,
        )
        .unwrap();
    assert_eq!(y, [2.0, 5.0]);
    assert_eq!(roots, [5.0]);
}
