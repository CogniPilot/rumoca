use super::*;

#[test]
fn root_condition_plan_keeps_full_values_but_neutralizes_search_roots() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![
                        constant_expression_root_row(),
                        param_minus_time_root_row(0),
                        direct_param_visible_value_row(1),
                        time_plus_one_root_row(),
                    ],
                    "root_plan.mo",
                ),
                root_relation_memory_targets: vec![None; 4],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![2.5, 9.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan
        .as_ref()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![2, 3]);
    assert_eq!(plan.search_rows, vec![3]);

    let full = runtime
        .eval_root_conditions_from_solver_y(1.0, &[], &model.parameters)
        .expect("full root values should evaluate");
    assert_eq!(full, vec![5.0, 1.5, 9.0, 2.0]);

    let mut search = vec![0.0; 4];
    runtime
        .eval_root_search_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("search root values should evaluate");
    assert_eq!(search, vec![1.0, 1.0, 1.0, 2.0]);
}

#[test]
fn root_condition_plan_reports_next_direct_time_root() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![param_minus_time_root_row(0)],
                    "direct_time_root.mo",
                ),
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![2.5],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    assert_eq!(
        runtime
            .next_planned_time_root(&model.parameters, 1.0, 3.0, 1.0e-12)
            .expect("direct time root should be found"),
        Some(2.5)
    );
    assert_eq!(
        runtime
            .next_planned_time_root(&model.parameters, 2.5, 3.0, 1.0e-12)
            .expect("current root should not be rescheduled"),
        None
    );
    assert_eq!(
        runtime
            .next_planned_time_root(&model.parameters, 1.0, 2.0, 1.0e-12)
            .expect("future root beyond target should be ignored"),
        None
    );
}

#[test]
fn root_search_uses_relation_memory_side_only_for_exact_zero_dynamic_roots() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![direct_time_visible_value_row()],
                    "root_plan.mo",
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0],
        ..Default::default()
    };

    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let full = runtime
        .eval_root_conditions_from_solver_y(0.0, &[], &model.parameters)
        .expect("full root evaluation must preserve exact zero");
    assert_eq!(full, vec![0.0]);

    let mut search = vec![0.0];
    runtime
        .eval_root_search_conditions_into(-0.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("initial raw root should evaluate");
    runtime
        .neutralize_initial_root_search_values(&model.parameters, 1.0e-12, &mut search)
        .expect("pre-crossing initial side should neutralize exact negative zero");
    assert_eq!(search, vec![1.0]);

    model.parameters[0] = 1.0;
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    runtime
        .eval_root_search_conditions_into(0.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("reset raw root should evaluate");
    runtime
        .neutralize_initial_root_search_values(&model.parameters, 1.0e-12, &mut search)
        .expect("post-crossing reset side should neutralize exact zero");
    assert_eq!(search, vec![-1.0]);

    runtime
        .eval_root_search_conditions_into(0.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("locator exact zero should remain physical after the start callback");
    assert_eq!(search, vec![0.0]);

    runtime
        .eval_root_search_conditions_into(-0.01, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("nonzero dynamic roots must retain their physical value");
    assert_eq!(search, vec![-0.01]);
}

#[test]
fn root_search_exact_zero_then_same_side_value_does_not_create_false_crossing() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(vec![one_minus_time_root_row()], "root_plan.mo"),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![1.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut at_reset = vec![0.0];
    let mut after_reset = vec![0.0];

    runtime
        .eval_root_search_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut at_reset)
        .expect("reset raw root should evaluate");
    runtime
        .neutralize_initial_root_search_values(&model.parameters, 1.0e-12, &mut at_reset)
        .expect("reset root should use relation side");
    runtime
        .eval_root_search_conditions_into(1.1, &[], &model.parameters, 1.0e-12, 1, &mut after_reset)
        .expect("same-side root should evaluate");

    assert!(at_reset[0] < 0.0);
    assert!(after_reset[0] < 0.0);
}

#[test]
fn consumed_root_override_disarms_nonzero_locator_residual_without_touching_neighbor() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![
                        direct_time_visible_value_row(),
                        direct_time_visible_value_row(),
                    ],
                    "root_plan.mo",
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0)), None],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![1.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
    let mut at_root_start = vec![2.7e-14, 7.0];

    runtime
        .apply_consumed_root_search_overrides(
            &model.parameters,
            1.0e-12,
            &[(0, 1.0)],
            &mut at_root_start,
        )
        .expect("confirmed root should use its post relation side");

    assert_eq!(at_root_start, vec![-1.0, 7.0]);

    let later_locator_values = vec![2.7e-14, 7.0];
    assert_eq!(later_locator_values, vec![2.7e-14, 7.0]);
}

fn param_minus_time_root_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::LoadTime { dst: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn direct_time_visible_value_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn one_minus_time_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        solve::LinearOp::LoadTime { dst: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn constant_expression_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 2.0 },
        solve::LinearOp::Const { dst: 1, value: 3.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn time_plus_one_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}
