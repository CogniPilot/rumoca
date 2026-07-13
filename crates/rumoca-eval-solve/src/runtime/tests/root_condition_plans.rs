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
