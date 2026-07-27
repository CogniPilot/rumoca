use super::*;

#[test]
fn visible_values_for_names_preserves_requested_order() {
    let model = solve::SolveModel {
        visible_names: vec!["b".to_string(), "a".to_string()],
        visible_value_rows: spanned_block(
            vec![const_visible_value_row(2.0), const_visible_value_row(1.0)],
            "visible_values.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let names = vec!["a".to_string(), "missing".to_string(), "b".to_string()];

    let values = runtime
        .visible_values_for_names(&[], &[], 0.0, &names)
        .expect("visible values should evaluate");

    assert_eq!(
        values.keys().cloned().collect::<Vec<_>>(),
        vec!["a".to_string(), "b".to_string()]
    );
    assert_eq!(values.get("a"), Some(&1.0));
    assert_eq!(values.get("b"), Some(&2.0));
}

#[test]
fn visible_values_fast_path_reads_direct_sources() {
    let model = solve::SolveModel {
        visible_names: vec!["y2".to_string(), "p1".to_string(), "time".to_string()],
        visible_value_rows: spanned_block(
            vec![
                direct_y_visible_value_row(1),
                direct_param_visible_value_row(0),
                direct_time_visible_value_row(),
            ],
            "visible_fast_path.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let values = runtime
        .visible_values(&[10.0, 20.0], &[3.5], 4.25)
        .expect("direct visible values should evaluate");

    assert_eq!(values, vec![20.0, 3.5, 4.25]);
}

#[test]
fn visible_values_mixed_plan_keeps_expression_rows() {
    let model = solve::SolveModel {
        visible_names: vec!["y2".to_string(), "computed".to_string()],
        visible_value_rows: spanned_block(
            vec![direct_y_visible_value_row(1), positive_sum_residual_row()],
            "visible_mixed_plan.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let values = runtime
        .visible_values(&[10.0, 20.0], &[], 0.0)
        .expect("mixed visible values should evaluate");

    assert_eq!(values, vec![20.0, 30.0]);
}

#[test]
fn visible_value_plan_deduplicates_equal_expression_rows() {
    let model = solve::SolveModel {
        visible_names: vec![
            "y2".to_string(),
            "computed_a".to_string(),
            "computed_b".to_string(),
        ],
        visible_value_rows: spanned_block(
            vec![
                direct_y_visible_value_row(1),
                positive_sum_residual_row(),
                positive_sum_residual_row(),
            ],
            "visible_duplicate_expressions.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .visible_value_plan
        .as_ref()
        .expect("visible value plan should build");

    assert_eq!(plan.expression_rows, vec![1]);
    assert_eq!(plan.expression_groups.len(), 1);
    assert_eq!(plan.expression_groups[0].row_index, 1);
    assert_eq!(plan.expression_groups[0].output_indices, vec![1, 2]);

    let values = runtime
        .visible_values(&[10.0, 20.0], &[], 0.0)
        .expect("deduplicated visible values should evaluate");

    assert_eq!(values, vec![20.0, 30.0, 30.0]);
}

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
                        indexed_param_root_row(),
                        time_plus_one_root_row(),
                    ],
                    "root_plan.mo",
                ),
                root_relation_memory_targets: vec![None; 5],
                root_zero_domains: vec![solve::RootZeroDomain::Previous; 5],
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

    assert_eq!(plan.evaluated_rows, vec![2, 3, 4]);
    assert_eq!(plan.search_rows, vec![4]);

    let full = runtime
        .eval_root_conditions_from_solver_y(1.0, &[], &model.parameters)
        .expect("full root values should evaluate");
    assert_eq!(full, vec![5.0, 1.5, 9.0, 9.0, 2.0]);

    let mut search = vec![0.0; 5];
    runtime
        .eval_root_search_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("search root values should evaluate");
    assert_eq!(search, vec![1.0, 1.0, 1.0, 1.0, 2.0]);
}

#[test]
fn initial_event_commits_delay_left_limit_before_the_synthetic_right_limit() {
    let delay = spanned_block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "initial_event_delay.mo",
    );
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: 2,
                compiled_parameter_len: 2,
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                scheduled_time_events: vec![0.0],
                delays: solve::SolveDelayPartition {
                    source_rhs: spanned_block(
                        vec![vec![
                            solve::LinearOp::LoadP { dst: 0, index: 0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ]],
                        "initial_event_delay.mo",
                    ),
                    delay_time_rhs: delay.clone(),
                    delay_max_rhs: delay,
                    value_parameter_indices: vec![1],
                    source_is_discrete: vec![false],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![1.0, 0.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("delay runtime should prepare");
    let mut p = model.parameters.clone();
    runtime
        .initialize_delay_history(0.0, &[], &mut p)
        .expect("delay history should initialize");
    let event_pre_p = p.clone();

    let outcome = runtime
        .apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut [],
                p: &mut p,
                t_start: 0.0,
                t_end: 1.0,
                tol: 1.0e-9,
                event_pre_y: &[],
                event_pre_p: &event_pre_p,
                max_iters: 8,
                dynamic_event: None,
                apply_without_initial_event: false,
            },
            |y, p, t| {
                let before = p.to_vec();
                if t > 0.0 {
                    p[0] = 2.0;
                }
                runtime.refresh_delay_values(t, y, p)?;
                Ok(p != before.as_slice())
            },
        )
        .expect("initial event boundary should settle");

    assert!(outcome.final_t > 0.0);
    assert_eq!(p[0], 2.0, "the right-limit source update must apply");
    assert_eq!(
        p[1], 1.0,
        "the delayed right limit must read the accepted event-time source"
    );
}

#[test]
fn initial_event_advances_pre_memory_before_the_synthetic_right_limit() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: 3,
                compiled_parameter_len: 3,
                pre_param_bindings: vec![solve::PreParamBinding {
                    dest_p_index: 1,
                    source: solve::PreParamSource::P { index: 0 },
                    clock_schedule: None,
                }],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                scheduled_time_events: vec![0.0],
                ..Default::default()
            },
            ..Default::default()
        },
        // p[0] is the converged current value, p[1] is its lowered pre slot,
        // and p[2] records what the post-event projection observed.
        parameters: vec![2.0, 1.0, 0.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
    let mut p = model.parameters.clone();
    let event_pre_p = p.clone();

    let outcome = runtime
        .apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut [],
                p: &mut p,
                t_start: 0.0,
                t_end: 1.0,
                tol: 1.0e-9,
                event_pre_y: &[],
                event_pre_p: &event_pre_p,
                max_iters: 8,
                dynamic_event: None,
                apply_without_initial_event: false,
            },
            |_, p, t| {
                if t > 0.0 {
                    p[2] = p[1];
                }
                Ok(false)
            },
        )
        .expect("initial event boundary should settle");

    assert!(outcome.final_t > 0.0);
    assert_eq!(p[1], 2.0, "the pre slot must advance after event iteration");
    assert_eq!(
        p[2], 2.0,
        "the synthetic right limit must observe advanced pre memory"
    );
}

#[test]
fn root_evaluation_rejects_non_finite_surfaces() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![vec![
                        solve::LinearOp::Const {
                            dst: 0,
                            value: f64::NAN,
                        },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "nonfinite_root.mo",
                ),
                root_relation_memory_targets: vec![None],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("root runtime should prepare");

    let error = runtime
        .eval_root_conditions_from_solver_y(0.0, &[], &[])
        .expect_err("non-finite root surfaces must fail before solver localization");
    assert!(
        error.to_string().contains("non-finite value NaN"),
        "{error}"
    );
}

#[test]
fn root_condition_plan_neutralizes_parameter_static_algebraic_outputs() {
    let model = algebraic_output_root_model(assignment_residual_row());
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan
        .as_ref()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![0]);
    assert!(plan.search_rows.is_empty());

    let full = runtime
        .eval_root_conditions_from_solver_y(0.0, &[0.0, 2.0], &[])
        .expect("full root value should evaluate");
    assert_eq!(full, vec![2.0]);

    let mut search = vec![0.0];
    runtime
        .eval_root_search_conditions_into(0.0, &[0.0], &[], 1.0e-12, 1, &mut search)
        .expect("search root value should evaluate");
    assert_eq!(search, vec![1.0]);
}

#[test]
fn root_condition_plan_keeps_state_dependent_algebraic_outputs_dynamic() {
    let model = algebraic_output_root_model(add_assignment_residual_row(1, 0, 1.0));
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan
        .as_ref()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![0]);
    assert_eq!(plan.search_rows, vec![0]);

    let mut search = vec![0.0];
    runtime
        .eval_root_search_conditions_into(0.0, &[3.0], &[], 1.0e-12, 1, &mut search)
        .expect("search root value should evaluate");
    assert_eq!(search, vec![4.0]);
}

#[test]
fn parameter_static_refresh_cache_invalidates_with_parameter_snapshot() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["state".to_string(), "static_output".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(0),
                        parameter_assignment_residual_row(1, 0),
                    ],
                    "parameter_static_refresh.mo",
                )),
                implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0],
        parameters: vec![2.0],
        ..Default::default()
    };
    set_causal_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    assert_eq!(runtime.algebraic_refresh.static_causal_seed_rows.len(), 1);
    assert!(
        runtime
            .algebraic_refresh
            .dynamic_causal_seed_rows
            .is_empty()
    );

    let mut solver_y = model.initial_y.clone();
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[2.0], 1.0e-12, 4)
        .expect("first static refresh should populate the cache");
    assert_eq!(solver_y[1], 2.0);

    solver_y[1] = 99.0;
    runtime
        .refresh_algebraic_and_output_slots(1.0, &mut solver_y, &[2.0], 1.0e-12, 4)
        .expect("unchanged parameters should restore the cached value");
    assert_eq!(solver_y[1], 2.0);

    runtime
        .refresh_algebraic_and_output_slots(1.0, &mut solver_y, &[3.0], 1.0e-12, 4)
        .expect("changed parameters should invalidate and recompute the value");
    assert_eq!(solver_y[1], 3.0);
}

fn algebraic_output_root_model(implicit_row: Vec<solve::LinearOp>) -> solve::SolveModel {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["state".to_string(), "output".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![derivative_placeholder_row(0), implicit_row],
                    "algebraic_output_root.mo",
                )),
                implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![direct_y_visible_value_row(1)],
                    "algebraic_output_root.mo",
                ),
                root_relation_memory_targets: vec![None],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 2.0],
        ..Default::default()
    };
    set_complete_test_projection_plan(&mut model);
    model
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
                root_relation_memory_targets: vec![None],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
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
fn visible_value_runtime_errors_keep_row_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("visible.mo"),
        4,
        9,
    );
    let model = solve::SolveModel {
        visible_names: vec!["x".to_string()],
        visible_value_rows: solve::ScalarProgramBlock::with_source_span(
            vec![derivative_placeholder_row(0)],
            span,
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let names = vec!["x".to_string()];
    let err = runtime
        .visible_values_for_names(&[], &[], 0.0, &names)
        .expect_err("missing input should fail visible row evaluation");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains("missing y[0]"),
        "error should explain the missing visible input: {err}"
    );
}
