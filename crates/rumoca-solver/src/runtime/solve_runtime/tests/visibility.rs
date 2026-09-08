use super::*;

fn explicit_visible_fixture(
    y_count: usize,
    p_count: usize,
    variables: Vec<crate::test_support::RealScalarVariableFixture>,
    visible_value_rows: solve::ScalarProgramBlock,
) -> solve::SolveModel {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: (0..y_count).map(|index| format!("y{index}")).collect(),
            ..Default::default()
        },
        state_scalar_count: y_count,
        compiled_parameter_len: p_count,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            y_count,
            fixture_provenance("visible_fixture_constant_states.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    }
    .seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), y_count, p_count),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("explicit visibility fixture satisfies the checked root contract"),
        initial_y: vec![0.0; y_count],
        solver_nominals: vec![1.0; y_count],
        parameters: vec![0.0; p_count],
        ..empty_binary64_first_product_model()
    };
    crate::test_support::with_explicit_real_scalar_catalog(model, variables, visible_value_rows)
}

fn fixture_provenance(source: &'static str) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(source), 1, 2)
}

#[test]
fn visible_values_for_names_preserves_requested_order() {
    let provenance = fixture_provenance("visible_values.mo");
    let model = explicit_visible_fixture(
        2,
        0,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "b", 0, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2, "a", 1, 0.0, 1.0, true, provenance,
            ),
        ],
        spanned_block(
            vec![const_visible_value_row(2.0), const_visible_value_row(1.0)],
            "visible_values.mo",
        ),
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
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
    let provenance = fixture_provenance("visible_fast_path.mo");
    let model = explicit_visible_fixture(
        2,
        1,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "y2", 1, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::external_input(
                2, "p1", 0, 0.0, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                3, "time", 0, 0.0, 1.0, true, provenance,
            ),
        ],
        spanned_block(
            vec![
                direct_y_visible_value_row(1),
                direct_param_visible_value_row(0),
                direct_time_visible_value_row(),
            ],
            "visible_fast_path.mo",
        ),
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");

    let values = runtime
        .visible_values(&[10.0, 20.0], &[3.5], 4.25)
        .expect("direct visible values should evaluate");

    assert_eq!(values, vec![20.0, 3.5, 4.25]);
}

#[test]
fn visible_values_mixed_plan_keeps_expression_rows() {
    let provenance = fixture_provenance("visible_mixed_plan.mo");
    let model = explicit_visible_fixture(
        2,
        0,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "y2", 1, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2, "computed", 0, 0.0, 1.0, true, provenance,
            ),
        ],
        spanned_block(
            vec![direct_y_visible_value_row(1), positive_sum_residual_row()],
            "visible_mixed_plan.mo",
        ),
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");

    let values = runtime
        .visible_values(&[10.0, 20.0], &[], 0.0)
        .expect("mixed visible values should evaluate");

    assert_eq!(values, vec![20.0, 30.0]);
}

#[test]
fn visible_value_plan_deduplicates_equal_expression_rows() {
    let mut different_sum = positive_sum_residual_row();
    let solve::LinearOp::LoadY { index, .. } = &mut different_sum[0] else {
        unreachable!("sum fixture begins with a state load")
    };
    *index = 2;
    let provenance = fixture_provenance("visible_duplicate_expressions.mo");
    let model = explicit_visible_fixture(
        3,
        0,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "y2", 1, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2,
                "computed_a",
                0,
                0.0,
                1.0,
                true,
                provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                3,
                "computed_different",
                2,
                0.0,
                1.0,
                true,
                provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                4,
                "computed_b",
                0,
                0.0,
                1.0,
                true,
                provenance,
            ),
        ],
        spanned_block(
            vec![
                direct_y_visible_value_row(1),
                positive_sum_residual_row(),
                different_sum,
                positive_sum_residual_row(),
            ],
            "visible_duplicate_expressions.mo",
        ),
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let plan = runtime
        .visible_value_plan
        .as_ref()
        .expect("visible value plan should build");

    assert_eq!(plan.expression_rows, vec![1, 2]);
    assert_eq!(plan.expression_groups.len(), 2);
    assert_eq!(plan.expression_groups[0].row_index, 1);
    assert_eq!(plan.expression_groups[0].output_indices, vec![1, 3]);
    assert_eq!(plan.expression_groups[1].row_index, 2);
    assert_eq!(plan.expression_groups[1].output_indices, vec![2]);

    let values = runtime
        .visible_values(&[10.0, 20.0, 40.0], &[], 0.0)
        .expect("deduplicated visible values should evaluate");

    assert_eq!(values, vec![20.0, 30.0, 60.0, 30.0]);
}

#[test]
fn root_condition_plan_keeps_full_values_but_neutralizes_search_roots() {
    let solve_layout = solve::SolveLayout {
        parameter_count: 2,
        static_parameter_names: vec!["p0".to_string(), "p1".to_string()],
        compiled_parameter_len: 2,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
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
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen; 5],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 0, 2),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("root-plan fixture satisfies the checked root contract"),
        parameters: vec![2.5, 9.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan_for_test()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![2, 3, 4]);
    assert_eq!(plan.search_rows, vec![4]);

    let full = runtime
        .eval_root_conditions_from_solver_y(1.0, &[], model.parameters())
        .expect("full root values should evaluate");
    assert_eq!(full, vec![5.0, 1.5, 9.0, 9.0, 2.0]);

    let mut search = vec![0.0; 5];
    runtime
        .eval_root_search_conditions_into(1.0, &[], model.parameters(), 1.0e-12, 1, &mut search)
        .expect("search root values should evaluate");
    assert_eq!(search, vec![1.0, 1.0, 1.0, 1.0, 2.0]);
}

#[test]
fn root_condition_plan_preserves_grouped_output_ownership() {
    let grouped_static = vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 0 },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let grouped_dynamic = vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 0 },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let solve_layout = solve::SolveLayout {
        state_scalar_count: 1,
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_owned()],
            ..Default::default()
        },
        parameter_count: 1,
        static_parameter_names: vec!["p".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        root_conditions: spanned_block(
            vec![grouped_static, grouped_dynamic],
            "grouped_root_plan.mo",
        ),
        root_relation_memory_targets: vec![None; 4],
        root_zero_domains: vec![solve::RootZeroDomain::Previous; 4],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen; 4],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            1,
            test_span("grouped_root_constant_state.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 1, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("grouped-root fixture satisfies the checked root contract"),
        parameters: vec![3.0],
        initial_y: vec![4.0],
        solver_nominals: vec![1.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan_for_test()
        .expect("grouped root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![0, 1, 2, 3]);
    assert_eq!(plan.search_rows, vec![2, 3]);
    assert_eq!(
        runtime
            .eval_root_conditions_from_solver_y(0.0, &[4.0], model.parameters())
            .expect("full grouped roots should evaluate"),
        vec![3.0, 4.0, 4.0, 8.0]
    );
    let mut search = vec![0.0; 4];
    runtime
        .eval_root_search_conditions_into(0.0, &[4.0], model.parameters(), 1.0e-12, 1, &mut search)
        .expect("grouped search roots should evaluate");
    assert_eq!(search, vec![1.0, 1.0, 4.0, 8.0]);
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
    let solve_layout = solve::SolveLayout {
        parameter_count: 1,
        static_parameter_names: vec!["source".to_string()],
        compiled_parameter_len: 2,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
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
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 0, 2),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("delay-event fixture satisfies the checked root contract"),
        parameters: vec![1.0, 0.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("delay runtime should prepare");
    let mut p = model.parameters().to_vec();
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
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 3,
        pre_param_bindings: vec![solve::PreParamBinding {
            dest_p_index: 1,
            source: solve::PreParamSource::P { index: 0 },
            clock_schedule: None,
        }],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        scheduled_time_events: vec![0.0],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 0, 3),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("pre-memory fixture satisfies the checked root contract"),
        // p[0] is the converged current value, p[1] is its lowered pre slot,
        // and p[2] records what the post-event projection observed.
        parameters: vec![2.0, 1.0, 0.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime = SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
    let mut p = model.parameters().to_vec();
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

fn phase_zero_clock_model() -> solve::SolveModel {
    let schedule = solve::PeriodicEventSchedule::new(
        rumoca_core::ClockLattice::from_seconds(0.1, 0.0).expect("positive phase-zero lattice"),
    )
    .expect("phase-zero schedule");
    let clocks = solve::SolveClockPartition {
        periodic_event_schedules: vec![schedule.clone()],
        activation_parameter_indices: vec![2],
    };
    let owner = clocks
        .periodic_clock_id(0)
        .expect("inserted clock has a typed identity");
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 3,
        pre_param_bindings: vec![solve::PreParamBinding {
            dest_p_index: 1,
            source: solve::PreParamSource::P { index: 0 },
            clock_schedule: Some(schedule),
        }],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem {
        rhs: spanned_block(
            vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 1 },
                solve::LinearOp::Const { dst: 1, value: 1.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            "phase_zero_clock_tick.mo",
        ),
        update_targets: vec![solve::scalar_slot_p(0)],
        row_roles: vec![solve::DiscreteRowRole::EventAction],
        pre_modes: vec![solve::DiscreteEventPreMode::EventEntry],
        observation_refresh: vec![false],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
        clock_owners: vec![Some(owner)],
        clock_partition_order: vec![solve::ClockPartitionStep::ScalarRows {
            start_row: 0,
            count: 1,
        }],
        ..Default::default()
    };
    let events = solve::SolveEventPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 0, 3),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("phase-zero fixture satisfies the checked root contract"),
        parameters: vec![0.0, 0.0, 0.0],
        ..empty_binary64_first_product_model()
    }
}

#[test]
fn phase_zero_clock_tick_executes_once_after_initialization() {
    let model = phase_zero_clock_model();
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("clock fixture should prepare");
    let mut p = model.parameters().to_vec();
    let event_pre_p = p.clone();

    let outcome = runtime
        .apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut [],
                p: &mut p,
                t_start: 0.0,
                t_end: 0.1,
                tol: 1.0e-9,
                event_pre_y: &[],
                event_pre_p: &event_pre_p,
                max_iters: 8,
                dynamic_event: None,
            },
            |_, _, _| Ok(false),
        )
        .expect("initial event and coincident tick should settle");

    assert_eq!(p[0], 1.0, "the first clock tick must execute exactly once");
    assert_eq!(
        outcome.observations.len(),
        1,
        "private clock scratch is not fabricated as a public catalog variable"
    );
    assert_eq!(outcome.observations[0].t, 0.0);
    assert_eq!(outcome.observations[0].p[0], 0.0);
}

#[test]
fn root_evaluation_rejects_non_finite_surfaces() {
    let solve_layout = solve::SolveLayout::default();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
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
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::default(),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("non-finite-root fixture satisfies the checked root contract"),
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("root runtime should prepare");

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
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan_for_test()
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
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan_for_test()
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
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_string(), "static_output".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        parameter_count: 1,
        static_parameter_names: vec!["parameter".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let implicit_span = test_span("parameter_static_refresh.mo");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![parameter_assignment_residual_row(1, 0)],
                vec![implicit_span],
                vec![1],
            )
            .expect("parameter-static implicit fixture is computable"),
        ),
        implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            1,
            test_span("parameter_static_constant_state.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 1),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("parameter-static fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 0.0],
        solver_nominals: vec![1.0; 2],
        parameters: vec![2.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    assert_eq!(runtime.algebraic_refresh.static_causal_seed_rows().len(), 1);
    assert!(
        runtime
            .algebraic_refresh
            .dynamic_causal_seed_rows()
            .is_empty()
    );
    assert!(
        runtime.algebraic_refresh.causal_solution_certified(),
        "unexpected stages: {:?}",
        runtime.algebraic_refresh.value_stages()
    );
    let mut solver_y = model.initial_y().to_vec();
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
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_string(), "output".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let implicit_span = test_span("algebraic_output_root.mo");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![implicit_row],
                vec![implicit_span],
                vec![1],
            )
            .expect("algebraic-output implicit fixture is computable"),
        ),
        implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            1,
            test_span("algebraic_output_root_constant_state.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        root_conditions: spanned_block(
            vec![direct_y_visible_value_row(1)],
            "algebraic_output_root.mo",
        ),
        root_relation_memory_targets: vec![None],
        root_zero_domains: vec![solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::AlgebraicDependent],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("algebraic-output fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 2.0],
        solver_nominals: vec![1.0; 2],
        ..empty_binary64_first_product_model()
    }
}

#[test]
fn visible_value_runtime_errors_keep_row_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("visible.mo"),
        4,
        9,
    );
    let model = explicit_visible_fixture(
        1,
        0,
        vec![crate::test_support::RealScalarVariableFixture::state(
            1, "x", 0, 0.0, 1.0, true, span,
        )],
        solve::ScalarProgramBlock::with_source_span(
            vec![derivative_placeholder_row(0)],
            span.require_provenance("visible-value runtime fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable"),
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");

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
