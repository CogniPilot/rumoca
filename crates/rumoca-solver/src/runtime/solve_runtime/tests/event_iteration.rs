use super::*;

#[test]
fn structured_discrete_map_updates_every_target_through_the_runtime_adapter() {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    let span = test_span("structured_discrete_runtime.mo");
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 0, 2),
            solve_layout: solve::SolveLayout {
                parameter_count: 2,
                compiled_parameter_len: 2,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0; 2],
        ..Default::default()
    };
    model.problem.discrete.structured_rhs = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("two-point output map is valid"),
            domain: domain.clone(),
            base_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 7.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span,
        }],
    };
    model
        .problem
        .discrete
        .structured_updates
        .push(solve::StructuredDiscreteUpdate {
            node_index: 0,
            target: solve::StructuredDiscreteTargetMap {
                base: solve::scalar_slot_p(0),
                map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                    .expect("two-point target map is valid"),
            },
            role: solve::DiscreteRowRole::EventAction,
            pre_mode: solve::DiscreteEventPreMode::FollowCurrent,
            observation_refresh: false,
            integrator_history_effect: solve::IntegratorHistoryEffect::Preserve,
            clock_owner: None,
        });
    model
        .problem
        .validate_shape_contract()
        .expect("compact structured update satisfies the Solve contract");
    let runtime = SolveRuntime::new_fixture(&model).expect("structured runtime should prepare");
    let mut y = Vec::new();
    let mut p = vec![0.0; 2];
    let event_pre_p = p.clone();

    runtime
        .apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut y,
                p: &mut p,
                t: 1.0,
                tol: 1.0e-12,
                event_pre_y: &[],
                event_pre_p: &event_pre_p,
                max_iters: 4,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
            },
            |_, _| Ok(false),
        )
        .expect("structured discrete fixed point should settle");

    assert_eq!(p, vec![7.0, 7.0]);
    assert!(model.problem.discrete.rhs.is_empty());
}

#[test]
fn guarded_assignment_range_stays_compact_until_the_runtime_write_boundary() {
    let span = test_span("guarded_assignment_runtime.mo");
    let condition = vec![
        solve::LinearOp::LoadP { dst: 0, index: 2 },
        solve::LinearOp::StoreOutput { src: 0 },
    ];
    let selected = vec![
        solve::LinearOp::Const { dst: 0, value: 7.0 },
        solve::LinearOp::Const { dst: 1, value: 8.0 },
        solve::LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 1,
        },
    ];
    let fallback = vec![
        solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::P,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        solve::LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 1,
        },
    ];
    let conditional =
        solve::FunctionConditionalProgram::checked(0, [2], [(condition, selected)], fallback)
            .expect("guarded fixture conditional is checked");
    let owner = solve::GuardedAssignmentProgram::checked(
        vec![
            solve::LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ],
        span.require_provenance("guarded runtime fixture")
            .expect("fixture provenance"),
        [(solve::scalar_slot_p(0), 2)],
        solve::DiscreteRowRole::EventAction,
        solve::DiscreteEventPreMode::FollowCurrent,
        false,
        solve::IntegratorHistoryEffect::Preserve,
        None,
    )
    .expect("guarded owner is checked");
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 0, 3),
            solve_layout: solve::SolveLayout {
                parameter_count: 3,
                compiled_parameter_len: 3,
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                guarded_assignments: vec![owner],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0; 3],
        ..Default::default()
    };
    model
        .problem
        .validate_shape_contract()
        .expect("guarded range satisfies the Solve contract");
    let runtime = SolveRuntime::new_fixture(&model).expect("guarded runtime should prepare");
    assert!(runtime.discrete_rhs.is_empty());
    assert_eq!(runtime.guarded_assignment_programs.len(), 1);
    assert_eq!(runtime.guarded_assignment_programs[0].output_count(), 2);
    let mut y = Vec::new();
    let mut p = vec![1.0, 2.0, 0.0];

    runtime
        .apply_unfiltered_discrete_rows_once(&mut y, &mut p, 0.0, 0.0)
        .expect("inactive guarded update evaluates its hold arm");
    assert_eq!(p, vec![1.0, 2.0, 0.0]);

    p[2] = 1.0;
    runtime
        .apply_unfiltered_discrete_rows_once(&mut y, &mut p, 0.0, 0.0)
        .expect("active guarded update evaluates its selected arm");
    assert_eq!(p, vec![7.0, 8.0, 1.0]);
}

#[test]
fn typed_root_override_keeps_other_relations_in_the_event_fixed_point() {
    let roots = spanned_block(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "typed_root_cascade.mo",
    );
    let discrete = spanned_block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "typed_root_cascade_discrete.mo",
    );
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["reported".to_string(), "cascade".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 2,
                compiled_parameter_len: 3,
                relation_memory_parameter_indices: vec![0, 1],
                discrete_valued_scalar_names: vec!["mode".to_string()],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: roots,
                root_relation_memory_targets: vec![
                    Some(solve::scalar_slot_p(0)),
                    Some(solve::scalar_slot_p(1)),
                ],
                root_zero_domains: vec![
                    solve::RootZeroDomain::Previous,
                    solve::RootZeroDomain::Previous,
                ],
                root_relation_refresh_roles: vec![
                    solve::RootRelationRefreshRole::Frozen,
                    solve::RootRelationRefreshRole::AlgebraicDependent,
                ],
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: discrete,
                update_targets: vec![solve::scalar_slot_p(2)],
                row_roles: vec![solve::DiscreteRowRole::Equation],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
                clock_owners: vec![None],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 1.0],
        parameters: vec![0.0, 0.0, 0.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new_fixture(&model).expect("root cascade runtime should prepare");
    let mut y = model.initial_y.clone();
    let mut p = model.parameters.clone();
    let event_pre_y = y.clone();
    let event_pre_p = p.clone();

    runtime
        .apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut y,
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                max_iters: 8,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[(0, 1.0)],
            },
            |solver_y, _| {
                let changed = solver_y[1].to_bits() != (-1.0_f64).to_bits();
                solver_y[1] = -1.0;
                Ok(changed)
            },
        )
        .expect("a typed root must allow a second relation to join the same event");

    assert_eq!(p, vec![1.0, 1.0, 1.0]);
}

#[test]
fn event_iteration_advances_discrete_pre_before_the_next_whole_equation_pass() {
    let relation_row = whole_event_relation_row();
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 1, 5),
            solve_layout: whole_event_solve_layout(),
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![vec![
                        solve::LinearOp::LoadY { dst: 0, index: 0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "whole_event_pass_implicit.mo",
                )),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                        tearing: None,
                    }],
                },
                ..Default::default()
            },
            discrete: whole_event_discrete_system(relation_row),
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![vec![
                        solve::LinearOp::LoadY { dst: 0, index: 0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "whole_event_pass_root.mo",
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(4))],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                root_relation_refresh_roles: vec![
                    solve::RootRelationRefreshRole::AlgebraicDependent,
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![1.0],
        parameters: vec![0.0; 5],
        ..Default::default()
    };
    model
        .problem
        .validate_shape_contract()
        .expect("the typed event-iteration fixture is complete");
    let runtime = SolveRuntime::new_fixture(&model).expect("the event fixture should prepare");
    let mut y = model.initial_y.clone();
    let mut p = model.parameters.clone();
    let event_pre_y = y.clone();
    let event_pre_p = p.clone();

    runtime
        .apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut y,
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                max_iters: 8,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
            },
            |solver_y, params| {
                let projected: f64 = if params[2] != 0.0 { -1.0 } else { 1.0 };
                let changed = solver_y[0].to_bits() != projected.to_bits();
                solver_y[0] = projected;
                Ok(changed)
            },
        )
        .expect("the whole fixed-pre event passes should converge");

    assert_eq!(y, vec![-1.0]);
    assert_eq!(&p[..4], &[1.0, 1.0, 1.0, 1.0]);
}

fn whole_event_relation_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::Const { dst: 1, value: 0.0 },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Lt,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadP { dst: 3, index: 4 },
        solve::LinearOp::Const { dst: 4, value: 0.0 },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::And,
            lhs: 3,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Or,
            lhs: 2,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

fn whole_event_solve_layout() -> solve::SolveLayout {
    solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["algebraic".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 1,
        compiled_parameter_len: 5,
        discrete_valued_scalar_names: vec!["mode".to_string(), "off".to_string()],
        relation_memory_parameter_indices: vec![4],
        pre_param_bindings: vec![
            solve::PreParamBinding {
                dest_p_index: 2,
                source: solve::PreParamSource::P { index: 0 },
                clock_schedule: None,
            },
            solve::PreParamBinding {
                dest_p_index: 3,
                source: solve::PreParamSource::P { index: 1 },
                clock_schedule: None,
            },
        ],
        variable_storage_runs: vec![solve::SolveVariableStorageRun {
            base: solve::scalar_slot_p(0),
            scalar_count: 2,
            role: solve::SolveVariableStorageRole::DiscreteValue,
            value_kind: solve::SolveVariableValueKind::Boolean,
        }],
        variable_declarations: vec![solve::SolveVariableDeclaration::new(
            solve::SolveVariableStorageRole::DiscreteValue,
            solve::SolveVariableValueKind::Boolean,
        )],
        ..Default::default()
    }
}

fn whole_event_discrete_system(relation_row: Vec<solve::LinearOp>) -> solve::DiscreteSolveSystem {
    solve::DiscreteSolveSystem {
        event_iteration_plan: solve::EventIterationPlan {
            runs: vec![solve::EventIterationRun {
                variable: 0,
                pre_binding_start: 0,
                owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
            }],
        },
        runtime_assignment_rhs: spanned_block(
            vec![relation_row.clone()],
            "whole_event_pass_runtime.mo",
        ),
        runtime_assignment_targets: vec![solve::scalar_slot_p(1)],
        runtime_assignment_roles: vec![solve::RuntimeAssignmentRole::RelationEvaluating],
        rhs: spanned_block(
            vec![
                vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                relation_row,
            ],
            "whole_event_pass_discrete.mo",
        ),
        update_targets: vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)],
        row_roles: vec![solve::DiscreteRowRole::Equation; 2],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; 2],
        observation_refresh: vec![false; 2],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve; 2],
        clock_owners: vec![None; 2],
        ..Default::default()
    }
}

fn mixed_pre_solve_layout() -> solve::SolveLayout {
    solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            ..Default::default()
        },
        variable_storage_runs: vec![
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(0),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteValue,
                value_kind: solve::SolveVariableValueKind::Boolean,
            },
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(1),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteValue,
                value_kind: solve::SolveVariableValueKind::Boolean,
            },
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_y(0),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::State,
                value_kind: solve::SolveVariableValueKind::Real,
            },
        ],
        variable_declarations: vec![
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteValue,
                solve::SolveVariableValueKind::Boolean,
            ),
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteValue,
                solve::SolveVariableValueKind::Boolean,
            ),
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::State,
                solve::SolveVariableValueKind::Real,
            ),
        ],
        state_scalar_count: 1,
        compiled_parameter_len: 5,
        discrete_valued_scalar_names: vec!["trigger".to_string(), "result".to_string()],
        pre_param_bindings: vec![
            solve::PreParamBinding {
                dest_p_index: 2,
                source: solve::PreParamSource::P { index: 0 },
                clock_schedule: None,
            },
            solve::PreParamBinding {
                dest_p_index: 3,
                source: solve::PreParamSource::P { index: 1 },
                clock_schedule: None,
            },
            solve::PreParamBinding {
                dest_p_index: 4,
                source: solve::PreParamSource::Y { index: 0 },
                clock_schedule: None,
            },
        ],
        ..Default::default()
    }
}

#[test]
fn event_iteration_mixes_advanced_discrete_pre_with_event_entry_continuous_pre() {
    // Appendix B advances pre(trigger) after the first whole event pass, while
    // pre(x) remains the continuous value captured at event entry. The second
    // row consumes both histories in one expression:
    //
    //   result = pre(trigger) and pre(x) > 0
    //
    // A row-wide EventEntry overlay incorrectly restores both histories on the
    // second pass. The binding-owned live view instead reads both required
    // values: pre(trigger)=true and pre(x)=1.
    let mixed_pre_row = vec![
        solve::LinearOp::LoadP { dst: 0, index: 2 },
        solve::LinearOp::LoadP { dst: 1, index: 4 },
        solve::LinearOp::Const { dst: 2, value: 0.0 },
        solve::LinearOp::Compare {
            dst: 3,
            op: solve::CompareOp::Gt,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::And,
            lhs: 0,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ];
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 1, 5),
            solve_layout: mixed_pre_solve_layout(),
            discrete: solve::DiscreteSolveSystem {
                event_iteration_plan: solve::EventIterationPlan {
                    runs: vec![
                        solve::EventIterationRun {
                            variable: 0,
                            pre_binding_start: 0,
                            owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
                        },
                        solve::EventIterationRun {
                            variable: 1,
                            pre_binding_start: 1,
                            owner: solve::EventIterationOwner::ScalarRows { start_row: 1 },
                        },
                    ],
                },
                rhs: spanned_block(
                    vec![
                        vec![
                            solve::LinearOp::Const { dst: 0, value: 1.0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                        mixed_pre_row,
                    ],
                    "mixed_discrete_continuous_pre.mo",
                ),
                update_targets: vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)],
                row_roles: vec![solve::DiscreteRowRole::Equation; 2],
                pre_modes: vec![
                    solve::DiscreteEventPreMode::FollowCurrent,
                    solve::DiscreteEventPreMode::EventEntry,
                ],
                observation_refresh: vec![false; 2],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve; 2],
                clock_owners: vec![None; 2],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![1.0],
        // The continuous pre lane is deliberately stale. Pass zero must seed
        // it from event_pre_y before projection mutates live y.
        parameters: vec![0.0, 0.0, 0.0, 0.0, -1.0],
        ..Default::default()
    };
    model
        .problem
        .validate_shape_contract()
        .expect("the mixed-pre fixture satisfies the Solve contract");
    let runtime = SolveRuntime::new_fixture(&model).expect("the mixed-pre fixture should prepare");
    let mut y = model.initial_y.clone();
    let mut p = model.parameters.clone();
    let event_pre_y = y.clone();
    let event_pre_p = p.clone();

    runtime
        .apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut y,
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                max_iters: 8,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
            },
            |solver_y, _| {
                let changed = solver_y[0] != -1.0;
                solver_y[0] = -1.0;
                Ok(changed)
            },
        )
        .expect("the mixed-pre whole event passes should converge");

    assert_eq!(p, vec![1.0, 1.0, 1.0, 1.0, 1.0]);
    assert_eq!(y, vec![-1.0]);
}

fn clock_first_pass_solve_layout() -> solve::SolveLayout {
    solve::SolveLayout {
        variable_storage_runs: vec![
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(0),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteValue,
                value_kind: solve::SolveVariableValueKind::Boolean,
            },
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(1),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteValue,
                value_kind: solve::SolveVariableValueKind::Boolean,
            },
        ],
        variable_declarations: vec![
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteValue,
                solve::SolveVariableValueKind::Boolean,
            ),
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteValue,
                solve::SolveVariableValueKind::Boolean,
            ),
        ],
        compiled_parameter_len: 5,
        discrete_valued_scalar_names: vec!["trigger".to_string(), "clocked_result".to_string()],
        pre_param_bindings: vec![
            solve::PreParamBinding {
                dest_p_index: 2,
                source: solve::PreParamSource::P { index: 0 },
                clock_schedule: None,
            },
            solve::PreParamBinding {
                dest_p_index: 3,
                source: solve::PreParamSource::P { index: 1 },
                clock_schedule: None,
            },
        ],
        ..Default::default()
    }
}

#[test]
fn clock_owned_equation_executes_only_on_the_first_whole_event_pass() {
    let schedule = solve::PeriodicEventSchedule::new(
        rumoca_core::ClockLattice::from_seconds(0.1, 0.0).expect("positive phase-zero lattice"),
    )
    .expect("phase-zero schedule");
    let clocks = solve::SolveClockPartition {
        periodic_event_schedules: vec![schedule],
        activation_parameter_indices: vec![4],
    };
    let clock = clocks
        .periodic_clock_id(0)
        .expect("inserted clock has typed identity");
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 0, 5),
            solve_layout: clock_first_pass_solve_layout(),
            clocks,
            discrete: solve::DiscreteSolveSystem {
                event_iteration_plan: solve::EventIterationPlan {
                    runs: vec![
                        solve::EventIterationRun {
                            variable: 0,
                            pre_binding_start: 0,
                            owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
                        },
                        solve::EventIterationRun {
                            variable: 1,
                            pre_binding_start: 1,
                            owner: solve::EventIterationOwner::ScalarRows { start_row: 1 },
                        },
                    ],
                },
                rhs: spanned_block(
                    vec![
                        vec![
                            solve::LinearOp::Const { dst: 0, value: 1.0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                        vec![
                            solve::LinearOp::LoadP { dst: 0, index: 1 },
                            solve::LinearOp::Const { dst: 1, value: 1.0 },
                            solve::LinearOp::Binary {
                                dst: 2,
                                op: solve::BinaryOp::Add,
                                lhs: 0,
                                rhs: 1,
                            },
                            solve::LinearOp::StoreOutput { src: 2 },
                        ],
                    ],
                    "clock_first_whole_pass.mo",
                ),
                update_targets: vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)],
                row_roles: vec![solve::DiscreteRowRole::Equation; 2],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent; 2],
                observation_refresh: vec![false; 2],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve; 2],
                clock_owners: vec![None, Some(clock)],
                // SOLVE-C57: every clock-owned producer carries its issued
                // same-tick step; construction validates exact coverage.
                clock_partition_order: vec![solve::ClockPartitionStep::ScalarRows {
                    start_row: 1,
                    count: 1,
                }],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0; 5],
        ..Default::default()
    };
    model
        .problem
        .validate_shape_contract()
        .expect("clock-first-pass fixture satisfies the Solve contract");
    let runtime =
        SolveRuntime::new_fixture(&model).expect("clock-first-pass fixture should prepare");
    let mut p = model.parameters.clone();
    let event_pre_p = p.clone();

    runtime
        .apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut [],
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                event_pre_y: &[],
                event_pre_p: &event_pre_p,
                max_iters: 8,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
            },
            |_, _| Ok(false),
        )
        .expect("clock and ordinary event owners should settle together");

    assert_eq!(p[0], 1.0, "the ordinary trigger must settle to true");
    assert_eq!(p[2], 1.0, "ordinary pre(trigger) advances between passes");
    assert_eq!(
        p[1], 1.0,
        "the clock-owned recurrence must execute exactly once at the tick"
    );
}

fn phase_zero_clock_partition() -> (solve::SolveClockPartition, solve::PeriodicClockId) {
    let schedule = solve::PeriodicEventSchedule::new(
        rumoca_core::ClockLattice::from_seconds(0.1, 0.0).expect("positive phase-zero lattice"),
    )
    .expect("phase-zero schedule");
    let clocks = solve::SolveClockPartition {
        periodic_event_schedules: vec![schedule],
        activation_parameter_indices: vec![3],
    };
    let clock = clocks
        .periodic_clock_id(0)
        .expect("inserted clock has typed identity");
    (clocks, clock)
}

fn clock_relation_solve_layout() -> solve::SolveLayout {
    solve::SolveLayout {
        variable_storage_runs: vec![solve::SolveVariableStorageRun {
            base: solve::scalar_slot_p(1),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::DiscreteValue,
            value_kind: solve::SolveVariableValueKind::Boolean,
        }],
        variable_declarations: vec![solve::SolveVariableDeclaration::new(
            solve::SolveVariableStorageRole::DiscreteValue,
            solve::SolveVariableValueKind::Boolean,
        )],
        compiled_parameter_len: 4,
        relation_memory_parameter_indices: vec![0],
        discrete_valued_scalar_names: vec!["clocked_result".to_string()],
        pre_param_bindings: vec![solve::PreParamBinding {
            dest_p_index: 2,
            source: solve::PreParamSource::P { index: 1 },
            clock_schedule: None,
        }],
        ..Default::default()
    }
}

#[test]
fn clock_owner_observes_projected_relation_memory_on_its_only_event_pass() {
    let (clocks, clock) = phase_zero_clock_partition();
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: solve::VarLayout::from_parts(IndexMap::new(), 0, 4),
            solve_layout: clock_relation_solve_layout(),
            clocks,
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![vec![
                        solve::LinearOp::Const {
                            dst: 0,
                            value: -1.0,
                        },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "clock_reads_relation_root.mo",
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                event_iteration_plan: solve::EventIterationPlan {
                    runs: vec![solve::EventIterationRun {
                        variable: 0,
                        pre_binding_start: 0,
                        owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
                    }],
                },
                rhs: spanned_block(
                    vec![vec![
                        solve::LinearOp::LoadP { dst: 0, index: 0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "clock_reads_relation_value.mo",
                ),
                update_targets: vec![solve::scalar_slot_p(1)],
                row_roles: vec![solve::DiscreteRowRole::Equation],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
                clock_owners: vec![Some(clock)],
                clock_partition_order: vec![solve::ClockPartitionStep::ScalarRows {
                    start_row: 0,
                    count: 1,
                }],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0; 4],
        ..Default::default()
    };
    model
        .problem
        .validate_shape_contract()
        .expect("clock/relation ordering fixture satisfies the Solve contract");
    let runtime =
        SolveRuntime::new_fixture(&model).expect("clock/relation ordering fixture should prepare");
    let mut p = model.parameters.clone();
    let event_pre_p = p.clone();

    runtime
        .apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut [],
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                event_pre_y: &[],
                event_pre_p: &event_pre_p,
                max_iters: 8,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
            },
            |_, _| Ok(false),
        )
        .expect("the relation and clock owner should settle in one event");

    assert_eq!(
        p[0], 1.0,
        "the projected root selects the true relation side"
    );
    assert_eq!(
        p[1], 1.0,
        "the clock-owned row must consume that side on its sole event pass"
    );
}

#[test]
fn root_refresh_uses_the_root_owned_relation_target_not_global_relation_order() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["surface".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                compiled_parameter_len: 2,
                // The first relation is scheduled/non-root; only the second
                // relation owns the continuously monitored root below.
                relation_memory_parameter_indices: vec![0, 1],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![vec![
                        solve::LinearOp::LoadY { dst: 0, index: 0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "root_relation_owner.mo",
                ),
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(1))],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![-1.0],
        parameters: vec![0.0, 0.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new_fixture(&model).expect("typed root target is a valid runtime");
    let mut params = model.parameters.clone();

    runtime
        .update_relation_memory_from_state(0.0, &model.initial_y, &mut params, 1.0e-12, 4)
        .expect("root relation memory refresh should succeed");

    assert_eq!(
        params,
        vec![0.0, 1.0],
        "the root's aligned target owns the update; an earlier non-root relation must remain intact"
    );

    model.problem.events.root_relation_memory_targets = vec![Some(solve::scalar_slot_y(0))];
    let runtime = SolveRuntime::new_fixture(&model).expect("the root target shape remains aligned");
    let error = runtime
        .update_relation_memory_from_state(0.0, &model.initial_y, &mut params, 1.0e-12, 4)
        .expect_err("a relation-memory root cannot write continuous solver storage");
    assert!(
        error.to_string().contains("not a parameter slot"),
        "the invalid typed target must fail closed: {error}"
    );
}

#[test]
fn post_commit_coupling_refreshes_only_algebraic_relation_roots() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        vec![
                            solve::LinearOp::Const { dst: 0, value: 0.0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                        vec![
                            solve::LinearOp::Const {
                                dst: 0,
                                value: -1.0,
                            },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                    ],
                    "algebraic_relation_assignment.mo",
                )),
                implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        tearing: None,
                    }],
                },
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["state".to_string(), "algebraic".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                compiled_parameter_len: 2,
                relation_memory_parameter_indices: vec![0, 1],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![
                        vec![
                            solve::LinearOp::LoadP { dst: 0, index: 0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                        vec![
                            solve::LinearOp::LoadY { dst: 0, index: 1 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ],
                    ],
                    "algebraic_relation_partition.mo",
                ),
                root_relation_memory_targets: vec![
                    Some(solve::scalar_slot_p(0)),
                    Some(solve::scalar_slot_p(1)),
                ],
                root_zero_domains: vec![
                    solve::RootZeroDomain::Previous,
                    solve::RootZeroDomain::Previous,
                ],
                root_relation_refresh_roles: vec![
                    solve::RootRelationRefreshRole::Frozen,
                    solve::RootRelationRefreshRole::AlgebraicDependent,
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, -1.0],
        parameters: vec![-1.0, 0.0],
        ..Default::default()
    };
    let runtime =
        SolveRuntime::new_fixture(&model).expect("typed relation partition should prepare");
    let mut params = model.parameters.clone();

    runtime
        .update_algebraic_relation_memory_from_solver_y_except_overrides(
            0.0,
            &model.initial_y,
            &mut params,
            &[],
        )
        .expect("algebraic relation refresh should succeed");
    assert_eq!(
        params,
        vec![-1.0, 1.0],
        "parameter-only relation memory stays fixed while the algebraic root joins the coupled closure"
    );

    let mut overridden = model.parameters.clone();
    runtime
        .update_algebraic_relation_memory_from_solver_y_except_overrides(
            0.0,
            &model.initial_y,
            &mut overridden,
            &[(1, 0.0)],
        )
        .expect("a typed located-root override should remain authoritative");
    assert_eq!(overridden, model.parameters);
}

#[test]
fn refresh_plan_does_not_let_residual_target_shadow_assignment_row() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "y".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 2,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        non_assignment_targeted_residual_row(),
                        assignment_residual_row(),
                    ],
                    "refresh_shadow.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(1)),
                    Some(solve::scalar_slot_y(1)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    };
    set_complete_test_projection_plan(&mut model);
    let block =
        PreparedScalarProgramBlock::from_compute_block(&model.problem.continuous.implicit_rhs)
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model, &block);

    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].source().program(), 1);
    assert_eq!(plan.rows[0].target_index(), 1);
    assert!(!plan.causal_solution_certified);
    assert_eq!(plan.simultaneous_plan.blocks.len(), 1);
    assert_eq!(plan.simultaneous_plan.blocks[0].rows, vec![0, 1]);
    assert_eq!(plan.simultaneous_plan.blocks[0].y_indices, vec![0, 1]);
}
