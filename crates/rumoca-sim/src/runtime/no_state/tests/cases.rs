use super::*;

#[test]
fn finalize_algebraic_outputs_removes_dummy_channel() {
    let all_names = vec!["__dummy_time_state".to_string(), "y".to_string()];
    let data = vec![vec![0.0, 0.0], vec![1.0, 2.0]];
    let (names, values, n_states) =
        finalize_algebraic_outputs(all_names, data, 1, "__dummy_time_state");
    assert_eq!(names, vec!["y".to_string()]);
    assert_eq!(values, vec![vec![1.0, 2.0]]);
    assert_eq!(n_states, 0);
}

#[test]
fn collect_reconstruction_discrete_context_names_collects_discrete_refs() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("d"),
        dae::Variable::new(dae::VarName::new("d")),
    );
    let mut elim = EliminationResult::default();
    elim.substitutions.push(rumoca_phase_solve::Substitution {
        var_name: dae::VarName::new("y"),
        expr: dae::Expression::VarRef {
            name: dae::VarName::new("d"),
            subscripts: vec![],
        },
        env_keys: vec![],
    });
    let extras = collect_reconstruction_discrete_context_names(&dae_model, &elim, &[]);
    assert_eq!(extras, vec!["d".to_string()]);
}

#[test]
fn sampled_names_need_eliminated_env_only_for_substitution_targets() {
    let mut elim = EliminationResult::default();
    elim.substitutions.push(rumoca_phase_solve::Substitution {
        var_name: dae::VarName::new("tmp"),
        expr: dae::Expression::Literal(dae::Literal::Real(1.0)),
        env_keys: vec!["tmp".to_string()],
    });

    assert!(!sampled_names_need_eliminated_env(
        &["y".to_string()],
        &elim
    ));
    assert!(sampled_names_need_eliminated_env(
        &["tmp".to_string()],
        &elim
    ));
}

#[test]
fn sampled_names_need_eliminated_env_tracks_direct_and_alias_dependency_closure() {
    let mut dae_model = dae::Dae::default();
    dae_model.algebraics.insert(
        dae::VarName::new("source"),
        dae::Variable::new(dae::VarName::new("source")),
    );
    dae_model.algebraics.insert(
        dae::VarName::new("mid"),
        dae::Variable::new(dae::VarName::new("mid")),
    );
    dae_model.outputs.insert(
        dae::VarName::new("obs"),
        dae::Variable::new(dae::VarName::new("obs")),
    );

    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("mid"),
        dae::Expression::VarRef {
            name: dae::VarName::new("source"),
            subscripts: vec![],
        },
        rumoca_core::Span::DUMMY,
        "mid = source",
    ));
    dae_model.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("obs"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("mid"),
                subscripts: vec![],
            }),
        },
        rumoca_core::Span::DUMMY,
        "obs - mid = 0",
    ));

    let mut elim = EliminationResult::default();
    elim.substitutions.push(rumoca_phase_solve::Substitution {
        var_name: dae::VarName::new("source"),
        expr: dae::Expression::Literal(dae::Literal::Real(1.0)),
        env_keys: vec!["source".to_string()],
    });

    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);

    assert!(!sampled_names_need_eliminated_env(
        &["obs".to_string()],
        &elim
    ));
    assert!(sampled_names_need_eliminated_env_with_runtime_closure(
        &["obs".to_string()],
        &elim,
        &direct_assignment_ctx,
        &alias_ctx,
    ));
}

#[test]
fn collect_algebraic_samples_reports_schedule_mismatch() {
    let dae_model = dae::Dae::default();
    let elim = EliminationResult::default();
    let all_names: Vec<String> = Vec::new();
    let solver_name_to_idx: HashMap<String, usize> = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    };

    let result = collect_algebraic_samples(
        &ctx,
        &[0.0],
        &[],
        vec![],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    );

    assert!(matches!(
        result,
        Err(NoStateSampleError::SampleScheduleMismatch { .. })
    ));
}

#[test]
fn no_state_projection_needs_event_refresh_skips_plain_continuous_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        },
        rumoca_core::Span::DUMMY,
        "y = 1",
    ));

    assert!(!no_state_projection_needs_event_refresh(&dae_model));
}

#[test]
fn no_state_projection_needs_event_refresh_detects_pre_reads() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("y"),
                subscripts: vec![],
            }],
        },
        rumoca_core::Span::DUMMY,
        "y = pre(y)",
    ));

    assert!(no_state_projection_needs_event_refresh(&dae_model));
}

#[test]
fn no_state_projection_needs_event_refresh_detects_plain_discrete_var_reads() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_valued.insert(
        dae::VarName::new("count"),
        dae::Variable::new(dae::VarName::new("count")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("T_start"),
        dae::Variable::new(dae::VarName::new("T_start")),
    );
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("count"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("T_start"),
                subscripts: vec![],
            }),
        },
        rumoca_core::Span::DUMMY,
        "y = count + T_start",
    ));

    assert!(no_state_projection_needs_event_refresh(&dae_model));
}

#[test]
fn no_state_projection_uses_lowered_pre_next_event_aliases_detects_table_style_refs() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("__pre__.table.nextTimeEventScaled"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        },
        rumoca_core::Span::DUMMY,
        "y = __pre__.table.nextTimeEventScaled + 1",
    ));

    assert!(no_state_projection_uses_lowered_pre_next_event_aliases(
        &dae_model
    ));
    assert!(!no_state_projection_needs_event_refresh(&dae_model));
}

#[test]
fn no_state_projection_needs_event_refresh_detects_relations() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                },
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(-1.0))),
        },
        rumoca_core::Span::DUMMY,
        "y = if u > 0 then 1 else -1",
    ));

    assert!(no_state_projection_needs_event_refresh(&dae_model));
}

#[test]
fn no_state_projection_needs_event_refresh_skips_noevent_relations() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::NoEvent,
            args: vec![dae::Expression::If {
                branches: vec![(
                    dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                        lhs: Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("u"),
                            subscripts: vec![],
                        }),
                        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                    },
                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(-1.0))),
            }],
        },
        rumoca_core::Span::DUMMY,
        "y = noEvent(if u > 0 then 1 else -1)",
    ));

    assert!(!no_state_projection_needs_event_refresh(&dae_model));
}

#[test]
fn no_state_requires_live_pre_values_skips_plain_continuous_dae() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::Literal(dae::Literal::Real(1.0)),
        rumoca_core::Span::DUMMY,
        "y = 1",
    ));

    assert!(!no_state_requires_live_pre_values(&dae_model));
}

#[test]
fn no_state_requires_live_pre_values_detects_discrete_partition_pre_reads() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("m"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("m"),
                subscripts: vec![],
            }],
        },
        rumoca_core::Span::DUMMY,
        "m := pre(m)",
    ));

    assert!(no_state_requires_live_pre_values(&dae_model));
}

#[test]
fn no_state_requires_live_pre_values_detects_root_relations() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .synthetic_root_conditions
        .push(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Gt(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
        });

    assert!(no_state_requires_live_pre_values(&dae_model));
}

#[test]
fn no_state_requires_frozen_event_pre_values_skips_plain_pre_chain() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }],
        },
        rumoca_core::Span::DUMMY,
        "y = pre(u)",
    ));

    assert!(!no_state_requires_frozen_event_pre_values(&dae_model));
}

#[test]
fn no_state_requires_frozen_event_pre_values_detects_clocked_sample_activity() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::VarRef {
                    name: dae::VarName::new("u"),
                    subscripts: vec![],
                },
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![dae::Expression::Literal(dae::Literal::Real(1.0))],
                    is_constructor: false,
                },
            ],
        },
        rumoca_core::Span::DUMMY,
        "y = sample(u, Clock(1.0))",
    ));

    assert!(no_state_requires_frozen_event_pre_values(&dae_model));
}

#[test]
fn no_state_inter_sample_pre_detection_includes_sampled_clocked_values() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::VarRef {
                    name: dae::VarName::new("u"),
                    subscripts: vec![],
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("clk"),
                    subscripts: vec![],
                },
            ],
        },
        rumoca_core::Span::DUMMY,
        "y = sample(u, clk)",
    ));

    assert!(no_state_requires_inter_sample_pre_values(&dae_model));
}

#[test]
fn build_settled_runtime_env_advances_plain_pre_values_across_non_clock_passes() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("u"),
        dae::Variable::new(dae::VarName::new("u")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("u"),
        dae::Expression::Literal(dae::Literal::Real(1.0)),
        rumoca_core::Span::DUMMY,
        "u = 1",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }],
        },
        rumoca_core::Span::DUMMY,
        "y = pre(u)",
    ));

    let elim = EliminationResult::default();
    let all_names: Vec<String> = Vec::new();
    let solver_name_to_idx: HashMap<String, usize> = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("u", 0.0);
    rumoca_eval_dae::runtime::set_pre_value("y", 0.0);

    let mut y = vec![];
    let env = build_settled_runtime_env(&ctx, &mut y, 1.0);

    assert_eq!(env.get("u"), 1.0);
    assert_eq!(env.get("y"), 1.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_solver_only_projection_path_reads_from_y() {
    let dae_model = dae::Dae::default();
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::from([(String::from("y"), 0usize)]);
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 1, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: true,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    };

    assert!(can_sample_solver_outputs_directly(&ctx));

    let (_, data) = collect_algebraic_samples(
        &ctx,
        &[0.0, 1.0],
        &[0.0, 1.0],
        vec![0.0],
        || Ok::<(), ()>(()),
        |y, t, requires_projection| {
            assert!(requires_projection);
            y[0] = t;
            Ok::<(), ()>(())
        },
    )
    .expect("solver-only projection path should collect samples");

    assert_eq!(data, vec![vec![0.0, 1.0]]);
}

#[test]
fn can_sample_solver_outputs_directly_ignores_plain_relation_event_refresh() {
    let dae_model = dae::Dae::default();
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::from([(String::from("y"), 0usize)]);
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 1, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: true,
        projection_needs_event_refresh: true,
        requires_live_pre_values: false,
    };

    assert!(can_sample_solver_outputs_directly(&ctx));
}

#[test]
fn can_sample_solver_outputs_directly_ignores_relation_only_live_pre_flag() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .synthetic_root_conditions
        .push(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Gt(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
        });
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::from([(String::from("y"), 0usize)]);
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 1, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: true,
        projection_needs_event_refresh: true,
        requires_live_pre_values: true,
    };

    assert!(can_sample_solver_outputs_directly(&ctx));
}

#[test]
fn can_sample_solver_outputs_directly_ignores_initial_only_event_marker() {
    let dae_model = dae::Dae::default();
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::from([(String::from("y"), 0usize)]);
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 1, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 1, 0);
    let clock_event_times = vec![-0.0];
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &clock_event_times,
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: true,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    };

    assert!(can_sample_solver_outputs_directly(&ctx));
}

#[test]
fn collect_algebraic_samples_preserves_time_guarded_qualified_enum_parameters() {
    let mut dae_model = dae::Dae::default();
    dae_model.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4,
        ),
    ]);

    let mut before = dae::Variable::new(dae::VarName::new("Enable.before"));
    before.start = Some(dae::Expression::VarRef {
        name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
    });
    dae_model
        .parameters
        .insert(dae::VarName::new("Enable.before"), before);

    let mut after = dae::Variable::new(dae::VarName::new("Enable.after"));
    after.start = Some(dae::Expression::VarRef {
        name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
        subscripts: vec![],
    });
    dae_model
        .parameters
        .insert(dae::VarName::new("Enable.after"), after);

    let mut step_time = dae::Variable::new(dae::VarName::new("Enable.stepTime"));
    step_time.start = Some(dae::Expression::Literal(dae::Literal::Real(1.0)));
    dae_model
        .parameters
        .insert(dae::VarName::new("Enable.stepTime"), step_time);

    dae_model.outputs.insert(
        dae::VarName::new("Enable.y"),
        dae::Variable::new(dae::VarName::new("Enable.y")),
    );
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("Enable.y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: OpBinary::Ge(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("time"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("Enable.stepTime"),
                        subscripts: vec![],
                    }),
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("Enable.after"),
                    subscripts: vec![],
                },
            )],
            else_branch: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("Enable.before"),
                subscripts: vec![],
            }),
        },
        rumoca_core::Span::DUMMY,
        "Enable.y = if time >= Enable.stepTime then Enable.after else Enable.before",
    ));

    let elim = EliminationResult::default();
    let all_names = vec!["Enable.y".to_string()];
    let solver_name_to_idx = HashMap::from([(String::from("Enable.y"), 0usize)]);
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 1, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, data) = collect_algebraic_samples(
        &ctx,
        &[0.0, 2.0],
        &[0.0, 2.0],
        vec![0.0],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("no-state sampling should preserve qualified enum parameter assignments");

    assert_eq!(data, vec![vec![3.0, 4.0]]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_preserves_env_only_enum_direct_assignment_alias_chain() {
    let harness = build_enum_direct_assignment_alias_harness(false);
    let data = sample_no_state_channels(&harness, &[0.0, 0.5, 2.0], &[0.0, 0.5, 2.0], Vec::new());
    assert_eq!(
        data,
        vec![
            vec![3.0, 3.0, 4.0],
            vec![3.0, 3.0, 4.0],
            vec![3.0, 3.0, 4.0]
        ]
    );
}

#[test]
fn collect_algebraic_samples_preserves_nested_if_enum_direct_assignment_alias_chain() {
    let harness = build_enum_direct_assignment_alias_harness(true);
    let data = sample_no_state_channels(
        &harness,
        &[0.0, 1.5, 2.5, 4.5],
        &[0.0, 1.5, 2.5, 4.5],
        Vec::new(),
    );
    assert_eq!(
        data,
        vec![
            vec![3.0, 4.0, 3.0, 3.0],
            vec![3.0, 4.0, 3.0, 3.0],
            vec![3.0, 4.0, 3.0, 3.0],
        ]
    );
}

#[test]
fn collect_algebraic_samples_advances_pre_only_at_event_times() {
    let mut dae_model = dae::Dae::default();
    dae_model.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::from([(String::from("y"), 0usize)]);
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 1, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[0.5],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: true,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let _ = collect_algebraic_samples(
        &ctx,
        &[0.0, 0.25, 0.5, 0.75],
        &[0.0, 0.25, 0.5, 0.75],
        vec![0.0],
        || Ok::<(), ()>(()),
        |y, t, _requires_projection| {
            y[0] = t;
            Ok::<(), ()>(())
        },
    )
    .expect("no-state sampling should succeed");

    assert_eq!(rumoca_eval_dae::runtime::get_pre_value("y"), Some(0.5));
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_observes_sample_trigger_right_limit_as_false() {
    let mut dae_model = dae::Dae::default();
    dae_model.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae_model.discrete_valued.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Real(0.5)),
            ],
        },
        rumoca_core::Span::DUMMY,
        "y := sample(0, 0.5)",
    ));

    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[0.0, 0.5, 1.0],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, data) = collect_algebraic_samples(
        &ctx,
        &[0.0, 0.25, 0.5, 0.75, 1.0],
        &[0.0, 0.25, 0.5, 0.75, 1.0],
        vec![],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("no-state sampling should observe sample triggers");

    // MLS §16.5.1: sample(start, interval) is an event indicator. The
    // observable post-event right-limit is therefore false at and between
    // output sample points.
    assert_eq!(data, vec![vec![0.0, 0.0, 0.0, 0.0, 0.0]]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_refreshes_boolean_pulse_between_events() {
    let harness = build_boolean_pulse_harness();
    let data = sample_no_state_channels(
        &harness,
        &[0.0, 0.25, 1.0, 1.25],
        &[0.0, 0.25, 1.0, 1.25],
        vec![],
    );
    assert_eq!(data, vec![vec![1.0, 0.0, 1.0, 0.0]]);
}

#[test]
fn collect_algebraic_samples_tracks_sample_shift_hold_chain_at_clock_events() {
    let harness = build_sample_shift_hold_harness(SampleShiftHoldOptions {
        flattened_boolean_alias: false,
        solver_backed_clock: false,
        include_alias_prefix_series: false,
    });
    let data = sample_no_state_channels(
        &harness,
        &[0.04, 0.05, 0.06, 0.16],
        &[0.04, 0.05, 0.06, 0.16],
        vec![],
    );
    assert_eq!(
        data,
        vec![
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0, 0.0, 0.0, 1.0],
            vec![0.0, 0.0, 0.0, 1.0],
            vec![0.0, 0.0, 0.0, 1.0],
        ]
    );
}

#[test]
fn collect_algebraic_samples_tracks_flattened_boolean_hold_alias_chain() {
    let harness = build_sample_shift_hold_harness(SampleShiftHoldOptions {
        flattened_boolean_alias: true,
        solver_backed_clock: false,
        include_alias_prefix_series: true,
    });
    let data = sample_no_state_channels(
        &harness,
        &[0.04, 0.05, 0.06, 0.16],
        &[0.04, 0.05, 0.06, 0.16],
        vec![],
    );
    assert_eq!(
        data,
        vec![
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 1.0, 1.0, 0.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0, 0.0, 1.0, 1.0],
            vec![0.0, 0.0, 0.0, 1.0],
            vec![0.0, 0.0, 0.0, 1.0],
            vec![0.0, 0.0, 0.0, 1.0],
        ]
    );
}

#[test]
fn collect_algebraic_samples_dense_grid_preserves_boolean_hold_transition_times() {
    let harness = build_sample_shift_hold_harness(SampleShiftHoldOptions {
        flattened_boolean_alias: true,
        solver_backed_clock: false,
        include_alias_prefix_series: false,
    });
    let times = dense_sample_times(0.2, 0.0004);
    let data = sample_no_state_channels(&harness, &times, &times, vec![]);
    assert_eq!(rounded_change_times(&times, &data[0]), vec![0.06, 0.16]);
    assert_eq!(rounded_change_times(&times, &data[1]), vec![0.06, 0.16]);
    assert_eq!(rounded_change_times(&times, &data[2]), vec![0.08, 0.18]);
    assert_eq!(rounded_change_times(&times, &data[3]), vec![0.08, 0.18]);
    assert_eq!(rounded_change_times(&times, &data[4]), vec![0.08, 0.18]);
}

#[test]
fn collect_algebraic_samples_handles_solver_backed_sample_clock_alias_chain() {
    let harness = build_sample_shift_hold_harness(SampleShiftHoldOptions {
        flattened_boolean_alias: true,
        solver_backed_clock: true,
        include_alias_prefix_series: false,
    });
    let times = dense_sample_times(0.2, 0.0004);
    let data = sample_no_state_channels_with_sync(
        &harness,
        &times,
        &times,
        vec![0.0, 0.0],
        |y, t, _requires_projection| {
            y[0] = 0.0;
            y[1] = if ((t / 0.02).round() * 0.02 - t).abs() <= 1.0e-12 {
                1.0
            } else {
                0.0
            };
            Ok::<(), ()>(())
        },
    );
    assert_eq!(rounded_change_times(&times, &data[0]), vec![0.06, 0.16]);
    assert_eq!(rounded_change_times(&times, &data[1]), vec![0.06, 0.16]);
    assert_eq!(rounded_change_times(&times, &data[2]), vec![0.08, 0.18]);
    assert_eq!(rounded_change_times(&times, &data[3]), vec![0.08, 0.18]);
    assert_eq!(rounded_change_times(&times, &data[4]), vec![0.08, 0.18]);
}

#[test]
fn collect_algebraic_samples_advances_pre_for_pulse_trigger_edges_between_clock_events() {
    let harness = build_pulse_trigger_edge_harness();
    let data = sample_no_state_channels(
        &harness,
        &[0.0, 0.5, 1.0, 1.5, 2.0, 2.5],
        &[0.0, 0.5, 1.0, 1.5, 2.0, 2.5],
        vec![],
    );
    assert_eq!(
        data,
        vec![
            vec![0.0, 0.0, 1.0, 1.0, 2.0, 2.0],
            vec![1.0, 0.0, 1.0, 0.0, 1.0, 0.0],
        ]
    );
}

#[test]
fn dynamic_event_detection_uses_carried_env_when_pre_store_has_advanced() {
    let dae_model = dae::Dae::default();
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let dynamic_time_event_names = vec!["nextEvent".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    let mut carried_env = eval::VarEnv::new();
    carried_env.set("nextEvent", 5.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("nextEvent", 7.0);

    assert!(should_advance_pre_values(&ctx, 5.0, Some(&carried_env)));

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_keeps_projected_function_outputs_on_event_entry_env() {
    let harness = build_projected_function_output_harness();
    let data = sample_no_state_channels(&harness, &[0.0], &[0.0], vec![0.0]);
    assert_eq!(data, vec![vec![1.0]]);
}

#[test]
fn collect_algebraic_samples_skips_event_refresh_reprojection_between_events() {
    let dae_model = dae::Dae::default();
    let elim = EliminationResult::default();
    let all_names: Vec<String> = vec![];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: true,
        projection_needs_event_refresh: true,
        requires_live_pre_values: false,
    };

    let mut projection_calls = 0usize;
    let _ = collect_algebraic_samples(
        &ctx,
        &[0.0, 0.5],
        &[0.0, 0.5],
        vec![],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| {
            projection_calls += 1;
            Ok::<(), ()>(())
        },
    )
    .expect("non-event samples should not force an extra refresh reprojection");

    // One initial projection happens before the sampling loop begins, then
    // the event-time sample at t_start performs a second refresh
    // projection. The non-event sample should add only one more call.
    assert_eq!(projection_calls, 3);
}

#[test]
fn collect_algebraic_samples_reconstructs_eliminated_discrete_inputs_before_event_settle() {
    let (dae_model, elim) = build_eliminated_change_event_model();
    let all_names = vec!["on".to_string()];
    let dynamic_time_event_names = vec!["nextEvent".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y0 = vec![];
    let env_t0 = build_settled_runtime_env(&ctx, &mut y0, 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let mut y1 = vec![];
    let env_t1 = build_settled_runtime_env(&ctx, &mut y1, 1.0);

    assert_eq!(env_t1.get("tableY"), 1.0);
    assert_eq!(env_t1.get("u"), 1.0);
    assert_eq!(env_t1.get("flag"), 1.0);
    assert_eq!(env_t1.get("on"), 1.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_uses_settled_right_limit_at_dynamic_event_times() {
    let (dae_model, elim) = build_eliminated_change_event_model();
    let all_names = vec!["on".to_string()];
    let dynamic_time_event_names = vec!["nextEvent".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, data) = collect_algebraic_samples(
        &ctx,
        &[0.0, 1.0, 1.02],
        &[0.0, 1.0, 1.02],
        vec![],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("event-time sampling should expose the settled right-limit");

    assert_eq!(data, vec![vec![0.0, 1.0, 1.0, 1.0]]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_injects_dynamic_event_times_into_output_schedule() {
    let (dae_model, elim) = build_eliminated_change_event_model();
    let all_names = vec!["on".to_string()];
    let dynamic_time_event_names = vec!["nextEvent".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    let mut output_times = vec![0.0, 1.02];
    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, final_output_times, data) = collect_algebraic_samples_with_schedule(
        &ctx,
        &mut output_times,
        &[0.0, 1.02],
        vec![],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("dynamic event observations should be exposed in the visible output schedule");

    assert!(
        final_output_times
            .iter()
            .any(|time| timeline::sample_time_match_with_tol(*time, 1.0))
    );
    assert_eq!(data, vec![vec![0.0, 1.0, 1.0, 1.0]]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_injects_direct_time_threshold_events_into_output_schedule() {
    let harness = build_direct_time_threshold_harness();
    let mut output_times = vec![0.0, 0.09];
    let (final_output_times, data) =
        sample_no_state_channels_with_schedule(&harness, &mut output_times, &[0.0, 0.09], vec![]);
    let event_idx = final_output_times
        .iter()
        .position(|time| timeline::sample_time_match_with_tol(*time, 0.05))
        .expect("expected injected threshold event at t=0.05");
    let final_idx = final_output_times
        .iter()
        .position(|time| timeline::sample_time_match_with_tol(*time, 0.09))
        .expect("expected final observation at t=0.09");
    let y_series = &data[0];
    assert!(
        (y_series[event_idx] - 0.05).abs() <= 1.0e-12,
        "expected y to settle at the event instant"
    );
    assert!(
        (y_series[final_idx] - 0.05).abs() <= 1.0e-12,
        "expected the held right-limit to persist after the event"
    );
}

#[test]
fn matched_direct_time_event_time_uses_synthetic_root_thresholds() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .synthetic_root_conditions
        .push(dae::Expression::Binary {
            op: OpBinary::Ge(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("time"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.05))),
        });

    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };
    let env = eval::VarEnv::default();

    // MLS Appendix B / §8.5: no-state scheduling must treat direct
    // time-threshold synthetic roots as real event instants even after the
    // outer edge(...) activation has been lowered elsewhere.
    assert_eq!(
        matched_direct_time_event_time(&ctx, 0.05, Some(&env)),
        Some(0.05)
    );
}

#[test]
fn collect_algebraic_samples_settles_recurrent_direct_threshold_events() {
    let harness = build_recurrent_direct_threshold_harness();
    let mut output_times = vec![0.0, 0.25];
    let (final_output_times, data) =
        sample_no_state_channels_with_schedule(&harness, &mut output_times, &[0.0, 0.25], vec![]);
    assert!(
        final_output_times
            .iter()
            .any(|time| timeline::sample_time_match_with_tol(*time, 0.1))
    );
    assert!(
        final_output_times
            .iter()
            .any(|time| timeline::sample_time_match_with_tol(*time, 0.2))
    );
    assert_eq!(data, vec![vec![0.0, 1.0, 2.0, 2.0]]);
}

#[test]
fn build_settled_runtime_env_reconverges_after_eliminated_bridge_updates() {
    let (dae_model, elim) = build_eliminated_event_convergence_model();
    let all_names = vec!["on".to_string()];
    let dynamic_time_event_names = vec!["nextEventScaled".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y0 = vec![];
    let env_t0 = build_settled_runtime_env(&ctx, &mut y0, 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let mut y1 = vec![];
    let env_t1 = build_settled_runtime_env(&ctx, &mut y1, 1.0);

    assert_eq!(env_t1.get("nextEventScaled"), 2.0);
    assert_eq!(env_t1.get("u"), 2.0);
    assert_eq!(env_t1.get("flag"), 1.0);
    assert_eq!(env_t1.get("on"), 1.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn build_settled_runtime_env_uses_initial_section_discrete_bindings() {
    let mut dae = dae::Dae::default();
    dae.outputs.insert(
        dae::VarName::new("y_out"),
        dae::Variable::new(dae::VarName::new("y_out")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("t_shift"),
        dae::Variable::new(dae::VarName::new("t_shift")),
    );
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("y_out"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                rhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("t_shift"),
                    subscripts: vec![],
                }),
            }),
        },
        rumoca_core::Span::DUMMY,
        "y_out = -t_shift",
    ));
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("t_shift"),
        dae::Expression::Literal(dae::Literal::Real(-0.035)),
        rumoca_core::Span::DUMMY,
        "init t_shift",
    ));

    let mut y = vec![0.0];
    let initial_runtime_env =
        crate::runtime::startup::build_initial_section_env(&dae, &mut y, &[], 0.0);
    crate::runtime::startup::refresh_pre_values_from_state_with_initial_assignments(
        &dae,
        &y,
        &[],
        0.0,
    );

    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae, 1, 0);
    let alias_ctx = crate::runtime::alias::build_runtime_alias_propagation_context(&dae, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae,
        elim: &EliminationResult::default(),
        param_values: &[],
        all_names: &["y_out".to_string()],
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &HashMap::new(),
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    };

    let _ = initial_runtime_env;
    let env = build_settled_runtime_env(&ctx, &mut y, 0.0);
    assert!(
        (env.get("t_shift") + 0.035).abs() <= 1.0e-12,
        "initial-section discrete binding should survive initial no-state settle"
    );
    assert!(
        (env.get("y_out") - 0.035).abs() <= 1.0e-12,
        "initial output should see the initial-section discrete binding"
    );
}

#[test]
fn build_settled_runtime_env_preserves_initial_enum_array_bindings() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
        2,
    );
    dae.discrete_valued.insert(
        dae::VarName::new("bits"),
        dae::Variable {
            name: dae::VarName::new("bits"),
            dims: vec![3],
            ..Default::default()
        },
    );
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("bits"),
        dae::Expression::VarRef {
            name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'X'"),
            subscripts: vec![],
        },
        rumoca_core::Span::DUMMY,
        "init bits=Logic.'X'",
    ));

    let mut y = Vec::<f64>::new();
    crate::runtime::startup::refresh_pre_values_from_state_with_initial_assignments(
        &dae,
        &y,
        &[],
        0.0,
    );

    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae, 0, 0);
    let alias_ctx = crate::runtime::alias::build_runtime_alias_propagation_context(&dae, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae,
        elim: &EliminationResult::default(),
        param_values: &[],
        all_names: &[],
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &HashMap::new(),
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: false,
    };

    let env = build_settled_runtime_env(&ctx, &mut y, 0.0);
    assert_eq!(env.get("bits[1]"), 2.0);
    assert_eq!(env.get("bits[2]"), 2.0);
    assert_eq!(env.get("bits[3]"), 2.0);
}

#[test]
fn build_settled_runtime_env_initial_pre_feedback_uses_current_logic_value() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );
    dae.discrete_valued.insert(
        dae::VarName::new("auxiliary_n"),
        dae::Variable::new(dae::VarName::new("auxiliary_n")),
    );
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("auxiliary_n"),
        dae::Expression::VarRef {
            name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
            subscripts: vec![],
        },
        rumoca_core::Span::DUMMY,
        "auxiliary_n := Logic.'0'",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("auxiliary_n"),
                subscripts: vec![],
            }],
        },
        rumoca_core::Span::DUMMY,
        "y := pre(auxiliary_n)",
    ));

    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae, 1, 0);
    let alias_ctx = crate::runtime::alias::build_runtime_alias_propagation_context(&dae, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae,
        elim: &EliminationResult::default(),
        param_values: &[],
        all_names: &["y".to_string()],
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &HashMap::new(),
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y = vec![0.0];
    let env = build_settled_runtime_env(&ctx, &mut y, 0.0);

    assert_eq!(env.get("auxiliary_n"), 3.0);
    assert_eq!(env.get("y"), 3.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn build_initial_settled_runtime_env_keeps_clocked_previous_on_initial_left_limit() {
    let harness = build_clocked_previous_initial_harness();
    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y = vec![];
    let env = build_initial_settled_runtime_env(&harness.ctx(), &mut y, 0.0);

    assert_eq!(env.get("unitDelay1.y"), 0.0);
    assert_eq!(env.get("sum.y"), 1.0);
    assert_eq!(env.get("assignClock1.u"), 1.0);
    assert_eq!(env.get("assignClock1.y"), 1.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn build_settled_runtime_env_initial_pre_feedback_handles_dynamic_lhs_subscripts() {
    let harness = build_dynamic_lhs_pre_feedback_harness();
    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y = vec![0.0];
    let startup_env =
        crate::runtime::startup::build_initial_section_env(&harness.dae, &mut y, &[], 0.0);
    assert_eq!(startup_env.get("auxiliary[1]"), 1.0);
    assert_eq!(startup_env.get("auxiliary[2]"), 1.0);
    assert_eq!(startup_env.get("auxiliary[3]"), 1.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&startup_env);
    let pass1 = settle_initial_runtime_event_pass(
        &harness.ctx(),
        &mut y,
        0.0,
        startup_env.clone(),
        &startup_env,
    );
    assert_eq!(pass1.get("auxiliary[3]"), 3.0);
    assert_eq!(pass1.get("auxiliary_n"), 3.0);
    let env = build_settled_runtime_env(&harness.ctx(), &mut y, 0.0);

    assert_eq!(env.get("auxiliary[3]"), 3.0);
    assert_eq!(env.get("auxiliary_n"), 3.0);
    assert_eq!(env.get("y"), 3.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn build_settled_runtime_env_initial_pre_feedback_handles_wrapped_pre_expression() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
    ]);
    dae.discrete_valued.insert(
        dae::VarName::new("auxiliary_n"),
        dae::Variable::new(dae::VarName::new("auxiliary_n")),
    );
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("auxiliary_n"),
        dae::Expression::VarRef {
            name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
            subscripts: vec![],
        },
        rumoca_core::Span::DUMMY,
        "auxiliary_n := Logic.'0'",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: OpBinary::Eq(Default::default()),
                    lhs: Box::new(dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Pre,
                        args: vec![dae::Expression::VarRef {
                            name: dae::VarName::new("auxiliary_n"),
                            subscripts: vec![],
                        }],
                    }),
                    rhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
                        subscripts: vec![],
                    }),
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
                    subscripts: vec![],
                },
            )],
            else_branch: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'U'"),
                subscripts: vec![],
            }),
        },
        rumoca_core::Span::DUMMY,
        // MLS §8.6 / SPEC_0022 EQN-035: wrapped uses of pre(v) must
        // converge to the initialization fixed point, not stay latched to
        // the bootstrap left-limit.
        "y := if pre(auxiliary_n) == Logic.'0' then Logic.'0' else Logic.'U'",
    ));

    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae, 1, 0);
    let alias_ctx = crate::runtime::alias::build_runtime_alias_propagation_context(&dae, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae,
        elim: &EliminationResult::default(),
        param_values: &[],
        all_names: &["y".to_string()],
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &HashMap::new(),
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y = vec![0.0];
    let env = build_settled_runtime_env(&ctx, &mut y, 0.0);

    assert_eq!(env.get("auxiliary_n"), 3.0);
    assert_eq!(env.get("y"), 3.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn build_settled_runtime_env_does_not_fire_ordinary_edge_when_at_initial_event() {
    let mut dae = dae::Dae::default();
    let mut trigger = dae::Variable::new(dae::VarName::new("trig"));
    trigger.start = Some(dae::Expression::Literal(dae::Literal::Boolean(false)));
    dae.discrete_valued
        .insert(dae::VarName::new("trig"), trigger);

    let mut y = dae::Variable::new(dae::VarName::new("y"));
    y.start = Some(dae::Expression::Literal(dae::Literal::Integer(2)));
    dae.outputs.insert(dae::VarName::new("y"), y);
    dae.discrete_valued.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("trig"),
        dae::Expression::Literal(dae::Literal::Boolean(true)),
        rumoca_core::Span::DUMMY,
        "init trig = true",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::If {
            // MLS §8.6 / SPEC_0022 EQN-035: ordinary when-style edge
            // guards must see pre(v)=v at the initialization event.
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("trig"),
                        subscripts: vec![],
                    }],
                },
                dae::Expression::Literal(dae::Literal::Integer(4)),
            )],
            else_branch: Box::new(dae::Expression::If {
                branches: vec![(
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Initial,
                        args: vec![],
                    },
                    dae::Expression::VarRef {
                        name: dae::VarName::new("y"),
                        subscripts: vec![],
                    },
                )],
                else_branch: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("y"),
                        subscripts: vec![],
                    }],
                }),
            }),
        },
        rumoca_core::Span::DUMMY,
        "y := if edge(trig) then 4 else if initial() then y else pre(y)",
    ));

    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae, 1, 0);
    let alias_ctx = crate::runtime::alias::build_runtime_alias_propagation_context(&dae, 1, 0);
    let ctx = NoStateSampleContext {
        dae: &dae,
        elim: &EliminationResult::default(),
        param_values: &[],
        all_names: &["y".to_string()],
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &[],
        solver_name_to_idx: &HashMap::from([(String::from("y"), 0usize)]),
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut y0 = vec![2.0];
    let env_t0 = build_settled_runtime_env(&ctx, &mut y0, 0.0);

    assert_eq!(env_t0.get("trig"), 1.0);
    assert_eq!(env_t0.get("y"), 2.0);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_algebraic_samples_refreshes_pre_outputs_after_event_times() {
    let dae_model = build_pre_output_dynamic_event_model();
    let elim = EliminationResult::default();
    let all_names = vec!["y".to_string()];
    let dynamic_time_event_names = vec!["nextEvent".to_string()];
    let solver_name_to_idx = HashMap::new();
    let direct_assignment_ctx =
        crate::runtime::assignment::build_runtime_direct_assignment_context(&dae_model, 0, 0);
    let alias_ctx =
        crate::runtime::alias::build_runtime_alias_propagation_context(&dae_model, 0, 0);
    let ctx = NoStateSampleContext {
        dae: &dae_model,
        elim: &elim,
        param_values: &[],
        all_names: &all_names,
        clock_event_times: &[],
        direct_assignment_ctx: &direct_assignment_ctx,
        alias_ctx: &alias_ctx,
        needs_eliminated_env: false,
        dynamic_time_event_names: &dynamic_time_event_names,
        solver_name_to_idx: &solver_name_to_idx,
        n_x: 0,
        t_start: 0.0,
        requires_projection: false,
        projection_needs_event_refresh: false,
        requires_live_pre_values: true,
    };

    rumoca_eval_dae::runtime::clear_pre_values();
    let (_, data) = collect_algebraic_samples(
        &ctx,
        &[0.0, 1.0, 1.02],
        &[0.0, 1.0, 1.02],
        vec![],
        || Ok::<(), ()>(()),
        |_y, _t, _requires_projection| Ok::<(), ()>(()),
    )
    .expect("event-time sampling should refresh pre-derived outputs after settling");

    assert_eq!(data, vec![vec![0.0, 0.0, 1.0, 1.0]]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn collect_dynamic_time_event_names_finds_pre_time_guards() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("nextEvent"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: OpBinary::Ge(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("time"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Pre,
                        args: vec![dae::Expression::VarRef {
                            name: dae::VarName::new("nextEvent"),
                            subscripts: vec![],
                        }],
                    }),
                },
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(0.5))),
        },
        rumoca_core::Span::DUMMY,
        "nextEvent guard",
    ));

    assert_eq!(
        collect_dynamic_time_event_names(&dae_model),
        vec!["nextEvent".to_string()]
    );
}

#[test]
fn collect_dynamic_time_event_names_finds_live_time_guards() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("gateOut"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: OpBinary::Ge(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("time"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("t_next"),
                        subscripts: vec![],
                    }),
                },
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
        },
        rumoca_core::Span::DUMMY,
        "gateOut guard",
    ));

    assert_eq!(
        collect_dynamic_time_event_names(&dae_model),
        vec!["t_next".to_string()]
    );
}
