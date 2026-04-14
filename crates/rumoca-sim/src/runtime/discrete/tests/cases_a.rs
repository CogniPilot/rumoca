use super::*;

#[test]
fn projected_function_outputs_use_event_entry_env_during_settle() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("last"),
        dae::Variable::new(dae::VarName::new("last")),
    );
    dae_model.f_m.push(dae::Equation {
        lhs: Some(dae::VarName::new("last")),
        rhs: dae::Expression::FunctionCall {
            name: dae::VarName::new("Pkg.advance.next"),
            args: vec![var("last")],
            is_constructor: false,
        },
        span: Span::DUMMY,
        origin: "algorithm when-assignment (projected function output)".to_string(),
        scalar_count: 1,
    });

    let mut advance = dae::Function::new("Pkg.advance", Default::default());
    advance.add_input(dae::FunctionParam::new("last", "Integer"));
    advance.add_output(dae::FunctionParam::new("next", "Integer").with_default(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(var("last")),
            rhs: Box::new(int(1)),
        },
    ));
    advance.body = vec![dae::Statement::Empty];

    let mut env = VarEnv::<f64>::new();
    env.functions = std::sync::Arc::new(indexmap::IndexMap::from([(
        "Pkg.advance".to_string(),
        advance,
    )]));
    env.set("last", 1.0);
    let guard_env = env.clone();

    let changed = apply_discrete_partition_updates_with_guard_env_and_scalar_override(
        &dae_model,
        &mut env,
        &guard_env,
        |_eq, _target, _solution, _env, _implicit_clock_active| None,
    );

    assert!(changed);
    assert_eq!(env.get("last"), 2.0);
}

#[test]
fn discrete_partition_settle_converges_long_reverse_dependency_chain() {
    let mut dae_model = dae::Dae::default();
    let chain_len = 20usize;
    let mut env = VarEnv::new();

    for idx in 1..=chain_len {
        let name = format!("x{idx}");
        dae_model.discrete_reals.insert(
            dae::VarName::new(name.as_str()),
            dae::Variable::new(dae::VarName::new(name.as_str())),
        );
        env.set(name.as_str(), 1.0);
    }

    for idx in (2..=chain_len).rev() {
        let target = format!("x{idx}");
        let source = format!("x{}", idx - 1);
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new(target.as_str()),
            var(source.as_str()),
            Span::DUMMY,
            "reverse dependency chain",
        ));
    }
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("x1"),
        real(3.0),
        Span::DUMMY,
        "x1 = 3",
    ));

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert_eq!(env.get("x1"), 3.0);
    assert_eq!(env.get("x20"), 3.0);
}

#[test]
fn discrete_partition_reselects_branches_after_earlier_same_round_updates() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_valued.insert(
        dae::VarName::new("flag"),
        dae::Variable::new(dae::VarName::new("flag")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("flag"),
        dae::Expression::Literal(dae::Literal::Boolean(true)),
        rumoca_core::Span::DUMMY,
        "flag = true",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::If {
            branches: vec![(var("flag"), real(2.0))],
            else_branch: Box::new(real(0.0)),
        },
        rumoca_core::Span::DUMMY,
        "y = if flag then 2 else 0",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("flag", 0.0);
    env.set("y", 0.0);
    let guard_env = env.clone();

    let changed = apply_discrete_partition_updates_with_guard_env_and_scalar_override(
        &dae_model,
        &mut env,
        &guard_env,
        |_eq, _target, _solution, _env, _implicit_clock_active| None,
    );

    assert!(changed);
    assert!((env.get("y") - 2.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_if_edge_branch_uses_event_entry_guard_but_current_rhs_env() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("nextEvent"),
        dae::Variable::new(dae::VarName::new("nextEvent")),
    );
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("nextEvent"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Ge(Default::default()),
                        lhs: Box::new(var("time")),
                        rhs: Box::new(dae::Expression::BuiltinCall {
                            function: dae::BuiltinFunction::Pre,
                            args: vec![var("nextEvent")],
                        }),
                    }],
                },
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(var("time")),
                    rhs: Box::new(real(1.0)),
                },
            )],
            else_branch: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![var("nextEvent")],
            }),
        },
        rumoca_core::Span::DUMMY,
        "nextEvent = if edge(time >= pre(nextEvent)) then time + 1 else pre(nextEvent)",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("time", 1.000001);
    env.set("nextEvent", 1.0);
    let mut guard_env = env.clone();
    guard_env.set("time", 1.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&guard_env);

    let changed = apply_discrete_partition_updates_with_guard_env_and_scalar_override(
        &dae_model,
        &mut env,
        &guard_env,
        |_eq, _target, _solution, _env, _implicit_clock_active| None,
    );

    assert!(changed);
    assert!((env.get("nextEvent") - 2.000001).abs() <= 1.0e-9);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn vectorized_assign_clock_updates_array_output_on_initial_tick() {
    let mut dae_model = dae::Dae::default();
    dae_model.algebraics.insert(
        dae::VarName::new("assignClock1.u"),
        dae::Variable {
            name: dae::VarName::new("assignClock1.u"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("assignClock1.y"),
        dae::Variable {
            name: dae::VarName::new("assignClock1.y"),
            dims: vec![2],
            ..Default::default()
        },
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("assignClock1.clock"),
        dae::Variable::new(dae::VarName::new("assignClock1.clock")),
    );
    dae_model.clock_schedules.push(dae::ClockSchedule {
        period_seconds: 0.02,
        phase_seconds: 0.0,
    });
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("assignClock1.clock"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![real(0.02)],
            is_constructor: false,
        },
        Span::DUMMY,
        "assignClock1.clock = Clock(0.02)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("assignClock1.y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![var("assignClock1.clock")],
                },
                var("assignClock1.u"),
            )],
            else_branch: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![var("assignClock1.y")],
            }),
        },
        Span::DUMMY,
        "assignClock1.y = if edge(assignClock1.clock) then assignClock1.u else pre(assignClock1.y)",
    ));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);
    env.set("assignClock1.u[1]", 1.0);
    env.set("assignClock1.u[2]", 2.0);
    env.set("assignClock1.y[1]", 0.0);
    env.set("assignClock1.y[2]", 0.0);
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([
        ("assignClock1.u".to_string(), vec![2]),
        ("assignClock1.y".to_string(), vec![2]),
    ]));

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!((env.get("assignClock1.y[1]") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("assignClock1.y[2]") - 2.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_array_target_clocked_sample_uses_left_limit_per_entry() {
    let dae_model = dae::Dae::default();
    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![
            var("sample1.u"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![real(0.02)],
                is_constructor: false,
            },
        ],
    };
    let eq = dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        solution.clone(),
        Span::DUMMY,
        "sample1.y = sample(sample1.u, Clock(0.02))",
    );
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.02);
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([
        ("sample1.u".to_string(), vec![2]),
        ("sample1.y".to_string(), vec![2]),
    ]));
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "sample1.u", &[2], &[5.0, 6.0]);
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "sample1.y", &[2], &[0.0, 0.0]);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("sample1.u[1]", 1.0);
    rumoca_eval_dae::runtime::set_pre_value("sample1.u[2]", 2.0);

    let changed = apply_scalar_discrete_partition_equation(
        ScalarDiscreteEquationInput {
            dae: &dae_model,
            eq: &eq,
            target: "sample1.y",
            solution: &solution,
            env: &mut env,
            rhs_env: None,
            implicit_clock_active: false,
        },
        |env, target, new_value| {
            let old = env.vars.get(target).copied().unwrap_or(0.0);
            env.set(target, new_value);
            (old - new_value).abs() > 1.0e-12
        },
        |_target, _old_value, _new_value| {},
    );

    assert!(changed);
    // MLS §16.5.1: sample(u, clk) reads the continuous source value at the
    // active tick. The left-limit of a continuous variable matches the
    // current event-time value, not pre(u).
    assert!((env.get("sample1.y[1]") - 5.0).abs() <= 1.0e-12);
    assert!((env.get("sample1.y[2]") - 6.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);
    env.set("time", 0.03);
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "sample1.u", &[2], &[9.0, 10.0]);

    let changed = apply_scalar_discrete_partition_equation(
        ScalarDiscreteEquationInput {
            dae: &dae_model,
            eq: &eq,
            target: "sample1.y",
            solution: &solution,
            env: &mut env,
            rhs_env: None,
            implicit_clock_active: false,
        },
        |env, target, new_value| {
            let old = env.vars.get(target).copied().unwrap_or(0.0);
            env.set(target, new_value);
            (old - new_value).abs() > 1.0e-12
        },
        |_target, _old_value, _new_value| {},
    );

    assert!(!changed);
    // MLS §16.5.1: sampled outputs hold their last sampled value between
    // ticks; changing the continuous source off-tick must not update y.
    assert!((env.get("sample1.y[1]") - 5.0).abs() <= 1.0e-12);
    assert!((env.get("sample1.y[2]") - 6.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_updates_guarded_implicit_clock_state_rows() {
    let dae_model = build_guarded_implicit_clock_pi_dae();
    let pre_env = seed_guarded_implicit_clock_pi_env();
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("time", 0.2);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!((env.get("periodicClock.y") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("PI.Ts") - 0.1).abs() <= 1.0e-12);
    assert!((env.get("PI.x") - 0.025).abs() <= 1.0e-12);
    assert!((env.get("PI.y") - 7.5).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn implicit_sample_active_uses_current_continuous_value() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("x", 5.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("x", 4.0);

    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("x")],
    };
    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, true);
    // MLS §16.5.1: sample(u) reads the continuous source at the active tick.
    assert!((value - 5.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn implicit_sample_active_uses_current_continuous_fill_value() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("x", 5.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("x", 4.0);

    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Fill,
            args: vec![var("x")],
        }],
    };
    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, true);
    assert!((value - 5.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn previous_expression_inactive_clock_holds_target_left_limit() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("x", 2.0);
    env.set("y", 1.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("y", 7.0);

    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("previous"),
        args: vec![var("x")],
        is_constructor: false,
    };
    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 7.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn state_target_assignment_uses_left_limit_state_value() {
    let mut dae_model = dae::Dae::default();
    dae_model.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    let mut env = VarEnv::<f64>::new();
    env.set("x", 2.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("x", -5.0);

    let solution = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Mul(Default::default()),
        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(-0.8))),
        rhs: Box::new(var("x")),
    };
    let value = eval_discrete_assignment_value(&dae_model, "x", &solution, &env, true);
    assert!((value - 4.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn state_target_assignment_uses_left_limit_fill_value() {
    let mut dae_model = dae::Dae::default();
    dae_model.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    let mut env = VarEnv::<f64>::new();
    env.set("x", 5.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("x", 2.0);

    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Fill,
        args: vec![dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        }],
    };
    let value = eval_discrete_assignment_value(&dae_model, "x", &solution, &env, true);
    assert!((value - 3.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_array_target_fast_paths_range_and_cat_values() {
    let dae_model = dae::Dae::default();
    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cat,
        args: vec![
            dae::Expression::Literal(dae::Literal::Integer(1)),
            dae::Expression::Range {
                start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                step: None,
                end: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            },
            dae::Expression::Tuple {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Real(3.0)),
                    dae::Expression::Literal(dae::Literal::Real(4.0)),
                ],
            },
        ],
    };
    let eq = dae::Equation::explicit(
        dae::VarName::new("y"),
        solution.clone(),
        rumoca_core::Span::DUMMY,
        "y = cat(1, 1:2, (3,4))",
    );
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("y".to_string(), vec![4])]));
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "y", &[4], &[0.0, 0.0, 0.0, 0.0]);

    let changed = apply_scalar_discrete_partition_equation(
        ScalarDiscreteEquationInput {
            dae: &dae_model,
            eq: &eq,
            target: "y",
            solution: &solution,
            env: &mut env,
            rhs_env: None,
            implicit_clock_active: true,
        },
        |env, target, new_value| {
            let old = env.vars.get(target).copied().unwrap_or(0.0);
            env.set(target, new_value);
            (old - new_value).abs() > 1.0e-12
        },
        |_target, _old_value, _new_value| {},
    );

    assert!(changed);
    assert!((env.get("y[1]") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("y[2]") - 2.0).abs() <= 1.0e-12);
    assert!((env.get("y[3]") - 3.0).abs() <= 1.0e-12);
    assert!((env.get("y[4]") - 4.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_assignment_value_handles_indexed_range_scalar_path() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("slot", 2.0);
    let solution = dae::Expression::Index {
        base: Box::new(dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
        }),
        subscripts: vec![dae::Subscript::Expr(Box::new(var("slot")))],
    };

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 2.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_assignment_value_handles_previous_indexed_history_scalar_path() {
    let dae_model = dae::Dae::default();
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("hist[1]", 5.0);
    rumoca_eval_dae::runtime::set_pre_value("hist[2]", 6.0);
    let mut env = VarEnv::<f64>::new();
    env.set("slot", 2.0);
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("hist".to_string(), vec![2])]));
    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("previous"),
        args: vec![dae::Expression::Index {
            base: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("hist"),
                subscripts: vec![],
            }),
            subscripts: vec![dae::Subscript::Expr(Box::new(var("slot")))],
        }],
        is_constructor: false,
    };

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 6.0).abs() <= 1.0e-12);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_array_target_fast_paths_binary_broadcast_expression() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("offset", 10.0);
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("y".to_string(), vec![3])]));
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "y", &[3], &[0.0, 0.0, 0.0]);
    let solution = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
        }),
        rhs: Box::new(var("offset")),
    };
    let eq = dae::Equation::explicit(
        dae::VarName::new("y"),
        solution.clone(),
        rumoca_core::Span::DUMMY,
        "y = 1:3 + offset",
    );

    let changed = apply_scalar_discrete_partition_equation(
        ScalarDiscreteEquationInput {
            dae: &dae_model,
            eq: &eq,
            target: "y",
            solution: &solution,
            env: &mut env,
            rhs_env: None,
            implicit_clock_active: true,
        },
        |env, target, new_value| {
            let old = env.vars.get(target).copied().unwrap_or(0.0);
            env.set(target, new_value);
            (old - new_value).abs() > 1.0e-12
        },
        |_target, _old_value, _new_value| {},
    );

    assert!(changed);
    assert!((env.get("y[1]") - 11.0).abs() <= 1.0e-12);
    assert!((env.get("y[2]") - 12.0).abs() <= 1.0e-12);
    assert!((env.get("y[3]") - 13.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_assignment_value_handles_tuple_elements_for_array_like_scalar_path() {
    let dae_model = dae::Dae::default();
    let env = VarEnv::<f64>::new();
    let solution = dae::Expression::Tuple {
        elements: vec![
            dae::Expression::Literal(dae::Literal::Real(4.5)),
            dae::Expression::Literal(dae::Literal::Real(5.5)),
        ],
    };

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 4.5).abs() <= 1.0e-12);
}

#[test]
fn discrete_partition_updates_handle_enum_lookup_table_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
            2,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4,
        ),
    ]);
    for name in ["lhs", "rhs", "y"] {
        dae_model.discrete_valued.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    let logic = |name: &str| dae::Expression::VarRef {
        name: dae::VarName::new(
            format!("Modelica.Electrical.Digital.Interfaces.Logic.'{name}'").as_str(),
        ),
        subscripts: vec![],
    };
    let lut = dae::Expression::Index {
        base: Box::new(dae::Expression::Array {
            elements: vec![
                dae::Expression::Array {
                    elements: vec![logic("U"), logic("X"), logic("0"), logic("1")],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![logic("X"), logic("0"), logic("1"), logic("U")],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![logic("0"), logic("1"), logic("U"), logic("X")],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![logic("1"), logic("U"), logic("X"), logic("0")],
                    is_matrix: false,
                },
            ],
            is_matrix: true,
        }),
        subscripts: vec![
            dae::Subscript::Expr(Box::new(var("lhs"))),
            dae::Subscript::Expr(Box::new(var("rhs"))),
        ],
    };
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        lut,
        Span::DUMMY,
        "y = lut[lhs, rhs]",
    ));

    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = std::sync::Arc::new(dae_model.enum_literal_ordinals.clone());
    env.set("lhs", 3.0);
    env.set("rhs", 4.0);
    env.set("y", 0.0);

    // MLS §10.5 / §10.6.9: the discrete runtime must evaluate scalar
    // lookup-table indexing with dynamic enum-valued subscripts, which the
    // digital library lowers heavily.
    let changed = apply_discrete_partition_updates(&dae_model, &mut env);
    assert!(changed);
    assert!((env.get("y") - 2.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_partition_updates_handle_dynamic_indexed_lhs_target() {
    let mut dae_model = dae::Dae::default();
    dae_model.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
    ]);

    let mut n = dae::Variable::new(dae::VarName::new("n"));
    n.start = Some(dae::Expression::Literal(dae::Literal::Integer(3)));
    dae_model.parameters.insert(dae::VarName::new("n"), n);

    let mut auxiliary = dae::Variable::new(dae::VarName::new("auxiliary"));
    auxiliary.dims = vec![3];
    auxiliary.start = Some(dae::Expression::VarRef {
        name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'U'"),
        subscripts: vec![],
    });
    dae_model
        .discrete_valued
        .insert(dae::VarName::new("auxiliary"), auxiliary);

    dae_model.f_m.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("auxiliary"),
                subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("n"),
                    subscripts: vec![],
                }))],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
                subscripts: vec![],
            }),
        },
        span: Span::DUMMY,
        origin: "auxiliary[n] = Logic.'0'".to_string(),
        scalar_count: 1,
    });

    let mut env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae_model, &[], 0.0);
    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!((env.get("auxiliary[1]") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("auxiliary[2]") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("auxiliary[3]") - 3.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_partition_updates_allows_scalar_override_callback() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("z"),
        dae::Expression::Literal(dae::Literal::Real(1.0)),
        rumoca_core::Span::DUMMY,
        "z assignment",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("z", 0.0);
    let changed = apply_discrete_partition_updates_with_scalar_override(
        &dae_model,
        &mut env,
        |_eq, _target, _solution, _env, _implicit_clock_active| Some(3.0),
    );
    assert!(changed);
    assert!((env.vars.get("z").copied().unwrap_or(0.0) - 3.0).abs() <= 1e-12);
}

#[test]
fn eval_clock_edge_assignment_handles_two_arg_timing_clock() {
    let dae_model = dae::Dae::default();
    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("Clock"),
        args: vec![var("count"), var("resolution")],
        is_constructor: false,
    };

    let mut env = VarEnv::<f64>::new();
    env.set("count", 3.0);
    env.set("resolution", 10.0);
    env.set("time", 0.45);
    assert_eq!(
        eval_clock_edge_assignment(&dae_model, &solution, &env),
        Some(0.0)
    );

    env.set("time", 0.6);
    assert_eq!(
        eval_clock_edge_assignment(&dae_model, &solution, &env),
        Some(1.0)
    );
}

#[test]
fn eval_sample_clock_active_resolves_exact_clock_alias_between_ticks() {
    let mut dae_model = dae::Dae::default();
    dae_model.algebraics.insert(
        dae::VarName::new("sample2.clock"),
        dae::Variable::new(dae::VarName::new("sample2.clock")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("periodicClock.c"),
        dae::Variable::new(dae::VarName::new("periodicClock.c")),
    );
    dae_model.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("periodicClock.c"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("sample2.clock"),
                subscripts: vec![],
            }),
        },
        rumoca_core::Span::DUMMY,
        "clock_alias",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
            is_constructor: false,
        },
        rumoca_core::Span::DUMMY,
        "periodic_clock",
    ));

    let clock_expr = dae::Expression::VarRef {
        name: dae::VarName::new("sample2.clock"),
        subscripts: vec![],
    };
    let mut env = VarEnv::<f64>::new();
    env.set("sample2.clock", 1.0);
    env.set("periodicClock.c", 1.0);
    env.set("time", 0.100001);
    assert!(
        !eval_sample_clock_active(&dae_model, &clock_expr, &env),
        "exact clock alias should not stay active between ticks"
    );

    env.set("time", 0.1);
    assert!(eval_sample_clock_active(&dae_model, &clock_expr, &env));
}

#[test]
fn sampled_output_tick_reads_current_continuous_input_but_pre_discrete_inputs() {
    let mut dae_model = dae::Dae::default();
    dae_model.algebraics.insert(
        dae::VarName::new("sample2.u"),
        dae::Variable::new(dae::VarName::new("sample2.u")),
    );
    for name in ["periodicClock.y", "sample2.clock", "bias", "sample2.y"] {
        dae_model.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
            is_constructor: false,
        },
        rumoca_core::Span::DUMMY,
        "periodicClock.y = Clock(0.1)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample2.clock"),
        var("periodicClock.y"),
        rumoca_core::Span::DUMMY,
        "sample2.clock = periodicClock.y",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample2.y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(var("sample2.u")),
                    rhs: Box::new(var("bias")),
                },
                var("sample2.clock"),
            ],
        },
        rumoca_core::Span::DUMMY,
        "sample2.y = sample(sample2.u + bias, sample2.clock)",
    ));

    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("time", 0.09);
    pre_env.set("sample2.u", 8.240426869418509e-5);
    pre_env.set("bias", 0.0);
    pre_env.set("periodicClock.y", 0.0);
    pre_env.set("sample2.clock", 0.0);
    pre_env.set("sample2.y", 8.240426869418509e-5);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("time", 0.1);
    env.set("sample2.u", 0.05);
    env.set("bias", 1.0);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!((env.get("periodicClock.y") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("sample2.clock") - 1.0).abs() <= 1.0e-12);
    assert!(
        (env.get("sample2.y") - 0.05).abs() <= 1.0e-12,
        "MLS §16.5.1: sample(u, clk) must read current continuous values at the tick while still using pre(discrete) for same-tick discrete inputs"
    );

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn sampled_output_tick_keeps_pre_value_for_discrete_source() {
    let mut dae_model = dae::Dae::default();
    for name in ["periodicClock.y", "sample2.clock", "source", "sample2.y"] {
        dae_model.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
            is_constructor: false,
        },
        rumoca_core::Span::DUMMY,
        "periodicClock.y = Clock(0.1)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample2.clock"),
        var("periodicClock.y"),
        rumoca_core::Span::DUMMY,
        "sample2.clock = periodicClock.y",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample2.y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var("source"), var("sample2.clock")],
        },
        rumoca_core::Span::DUMMY,
        "sample2.y = sample(source, sample2.clock)",
    ));

    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("time", 0.09);
    pre_env.set("source", 0.25);
    pre_env.set("periodicClock.y", 0.0);
    pre_env.set("sample2.clock", 0.0);
    pre_env.set("sample2.y", 0.25);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("time", 0.1);
    env.set("source", 0.75);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!((env.get("sample2.y") - 0.25).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn sampled_output_tick_recovers_missing_continuous_helper_from_runtime_alias_closure() {
    let mut dae_model = dae::Dae::default();
    dae_model.inputs.insert(
        dae::VarName::new("loadSrc"),
        dae::Variable::new(dae::VarName::new("loadSrc")),
    );
    for name in ["load.w", "speed.w", "sample1.u"] {
        dae_model.algebraics.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    for name in ["periodicClock.y", "sample1.clock", "sample1.y"] {
        dae_model.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("load.w"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(var("loadSrc")),
            rhs: Box::new(real(0.0)),
        },
        rumoca_core::Span::DUMMY,
        "load.w = loadSrc + 0.0",
    ));
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("speed.w"),
        var("load.w"),
        rumoca_core::Span::DUMMY,
        "speed.w = load.w",
    ));
    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("speed.w"),
        var("sample1.u"),
        rumoca_core::Span::DUMMY,
        "speed.w = sample1.u",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
            is_constructor: false,
        },
        rumoca_core::Span::DUMMY,
        "periodicClock.y = Clock(0.1)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample1.clock"),
        var("periodicClock.y"),
        rumoca_core::Span::DUMMY,
        "sample1.clock = periodicClock.y",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var("sample1.u"), var("sample1.clock")],
        },
        rumoca_core::Span::DUMMY,
        "sample1.y = sample(sample1.u, sample1.clock)",
    ));

    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("time", 0.09);
    pre_env.set("loadSrc", 0.05);
    pre_env.set("periodicClock.y", 0.0);
    pre_env.set("sample1.clock", 0.0);
    pre_env.set("sample1.y", 0.05);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("time", 0.1);
    env.set("loadSrc", 0.11);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!(
        (env.get("sample1.y") - 0.11).abs() <= 1.0e-12,
        "MLS §16.5.1: sample() must observe the current left-limit of a continuous source even when prepare/runtime eliminated an intermediate helper such as sample1.u; periodicClock.y={} sample1.clock={} loadSrc={} load.w={} speed.w={} sample1.u={} sample1.y={}",
        env.get("periodicClock.y"),
        env.get("sample1.clock"),
        env.get("loadSrc"),
        env.get("load.w"),
        env.get("speed.w"),
        env.get("sample1.u"),
        env.get("sample1.y"),
    );

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn sampled_output_tick_recovers_env_only_helper_from_alias_equation_not_in_var_tables() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("periodicClock.y"),
        dae::Variable::new(dae::VarName::new("periodicClock.y")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("sample1.clock"),
        dae::Variable::new(dae::VarName::new("sample1.clock")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("sample1.y"),
        dae::Variable::new(dae::VarName::new("sample1.y")),
    );
    dae_model.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("speed.w")),
            rhs: Box::new(var("sample1.u")),
        },
        rumoca_core::Span::DUMMY,
        "speed.w = sample1.u",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
            is_constructor: false,
        },
        rumoca_core::Span::DUMMY,
        "periodicClock.y = Clock(0.1)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample1.clock"),
        var("periodicClock.y"),
        rumoca_core::Span::DUMMY,
        "sample1.clock = periodicClock.y",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var("sample1.u"), var("sample1.clock")],
        },
        rumoca_core::Span::DUMMY,
        "sample1.y = sample(sample1.u, sample1.clock)",
    ));

    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("time", 0.09);
    pre_env.set("speed.w", 0.05);
    pre_env.set("periodicClock.y", 0.0);
    pre_env.set("sample1.clock", 0.0);
    pre_env.set("sample1.y", 0.05);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("time", 0.1);
    env.set("speed.w", 0.11);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!(
        (env.get("sample1.y") - 0.11).abs() <= 1.0e-12,
        "MLS §16.5.1: sample() must recover eliminated continuous helper inputs from the remaining exact-alias runtime closure even when names like sample1.u are gone from the prepared variable tables; speed.w={} sample1.u={} sample1.y={}",
        env.get("speed.w"),
        env.get("sample1.u"),
        env.get("sample1.y"),
    );

    rumoca_eval_dae::runtime::clear_pre_values();
}
