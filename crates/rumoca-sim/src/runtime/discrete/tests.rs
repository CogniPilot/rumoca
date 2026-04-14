use super::*;
use rumoca_core::Span;

fn var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn real(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn int(value: i64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Integer(value))
}

fn insert_algebraics(dae_model: &mut dae::Dae, names: &[&str]) {
    for name in names {
        dae_model.algebraics.insert(
            dae::VarName::new(*name),
            dae::Variable::new(dae::VarName::new(*name)),
        );
    }
}

fn insert_discrete_reals(dae_model: &mut dae::Dae, names: &[&str]) {
    for name in names {
        dae_model.discrete_reals.insert(
            dae::VarName::new(*name),
            dae::Variable::new(dae::VarName::new(*name)),
        );
    }
}

fn insert_discrete_valued(dae_model: &mut dae::Dae, names: &[&str]) {
    for name in names {
        dae_model.discrete_valued.insert(
            dae::VarName::new(*name),
            dae::Variable::new(dae::VarName::new(*name)),
        );
    }
}

fn insert_parameters(dae_model: &mut dae::Dae, names: &[&str]) {
    for name in names {
        dae_model.parameters.insert(
            dae::VarName::new(*name),
            dae::Variable::new(dae::VarName::new(*name)),
        );
    }
}

fn clock_call_real(period: f64) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("Clock"),
        args: vec![real(period)],
        is_constructor: false,
    }
}

fn shift_sample_call(
    input: dae::Expression,
    shift_counter: dae::Expression,
    resolution: dae::Expression,
) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("shiftSample"),
        args: vec![input, shift_counter, resolution],
        is_constructor: false,
    }
}

fn hold_call(input: dae::Expression) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new("hold"),
        args: vec![input],
        is_constructor: false,
    }
}

fn sample_call(signal: dae::Expression, clock: dae::Expression) -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![signal, clock],
    }
}

fn add_sample_shift_hold_chain(
    dae_model: &mut dae::Dae,
    sample_clock: dae::Expression,
    shift_counter: dae::Expression,
    resolution: dae::Expression,
) {
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        sample_call(var("sample1.u"), sample_clock),
        Span::DUMMY,
        "sample1.y = sample(sample1.u, sample1.clock)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("shiftSample1.u"),
        var("sample1.y"),
        Span::DUMMY,
        "shiftSample1.u = sample1.y",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("shiftSample1.y"),
        shift_sample_call(var("shiftSample1.u"), shift_counter, resolution),
        Span::DUMMY,
        "shiftSample1.y = shiftSample(shiftSample1.u, shiftCounter, resolution)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("hold1.u"),
        var("shiftSample1.y"),
        Span::DUMMY,
        "hold1.u = shiftSample1.y",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("hold1.y"),
        hold_call(var("hold1.u")),
        Span::DUMMY,
        "hold1.y = hold(hold1.u)",
    ));
}

fn build_guarded_implicit_clock_pi_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_discrete_reals(
        &mut dae_model,
        &["periodicClock.y", "PI.Ts", "PI.u", "PI.x", "PI.y"],
    );
    dae_model.clock_schedules.push(dae::ClockSchedule {
        period_seconds: 0.1,
        phase_seconds: 0.0,
    });
    dae_model.clock_intervals.insert("PI.u".to_string(), 0.1);
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("PI.Ts"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("interval"),
            args: vec![var("PI.u")],
            is_constructor: false,
        },
        Span::DUMMY,
        "PI.Ts = interval(PI.u)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        clock_call_real(0.1),
        Span::DUMMY,
        "periodicClock.y = Clock(0.1)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("PI.x"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![dae::Expression::FunctionCall {
                        name: dae::VarName::new("Clock"),
                        args: vec![],
                        is_constructor: false,
                    }],
                },
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(dae::Expression::FunctionCall {
                        name: dae::VarName::new("previous"),
                        args: vec![var("PI.x")],
                        is_constructor: false,
                    }),
                    rhs: Box::new(dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                        lhs: Box::new(var("PI.u")),
                        rhs: Box::new(dae::Expression::Binary {
                            op: rumoca_ir_core::OpBinary::Div(Default::default()),
                            lhs: Box::new(var("PI.Ts")),
                            rhs: Box::new(real(0.2)),
                        }),
                    }),
                },
            )],
            else_branch: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![var("PI.x")],
            }),
        },
        Span::DUMMY,
        "PI.x = if edge(Clock()) then previous(PI.x) + PI.u * PI.Ts / 0.2 else pre(PI.x)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("PI.y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![dae::Expression::FunctionCall {
                        name: dae::VarName::new("Clock"),
                        args: vec![],
                        is_constructor: false,
                    }],
                },
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Mul(Default::default()),
                    lhs: Box::new(real(100.0)),
                    rhs: Box::new(dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Add(Default::default()),
                        lhs: Box::new(var("PI.x")),
                        rhs: Box::new(var("PI.u")),
                    }),
                },
            )],
            else_branch: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![var("PI.y")],
            }),
        },
        Span::DUMMY,
        "PI.y = if edge(Clock()) then 100 * (PI.x + PI.u) else pre(PI.y)",
    ));
    dae_model
}

fn seed_guarded_implicit_clock_pi_env() -> VarEnv<f64> {
    let mut pre_env = VarEnv::<f64>::new();
    pre_env.clock_intervals =
        std::sync::Arc::new(indexmap::IndexMap::from([("PI.u".to_string(), 0.1)]));
    pre_env.set("time", 0.1);
    pre_env.set("periodicClock.y", 0.0);
    pre_env.set("PI.Ts", 0.0);
    pre_env.set("PI.u", 0.05);
    pre_env.set("PI.x", 0.0);
    pre_env.set("PI.y", 0.0);
    pre_env
}

fn build_solver_backed_shift_hold_chain_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_algebraics(
        &mut dae_model,
        &[
            "sample1.clock",
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
        ],
    );
    insert_discrete_reals(&mut dae_model, &["periodicClock.c"]);
    dae_model.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("periodicClock.c")),
            rhs: Box::new(var("sample1.clock")),
        },
        Span::DUMMY,
        "periodicClock.c - sample1.clock",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        clock_call_real(0.02),
        Span::DUMMY,
        "periodicClock.c = Clock(0.02)",
    ));
    add_sample_shift_hold_chain(&mut dae_model, var("sample1.clock"), real(2.0), real(1.0));
    dae_model
}

fn build_flattened_clock_alias_shift_hold_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_discrete_valued(
        &mut dae_model,
        &[
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
        ],
    );
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        var("sample1.clock"),
        Span::DUMMY,
        "periodicClock.y = sample1.clock",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        clock_call_real(0.02),
        Span::DUMMY,
        "periodicClock.c = Clock(0.02)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        var("periodicClock.c"),
        Span::DUMMY,
        "periodicClock.y = periodicClock.c",
    ));
    add_sample_shift_hold_chain(&mut dae_model, var("sample1.clock"), real(2.0), real(1.0));
    dae_model
}

fn build_periodic_exact_subsample_chain_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model
        .enum_literal_ordinals
        .insert("Modelica.Clocked.Types.Resolution.s".to_string(), 5);
    insert_parameters(
        &mut dae_model,
        &[
            "periodicClock.factor",
            "periodicClock.resolution",
            "periodicClock.resolutionFactor",
            "subSample1.factor",
        ],
    );
    insert_discrete_valued(
        &mut dae_model,
        &[
            "periodicClock.c",
            "periodicClock.y",
            "subSample1.u",
            "subSample1.y",
            "sample1.clock",
            "sample1.u",
            "sample1.y",
        ],
    );
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Lt(Default::default()),
                    lhs: Box::new(var("periodicClock.resolution")),
                    rhs: Box::new(var("Modelica.Clocked.Types.Resolution.s")),
                },
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("subSample"),
                    args: vec![
                        dae::Expression::FunctionCall {
                            name: dae::VarName::new("Clock"),
                            args: vec![var("periodicClock.factor")],
                            is_constructor: false,
                        },
                        var("periodicClock.resolutionFactor"),
                    ],
                    is_constructor: false,
                },
            )],
            else_branch: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![
                    var("periodicClock.factor"),
                    var("periodicClock.resolutionFactor"),
                ],
                is_constructor: false,
            }),
        },
        Span::DUMMY,
        "periodicClock.c = if resolution < s then subSample(Clock(factor), resolutionFactor) else Clock(factor, resolutionFactor)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        var("subSample1.u"),
        Span::DUMMY,
        "periodicClock.y = subSample1.u",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        var("periodicClock.c"),
        Span::DUMMY,
        "periodicClock.y = periodicClock.c",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("subSample1.y"),
        var("sample1.clock"),
        Span::DUMMY,
        "subSample1.y = sample1.clock",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("subSample1.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("subSample"),
            args: vec![var("subSample1.u"), var("subSample1.factor")],
            is_constructor: false,
        },
        Span::DUMMY,
        "subSample1.y = subSample(subSample1.u, subSample1.factor)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        sample_call(var("sample1.u"), var("sample1.clock")),
        Span::DUMMY,
        "sample1.y = sample(sample1.u, sample1.clock)",
    ));
    dae_model
}

fn seed_periodic_exact_subsample_env() -> VarEnv<f64> {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);
    env.set("periodicClock.factor", 20.0);
    env.set("periodicClock.resolution", 6.0);
    env.set("periodicClock.resolutionFactor", 1000.0);
    env.set("subSample1.factor", 3.0);
    env.set("Modelica.Clocked.Types.Resolution.s", 5.0);
    env.set("sample1.u", 0.1);
    env.set("sample1.y", 0.0);
    env
}

fn build_parameterized_shift_hold_chain_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    insert_discrete_valued(
        &mut dae_model,
        &[
            "sample1.u",
            "sample1.y",
            "shiftSample1.u",
            "shiftSample1.y",
            "hold1.u",
            "hold1.y",
        ],
    );
    insert_parameters(
        &mut dae_model,
        &[
            "periodicClock.factor",
            "periodicClock.resolutionFactor",
            "shiftSample1.shiftCounter",
            "shiftSample1.resolution",
        ],
    );
    add_sample_shift_hold_chain(
        &mut dae_model,
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![
                var("periodicClock.factor"),
                var("periodicClock.resolutionFactor"),
            ],
            is_constructor: false,
        },
        var("shiftSample1.shiftCounter"),
        var("shiftSample1.resolution"),
    );
    dae_model
}

fn seed_parameterized_shift_hold_env() -> VarEnv<f64> {
    let mut env = VarEnv::<f64>::new();
    env.set("periodicClock.factor", 20.0);
    env.set("periodicClock.resolutionFactor", 1000.0);
    env.set("shiftSample1.shiftCounter", 2.0);
    env.set("shiftSample1.resolution", 1.0);
    env
}

fn run_parameterized_shift_hold_series(
    dae_model: &dae::Dae,
    env: &mut VarEnv<f64>,
    times: &[(f64, f64)],
) -> (Vec<f64>, Vec<f64>) {
    let mut shift_series = Vec::new();
    let mut hold_series = Vec::new();
    for (time, sample_u) in times {
        env.set("time", *time);
        env.set("sample1.u", *sample_u);
        apply_discrete_partition_updates(dae_model, env);
        let event_time = (((*time) / 0.02).round() * 0.02 - *time).abs() <= 1.0e-12;
        rumoca_eval_dae::runtime::seed_pre_values_from_env(env);
        if event_time {
            let _ = refresh_post_event_observation_values_at_time(dae_model, env, *time + 1.0e-6);
        }
        shift_series.push(env.get("shiftSample1.y"));
        hold_series.push(env.get("hold1.y"));
    }
    (shift_series, hold_series)
}

mod cases_a;
mod cases_b;
