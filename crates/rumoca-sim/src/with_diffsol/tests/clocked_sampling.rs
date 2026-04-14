use super::*;
use crate::with_diffsol::test_support::{real, sub, var_ref};
use rumoca_ir_dae as dae;

fn build_stateful_clocked_sample_dae() -> Dae {
    let mut dae = Dae::new();
    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.start = Some(real(0.0));
    dae.states.insert(dae::VarName::new("x"), x);
    dae.outputs.insert(
        dae::VarName::new("y_out"),
        dae::Variable::new(dae::VarName::new("y_out")),
    );
    dae.algebraics.insert(
        dae::VarName::new("u"),
        dae::Variable::new(dae::VarName::new("u")),
    );
    for name in ["clk", "y"] {
        dae.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae.clock_schedules.push(dae::ClockSchedule {
        period_seconds: 0.1,
        phase_seconds: 0.0,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(1000.0),
        ),
        span: Span::DUMMY,
        origin: "x_ramp".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("u"), var_ref("x")),
        span: Span::DUMMY,
        origin: "u_alias".to_string(),
        scalar_count: 1,
    });
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("clk")),
        rhs: Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![real(0.1)],
            is_constructor: false,
        },
        span: Span::DUMMY,
        origin: "clk".to_string(),
        scalar_count: 1,
    });
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("y")),
        rhs: Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var_ref("u"), var_ref("clk")],
        },
        span: Span::DUMMY,
        origin: "sample".to_string(),
        scalar_count: 1,
    });
    dae
}

#[test]
fn test_simulate_no_state_initial_clocked_sample_uses_consistent_t_start_pre_values() {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("u"),
        dae::Variable::new(dae::VarName::new("u")),
    );
    for name in ["clk", "y"] {
        dae.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    // 0 = u - 0.1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("u"), real(0.1)),
        span: Span::DUMMY,
        origin: "u_const".to_string(),
        scalar_count: 1,
    });
    // 0 = y_out - y
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y_out"), var_ref("y")),
        span: Span::DUMMY,
        origin: "out_alias".to_string(),
        scalar_count: 1,
    });

    // clk = Clock(0.01)
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("clk")),
        rhs: Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![real(0.01)],
            is_constructor: false,
        },
        span: Span::DUMMY,
        origin: "clk".to_string(),
        scalar_count: 1,
    });
    // y = sample(u, clk)
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("y")),
        rhs: Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var_ref("u"), var_ref("clk")],
        },
        span: Span::DUMMY,
        origin: "sample".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.03,
            dt: Some(0.005),
            max_wall_seconds: Some(5.0),
            ..SimOptions::default()
        },
    )
    .expect("clocked no-state simulation should succeed");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y_out")
        .expect("result should include y_out");
    let y = &result.data[y_idx];
    assert!(
        !y.is_empty(),
        "expected output samples for y_out over the requested horizon"
    );
    assert!(
        (y[0] - 0.1).abs() < 1.0e-9,
        "y_out(t_start) should use consistent pre-seeded u=0.1, got {}",
        y[0]
    );
    assert!(
        y.iter().all(|value| (value - 0.1).abs() < 1.0e-9),
        "clocked sampled output should hold constant 0.1 in this setup: {:?}",
        y
    );
}

#[test]
fn test_simulate_no_state_clock_schedule_resolves_via_discrete_alias() {
    let mut dae = Dae::new();
    dae.outputs.insert(
        dae::VarName::new("y_out"),
        dae::Variable::new(dae::VarName::new("y_out")),
    );
    dae.algebraics.insert(
        dae::VarName::new("u"),
        dae::Variable::new(dae::VarName::new("u")),
    );
    for name in ["period", "clk", "y"] {
        dae.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("u"), real(0.1)),
        span: Span::DUMMY,
        origin: "u_const".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y_out"), var_ref("y")),
        span: Span::DUMMY,
        origin: "out_alias".to_string(),
        scalar_count: 1,
    });

    // period = 0.01
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("period")),
        rhs: real(0.01),
        span: Span::DUMMY,
        origin: "period_const".to_string(),
        scalar_count: 1,
    });
    // clk = Clock(period)
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("clk")),
        rhs: Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![var_ref("period")],
            is_constructor: false,
        },
        span: Span::DUMMY,
        origin: "clk_alias_period".to_string(),
        scalar_count: 1,
    });
    // y = sample(u, clk)
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("y")),
        rhs: Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var_ref("u"), var_ref("clk")],
        },
        span: Span::DUMMY,
        origin: "sample".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.03,
            dt: Some(0.005),
            max_wall_seconds: Some(5.0),
            ..SimOptions::default()
        },
    )
    .expect("clocked no-state simulation with aliased period should succeed");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y_out")
        .expect("result should include y_out");
    let y = &result.data[y_idx];
    assert!(
        y.iter().all(|value| (value - 0.1).abs() < 1.0e-9),
        "clocked sampled output should hold constant 0.1: {:?}",
        y
    );
}

#[test]
fn test_stateful_clocked_sample_observes_continuous_source_at_event_time() {
    let dae = build_stateful_clocked_sample_dae();
    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.21,
            dt: Some(0.2),
            max_wall_seconds: Some(5.0),
            ..SimOptions::default()
        },
    )
    .expect("stateful clocked sampling should succeed");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result should include discrete sampled channel y");
    let y = &result.data[y_idx];
    let idx_01 = result
        .times
        .iter()
        .position(|t| (*t - 0.1).abs() < 1.0e-12)
        .expect("expected event observation at t=0.1");
    let idx_02 = result
        .times
        .iter()
        .position(|t| (*t - 0.2).abs() < 1.0e-12)
        .expect("expected event observation at t=0.2");

    // MLS §16.5.1: sample(u, clk) captures the continuous source at the clock
    // tick itself, not from the post-event restart state just after the tick.
    let tol = 2.0e-3;
    assert!(
        (y[idx_01] - 100.0).abs() < tol,
        "expected y(t=0.1)≈100.0 from x(t_tick), got {} with times {:?} and series {:?}",
        y[idx_01],
        result.times,
        y
    );
    assert!(
        (y[idx_02] - 200.0).abs() < tol,
        "expected y(t=0.2)≈200.0 from x(t_tick), got {} with times {:?} and series {:?}",
        y[idx_02],
        result.times,
        y
    );
}

#[test]
fn test_stateful_runtime_capture_preserves_direct_time_threshold_history() {
    let mut dae = Dae::new();
    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.start = Some(real(0.0));
    dae.states.insert(dae::VarName::new("x"), x);
    dae.outputs.insert(
        dae::VarName::new("y_out"),
        dae::Variable::new(dae::VarName::new("y_out")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "x_ramp".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y_out"), var_ref("y")),
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });
    dae.f_z.push(dae::Equation {
        lhs: Some(dae::VarName::new("y")),
        rhs: Expression::If {
            branches: vec![(
                Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Ge(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.05)),
                    }],
                },
                var_ref("time"),
            )],
            else_branch: Box::new(Expression::If {
                branches: vec![(
                    Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Initial,
                        args: vec![],
                    },
                    var_ref("y"),
                )],
                else_branch: Box::new(Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![var_ref("y")],
                }),
            }),
        },
        span: Span::DUMMY,
        // MLS §8.5 / §8.6: a direct time-threshold event still updates the
        // discrete right limit at the exact event instant, and later regular
        // output samples must observe that settled history through pre(y).
        origin: "y := if edge(time >= 0.05) then time else if initial() then y else pre(y)"
            .to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.09,
            dt: Some(0.09),
            max_wall_seconds: Some(5.0),
            ..SimOptions::default()
        },
    )
    .expect("stateful runtime capture should preserve direct time-threshold history");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result should include discrete channel y");
    let y = &result.data[y_idx];
    let y_end = *y.last().expect("expected final y sample");
    assert!(
        (y_end - 0.05).abs() < 1.0e-9,
        "expected y(t_end) to hold the right-limit event value 0.05, got {y_end}; times={:?} y={:?}",
        result.times,
        y
    );
}
