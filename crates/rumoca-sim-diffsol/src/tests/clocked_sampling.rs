use super::*;
use crate::test_support::{real, sub, var_ref};
use rumoca_ir_dae as dae;

#[test]
fn test_simulate_no_state_initial_clocked_sample_uses_consistent_t_start_pre_values() {
    let mut dae = Dae::new();
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
