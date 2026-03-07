use super::*;

#[test]
fn test_simulate_no_state_preserves_runtime_equation_output_alias() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), var_ref("z")),
        span: Span::DUMMY,
        origin: "alias".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(4.0),
                )],
                else_branch: Box::new(real(3.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "runtime-y".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.5),
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    )
    .expect("simulation should succeed");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y" || name == "y[1]")
        .unwrap_or_else(|| panic!("y should appear in outputs, got {:?}", result.names));
    let y_series = &result.data[y_idx];
    assert_eq!(y_series.len(), result.times.len());
    assert!(
        (y_series.first().copied().unwrap_or_default() - 3.0).abs() < 1.0e-9,
        "expected y(0) from runtime algorithm branch"
    );
    assert!(
        (y_series.last().copied().unwrap_or_default() - 4.0).abs() < 1.0e-9,
        "expected y(t_end) from runtime algorithm branch"
    );
}

#[test]
fn test_simulate_no_state_time_guard_step_trace() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // 0 = y - (if time > 5 then 1 else 0)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(5.0)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(0.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "time_guard_step".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 10.0,
            dt: Some(0.5),
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    )
    .expect("simulation should succeed");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y" || name == "y[1]")
        .unwrap_or_else(|| panic!("y should appear in outputs, got {:?}", result.names));
    let y_series = &result.data[y_idx];
    assert_eq!(y_series.len(), result.times.len());

    let mut saw_post_switch = false;
    for (&t, &y) in result.times.iter().zip(y_series.iter()) {
        if t <= 5.0 + 1.0e-12 {
            assert!(
                (y - 0.0).abs() < 1.0e-9,
                "expected y=0.0 up to t=5, got y={y} at t={t}"
            );
        } else {
            saw_post_switch = true;
            assert!(
                (y - 1.0).abs() < 1.0e-9,
                "expected y=1.0 after t=5, got y={y} at t={t}"
            );
        }
    }
    assert!(
        saw_post_switch,
        "expected at least one sample strictly after t=5"
    );
}

#[test]
fn test_simulate_no_state_propagates_aliases_after_runtime_algorithm_updates() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));

    // Keep one extra algebraic equation so `y` and `x` are both inside the
    // no-state solver vector prefix (`n_total = f_x.len()` after dummy state
    // injection).
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("a"), real(0.0)),
        span: Span::DUMMY,
        origin: "a_zero".to_string(),
        scalar_count: 1,
    });
    // 0 = x - y (runtime-driven alias)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("y")),
        span: Span::DUMMY,
        origin: "alias_xy".to_string(),
        scalar_count: 1,
    });

    // Runtime equation drives y.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(2.0),
                )],
                else_branch: Box::new(real(1.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "runtime_y".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.5),
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    )
    .expect("simulation should succeed");

    let idx = |name: &str| -> usize {
        result
            .names
            .iter()
            .position(|n| n == name)
            .unwrap_or_else(|| panic!("missing channel {name}, got {:?}", result.names))
    };
    let y = &result.data[idx("y")];
    let x = &result.data[idx("x")];
    assert_eq!(y.len(), result.times.len());
    assert_eq!(x.len(), result.times.len());

    for (&yv, &xv) in y.iter().zip(x.iter()) {
        assert!(
            (xv - yv).abs() < 1.0e-9,
            "expected x to alias y, got x={xv} y={yv}"
        );
    }
}
