use super::*;

fn build_wrapped_seed_connected_solver_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["drive", "conn", "current", "z"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            sub(var_ref("drive"), real(0.0)),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(5.0),
                )],
                else_branch: Box::new(real(-5.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "wrapped_runtime_source".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("drive"), var_ref("conn")),
        span: Span::DUMMY,
        origin: "connection equation: drive = conn".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("conn"),
            Expression::Binary {
                op: OpBinary::Mul(Default::default()),
                lhs: Box::new(real(1000.0)),
                rhs: Box::new(var_ref("current")),
            },
        ),
        span: Span::DUMMY,
        origin: "ohms_law".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(var_ref("z")),
                rhs: Box::new(var_ref("z")),
            },
            real(2.0),
        ),
        span: Span::DUMMY,
        origin: "implicit_projection_anchor".to_string(),
        scalar_count: 1,
    });
    dae
}

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

#[test]
fn test_simulate_no_state_projection_seeds_runtime_discrete_targets_from_projected_algebraics() {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("table_y"),
        Variable::new(VarName::new("table_y")),
    );
    dae.outputs
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.discrete_reals
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("table_y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(0.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "projected_table_y".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("table_y"), var_ref("u")),
        span: Span::DUMMY,
        origin: "runtime_discrete_projection_alias".to_string(),
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
    .expect("simulation should seed runtime discrete targets after no-state projection");

    let u_idx = result
        .names
        .iter()
        .position(|name| name == "u" || name == "u[1]")
        .unwrap_or_else(|| panic!("u should appear in outputs, got {:?}", result.names));
    let u_series = &result.data[u_idx];
    assert_eq!(u_series.len(), result.times.len());

    for (&t, &u) in result.times.iter().zip(u_series.iter()) {
        let expected = if t > 0.5 + 1.0e-12 { 1.0 } else { 0.0 };
        assert!(
            (u - expected).abs() < 1.0e-9,
            "expected u={expected} at t={t}, got {u}"
        );
    }
}

#[test]
fn test_simulate_no_state_projection_refreshes_connected_solver_unknowns_after_wrapped_seed() {
    let dae = build_wrapped_seed_connected_solver_dae();
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
    let drive = &result.data[idx("drive")];
    let conn = &result.data[idx("conn")];
    let current = &result.data[idx("current")];

    for ((&t, &drive_v), (&conn_v, &current_v)) in result
        .times
        .iter()
        .zip(drive.iter())
        .zip(conn.iter().zip(current.iter()))
    {
        let expected_drive = if t < 0.5 - 1.0e-12 { 5.0 } else { -5.0 };
        let expected_current = expected_drive / 1000.0;
        assert!(
            (drive_v - expected_drive).abs() < 1.0e-9,
            "expected drive={expected_drive} at t={t}, got {drive_v}"
        );
        assert!(
            (conn_v - expected_drive).abs() < 1.0e-9,
            "expected conn to follow drive after projection refresh at t={t}, got {conn_v}"
        );
        assert!(
            (current_v - expected_current).abs() < 1.0e-9,
            "expected current={expected_current} at t={t}, got {current_v}"
        );
    }
}

#[test]
fn test_simulate_no_state_projection_bootstraps_initial_discrete_seed_values() {
    let mut dae = Dae::new();
    for name in ["drive", "z"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    dae.discrete_reals.insert(
        VarName::new("t_start"),
        Variable::new(VarName::new("t_start")),
    );
    dae.initial_equations.push(dae::Equation::explicit(
        VarName::new("t_start"),
        real(-1.0),
        Span::DUMMY,
        "init t_start=-1",
    ));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            sub(var_ref("drive"), real(0.0)),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(var_ref("t_start")),
                    },
                    real(5.0),
                )],
                else_branch: Box::new(real(-5.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "wrapped_runtime_source_with_initial_discrete".to_string(),
        scalar_count: 1,
    });
    // Keep the live path on runtime projection rather than the solver-only
    // direct sampling fast path.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(var_ref("z")),
                rhs: Box::new(var_ref("z")),
            },
            real(2.0),
        ),
        span: Span::DUMMY,
        origin: "implicit_projection_anchor".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.5,
            dt: Some(0.5),
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    )
    .expect("simulation should seed runtime direct assignments from initial discrete values");

    let drive_idx = result
        .names
        .iter()
        .position(|name| name == "drive")
        .unwrap_or_else(|| panic!("drive should appear in outputs, got {:?}", result.names));
    for (&t, &drive) in result.times.iter().zip(result.data[drive_idx].iter()) {
        assert!(
            (drive - 5.0).abs() < 1.0e-9,
            "expected drive=5.0 from initialized t_start at t={t}, got {drive}"
        );
    }
}

#[test]
fn test_simulate_no_state_projection_recomputes_dependent_discrete_partition_after_seed() {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("table_y"),
        Variable::new(VarName::new("table_y")),
    );
    dae.outputs
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.discrete_reals
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.outputs
        .insert(VarName::new("flag"), Variable::new(VarName::new("flag")));
    dae.discrete_valued
        .insert(VarName::new("flag"), Variable::new(VarName::new("flag")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("table_y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(0.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "projected_table_y".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("table_y"), var_ref("u")),
        span: Span::DUMMY,
        origin: "runtime_discrete_projection_alias".to_string(),
        scalar_count: 1,
    });
    dae.f_m.push(dae::Equation::explicit(
        VarName::new("flag"),
        Expression::Binary {
            op: OpBinary::Ge(Default::default()),
            lhs: Box::new(var_ref("u")),
            rhs: Box::new(real(0.5)),
        },
        Span::DUMMY,
        "flag := u >= 0.5",
    ));

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
    .expect("simulation should recompute dependent discrete outputs after projection seed");

    let flag_idx = result
        .names
        .iter()
        .position(|name| name == "flag" || name == "flag[1]")
        .unwrap_or_else(|| panic!("flag should appear in outputs, got {:?}", result.names));
    let flag_series = &result.data[flag_idx];
    assert_eq!(flag_series.len(), result.times.len());

    for (&t, &flag) in result.times.iter().zip(flag_series.iter()) {
        let expected = if t > 0.5 + 1.0e-12 { 1.0 } else { 0.0 };
        assert!(
            (flag - expected).abs() < 1.0e-9,
            "expected flag={expected} at t={t}, got {flag}"
        );
    }
}

#[test]
fn test_simulate_no_state_without_projection_seeds_solver_backed_runtime_discrete_target() {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("table_y"),
        Variable::new(VarName::new("table_y")),
    );
    dae.outputs
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.discrete_reals
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("table_y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(0.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "projected_table_y".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("table_y"), var_ref("u")),
        span: Span::DUMMY,
        origin: "runtime_discrete_solver_alias".to_string(),
        scalar_count: 1,
    });

    assert!(
        !crate::problem::no_state_runtime_projection_required(&dae, 0),
        "solver-backed runtime discrete targets should stay on the no-projection path",
    );

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
    .expect("simulation should seed runtime discrete targets on the no-projection path");

    let u_idx = result
        .names
        .iter()
        .position(|name| name == "u" || name == "u[1]")
        .unwrap_or_else(|| panic!("u should appear in outputs, got {:?}", result.names));
    let u_series = &result.data[u_idx];
    assert_eq!(u_series.len(), result.times.len());

    for (&t, &u) in result.times.iter().zip(u_series.iter()) {
        let expected = if t > 0.5 + 1.0e-12 { 1.0 } else { 0.0 };
        assert!(
            (u - expected).abs() < 1.0e-9,
            "expected u={expected} at t={t}, got {u}"
        );
    }
}

#[test]
fn test_simulate_no_state_projection_falls_back_when_change_is_not_compilable() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.discrete_valued
        .insert(VarName::new("flag"), Variable::new(VarName::new("flag")));

    // MLS §3.7.5 / Appendix B: `change(flag)` is a left-limit/event operator.
    // The no-state projection path must keep working even when compiled PR2
    // rows cannot lower it and have to fall back to runtime evaluation.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("y"),
            Expression::If {
                branches: vec![(
                    Expression::BuiltinCall {
                        function: BuiltinFunction::Change,
                        args: vec![var_ref("flag")],
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(0.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "runtime_change_guard".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.05),
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    )
    .expect("no-state runtime projection should fall back for change(flag)");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y" || name == "y[1]")
        .unwrap_or_else(|| panic!("y should appear in outputs, got {:?}", result.names));
    let y_series = &result.data[y_idx];
    assert_eq!(y_series.len(), result.times.len());
    assert!(
        y_series.iter().all(|value| value.abs() < 1.0e-9),
        "expected change(flag) to stay false with no flag updates, got {:?}",
        y_series
    );
}
