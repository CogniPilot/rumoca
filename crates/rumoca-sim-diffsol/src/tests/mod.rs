use super::*;
use crate::test_support::{comp_ref, real, sub, var_ref};
use rumoca_ir_dae as dae;
type BuiltinFunction = dae::BuiltinFunction;
type ExternalFunction = dae::ExternalFunction;
type Function = dae::Function;
type FunctionParam = dae::FunctionParam;
type Literal = dae::Literal;
type OpBinary = dae::OpBinary;
type Statement = dae::Statement;
type VarName = dae::VarName;
type Variable = dae::Variable;

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
fn test_sim_options_default() {
    let opts = SimOptions::default();
    assert_eq!(opts.t_start, 0.0);
    assert_eq!(opts.t_end, 1.0);
    assert_eq!(opts.rtol, 1e-6);
    assert_eq!(opts.atol, 1e-6);
    assert!(opts.scalarize);
    assert_eq!(opts.max_wall_seconds, None);
    assert_eq!(opts.solver_mode, SimSolverMode::Auto);
}

#[test]
fn test_record_outputs_until_accepts_boundary_sample_with_time_tolerance() {
    let t_out_list = vec![0.0, 0.20000000000000015, 0.4];
    let mut t_out_idx = 1usize; // t=0.0 is already captured at initialization.
    let mut buf = OutputBuffers::new(1, t_out_list.len());
    let mut captured = Vec::new();

    record_outputs_until(&t_out_list, &mut t_out_idx, 0.2, &mut buf, |t_interp, _| {
        captured.push(t_interp);
        Ok(())
    })
    .expect("recording boundary sample should succeed");

    assert_eq!(t_out_idx, 2);
    assert_eq!(captured.len(), 1);
    assert!(
        (captured[0] - 0.2).abs() <= 1e-12,
        "boundary sample should be clamped to stop time, got {}",
        captured[0]
    );
}

#[test]
fn test_record_outputs_until_rejects_sample_beyond_tolerance() {
    let t_out_list = vec![0.0, 0.2001, 0.4];
    let mut t_out_idx = 1usize;
    let mut buf = OutputBuffers::new(1, t_out_list.len());
    let mut captured = Vec::new();

    record_outputs_until(&t_out_list, &mut t_out_idx, 0.2, &mut buf, |t_interp, _| {
        captured.push(t_interp);
        Ok(())
    })
    .expect("recording should succeed");

    assert_eq!(t_out_idx, 1);
    assert!(captured.is_empty());
}

#[test]
fn test_startup_interval_cap_uses_positive_finite_interval() {
    let opts = SimOptions {
        dt: Some(0.001),
        ..SimOptions::default()
    };
    let cap = startup_interval_cap(&opts).expect("positive finite dt should produce startup cap");
    assert!((cap - 0.02).abs() < 1e-15);
}

#[test]
fn test_startup_interval_cap_rejects_invalid_interval() {
    let mut opts = SimOptions::default();
    assert!(startup_interval_cap(&opts).is_none());

    opts.dt = Some(0.0);
    assert!(startup_interval_cap(&opts).is_none());

    opts.dt = Some(-1.0);
    assert!(startup_interval_cap(&opts).is_none());

    opts.dt = Some(f64::NAN);
    assert!(startup_interval_cap(&opts).is_none());
}

#[test]
fn test_startup_interval_cap_ignores_extremely_fine_output_intervals() {
    let mut opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        ..SimOptions::default()
    };
    opts.dt = Some(1.0e-5);
    assert!(startup_interval_cap(&opts).is_none());
}

#[test]
fn test_startup_interval_cap_scales_regular_intervals() {
    let mut opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        ..SimOptions::default()
    };
    opts.dt = Some(1.0e-3);
    let cap = startup_interval_cap(&opts).expect("expected startup interval cap");
    assert!((cap - 2.0e-2).abs() < 1.0e-15);
}

#[test]
fn test_nonlinear_solver_tolerance_tracks_sim_tolerances() {
    let mut opts = SimOptions {
        atol: 1.0e-6,
        rtol: 1.0e-6,
        ..SimOptions::default()
    };
    let default_tol = nonlinear_solver_tolerance(&opts, SolverStartupProfile::Default);
    assert!((default_tol - 1.0e-5).abs() < 1.0e-15);

    let robust_tol = nonlinear_solver_tolerance(&opts, SolverStartupProfile::RobustTinyStep);
    assert!((robust_tol - 1.0e-4).abs() < 1.0e-15);

    opts.atol = 1.0e-16;
    opts.rtol = 1.0e-16;
    let clamped_low = nonlinear_solver_tolerance(&opts, SolverStartupProfile::Default);
    assert!((clamped_low - 1.0e-8).abs() < 1.0e-18);

    opts.atol = 1.0;
    opts.rtol = 1.0;
    let clamped_high_default = nonlinear_solver_tolerance(&opts, SolverStartupProfile::Default);
    assert!((clamped_high_default - 1.0e-3).abs() < 1.0e-15);
    let clamped_high_robust =
        nonlinear_solver_tolerance(&opts, SolverStartupProfile::RobustTinyStep);
    assert!((clamped_high_robust - 1.0e-2).abs() < 1.0e-15);
}

#[test]
fn test_solver_mode_external_name_mapping() {
    assert_eq!(
        SimSolverMode::from_external_name("dassl"),
        SimSolverMode::Bdf
    );
    assert_eq!(SimSolverMode::from_external_name("IDA"), SimSolverMode::Bdf);
    assert_eq!(
        SimSolverMode::from_external_name("rungekutta"),
        SimSolverMode::RkLike
    );
    assert_eq!(
        SimSolverMode::from_external_name("trbdf2"),
        SimSolverMode::RkLike
    );
    assert_eq!(
        SimSolverMode::from_external_name("unknown"),
        SimSolverMode::Bdf
    );
    assert_eq!(SimSolverMode::from_external_name(""), SimSolverMode::Bdf);
    assert_eq!(
        SimSolverMode::from_external_name("auto"),
        SimSolverMode::Auto
    );
}

#[test]
fn test_is_interpolation_outside_step_sim_error_detects_expected_messages() {
    let interpolation_err = SimError::SolverError(
        "Interpolation failed at t=1.0: ODE solver error: InterpolationTimeOutsideCurrentStep"
            .to_string(),
    );
    assert!(is_interpolation_outside_step_sim_error(&interpolation_err));

    let other_err = SimError::SolverError("Step size is too small at time = 0.1".to_string());
    assert!(!is_interpolation_outside_step_sim_error(&other_err));
}

#[test]
fn test_should_recover_nonlinear_failure_near_active_stop() {
    let msg = "ODE solver error: Exceeded maximum number of nonlinear solver failures (121)";
    assert!(should_recover_nonlinear_failure_near_active_stop(
        msg,
        0.0150000004,
        0.015
    ));
    assert!(!should_recover_nonlinear_failure_near_active_stop(
        msg, 0.03, 0.015
    ));
}

#[test]
fn test_overwrite_solver_state_recomputes_dy_from_updated_event_state() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // z = if time < 0.5 then 1 else 2
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("z"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(2.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "alg_z".to_string(),
        scalar_count: 1,
    });

    let budget = TimeoutBudget::new(None);
    let mass = rumoca_sim_core::compute_mass_matrix(&dae, 1, &[], &budget).expect("mass matrix");
    let mut problem =
        problem::build_problem(&dae, 1e-6, 1e-6, 1e-6, &mass).expect("build diffsol problem");
    configure_solver_problem_with_profile(
        &mut problem,
        &SimOptions::default(),
        SolverStartupProfile::Default,
    );
    let mut solver = problem.bdf::<LS>().expect("build BDF solver");

    // Seed a bogus derivative to ensure overwrite recomputes it from the DAE.
    {
        let state = solver.state_mut();
        state.dy.as_mut_slice().fill(123.0);
    }

    overwrite_solver_state::<_, _>(
        &mut solver,
        SolverStateOverwriteInput {
            opts: &SimOptions::default(),
            startup_profile: SolverStartupProfile::Default,
            dae: &dae,
            param_values: &[],
            n_x: 1,
            t: 0.75,
            y: &[0.0, 2.0],
        },
    )
    .expect("overwrite state should succeed");

    let state = solver.state();
    assert!((state.t - 0.75).abs() < 1e-12);
    assert!((state.y.as_slice()[0] - 0.0).abs() < 1e-12);
    assert!((state.y.as_slice()[1] - 2.0).abs() < 1e-12);
    assert!(
        (state.dy.as_slice()[0] - 2.0).abs() < 1e-9,
        "derivative should be recomputed at event state, got dy={:?}",
        state.dy.as_slice()
    );
    assert!(state.dy.as_slice()[1].abs() < 1e-9);
}

#[test]
fn test_relaxed_ic_hint_has_disjoint_drop_row_detects_disjoint_pairing() {
    let mut dae = Dae::new();
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("y")),
        span: Span::DUMMY,
        origin: "eq0".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("i"), var_ref("j")),
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });
    let hint = rumoca_phase_structural::IcRelaxationHint {
        dropped_eq_global: vec![1],
        dropped_unknown_names: vec!["x".to_string()],
    };
    assert!(relaxed_ic_hint_has_disjoint_drop_row(&dae, &hint));
}

#[test]
fn test_relaxed_ic_hint_has_disjoint_drop_row_accepts_touching_pairing() {
    let mut dae = Dae::new();
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("y")),
        span: Span::DUMMY,
        origin: "eq0".to_string(),
        scalar_count: 1,
    });
    let hint = rumoca_phase_structural::IcRelaxationHint {
        dropped_eq_global: vec![0],
        dropped_unknown_names: vec!["x".to_string()],
    };
    assert!(!relaxed_ic_hint_has_disjoint_drop_row(&dae, &hint));
}

#[test]
fn test_timeout_solver_caps_apply_for_finite_budget() {
    let caps_10 = timeout_solver_caps(Some(10.0), SolverStartupProfile::Default)
        .expect("finite timeout should enable solver caps");
    assert_eq!(caps_10.max_nonlinear_iters, 20);
    assert_eq!(caps_10.max_nonlinear_failures, 120);
    assert_eq!(caps_10.max_error_failures, 80);
    assert_eq!(caps_10.min_timestep, 1e-14);

    let caps_long = timeout_solver_caps(Some(60.0), SolverStartupProfile::RobustTinyStep)
        .expect("finite timeout should enable solver caps even for long budgets");
    assert_eq!(caps_long.max_nonlinear_iters, 40);
    assert_eq!(caps_long.max_nonlinear_failures, 4000);
    assert_eq!(caps_long.max_error_failures, 2000);
    assert_eq!(caps_long.min_timestep, 1e-16);

    let caps_10_robust = timeout_solver_caps(Some(10.0), SolverStartupProfile::RobustTinyStep)
        .expect("finite timeout should enable robust solver caps");
    assert_eq!(caps_10_robust.max_nonlinear_iters, 40);
    assert_eq!(caps_10_robust.max_nonlinear_failures, 800);
    assert_eq!(caps_10_robust.max_error_failures, 400);
    assert_eq!(caps_10_robust.min_timestep, 1e-16);
}

#[test]
fn test_timeout_solver_caps_disabled_without_budget() {
    assert!(timeout_solver_caps(None, SolverStartupProfile::Default).is_none());
    assert!(timeout_solver_caps(Some(0.0), SolverStartupProfile::Default).is_none());
}

#[test]
fn test_scalarize_scalar_equations_unchanged() {
    let mut dae = Dae::new();
    let eq = dae::Equation {
        lhs: Some(VarName::new("x")),
        rhs: Expression::Literal(Literal::Real(1.0)),
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    };
    dae.f_x.push(eq);
    scalarize_equations(&mut dae);
    assert_eq!(dae.f_x.len(), 1);
    assert_eq!(dae.f_x[0].scalar_count, 1);
}

#[test]
fn test_scalarize_array_equation_expands() {
    let mut dae = Dae::new();

    // Register array state variable x[3]
    let mut var = Variable::new(VarName::new("x"));
    var.dims = vec![3];
    dae.states.insert(VarName::new("x"), var);

    // Array equation: der(x) with scalar_count=3
    let eq = dae::Equation {
        lhs: Some(VarName::new("x")),
        rhs: Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: VarName::new("x"),
                subscripts: vec![],
            }],
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 3,
    };
    dae.f_x.push(eq);

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 3);
    for (idx, eq) in dae.f_x.iter().enumerate() {
        assert_eq!(eq.scalar_count, 1);
        let expected_lhs = format!("x[{}]", idx + 1);
        assert_eq!(eq.lhs.as_ref().unwrap().as_str(), expected_lhs);
    }

    // Check that der(x) became der(x[1]) in the first equation
    if let Expression::BuiltinCall { function, args } = &dae.f_x[0].rhs {
        assert_eq!(*function, BuiltinFunction::Der);
        if let Expression::VarRef { name, subscripts } = &args[0] {
            assert_eq!(name.as_str(), "x");
            assert_eq!(subscripts.len(), 1);
            assert!(matches!(subscripts[0], dae::Subscript::Index(1)));
        } else {
            panic!("expected VarRef inside der()");
        }
    } else {
        panic!("expected BuiltinCall(Der)");
    }
}

#[test]
fn test_scalarize_array_elements_extracted() {
    let mut dae = Dae::new();

    // Array equation with Array expression RHS, scalar_count=2
    let eq = dae::Equation {
        lhs: Some(VarName::new("z")),
        rhs: Expression::Array {
            elements: vec![
                Expression::Literal(Literal::Real(10.0)),
                Expression::Literal(Literal::Real(20.0)),
            ],
            is_matrix: false,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 2,
    };
    dae.f_x.push(eq);

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 2);
    assert!(matches!(&dae.f_x[0].rhs, Expression::Literal(Literal::Real(v)) if *v == 10.0));
    assert!(matches!(&dae.f_x[1].rhs, Expression::Literal(Literal::Real(v)) if *v == 20.0));
}

#[test]
fn test_scalarize_varref_gets_subscript() {
    let mut dae = Dae::new();
    let mut var = Variable::new(VarName::new("y"));
    var.dims = vec![2];
    dae.algebraics.insert(VarName::new("y"), var);

    let var_dims = build_var_dims_map(&dae);
    let complex_fields = build_complex_field_map(&dae);
    let component_index_map = build_component_index_projection_map(&dae);
    let function_output_index_map = HashMap::new();
    let expr = Expression::VarRef {
        name: VarName::new("y"),
        subscripts: vec![],
    };
    let result = index_into_expr(
        &expr,
        2,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    if let Expression::VarRef { name, subscripts } = &result {
        assert_eq!(name.as_str(), "y");
        assert_eq!(subscripts.len(), 1);
        assert!(matches!(subscripts[0], dae::Subscript::Index(2)));
    } else {
        panic!("expected VarRef with subscript");
    }
}

#[test]
fn test_scalarize_varref_singleton_array_does_not_inject_out_of_range_subscript() {
    let mut dae = Dae::new();
    let mut var = Variable::new(VarName::new("bw.im"));
    var.dims = vec![1];
    dae.algebraics.insert(VarName::new("bw.im"), var);

    let var_dims = build_var_dims_map(&dae);
    let complex_fields = build_complex_field_map(&dae);
    let component_index_map = build_component_index_projection_map(&dae);
    let function_output_index_map = HashMap::new();
    let expr = Expression::VarRef {
        name: VarName::new("bw.im"),
        subscripts: vec![],
    };
    let result = index_into_expr(
        &expr,
        2,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    assert!(matches!(
        result,
        Expression::VarRef { name, subscripts }
            if name.as_str() == "bw.im" && subscripts.is_empty()
    ));
}

#[test]
fn test_scalarize_complex_constructor_field_projection_keeps_singleton_sum_argument_unindexed() {
    let mut dae = Dae::new();
    let mut bw_re = Variable::new(VarName::new("bw.re"));
    bw_re.dims = vec![1];
    dae.algebraics.insert(VarName::new("bw.re"), bw_re);
    let mut bw_im = Variable::new(VarName::new("bw.im"));
    bw_im.dims = vec![1];
    dae.algebraics.insert(VarName::new("bw.im"), bw_im);
    dae.algebraics.insert(
        VarName::new("bSum.re"),
        Variable::new(VarName::new("bSum.re")),
    );
    dae.algebraics.insert(
        VarName::new("bSum.im"),
        Variable::new(VarName::new("bSum.im")),
    );

    let sum_re = Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![var_ref("bw.re")],
    };
    let sum_im = Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![var_ref("bw.im")],
    };
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("bSum"),
            Expression::FunctionCall {
                name: VarName::new("Complex"),
                args: vec![sum_re, sum_im],
                is_constructor: true,
            },
        ),
        span: Span::DUMMY,
        origin: "complex_sum_projection".to_string(),
        scalar_count: 2,
    });

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 2);

    let im_eq = dae
        .f_x
        .iter()
        .find(|eq| {
            matches!(
                &eq.rhs,
                Expression::Binary {
                    op: OpBinary::Sub(_),
                    lhs,
                    ..
                } if matches!(
                    lhs.as_ref(),
                    Expression::VarRef { name, subscripts }
                        if name.as_str() == "bSum.im" && subscripts.is_empty()
                )
            )
        })
        .expect("expected scalarized bSum.im equation");

    let Expression::Binary { rhs, .. } = &im_eq.rhs else {
        panic!("expected binary residual");
    };
    assert!(
        matches!(
            rhs.as_ref(),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sum,
                args
            } if matches!(
                args.as_slice(),
                [Expression::VarRef { name, subscripts }]
                    if name.as_str() == "bw.im" && subscripts.is_empty()
            )
        ),
        "expected imag branch to sum bw.im without out-of-range indexing, got {:?}",
        rhs
    );
}

#[test]
fn test_scalarize_singleton_record_array_projects_im_component_to_scalar_rhs() {
    let mut dae = Dae::new();

    let mut bw_re = Variable::new(VarName::new("bw.re"));
    bw_re.dims = vec![1];
    dae.algebraics.insert(VarName::new("bw.re"), bw_re);
    let mut bw_im = Variable::new(VarName::new("bw.im"));
    bw_im.dims = vec![1];
    dae.algebraics.insert(VarName::new("bw.im"), bw_im);

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("bw"),
            Expression::Array {
                elements: vec![Expression::FunctionCall {
                    name: VarName::new("Complex"),
                    args: vec![real(1.0), real(2.0)],
                    is_constructor: true,
                }],
                is_matrix: false,
            },
        ),
        span: Span::DUMMY,
        origin: "singleton_record_array".to_string(),
        scalar_count: 2,
    });

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 2);
    let im_eq = dae
        .f_x
        .iter()
        .find(|eq| {
            matches!(
                &eq.rhs,
                Expression::Binary {
                    op: OpBinary::Sub(_),
                    lhs,
                    ..
                } if matches!(
                    lhs.as_ref(),
                    Expression::VarRef { name, subscripts }
                        if name.as_str() == "bw.im" && subscripts.is_empty()
                )
            )
        })
        .expect("expected scalarized bw.im equation");

    let Expression::Binary { rhs, .. } = &im_eq.rhs else {
        panic!("expected binary residual");
    };
    assert!(
        matches!(rhs.as_ref(), Expression::Literal(Literal::Real(v)) if (*v - 2.0).abs() < 1e-12),
        "expected singleton-array imag projection to be scalar literal 2.0, got {:?}",
        rhs
    );
}

#[test]
fn test_scalarize_constructor_function_projects_component_arguments() {
    let mut dae = Dae::new();
    let mut var = Variable::new(VarName::new("z"));
    var.dims = vec![2];
    dae.algebraics.insert(VarName::new("z"), var);

    dae.f_x.push(dae::Equation {
        lhs: Some(VarName::new("z")),
        rhs: Expression::FunctionCall {
            name: VarName::new("Complex"),
            args: vec![real(10.0), real(20.0)],
            is_constructor: true,
        },
        span: Span::DUMMY,
        origin: "constructor_eq".to_string(),
        scalar_count: 2,
    });

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 2);
    assert!(matches!(
        &dae.f_x[0].rhs,
        Expression::Literal(Literal::Real(v)) if *v == 10.0
    ));
    assert!(matches!(
        &dae.f_x[1].rhs,
        Expression::Literal(Literal::Real(v)) if *v == 20.0
    ));
}

#[test]
fn test_scalarize_complex_array_assignment_projects_all_scalar_targets() {
    let mut dae = Dae::new();

    let mut z_re = Variable::new(VarName::new("z.re"));
    z_re.dims = vec![3];
    dae.algebraics.insert(VarName::new("z.re"), z_re);

    let mut z_im = Variable::new(VarName::new("z.im"));
    z_im.dims = vec![3];
    dae.algebraics.insert(VarName::new("z.im"), z_im);

    let complex = |re: f64, im: f64| Expression::FunctionCall {
        name: VarName::new("Complex"),
        args: vec![real(re), real(im)],
        is_constructor: true,
    };
    dae.f_x.push(dae::Equation {
        lhs: Some(VarName::new("z")),
        rhs: Expression::Array {
            elements: vec![complex(1.0, 10.0), complex(2.0, 20.0), complex(3.0, 30.0)],
            is_matrix: false,
        },
        span: Span::DUMMY,
        origin: "complex_array".to_string(),
        scalar_count: 6,
    });

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 6);
    let expected_lhs = [
        "z.re[1]", "z.re[2]", "z.re[3]", "z.im[1]", "z.im[2]", "z.im[3]",
    ];
    let expected_rhs = [1.0, 2.0, 3.0, 10.0, 20.0, 30.0];

    for (idx, eq) in dae.f_x.iter().enumerate() {
        assert_eq!(
            eq.lhs
                .as_ref()
                .expect("scalarized equation should keep explicit LHS")
                .as_str(),
            expected_lhs[idx]
        );
        assert!(
            matches!(eq.rhs, Expression::Literal(Literal::Real(v)) if (v - expected_rhs[idx]).abs() < 1e-12),
            "unexpected rhs at idx {idx}: {:?}",
            eq.rhs
        );
    }
}

#[test]
fn test_scalarize_complex_array_residual_projects_all_scalar_targets() {
    let mut dae = Dae::new();

    let mut z_re = Variable::new(VarName::new("z.re"));
    z_re.dims = vec![3];
    dae.algebraics.insert(VarName::new("z.re"), z_re);

    let mut z_im = Variable::new(VarName::new("z.im"));
    z_im.dims = vec![3];
    dae.algebraics.insert(VarName::new("z.im"), z_im);

    let complex = |re: f64, im: f64| Expression::FunctionCall {
        name: VarName::new("Complex"),
        args: vec![real(re), real(im)],
        is_constructor: true,
    };
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("z"),
            Expression::Array {
                elements: vec![complex(1.0, 10.0), complex(2.0, 20.0), complex(3.0, 30.0)],
                is_matrix: false,
            },
        ),
        span: Span::DUMMY,
        origin: "complex_array_residual".to_string(),
        scalar_count: 6,
    });

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 6);
    let expected_lhs_base = ["z.re", "z.re", "z.re", "z.im", "z.im", "z.im"];
    let expected_lhs_index = [1_i64, 2, 3, 1, 2, 3];
    let expected_rhs = [1.0, 2.0, 3.0, 10.0, 20.0, 30.0];

    for (idx, eq) in dae.f_x.iter().enumerate() {
        assert!(
            eq.lhs.is_none(),
            "residual equations should keep lhs=None after scalarization"
        );
        let Expression::Binary {
            op: OpBinary::Sub(_),
            lhs,
            rhs,
        } = &eq.rhs
        else {
            panic!("expected scalarized residual equation, got {:?}", eq.rhs);
        };

        assert!(
            matches!(
                lhs.as_ref(),
                Expression::VarRef { name, subscripts }
                    if name.as_str() == expected_lhs_base[idx]
                        && matches!(subscripts.as_slice(), [dae::Subscript::Index(i)] if *i == expected_lhs_index[idx])
            ),
            "unexpected scalarized lhs at idx {idx}: {:?}",
            lhs
        );
        assert!(
            matches!(rhs.as_ref(), Expression::Literal(Literal::Real(v)) if (v - expected_rhs[idx]).abs() < 1e-12),
            "unexpected scalarized rhs at idx {idx}: {:?}",
            rhs
        );
    }
}

#[test]
fn test_scalarize_function_call_projects_multi_output_components() {
    let mut dae = Dae::new();
    let mut func = Function::new("F.random", Span::DUMMY);
    func.outputs.push(FunctionParam {
        name: "x".to_string(),
        type_name: "Real".to_string(),
        dims: vec![],
        default: None,
        description: None,
    });
    func.outputs.push(FunctionParam {
        name: "seedOut".to_string(),
        type_name: "Integer".to_string(),
        dims: vec![3],
        default: None,
        description: None,
    });
    dae.functions.insert(VarName::new("F.random"), func);

    let var_dims = build_var_dims_map(&dae);
    let complex_fields = build_complex_field_map(&dae);
    let component_index_map = build_component_index_projection_map(&dae);
    let function_output_index_map = build_function_output_projection_map(&dae);
    let expr = Expression::FunctionCall {
        name: VarName::new("F.random"),
        args: vec![var_ref("seedIn")],
        is_constructor: false,
    };

    let projected1 = index_into_expr(
        &expr,
        1,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    let projected3 = index_into_expr(
        &expr,
        3,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );

    assert!(matches!(
        projected1,
        Expression::FunctionCall { name, .. } if name.as_str() == "F.random.x"
    ));
    assert!(matches!(
        projected3,
        Expression::FunctionCall { name, .. } if name.as_str() == "F.random.seedOut[2]"
    ));
}

#[test]
fn test_scalarize_function_call_projects_complex_output_components() {
    let mut dae = Dae::new();
    let mut func = Function::new("F.powerOfJ", Span::DUMMY);
    func.outputs.push(FunctionParam {
        name: "x".to_string(),
        type_name: "Modelica.ComplexMath.Complex".to_string(),
        dims: vec![],
        default: None,
        description: None,
    });
    dae.functions.insert(VarName::new("F.powerOfJ"), func);

    let var_dims = build_var_dims_map(&dae);
    let complex_fields = build_complex_field_map(&dae);
    let component_index_map = build_component_index_projection_map(&dae);
    let function_output_index_map = build_function_output_projection_map(&dae);
    let expr = Expression::FunctionCall {
        name: VarName::new("F.powerOfJ"),
        args: vec![var_ref("k")],
        is_constructor: false,
    };

    let projected_re = index_into_expr(
        &expr,
        1,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    let projected_im = index_into_expr(
        &expr,
        2,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );

    assert!(matches!(
        projected_re,
        Expression::FunctionCall { name, .. } if name.as_str() == "F.powerOfJ.x.re"
    ));
    assert!(matches!(
        projected_im,
        Expression::FunctionCall { name, .. } if name.as_str() == "F.powerOfJ.x.im"
    ));
}

#[test]
fn test_scalarize_varref_projects_complex_record_fields() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("z.re"), Variable::new(VarName::new("z.re")));
    dae.algebraics
        .insert(VarName::new("z.im"), Variable::new(VarName::new("z.im")));

    let var_dims = build_var_dims_map(&dae);
    let complex_fields = build_complex_field_map(&dae);
    let component_index_map = build_component_index_projection_map(&dae);
    let function_output_index_map = HashMap::new();
    let expr = Expression::VarRef {
        name: VarName::new("z"),
        subscripts: vec![],
    };

    let re = index_into_expr(
        &expr,
        1,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    let im = index_into_expr(
        &expr,
        2,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );

    assert!(matches!(
        re,
        Expression::VarRef { name, subscripts } if name.as_str() == "z.re" && subscripts.is_empty()
    ));
    assert!(matches!(
        im,
        Expression::VarRef { name, subscripts } if name.as_str() == "z.im" && subscripts.is_empty()
    ));
}

#[test]
fn test_scalarize_literal_broadcasts() {
    let var_dims = HashMap::new();
    let complex_fields = HashMap::new();
    let component_index_map = HashMap::new();
    let function_output_index_map = HashMap::new();
    let expr = Expression::Literal(Literal::Real(42.0));
    let result = index_into_expr(
        &expr,
        1,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    assert!(matches!(result, Expression::Literal(Literal::Real(v)) if v == 42.0));
}

#[test]
fn test_scalarize_varref_projects_component_array_field() {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("plug.pin[1].v"),
        Variable::new(VarName::new("plug.pin[1].v")),
    );
    dae.algebraics.insert(
        VarName::new("plug.pin[2].v"),
        Variable::new(VarName::new("plug.pin[2].v")),
    );
    dae.algebraics.insert(
        VarName::new("plug.pin[3].v"),
        Variable::new(VarName::new("plug.pin[3].v")),
    );

    let var_dims = build_var_dims_map(&dae);
    let complex_fields = build_complex_field_map(&dae);
    let component_index_map = build_component_index_projection_map(&dae);
    let function_output_index_map = HashMap::new();
    let expr = Expression::VarRef {
        name: VarName::new("plug.pin.v"),
        subscripts: vec![],
    };

    let projected = index_into_expr(
        &expr,
        2,
        &var_dims,
        &complex_fields,
        &component_index_map,
        &function_output_index_map,
    );
    assert!(matches!(
        projected,
        Expression::VarRef { name, subscripts }
            if name.as_str() == "plug.pin[2].v" && subscripts.is_empty()
    ));
}

#[test]
fn test_scalarize_mixed_scalar_and_array() {
    let mut dae = Dae::new();

    // Array state x[2]
    let mut var_x = Variable::new(VarName::new("x"));
    var_x.dims = vec![2];
    dae.states.insert(VarName::new("x"), var_x);

    // Scalar algebraic z
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // Array equation (scalar_count=2)
    dae.f_x.push(dae::Equation {
        lhs: Some(VarName::new("x")),
        rhs: Expression::VarRef {
            name: VarName::new("x"),
            subscripts: vec![],
        },
        span: Span::DUMMY,
        origin: "array_eq".to_string(),
        scalar_count: 2,
    });

    // Scalar equation (scalar_count=1)
    dae.f_x.push(dae::Equation {
        lhs: Some(VarName::new("z")),
        rhs: Expression::Literal(Literal::Real(0.0)),
        span: Span::DUMMY,
        origin: "scalar_eq".to_string(),
        scalar_count: 1,
    });

    scalarize_equations(&mut dae);

    assert_eq!(dae.f_x.len(), 3); // 2 expanded + 1 scalar
    assert_eq!(dae.f_x[0].lhs.as_ref().unwrap().as_str(), "x[1]");
    assert_eq!(dae.f_x[1].lhs.as_ref().unwrap().as_str(), "x[2]");
    assert_eq!(dae.f_x[2].lhs.as_ref().unwrap().as_str(), "z");
}

#[test]
fn test_simulate_empty_dae() {
    let dae = Dae::new();
    let result = simulate(&dae, &SimOptions::default());
    assert!(matches!(result, Err(SimError::EmptySystem)));
}

#[test]
fn test_simulate_no_state_time_dependent_output_evolves_over_time() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // 0 = y - (if time < 0.5 then 1 else 2)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(2.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "alg_time_switch".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.1),
            max_wall_seconds: Some(5.0),
            ..SimOptions::default()
        },
    )
    .expect("no-state time-dependent simulation should succeed");

    assert_eq!(
        result.n_states, 0,
        "dummy state must be hidden from outputs"
    );
    assert!(
        !result
            .names
            .iter()
            .any(|name| name == "_rumoca_dummy_state"),
        "dummy state should not leak into result names"
    );
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result should include y output");
    let y = &result.data[y_idx];
    assert!(
        !y.is_empty(),
        "expected output samples for y over the requested time horizon"
    );
    let first = *y.first().expect("first sample");
    let last = *y.last().expect("last sample");
    assert!(
        (first - 1.0).abs() < 1.0e-6,
        "y(0) should match the first branch value, got {first}"
    );
    assert!(
        (last - 2.0).abs() < 1.0e-6,
        "y(t_end) should switch to the second branch value, got {last}"
    );
}

#[test]
fn test_simulate_no_state_skips_ic_newton_for_singular_algebraic_seed() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // Add algebraics that are structurally singular (`0 = a - a`). These are
    // harmless for sampled algebraic projection but can stall IC Newton if run
    // up-front on no-state systems.
    for idx in 0..40 {
        let name = VarName::new(format!("a{idx}"));
        let mut var = Variable::new(name.clone());
        var.start = Some(real(1.0));
        dae.algebraics.insert(name.clone(), var);
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(var_ref(name.as_str()), var_ref(name.as_str())),
            span: Span::DUMMY,
            origin: "singular_alg_identity".to_string(),
            scalar_count: 1,
        });
    }

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), real(1.0)),
        span: Span::DUMMY,
        origin: "output_assignment".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            max_wall_seconds: Some(0.2),
            ..SimOptions::default()
        },
    )
    .expect("no-state simulation should bypass IC Newton and complete");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result should include y");
    assert!(
        result.data[y_idx].iter().all(|v| (v - 1.0).abs() < 1.0e-6),
        "y should stay at assigned value"
    );
}

#[test]
fn test_simulate_timeout_enforced_during_initialization() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // 0 = z - der(x)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(Expression::VarRef {
                name: VarName::new("z"),
                subscripts: vec![],
            }),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x"),
                    subscripts: vec![],
                }],
            }),
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });
    // 0 = z - 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(Expression::VarRef {
                name: VarName::new("z"),
                subscripts: vec![],
            }),
            rhs: Box::new(Expression::Literal(Literal::Real(1.0))),
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let opts = SimOptions {
        max_wall_seconds: Some(f64::MIN_POSITIVE),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts);
    assert!(
        matches!(result, Err(SimError::Timeout { .. })),
        "expected timeout, got {result:?}"
    );
}

#[test]
fn test_simulate_fails_fast_for_unsupported_external_function_call() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("f"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut external_stub = rumoca_ir_dae::Function::new("f", Span::DUMMY);
    external_stub.external = Some(ExternalFunction {
        language: "C".to_string(),
        function_name: Some("f".to_string()),
        output_name: None,
        arg_names: vec!["x".to_string()],
    });
    dae.functions.insert(VarName::new("f"), external_stub);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(result, Err(SimError::UnsupportedFunction { .. })),
        "expected unsupported function error, got {result:?}"
    );
}

#[test]
fn test_simulate_rejects_member_style_function_call_without_exact_definition() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("world.gravityAcceleration"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut fn_def = rumoca_ir_dae::Function::new(
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration",
        Span::DUMMY,
    );
    fn_def.body.push(rumoca_ir_dae::Statement::Return);
    dae.functions.insert(fn_def.name.clone(), fn_def);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(
            result,
            Err(SimError::UnsupportedFunction { ref name, .. })
                if name == "world.gravityAcceleration"
        ),
        "expected unresolved member-style function to fail fast, got {result:?}"
    );
}

#[test]
fn test_simulate_rejects_constructor_field_projection_without_definition() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    let constructor_field = Expression::FieldAccess {
        base: Box::new(Expression::FunctionCall {
            name: VarName::new("My.Record"),
            args: vec![real(2.0), real(3.0)],
            is_constructor: true,
        }),
        field: "C".to_string(),
    };

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            constructor_field,
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(
            result,
            Err(SimError::UnsupportedFunction { ref name, .. }) if name == "My.Record.C"
        ),
        "expected unresolved constructor field projection to fail fast, got {result:?}"
    );
}

#[test]
fn test_simulate_allows_constructor_field_projection_with_signature() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    let constructor_field = Expression::FieldAccess {
        base: Box::new(Expression::FunctionCall {
            name: VarName::new("My.Record"),
            args: vec![real(2.0), real(3.0)],
            is_constructor: true,
        }),
        field: "C".to_string(),
    };

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            constructor_field,
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut record_ctor = rumoca_ir_dae::Function::new("My.Record", Span::DUMMY);
    record_ctor.inputs.push(rumoca_ir_dae::FunctionParam {
        name: "R".to_string(),
        type_name: "Real".to_string(),
        dims: Vec::new(),
        default: None,
        description: None,
    });
    record_ctor.inputs.push(rumoca_ir_dae::FunctionParam {
        name: "C".to_string(),
        type_name: "Real".to_string(),
        dims: Vec::new(),
        default: None,
        description: None,
    });
    dae.functions.insert(record_ctor.name.clone(), record_ctor);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        result.is_ok(),
        "constructor field projection with constructor signature should be simulatable, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_assert_statement_message_helpers() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Modelica.Utilities.Strings.length"),
                args: vec![Expression::Literal(Literal::String("hello".to_string()))],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: "string-helper".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "assert message helpers should not block simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_assert_function_call_message_helpers() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Modelica.Utilities.Strings.length"),
                args: vec![Expression::Literal(Literal::String("hello".to_string()))],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: "string-helper-2".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "assert(...) call should be accepted in simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_time_table_next_event_function() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("table_id"),
        Variable::new(VarName::new("table_id")),
    );
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::FunctionCall {
            name: VarName::new("Modelica.Blocks.Tables.Internal.getNextTimeEvent"),
            args: vec![var_ref("table_id"), real(0.0)],
            is_constructor: false,
        },
        span: Span::DUMMY,
        origin: "next-time-event".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "table next-time-event helper should be accepted in simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_function_parameter_call_aliases() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("wrapper"),
                args: vec![
                    Expression::FunctionCall {
                        name: VarName::new("fun_impl"),
                        args: vec![real(2.0)],
                        is_constructor: false,
                    },
                    var_ref("x"),
                ],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut fun_impl = rumoca_ir_dae::Function::new("fun_impl", Span::DUMMY);
    fun_impl
        .inputs
        .push(rumoca_ir_dae::FunctionParam::new("u", "Real"));
    fun_impl
        .inputs
        .push(rumoca_ir_dae::FunctionParam::new("a", "Real"));
    fun_impl
        .outputs
        .push(
            rumoca_ir_dae::FunctionParam::new("y", "Real").with_default(Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(var_ref("u")),
                rhs: Box::new(var_ref("a")),
            }),
        );
    fun_impl.body.push(Statement::Empty);
    dae.functions.insert(fun_impl.name.clone(), fun_impl);

    let mut wrapper = rumoca_ir_dae::Function::new("wrapper", Span::DUMMY);
    wrapper.inputs.push(rumoca_ir_dae::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    wrapper
        .inputs
        .push(rumoca_ir_dae::FunctionParam::new("x", "Real"));
    wrapper
        .outputs
        .push(rumoca_ir_dae::FunctionParam::new("y", "Real").with_default(
            Expression::FunctionCall {
                name: VarName::new("wrapper.f"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
        ));
    wrapper.body.push(Statement::Empty);
    dae.functions.insert(wrapper.name.clone(), wrapper);

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "function-typed input aliases (wrapper.f) should be accepted in simulation preflight, got {result:?}"
    );
}

#[test]
fn test_simulate_rejects_reachable_nested_unsupported_external_function() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("wrapper"),
                args: vec![],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut bad = rumoca_ir_dae::Function::new("bad_external", Span::DUMMY);
    bad.external = Some(ExternalFunction {
        language: "C".to_string(),
        function_name: Some("bad_external".to_string()),
        output_name: None,
        arg_names: vec![],
    });
    dae.functions.insert(VarName::new("bad_external"), bad);

    let mut wrapper = rumoca_ir_dae::Function::new("wrapper", Span::DUMMY);
    wrapper.body.push(rumoca_ir_dae::Statement::FunctionCall {
        comp: comp_ref("bad_external"),
        args: vec![],
        outputs: vec![],
    });
    dae.functions.insert(VarName::new("wrapper"), wrapper);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(
            result,
            Err(SimError::UnsupportedFunction { ref name, .. }) if name == "bad_external"
        ),
        "expected reachable nested external function to fail preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_ignores_unreachable_nested_unsupported_function() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(0.0),
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut bad = rumoca_ir_dae::Function::new("bad_external", Span::DUMMY);
    bad.external = Some(ExternalFunction {
        language: "C".to_string(),
        function_name: Some("bad_external".to_string()),
        output_name: None,
        arg_names: vec![],
    });
    dae.functions.insert(VarName::new("bad_external"), bad);

    let mut wrapper = rumoca_ir_dae::Function::new("wrapper", Span::DUMMY);
    wrapper.body.push(rumoca_ir_dae::Statement::FunctionCall {
        comp: comp_ref("bad_external"),
        args: vec![],
        outputs: vec![],
    });
    dae.functions.insert(VarName::new("wrapper"), wrapper);

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "unreachable nested unsupported functions should not fail simulation preflight, got {result:?}"
    );
}

#[test]
fn test_simulate_reports_division_by_zero_at_initialization() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.parameters
        .insert(VarName::new("p"), Variable::new(VarName::new("p")));

    // 0 = der(x) - (1 / p), with p defaulting to 0.0.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::Binary {
                op: OpBinary::Div(Default::default()),
                lhs: Box::new(real(1.0)),
                rhs: Box::new(var_ref("p")),
            },
        ),
        span: Span::DUMMY,
        origin: "ode_with_div_zero".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    match result {
        Err(SimError::SolverError(msg)) => {
            assert!(
                msg.contains("division by zero at initialization"),
                "expected division-by-zero diagnostic, got: {msg}"
            );
            assert!(
                msg.contains("divisor expression is: p"),
                "expected divisor expression in diagnostic, got: {msg}"
            );
            assert!(
                msg.contains("origin='ode_with_div_zero'"),
                "expected equation origin in diagnostic, got: {msg}"
            );
        }
        other => panic!("expected solver error, got: {other:?}"),
    }
}

mod clocked_sampling;
mod core;
mod direct_assignment_demotion;
mod scalarization_regressions;
mod solver_regressions;
mod state_derivative;
