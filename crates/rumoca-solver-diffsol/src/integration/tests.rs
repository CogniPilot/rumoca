use super::*;
use crate::test_support::{binop, eq_from, lit, var};
use rumoca_ir_dae as ir_dae;

fn time_lt_expr(rhs: f64) -> ir_dae::Expression {
    ir_dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Lt(Default::default()),
        lhs: Box::new(ir_dae::Expression::VarRef {
            name: ir_dae::VarName::new("time"),
            subscripts: Vec::new(),
        }),
        rhs: Box::new(ir_dae::Expression::Literal(ir_dae::Literal::Real(rhs))),
    }
}

#[test]
fn runtime_event_uses_frozen_pre_values_for_synthetic_time_root() {
    let mut dae = Dae::new();
    dae.synthetic_root_conditions.push(time_lt_expr(1.0e-6));

    assert!(runtime_event_uses_frozen_pre_values(
        &dae,
        &SimOptions::default(),
        &[],
        &[],
        1.0e-6,
    ));
}

#[test]
fn runtime_event_uses_frozen_pre_values_skips_state_root_relations() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.synthetic_root_conditions
        .push(ir_dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(Default::default()),
            lhs: Box::new(ir_dae::Expression::VarRef {
                name: ir_dae::VarName::new("x"),
                subscripts: Vec::new(),
            }),
            rhs: Box::new(ir_dae::Expression::Literal(ir_dae::Literal::Real(0.0))),
        });

    assert!(!runtime_event_uses_frozen_pre_values(
        &dae,
        &SimOptions::default(),
        &[0.0],
        &[],
        0.0,
    ));
}

fn build_diagnostics_test_dae() -> ir_dae::Dae {
    let mut dae = ir_dae::Dae::new();
    dae.states.insert(
        ir_dae::VarName::new("x1"),
        ir_dae::Variable::new(ir_dae::VarName::new("x1")),
    );
    dae.states.insert(
        ir_dae::VarName::new("x2"),
        ir_dae::Variable::new(ir_dae::VarName::new("x2")),
    );
    dae.algebraics.insert(
        ir_dae::VarName::new("z1"),
        ir_dae::Variable::new(ir_dae::VarName::new("z1")),
    );
    dae.algebraics.insert(
        ir_dae::VarName::new("z2"),
        ir_dae::Variable::new(ir_dae::VarName::new("z2")),
    );

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        ir_dae::Expression::BuiltinCall {
            function: ir_dae::BuiltinFunction::Der,
            args: vec![var("x1")],
        },
        var("z1"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        ir_dae::Expression::BuiltinCall {
            function: ir_dae::BuiltinFunction::Der,
            args: vec![var("x2")],
        },
        var("z2"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z1"),
        binop(
            rumoca_ir_core::OpBinary::Mul(Default::default()),
            var("x1"),
            var("x1"),
        ),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z2"),
        ir_dae::Expression::BuiltinCall {
            function: ir_dae::BuiltinFunction::Sin,
            args: vec![var("x2")],
        },
    )));

    dae
}

fn write_preview_column(preview: &mut [Vec<f64>], values: &[f64], col: usize) {
    for (row, preview_row) in preview.iter_mut().enumerate() {
        preview_row[col] = values[row];
    }
}

fn expected_jacobian_failure_summary_from_ad(
    dae: &ir_dae::Dae,
    y: &[f64],
    p: &[f64],
    n_x: usize,
) -> JacobianFailureSummary {
    let n_total = y.len();
    let preview_n = n_total.min(8);
    let mut row_norms = vec![0.0_f64; n_total];
    let mut col_norms = vec![0.0_f64; n_total];
    let mut jac_preview = vec![vec![0.0_f64; preview_n]; preview_n];
    let mut v = vec![0.0_f64; n_total];
    let mut jv = vec![0.0_f64; n_total];

    for col in 0..n_total {
        v[col] = 1.0;
        crate::problem::eval_jacobian_vector_ad(dae, y, p, 0.0, &v, &mut jv, n_x);
        v[col] = 0.0;
        col_norms[col] = jv.iter().fold(0.0_f64, |acc, value| acc.max(value.abs()));
        if col < preview_n {
            write_preview_column(&mut jac_preview, &jv, col);
        }
        for (row, value) in jv.iter().copied().enumerate() {
            row_norms[row] = row_norms[row].max(value.abs());
        }
    }

    JacobianFailureSummary {
        row_norms,
        col_norms,
        jac_preview,
    }
}

#[test]
fn collect_residual_diagnostics_uses_compiled_runtime_residuals() {
    let dae = build_diagnostics_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = crate::problem::default_params(&dae);
    let compiled = crate::problem::build_compiled_runtime_newton_context(&dae, y.len())
        .expect("compile runtime Newton context");
    let residuals = collect_residual_diagnostics(&dae, &compiled, &y, &p, 0.0);
    let mut expected = vec![0.0; y.len()];
    crate::problem::eval_rhs_equations(&dae, &y, &p, 0.0, &mut expected, 2);
    let mut expected_sorted: Vec<(usize, f64)> = expected.into_iter().enumerate().collect();
    expected_sorted.sort_by(|lhs, rhs| {
        rhs.1
            .abs()
            .partial_cmp(&lhs.1.abs())
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    assert_eq!(residuals.len(), expected_sorted.len());
    for (diagnostic, (eq_idx, expected_value)) in residuals.iter().zip(expected_sorted.iter()) {
        assert_eq!(diagnostic.eq_idx, *eq_idx);
        assert!((diagnostic.abs - expected_value.abs()).abs() <= 1.0e-12);
        assert!(!diagnostic.non_finite);
    }
}

#[test]
fn collect_jacobian_failure_summary_uses_compiled_runtime_jacobian() {
    let dae = build_diagnostics_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = crate::problem::default_params(&dae);
    let compiled = crate::problem::build_compiled_runtime_newton_context(&dae, y.len())
        .expect("compile runtime Newton context");
    let summary = collect_jacobian_failure_summary(&compiled, &y, &p, 0.0);
    let expected = expected_jacobian_failure_summary_from_ad(&dae, &y, &p, 2);
    assert_eq!(summary.row_norms.len(), expected.row_norms.len());
    assert_eq!(summary.col_norms.len(), expected.col_norms.len());
    for (got, expected) in summary.row_norms.iter().zip(expected.row_norms.iter()) {
        assert!((got - expected).abs() <= 1.0e-12);
    }
    for (got, expected) in summary.col_norms.iter().zip(expected.col_norms.iter()) {
        assert!((got - expected).abs() <= 1.0e-12);
    }
    for (got_row, expected_row) in summary.jac_preview.iter().zip(expected.jac_preview.iter()) {
        for (got, expected) in got_row.iter().zip(expected_row.iter()) {
            assert!((got - expected).abs() <= 1.0e-12);
        }
    }
}

#[test]
fn residuals_need_function_eval_diagnostics_skips_function_free_rows() {
    let dae = build_diagnostics_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = crate::problem::default_params(&dae);
    let compiled = crate::problem::build_compiled_runtime_newton_context(&dae, y.len())
        .expect("compile runtime Newton context");
    let residuals = collect_residual_diagnostics(&dae, &compiled, &y, &p, 0.0);
    assert!(!residuals_need_function_eval_diagnostics(&dae, &residuals));
}

#[test]
fn residuals_need_function_eval_diagnostics_detects_function_calls() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(eq_from(Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(Expression::FunctionCall {
            name: VarName::new("userFn"),
            args: vec![lit(1.0)],
            is_constructor: false,
        }),
    }));
    let residuals = vec![ResidualDiagnostic {
        eq_idx: 0,
        abs: 1.0,
        non_finite: false,
        origin: "function row".to_string(),
        rhs: "FunctionCall(userFn)".to_string(),
    }];
    assert!(residuals_need_function_eval_diagnostics(&dae, &residuals));
}

#[test]
fn trace_function_calls_in_expr_counts_nested_user_functions_without_env_eval() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![Expression::FunctionCall {
            name: VarName::new("userFn"),
            args: vec![lit(1.0)],
            is_constructor: false,
        }],
    };
    let mut remaining = 8usize;
    let found = trace_function_calls_in_expr(&expr, &mut remaining);
    assert_eq!(found, 1);
    assert_eq!(remaining, 7);
}

#[test]
fn next_restart_time_if_synthetic_roots_still_armed_advances_time_root_surface() {
    let mut dae = Dae::new();
    dae.synthetic_root_conditions.push(time_lt_expr(1.0e-6));
    let opts = SimOptions::default();
    let restart_t = event_restart_time(&opts, 0.0);
    let compiled_synthetic_root = crate::problem::build_compiled_synthetic_root_context(&dae, 0)
        .expect("compile synthetic root context");
    let next_restart_t = next_restart_time_if_synthetic_roots_still_armed(
        &compiled_synthetic_root,
        &dae,
        &[],
        &[],
        &opts,
        restart_t,
        opts.atol,
    )
    .expect("restart should advance when synthetic root remains armed");
    let clearance = synthetic_root_restart_clearance(&dae, &opts);
    assert!(
        next_restart_t >= restart_t + clearance,
        "expected restart time to advance by at least the synthetic-root clearance, got restart_t={} next_restart_t={} clearance={}",
        restart_t,
        next_restart_t,
        clearance
    );
}

#[test]
fn synthetic_root_restart_clearance_uses_output_interval_hint() {
    let opts = SimOptions {
        dt: Some(0.01),
        ..SimOptions::default()
    };
    assert_eq!(synthetic_root_restart_clearance(&Dae::new(), &opts), 1.0e-4);
}

#[test]
fn synthetic_root_restart_clearance_uses_clock_interval_hint() {
    let mut dae = Dae::new();
    dae.clock_intervals.insert("sample1.y".to_string(), 0.1);
    assert_eq!(
        synthetic_root_restart_clearance(&dae, &SimOptions::default()),
        1.0e-3
    );
}

#[test]
fn event_restart_step_hint_uses_clock_interval_floor_after_event() {
    let mut dae = Dae::new();
    dae.clock_intervals.insert("sample1.y".to_string(), 0.1);
    let opts = SimOptions {
        t_start: 0.0,
        t_end: 5.0,
        ..SimOptions::default()
    };
    let hint = event_restart_step_hint(&dae, &opts, 1.0, SolverStartupProfile::RobustTinyStep)
        .expect("restart hint");
    assert!((hint - 0.001).abs() <= f64::EPSILON);
}

#[test]
fn event_restart_step_hint_keeps_tiny_startup_step_at_initial_time() {
    let mut dae = Dae::new();
    dae.clock_intervals.insert("sample1.y".to_string(), 0.1);
    let opts = SimOptions {
        t_start: 0.0,
        t_end: 5.0,
        ..SimOptions::default()
    };
    let hint = event_restart_step_hint(&dae, &opts, 0.0, SolverStartupProfile::RobustTinyStep)
        .expect("startup hint");
    assert!((hint - 1.0e-6).abs() <= f64::EPSILON);
}

#[test]
fn next_restart_time_if_synthetic_roots_still_armed_keeps_cleared_root() {
    let mut dae = Dae::new();
    dae.synthetic_root_conditions.push(time_lt_expr(-1.0));
    let opts = SimOptions::default();
    let restart_t = event_restart_time(&opts, 0.0);
    let compiled_synthetic_root = crate::problem::build_compiled_synthetic_root_context(&dae, 0)
        .expect("compile synthetic root context");
    assert!(
        next_restart_time_if_synthetic_roots_still_armed(
            &compiled_synthetic_root,
            &dae,
            &[],
            &[],
            &opts,
            restart_t,
            opts.atol
        )
        .is_none(),
        "restart time should stay put when synthetic roots are already cleared"
    );
}

#[test]
fn stateful_runtime_capture_reconstructs_eliminated_sample_input_before_tick() {
    let mut dae = ir_dae::Dae::new();
    dae.algebraics.insert(
        ir_dae::VarName::new("load.w"),
        ir_dae::Variable::new(ir_dae::VarName::new("load.w")),
    );
    dae.discrete_reals.insert(
        ir_dae::VarName::new("sample1.y"),
        ir_dae::Variable::new(ir_dae::VarName::new("sample1.y")),
    );
    dae.f_m.push(ir_dae::Equation::explicit(
        ir_dae::VarName::new("sample1.y"),
        ir_dae::Expression::BuiltinCall {
            function: ir_dae::BuiltinFunction::Sample,
            args: vec![
                var("sample1.u"),
                ir_dae::Expression::FunctionCall {
                    name: ir_dae::VarName::new("Clock"),
                    args: vec![lit(0.1)],
                    is_constructor: false,
                },
            ],
        },
        rumoca_core::Span::DUMMY,
        "sample1.y = sample(sample1.u, Clock(0.1))",
    ));

    let elim = eliminate::EliminationResult {
        substitutions: vec![rumoca_phase_solve::Substitution {
            var_name: ir_dae::VarName::new("sample1.u"),
            expr: var("load.w"),
            env_keys: vec!["sample1.u".to_string()],
        }],
        n_eliminated: 1,
    };

    let mut y = vec![3.0];
    let env = settle_runtime_discrete_capture_env(&dae, &elim, &mut y, &[], 0, 0.1);

    assert!((env.get("sample1.u") - 3.0).abs() <= 1.0e-12);
    assert!((env.get("sample1.y") - 3.0).abs() <= 1.0e-12);
}

#[test]
fn runtime_discrete_capture_context_marks_direct_sample_dependency() {
    let mut dae = ir_dae::Dae::new();
    dae.algebraics.insert(
        ir_dae::VarName::new("load.w"),
        ir_dae::Variable::new(ir_dae::VarName::new("load.w")),
    );
    dae.discrete_reals.insert(
        ir_dae::VarName::new("sample1.y"),
        ir_dae::Variable::new(ir_dae::VarName::new("sample1.y")),
    );
    dae.f_m.push(ir_dae::Equation::explicit(
        ir_dae::VarName::new("sample1.y"),
        ir_dae::Expression::BuiltinCall {
            function: ir_dae::BuiltinFunction::Sample,
            args: vec![
                var("sample1.u"),
                ir_dae::Expression::FunctionCall {
                    name: ir_dae::VarName::new("Clock"),
                    args: vec![lit(0.1)],
                    is_constructor: false,
                },
            ],
        },
        rumoca_core::Span::DUMMY,
        "sample1.y = sample(sample1.u, Clock(0.1))",
    ));

    let elim = eliminate::EliminationResult {
        substitutions: vec![rumoca_phase_solve::Substitution {
            var_name: ir_dae::VarName::new("sample1.u"),
            expr: var("load.w"),
            env_keys: vec!["sample1.u".to_string()],
        }],
        n_eliminated: 1,
    };

    let capture_ctx =
        build_runtime_discrete_capture_context(&dae, &elim, 1, 0, &["sample1.y".to_string()]);

    assert!(capture_ctx.needs_eliminated_env);
}
