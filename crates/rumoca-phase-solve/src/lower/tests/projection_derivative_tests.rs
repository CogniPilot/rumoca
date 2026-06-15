use super::*;

#[test]
fn lower_derivative_rhs_extracts_explicit_state_derivative_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("k"), scalar_var("k"));
    dae_model.continuous.equations.push(residual(sub(
        der(var("x")),
        mul(
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Box::new(var("k")),
                span: rumoca_core::Span::DUMMY,
            },
            var("x"),
        ),
    )));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout).expect("explicit xdot should lower"),
    );
    let (_, output) = eval_linear_ops(&rows.programs[0], &[2.0], &[3.0], 0.0);

    assert!((output.expect("xdot row") + 6.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_additive_zero_residual_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("q"), scalar_var("q"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("qd"), scalar_var("qd"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("wq"), scalar_var("wq"));
    dae_model
        .continuous
        .equations
        .push(residual(sub(var("qd"), der(var("q")))));
    dae_model
        .continuous
        .equations
        .push(residual(add(der(var("qd")), mul(var("wq"), var("q")))));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("additive zero residual derivative rows should lower"),
    );
    let (_, q_derivative) = eval_linear_ops(&rows.programs[0], &[2.0, 5.0], &[3.0], 0.0);
    let (_, qd_derivative) = eval_linear_ops(&rows.programs[1], &[2.0, 5.0], &[3.0], 0.0);

    assert!((q_derivative.expect("q derivative row") - 5.0).abs() < 1e-12);
    assert!((qd_derivative.expect("qd derivative row") + 6.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_piecewise_state_derivative_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("limited"), scalar_var("limited"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .continuous
        .equations
        .push(residual(rumoca_core::Expression::If {
            branches: vec![(var("limited"), sub(der(var("x")), real_lit(0.0)))],
            else_branch: Box::new(sub(der(var("x")), var("u"))),
            span: rumoca_core::Span::DUMMY,
        }));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("piecewise derivative equation should lower"),
    );
    let mut p = vec![0.0; 2];
    set_p_value(&layout, &mut p, "limited", 0.0);
    set_p_value(&layout, &mut p, "u", 4.0);
    let (_, unconstrained_output) = eval_linear_ops(&rows.programs[0], &[0.0], &p, 0.0);

    set_p_value(&layout, &mut p, "limited", 1.0);
    let (_, limited_output) = eval_linear_ops(&rows.programs[0], &[0.0], &p, 0.0);

    assert!((unconstrained_output.expect("unconstrained derivative") - 4.0).abs() < 1e-12);
    assert!(limited_output.expect("limited derivative").abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_nested_piecewise_inductor_row() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("i"), scalar_var("i"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("v"), scalar_var("v"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("L"), scalar_var("L"));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("quasiStatic"),
        scalar_var("quasiStatic"),
    );
    dae_model.continuous.equations.push(residual(sub(
        var("v"),
        rumoca_core::Expression::If {
            branches: vec![(var("quasiStatic"), real_lit(0.0))],
            else_branch: Box::new(mul(var("L"), der(var("i")))),
            span: rumoca_core::Span::DUMMY,
        },
    )));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("nested piecewise derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "v", 6.0);
    set_p_value(&layout, &mut p, "L", 2.0);
    set_p_value(&layout, &mut p, "quasiStatic", 0.0);

    let (_, output) = eval_linear_ops(&rows.programs[0], &y, &p, 0.0);

    assert!((output.expect("inductor derivative") - 3.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_extracts_whole_if_with_derivative_free_branch() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("T"), scalar_var("T"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("flow_a"), scalar_var("flow_a"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("flow_b"), scalar_var("flow_b"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("m"), scalar_var("m"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("cv"), scalar_var("cv"));
    dae_model
        .continuous
        .equations
        .push(residual(rumoca_core::Expression::If {
            branches: vec![(
                binary(rumoca_core::OpBinary::Gt, var("m"), real_lit(0.0)),
                sub(
                    add(var("flow_a"), var("flow_b")),
                    mul(mul(var("m"), var("cv")), der(var("T"))),
                ),
            )],
            else_branch: Box::new(sub(add(var("flow_a"), var("flow_b")), real_lit(0.0))),
            span: rumoca_core::Span::DUMMY,
        }));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("whole-if derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "flow_a", 10.0);
    set_y_value(&layout, &mut y, "flow_b", 20.0);
    set_p_value(&layout, &mut p, "m", 2.0);
    set_p_value(&layout, &mut p, "cv", 5.0);

    let (_, output) = eval_linear_ops(&rows.programs[0], &y, &p, 0.0);

    assert!((output.expect("state derivative") - 3.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_preserves_one_element_array_state_index() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            dims: vec![1],
            ..Default::default()
        },
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(indexed_var("x", 1)), var("u"))));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("one-element array-state derivative should lower"),
    );
    let (_, output) = eval_linear_ops(&rows.programs[0], &[0.0, 4.0], &[], 0.0);

    assert!((output.expect("x[1] derivative") - 4.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_lowers_coupled_array_state_solve_to_solver_ir() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("omega"),
        dae::Variable {
            name: rumoca_core::VarName::new("omega"),
            dims: vec![2],
            ..Default::default()
        },
    );
    for name in ["J[1,1]", "J[1,2]", "J[2,1]", "J[2,2]", "tau[1]", "tau[2]"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(residual(sub(
        add(
            mul(var("J[1,1]"), der(indexed_var("omega", 1))),
            mul(var("J[1,2]"), der(indexed_var("omega", 2))),
        ),
        var("tau[1]"),
    )));
    dae_model.continuous.equations.push(residual(sub(
        add(
            mul(var("J[2,1]"), der(indexed_var("omega", 1))),
            mul(var("J[2,2]"), der(indexed_var("omega", 2))),
        ),
        var("tau[2]"),
    )));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = rumoca_eval_solve::to_scalar_program_block(
        &lower_derivative_rhs(&dae_model, &layout).expect("coupled xdot should lower"),
    );
    assert!(rows.programs[0].iter().any(|op| {
        matches!(
            op,
            LinearOp::LinearSolveComponent {
                n: 2,
                component: 0,
                ..
            }
        )
    }));
    let y = [0.0, 0.0, 2.0, 0.0, 0.0, 4.0, 8.0, 20.0];
    let (_, first) = eval_linear_ops(&rows.programs[0], &y, &[], 0.0);
    let (_, second) = eval_linear_ops(&rows.programs[1], &y, &[], 0.0);

    assert!((first.expect("omega[1] derivative") - 4.0).abs() < 1e-12);
    assert!((second.expect("omega[2] derivative") - 5.0).abs() < 1e-12);
}

#[test]
fn lower_derivative_rhs_scalar_programs_reuse_coupled_state_components() {
    let mut dae_model = dae::Dae::default();
    for name in ["x", "y"] {
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    for name in ["u", "v"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model
        .continuous
        .equations
        .push(residual(sub(add(der(var("x")), der(var("y"))), var("u"))));
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(var("y")), var("v"))));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_derivative_rhs_scalar_programs(&dae_model, &layout)
        .expect("scalar derivative rows should preserve coupled derivative groups");

    assert_eq!(rows.len(), 2);
    assert!(rows[0].iter().any(|op| {
        matches!(
            op,
            LinearOp::LinearSolveComponent {
                n: 2,
                component: 0,
                ..
            }
        )
    }));
    assert!(rows[1].iter().any(|op| {
        matches!(
            op,
            LinearOp::LinearSolveComponent {
                n: 2,
                component: 1,
                ..
            }
        )
    }));
}
