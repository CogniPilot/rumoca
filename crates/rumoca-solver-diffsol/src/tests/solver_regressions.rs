use super::*;

#[test]
fn test_reorder_trims_array_state_size_to_available_derivative_rows() {
    let mut dae = Dae::new();
    let mut x = Variable::new(VarName::new("x"));
    x.dims = vec![3];
    dae.states.insert(VarName::new("x"), x);

    // Only two derivative rows are present for x[1], x[2].
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x[1]")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x1".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x[2]")],
            },
            real(2.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x2".to_string(),
        scalar_count: 1,
    });

    problem::reorder_equations_for_solver(&mut dae)
        .expect("reorder should trim unmatched trailing state elements");

    let x_after = dae
        .states
        .get(&VarName::new("x"))
        .expect("state x should remain present");
    assert_eq!(x_after.size(), 2, "x[3] should be trimmed from state size");
    assert_eq!(x_after.dims, vec![2]);
}

#[test]
fn test_initial_equation_seeds_discrete_startup_value_for_ode_rhs() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.discrete_reals
        .insert(VarName::new("d"), Variable::new(VarName::new("d")));

    // der(x) = d
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("d"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });

    dae.initial_equations.push(dae::Equation {
        lhs: Some(VarName::new("d")),
        rhs: real(3.0),
        span: Span::DUMMY,
        origin: "init_d".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.01),
            atol: 1.0e-8,
            rtol: 1.0e-8,
            max_wall_seconds: Some(5.0),
            ..Default::default()
        },
    )
    .expect("simulation should succeed");

    let x_idx = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x should be present in results");
    let x_final = result.data[x_idx]
        .last()
        .copied()
        .expect("x trace should contain samples");
    assert!(
        (x_final - 3.0).abs() < 5.0e-2,
        "initial equation d=3 must drive der(x)=3, got x_final={x_final}"
    );
}
