use super::*;

#[test]
fn test_simulate_reconstruct_resolves_qualified_k_q_constants_after_elimination() {
    let mut dae = Dae::new();

    let mut x = Variable::new(VarName::new("x"));
    x.start = Some(real(0.0));
    x.fixed = Some(true);
    dae.states.insert(VarName::new("x"), x);
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));

    // 0 = der(x)
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

    // 0 = a - (device.k / device.q)
    // The `a` equation is trivially eliminated, so reconstruction must still
    // see finite injected values for the qualified constants.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("a"),
            Expression::Binary {
                op: OpBinary::Div(Default::default()),
                lhs: Box::new(var_ref("device.k")),
                rhs: Box::new(var_ref("device.q")),
            },
        ),
        span: Span::DUMMY,
        origin: "alg_constant_ratio".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    )
    .expect("simulation should succeed with qualified k/q reconstruction");

    let a_idx = result
        .names
        .iter()
        .position(|name| name == "a")
        .expect("reconstructed variable a should be present");
    assert!(
        result.data[a_idx].iter().all(|v| v.is_finite()),
        "reconstructed a should remain finite, got {:?}",
        result.data[a_idx]
    );
}

#[test]
fn test_index_reduction_differentiates_constraint_for_missing_state_derivative() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // 0 = x - z       (constraint for x, no der(x) initially)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("z")),
        span: Span::DUMMY,
        origin: "constraint_x".to_string(),
        scalar_count: 1,
    });
    // 0 = z - v       (defines z = v so der(z) is resolvable)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), var_ref("v")),
        span: Span::DUMMY,
        origin: "def_z".to_string(),
        scalar_count: 1,
    });
    // 0 = der(v) - 1  (provides known derivative for v)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("v")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_v".to_string(),
        scalar_count: 1,
    });

    let mut dae_before = dae.clone();
    assert!(
        matches!(
            problem::reorder_equations_for_solver(&mut dae_before),
            Err(SimError::MissingStateEquation(name)) if name == "x"
        ),
        "expected missing der(x) before index reduction"
    );

    let changed = index_reduce_missing_state_derivatives(&mut dae);
    assert_eq!(changed, 1, "expected one differentiated constraint");
    assert!(
        dae.f_x
            .iter()
            .any(|eq| problem::expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "expected index reduction to introduce der(x)"
    );
    assert!(
        dae.f_x
            .iter()
            .any(|eq| eq.origin.contains("index_reduction:d_dt_for_x")),
        "expected transformed equation origin marker"
    );

    let mut dae_after = dae.clone();
    assert!(
        problem::reorder_equations_for_solver(&mut dae_after).is_ok(),
        "expected reorder to succeed after index reduction"
    );
}

#[test]
fn test_expr_contains_der_of_matches_field_access_state_reference() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::FieldAccess {
            base: Box::new(var_ref("x")),
            field: "im".to_string(),
        }],
    };
    assert!(
        problem::expr_contains_der_of(&expr, &VarName::new("x.im")),
        "der(FieldAccess(x, im)) should match scalarized state x.im"
    );
}

#[test]
fn test_expr_contains_der_of_matches_indexed_field_access_state_reference() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::FieldAccess {
            base: Box::new(Expression::Index {
                base: Box::new(var_ref("x")),
                subscripts: vec![dae::Subscript::Index(2)],
            }),
            field: "im".to_string(),
        }],
    };
    assert!(
        problem::expr_contains_der_of(&expr, &VarName::new("x[2].im")),
        "der(FieldAccess(Index(x,2), im)) should match scalarized state x[2].im"
    );
}
