use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_sim_core::simulation::dae_prepare::expr_contains_der_of;
use rumoca_sim_core::{EquationReorderError, reorder_equations_for_solver};

fn var_ref(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn real(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

#[test]
fn test_reorder_equations_for_solver_uses_maximum_matching_for_shared_derivative_rows() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.states.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("y")],
            },
        ),
        span: Span::DUMMY,
        origin: "ode_shared_xy".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x_only".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), var_ref("x")),
        span: Span::DUMMY,
        origin: "alg_z".to_string(),
        scalar_count: 1,
    });

    reorder_equations_for_solver(&mut dae)
        .expect("state-row matching should pick a valid assignment for x and y");

    let ode_rows = &dae.f_x[..2];
    assert!(
        ode_rows
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &dae::VarName::new("x"))),
        "expected reordered ODE rows to include der(x)"
    );
    assert!(
        ode_rows
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &dae::VarName::new("y"))),
        "expected reordered ODE rows to include der(y)"
    );
}

#[test]
fn test_reorder_equations_for_solver_reports_mismatch_when_rows_are_insufficient() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.states.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(0.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x_only".to_string(),
        scalar_count: 1,
    });

    let err = reorder_equations_for_solver(&mut dae)
        .expect_err("reorder should fail when equation count is below state count");
    assert!(matches!(
        err,
        EquationReorderError::EquationMismatch {
            n_equations: 1,
            n_states: 2,
            ..
        }
    ));
}

#[test]
fn test_reorder_trims_array_state_size_to_available_derivative_rows() {
    let mut dae = dae::Dae::new();
    let mut x = dae::Variable::new(dae::VarName::new("x"));
    x.dims = vec![3];
    dae.states.insert(dae::VarName::new("x"), x);

    // Only two derivative rows are present for x[1], x[2].
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
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
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var_ref("x[2]")],
            },
            real(2.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x2".to_string(),
        scalar_count: 1,
    });

    reorder_equations_for_solver(&mut dae)
        .expect("reorder should trim unmatched trailing state elements");

    let x_after = dae
        .states
        .get(&dae::VarName::new("x"))
        .expect("state x should remain present");
    assert_eq!(x_after.size(), 2, "x[3] should be trimmed from state size");
    assert_eq!(x_after.dims, vec![2]);
}
