use rumoca_core::Span;
use rumoca_ir_dae as dae;

fn var_ref(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn real(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn der(name: &str) -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![var_ref(name)],
    }
}

fn add(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Add(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn equation(rhs: dae::Expression, origin: &str) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        scalar_count: 1,
        origin: origin.to_string(),
    }
}

fn simple_decay_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::new();
    let mut x_var = dae::Variable::new(dae::VarName::new("x"));
    x_var.start = Some(real(1.0));
    dae_model.states.insert(dae::VarName::new("x"), x_var);

    // Residual form: der(x) + x = 0, so explicit form is x_dot = -x.
    dae_model
        .f_x
        .push(equation(add(der("x"), var_ref("x")), "ode_x"));
    dae_model
}

#[test]
fn rk45_tracks_simple_decay_solution() {
    let dae_model = simple_decay_dae();
    let opts = rumoca_sim_core::SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        dt: Some(0.05),
        rtol: 1e-7,
        atol: 1e-9,
        max_wall_seconds: Some(5.0),
        ..rumoca_sim_core::SimOptions::default()
    };

    let result = rumoca_sim_rk45::simulate(&dae_model, &opts).expect("rk45 simulation");
    let final_x = result
        .data
        .first()
        .and_then(|series| series.last())
        .copied()
        .expect("state series");

    let expected = (-1.0_f64).exp();
    assert!(
        (final_x - expected).abs() <= 5.0e-3,
        "expected x(1)=e^-1 ~= {expected}, got {final_x}"
    );
}

#[test]
fn rk45_rejects_non_causal_dae_shape() {
    let mut dae_model = dae::Dae::new();
    dae_model.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae_model.states.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    // Cross-derivative row keeps DAE shape non-causal for explicit RK methods.
    dae_model
        .f_x
        .push(equation(sub(add(der("x"), der("y")), real(1.0)), "ode_xy"));
    dae_model
        .f_x
        .push(equation(sub(der("y"), real(1.0)), "ode_y"));

    let result = rumoca_sim_rk45::simulate(&dae_model, &rumoca_sim_core::SimOptions::default());
    let err = result.expect_err("rk45 must reject non-causal DAE");

    match err {
        rumoca_sim_core::SimError::SolverError(msg) => {
            assert!(
                msg.contains("causal RK45 form check failed")
                    && msg.contains("must reference exactly der(x) only"),
                "unexpected rk45 rejection message: {msg}"
            );
        }
        other => panic!("unexpected error type: {other:?}"),
    }
}

#[test]
fn rk45_rejects_reverse_time_span() {
    let dae_model = simple_decay_dae();
    let opts = rumoca_sim_core::SimOptions {
        t_start: 1.0,
        t_end: 0.0,
        ..rumoca_sim_core::SimOptions::default()
    };

    let result = rumoca_sim_rk45::simulate(&dae_model, &opts);
    let err = result.expect_err("rk45 must reject reverse-time spans");
    match err {
        rumoca_sim_core::SimError::SolverError(msg) => {
            assert!(
                msg.contains("forward-time simulation only"),
                "unexpected reverse-time rejection message: {msg}"
            );
        }
        other => panic!("unexpected error type: {other:?}"),
    }
}
