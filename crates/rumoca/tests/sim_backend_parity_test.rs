use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_sim_diffsol::SimOptions;

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
fn diffsol_and_rk45_match_on_simple_causal_ode() {
    let dae_model = simple_decay_dae();
    let opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0,
        dt: Some(0.05),
        rtol: 1e-7,
        atol: 1e-9,
        max_wall_seconds: Some(5.0),
        ..SimOptions::default()
    };

    let diffsol = rumoca_sim_diffsol::simulate(&dae_model, &opts).expect("diffsol simulation");
    let rk45 = rumoca_sim_rk45::simulate(&dae_model, &opts).expect("rk45 simulation");

    assert_eq!(diffsol.times.len(), rk45.times.len());
    assert_eq!(diffsol.n_states, 1);
    assert_eq!(rk45.n_states, 1);

    let diffsol_x = &diffsol.data[0];
    let rk45_x = &rk45.data[0];
    let max_abs_diff = diffsol_x
        .iter()
        .zip(rk45_x.iter())
        .map(|(a, b)| (a - b).abs())
        .fold(0.0_f64, f64::max);

    assert!(
        max_abs_diff <= 5.0e-3,
        "expected rk45/diffsol parity for simple causal ODE, max_abs_diff={max_abs_diff}"
    );
}
