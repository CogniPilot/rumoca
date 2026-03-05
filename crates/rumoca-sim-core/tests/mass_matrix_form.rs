use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_sim_core::{MassMatrixFormError, TimeoutBudget, validate_constant_mass_matrix_form};

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
fn test_validate_constant_mass_matrix_form_rejects_non_affine_derivative_row() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );

    let der_x = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![var_ref("x")],
    };
    let nonlinear_derivative = dae::Expression::Binary {
        op: dae::OpBinary::Mul(Default::default()),
        lhs: Box::new(der_x.clone()),
        rhs: Box::new(der_x),
    };

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(nonlinear_derivative, real(1.0)),
        span: Span::DUMMY,
        origin: "non_affine_derivative_row".to_string(),
        scalar_count: 1,
    });

    let budget = TimeoutBudget::new(None);
    let mass_matrix = vec![vec![1.0]];
    let err = validate_constant_mass_matrix_form(&dae, 1, &mass_matrix, &budget)
        .expect_err("non-affine derivative row should be rejected");
    match err {
        MassMatrixFormError::Invalid(msg) => {
            assert!(
                msg.contains("symbolic affine check failed"),
                "expected symbolic affine rejection, got: {msg}"
            );
        }
        other => panic!("expected Invalid error, got {other:?}"),
    }
}

#[test]
fn test_validate_constant_mass_matrix_form_accepts_parameter_dependent_coefficient() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.parameters.insert(
        dae::VarName::new("p"),
        dae::Variable::new(dae::VarName::new("p")),
    );

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            dae::Expression::Binary {
                op: dae::OpBinary::Mul(Default::default()),
                lhs: Box::new(var_ref("p")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                }),
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "parameter_scaled_derivative_row".to_string(),
        scalar_count: 1,
    });

    let budget = TimeoutBudget::new(None);
    let mass_matrix = vec![vec![1.0]];
    let result = validate_constant_mass_matrix_form(&dae, 1, &mass_matrix, &budget);
    assert!(
        result.is_ok(),
        "parameter-dependent coefficient should be accepted, got {result:?}"
    );
}

#[test]
fn test_validate_constant_mass_matrix_form_rejects_runtime_dependent_coefficient() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            dae::Expression::Binary {
                op: dae::OpBinary::Mul(Default::default()),
                lhs: Box::new(dae::Expression::Binary {
                    op: dae::OpBinary::Add(Default::default()),
                    lhs: Box::new(real(1.0)),
                    rhs: Box::new(var_ref("x")),
                }),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                }),
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "runtime_scaled_derivative_row".to_string(),
        scalar_count: 1,
    });

    let budget = TimeoutBudget::new(None);
    let mass_matrix = vec![vec![1.0]];
    let err = validate_constant_mass_matrix_form(&dae, 1, &mass_matrix, &budget)
        .expect_err("runtime-dependent coefficient should be rejected");
    match err {
        MassMatrixFormError::Invalid(msg) => {
            assert!(
                msg.contains("depends on runtime variables"),
                "expected runtime-dependent coefficient rejection, got: {msg}"
            );
        }
        other => panic!("expected Invalid error, got {other:?}"),
    }
}
