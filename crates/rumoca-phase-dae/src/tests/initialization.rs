use super::*;

fn real_lit(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: Span::DUMMY,
    }
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn der_of(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref(name)],
        span: Span::DUMMY,
    }
}

fn real_var(name: &str, fixed: Option<bool>, start: Option<Expression>) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        is_primitive: true,
        fixed,
        start,
        ..Default::default()
    }
}

fn add_derivative_equation(flat: &mut Model, state_name: &str) {
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(der_of(state_name)),
            rhs: Box::new(real_lit(0.0)),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: flat::EquationOrigin::ComponentEquation {
            component: "init-fixture".to_string(),
        },
        scalar_count: 1,
    });
}

fn fixed_start_equations_for<'a>(dae: &'a Dae, name: &str) -> Vec<&'a dae::Equation> {
    dae.initialization
        .equations
        .iter()
        .filter(|eq| {
            eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name)
                && eq.origin.starts_with("fixed start initialization")
        })
        .collect()
}

#[test]
fn todae_adds_fixed_start_initial_equation_for_state() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("x"),
        real_var("x", Some(true), Some(real_lit(2.0))),
    );
    add_derivative_equation(&mut flat, "x");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("state with fixed start should lower");

    let equations = fixed_start_equations_for(&dae, "x");
    assert_eq!(equations.len(), 1);
    assert!(matches!(
        equations[0].rhs,
        Expression::Literal {
            value: Literal::Real(2.0),
            span: Span::DUMMY
        }
    ));
}

#[test]
fn todae_adds_fixed_start_initial_equation_for_algebraic() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("y"),
        real_var("y", Some(true), Some(real_lit(3.0))),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("algebraic with fixed start should lower");

    let equations = fixed_start_equations_for(&dae, "y");
    assert_eq!(equations.len(), 1);
    assert!(matches!(
        equations[0].rhs,
        Expression::Literal {
            value: Literal::Real(3.0),
            span: Span::DUMMY
        }
    ));
}

#[test]
fn todae_does_not_add_fixed_start_initial_equation_by_default() {
    let mut flat = Model::new();
    flat.add_variable(VarName::new("y"), real_var("y", None, Some(real_lit(3.0))));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("default non-parameter fixed=false should lower");

    assert!(fixed_start_equations_for(&dae, "y").is_empty());
}

#[test]
fn todae_respects_explicit_fixed_false_for_start_attribute() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("y"),
        real_var("y", Some(false), Some(real_lit(3.0))),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("fixed=false start attribute should lower");

    assert!(fixed_start_equations_for(&dae, "y").is_empty());
}

#[test]
fn todae_skips_fixed_start_initial_equation_for_zero_size_array() {
    let mut flat = Model::new();
    let mut variable = real_var("x", Some(true), Some(real_lit(1.0)));
    variable.dims = vec![0];
    flat.add_variable(VarName::new("x"), variable);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("zero-size fixed variable should lower");

    assert!(fixed_start_equations_for(&dae, "x").is_empty());
}
