use super::*;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("initialization_test.mo"),
        5,
        17,
    )
}

fn real_lit(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn der_of(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref(name)],
        span: test_span(),
    }
}

fn real_var(name: &str, fixed: Option<bool>, start: Option<Expression>) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        component_ref: Some(component_ref(name)),
        is_primitive: true,
        fixed,
        start,
        source_span: test_span(),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: test_span(),
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
        }],
        def_id: None,
    }
}

fn add_derivative_equation(flat: &mut Model, state_name: &str) {
    flat.add_equation(flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(der_of(state_name)),
            rhs: Box::new(real_lit(0.0)),
            span: test_span(),
        },
        span: test_span(),
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
        },
    )
    .expect("state with fixed start should lower");

    let equations = fixed_start_equations_for(&dae, "x");
    assert_eq!(equations.len(), 1);
    assert!(matches!(
        equations[0].rhs,
        Expression::Literal {
            value: Literal::Real(2.0),
            span
        } if span == test_span()
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
        },
    )
    .expect("algebraic with fixed start should lower");

    let equations = fixed_start_equations_for(&dae, "y");
    assert_eq!(equations.len(), 1);
    assert!(matches!(
        equations[0].rhs,
        Expression::Literal {
            value: Literal::Real(3.0),
            span
        } if span == test_span()
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
        },
    )
    .expect("zero-size fixed variable should lower");

    assert!(fixed_start_equations_for(&dae, "x").is_empty());
}
