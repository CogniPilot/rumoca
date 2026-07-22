use super::*;

fn complex_environment() -> VarEnv<f64> {
    let mut env = VarEnv::new();
    let mut functions = IndexMap::new();
    let mut complex = rumoca_core::Function::new("Complex", rumoca_core::Span::DUMMY);
    complex.is_constructor = true;
    complex.add_input(rumoca_core::FunctionParam::new(
        "re",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    complex.add_input(rumoca_core::FunctionParam::new(
        "im",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    functions.insert("Complex".to_string(), complex);
    env.functions = std::sync::Arc::new(functions);
    env
}

fn complex(re: f64, im: f64) -> Expression {
    Expression::FunctionCall {
        name: Reference::new("Complex"),
        args: vec![lit(re), lit(im)],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn field(base: Expression, field: &str) -> Expression {
    Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn evaluates_scalar_times_complex_components_without_materializing_record() {
    let product = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lit(2.0)),
        rhs: Box::new(complex(3.0, 4.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let env = complex_environment();

    assert_eq!(eval_expr_value(&field(product.clone(), "re"), &env), 6.0);
    assert_eq!(eval_expr_value(&field(product, "im"), &env), 8.0);
}

#[test]
fn evaluates_complex_multiplication_components() {
    let product = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(complex(1.0, 2.0)),
        rhs: Box::new(complex(3.0, 4.0)),
        span: rumoca_core::Span::DUMMY,
    };
    let env = complex_environment();

    assert_eq!(eval_expr_value(&field(product.clone(), "re"), &env), -5.0);
    assert_eq!(eval_expr_value(&field(product, "im"), &env), 10.0);
}
