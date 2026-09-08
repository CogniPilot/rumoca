use super::super::*;
use super::support::*;

fn marker(name: &str, value: Expression, span: Span) -> Expression {
    Expression::FunctionCall {
        name: rumoca_core::Reference::generated(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    }
}

fn marker_model(residual: Expression, span: Span) -> flat::Model {
    let mut model = test_model();
    model.is_partial = true;
    model.add_equation(flat::Equation::new(
        residual,
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

#[test]
fn standalone_generated_named_argument_rejects_at_dae_boundary() {
    let source = TestSource::new("__rumoca_named_arg__.x(1.0)");
    let span = source.span("__rumoca_named_arg__.x(1.0)", 0);
    let expression = marker(
        "x",
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        span,
    );
    let model = marker_model(expression, span);
    let error = construct(&model, source.map)
        .expect_err("a named-argument wrapper is not a standalone Flat value");
    assert!(error.to_string().contains("eliminated before DAE"));
}

#[test]
fn nested_generated_named_argument_rejects_at_dae_boundary() {
    let source = TestSource::new("2.0 + __rumoca_named_arg__.x(1.0)");
    let marker_span = source.span("__rumoca_named_arg__.x(1.0)", 0);
    let expression = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(Expression::Literal {
            value: Literal::Real(2.0),
            span: source.span("2.0", 0),
        }),
        rhs: Box::new(marker(
            "x",
            Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            },
            marker_span,
        )),
        span: source.span("2.0 + __rumoca_named_arg__.x(1.0)", 0),
    };
    let equation_span = expression
        .span()
        .expect("fixture expression has provenance");
    let model = marker_model(expression, equation_span);
    let error = construct(&model, source.map)
        .expect_err("nested named-argument wrappers cannot be erased as ordinary values");
    assert!(error.to_string().contains("eliminated before DAE"));
}

#[test]
fn nonformal_generated_named_argument_name_is_not_ignored() {
    let source = TestSource::new("__rumoca_named_arg__.not_a_formal(1.0)");
    let span = source.span("__rumoca_named_arg__.not_a_formal(1.0)", 0);
    let expression = marker(
        "not_a_formal",
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        span,
    );
    let model = marker_model(expression, span);
    let error = construct(&model, source.map)
        .expect_err("DAE accepts no leftover marker regardless of its suffix");
    assert!(error.to_string().contains("eliminated before DAE"));
}
