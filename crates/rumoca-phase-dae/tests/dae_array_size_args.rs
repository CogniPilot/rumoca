use rumoca_core::{Expression, Function, FunctionParam, Literal, Span, VarName};
use rumoca_ir_dae::{Dae, Equation};
use rumoca_phase_dae::insert_array_size_args_dae;

fn test_span(start: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name(file!()),
        start,
        start + 5,
    )
}

fn lit(value: i64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span,
    }
}

fn array(values: &[i64], span: Span) -> Expression {
    Expression::Array {
        elements: values.iter().map(|value| lit(*value, span)).collect(),
        is_matrix: false,
        span,
    }
}

fn named_arg(name: &str, value: Expression, span: Span) -> Expression {
    Expression::FunctionCall {
        name: VarName::new(format!("__rumoca_named_arg__.{name}")).into(),
        args: vec![value],
        is_constructor: true,
        span,
    }
}

fn function_with_array_input(span: Span) -> Function {
    let mut function = Function::new("Pkg.g", span);
    function.add_input(FunctionParam::new("u", "Real", span).with_dims(vec![-1]));
    function.add_output(FunctionParam::new("y", "Real", span));
    function
}

#[test]
fn dae_array_size_arg_uses_named_actual_value() {
    let span = test_span(71);
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.g"), function_with_array_input(span));
    dae.continuous.equations.push(Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: Expression::FunctionCall {
            name: VarName::new("Pkg.g").into(),
            args: vec![named_arg("u", array(&[1, 2, 3], span), span)],
            is_constructor: false,
            span,
        },
        span,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    insert_array_size_args_dae(&mut dae).expect("array size insertion should lower named args");

    let Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[1],
        Expression::Literal {
            value: Literal::Integer(3),
            ..
        }
    ));
}
