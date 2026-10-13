use super::*;
use rumoca_core::{
    BuiltinFunction, Expression, InstanceId, Literal, OpBinary, Reference, SourceId, Span, Token,
    VarName, Variability,
};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("structural_index.mo"), 0, 1)
}
fn literal(value: Literal) -> Expression {
    Expression::Literal {
        value,
        span: span(),
    }
}
fn reference(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}
fn parameter(name: &str, binding: Expression) -> flat::Variable {
    flat::Variable {
        name: VarName::new(name),
        instance_id: InstanceId::new(1),
        variability: Variability::Parameter(Token::default()),
        binding: Some(binding),
        ..flat::Variable::empty_with_span(span())
    }
}

#[test]
fn structural_values_refuse_runtime_and_non_evaluable_bindings() {
    let mut model = flat::Model::default();
    let mut value = parameter("k", literal(Literal::Integer(2)));
    model.variables.insert(value.name.clone(), value.clone());
    assert_eq!(try_eval_flat_expr_i64(&reference("k"), &model, 0), Some(2));
    for state in 0..3 {
        value = parameter("k", literal(Literal::Integer(2)));
        match state {
            0 => value.variability = Variability::Empty,
            1 => value.evaluate_refused = true,
            _ => value.fixed = Some(vec![false]),
        }
        model.variables.insert(value.name.clone(), value.clone());
        assert_eq!(try_eval_flat_expr_i64(&reference("k"), &model, 0), None);
        assert_eq!(structural_integer_value(&value, &model), None);
    }
}

#[test]
fn integer_result_uses_language_conversion_and_never_integral_real_coercion() {
    let model = flat::Model::default();
    assert_eq!(
        try_eval_flat_expr_i64(&literal(Literal::Real(1.0)), &model, 0),
        None
    );
    let quotient = Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(literal(Literal::Integer(2))),
        rhs: Box::new(literal(Literal::Integer(2))),
        span: span(),
    };
    assert_eq!(try_eval_flat_expr_i64(&quotient, &model, 0), None);
    let conversion = Expression::BuiltinCall {
        function: BuiltinFunction::Integer,
        args: vec![literal(Literal::Real(1.9))],
        span: span(),
    };
    assert_eq!(try_eval_flat_expr_i64(&conversion, &model, 0), Some(1));
}

#[test]
fn fixed_shape_is_available_without_nonparameter_array_values_and_binding_cycles_fail() {
    let mut model = flat::Model::default();
    model.variables.insert(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            instance_id: InstanceId::new(2),
            dims: vec![3, 4],
            ..flat::Variable::empty_with_span(span())
        },
    );
    assert_eq!(
        eval_size_call_i64(&reference("x"), &literal(Literal::Integer(2)), &model, 0),
        Some(4)
    );
    assert_eq!(try_eval_flat_expr_i64(&reference("x"), &model, 0), None);
    model
        .variables
        .insert(VarName::new("k"), parameter("k", reference("k")));
    assert_eq!(try_eval_flat_expr_i64(&reference("k"), &model, 0), None);
}
