//! EXPR-042 also applies when the original function runs at translation time.

use super::*;

fn guarded(operator: rumoca_core::OpBinary) -> Function {
    let mut function = Function::new("guarded", test_span());
    let real = rumoca_core::TypeId::new(1);
    let array_type = rumoca_core::EffectiveType::new(real, real, vec![3]).unwrap();
    function.add_input(rumoca_core::FunctionParam::new(
        "a",
        "Real",
        array_type,
        test_span(),
    ));
    function.add_input(integer_param("k"));
    function.add_input(integer_param("n"));
    function.add_output(real_param("y"));
    function.pure = true;
    let comparison = if operator == rumoca_core::OpBinary::And {
        rumoca_core::OpBinary::Le
    } else {
        rumoca_core::OpBinary::Gt
    };
    let condition = binary(
        operator,
        binary(comparison, var_ref("k"), var_ref("n")),
        binary(
            rumoca_core::OpBinary::Gt,
            indexed("a", "k"),
            integer_literal(5),
        ),
    );
    function.body.push(Statement::Assignment {
        comp: component_reference("y"),
        value: Expression::If {
            branches: vec![(condition, integer_literal(1))],
            else_branch: Box::new(integer_literal(0)),
            span: test_span(),
        },
        span: test_span(),
    });
    function
}

fn context(operator: rumoca_core::OpBinary) -> EvalContext {
    let mut context = EvalContext::new();
    context.add_function(guarded(operator));
    context
}

fn call_guarded(index: i64) -> Expression {
    Expression::FunctionCall {
        name: rumoca_core::Reference::new("guarded"),
        args: vec![
            Expression::Array {
                elements: [1., 2., 9.].into_iter().map(real_literal).collect(),
                kind: rumoca_core::ArrayConstructor::Array,
                span: test_span(),
            },
            integer_literal(index),
            integer_literal(3),
        ],
        is_constructor: false,
        span: test_span(),
    }
}

#[test]
fn issue_403_constant_original_and_skips_the_inactive_index() {
    let context = context(rumoca_core::OpBinary::And);
    for (index, expected) in [(4, 0), (3, 1), (2, 0), (4, 0)] {
        assert_eq!(
            crate::constant::eval_expr(&call_guarded(index), &context).unwrap(),
            Value::Integer(expected),
            "original constant call with k={index}"
        );
    }
}

#[test]
fn issue_403_constant_or_skips_the_inactive_index() {
    let context = context(rumoca_core::OpBinary::Or);
    for (index, expected) in [(4, 1), (3, 1), (2, 0), (4, 1)] {
        assert_eq!(
            crate::constant::eval_expr(&call_guarded(index), &context).unwrap(),
            Value::Integer(expected),
            "constant Or call with k={index}"
        );
    }
}

#[test]
fn issue_403_constant_selected_and_independent_indices_still_fault() {
    for operator in [rumoca_core::OpBinary::And, rumoca_core::OpBinary::Or] {
        let context = context(operator);
        assert!(matches!(
            crate::constant::eval_expr(&call_guarded(0), &context),
            Err(EvalError::IndexOutOfBounds {
                index: 0,
                size: 3,
                ..
            })
        ));
    }
    let mut context = context(rumoca_core::OpBinary::And);
    context.add_parameter(
        "a",
        Value::Array(vec![Value::Real(1.), Value::Real(2.), Value::Real(9.)]),
    );
    context.add_parameter("k", Value::Integer(4));
    let independent = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(call_guarded(4)),
        rhs: Box::new(indexed("a", "k")),
        span: test_span(),
    };
    assert!(matches!(
        crate::constant::eval_expr(&independent, &context),
        Err(EvalError::IndexOutOfBounds {
            index: 4,
            size: 3,
            ..
        })
    ));
}
