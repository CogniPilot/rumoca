use super::*;
use crate::constant::{EvalContext, eval_expr_with_span};
use rumoca_core::Literal;

fn span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::from_source_name("slice.mo"), 0, 1)
}

fn vector(values: &[i64]) -> Value {
    Value::Array(values.iter().copied().map(Value::Integer).collect())
}

fn matrix() -> Value {
    Value::Array(vec![
        vector(&[11, 12]),
        vector(&[21, 22]),
        vector(&[31, 32]),
    ])
}

fn indices(values: &[i64]) -> Subscript {
    Subscript::Expr {
        expr: Box::new(Expression::Array {
            elements: values
                .iter()
                .map(|value| Expression::Literal {
                    value: Literal::Integer(*value),
                    span: span(),
                })
                .collect(),
            kind: rumoca_core::ArrayConstructor::Array,
            span: span(),
        }),
        span: span(),
    }
}

fn apply(value: Value, subscripts: &[Subscript]) -> Result<Value, EvalError> {
    let ctx = EvalContext::new();
    apply_subscripts(
        &value,
        subscripts,
        |expr| eval_expr_with_span(expr, &ctx, span()),
        span(),
    )
}

#[test]
fn vector_axes_preserve_order_and_repeated_indices() {
    assert_eq!(
        apply(matrix(), &[indices(&[3, 1, 3]), indices(&[2, 1])]).unwrap(),
        Value::Array(vec![
            vector(&[32, 31]),
            vector(&[12, 11]),
            vector(&[32, 31])
        ]),
    );
}

#[test]
fn colon_then_scalar_projects_each_row() {
    assert_eq!(
        apply(
            matrix(),
            &[
                Subscript::Colon { span: span() },
                Subscript::Index {
                    value: 2,
                    span: span()
                }
            ]
        )
        .unwrap(),
        vector(&[12, 22, 32]),
    );
}

#[test]
fn scalar_and_singleton_vector_have_different_ranks() {
    assert_eq!(
        apply(
            matrix(),
            &[Subscript::Index {
                value: 2,
                span: span()
            }]
        )
        .unwrap(),
        vector(&[21, 22])
    );
    assert_eq!(
        apply(matrix(), &[indices(&[2])]).unwrap(),
        Value::Array(vec![vector(&[21, 22])])
    );
}

#[test]
fn empty_vector_selects_no_rows() {
    assert_eq!(
        apply(matrix(), &[indices(&[]), Subscript::Colon { span: span() }]).unwrap(),
        Value::Array(vec![])
    );
}

#[test]
fn every_selected_index_remains_bounds_checked() {
    for bad in [i64::MIN, -1, 0, 4, i64::MAX] {
        assert!(
            matches!(apply(matrix(), &[indices(&[1, bad])]), Err(EvalError::IndexOutOfBounds { index, size: 3, .. }) if index == bad)
        );
    }
    assert!(matches!(
        apply(matrix(), &[indices(&[1, 2]), indices(&[3])]),
        Err(EvalError::IndexOutOfBounds {
            index: 3,
            size: 2,
            ..
        })
    ));
}

#[test]
fn scalar_projection_preserves_real_payload_bits() {
    let patterns = [
        0_u64,
        0x8000_0000_0000_0000,
        1,
        0x7ff8_0000_0000_1234,
        0x7ff0_0000_0000_0000,
        0xfff0_0000_0000_0000,
    ];
    let value = Value::Array(
        patterns
            .iter()
            .map(|bits| Value::Real(f64::from_bits(*bits)))
            .collect(),
    );
    let ctx = EvalContext::new();
    for (index, expected) in patterns.into_iter().enumerate() {
        let selected = apply_subscripts(
            &value,
            &[Subscript::Index {
                value: index as i64 + 1,
                span: span(),
            }],
            |expr| eval_expr_with_span(expr, &ctx, span()),
            span(),
        )
        .unwrap();
        let Value::Real(actual) = selected else {
            panic!("selected non-Real value")
        };
        assert_eq!(actual.to_bits(), expected);
    }
}

#[test]
fn index_evaluation_precedes_bounds_selection() {
    let value = matrix();
    let mut evaluated = 0;
    let result = apply_subscripts(
        &value,
        &[
            Subscript::Index {
                value: 99,
                span: span(),
            },
            Subscript::Expr {
                expr: Box::new(Expression::Literal {
                    value: Literal::Boolean(true),
                    span: span(),
                }),
                span: span(),
            },
        ],
        |_| {
            evaluated += 1;
            Ok(Value::Bool(true))
        },
        span(),
    );
    assert_eq!(evaluated, 1);
    assert!(matches!(result, Err(EvalError::TypeMismatch { .. })));
}

#[test]
fn selected_slice_and_whole_value_are_owned_results() {
    let value = matrix();
    let ctx = EvalContext::new();
    for subscripts in [
        vec![],
        vec![Subscript::Index {
            value: 1,
            span: span(),
        }],
    ] {
        let mut selected = apply_subscripts(
            &value,
            &subscripts,
            |expr| eval_expr_with_span(expr, &ctx, span()),
            span(),
        )
        .unwrap();
        let Value::Array(ref mut elements) = selected else {
            panic!("selected non-array value")
        };
        elements[0] = Value::Integer(-999);
        assert_eq!(value, matrix());
    }
}
