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
        value,
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
