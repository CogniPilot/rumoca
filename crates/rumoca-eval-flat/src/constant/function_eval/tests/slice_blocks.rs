//! Slice assignment through several subscripts (MLS 3.6 §10.5).

use super::*;

fn integer_range(start: i64, end: i64) -> Subscript {
    range_subscript(start, None, end)
}

fn real_rows(rows: &[&[f64]]) -> Expression {
    Expression::Array {
        elements: rows
            .iter()
            .map(|row| Expression::Array {
                elements: row.iter().copied().map(real_literal).collect(),
                kind: rumoca_core::ArrayConstructor::Array,
                span: Span::DUMMY,
            })
            .collect(),
        kind: rumoca_core::ArrayConstructor::Array,
        span: Span::DUMMY,
    }
}

/// MLS 3.6 §10.5: every subscript of an assignment target selects within its
/// own dimension, so `Is[1:2, 2:3] := B` writes a block (the
/// `Is[1:3, 1:3] := ...` of a spatial inertia) and `Is[3, 1:2] := v` writes
/// part of one row; every other element keeps its value.
#[test]
fn slice_assignment_writes_a_block_of_a_matrix() {
    let real = rumoca_core::TypeId::new(1);
    let mut func = Function::new("test.block", Span::DUMMY);
    func.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        rumoca_core::EffectiveType::new(real, real, vec![3, 3]).expect("matrix type"),
        test_span(),
    ));
    func.pure = true;
    func.body = vec![
        assign(
            element_target("y", vec![integer_range(1, 2), integer_range(2, 3)]),
            real_rows(&[&[1.0, 2.0], &[3.0, 4.0]]),
        ),
        assign(
            element_target(
                "y",
                vec![
                    Subscript::Index {
                        value: 3,
                        span: test_span(),
                    },
                    integer_range(1, 2),
                ],
            ),
            Expression::Array {
                elements: vec![integer_literal(5), integer_literal(6)],
                kind: rumoca_core::ArrayConstructor::Array,
                span: Span::DUMMY,
            },
        ),
    ];

    let rows = |rows: [[f64; 3]; 3]| {
        Value::Array(
            rows.iter()
                .map(|row| Value::Array(row.iter().copied().map(Value::Real).collect()))
                .collect(),
        )
    };
    assert_eq!(
        fold(&func).expect("block writes fold"),
        rows([[0.0, 1.0, 2.0], [0.0, 3.0, 4.0], [5.0, 6.0, 0.0]])
    );

    // A block value of the wrong shape is refused, never written partially.
    func.body.truncate(1);
    func.body.push(assign(
        element_target("y", vec![integer_range(1, 2), integer_range(1, 3)]),
        real_rows(&[&[1.0, 2.0], &[3.0, 4.0]]),
    ));
    assert!(fold(&func).is_err());
}

/// MLS §10.5: vector subscripts retain their axis, and the RHS is read
/// before an overlapping assignment (the pivot swap in LinearAlgebra.solve).
#[test]
fn vector_subscript_assignment_swaps_matrix_rows() {
    let real = rumoca_core::TypeId::new(1);
    let mut func = Function::new("test.swap_rows", Span::DUMMY);
    func.add_output(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        rumoca_core::EffectiveType::new(real, real, vec![2, 2]).expect("matrix type"),
        test_span(),
    ));
    func.pure = true;
    let rows = |indices: &[i64]| Subscript::Expr {
        expr: Box::new(Expression::Array {
            elements: indices.iter().copied().map(integer_literal).collect(),
            kind: rumoca_core::ArrayConstructor::Array,
            span: Span::DUMMY,
        }),
        span: test_span(),
    };
    func.body = vec![
        assign(
            component_reference("y"),
            real_rows(&[&[1.0, 2.0], &[3.0, 4.0]]),
        ),
        assign(
            element_target(
                "y",
                vec![rows(&[1, 2]), Subscript::Colon { span: test_span() }],
            ),
            Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "y",
                    component_reference("y"),
                ),
                subscripts: vec![rows(&[2, 1]), Subscript::Colon { span: test_span() }],
                span: test_span(),
            },
        ),
    ];
    let expected = Value::Array(vec![
        Value::Array(vec![Value::Real(3.0), Value::Real(4.0)]),
        Value::Array(vec![Value::Real(1.0), Value::Real(2.0)]),
    ]);
    assert_eq!(fold(&func).expect("pivot swap folds"), expected);

    // A vector alone also retains its axis; it needs no colon to be a slice.
    func.body[1] = assign(
        element_target("y", vec![rows(&[2, 1])]),
        real_rows(&[&[1.0, 2.0], &[3.0, 4.0]]),
    );
    assert_eq!(fold(&func).expect("vector-only target folds"), expected);

    for indices in [&[0, 1][..], &[1, 3][..], &[1][..]] {
        func.body[1] = assign(
            element_target("y", vec![rows(indices)]),
            real_rows(&[&[1.0, 2.0], &[3.0, 4.0]]),
        );
        assert!(fold(&func).is_err(), "invalid bounds or shape must fail");
    }
}
