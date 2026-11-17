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
