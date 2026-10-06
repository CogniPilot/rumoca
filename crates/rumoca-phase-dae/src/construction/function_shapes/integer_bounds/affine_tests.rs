use super::*;

fn reference(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    }
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: Span::DUMMY,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

#[test]
fn shared_unproven_terms_cancel_to_an_exact_distance() {
    let values = ShapeEnvironment::default();
    let start = binary(OpBinary::Sub, reference("i"), integer(2));
    let end = binary(OpBinary::Sub, reference("i"), integer(1));
    assert_eq!(exact_range_distance(&start, &end, &values), Some(1));
    let scaled_start = binary(OpBinary::Mul, integer(2), reference("i"));
    let scaled_end = binary(
        OpBinary::Add,
        binary(OpBinary::Mul, reference("i"), integer(2)),
        integer(3),
    );
    assert_eq!(
        exact_range_distance(&scaled_start, &scaled_end, &values),
        Some(3)
    );
}

#[test]
fn distinct_unproven_terms_leave_the_distance_unproven() {
    let values = ShapeEnvironment::default();
    let start = reference("i");
    let end = binary(OpBinary::Add, reference("j"), integer(1));
    assert_eq!(exact_range_distance(&start, &end, &values), None);
    let product = binary(OpBinary::Mul, reference("i"), reference("i"));
    assert_eq!(exact_range_distance(&start, &product, &values), None);
}
