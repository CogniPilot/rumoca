//! Unit tests for the initialization reciprocal residual rewrite.
//!
//! Kept beside `expression_rows.rs` rather than inside it so the lowering
//! module stays under the file-size limit while the tests keep direct access
//! to the private rewrite they pin.

use super::*;

fn reciprocal_test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_expression_rows_reciprocal.mo"),
        1,
        9,
    )
}

fn reciprocal_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: Vec::new(),
        span: reciprocal_test_span(),
    }
}

fn reciprocal_literal(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: reciprocal_test_span(),
    }
}

fn reciprocal_binary(
    op: OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: reciprocal_test_span(),
    }
}

#[test]
fn initial_reciprocal_residual_multiplies_the_denominator_through() {
    let residual = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_var("R_m"),
        reciprocal_binary(OpBinary::Div, reciprocal_literal(1), reciprocal_var("G_m")),
    );

    let cleared =
        denominator_cleared_initial_residual(&residual).expect("reciprocal row is cleared");

    let expected = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_binary(OpBinary::Mul, reciprocal_var("R_m"), reciprocal_var("G_m")),
        reciprocal_literal(1),
    );
    assert_eq!(cleared, expected);
}

#[test]
fn initial_reciprocal_residual_clears_a_left_hand_reciprocal() {
    let residual = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_binary(OpBinary::Div, reciprocal_literal(2), reciprocal_var("G_m")),
        reciprocal_var("R_m"),
    );

    let cleared =
        denominator_cleared_initial_residual(&residual).expect("reciprocal row is cleared");

    let expected = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_literal(2),
        reciprocal_binary(OpBinary::Mul, reciprocal_var("R_m"), reciprocal_var("G_m")),
    );
    assert_eq!(cleared, expected);
}

#[test]
fn initial_reciprocal_residual_keeps_a_zero_numerator_row() {
    // `a - 0/b = 0` and `a*b - 0 = 0` do NOT have the same solution set:
    // the cleared form admits `b = 0`. Such a row must stay as written.
    let residual = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_var("a"),
        reciprocal_binary(OpBinary::Div, reciprocal_literal(0), reciprocal_var("b")),
    );

    assert_eq!(denominator_cleared_initial_residual(&residual), None);
}

#[test]
fn initial_reciprocal_residual_keeps_a_computed_denominator_row() {
    let residual = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_var("a"),
        reciprocal_binary(
            OpBinary::Div,
            reciprocal_literal(1),
            reciprocal_binary(OpBinary::Add, reciprocal_var("b"), reciprocal_var("c")),
        ),
    );

    assert_eq!(denominator_cleared_initial_residual(&residual), None);
}

#[test]
fn initial_reciprocal_residual_keeps_a_variable_numerator_row() {
    let residual = reciprocal_binary(
        OpBinary::Sub,
        reciprocal_var("a"),
        reciprocal_binary(OpBinary::Div, reciprocal_var("c"), reciprocal_var("b")),
    );

    assert_eq!(denominator_cleared_initial_residual(&residual), None);
}
