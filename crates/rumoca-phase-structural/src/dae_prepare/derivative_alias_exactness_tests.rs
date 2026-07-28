//! Regression coverage for [`super::try_extract_derivative_alias`]: only a row
//! that equates a variable to the derivative of the *whole* named state is an
//! alias.
//!
//! `eliminate_derivative_aliases` rewrites the alias it gets back into
//! `der(<state_name>)` and deletes the row. Matching the `der` argument by base
//! component name made every element row of `for i in 1:n loop der(x[i]) = u`
//! (MLS §8.3.2, with `x[i]` one element of `x` per MLS §10.5) report `u` as an
//! alias of the whole array derivative `der(x)`, so several element rows were
//! deleted for one scalar substitution.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("derivative_alias_exactness_test.mo"),
        1,
        2,
    )
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn element(name: &str, index: i64) -> Expression {
    Expression::Index {
        base: Box::new(var(name)),
        subscripts: vec![Subscript::Index {
            value: index,
            span: test_span(),
        }],
        span: test_span(),
    }
}

fn der(argument: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![argument],
        span: test_span(),
    }
}

fn residual(lhs: Expression, rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: test_span(),
        },
        span: test_span(),
        origin: "top-level model equation".to_string(),
        scalar_count: 1,
    }
}

#[test]
fn whole_state_derivative_row_is_an_alias() {
    let row = residual(var("u"), der(var("x")));

    assert_eq!(
        try_extract_derivative_alias(&row, &VarName::new("x")),
        Some(VarName::new("u"))
    );
}

#[test]
fn element_derivative_row_is_not_an_array_state_alias() {
    let row = residual(der(element("x", 1)), var("u"));

    assert_eq!(try_extract_derivative_alias(&row, &VarName::new("x")), None);
}

#[test]
fn element_derivative_row_is_an_alias_of_its_own_scalarized_state() {
    let row = residual(der(element("x", 1)), var("u"));

    assert_eq!(
        try_extract_derivative_alias(&row, &VarName::new("x[1]")),
        Some(VarName::new("u"))
    );
}
