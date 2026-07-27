//! Regression tests for fingerprint-bucketed duplicate continuous-equation
//! removal.
//!
//! The pruning key is span-insensitive by construction, so the cases below pin
//! both directions: rows that differ only in provenance must collapse, and rows
//! that differ in an actual semantic component (lhs name, state, operand order,
//! scalar count) must survive. The `FunctionCall` pair pins the one asymmetry
//! the module doc calls out — provenance-sensitive callee, provenance-insensitive
//! arguments — and `dedup_confirms_once_per_duplicate_row` pins the comparison
//! budget as an exact operation count rather than a wall-clock bound.

use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, Reference, SourceId, Span, Subscript, VarName,
};
use rumoca_ir_dae as dae;

use super::remove_duplicate_continuous_equations;

/// Distinct spans, so two structurally identical rows can only be told apart by
/// provenance.
fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("sim_expr_util_dedup.mo"),
        offset,
        offset + 1,
    )
}

fn other_source_span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("sim_expr_util_dedup_other.mo"),
        offset,
        offset + 1,
    )
}

fn equation(lhs: Option<Reference>, rhs: Expression, at: Span) -> dae::Equation {
    dae::Equation {
        lhs,
        rhs,
        span: at,
        origin: "expr_util_dedup_test".to_string(),
        scalar_count: 1,
    }
}

fn dae_with(equations: Vec<dae::Equation>) -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.continuous.equations = equations;
    dae
}

fn real(value: f64, at: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: at,
    }
}

fn integer(value: i64, at: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: at,
    }
}

fn var(name: &str, at: Span) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: at,
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression, at: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: at,
    }
}

fn der(name: &str, at: Span) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name, at)],
        span: at,
    }
}

/// `der(state) - residual`, the shape that gets the state-keyed identity.
fn derivative_residual(state: &str, residual: Expression, at: Span) -> Expression {
    binary(OpBinary::Sub, der(state, at), residual, at)
}

fn lhs_names(dae: &dae::Dae) -> Vec<String> {
    dae.continuous
        .equations
        .iter()
        .map(|equation| match equation.lhs.as_ref() {
            Some(lhs) => lhs.as_str().to_string(),
            None => "<none>".to_string(),
        })
        .collect()
}

#[test]
fn dedup_removes_span_only_duplicate_literal_rows() {
    let mut dae = dae_with(vec![
        equation(Some(Reference::new("x")), real(1.0, span(10)), span(10)),
        equation(Some(Reference::new("x")), real(1.0, span(90)), span(90)),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 1);
}

#[test]
fn dedup_removes_span_only_duplicate_array_rows() {
    let first = Expression::Array {
        elements: vec![
            integer(1, span(11)),
            integer(2, span(12)),
            integer(3, span(13)),
        ],
        is_matrix: false,
        span: span(10),
    };
    let second = Expression::Array {
        elements: vec![
            integer(1, other_source_span(211)),
            integer(2, other_source_span(212)),
            integer(3, other_source_span(213)),
        ],
        is_matrix: false,
        span: other_source_span(210),
    };
    let mut dae = dae_with(vec![
        equation(Some(Reference::new("x")), first, span(10)),
        equation(Some(Reference::new("x")), second, other_source_span(210)),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 1);
}

#[test]
fn dedup_ignores_lhs_reference_provenance() {
    let structured_lhs =
        Reference::generated_component("x", Vec::<Subscript>::new(), other_source_span(400));
    assert_eq!(structured_lhs.var_name(), &VarName::new("x"));

    let mut dae = dae_with(vec![
        equation(Some(Reference::new("x")), real(2.5, span(10)), span(10)),
        equation(
            Some(structured_lhs),
            real(2.5, other_source_span(401)),
            other_source_span(401),
        ),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 1);
}

#[test]
fn dedup_collapses_derivative_residuals_across_lhs_and_keeps_first() {
    let mut dae = dae_with(vec![
        equation(
            Some(Reference::new("y1")),
            derivative_residual("x", var("f", span(11)), span(10)),
            span(10),
        ),
        equation(
            Some(Reference::new("y2")),
            derivative_residual(
                "x",
                var("f", other_source_span(511)),
                other_source_span(510),
            ),
            other_source_span(510),
        ),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(lhs_names(&dae), ["y1"]);
}

#[test]
fn dedup_keeps_distinct_derivative_states() {
    let mut dae = dae_with(vec![
        equation(
            None,
            derivative_residual("x", var("f", span(11)), span(10)),
            span(10),
        ),
        equation(
            None,
            derivative_residual("z", var("f", span(21)), span(20)),
            span(20),
        ),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 2);
}

#[test]
fn dedup_keeps_operand_order_sensitive_rows() {
    let mut dae = dae_with(vec![
        equation(
            Some(Reference::new("x")),
            binary(
                OpBinary::Add,
                var("a", span(11)),
                var("b", span(12)),
                span(10),
            ),
            span(10),
        ),
        equation(
            Some(Reference::new("x")),
            binary(
                OpBinary::Add,
                var("b", span(21)),
                var("a", span(22)),
                span(20),
            ),
            span(20),
        ),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 2);
}

#[test]
fn dedup_keeps_rows_with_different_scalar_count() {
    let mut first = equation(Some(Reference::new("x")), real(1.0, span(10)), span(10));
    first.scalar_count = 1;
    let mut second = equation(Some(Reference::new("x")), real(1.0, span(20)), span(20));
    second.scalar_count = 3;
    let mut dae = dae_with(vec![first, second]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 2);
}

/// The lhs name is part of the bucket key, not just of the confirmation: two
/// rows that assign the *same* value to *different* variables are independent
/// equations. Dropping the name from the key would put them in one bucket,
/// where the rhs-only identity comparison would silently delete one of them.
#[test]
fn dedup_keeps_rows_with_equal_rhs_and_distinct_lhs() {
    let mut dae = dae_with(vec![
        equation(Some(Reference::new("x")), real(1.0, span(10)), span(10)),
        equation(Some(Reference::new("y")), real(1.0, span(20)), span(20)),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(lhs_names(&dae), ["x", "y"]);
}

#[test]
fn dedup_preserves_source_order_of_survivors() {
    let mut dae = dae_with(vec![
        equation(Some(Reference::new("a")), real(1.0, span(10)), span(10)),
        equation(Some(Reference::new("b")), real(2.0, span(20)), span(20)),
        equation(Some(Reference::new("a")), real(1.0, span(30)), span(30)),
        equation(Some(Reference::new("c")), real(3.0, span(40)), span(40)),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(lhs_names(&dae), ["a", "b", "c"]);
}

fn function_call(callee: Reference, args: Vec<Expression>, at: Span) -> Expression {
    Expression::FunctionCall {
        name: callee,
        args,
        is_constructor: false,
        span: at,
    }
}

/// Pins half of the module doc's provenance claim: a `FunctionCall`'s
/// *arguments* are compared span-insensitively like any other operand, so rows
/// that differ only in argument provenance collapse. (Under the superseded
/// rendered-`String` key the whole call went through `format!("{expr:?}")`, so
/// both rows survived.)
#[test]
fn dedup_removes_function_call_rows_differing_only_in_argument_spans() {
    let mut dae = dae_with(vec![
        equation(
            Some(Reference::new("x")),
            function_call(
                Reference::new("f"),
                vec![var("a", span(11)), real(2.0, span(12))],
                span(10),
            ),
            span(10),
        ),
        equation(
            Some(Reference::new("x")),
            function_call(
                Reference::new("f"),
                vec![
                    var("a", other_source_span(611)),
                    real(2.0, other_source_span(612)),
                ],
                other_source_span(610),
            ),
            other_source_span(610),
        ),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 1);
}

/// Pins the other half: the *callee* reference is still compared with
/// `Reference`'s `PartialEq`, so two calls whose callees differ only in
/// resolution metadata share a fingerprint bucket, fail confirmation, and are
/// both kept.
#[test]
fn dedup_keeps_function_call_rows_with_distinct_callee_provenance() {
    let plain = Reference::new("f");
    let structured =
        Reference::generated_component("f", Vec::<Subscript>::new(), other_source_span(700));
    assert_eq!(plain.var_name(), structured.var_name());

    let mut dae = dae_with(vec![
        equation(
            Some(Reference::new("x")),
            function_call(plain, vec![var("a", span(11))], span(10)),
            span(10),
        ),
        equation(
            Some(Reference::new("x")),
            function_call(structured, vec![var("a", span(21))], span(20)),
            span(20),
        ),
    ]);

    remove_duplicate_continuous_equations(&mut dae);

    assert_eq!(dae.continuous.equations.len(), 2);
}

/// Confirmations performed on this thread since the previous call.
fn take_semantic_equality_confirmations() -> usize {
    super::SEMANTIC_EQUALITY_CONFIRMATIONS.with(|count| count.replace(0))
}

/// Pruning must cost one full comparison per duplicate row, not one per
/// surviving row — asserted as an operation count, which is what actually
/// separates `O(n)` from `O(n^2)`, and is deterministic where a wall-clock
/// bound on a shared runner is not.
///
/// Every row here has a distinct rhs, so a per-row linear scan over all
/// survivors would still produce the right answer while performing roughly
/// `DISTINCT` squared (~260k) comparisons instead of the `DISTINCT` below. The
/// count is exact: bucket membership is decided by full `DuplicateEquationKey`
/// equality, never by a raw hash, so distinct rows never share a bucket.
#[test]
fn dedup_confirms_once_per_duplicate_row() {
    const DISTINCT: usize = 512;
    let mut equations = Vec::with_capacity(DISTINCT * 2);
    for index in 0..DISTINCT {
        let name = format!("dedup_scale_{index:04}");
        let value = index as f64;
        equations.push(equation(
            Some(Reference::new(name.clone())),
            real(value, span(index)),
            span(index),
        ));
        // Same row, different provenance: a duplicate that must collapse.
        equations.push(equation(
            Some(Reference::new(name)),
            real(value, other_source_span(index)),
            other_source_span(index),
        ));
    }
    let mut dae = dae_with(equations);

    take_semantic_equality_confirmations();
    remove_duplicate_continuous_equations(&mut dae);
    let confirmations = take_semantic_equality_confirmations();

    assert_eq!(dae.continuous.equations.len(), DISTINCT);
    // Each duplicate is confirmed against the one row already in its bucket;
    // every other row opens an empty bucket and is compared against nothing.
    assert_eq!(confirmations, DISTINCT);
}
