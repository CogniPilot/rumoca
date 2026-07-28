//! Coverage for the two rank gates that judge a *rewrite* rather than a
//! demotion: row consumption in [`super::state_row_reduction`] and the
//! row-group naming form in [`super::dummy_row_group`].
//!
//! Both are shapes the counting invariants cannot see. Consumption replaces one
//! row with its own time derivative, so rows and columns are untouched and only
//! a matching notices that the row stopped determining what it determined. The
//! row-group form appends a row and moves a state, so its counts balance by
//! construction whether or not the appended row had a column to land on.
//!
//! The gates therefore act on the scalar-width rank witness, and only on a
//! *rise* in it. What these tests pin down is that direction and the scoping
//! that keeps each gate off the vector-equation rows it cannot read.

use super::demotion_rank_check::{consumption_is_rank_justified, row_group_is_rank_justified};
use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("rank_gate_test.mo"),
        1,
        2,
    )
}

fn algebraic(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn literal(value: i64) -> Expression {
    Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: test_span(),
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn row(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(),
        origin: "top-level model equation".to_string(),
        scalar_count: 1,
    }
}

/// Two algebraics determined by two rows: `a - b = 0` and `b - 1 = 0`.
///
/// The witness matches both rows, so the deficiency reads zero and any rewrite
/// that strands a column is visible as a rise.
fn determined_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["a", "b"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), algebraic(name));
    }
    dae.continuous.equations = vec![row(sub(var("a"), var("b"))), row(sub(var("b"), literal(1)))];
    dae
}

/// [`determined_dae`] with row 0 rewritten so it no longer reads `a`.
///
/// This is the consumption shape in miniature: the row that was `a`'s only row
/// now reads only `b`, which row 1 already determines, so `a` is left as a
/// column with nothing to match it and the witness rises from 0 to 1. The
/// literal differs from row 1's so the distinct-row filter keeps both.
fn staged_orphaning_dae() -> Dae {
    let mut dae = determined_dae();
    dae.continuous.equations[0].rhs = sub(var("b"), literal(2));
    dae
}

#[test]
fn a_consumption_that_orphans_a_column_is_refused() {
    let staged = staged_orphaning_dae();
    assert!(
        !consumption_is_rank_justified(Some(0), &staged, &VarName::new("q"), 0),
        "a rewrite that leaves `a` with no row must be refused"
    );
}

#[test]
fn a_consumption_that_leaves_the_witness_level_is_accepted() {
    let staged = determined_dae();
    assert!(
        consumption_is_rank_justified(Some(0), &staged, &VarName::new("q"), 0),
        "only a rise is acted on; a level reading is left to the matcher"
    );
}

/// The gate reads a single row, so it needs that row to stand for a single
/// scalar row. A wider row's columns are the union over its whole family and a
/// change in the union says nothing about any one of them — which is what keeps
/// every vector-equation model out of the gate's reach.
#[test]
fn a_consumption_of_a_wider_row_is_left_to_the_matcher() {
    let mut staged = staged_orphaning_dae();
    staged.continuous.equations[0].scalar_count = 3;
    assert!(
        consumption_is_rank_justified(Some(0), &staged, &VarName::new("q"), 0),
        "a row standing for three scalar rows is outside what this witness reads"
    );
}

#[test]
fn a_consumption_with_no_reading_is_accepted() {
    let staged = staged_orphaning_dae();
    assert!(
        consumption_is_rank_justified(None, &staged, &VarName::new("q"), 0),
        "a model that admits no witness is left exactly as it was before this gate"
    );
}

#[test]
fn a_row_group_that_orphans_a_column_is_refused() {
    let before = determined_dae();
    let staged = staged_orphaning_dae();
    assert!(
        !row_group_is_rank_justified(&before, &staged, &VarName::new("q"), 1),
        "funding a scalar state with a row the system cannot place must be refused"
    );
}

#[test]
fn a_row_group_funding_a_wider_state_is_left_to_the_matcher() {
    let before = determined_dae();
    let staged = staged_orphaning_dae();
    assert!(
        row_group_is_rank_justified(&before, &staged, &VarName::new("q"), 3),
        "a state wider than one scalar is funded by a family the witness reads as an aggregate"
    );
}

#[test]
fn a_row_group_that_leaves_the_witness_level_is_accepted() {
    let before = determined_dae();
    let staged = determined_dae();
    assert!(
        row_group_is_rank_justified(&before, &staged, &VarName::new("q"), 1),
        "the naming form exists to avoid consumption; only a rise is refused"
    );
}
