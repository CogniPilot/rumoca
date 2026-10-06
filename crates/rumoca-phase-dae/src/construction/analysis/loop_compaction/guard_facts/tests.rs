//! Unit controls for guard facts: relation polarity, Boolean locals, joins
//! and invalidation by a later write.

use super::super::preservation_corpus::{assign, binary, integer, span, var};
use super::*;

fn integers() -> HashSet<VarName> {
    ["r", "n"].into_iter().map(VarName::new).collect()
}

fn bound(facts: &GuardFacts<'_>, name: &str) -> IntegerInterval {
    let mut shapes = ShapeEnvironment::default();
    facts.refine(&mut shapes);
    shapes.proven_integer_interval(&var(name))
}

fn not(expression: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(expression),
        span: span(),
    }
}

#[test]
fn a_relation_bounds_its_scalar_on_each_branch() {
    let integers = integers();
    let facts = GuardFacts::entry(&integers);
    let condition = binary(OpBinary::Gt, var("r"), integer(4));
    let entries = facts.branch_entries(&[&condition], &ShapeEnvironment::default());
    assert_eq!(bound(&entries[0], "r").lower, Some(5));
    assert_eq!(bound(&entries[1], "r").upper, Some(4));
    assert_eq!(bound(&entries[1], "r").lower, None);
}

#[test]
fn a_boolean_local_carries_the_facts_of_its_value_and_path() {
    let integers = integers();
    let shapes = ShapeEnvironment::default();
    let mut facts = GuardFacts::entry(&integers);
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        &shapes,
    );
    let entries = facts.branch_entries(&[&var("b")], &shapes);
    assert_eq!(bound(&entries[0], "r").upper, Some(4));
    assert_eq!(bound(&entries[1], "r").lower, Some(5));
    // `not b` selects the false side.
    let entries = facts.branch_entries(&[&not(var("b"))], &shapes);
    assert_eq!(bound(&entries[0], "r").lower, Some(5));
}

#[test]
fn a_write_forgets_every_fact_that_mentions_it() {
    let integers = integers();
    let shapes = ShapeEnvironment::default();
    let mut facts = GuardFacts::entry(&integers);
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        &shapes,
    );
    facts.after(&assign("r", var("n")), &shapes);
    let entries = facts.branch_entries(&[&var("b")], &shapes);
    assert!(bound(&entries[0], "r").is_unbounded());
}

#[test]
fn a_join_keeps_only_what_every_path_proves() {
    let integers = integers();
    let shapes = ShapeEnvironment::default();
    let facts = GuardFacts::entry(&integers);
    let low = binary(OpBinary::Le, var("r"), integer(2));
    let entries = facts.branch_entries(&[&low], &shapes);
    // r <= 2 on one path, r >= 3 on the other: nothing bounds r after both.
    assert!(bound(&GuardFacts::join(&entries), "r").is_unbounded());
    let within = binary(
        OpBinary::And,
        binary(OpBinary::Ge, var("r"), integer(0)),
        binary(OpBinary::Le, var("r"), integer(3)),
    );
    let entries = facts.branch_entries(&[&within], &shapes);
    assert_eq!(bound(&entries[0], "r"), IntegerInterval::finite(0, 3));
    // The false side of a conjunction is a disjunction: no single bound.
    assert!(bound(&entries[1], "r").is_unbounded());
}
