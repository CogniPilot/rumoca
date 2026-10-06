//! Unit controls for guard facts: relation polarity, Boolean locals, literal
//! Real facts, joins and invalidation by a later write.

use super::super::loop_compaction::preservation_corpus::{
    assign, binary, integer, real, span, var,
};
use super::*;

fn names(list: &[&str]) -> HashSet<VarName> {
    list.iter().copied().map(VarName::new).collect()
}

struct Names {
    integers: HashSet<VarName>,
    reals: HashSet<VarName>,
    shapes: ShapeEnvironment,
}

impl Names {
    fn new() -> Self {
        Self {
            integers: names(&["r", "n"]),
            reals: names(&["accepted"]),
            shapes: ShapeEnvironment::default(),
        }
    }

    fn scope(&self) -> FactScope<'_> {
        FactScope {
            shapes: &self.shapes,
            integers: &self.integers,
            reals: &self.reals,
        }
    }
}

fn bound(facts: &GuardFacts, name: &str) -> IntegerInterval {
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
    let names = Names::new();
    let facts = GuardFacts::entry();
    let condition = binary(OpBinary::Gt, var("r"), integer(4));
    let entries = facts.branch_entries(&[&condition], names.scope());
    assert_eq!(bound(&entries[0], "r").lower, Some(5));
    assert_eq!(bound(&entries[1], "r").upper, Some(4));
    assert_eq!(bound(&entries[1], "r").lower, None);
}

#[test]
fn a_boolean_local_carries_the_facts_of_its_value_and_path() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        names.scope(),
    );
    let entries = facts.branch_entries(&[&var("b")], names.scope());
    assert_eq!(bound(&entries[0], "r").upper, Some(4));
    assert_eq!(bound(&entries[1], "r").lower, Some(5));
    // `not b` selects the false side.
    let entries = facts.branch_entries(&[&not(var("b"))], names.scope());
    assert_eq!(bound(&entries[0], "r").lower, Some(5));
}

#[test]
fn a_write_forgets_every_fact_that_mentions_it() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        names.scope(),
    );
    facts.after(&assign("r", var("n")), names.scope());
    let entries = facts.branch_entries(&[&var("b")], names.scope());
    assert!(bound(&entries[0], "r").is_unbounded());
}

#[test]
fn a_join_keeps_only_what_every_path_proves() {
    let names = Names::new();
    let facts = GuardFacts::entry();
    let low = binary(OpBinary::Le, var("r"), integer(2));
    let entries = facts.branch_entries(&[&low], names.scope());
    // r <= 2 on one path, r >= 3 on the other: nothing bounds r after both.
    assert!(bound(&GuardFacts::join(&entries), "r").is_unbounded());
    let within = binary(
        OpBinary::And,
        binary(OpBinary::Ge, var("r"), integer(0)),
        binary(OpBinary::Le, var("r"), integer(3)),
    );
    let entries = facts.branch_entries(&[&within], names.scope());
    assert_eq!(bound(&entries[0], "r"), IntegerInterval::finite(0, 3));
    // The false side of a conjunction is a disjunction: no single bound.
    assert!(bound(&entries[1], "r").is_unbounded());
}

#[test]
fn a_literal_real_fact_contradicts_a_branch_that_needs_another_value() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(&assign("accepted", real(0.0)), names.scope());
    let differs = binary(OpBinary::Neq, var("accepted"), real(1.0));
    // `accepted <> 1` holds; its false side needs `accepted == 1`.
    let entries = facts.branch_entries(&[&differs], names.scope());
    assert!(!entries[0].is_unreachable());
    assert!(entries[1].is_unreachable());
    // A computed value carries no fact, so nothing is contradicted.
    facts.after(&assign("accepted", var("n")), names.scope());
    let entries = facts.branch_entries(&[&differs], names.scope());
    assert!(!entries[1].is_unreachable());
}

#[test]
fn an_unreachable_path_adds_nothing_to_a_join() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(&assign("r", integer(0)), names.scope());
    let impossible = binary(OpBinary::Eq, var("r"), integer(1));
    let entries = facts.branch_entries(&[&impossible], names.scope());
    assert!(entries[0].is_unreachable());
    assert_eq!(
        bound(&GuardFacts::join(&entries), "r"),
        IntegerInterval::exact(0)
    );
}
