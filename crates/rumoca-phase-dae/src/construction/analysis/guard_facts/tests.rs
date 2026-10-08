//! Unit controls for guard facts: relation polarity, Boolean locals, literal
//! Real facts, joins and invalidation by a later write.

use super::super::loop_compaction::preservation_corpus::{
    assign, assign_element, binary, branch, element, for_loop, integer, real, span, var,
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
            integers: names(&["r", "n", "count", "idx", "w", "h"]),
            reals: names(&["accepted", "valid", "settings"]),
            shapes: {
                let mut shapes = ShapeEnvironment::default();
                shapes.insert(VarName::new("idx"), vec![5]);
                shapes
            },
        }
    }

    fn scope(&self) -> FactScope<'_> {
        FactScope {
            shapes: &self.shapes,
            integers: &self.integers,
            reals: &self.reals,
            generated: &[],
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

fn if_expression(condition: Expression, then: Expression, otherwise: Expression) -> Expression {
    Expression::If {
        branches: vec![(condition, then)],
        else_branch: Box::new(otherwise),
        span: span(),
    }
}

fn integer_call(value: Expression) -> Expression {
    Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Integer,
        args: vec![value],
        span: span(),
    }
}

#[test]
fn a_real_indicator_carries_the_facts_of_the_condition_that_selects_it() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    let stride = element("settings", integer(1));
    let condition = binary(OpBinary::Ge, stride.clone(), real(1.0));
    facts.after(
        &assign("valid", if_expression(condition, real(1.0), real(0.0))),
        names.scope(),
    );
    // `r` is the stride on the accepted path and 1 otherwise.
    let positive = binary(OpBinary::Gt, var("valid"), real(0.0));
    facts.after(
        &assign(
            "r",
            if_expression(positive.clone(), integer_call(stride), integer(1)),
        ),
        names.scope(),
    );
    assert_eq!(bound(&facts, "r").lower, Some(1));
    let entries = facts.branch_entries(&[&positive], names.scope());
    // `valid > 0.0` admits only the 1.0 arm; its false side only the 0.0 arm.
    assert!(!entries[0].is_unreachable());
    let rejected = binary(OpBinary::Eq, var("valid"), real(1.0));
    assert!(
        entries[1]
            .assuming(&rejected, true, names.scope())
            .is_unreachable()
    );
}

#[test]
fn a_failed_ordered_comparison_proves_no_real_fact() {
    // `settings[1] >= 1.0` fails for a NaN as well, so its false side bounds
    // nothing: integer(settings[1]) stays unbounded there.
    let names = Names::new();
    let facts = GuardFacts::entry();
    let condition = binary(OpBinary::Ge, element("settings", integer(1)), real(1.0));
    let entries = facts.branch_entries(&[&condition], names.scope());
    let converted = integer_call(element("settings", integer(1)));
    assert_eq!(
        entries[0].integer_interval(&converted, names.scope()).lower,
        Some(1)
    );
    assert!(
        entries[1]
            .integer_interval(&converted, names.scope())
            .is_unbounded()
    );
}

#[test]
fn a_completed_element_write_bounds_its_index_by_the_extent() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(&assign_element("idx", var("r"), integer(0)), names.scope());
    assert_eq!(bound(&facts, "r"), IntegerInterval::finite(1, 5));
}

#[test]
fn a_loop_head_keeps_a_counter_bounded_by_the_array_it_indexes() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(&assign("count", integer(0)), names.scope());
    let counted = vec![
        assign("count", binary(OpBinary::Add, var("count"), integer(1))),
        assign_element("idx", var("count"), integer(1)),
    ];
    let body = vec![branch(
        vec![(binary(OpBinary::Gt, var("n"), integer(0)), counted)],
        None,
    )];
    facts.after(&for_loop("i", 9, body), names.scope());
    // Widening steps to the extent 5 and the head is proven there.
    assert_eq!(bound(&facts, "count"), IntegerInterval::finite(0, 5));
}

#[test]
fn a_bounded_product_bounds_each_factor_proven_at_least_one() {
    let names = Names::new();
    let facts = GuardFacts::entry();
    let positive = binary(
        OpBinary::And,
        binary(OpBinary::Gt, var("w"), integer(0)),
        binary(OpBinary::Gt, var("h"), integer(0)),
    );
    let condition = binary(
        OpBinary::And,
        positive,
        binary(
            OpBinary::Eq,
            integer(12),
            binary(OpBinary::Mul, var("w"), var("h")),
        ),
    );
    let entry = facts.assuming(&condition, true, names.scope());
    assert_eq!(bound(&entry, "w"), IntegerInterval::finite(1, 12));
    assert_eq!(bound(&entry, "h"), IntegerInterval::finite(1, 12));
}

#[test]
fn a_selection_over_its_own_old_value_keeps_no_fact_of_it() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    let high = binary(OpBinary::Gt, var("r"), integer(5));
    facts.after(
        &assign("r", if_expression(high, integer(1), integer(2))),
        names.scope(),
    );
    // The 1 arm was selected by the old `r > 5`; that fact says nothing
    // about the new `r`, so `r <> 1` and `r == 1` both stay reachable.
    let differs = binary(OpBinary::Neq, var("r"), integer(1));
    let entries = facts.branch_entries(&[&differs], names.scope());
    assert!(!entries[0].is_unreachable());
    assert!(!entries[1].is_unreachable());
    assert_eq!(bound(&facts, "r"), IntegerInterval::finite(1, 2));
}

fn while_loop(cond: Expression, stmts: Vec<rumoca_core::Statement>) -> rumoca_core::Statement {
    rumoca_core::Statement::While {
        block: rumoca_core::StatementBlock { cond, stmts },
        span: span(),
    }
}

/// A normal exit evaluated the condition false; a `break` leaves with it
/// either way, so a loop that can break proves no exit fact from it.
#[test]
fn only_a_loop_without_break_exits_with_its_condition_false() {
    let names = Names::new();
    let below = binary(OpBinary::Lt, var("r"), integer(3));
    let step = assign("r", binary(OpBinary::Add, var("r"), integer(1)));
    let mut facts = GuardFacts::entry();
    facts.after(&assign("r", integer(0)), names.scope());
    facts.after(
        &while_loop(below.clone(), vec![step.clone()]),
        names.scope(),
    );
    assert_eq!(bound(&facts, "r").lower, Some(3));

    let leave = branch(
        vec![(
            binary(OpBinary::Eq, var("n"), integer(0)),
            vec![rumoca_core::Statement::Break { span: span() }],
        )],
        None,
    );
    let mut facts = GuardFacts::entry();
    facts.after(&assign("r", integer(0)), names.scope());
    facts.after(&while_loop(below, vec![leave, step]), names.scope());
    assert_eq!(bound(&facts, "r").lower, None);
}

#[test]
fn a_conjunction_local_proves_its_operands_where_it_holds() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("v", binary(OpBinary::Gt, var("r"), integer(4))),
        names.scope(),
    );
    facts.after(
        &assign("g", binary(OpBinary::And, var("v"), var("other"))),
        names.scope(),
    );
    let entries = facts.branch_entries(&[&var("g")], names.scope());
    assert_eq!(bound(&entries[0], "r").lower, Some(5));
    // `v` is true wherever `g` is, so the path that selects `g` cannot take `not v`.
    assert!(
        entries[0]
            .assuming(&var("v"), false, names.scope())
            .is_unreachable()
    );
    // `g` false proves neither operand.
    assert!(
        !entries[1]
            .assuming(&var("v"), false, names.scope())
            .is_unreachable()
    );
}

#[test]
fn a_write_forgets_what_a_conjunction_local_proved_of_the_value_it_names() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("v", binary(OpBinary::Gt, var("r"), integer(4))),
        names.scope(),
    );
    facts.after(
        &assign("g", binary(OpBinary::And, var("v"), var("other"))),
        names.scope(),
    );
    facts.after(
        &assign("v", binary(OpBinary::Lt, var("n"), integer(0))),
        names.scope(),
    );
    let entries = facts.branch_entries(&[&var("g")], names.scope());
    // What `v` meant about `r` when `g` was assigned still holds, `r` being unwritten.
    assert_eq!(bound(&entries[0], "r").lower, Some(5));
    // The new `v` is no operand of `g`, so `g` no longer excludes `not v`.
    assert!(
        !entries[0]
            .assuming(&var("v"), false, names.scope())
            .is_unreachable()
    );
}

#[test]
fn the_paths_leaving_a_value_undefined_stay_apart() {
    let names = Names::new();
    let scope = names.scope();
    let mut decided = GuardFacts::entry();
    decided.after(
        &assign("a", binary(OpBinary::Gt, var("r"), integer(4))),
        scope,
    );
    let decided = decided.assuming(&var("a"), false, scope);
    let mut paths = PathSet::single(decided);
    paths.join_path(&PathSet::single(GuardFacts::entry().assuming(
        &var("b"),
        true,
        scope,
    )));
    // One path knows `a` is false, the other never saw `a`: a later `a`
    // true contradicts only the first, so the set stays reachable.
    let after_a = paths.entering(&[&var("a")], 0, scope);
    assert!(!after_a.is_unreachable());
    // `a` and `not b` together contradict both.
    let mut both = paths.entering(&[&var("a")], 0, scope);
    both = both.entering(&[&var("b")], 1, scope);
    assert!(both.is_unreachable());
}

#[test]
fn a_loop_branch_the_path_excludes_writes_nothing_on_that_path() {
    let names = Names::new();
    let scope = names.scope();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("v", binary(OpBinary::Gt, var("r"), integer(4))),
        scope,
    );
    facts.after(&assign("w", integer(3)), scope);
    let facts = facts.assuming(&var("v"), false, scope);
    let body = vec![branch(
        vec![(var("v"), vec![assign("w", integer(9))])],
        None,
    )];
    // `v` is false here and the loop never writes it, so `w` keeps its value.
    let head = facts.loop_entry_on_path(&body, &[], scope);
    assert_eq!(bound(&head, "w"), IntegerInterval::exact(3));
    // Where `v` is open the branch may run, and `w` is forgotten.
    let mut open = GuardFacts::entry();
    open.after(&assign("w", integer(3)), scope);
    assert_eq!(
        bound(&open.loop_entry_on_path(&body, &[], scope), "w").lower,
        None
    );
}

#[test]
fn a_loop_branch_under_a_condition_the_loop_writes_may_still_run() {
    let names = Names::new();
    let scope = names.scope();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("v", binary(OpBinary::Gt, var("r"), integer(4))),
        scope,
    );
    facts.after(&assign("w", integer(3)), scope);
    let facts = facts.assuming(&var("v"), false, scope);
    let body = vec![
        branch(vec![(var("v"), vec![assign("w", integer(9))])], None),
        assign("v", binary(OpBinary::Lt, var("n"), integer(0))),
    ];
    let head = facts.loop_entry_on_path(&body, &[], scope);
    assert_eq!(bound(&head, "w").lower, None);
}

#[test]
fn capturing_a_boolean_keeps_the_selection_of_the_captured_value() {
    let names = Names::new();
    let scope = names.scope();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("v", binary(OpBinary::Gt, var("r"), integer(4))),
        scope,
    );
    let mut facts = facts.assuming(&var("v"), false, scope);
    facts.after(&assign("g", var("v")), scope);
    // `g` holds `v`, which is false here: both are excluded from `true`.
    assert!(facts.assuming(&var("g"), true, scope).is_unreachable());
    assert!(facts.assuming(&var("v"), true, scope).is_unreachable());
}

#[test]
fn a_guarded_if_expression_proves_its_condition_where_it_holds() {
    let names = Names::new();
    let scope = names.scope();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("a", binary(OpBinary::Gt, var("r"), integer(4))),
        scope,
    );
    // `if a then b else false` is `a and b`.
    let conjunction = Expression::If {
        branches: vec![(var("a"), var("b"))],
        else_branch: Box::new(Expression::Literal {
            value: Literal::Boolean(false),
            span: span(),
        }),
        span: span(),
    };
    let held = facts.assuming(&conjunction, true, scope);
    assert_eq!(bound(&held, "r").lower, Some(5));
    assert!(held.assuming(&var("a"), false, scope).is_unreachable());
    // Its falsity proves neither operand.
    let failed = facts.assuming(&conjunction, false, scope);
    assert!(!failed.assuming(&var("a"), false, scope).is_unreachable());
}

#[test]
fn a_captured_boolean_holds_its_definition_after_its_statement() {
    let names = Names::new();
    let scope = names.scope();
    let definition = crate::construction::analysis::function_returns::GeneratedBooleanDefinition {
        target: VarName::new("g"),
        value: binary(OpBinary::Gt, var("r"), integer(4)),
        span: span(),
    };
    let generated = [definition];
    let scope = FactScope {
        generated: &generated,
        ..scope
    };
    let mut facts = GuardFacts::entry();
    facts.after(&rumoca_core::Statement::Empty { span: span() }, scope);
    let held = facts.assuming(&var("g"), true, scope);
    assert_eq!(bound(&held, "r").lower, Some(5));
}
