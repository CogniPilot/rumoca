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
fn a_write_forgets_only_the_facts_about_its_own_name() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    for (target, literal) in [("n", 3), ("r", 4), ("w", 5)] {
        facts.after(&assign(target, integer(literal)), names.scope());
    }
    let shared = facts.clone();
    facts.after(&assign("r", var("count")), names.scope());
    assert!(bound(&facts, "r").is_unbounded());
    assert_eq!(bound(&facts, "n"), IntegerInterval::finite(3, 3));
    assert_eq!(bound(&facts, "w"), IntegerInterval::finite(5, 5));
    // The fork that did not see the write keeps its fact.
    assert_eq!(bound(&shared, "r"), IntegerInterval::finite(4, 4));
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
        mutable: false,
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

#[test]
fn a_false_capture_proves_the_operand_left_when_the_other_is_known() {
    let names = Names::new();
    let scope = names.scope();
    let conjunction = |lhs, rhs| Expression::If {
        branches: vec![(lhs, rhs)],
        else_branch: Box::new(Expression::Literal {
            value: Literal::Boolean(false),
            span: span(),
        }),
        span: span(),
    };
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("a", binary(OpBinary::Gt, var("r"), integer(4))),
        scope,
    );
    facts.after(
        &assign("b", binary(OpBinary::Gt, var("n"), integer(2))),
        scope,
    );
    // `a` is known true: a false `g := a and b` proves `b` false.
    let mut known = facts.assuming(&var("a"), true, scope);
    known.after(&assign("g", conjunction(var("a"), var("b"))), scope);
    let failed = known.assuming(&var("g"), false, scope);
    assert!(failed.assuming(&var("b"), true, scope).is_unreachable());
    assert!(!failed.assuming(&var("b"), false, scope).is_unreachable());
    // `a` open: a false `g` proves neither operand.
    let mut open = facts.clone();
    open.after(&assign("g", conjunction(var("a"), var("b"))), scope);
    let failed = open.assuming(&var("g"), false, scope);
    assert!(!failed.assuming(&var("b"), true, scope).is_unreachable());
    // `a` known false: a true `q := a or b` proves `b` true.
    let mut known = facts.assuming(&var("a"), false, scope);
    known.after(
        &assign("q", binary(OpBinary::Or, var("a"), var("b"))),
        scope,
    );
    let held = known.assuming(&var("q"), true, scope);
    assert!(
        held.assuming_boolean(&VarName::new("b"), false)
            .is_unreachable()
    );
}

#[test]
fn a_chain_proves_only_what_every_branch_proves() {
    let names = Names::new();
    let scope = names.scope();
    let chain = |first: &str, second: &str| Expression::If {
        branches: vec![(var("a"), var(first)), (var("c"), var(second))],
        else_branch: Box::new(Expression::Literal {
            value: Literal::Boolean(false),
            span: span(),
        }),
        span: span(),
    };
    let held = |value: Expression, name: &str| {
        let mut facts = GuardFacts::entry();
        facts.after(&assign("g", value), scope);
        facts
            .assuming(&var("g"), true, scope)
            .assuming_boolean(&VarName::new(name), false)
            .is_unreachable()
    };
    // Both branches prove `b`.
    assert!(held(chain("b", "b"), "b"));
    // `if a then b elseif c then d else false` is `a and b` or
    // `not a and c and d`: it proves neither `b` nor `d`.
    assert!(!held(chain("b", "d"), "b"));
    assert!(!held(chain("b", "d"), "d"));
    // The first branch's condition is no fact of the chain.
    assert!(!held(chain("b", "b"), "a"));
}

#[test]
fn a_mutable_generated_store_invalidates_a_loop_entry_selection() {
    let names = Names::new();
    let generated = [function_returns::GeneratedBooleanDefinition {
        mutable: true,
        target: VarName::new("g"),
        value: Expression::Literal {
            value: Literal::Boolean(true),
            span: span(),
        },
        span: span(),
    }];
    let scope = FactScope {
        generated: &generated,
        ..names.scope()
    };
    let entry = GuardFacts::entry().assuming(&var("g"), false, scope);
    let body = [branch(
        vec![(
            not(var("g")),
            vec![rumoca_core::Statement::Empty { span: span() }],
        )],
        None,
    )];
    let head = entry.loop_entry_on_path(&body, &[], scope);
    assert!(!head.assuming(&var("g"), true, scope).is_unreachable());
    assert!(!head.assuming(&var("g"), false, scope).is_unreachable());
}

fn boolean(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: span(),
    }
}

fn activation(condition: Expression, selected: Expression) -> Expression {
    Expression::If {
        branches: vec![(condition, selected)],
        else_branch: Box::new(boolean(false)),
        span: span(),
    }
}

#[test]
fn complementary_guarded_definitions_remove_only_the_active_undefined_paths() {
    let names = Names::new();
    let scope = names.scope();
    let mut original = PathSet::single(GuardFacts::entry());
    original.assign(
        &VarName::new("selected"),
        &activation(var("active"), var("choice")),
        scope,
    );
    for else_prefix in [
        activation(var("active"), not(var("selected"))),
        binary(OpBinary::And, var("active"), not(var("selected"))),
    ] {
        // The else prefix writes the value where it runs. The tuple writes
        // it where the captured selection holds. Keep only their skips.
        let after_else = original.entering(&[&else_prefix], 1, scope);
        let after_tuple = after_else.entering(&[&var("selected")], 1, scope);
        assert!(
            after_tuple
                .entering(&[&var("active")], 0, scope)
                .is_unreachable()
        );
        assert!(
            !after_tuple
                .entering(&[&var("active")], 1, scope)
                .is_unreachable()
        );
        // Without the tuple arm, a reached active path is still undefined.
        assert!(
            !after_else
                .entering(&[&var("active")], 0, scope)
                .is_unreachable()
        );
    }
}

#[test]
fn boolean_junction_alternatives_preserve_each_possible_path() {
    let names = Names::new();
    let scope = names.scope();
    let original = PathSet::single(GuardFacts::entry());
    let conjunction = binary(OpBinary::And, var("a"), var("b"));
    let failed = original.entering(&[&conjunction], 1, scope);
    assert!(
        failed
            .entering(&[&var("a")], 0, scope)
            .entering(&[&var("b")], 0, scope)
            .is_unreachable()
    );
    assert!(
        !failed
            .entering(&[&var("a")], 1, scope)
            .entering(&[&var("b")], 0, scope)
            .is_unreachable()
    );
    let disjunction = binary(OpBinary::Or, var("a"), var("b"));
    let held = original.entering(&[&disjunction], 0, scope);
    assert!(
        held.entering(&[&var("a")], 1, scope)
            .entering(&[&var("b")], 1, scope)
            .is_unreachable()
    );
    assert!(
        !held
            .entering(&[&var("a")], 1, scope)
            .entering(&[&var("b")], 0, scope)
            .is_unreachable()
    );
}

#[test]
fn boolean_conditional_alternatives_keep_first_true_selection() {
    let names = Names::new();
    let scope = names.scope();
    let condition = Expression::If {
        branches: vec![(var("a"), boolean(false)), (var("b"), boolean(true))],
        else_branch: Box::new(boolean(false)),
        span: span(),
    };
    let held = PathSet::single(GuardFacts::entry()).entering(&[&condition], 0, scope);
    assert!(!held.is_unreachable());
    assert!(held.entering(&[&var("a")], 0, scope).is_unreachable());
    assert!(held.entering(&[&var("b")], 1, scope).is_unreachable());
}

#[test]
fn a_mutable_write_does_not_reuse_the_previous_capture_path() {
    let names = Names::new();
    let scope = names.scope();
    let mut paths = PathSet::single(GuardFacts::entry());
    paths.assign(&VarName::new("selected"), &var("choice"), scope);
    let mut undefined = paths.entering(&[&var("selected")], 1, scope);
    undefined.assign(&VarName::new("choice"), &boolean(true), scope);
    assert!(
        !undefined
            .entering(&[&var("choice")], 0, scope)
            .is_unreachable()
    );
    assert!(
        undefined
            .entering(&[&var("selected")], 0, scope)
            .is_unreachable()
    );
}

#[test]
fn bounded_boolean_alternatives_collapse_only_to_a_conservative_hull() {
    let names = Names::new();
    let scope = names.scope();
    let condition = (0..32).fold(boolean(true), |earlier, index| {
        binary(OpBinary::And, earlier, var(&format!("choice_{index}")))
    });
    let original = PathSet::single(GuardFacts::entry());
    let failed = original.entering(&[&condition], 1, scope);
    // More alternatives than the owner retains lose proof, never gain it.
    assert!(!failed.entering(&[&condition], 0, scope).is_unreachable());
}

#[test]
fn a_false_captured_conjunction_retains_each_skipped_producer_path() {
    let names = Names::new();
    let capture = function_returns::GeneratedBooleanDefinition {
        mutable: false,
        target: VarName::new("selected"),
        value: activation(var("active"), var("choice")),
        span: span(),
    };
    let captures = [capture];
    let scope = FactScope {
        generated: &captures,
        ..names.scope()
    };
    let mut paths = PathSet::single(GuardFacts::entry());
    paths.after(&rumoca_core::Statement::Empty { span: span() }, scope);
    let mut undefined = paths.entering(&[&var("selected")], 1, scope);
    undefined.after(
        &branch(
            vec![(var("selected"), vec![assign("choice", var("next"))])],
            None,
        ),
        scope,
    );
    // A replacement only in the producer's selected branch cannot re-enable
    // an undefined path, even though the mutable choice is no longer its old value.
    assert!(
        undefined
            .entering(&[&activation(var("active"), var("choice"))], 0, scope)
            .is_unreachable()
    );
    assert!(
        !undefined
            .entering(&[&var("active")], 1, scope)
            .is_unreachable()
    );
}

#[test]
fn a_true_captured_disjunction_retains_each_skipped_producer_path() {
    let names = Names::new();
    let captures = [function_returns::GeneratedBooleanDefinition {
        mutable: false,
        target: VarName::new("selected"),
        value: binary(OpBinary::Or, var("active"), var("choice")),
        span: span(),
    }];
    let scope = FactScope {
        generated: &captures,
        ..names.scope()
    };
    let mut paths = PathSet::single(GuardFacts::entry());
    paths.after(&rumoca_core::Statement::Empty { span: span() }, scope);
    let mut undefined = paths.entering(&[&var("selected")], 0, scope);
    undefined.after(
        &branch(
            vec![(not(var("selected")), vec![assign("choice", var("next"))])],
            None,
        ),
        scope,
    );
    assert!(
        undefined
            .entering(&[&not(var("active"))], 0, scope)
            .entering(&[&not(var("choice"))], 0, scope)
            .is_unreachable()
    );
    assert!(
        !undefined
            .entering(&[&var("active")], 0, scope)
            .is_unreachable()
    );
}

#[test]
fn an_unconditional_reenable_does_not_inherit_a_capture_producer() {
    let names = Names::new();
    let captures = [function_returns::GeneratedBooleanDefinition {
        mutable: false,
        target: VarName::new("selected"),
        value: activation(var("active"), var("choice")),
        span: span(),
    }];
    let scope = FactScope {
        generated: &captures,
        ..names.scope()
    };
    let mut paths = PathSet::single(GuardFacts::entry());
    paths.after(&rumoca_core::Statement::Empty { span: span() }, scope);
    let mut undefined = paths.entering(&[&var("selected")], 1, scope);
    undefined.after(&assign("choice", boolean(true)), scope);
    assert!(
        !undefined
            .entering(&[&activation(var("active"), var("choice"))], 0, scope)
            .is_unreachable()
    );
    assert!(
        undefined
            .entering(&[&var("selected")], 0, scope)
            .is_unreachable()
    );
}

fn sixteen_independent_boolean_paths(scope: FactScope<'_>) -> PathSet {
    let mut paths = PathSet::empty();
    for bits in 0..16 {
        let mut facts = GuardFacts::entry();
        for bit in 0..4 {
            facts.assign(
                VarName::new(format!("varying_{bit}")),
                &boolean(bits & (1 << bit) != 0),
                scope,
            );
        }
        paths.join_path(&PathSet::single(facts));
    }
    paths
}

#[test]
fn a_capture_exclusion_survives_more_than_sixteen_boolean_alternatives() {
    let names = Names::new();
    for conjunction in [true, false] {
        let value = if conjunction {
            activation(var("active"), var("choice"))
        } else {
            binary(OpBinary::Or, var("active"), var("choice"))
        };
        let captures = [function_returns::GeneratedBooleanDefinition {
            mutable: false,
            target: VarName::new("selected"),
            value,
            span: span(),
        }];
        let scope = FactScope {
            generated: &captures,
            ..names.scope()
        };
        let mut paths = sixteen_independent_boolean_paths(scope);
        // Sixteen existing paths each have three Boolean RHS alternatives,
        // so capture issuance necessarily uses the bounded owner's hull.
        paths.after(&rumoca_core::Statement::Empty { span: span() }, scope);
        let skipped = paths.entering(&[&var("selected")], usize::from(conjunction), scope);
        let impossible = skipped
            .entering(&[&var("active")], usize::from(!conjunction), scope)
            .entering(&[&var("choice")], usize::from(!conjunction), scope);
        assert!(impossible.is_unreachable());
        // A path where an operand decides the skipped truth remains possible.
        assert!(
            !skipped
                .entering(&[&var("active")], usize::from(conjunction), scope)
                .is_unreachable()
        );
    }
}

#[test]
fn a_reenabled_operand_invalidates_the_capture_exclusion_after_a_hull() {
    let names = Names::new();
    let captures = [function_returns::GeneratedBooleanDefinition {
        mutable: false,
        target: VarName::new("selected"),
        value: activation(var("active"), var("choice")),
        span: span(),
    }];
    let scope = FactScope {
        generated: &captures,
        ..names.scope()
    };
    let mut paths = sixteen_independent_boolean_paths(scope);
    paths.after(&rumoca_core::Statement::Empty { span: span() }, scope);
    let mut skipped = paths.entering(&[&var("selected")], 1, scope);
    skipped.assign(&VarName::new("choice"), &boolean(true), scope);
    assert!(
        !skipped
            .entering(&[&var("active")], 0, scope)
            .entering(&[&var("choice")], 0, scope)
            .is_unreachable()
    );
    assert!(
        skipped
            .entering(&[&var("selected")], 0, scope)
            .is_unreachable()
    );
}

#[test]
fn a_boolean_call_is_not_an_exact_capture_exclusion_cube() {
    let names = Names::new();
    let call = Expression::FunctionCall {
        name: rumoca_core::Reference::new("unknown"),
        args: Vec::new(),
        is_constructor: false,
        span: span(),
    };
    let captures = [function_returns::GeneratedBooleanDefinition {
        mutable: false,
        target: VarName::new("selected"),
        value: activation(var("active"), activation(var("choice"), call)),
        span: span(),
    }];
    let scope = FactScope {
        generated: &captures,
        ..names.scope()
    };
    let mut paths = sixteen_independent_boolean_paths(scope);
    paths.after(&rumoca_core::Statement::Empty { span: span() }, scope);
    let failed = paths.entering(&[&var("selected")], 1, scope);
    // Both operands may hold while the unsupported call returns false.
    assert!(
        !failed
            .entering(&[&var("active")], 0, scope)
            .entering(&[&var("choice")], 0, scope)
            .is_unreachable()
    );
}

#[test]
fn path_sets_holding_equal_facts_advance_to_equal_path_sets() {
    let names = Names::new();
    let mut origin = GuardFacts::entry();
    origin.after(&assign("n", integer(3)), names.scope());
    let fork = origin.clone();
    let rebuilt = {
        let mut facts = GuardFacts::entry();
        facts.after(&assign("n", integer(3)), names.scope());
        facts
    };
    // A fork shares its map; a rebuilt path holds an equal map apart.
    assert_eq!(origin, fork);
    assert_eq!(origin, rebuilt);
    let mut other = origin.clone();
    other.after(&assign("n", integer(4)), names.scope());
    assert_ne!(origin, other);

    let statement = assign("w", var("n"));
    let mut shared = PathSet::single(origin);
    let mut apart = PathSet::single(rebuilt);
    assert_eq!(shared, apart);
    shared.after(&statement, names.scope());
    apart.after(&statement, names.scope());
    assert_eq!(shared, apart);
}

#[test]
fn a_write_to_an_operand_forgets_what_a_selection_proved_only_on_its_own_path() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        names.scope(),
    );
    let fork = facts.clone();
    facts.after(&assign("r", var("n")), names.scope());
    let written = facts.branch_entries(&[&var("b")], names.scope());
    assert!(bound(&written[0], "r").is_unbounded());
    // The fork that did not see the write still knows what `b` proved of `r`.
    let kept = fork.branch_entries(&[&var("b")], names.scope());
    assert_eq!(bound(&kept[0], "r").upper, Some(4));
}

#[test]
fn a_write_to_an_unrelated_name_leaves_the_shared_selections_uncopied() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        names.scope(),
    );
    let fork = facts.clone();
    facts.after(&assign("w", integer(7)), names.scope());
    assert!(std::sync::Arc::ptr_eq(
        &facts.selections.0,
        &fork.selections.0
    ));
    assert_eq!(facts.selections, fork.selections);
}

#[test]
fn clearing_a_shared_map_leaves_the_fork_it_was_shared_with_intact() {
    let mut origin: SharedMap<i32, i32> = SharedMap::new();
    origin.insert(1, 10);
    let fork = origin.clone();
    origin.clear();
    assert!(origin.is_empty());
    assert_eq!(fork.get(&1), Some(&10));
    origin.insert(2, 20);
    assert_eq!(fork.len(), 1);
}

#[test]
fn joining_a_path_with_its_unchanged_fork_keeps_every_fact_and_selection() {
    let names = Names::new();
    let mut facts = GuardFacts::entry();
    facts.after(&assign("n", integer(3)), names.scope());
    facts.after(
        &assign("b", binary(OpBinary::Le, var("r"), integer(4))),
        names.scope(),
    );
    let fork = facts.clone();
    let mut joined = facts.clone();
    joined.join_path(&fork);
    assert_eq!(joined, facts);
    assert_eq!(bound(&joined, "n"), IntegerInterval::finite(3, 3));
    let entries = joined.branch_entries(&[&var("b")], names.scope());
    assert_eq!(bound(&entries[0], "r").upper, Some(4));
    assert_eq!(bound(&entries[1], "r").lower, Some(5));
}
