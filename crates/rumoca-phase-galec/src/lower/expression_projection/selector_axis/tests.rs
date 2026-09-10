//! What the selector-axis collapse accepts, and what it refuses.

use super::{AxisBounds, collapse_selector_axes};
use crate::lower::user_functions::nest_tensor_loops;
use rumoca_core::Span;
use rumoca_ir_galec::ast as gast;

/// A 3-by-4 nest whose selected coordinates are all proven in range.
fn bounds() -> AxisBounds<'static> {
    AxisBounds {
        extents: &[3, 4],
        proven: &|_, _| true,
    }
}

/// The same nest with nothing proven, which is what a caller that cannot bound
/// the selected coordinate hands over.
fn unproven() -> AxisBounds<'static> {
    AxisBounds {
        extents: &[3, 4],
        proven: &|_, _| false,
    }
}

fn collapse(body: &[gast::Spanned<gast::Statement>]) -> Option<super::SelectorCollapse> {
    collapse_selector_axes(body, &iterators(), &bounds())
}

fn local(name: &str) -> gast::Expression {
    gast::Expression::Ref(gast::Reference::local(gast::Name::ident(name)))
}

fn element(name: &str, subscripts: &[&str]) -> gast::Expression {
    gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
        name: gast::Name::ident(name),
        subscripts: subscripts.iter().copied().map(local).collect(),
        span: Span::DUMMY,
    }))
}

fn write(target: gast::Expression, value: gast::Expression) -> gast::Spanned<gast::Statement> {
    let gast::Expression::Ref(target) = target else {
        panic!("a write needs a reference target")
    };
    gast::Spanned::dummy(gast::Statement::Assignment { target, value })
}

fn iterators() -> Vec<gast::Name> {
    vec![gast::Name::ident("row"), gast::Name::ident("column")]
}

fn equals(lhs: gast::Expression, rhs: gast::Expression) -> gast::Expression {
    gast::Expression::binary(gast::BinaryOp::Eq, lhs, rhs)
}

/// The emitted element of `out[i, :] := value`: a guard picking the selected
/// row, whose false arm restores the element the store below writes.
fn guarded_row_store(
    condition: gast::Expression,
    taken: Vec<gast::Spanned<gast::Statement>>,
    restored: gast::Expression,
) -> Vec<gast::Spanned<gast::Statement>> {
    vec![
        gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(condition),
                body: taken,
                span: Span::DUMMY,
            }],
            else_body: Some(vec![write(local("selected"), restored)]),
        })),
        write(element("out", &["row", "column"]), local("selected")),
    ]
}

fn row_selection() -> Vec<gast::Spanned<gast::Statement>> {
    guarded_row_store(
        equals(local("i"), local("row")),
        vec![write(local("selected"), element("value", &["column"]))],
        element("out", &["row", "column"]),
    )
}

fn assignment_target(statement: &gast::Spanned<gast::Statement>) -> &gast::Reference {
    let gast::Statement::Assignment { target, .. } = &statement.node else {
        panic!("expected an assignment")
    };
    target
}

fn subscripts(reference: &gast::Reference) -> &[gast::Expression] {
    let gast::Reference::Local(part) = reference else {
        panic!("expected a local reference")
    };
    &part.subscripts
}

fn loop_iterator(statement: &gast::Spanned<gast::Statement>) -> &str {
    let gast::Statement::For(value) = &statement.node else {
        panic!("expected a loop")
    };
    value
        .iterator
        .as_ref()
        .expect("the nest declares every iterator")
        .lexeme()
}

fn loop_body(statement: &gast::Spanned<gast::Statement>) -> &[gast::Spanned<gast::Statement>] {
    let gast::Statement::For(value) = &statement.node else {
        panic!("expected a loop")
    };
    &value.body
}

#[test]
fn a_row_selecting_axis_is_bound_instead_of_walked() {
    let collapse =
        collapse(&row_selection()).expect("the guard pins the row axis to the selected coordinate");
    assert_eq!(collapse.axes, vec![0], "only the row axis is pinned");
    assert_eq!(
        collapse.body.len(),
        2,
        "the guard is discharged, leaving the taken arm and the store"
    );
    let stored = assignment_target(&collapse.body[1]);
    assert_eq!(
        subscripts(stored),
        &[local("i"), local("column")],
        "the store is redirected to the selected row"
    );
}

#[test]
fn the_collapsed_axis_contributes_no_loop_to_the_nest() {
    let nest = nest_tensor_loops(row_selection(), &iterators(), &bounds(), Span::DUMMY);
    let [outer] = nest.as_slice() else {
        panic!("the nest is one surviving loop")
    };
    assert_eq!(
        loop_iterator(outer),
        "column",
        "the row axis is written directly rather than walked"
    );
    assert_eq!(loop_body(outer).len(), 2);
}

/// The emitted element of `out[:, 16] := column` when the taken arm needed no
/// statements: one store of a conditional whose else value is the element.
fn conditional_column_store(condition: gast::Expression) -> Vec<gast::Spanned<gast::Statement>> {
    vec![write(
        element("out", &["row", "column"]),
        gast::Expression::If(gast::IfExpression::new(
            vec![(condition, element("value", &["row"]))],
            element("out", &["row", "column"]),
        )),
    )]
}

#[test]
fn a_conditional_store_restoring_its_element_collapses_too() {
    let body = conditional_column_store(equals(gast::Expression::Integer(16), local("column")));
    let collapse =
        collapse(&body).expect("the conditional pins the column axis to a literal coordinate");
    assert_eq!(collapse.axes, vec![1], "only the column axis is pinned");
    let [store] = collapse.body.as_slice() else {
        panic!("the conditional is discharged into a plain store")
    };
    assert_eq!(
        subscripts(assignment_target(store)),
        &[local("row"), gast::Expression::Integer(16)],
        "the store is redirected to the selected column"
    );
}

#[test]
fn a_conditional_store_restoring_another_element_keeps_its_loop() {
    let body = vec![write(
        element("out", &["row", "column"]),
        gast::Expression::If(gast::IfExpression::new(
            vec![(
                equals(gast::Expression::Integer(16), local("column")),
                element("value", &["row"]),
            )],
            element("previous", &["row", "column"]),
        )),
    )];
    assert!(
        collapse(&body).is_none(),
        "the rejected iterations write a different array"
    );
}

#[test]
fn a_coordinate_the_caller_cannot_bound_keeps_its_loop() {
    let body = row_selection();
    assert!(
        collapse_selector_axes(&body, &iterators(), &unproven()).is_none(),
        "writing an unbounded coordinate directly could leave the array; the \
         guard form merely never matches"
    );
    let nest = nest_tensor_loops(body, &iterators(), &unproven(), Span::DUMMY);
    let [outer] = nest.as_slice() else {
        panic!("the unproven nest keeps both loops")
    };
    assert_eq!(loop_iterator(outer), "row");
    assert_eq!(loop_iterator(&loop_body(outer)[0]), "column");
}

#[test]
fn a_dense_body_without_a_guard_keeps_every_axis() {
    let dense = vec![write(
        element("out", &["row", "column"]),
        element("value", &["row", "column"]),
    )];
    assert!(
        collapse(&dense).is_none(),
        "a conjugation by a dense operator has no coordinate to pin"
    );
    let nest = nest_tensor_loops(dense, &iterators(), &bounds(), Span::DUMMY);
    let [outer] = nest.as_slice() else {
        panic!("the dense nest is one outer loop")
    };
    assert_eq!(loop_iterator(outer), "row");
    assert_eq!(loop_iterator(&loop_body(outer)[0]), "column");
}

#[test]
fn a_false_arm_restoring_another_element_keeps_its_loop() {
    let body = guarded_row_store(
        equals(local("i"), local("row")),
        vec![write(local("selected"), element("value", &["column"]))],
        element("previous", &["row", "column"]),
    );
    assert!(
        collapse(&body).is_none(),
        "the rejected iterations are not stores of an element to itself"
    );
}

#[test]
fn a_coordinate_reading_an_iterator_pins_nothing() {
    let body = guarded_row_store(
        equals(local("column"), local("row")),
        vec![write(local("selected"), element("value", &["column"]))],
        element("out", &["row", "column"]),
    );
    assert!(
        collapse(&body).is_none(),
        "a diagonal test admits a different coordinate on every iteration"
    );
}

#[test]
fn an_offset_store_keeps_its_loop() {
    let mut body = row_selection();
    body[1] = write(element("out", &["shifted", "column"]), local("selected"));
    assert!(
        collapse(&body).is_none(),
        "binding the iterator would not redirect a store that never named it"
    );
}

#[test]
fn a_residual_range_guard_survives_the_collapse() {
    let range = gast::Expression::binary(
        gast::BinaryOp::Le,
        local("column"),
        gast::Expression::Integer(2),
    );
    let body = guarded_row_store(
        gast::Expression::binary(
            gast::BinaryOp::And,
            equals(local("i"), local("row")),
            range.clone(),
        ),
        vec![write(local("selected"), element("value", &["column"]))],
        element("out", &["row", "column"]),
    );
    let collapse = collapse(&body).expect("the row equality pins the row axis");
    assert_eq!(collapse.axes, vec![0]);
    let [guarded] = collapse.body.as_slice() else {
        panic!("the residual guard wraps the collapsed body")
    };
    let gast::Statement::If(guarded) = &guarded.node else {
        panic!("expected the residual guard")
    };
    assert_eq!(
        guarded.branches[0].condition,
        gast::Condition::Expression(range),
        "the slice range still decides whether the store happens"
    );
    assert!(
        guarded.else_body.is_none(),
        "the coordinates the range rejects are left alone"
    );
}

#[test]
fn a_selector_read_inside_a_correlated_selection_keeps_its_loop() {
    let table = gast::Reference::Local(gast::RefPart {
        name: gast::Name::ident("table"),
        subscripts: vec![local("row")],
        span: Span::DUMMY,
    });
    let correlated = gast::IfExpression::bounded_selection(table, vec![3])
        .expect("a runtime subscript over a known extent is a bounded selection");
    assert!(
        correlated.bounded_selection_correlation().is_some(),
        "the fixture carries the twin the binder must not disturb"
    );
    let body = guarded_row_store(
        equals(local("i"), local("row")),
        vec![write(local("selected"), gast::Expression::If(correlated))],
        element("out", &["row", "column"]),
    );
    assert!(
        collapse(&body).is_none(),
        "a bounded selection carries a legalized twin the binder cannot reach"
    );
}

#[test]
fn a_plain_conditional_operand_is_bound_like_any_other() {
    let conditional = gast::Expression::If(gast::IfExpression::new(
        vec![(
            equals(local("row"), gast::Expression::Integer(1)),
            gast::Expression::Real(1.0),
        )],
        gast::Expression::Real(0.0),
    ));
    let body = guarded_row_store(
        equals(local("i"), local("row")),
        vec![write(local("selected"), conditional)],
        element("out", &["row", "column"]),
    );
    let collapse = collapse(&body).expect("a conditional without a twin is rewritten in place");
    let gast::Statement::Assignment { value, .. } = &collapse.body[0].node else {
        panic!("expected the selection write")
    };
    let gast::Expression::If(bound) = value else {
        panic!("expected the conditional operand")
    };
    assert_eq!(
        bound.branches[0].0,
        equals(local("i"), gast::Expression::Integer(1)),
        "the conditional now reads the coordinate the assignment selects"
    );
}

#[test]
fn a_nested_loop_redeclaring_the_iterator_keeps_its_axis() {
    let inner = gast::Spanned::dummy(gast::Statement::for_loop(gast::ForLoop::new(
        Some(gast::Name::ident("row")),
        gast::Expression::Integer(1),
        None,
        gast::Expression::Integer(3),
        vec![write(local("selected"), element("value", &["row"]))],
    )));
    let body = guarded_row_store(
        equals(local("i"), local("row")),
        vec![inner],
        element("out", &["row", "column"]),
    );
    assert!(
        collapse(&body).is_none(),
        "a redeclared iterator does not mean the same thing throughout the body"
    );
}

/// A loop bound, a loop body, and a call argument all read the pinned
/// iterator; binding must reach every one of them, or the collapsed body
/// still walks a coordinate the axis no longer supplies.
#[test]
fn iterator_reads_inside_loops_and_calls_are_bound_to_the_selected_coordinate() {
    let taken = vec![
        gast::Spanned::dummy(gast::Statement::for_loop(gast::ForLoop::new(
            Some(gast::Name::ident("m")),
            gast::Expression::Integer(1),
            None,
            local("row"),
            vec![write(local("selected"), element("value", &["m", "row"]))],
        ))),
        gast::Spanned::dummy(gast::Statement::Call(gast::FunctionCall {
            function: gast::Name::ident("observe"),
            arguments: vec![local("row")],
        })),
        write(local("selected"), element("value", &["column"])),
    ];
    let body = guarded_row_store(
        equals(local("i"), local("row")),
        taken,
        element("out", &["row", "column"]),
    );
    let collapse = collapse(&body).expect("the guard pins the row axis");
    assert_eq!(collapse.axes, vec![0], "only the row axis is pinned");

    let gast::Statement::For(bound_loop) = &collapse.body[0].node else {
        panic!("expected the inner loop")
    };
    assert_eq!(
        bound_loop.stop,
        local("i"),
        "the loop bound reads the selected coordinate"
    );
    let gast::Statement::Assignment { value, .. } = &bound_loop.body[0].node else {
        panic!("expected the loop body write")
    };
    assert_eq!(
        *value,
        element("value", &["m", "i"]),
        "the loop body reads the selected coordinate, not the dropped iterator"
    );

    let gast::Statement::Call(call) = &collapse.body[1].node else {
        panic!("expected the call statement")
    };
    assert_eq!(
        call.arguments,
        vec![local("i")],
        "the call hands over the selected coordinate"
    );
}
