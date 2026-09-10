//! Both directions: the rewrite must fire on the shape it exists for, and must
//! decline on every shape whose proof does not close.

use super::bind_invariant_guards;
use rumoca_core::Span;
use rumoca_ir_galec::ast as gast;

fn local(name: &str) -> gast::Expression {
    gast::Expression::Ref(gast::Reference::local(gast::Name::ident(name)))
}

fn state(name: &str) -> gast::Expression {
    gast::Expression::Ref(gast::Reference::state(gast::Name::ident(name)))
}

/// `speed > 0.0`: invariant, computed, and raising, which is the whole point.
fn raising_guard() -> gast::Expression {
    gast::Expression::binary(
        gast::BinaryOp::Gt,
        state("speed"),
        gast::Expression::Real(0.0),
    )
}

fn assign(target: &str, value: gast::Expression) -> gast::Spanned<gast::Statement> {
    gast::Spanned::new(
        gast::Statement::Assignment {
            target: gast::Reference::local(gast::Name::ident(target)),
            value,
        },
        Span::DUMMY,
    )
}

/// The staging-nest element body: one total guard over an index-dependent
/// element, then the store.
fn element_body(condition: gast::Condition) -> Vec<gast::Spanned<gast::Statement>> {
    vec![
        gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition,
                    body: vec![assign("t", local("i"))],
                    span: Span::DUMMY,
                }],
                else_body: Some(vec![assign("t", gast::Expression::Real(0.0))]),
            }),
            Span::DUMMY,
        ),
        assign("argument", local("t")),
    ]
}

fn iterators() -> Vec<gast::Name> {
    vec![gast::Name::ident("i")]
}

/// Run the pass over `body` with one axis of `extent`, returning the bindings.
fn run(
    body: &mut [gast::Spanned<gast::Statement>],
    extent: u32,
    counter: &mut usize,
) -> Vec<super::BoundGuard> {
    bind_invariant_guards(
        body,
        &iterators(),
        &[extent],
        "clocked0",
        counter,
        Span::DUMMY,
    )
}

#[test]
fn a_loop_invariant_raising_guard_moves_out_of_the_nest() {
    let mut body = element_body(gast::Condition::Expression(raising_guard()));
    let mut counter = 7;
    let bound = run(&mut body, 16, &mut counter);

    assert_eq!(bound.len(), 1);
    assert_eq!(
        counter, 8,
        "the temporary counter must advance once per binding"
    );
    assert_eq!(
        bound[0].declaration.name,
        gast::Name::ident("rumoca_clocked0_guard_7")
    );
    assert_eq!(
        bound[0].declaration.ty,
        gast::TypeRef::Primitive(gast::ScalarType::Boolean)
    );
    // The binding carries the original condition, and the guard now tests the
    // local: the comparison exists exactly once.
    let gast::Statement::Assignment { value, .. } = &bound[0].binding.node else {
        panic!("binding must be an assignment");
    };
    assert_eq!(value, &raising_guard());
    let gast::Statement::If(guard) = &body[0].node else {
        panic!("the guard must still be a conditional");
    };
    assert_eq!(
        guard.branches[0].condition,
        gast::Condition::Expression(local("rumoca_clocked0_guard_7"))
    );
}

#[test]
fn an_index_dependent_condition_stays_inside() {
    // `i > 0.0` changes with the iterator, so its value is not loop-invariant
    // and there is nothing to bind.
    let condition = gast::Condition::Expression(gast::Expression::binary(
        gast::BinaryOp::Gt,
        local("i"),
        gast::Expression::Real(0.0),
    ));
    let mut body = element_body(condition.clone());
    let mut counter = 0;
    assert!(run(&mut body, 16, &mut counter).is_empty());
    let gast::Statement::If(guard) = &body[0].node else {
        panic!("conditional");
    };
    assert_eq!(guard.branches[0].condition, condition);
}

#[test]
fn a_condition_reading_a_name_the_body_assigns_stays_inside() {
    // `t` is written by both arms, so the condition's value differs between the
    // first iteration and the rest. Mentioning no iterator is not enough.
    let condition = gast::Condition::Expression(gast::Expression::binary(
        gast::BinaryOp::Gt,
        local("t"),
        gast::Expression::Real(0.0),
    ));
    let mut body = element_body(condition);
    let mut counter = 0;
    assert!(run(&mut body, 16, &mut counter).is_empty());
}

#[test]
fn a_possibly_empty_nest_declines() {
    // With a zero extent the body never runs, so a hoisted evaluation would
    // raise on an execution that raised nothing. This is the clause that makes
    // the status word bit-identical rather than merely equivalent.
    let mut body = element_body(gast::Condition::Expression(raising_guard()));
    let mut counter = 0;
    assert!(run(&mut body, 0, &mut counter).is_empty());
}

#[test]
fn a_signal_check_condition_declines() {
    // Checking is catching: it clears the bits it tests, so accumulation is no
    // longer monotone and the count becomes observable.
    let condition = gast::Condition::SignalCheck(gast::SignalCheck {
        closure: None,
        test: Some(gast::SignalTest {
            negated: false,
            signals: vec![gast::Identifier::new("NAN")],
        }),
        fallback: None,
    });
    let mut body = element_body(condition);
    let mut counter = 0;
    assert!(run(&mut body, 16, &mut counter).is_empty());
}

#[test]
fn a_body_holding_a_call_declines_even_when_the_guard_itself_is_provable() {
    // The callee's body is not in scope, so it may hold a signal check. The
    // guard is impeccable and the nest is still refused.
    let mut body = element_body(gast::Condition::Expression(raising_guard()));
    body.push(gast::Spanned::new(
        gast::Statement::Call(gast::FunctionCall {
            function: gast::Name::ident("helper"),
            arguments: vec![local("i")],
        }),
        Span::DUMMY,
    ));
    let mut counter = 0;
    assert!(run(&mut body, 16, &mut counter).is_empty());
}

#[test]
fn a_bare_reference_condition_is_not_worth_binding() {
    // A Boolean state read costs one load either way, so binding it would only
    // add a store.
    let condition = gast::Condition::Expression(state("enabled"));
    let mut body = element_body(condition.clone());
    let mut counter = 0;
    assert!(run(&mut body, 16, &mut counter).is_empty());
    let gast::Statement::If(guard) = &body[0].node else {
        panic!("conditional");
    };
    assert_eq!(guard.branches[0].condition, condition);
}

#[test]
fn a_condition_calling_a_function_declines() {
    // A call in the condition could write what the body reads; deciding that
    // needs a resolved callee this pass does not have.
    let condition = gast::Condition::Expression(gast::Expression::binary(
        gast::BinaryOp::Gt,
        gast::Expression::Call(gast::FunctionCall {
            function: gast::Name::ident("absolute"),
            arguments: vec![state("speed")],
        }),
        gast::Expression::Real(0.0),
    ));
    let mut body = element_body(condition);
    let mut counter = 0;
    assert!(run(&mut body, 16, &mut counter).is_empty());
}
