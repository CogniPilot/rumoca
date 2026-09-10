//! The classification is the checker SPEC_0034 GAL-040 trusts, so these tests
//! pin both directions: what mints the permission and, more importantly, what
//! refuses it.

use super::{RepeatableSignalEffect, StatusEffect, expression_effect, statement_effect};
use crate::ast::{
    BinaryOp, Condition, Expression, IfBranch, IfStatement, Reference, Spanned, Statement,
};
use crate::ast::{FunctionCall, Identifier, Name, SignalCheck, SignalTest};
use rumoca_core::Span;

fn local(name: &str) -> Expression {
    Expression::Ref(Reference::local(Name::ident(name)))
}

fn spanned(statement: Statement) -> Spanned<Statement> {
    Spanned::new(statement, Span::DUMMY)
}

#[test]
fn literals_and_references_cannot_reach_the_word() {
    assert_eq!(
        expression_effect(&Expression::Real(1.0)),
        StatusEffect::Inert
    );
    assert_eq!(expression_effect(&local("x")), StatusEffect::Inert);
    assert_eq!(
        expression_effect(&Expression::binary(
            BinaryOp::Add,
            local("x"),
            Expression::Real(1.0)
        )),
        StatusEffect::Inert
    );
}

#[test]
fn a_comparison_accumulates_because_a_qnan_operand_raises_nan() {
    let compare = Expression::binary(BinaryOp::Gt, local("x"), Expression::Real(0.0));
    assert_eq!(expression_effect(&compare), StatusEffect::AccumulateOr);
    assert!(RepeatableSignalEffect::prove(&compare).is_some());
}

#[test]
fn a_builtin_call_accumulates_and_a_user_call_is_opaque() {
    let builtin = Expression::Call(FunctionCall {
        function: Name::ident("integer"),
        arguments: vec![local("x")],
    });
    assert_eq!(expression_effect(&builtin), StatusEffect::AccumulateOr);
    assert!(RepeatableSignalEffect::prove(&builtin).is_some());

    // A user function's body is not in scope here, so its effect is unbounded
    // and the permission must be refused rather than guessed.
    let user = Expression::Call(FunctionCall {
        function: Name::ident("myHelper"),
        arguments: vec![local("x")],
    });
    assert_eq!(expression_effect(&user), StatusEffect::Opaque);
    assert!(RepeatableSignalEffect::prove(&user).is_none());
}

#[test]
fn a_signal_check_consumes_and_is_refused() {
    let check = Condition::SignalCheck(SignalCheck {
        closure: None,
        test: Some(SignalTest {
            negated: false,
            signals: vec![Identifier::new("NAN")],
        }),
        fallback: None,
    });
    assert_eq!(super::condition_effect(&check), StatusEffect::Consume);
    assert!(RepeatableSignalEffect::prove_condition(&check).is_none());
}

#[test]
fn a_signal_check_fallback_does_not_rescue_the_condition() {
    // The fallback is a plain expression, but the check still catches, so the
    // join must stay at `Consume`.
    let check = Condition::SignalCheck(SignalCheck {
        closure: None,
        test: None,
        fallback: Some(local("x")),
    });
    assert_eq!(super::condition_effect(&check), StatusEffect::Consume);
    assert!(RepeatableSignalEffect::prove_condition(&check).is_none());
}

#[test]
fn a_signal_statement_accumulates_and_limit_is_inert() {
    assert_eq!(
        statement_effect(&Statement::Signal(vec![Identifier::new("NAN")])),
        StatusEffect::AccumulateOr
    );
    assert_eq!(
        statement_effect(&Statement::Limit(Vec::new())),
        StatusEffect::Inert
    );
}

#[test]
fn a_conditional_joins_its_condition_and_every_arm() {
    // The condition is inert but an arm raises, so the statement accumulates:
    // the effect is the join over the whole construct, not the head alone.
    let statement = Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(local("flag")),
            body: vec![spanned(Statement::Signal(vec![Identifier::new("NAN")]))],
            span: Span::DUMMY,
        }],
        else_body: Some(Vec::new()),
    });
    assert_eq!(statement_effect(&statement), StatusEffect::AccumulateOr);

    // An arm holding a user call drags the whole statement to `Opaque`.
    let opaque = Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(local("flag")),
            body: vec![spanned(Statement::Call(FunctionCall {
                function: Name::ident("myHelper"),
                arguments: Vec::new(),
            }))],
            span: Span::DUMMY,
        }],
        else_body: Some(Vec::new()),
    });
    assert_eq!(statement_effect(&opaque), StatusEffect::Opaque);
}

#[test]
fn the_lattice_orders_the_two_refusing_variants_above_the_two_permitting_ones() {
    // `join` must never be able to weaken a refusal into a permission, which is
    // what makes the fold in `join_all` fail closed.
    for effect in [
        StatusEffect::Inert,
        StatusEffect::AccumulateOr,
        StatusEffect::Consume,
        StatusEffect::Opaque,
    ] {
        assert!(!effect.join(StatusEffect::Consume).is_repeatable());
        assert!(!effect.join(StatusEffect::Opaque).is_repeatable());
        assert_eq!(effect.join(StatusEffect::Inert), effect);
    }
    assert!(StatusEffect::Inert.is_repeatable());
    assert!(StatusEffect::AccumulateOr.is_repeatable());
}

#[test]
fn a_loop_cannot_hide_raising_consuming_or_opaque_body_effects() {
    use crate::ast::ForLoop;

    // SPEC_0034 GAL-040: the body can end the repeatable signal region,
    // even though every bound is inert and the loop is statically bounded.
    let consuming = Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::SignalCheck(SignalCheck {
                closure: None,
                test: None,
                fallback: None,
            }),
            body: Vec::new(),
            span: Span::DUMMY,
        }],
        else_body: None,
    });
    for (body, expected) in [
        (Statement::Limit(Vec::new()), StatusEffect::Inert),
        (
            Statement::Signal(vec![Identifier::new("NAN")]),
            StatusEffect::AccumulateOr,
        ),
        (consuming, StatusEffect::Consume),
        (
            Statement::Call(FunctionCall {
                function: Name::ident("myHelper"),
                arguments: Vec::new(),
            }),
            StatusEffect::Opaque,
        ),
    ] {
        let statement = Statement::for_loop(ForLoop::new(
            Some(Name::ident("i")),
            Expression::Integer(1),
            Some(Expression::Integer(1)),
            Expression::Integer(3),
            vec![spanned(body)],
        ));
        assert_eq!(statement_effect(&statement), expected);
    }
}
