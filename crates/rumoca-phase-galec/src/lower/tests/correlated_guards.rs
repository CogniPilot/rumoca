use rumoca_core::Span;

use crate::lower::*;

fn guarded_statements(
    condition: &gast::Name,
    body: Vec<gast::Spanned<gast::Statement>>,
    fallback: Vec<gast::Spanned<gast::Statement>>,
) -> gast::Spanned<gast::Statement> {
    gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
        branches: vec![gast::IfBranch {
            condition: gast::Condition::Expression(gast::Expression::Ref(gast::Reference::local(
                condition.clone(),
            ))),
            body,
            span: Span::DUMMY,
        }],
        else_body: Some(fallback),
    }))
}

#[test]
fn correlated_call_consumers_join_their_lazy_producer_branch() {
    let condition = gast::Name::ident("enabled");
    let result = gast::Name::ident("result");
    let output = gast::Name::ident("output");
    let unrelated = gast::Name::ident("unrelated");
    let producer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::MultiAssignment {
            targets: vec![gast::Reference::local(result.clone())],
            call: gast::FunctionCall {
                function: gast::Name::ident("produce"),
                arguments: Vec::new(),
            },
        })],
        Vec::new(),
    );
    let middle = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(unrelated),
        value: gast::Expression::Integer(1),
    });
    let consumer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output.clone()),
            value: gast::Expression::Ref(gast::Reference::local(result.clone())),
        })],
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output),
            value: gast::Expression::Real(0.0),
        })],
    );

    let coalesced = user_functions::coalesce_correlated_guards(vec![
        producer,
        middle.clone(),
        consumer.clone(),
    ]);
    assert_eq!(coalesced.len(), 2);
    let gast::Statement::If(guard) = &coalesced[0].node else {
        panic!("the producer remains the shared guard")
    };
    assert!(matches!(
        guard.branches[0].body.as_slice(),
        [
            gast::Spanned {
                node: gast::Statement::MultiAssignment { .. },
                ..
            },
            gast::Spanned {
                node: gast::Statement::Assignment { .. },
                ..
            }
        ]
    ));
    assert_eq!(coalesced[1], middle);

    let dependency = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(result),
        value: gast::Expression::Real(2.0),
    });
    let blocked = user_functions::coalesce_correlated_guards(vec![
        coalesced[0].clone(),
        dependency,
        consumer,
    ]);
    assert_eq!(
        blocked.len(),
        3,
        "a dependency between producer and consumer must block reordering"
    );
}

/// One call standing between a lazy producer and its correlated consumer must
/// not keep the two apart.
///
/// The producer arm is where the consumer's value is defined, so leaving the
/// consumer behind the call leaves a local defined under one test and read
/// under a second copy of it, with a call in between that a C compiler cannot
/// see through. It reports the read as possibly uninitialized and the assurance
/// preflight, which is `-Werror`, rejects the artifact. Joining the two puts the
/// definition and the use in one arm, where definite assignment is visible.
#[test]
fn a_call_between_a_producer_and_its_consumer_does_not_block_the_join() {
    let condition = gast::Name::ident("enabled");
    let result = gast::Name::ident("result");
    let output = gast::Name::ident("output");
    let producer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::MultiAssignment {
            targets: vec![gast::Reference::local(result.clone())],
            call: gast::FunctionCall {
                function: gast::Name::ident("produce"),
                arguments: Vec::new(),
            },
        })],
        Vec::new(),
    );
    let middle = gast::Spanned::dummy(gast::Statement::MultiAssignment {
        targets: vec![gast::Reference::local(gast::Name::ident("elsewhere"))],
        call: gast::FunctionCall {
            function: gast::Name::ident("unrelated"),
            arguments: Vec::new(),
        },
    });
    let consumer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output.clone()),
            value: gast::Expression::Ref(gast::Reference::local(result)),
        })],
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output),
            value: gast::Expression::Bool(false),
        })],
    );

    let coalesced =
        user_functions::coalesce_correlated_guards(vec![producer, middle.clone(), consumer]);
    assert_eq!(
        coalesced.len(),
        2,
        "the consumer must join the producer's guard across the call"
    );
    let gast::Statement::If(guard) = &coalesced[0].node else {
        panic!("the producer remains the shared guard")
    };
    assert_eq!(
        guard.branches[0].body.len(),
        2,
        "the producing arm carries both the call and the read of its result"
    );
    assert_eq!(
        guard
            .else_body
            .as_ref()
            .expect("the fallback arm survives")
            .len(),
        1,
        "the fallback arm carries the consumer's own value"
    );
    assert_eq!(coalesced[1], middle);
}

/// A guard that can raise an error signal keeps the conservative answer.
///
/// The block's error signal status is state a callee both writes and, through a
/// signal check, may read, so moving a raise across a call is a reordering of
/// two observables and not of one value. Only a guard that raises nothing is
/// invisible to the callee, and a comparison is the one raise a reorderable
/// guard can still carry.
#[test]
fn a_signalling_guard_does_not_cross_a_call() {
    let condition = gast::Name::ident("enabled");
    let output = gast::Name::ident("output");
    let comparison = gast::Expression::Binary {
        op: gast::BinaryOp::Lt,
        lhs: Box::new(gast::Expression::Ref(gast::Reference::local(
            gast::Name::ident("measured"),
        ))),
        rhs: Box::new(gast::Expression::Real(1.0)),
    };
    let producer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(gast::Name::ident("produced")),
            value: gast::Expression::Bool(true),
        })],
        Vec::new(),
    );
    let middle = gast::Spanned::dummy(gast::Statement::MultiAssignment {
        targets: vec![gast::Reference::local(gast::Name::ident("elsewhere"))],
        call: gast::FunctionCall {
            function: gast::Name::ident("unrelated"),
            arguments: Vec::new(),
        },
    });
    let consumer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output.clone()),
            value: comparison,
        })],
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output),
            value: gast::Expression::Bool(false),
        })],
    );

    let coalesced =
        user_functions::coalesce_correlated_guards(vec![producer, middle, consumer.clone()]);
    assert_eq!(
        coalesced.len(),
        3,
        "a guard carrying a comparison must not move across a call"
    );
    assert_eq!(coalesced[2], consumer);
}

/// The crossing is a data-flow question first: a call the guard depends on, or
/// that depends on the guard, still keeps them apart.
#[test]
fn a_guard_does_not_cross_a_call_it_shares_a_name_with() {
    let condition = gast::Name::ident("enabled");
    let output = gast::Name::ident("output");
    let carried = gast::Name::ident("carried");
    let producer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(gast::Name::ident("produced")),
            value: gast::Expression::Bool(true),
        })],
        Vec::new(),
    );
    let middle = gast::Spanned::dummy(gast::Statement::MultiAssignment {
        targets: vec![gast::Reference::local(carried.clone())],
        call: gast::FunctionCall {
            function: gast::Name::ident("unrelated"),
            arguments: Vec::new(),
        },
    });
    let consumer = guarded_statements(
        &condition,
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output.clone()),
            value: gast::Expression::Ref(gast::Reference::local(carried)),
        })],
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(output),
            value: gast::Expression::Bool(false),
        })],
    );

    let coalesced =
        user_functions::coalesce_correlated_guards(vec![producer, middle, consumer.clone()]);
    assert_eq!(
        coalesced.len(),
        3,
        "a guard that reads what the call writes must stay behind it"
    );
    assert_eq!(coalesced[2], consumer);
}
