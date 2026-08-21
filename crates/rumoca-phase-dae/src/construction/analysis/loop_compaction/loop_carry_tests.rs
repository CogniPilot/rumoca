//! Loop-carried scalar locals must survive dead-store inlining.
//!
//! A function local written inside a compact loop and read after the loop is
//! live across the loop-exit edge. The straight-line substitution that removes
//! genuinely dead scratch definitions must refuse those stores whether or not
//! the written value depends on the local's previous value.

use super::*;
use rumoca_core::{ComponentReference, Literal, OpBinary, Reference, Subscript};

fn span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::DUMMY, 0, 1)
}

fn component(name: &str) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(1),
        }],
    )
    .expect("test component reference has exact identity")
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn indexed(name: &str, index: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![Subscript::Expr {
            expr: Box::new(var(index)),
            span: span(),
        }],
        span: span(),
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: span(),
    }
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: span(),
    }
}

fn assign(target: &str, value: Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: component(target),
        value,
        span: span(),
    }
}

fn for_loop(index: &str, count: i64, body: Vec<rumoca_core::Statement>) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: index.to_string(),
            range: Expression::Range {
                start: Box::new(integer(1)),
                step: None,
                end: Box::new(integer(count)),
                span: span(),
            },
        }],
        equations: body,
        span: span(),
    }
}

fn names(names: &[&str]) -> HashSet<VarName> {
    names.iter().map(|name| VarName::new(*name)).collect()
}

fn loop_body(statements: &[rumoca_core::Statement]) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .find_map(|statement| match statement {
            rumoca_core::Statement::For { equations, .. } => Some(equations.clone()),
            _ => None,
        })
        .expect("the compacted statements still contain the loop")
}

/// ```text
/// acc := 0.0;
/// for i loop
///   w[i] := acc;        // reads what the previous outer iteration left
///   for j loop
///     acc := 1.0;       // dead store, but it "definitely defines" acc
///     acc := s[j];      // live across the OUTER back edge
///   end for;
/// end for;
/// ```
///
/// The outer loop re-enters without re-running the inner body, so the inner
/// body's own write cannot settle the value `w[i] := acc` observes. Chaining
/// the two re-entry paths into one sequential walk let it, and the live store
/// was deleted.
#[test]
fn an_inner_body_write_does_not_settle_what_an_outer_back_edge_reads() {
    let inner = for_loop(
        "j",
        2,
        vec![assign("acc", real(1.0)), assign("acc", indexed("s", "j"))],
    );
    let statements = vec![
        assign("acc", real(0.0)),
        for_loop(
            "i",
            2,
            vec![
                rumoca_core::Statement::Assignment {
                    comp: ComponentReference::construct(
                        false,
                        span(),
                        vec![rumoca_core::ComponentRefPart {
                            ident: "w".to_string(),
                            span: span(),
                            subs: vec![Subscript::Expr {
                                expr: Box::new(var("i")),
                                span: span(),
                            }],
                            def_id: rumoca_core::DefId::new(3),
                        }],
                    )
                    .expect("test component reference has exact identity"),
                    value: var("acc"),
                    span: span(),
                },
                inner,
            ],
        ),
    ];
    let compacted = inline_loop_local_prefixes(&statements, &names(&["acc"]), &names(&["w"]));
    let outer = loop_body(&compacted);
    let rumoca_core::Statement::For { equations, .. } = &outer[1] else {
        panic!("the inner loop was rewritten away: {compacted:#?}");
    };
    assert_eq!(
        equations.len(),
        2,
        "the store live across the outer back edge was deleted: {compacted:#?}"
    );
}

/// The loop-exit edge is an alternative successor of the body, not a
/// continuation of it. Chaining the body ahead of the exit path lets the
/// body's own kill hide the read after the loop, which is exactly the
/// question the store's liveness turns on.
#[test]
fn a_body_kill_does_not_hide_a_read_after_the_loop() {
    let body = vec![assign("acc", indexed("r", "i"))];
    let suffix = vec![assign("last", var("acc"))];
    let acc = VarName::new("acc");
    assert!(!statements_read_incoming_name(&body, &acc));
    assert!(statements_read_incoming_name(&suffix, &acc));
    // Entering the follower chain as the loop itself keeps the exit read
    // visible: a compact domain may run zero times, so a loop never settles a
    // value for its own successor.
    let back_edge = vec![for_loop("i", 4, body)];
    assert!(statement_segments_read_incoming_name(
        &[back_edge.as_slice(), suffix.as_slice()],
        &acc,
    ));
}

/// `acc := -1; for i loop acc := r[i]; end for; last := acc;`
///
/// Every write in the loop overwrites the previous value, and the last one is
/// read after the loop. Deleting the body's store loses it.
#[test]
fn overwriting_loop_carry_read_after_the_loop_survives() {
    let statements = vec![
        assign("acc", real(-1.0)),
        for_loop("i", 4, vec![assign("acc", indexed("r", "i"))]),
        assign("last", var("acc")),
    ];
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["acc"]), &HashSet::new());
    assert_eq!(
        loop_body(&compacted).len(),
        1,
        "the loop body's live store was deleted: {compacted:#?}"
    );
}

/// `acc := 0; for i loop acc := acc + r[i]; end for; last := acc;`
///
/// A self-dependent accumulation is loop-carried through the body itself and
/// was already preserved; it must stay preserved.
#[test]
fn accumulating_loop_carry_survives() {
    let statements = vec![
        assign("acc", real(0.0)),
        for_loop(
            "i",
            4,
            vec![assign(
                "acc",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(var("acc")),
                    rhs: Box::new(indexed("r", "i")),
                    span: span(),
                },
            )],
        ),
        assign("last", var("acc")),
    ];
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["acc"]), &HashSet::new());
    assert_eq!(
        loop_body(&compacted).len(),
        1,
        "the accumulator's store was deleted: {compacted:#?}"
    );
}

/// `acc := -1; for i loop if c[i] then acc := r[i]; end if; end for; last := acc;`
///
/// A conditional write is not straight-line, so the substitution never applied
/// to it; the guard must keep holding.
#[test]
fn conditionally_written_loop_carry_survives() {
    let statements = vec![
        assign("acc", real(-1.0)),
        for_loop(
            "i",
            4,
            vec![rumoca_core::Statement::If {
                cond_blocks: vec![rumoca_core::StatementBlock {
                    cond: indexed("c", "i"),
                    stmts: vec![assign("acc", indexed("r", "i"))],
                }],
                else_block: None,
                span: span(),
            }],
        ),
        assign("last", var("acc")),
    ];
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["acc"]), &HashSet::new());
    let body = loop_body(&compacted);
    let rumoca_core::Statement::If { cond_blocks, .. } = &body[0] else {
        panic!("the conditional body was rewritten: {compacted:#?}");
    };
    assert_eq!(
        cond_blocks[0].stmts.len(),
        1,
        "the guarded store was deleted: {compacted:#?}"
    );
}

/// `for i loop t := r[i] * 2; y[i] := t; end for;`
///
/// A temporary written and consumed inside one iteration is dead at the loop
/// exit; inlining it is the optimization this analysis exists for.
#[test]
fn same_iteration_temporary_is_still_inlined() {
    let statements = vec![
        for_loop(
            "i",
            4,
            vec![
                assign(
                    "t",
                    Expression::Binary {
                        op: OpBinary::Mul,
                        lhs: Box::new(indexed("r", "i")),
                        rhs: Box::new(real(2.0)),
                        span: span(),
                    },
                ),
                rumoca_core::Statement::Assignment {
                    comp: ComponentReference::construct(
                        false,
                        span(),
                        vec![rumoca_core::ComponentRefPart {
                            ident: "y".to_string(),
                            span: span(),
                            subs: vec![Subscript::Expr {
                                expr: Box::new(var("i")),
                                span: span(),
                            }],
                            def_id: rumoca_core::DefId::new(2),
                        }],
                    )
                    .expect("test component reference has exact identity"),
                    value: var("t"),
                    span: span(),
                },
            ],
        ),
        assign("last", real(0.0)),
    ];
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["t"]), &names(&["t"]));
    assert_eq!(
        loop_body(&compacted).len(),
        1,
        "the same-iteration temporary was not inlined: {compacted:#?}"
    );
}
