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

fn element(name: &str, index: &str, def_id: u32) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: vec![Subscript::Expr {
                expr: Box::new(var(index)),
                span: span(),
            }],
            def_id: rumoca_core::DefId::new(def_id),
        }],
    )
    .expect("test component reference has exact identity")
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

/// ```text
/// for i loop
///   for j loop
///     t := r[j];            // the store under classification
///   end for;
///   if c[i] then break; end if;
///   t := 0.0;
/// end for;
/// last := t;
/// ```
///
/// `break` leaves the outer loop from a point no backward walk of the body
/// reaches, so `t := 0.0` does not settle the value `last := t` observes on
/// that path. Only the store inside the inner loop does. A proof that walks
/// past the jump as though it fell through sees the later kill, calls the
/// inner store dead, and loses the value the break path returns.
#[test]
fn a_break_past_a_later_kill_keeps_the_store() {
    let statements = vec![
        for_loop(
            "i",
            2,
            vec![
                for_loop("j", 2, vec![assign("t", indexed("r", "j"))]),
                rumoca_core::Statement::If {
                    cond_blocks: vec![rumoca_core::StatementBlock {
                        cond: indexed("c", "i"),
                        stmts: vec![rumoca_core::Statement::Break { span: span() }],
                    }],
                    else_block: None,
                    span: span(),
                },
                assign("t", real(0.0)),
            ],
        ),
        assign("last", var("t")),
    ];
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["t"]), &HashSet::new());
    let outer = loop_body(&compacted);
    let rumoca_core::Statement::For { equations, .. } = &outer[0] else {
        panic!("the inner loop was rewritten away: {compacted:#?}");
    };
    assert_eq!(
        equations.len(),
        1,
        "the store the break path returns was deleted: {compacted:#?}"
    );
}

/// `for i loop t := r[i]; y[i] := t[i]; end for;`
///
/// Nothing after the loop observes `t`, so its value escapes nowhere. The read
/// inside the loop is subscripted, though, and substitution replaces only a
/// plain unsubscripted read; deleting the store would leave that read with no
/// definition at all.
#[test]
fn a_subscripted_read_of_the_local_keeps_the_store() {
    let statements = vec![for_loop(
        "i",
        4,
        vec![
            assign("t", indexed("r", "i")),
            rumoca_core::Statement::Assignment {
                comp: element("y", "i", 2),
                value: indexed("t", "i"),
                span: span(),
            },
        ],
    )];
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["t"]), &HashSet::new());
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the store feeding a read substitution cannot rewrite was deleted: {compacted:#?}"
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

/// ```text
/// for i loop t := r[i]; end for;
/// f[t]();
/// ```
///
/// The callable of a call statement is selected by a component reference, and
/// the subscripts on it are evaluated where the call runs (MLS §12.4.4). That
/// makes `t` live after the loop, so the loop's store is its only definition.
/// A liveness walk that reads only the arguments and the receiving elements of
/// a call misses the subscript, certifies the store as unobserved, and deletes
/// the definition the call depends on.
#[test]
fn a_subscripted_callable_reference_keeps_the_store() {
    let call = vec![rumoca_core::Statement::FunctionCall {
        comp: Reference::from_component_reference(element("f", "t", 4)),
        args: Vec::new(),
        outputs: Vec::new(),
        span: span(),
    }];
    let mut statements = vec![for_loop("i", 4, vec![assign("t", indexed("r", "i"))])];
    statements.extend(call.clone());
    let compacted = inline_dead_loop_scalar_locals(&statements, &names(&["t"]), &HashSet::new());
    assert_eq!(
        loop_body(&compacted).len(),
        1,
        "the store the callable's subscript reads was deleted: {compacted:#?}"
    );
    assert!(
        liveness::reads_incoming_value(&call, &VarName::new("t")),
        "the callable's subscript is a read of `t`"
    );
}

/// ```text
/// for i loop
///   t := 1.0;
///   for j loop
///     y[j] := t;      // reads what the previous inner iteration left
///     t := q[j];
///   end for;
/// end for;
/// ```
///
/// The trailing statement is rewritten in a single pass that replaces every
/// unsubscripted read of `t`, including the one the inner loop performs after
/// its own write to `t`. Folding `t := 1.0` into it would therefore make every
/// iteration read `1.0` where only the first one does.
#[test]
fn a_trailing_statement_that_rewrites_the_local_keeps_the_prefix_definition() {
    let inner = for_loop(
        "j",
        2,
        vec![
            rumoca_core::Statement::Assignment {
                comp: element("y", "j", 5),
                value: var("t"),
                span: span(),
            },
            assign("t", indexed("q", "j")),
        ],
    );
    let statements = vec![for_loop("i", 2, vec![assign("t", real(1.0)), inner])];
    let compacted = inline_loop_local_prefixes(&statements, &names(&["t"]), &names(&["y"]));
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the definition the inner loop's first iteration reads was folded away: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// ```text
/// for i loop
///   t := q;
///   if c then
///     q := 3.0;
///     y := t;         // must still observe the value `q` held on entry
///   end if;
/// end for;
/// ```
///
/// Substituting `q` for `t` inside the trailing statement moves the read of `q`
/// past the write the same statement performs (MLS §11.1), so `y` would take
/// the new value instead of the one the definition captured.
#[test]
fn a_trailing_statement_that_rewrites_a_dependency_keeps_the_prefix_definition() {
    let trailing = rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: var("c"),
            stmts: vec![assign("q", real(3.0)), assign("y", var("t"))],
        }],
        else_block: None,
        span: span(),
    };
    let statements = vec![for_loop("i", 2, vec![assign("t", var("q")), trailing])];
    let compacted = inline_loop_local_prefixes(&statements, &names(&["t", "q"]), &names(&["y"]));
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the definition that captured the earlier `q` was folded away: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// ```text
/// for i loop o := r[i]; z[i] := i; end for;
/// <exit>
/// ```
///
/// One loop whose body defines `o` and one statement after it. The second body
/// statement keeps the body non-empty, so a folded definition stays folded
/// rather than reverting with the loop.
fn loop_defining_o(exit: rumoca_core::Statement) -> Vec<rumoca_core::Statement> {
    vec![
        for_loop(
            "i",
            4,
            vec![
                assign("o", indexed("r", "i")),
                rumoca_core::Statement::Assignment {
                    comp: element("z", "i", 7),
                    value: var("i"),
                    span: span(),
                },
            ],
        ),
        exit,
    ]
}

/// An output the exit path definitely rewrites before the function returns is
/// not observed at the store under proof, so the witness certifies it where a
/// membership test in the output set could only refuse.
#[test]
fn an_output_the_exit_path_definitely_rewrites_is_still_folded() {
    let statements = loop_defining_o(assign("o", real(0.0)));
    let compacted = inline_loop_local_prefixes(&statements, &names(&["o"]), &names(&["o"]));
    assert_eq!(
        loop_body(&compacted).len(),
        1,
        "the exit path settles `o`, so the loop's store is not observed: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// The same loop, with the output read after it instead of rewritten, keeps its
/// store: the caller observes what the last iteration left.
#[test]
fn an_output_the_exit_path_reads_keeps_its_store() {
    let statements = loop_defining_o(assign("last", var("o")));
    let compacted = inline_loop_local_prefixes(&statements, &names(&["o"]), &names(&["o"]));
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the store the exit path reads was folded away: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// The same loop again, with nothing after it at all: the caller still observes
/// the output, so the store stays.
#[test]
fn an_output_no_statement_rewrites_keeps_its_store() {
    let statements = loop_defining_o(assign("last", real(0.0)));
    let compacted = inline_loop_local_prefixes(&statements, &names(&["o"]), &names(&["o"]));
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the caller observes `o`, so the loop's store is not dead: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// The same loop with `o` a plain local rather than an output. Nothing but the
/// loop-exit path can object, so this is the shape that holds the exit-path
/// obligation on its own.
#[test]
fn a_local_the_exit_path_reads_keeps_its_store() {
    let statements = loop_defining_o(assign("last", var("o")));
    let compacted = inline_loop_local_prefixes(&statements, &names(&["o"]), &names(&["last"]));
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the store the exit path reads was folded away: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// And with nothing after the loop reading it, the same local is folded away.
#[test]
fn a_local_no_later_statement_reads_is_folded() {
    let statements = loop_defining_o(assign("last", real(0.0)));
    let compacted = inline_loop_local_prefixes(&statements, &names(&["o"]), &names(&["last"]));
    assert_eq!(
        loop_body(&compacted).len(),
        1,
        "nothing observes `o`, so its store had no reason to stay: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// `for i loop t := q; y[i] := t[i]; end for;`
///
/// Nothing observes `t` outside the loop, but the read inside it is subscripted
/// and the substitution replaces only a plain unsubscripted read. Folding the
/// definition away would leave that read with no definition at all.
#[test]
fn a_subscripted_read_keeps_the_prefix_definition() {
    let statements = vec![for_loop(
        "i",
        3,
        vec![
            assign("t", var("q")),
            rumoca_core::Statement::Assignment {
                comp: element("y", "i", 14),
                value: indexed("t", "i"),
                span: span(),
            },
        ],
    )];
    let compacted = inline_loop_local_prefixes(&statements, &names(&["t"]), &names(&["y"]));
    assert_eq!(
        loop_body(&compacted).len(),
        2,
        "the definition feeding a read the substitution cannot rewrite was folded away: {}",
        super::liveness_corpus::render(&compacted)
    );
}

fn field_read(record: &str, field: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(format!("{record}.{field}")),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn field_target(record: &str, field: &str) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![
            rumoca_core::ComponentRefPart {
                ident: record.to_string(),
                span: span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(11),
            },
            rumoca_core::ComponentRefPart {
                ident: field.to_string(),
                span: span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(12),
            },
        ],
    )
    .expect("test component reference has exact identity")
}

/// ```text
/// for i loop
///   w := t.f;
///   t := r[i];      // defines every field beneath `t`
///   y[i] := w;
/// end for;
/// ```
///
/// Writing the whole record defines `t.f` with it, so folding `w := t.f` into
/// the trailing statement would move the field read past the write that
/// replaces it (MLS §11.1). Nothing assigns the name `t.f`, so only a
/// dependency test that reads the two names as nested paths can see it.
#[test]
fn a_whole_record_write_moves_a_later_field_read() {
    let statements = vec![for_loop(
        "i",
        3,
        vec![
            assign("w", field_read("t", "f")),
            assign("t", indexed("r", "i")),
            rumoca_core::Statement::Assignment {
                comp: element("y", "i", 9),
                value: var("w"),
                span: span(),
            },
        ],
    )];
    let compacted = inline_loop_local_prefixes(&statements, &names(&["t", "w"]), &names(&["y"]));
    assert_eq!(
        loop_body(&compacted).len(),
        3,
        "the field read was moved past the write that replaces it: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// ```text
/// for i loop
///   w := t;
///   t.f := q[i];    // redefines part of `t`
///   y[i] := w;
/// end for;
/// ```
///
/// The same question from the other side: the write names a field and the read
/// names the whole record. Nothing assigns the name `t`, so a scan that
/// compares the two names for equality reports no change and the substituted
/// read picks up the new field.
#[test]
fn a_field_write_moves_a_later_whole_record_read() {
    let statements = vec![for_loop(
        "i",
        3,
        vec![
            assign("w", var("t")),
            rumoca_core::Statement::Assignment {
                comp: field_target("t", "f"),
                value: indexed("q", "i"),
                span: span(),
            },
            rumoca_core::Statement::Assignment {
                comp: element("y", "i", 10),
                value: var("w"),
                span: span(),
            },
        ],
    )];
    let compacted = inline_loop_local_prefixes(&statements, &names(&["t", "w"]), &names(&["y"]));
    assert_eq!(
        loop_body(&compacted).len(),
        3,
        "the record read was moved past the field write: {}",
        super::liveness_corpus::render(&compacted)
    );
}

/// ```text
/// for k loop
///   w := t;               // reads what the previous OUTER iteration left
///   for i loop
///     t := r[i];
///     y[i] := t;
///   end for;
/// end for;
/// last := w;
/// ```
///
/// The inner loop's prefix definition is dead inside its own body and dead on
/// the path out of it, but the outer loop re-enters at `w := t` without
/// re-running the inner body. Only the enclosing body, asked as a re-entry path
/// of its own, sees that read.
#[test]
fn an_enclosing_back_edge_keeps_an_inner_prefix_definition() {
    let inner = for_loop(
        "i",
        3,
        vec![
            assign("t", indexed("r", "i")),
            rumoca_core::Statement::Assignment {
                comp: element("y", "i", 13),
                value: var("t"),
                span: span(),
            },
        ],
    );
    let statements = vec![
        for_loop("k", 2, vec![assign("w", var("t")), inner]),
        assign("last", var("w")),
    ];
    let compacted =
        inline_loop_local_prefixes(&statements, &names(&["t", "w"]), &names(&["y", "last"]));
    let outer = loop_body(&compacted);
    let rumoca_core::Statement::For { equations, .. } = &outer[1] else {
        panic!(
            "the inner loop was rewritten away: {}",
            super::liveness_corpus::render(&compacted)
        );
    };
    assert_eq!(
        equations.len(),
        2,
        "the definition the outer back edge reads was folded away: {}",
        super::liveness_corpus::render(&compacted)
    );
}
