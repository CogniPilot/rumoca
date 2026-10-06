//! Structural controls for branch-selection captures and their scope proof.

use rumoca_core::{ComponentRefPart, ComponentReference, DefId, ForIndex, SourceMap, Statement};

use super::*;

fn span(start: usize) -> Span {
    let mut sources = SourceMap::new();
    let source = sources.add("captures.mo", &" ".repeat(256));
    Span::from_offsets(source, start, start + 1)
}

fn target(name: &str, id: u32, at: Span) -> ComponentReference {
    ComponentReference::construct(
        true,
        at,
        vec![ComponentRefPart {
            ident: name.into(),
            span: at,
            subs: vec![],
            def_id: DefId::new(id),
        }],
    )
    .unwrap()
}

fn read(name: &str, at: Span) -> Expression {
    Expression::VarRef {
        name: Reference::generated(name),
        subscripts: vec![],
        span: at,
    }
}

fn assign(name: &str, id: u32, value: Expression, at: Span) -> Statement {
    Statement::Assignment {
        comp: target(name, id, at),
        value,
        span: at,
    }
}

fn boolean(value: bool, at: Span) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: at,
    }
}

fn for_loop(binder: &str, body: Vec<Statement>, at: Span) -> Statement {
    Statement::For {
        indices: vec![ForIndex {
            ident: binder.into(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: at,
                }),
                step: None,
                end: Box::new(Expression::Literal {
                    value: Literal::Integer(2),
                    span: at,
                }),
                span: at,
            },
        }],
        equations: body,
        span: at,
    }
}

/// `for i loop if valid then valid := false; for j loop x := 1 end for end if end for`
fn nested_mutable_gate() -> Vec<Statement> {
    let inner = for_loop(
        "j",
        vec![assign("x", 2, boolean(true, span(40)), span(41))],
        span(42),
    );
    let gate = Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: read("valid", span(20)),
            stmts: vec![
                assign("valid", 1, boolean(false, span(30)), span(31)),
                inner,
            ],
        }],
        else_block: None,
        span: span(21),
    };
    vec![for_loop("i", vec![gate], span(10))]
}

#[test]
fn a_conditional_inside_a_loop_body_is_captured_in_that_body() {
    let mut guards = Vec::new();
    let normalized = snapshot_loop_conditionals(&nested_mutable_gate(), &mut guards).unwrap();
    let [guard] = guards.as_slice() else {
        panic!("one capture for one mutable selection: {guards:?}")
    };
    // The capture reads the source predicate at the conditional's position.
    assert_eq!(guard.value, read("valid", span(20)));
    let [Statement::For { equations, .. }] = normalized.as_slice() else {
        panic!("the outer loop is preserved: {normalized:#?}")
    };
    assert!(
        matches!(&equations[0], Statement::Empty { span: at } if *at == guard.span),
        "the capture is the first statement of each outer iteration: {equations:#?}"
    );
    // The predicate's own write and the inner loop body run under the capture,
    // never under the live source predicate.
    let selection = read(guard.target.as_str(), guard.span);
    let Statement::If { cond_blocks, .. } = &equations[1] else {
        panic!("the prefix write is guarded: {equations:#?}")
    };
    assert_eq!(cond_blocks[0].cond, selection);
    let Statement::For {
        equations: inner, ..
    } = &equations[2]
    else {
        panic!("the inner loop stays a loop: {equations:#?}")
    };
    let Statement::If { cond_blocks, .. } = &inner[0] else {
        panic!("the guard moves into the inner loop body: {inner:#?}")
    };
    assert_eq!(cond_blocks[0].cond, selection);
}

#[test]
fn an_immutable_selection_issues_no_capture() {
    let mut guards = vec![GeneratedBooleanDefinition {
        target: VarName::new("returned"),
        value: boolean(true, span(5)),
        span: span(5),
    }];
    let body = vec![
        Statement::Empty { span: span(5) },
        Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: read("returned", span(6)),
                stmts: vec![for_loop(
                    "j",
                    vec![assign("x", 2, boolean(true, span(40)), span(41))],
                    span(42),
                )],
            }],
            else_block: None,
            span: span(7),
        },
    ];
    snapshot_loop_conditionals(&body, &mut guards).unwrap();
    assert_eq!(
        guards.len(),
        1,
        "an immutable condition is its own selection"
    );
}

fn capture(name: &str, at: Span) -> GeneratedBooleanDefinition {
    GeneratedBooleanDefinition {
        target: VarName::new(name),
        value: boolean(true, at),
        span: at,
    }
}

#[test]
fn a_read_before_its_capture_is_refused() {
    let guards = [capture("g", span(50))];
    let body = vec![
        assign("x", 2, read("g", span(51)), span(52)),
        Statement::Empty { span: span(50) },
    ];
    assert!(certify_generated_scopes(&body, &guards).is_err());
}

#[test]
fn a_read_after_its_sequence_ends_is_refused() {
    let guards = [capture("g", span(50))];
    let body = vec![
        for_loop("i", vec![Statement::Empty { span: span(50) }], span(53)),
        assign("x", 2, read("g", span(51)), span(52)),
    ];
    assert!(certify_generated_scopes(&body, &guards).is_err());
}

#[test]
fn a_nested_read_after_its_capture_is_in_scope() {
    let guards = [capture("g", span(50))];
    let body = vec![
        Statement::Empty { span: span(50) },
        for_loop(
            "i",
            vec![assign("x", 2, read("g", span(51)), span(52))],
            span(53),
        ),
    ];
    certify_generated_scopes(&body, &guards).unwrap();
}

#[test]
fn a_repeated_capture_in_one_scope_is_refused() {
    let guards = [capture("g", span(50))];
    let body = vec![
        Statement::Empty { span: span(50) },
        Statement::Empty { span: span(50) },
    ];
    assert!(certify_generated_scopes(&body, &guards).is_err());
}
