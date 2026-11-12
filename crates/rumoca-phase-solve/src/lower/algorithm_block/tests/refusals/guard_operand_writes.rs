//! A store to what an enclosing guard reads keeps what that guard's arm has
//! proven so far, and nothing past its `if`.
//!
//! The GALEC projection stores an element write through a computed subscript,
//! `y[li(2, 1)] := 1.0`, as a call into an index local, a value binding and
//! one branch per candidate element, all inside the source arm. The first
//! candidate store changes what the arm's guard reads (`y[1] > 0.0`); the
//! index and the binding made before it are still assigned on that path, so
//! every later candidate may read them.

use rumoca_ir_galec::ast::{Direction, FunctionCall, FunctionKind, Identifier, Parameter};

use super::super::coverage_regressions::compare_step;
use super::*;

fn chain(
    branches: Vec<(Expression, Vec<Spanned<Statement>>)>,
    else_body: Option<Vec<Spanned<Statement>>>,
) -> Spanned<Statement> {
    Spanned::dummy(Statement::If(IfStatement {
        branches: branches
            .into_iter()
            .map(|(condition, body)| IfBranch {
                condition: Condition::Expression(condition),
                body,
                span: Span::DUMMY,
            })
            .collect(),
        else_body,
    }))
}

fn int(value: i64) -> Expression {
    Expression::Integer(value)
}

fn y(index: i64) -> Reference {
    element_ref("y", int(index))
}

/// `if y[1] > 0.0 then <body> [else <else_body>] end if`: a guard reading `y`.
fn when_positive(
    body: Vec<Spanned<Statement>>,
    else_body: Option<Vec<Spanned<Statement>>>,
) -> Spanned<Statement> {
    chain(
        vec![(bin(BinaryOp::Gt, Expression::Ref(y(1)), r(0.0)), body)],
        else_body,
    )
}

/// `for t in 1:3 loop y[t] := x; end for;`: `y` wholly assigned.
fn fill() -> Spanned<Statement> {
    Spanned::dummy(Statement::for_loop(rumoca_ir_galec::ast::ForLoop::new(
        Some(n("t")),
        int(1),
        None,
        int(3),
        vec![plain(element_ref("y", lsref("t")), lsref("x"))],
    )))
}

/// `y[li(row, column)] := value` as the projection emits it.
fn call_subscript_write(
    row: Expression,
    column: Expression,
    value: Expression,
) -> Vec<Spanned<Statement>> {
    let candidate = |index| plain(y(index), lsref("e"));
    let is = |index| bin(BinaryOp::Eq, lsref("k"), int(index));
    vec![
        Spanned::dummy(Statement::MultiAssignment {
            targets: vec![local("k")],
            call: FunctionCall {
                function: n("li"),
                arguments: vec![row, column],
            },
        }),
        plain(local("e"), value),
        chain(
            vec![(is(1), vec![candidate(1)]), (is(2), vec![candidate(2)])],
            Some(vec![candidate(3)]),
        ),
    ]
}

fn parameter(direction: Direction, decl: VariableDeclaration) -> Parameter {
    Parameter { direction, decl }
}

/// `y[li(2, 1)] := 1.0`.
fn literal_call_subscript_write() -> Vec<Spanned<Statement>> {
    call_subscript_write(int(2), int(1), r(1.0))
}

/// A block with input `v`, output `y` and tunable `period` whose DoStep calls
/// `f(x)` (output `y[3]`, locals `k` (Integer), `e` and `value`, body
/// `statements`) as `(w) := f(v); y := w[1] + 10 * w[2] + 100 * w[3];`; `li(i, j)` is `imin(i, j)` (an arithmetic body may
/// overflow, which the fault proof refuses in a loop that also raises), and
/// is declared when `statements` call it (an unreachable function is
/// invalid).
fn function_block(statements: Vec<Spanned<Statement>>, calls_li: bool) -> Block {
    let integer = |name| VariableDeclaration::scalar(ScalarType::Integer, n(name));
    let li = rumoca_ir_galec::ast::UserFunction {
        kind: FunctionKind::Stateless,
        name: n("li"),
        signals: Vec::new(),
        parameters: vec![
            parameter(Direction::Input, integer("i")),
            parameter(Direction::Input, integer("j")),
            parameter(Direction::Output, integer("k")),
        ],
        locals: Vec::new(),
        statements: vec![plain(
            local("k"),
            builtin_call("imin", vec![lsref("i"), lsref("j")]),
        )],
        span: Span::DUMMY,
    };
    let f = rumoca_ir_galec::ast::UserFunction {
        kind: FunctionKind::Stateless,
        name: n("f"),
        signals: vec![Identifier::new("NAN")],
        parameters: vec![
            parameter(Direction::Input, real_decl(n("x"))),
            parameter(Direction::Output, dims(real_decl(n("y")), &[3])),
        ],
        locals: vec![integer("k"), real_decl(n("e")), real_decl(n("value"))],
        statements,
        span: Span::DUMMY,
    };
    let real_io = |kind, name: &str, start| InterfaceVariable {
        kind,
        decl: real_decl(n(name)),
        start,
    };
    let mut block = Block::new(n("FunctionCaller"));
    block.interface = vec![
        real_io(InterfaceKind::Input, "v", None),
        real_io(InterfaceKind::Output, "y", None),
        real_io(InterfaceKind::TunableParameter, "period", Some(r(0.01))),
    ];
    block.startup.statements = vec![plain(state("y"), r(0.0))];
    block.do_step.statements = vec![
        Spanned::dummy(Statement::MultiAssignment {
            targets: vec![local("w")],
            call: FunctionCall {
                function: n("f"),
                arguments: vec![sref("v")],
            },
        }),
        plain(
            state("y"),
            [(1, 1.0), (2, 10.0), (3, 100.0)]
                .into_iter()
                .map(|(index, weight)| {
                    bin(
                        BinaryOp::Mul,
                        r(weight),
                        Expression::Ref(element_ref("w", int(index))),
                    )
                })
                .reduce(|sum, term| bin(BinaryOp::Add, sum, term))
                .expect("three terms"),
        ),
    ];
    block.do_step.locals = vec![dims(real_decl(n("w")), &[3])];
    block.do_step.signals = vec![PredefinedSignal::Nan];
    block.protected_functions = if calls_li { vec![li, f] } else { vec![f] };
    respan(&mut block);
    block
}

fn lower(block: Block) -> Result<SolveAlgorithmBlock, AlgorithmBlockLowerError> {
    lower_algorithm_block(&package(block), binary64())
}

fn refused(block: Block, why: &str) {
    let Err(error) = lower(block) else {
        panic!("{why}");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(error.to_string().contains("unassigned local"), "{error}");
}

#[test]
fn a_guarded_store_through_a_call_subscript_keeps_its_index_and_value() {
    // Regression: `y[1] := e` changed what `y[1] > 0.0` reads, and the index
    // `k` the next candidate's condition reads was forgotten with the guard.
    let block = || {
        function_block(
            vec![fill(), when_positive(literal_call_subscript_write(), None)],
            true,
        )
    };
    lower(block()).expect("k and e are assigned on the arm's path whatever it stores to y");
    // Guard true: y = {2, 2, 2} and y[li(2, 1)] = y[1] := 1.
    compare_step(block(), &[("v", 2.0), ("period", 0.01)], &[("y", 221.0)]);
    // Guard false: nothing is stored.
    compare_step(block(), &[("v", -2.0), ("period", 0.01)], &[("y", -222.0)]);
}

#[test]
fn the_packed_factorization_loop_shape_refines_and_matches_the_oracle() {
    // factorPSD's packed branch: nested loops, a scalar assigned at the top
    // of each pass, and an `elseif` guard reading y before a store through
    // a call subscript. With `li = imin`, the pass (i, j) stores
    // `x * i / y[j]` into y[j] below the diagonal and `x * i` into y[i] on it.
    let y_j = || Expression::Ref(element_ref("y", lsref("j")));
    let value = || lsref("value");
    let pass = chain(
        vec![(
            bin(BinaryOp::Le, lsref("j"), lsref("i")),
            vec![
                plain(
                    local("value"),
                    bin(
                        BinaryOp::Mul,
                        lsref("x"),
                        builtin_call("real", vec![lsref("i")]),
                    ),
                ),
                chain(
                    vec![
                        (
                            bin(BinaryOp::Eq, lsref("i"), lsref("j")),
                            call_subscript_write(lsref("i"), lsref("j"), value()),
                        ),
                        (
                            bin(BinaryOp::Gt, y_j(), r(0.0)),
                            call_subscript_write(
                                lsref("i"),
                                lsref("j"),
                                bin(BinaryOp::Div, value(), y_j()),
                            ),
                        ),
                        (
                            bin(BinaryOp::Ne, value(), r(0.0)),
                            vec![plain(local("value"), r(0.0))],
                        ),
                    ],
                    None,
                ),
            ],
        )],
        None,
    );
    let block = || {
        function_block(
            vec![
                fill(),
                over("i", 1, 3, vec![over("j", 1, 3, vec![pass.clone()])]),
            ],
            true,
        )
    };
    // x = 2: every guard below the diagonal is true; y = {3, 1.5, 6}.
    compare_step(block(), &[("v", 2.0), ("period", 0.01)], &[("y", 618.0)]);
    // x = -2: every such guard is false; y = {-2, -4, -6}.
    compare_step(block(), &[("v", -2.0), ("period", 0.01)], &[("y", -642.0)]);
}

#[test]
fn a_call_subscript_store_defines_no_unassigned_array() {
    // Without the fill the guard itself reads an unassigned element.
    refused(
        function_block(
            vec![when_positive(literal_call_subscript_write(), None), fill()],
            true,
        ),
        "the guard reads y[1] before anything assigns it",
    );
    // One candidate element written is not y[1] written.
    let mut statements = literal_call_subscript_write();
    statements.extend([plain(local("e"), Expression::Ref(y(1))), fill()]);
    refused(
        function_block(statements, true),
        "y[1] is read where only y[k] is written",
    );
}

#[test]
fn a_fact_under_a_changed_guard_never_reaches_a_later_evaluation() {
    // Each program reads e unassigned for some v (the oracle faults there).
    let read_e = || plain(element_ref("y", int(2)), lsref("e"));
    let assign_e = || plain(local("e"), r(1.0));
    // The false arm makes the guard true without assigning e; the fact from
    // the true arm must not reach the later true arm.
    refused(
        function_block(
            vec![
                fill(),
                when_positive(vec![assign_e()], Some(vec![plain(y(1), r(1.0))])),
                when_positive(vec![read_e()], None),
            ],
            false,
        ),
        "the false arm changed the guard and never assigned e",
    );
    // The true arm makes the guard false without assigning e; the fact from
    // the false arm must not reach the later false arm, even when that arm
    // evaluated the guard again (regression: the re-evaluation once let the
    // false arm record e under the bare guard text).
    for between in [Vec::new(), vec![when_positive(Vec::new(), None)]] {
        let mut else_body = between;
        else_body.push(assign_e());
        refused(
            function_block(
                vec![
                    fill(),
                    when_positive(vec![plain(y(1), r(-1.0))], Some(else_body)),
                    when_positive(Vec::new(), Some(vec![read_e()])),
                ],
                false,
            ),
            "the true arm changed the guard and never assigned e",
        );
    }
}

#[test]
fn an_earlier_fact_under_the_same_guard_holds_until_its_arm_ends() {
    // e is assigned under `y[1] > 0.0`; a second evaluation with nothing
    // changed in between has the same value, so its arm may read e after it
    // stores to y. Past that arm the fact is gone: a third evaluation is not
    // the one it was proven under.
    let read_e = || plain(element_ref("y", int(2)), lsref("e"));
    let defined_then_read = || {
        vec![
            fill(),
            when_positive(vec![plain(local("e"), r(1.0))], None),
            when_positive(vec![plain(y(1), r(-1.0)), read_e()], None),
        ]
    };
    lower(function_block(defined_then_read(), false))
        .expect("the second evaluation repeats the first, so e is assigned in its arm");
    let mut statements = defined_then_read();
    statements.push(when_positive(vec![read_e()], None));
    refused(
        function_block(statements, false),
        "y[1] > 0.0 was evaluated again after y[1] changed",
    );
}
