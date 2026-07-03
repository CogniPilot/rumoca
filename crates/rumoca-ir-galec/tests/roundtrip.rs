//! WI-6 round-trip property suite (contract §5, §6, §7): the single place that
//! earns the SPEC_0034 GAL-021 "GALEC language conformance" rung by proving the
//! parser (`parse` / `parse_expression`) is the exact inverse of the printer
//! (`print_block` / `print_expression`) over the printable surface.
//!
//! Three properties are asserted here:
//!
//! * **Primary — text-level idempotence (§5.1):** for every printable AST `a`,
//!   `print(parse(print(a))) == print(a)`, i.e. `print ∘ parse ∘ print == print`.
//!   This sidesteps the manifest-bound `start` (already absent from printed
//!   `.alg`) and the non-injectivity of `Expression::Paren`, and is the property
//!   that unlocks the conformance rung. Asserted over ALL fixtures F1–F11.
//! * **Secondary — AST-level equality (§5.2):** `parse(print(a)) == a` on a
//!   *canonical generator* — ASTs with `start: None`, finite Reals, and only
//!   observable parentheses. Redundant `Paren` nodes are stripped to a fixpoint
//!   on both sides before comparison (the printer re-inserts grouping from
//!   precedence, so `print` is not injective in operand position). Run over
//!   F1–F11 and over the `tests/validate.rs`-style canonical blocks
//!   (`minimal`, `estimator`).
//! * **Negative parse cases (§7.3):** each malformed / non-conformant input maps
//!   to a specific typed `GalecParseError` variant carrying the stable code
//!   EG050–EG055.
//!
//! Every fixture is a Rust-built `ast::Block` / `ast::Expression` value produced
//! by our own in-tree builders (the same independently-authored style as
//! `tests/block_print.rs`, `tests/expression_print.rs`, `tests/validate.rs`).
//! No CC-BY-SA standard prose or examples are reproduced (GAL-023 / D10).

#![cfg(feature = "parse")]

use rumoca_ir_galec::ast::{
    BinaryOp, Block, BlockMethod, Condition, Dimension, Direction, Expression, ForLoop,
    FunctionCall, FunctionKind, Identifier, IfBranch, IfExpression, IfStatement, InterfaceKind,
    InterfaceVariable, LimitTarget, Name, Parameter, PredefinedSignal, ProtectedEntity,
    ProtectedKind, RangeAttributes, RefPart, Reference, ScalarType, SignalCheck, SignalTest,
    StateCompartment, Statement, UserFunction, VariableDeclaration,
};
use rumoca_ir_galec::parse::{GalecParseError, parse, parse_expression};
use rumoca_ir_galec::{print_block, print_expression};

// ===========================================================================
// Builders (our own, mirroring tests/block_print.rs + tests/validate.rs style)
// ===========================================================================

fn n(name: &str) -> Name {
    Name::ident(name)
}

fn id(name: &str) -> Identifier {
    Identifier::new(name)
}

fn real_decl(name: Name) -> VariableDeclaration {
    VariableDeclaration::scalar(ScalarType::Real, name)
}

fn ranged(mut decl: VariableDeclaration, min: f64, max: f64) -> VariableDeclaration {
    decl.range = RangeAttributes {
        min: Some(Expression::Real(min)),
        max: Some(Expression::Real(max)),
    };
    decl
}

fn dims(mut decl: VariableDeclaration, sizes: &[i64]) -> VariableDeclaration {
    decl.dimensions = sizes
        .iter()
        .map(|s| Dimension::Expr(Expression::Integer(*s)))
        .collect();
    decl
}

fn interface(
    kind: InterfaceKind,
    decl: VariableDeclaration,
    start: Option<Expression>,
) -> InterfaceVariable {
    InterfaceVariable { kind, decl, start }
}

fn entity(
    kind: ProtectedKind,
    decl: VariableDeclaration,
    start: Option<Expression>,
) -> ProtectedEntity {
    ProtectedEntity { kind, decl, start }
}

fn state(name: &str) -> Reference {
    Reference::state(n(name))
}

fn stateq(content: &str) -> Reference {
    Reference::state(Name::quoted(content))
}

fn state_idx(name: &str, subscripts: Vec<Expression>) -> Reference {
    Reference::State(vec![RefPart {
        name: n(name),
        subscripts,
    }])
}

fn local(name: &str) -> Reference {
    Reference::local(n(name))
}

fn local_idx(name: &str, index: i64) -> Reference {
    Reference::Local(RefPart {
        name: n(name),
        subscripts: vec![Expression::Integer(index)],
    })
}

fn sref(name: &str) -> Expression {
    Expression::Ref(state(name))
}

fn lref(name: &str) -> Expression {
    Expression::Ref(local(name))
}

fn r(value: f64) -> Expression {
    Expression::Real(value)
}

fn int(value: i64) -> Expression {
    Expression::Integer(value)
}

fn bin(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::binary(op, lhs, rhs)
}

fn assign(target: Reference, value: Expression) -> Statement {
    Statement::Assignment { target, value }
}

fn fcall(function: &str, arguments: Vec<Expression>) -> FunctionCall {
    FunctionCall {
        function: n(function),
        arguments,
    }
}

fn ecall(function: &str, arguments: Vec<Expression>) -> Expression {
    Expression::Call(fcall(function, arguments))
}

fn call_stmt(function: &str, arguments: Vec<Expression>) -> Statement {
    Statement::Call(fcall(function, arguments))
}

fn in_real(name: &str) -> Parameter {
    Parameter {
        direction: Direction::Input,
        decl: real_decl(n(name)),
    }
}

fn out_real(name: &str) -> Parameter {
    Parameter {
        direction: Direction::Output,
        decl: real_decl(n(name)),
    }
}

// ===========================================================================
// Round-trip harness
// ===========================================================================

/// Primary property (§5.1): `print(parse(print(b))) == print(b)`.
fn assert_block_text_stable(block: &Block) {
    let text = print_block(block).expect("fixture must print");
    let parsed = parse(&text, "roundtrip")
        .unwrap_or_else(|err| panic!("parse failed:\n{text}\nerror: {err}"));
    let reprinted = print_block(&parsed).expect("reparsed block must print");
    assert_eq!(reprinted, text, "print-stability failed");
}

/// Secondary property (§5.2): `parse(print(b)) == b`, both sides normalized —
/// manifest-bound `start` values (absent from `.alg` syntax) to `None`, and
/// redundant `Paren` nodes stripped (the printer re-inserts grouping from
/// precedence, so `print` is not injective in operand position).
fn assert_block_ast_stable(block: &Block) {
    let text = print_block(block).expect("fixture must print");
    let mut parsed = parse(&text, "roundtrip").expect("fixture must parse");
    normalize(&mut parsed);
    let mut expected = block.clone();
    normalize(&mut expected);
    assert_eq!(parsed, expected, "ast round-trip failed for:\n{text}");
}

/// Both properties over a single block fixture.
fn assert_block_round_trips(block: &Block) {
    assert_block_text_stable(block);
    assert_block_ast_stable(block);
}

fn printed_expr(expression: &Expression) -> String {
    print_expression(expression).expect("fixture is printable")
}

fn reparse_expr(text: &str) -> Expression {
    parse_expression(text, "roundtrip")
        .unwrap_or_else(|err| panic!("parse failed for {text:?}: {err}"))
}

/// Primary property over an expression: `print(parse(print(e))) == print(e)`.
fn assert_expr_text_stable(expression: &Expression) {
    let text = printed_expr(expression);
    let reparsed = reparse_expr(&text);
    assert_eq!(
        printed_expr(&reparsed),
        text,
        "print-stability failed for {text:?}"
    );
}

/// Secondary property over an expression: `parse(print(e)) == e` (only where
/// `print` is injective — no printer-inserted parentheses).
fn assert_expr_ast_stable(expression: &Expression) {
    let text = printed_expr(expression);
    let reparsed = reparse_expr(&text);
    assert_eq!(&reparsed, expression, "ast round-trip failed for {text:?}");
    assert_eq!(printed_expr(&reparsed), text);
}

/// Normalize a block to the canonical comparison form: drop every
/// manifest-bound `start` and strip all `Paren` wrappers (pure syntactic
/// grouping already carried by the `Binary` tree shape).
fn normalize(block: &mut Block) {
    for variable in &mut block.interface {
        variable.start = None;
        norm_decl(&mut variable.decl);
    }
    for entity in &mut block.protected {
        entity.start = None;
        norm_decl(&mut entity.decl);
    }
    for compartment in &mut block.compartments {
        for entity in &mut compartment.entities {
            entity.start = None;
            norm_decl(&mut entity.decl);
        }
    }
    for function in block
        .protected_functions
        .iter_mut()
        .chain(block.public_functions.iter_mut())
    {
        for parameter in &mut function.parameters {
            norm_decl(&mut parameter.decl);
        }
        norm_body(&mut function.locals, &mut function.statements);
    }
    norm_body(&mut block.startup.locals, &mut block.startup.statements);
    norm_body(
        &mut block.recalibrate.locals,
        &mut block.recalibrate.statements,
    );
    norm_body(&mut block.do_step.locals, &mut block.do_step.statements);
}

fn norm_body(locals: &mut [VariableDeclaration], statements: &mut [Statement]) {
    for local in locals {
        norm_decl(local);
    }
    for statement in statements {
        norm_stmt(statement);
    }
}

fn norm_decl(decl: &mut VariableDeclaration) {
    for dimension in &mut decl.dimensions {
        if let Dimension::Expr(e) = dimension {
            norm_expr(e);
        }
    }
    if let Some(min) = &mut decl.range.min {
        norm_expr(min);
    }
    if let Some(max) = &mut decl.range.max {
        norm_expr(max);
    }
}

fn norm_stmt(statement: &mut Statement) {
    match statement {
        Statement::Assignment { target, value } => {
            norm_ref(target);
            norm_expr(value);
        }
        Statement::MultiAssignment { targets, call } => {
            targets.iter_mut().for_each(norm_ref);
            call.arguments.iter_mut().for_each(norm_expr);
        }
        Statement::Call(call) => call.arguments.iter_mut().for_each(norm_expr),
        Statement::If(if_statement) => {
            for branch in &mut if_statement.branches {
                norm_condition(&mut branch.condition);
                branch.body.iter_mut().for_each(norm_stmt);
            }
            if let Some(else_body) = &mut if_statement.else_body {
                else_body.iter_mut().for_each(norm_stmt);
            }
        }
        Statement::For(for_loop) => {
            norm_expr(&mut for_loop.start);
            if let Some(step) = &mut for_loop.step {
                norm_expr(step);
            }
            norm_expr(&mut for_loop.stop);
            for_loop.body.iter_mut().for_each(norm_stmt);
        }
        Statement::Limit(targets) => {
            for target in targets {
                if let LimitTarget::Reference(reference) = target {
                    norm_ref(reference);
                }
            }
        }
        Statement::Signal(_) => {}
    }
}

fn norm_condition(condition: &mut Condition) {
    match condition {
        Condition::Expression(e) => norm_expr(e),
        Condition::SignalCheck(check) => {
            if let Some(fallback) = &mut check.fallback {
                norm_expr(fallback);
            }
        }
    }
}

fn norm_ref(reference: &mut Reference) {
    let parts = match reference {
        Reference::Local(part) => std::slice::from_mut(part),
        Reference::State(parts) => parts.as_mut_slice(),
    };
    for part in parts {
        part.subscripts.iter_mut().for_each(norm_expr);
    }
}

fn norm_expr(expression: &mut Expression) {
    // Collapse `Paren(inner)` to a normalized `inner`, repeatedly.
    while let Expression::Paren(inner) = expression {
        *expression = std::mem::replace(inner.as_mut(), Expression::Bool(false));
    }
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
        Expression::Ref(reference) | Expression::Neg(reference) => norm_ref(reference),
        Expression::Size { array, dimension } => {
            norm_ref(array);
            norm_expr(dimension);
        }
        Expression::Call(call) => call.arguments.iter_mut().for_each(norm_expr),
        Expression::Paren(_) => unreachable!("Paren collapsed above"),
        Expression::If(if_expression) => {
            for (condition, value) in &mut if_expression.branches {
                norm_expr(condition);
                norm_expr(value);
            }
            norm_expr(&mut if_expression.else_value);
        }
        Expression::Array(elements) => elements.iter_mut().for_each(norm_expr),
        Expression::Not(inner) => norm_expr(inner),
        Expression::Binary { lhs, rhs, .. } => {
            norm_expr(lhs);
            norm_expr(rhs);
        }
    }
}

// ===========================================================================
// Expression cascade coverage (F11 machinery + §7.1 per-production checks)
// ===========================================================================

#[test]
fn cascade_levels_round_trip() {
    // Each level's associative chain prints bare and reparses to the same AST.
    assert_expr_ast_stable(&bin(
        BinaryOp::Or,
        bin(BinaryOp::Or, lref("a"), lref("b")),
        lref("c"),
    ));
    assert_expr_ast_stable(&bin(
        BinaryOp::And,
        bin(BinaryOp::And, lref("a"), lref("b")),
        lref("c"),
    ));
    assert_expr_ast_stable(&bin(BinaryOp::Eq, lref("a"), lref("b")));
    assert_expr_ast_stable(&bin(BinaryOp::Ne, lref("a"), lref("b")));
    assert_expr_ast_stable(&bin(BinaryOp::Lt, lref("a"), lref("b")));
    assert_expr_ast_stable(&bin(BinaryOp::Ge, lref("a"), lref("b")));
    assert_expr_ast_stable(&bin(
        BinaryOp::Add,
        bin(BinaryOp::Sub, lref("a"), lref("b")),
        lref("c"),
    ));
    assert_expr_ast_stable(&bin(
        BinaryOp::Mul,
        bin(BinaryOp::Div, lref("a"), lref("b")),
        lref("c"),
    ));
    // Right-associative power.
    assert_expr_ast_stable(&bin(
        BinaryOp::Pow,
        lref("a"),
        bin(BinaryOp::Pow, lref("b"), lref("c")),
    ));
}

#[test]
fn constants_round_trip() {
    assert_expr_ast_stable(&Expression::Bool(true));
    assert_expr_ast_stable(&Expression::Bool(false));
    assert_expr_ast_stable(&Expression::Integer(42));
    assert_expr_ast_stable(&Expression::Integer(-7));
    assert_expr_ast_stable(&Expression::Real(3.5));
    assert_expr_ast_stable(&Expression::Real(-2.5));
}

#[test]
fn references_round_trip() {
    assert_expr_ast_stable(&lref("x"));
    assert_expr_ast_stable(&sref("y"));
    assert_expr_ast_stable(&Expression::Ref(local_idx("solution", 1)));
    assert_expr_ast_stable(&Expression::Ref(Reference::State(vec![
        RefPart::plain(n("shaft")),
        RefPart {
            name: n("gear"),
            subscripts: vec![int(2)],
        },
    ])));
}

// ===========================================================================
// F1 — scalar assignment; F2 — quoted state commit
// ===========================================================================

fn minimal_do_step(do_step: BlockMethod) -> Block {
    Block {
        do_step,
        ..Block::new(n("Minimal"))
    }
}

#[test]
fn f1_scalar_assignment() {
    let block = minimal_do_step(BlockMethod {
        locals: vec![real_decl(n("k")), real_decl(n("e"))],
        statements: vec![assign(local("x"), bin(BinaryOp::Mul, lref("k"), lref("e")))],
        ..Default::default()
    });
    assert_block_round_trips(&block);
}

#[test]
fn f2_quoted_state_commit() {
    let block = minimal_do_step(BlockMethod {
        locals: vec![real_decl(n("trackingError"))],
        statements: vec![assign(
            stateq("previous(trackingError)"),
            lref("trackingError"),
        )],
        ..Default::default()
    });
    assert_block_round_trips(&block);
}

// ===========================================================================
// F3 — if-expression with mandatory `else`, self-parens, negative-literal parens
// ===========================================================================

#[test]
fn f3_if_expression() {
    let limiter = Expression::If(IfExpression {
        branches: vec![
            (bin(BinaryOp::Gt, lref("u"), r(50.0)), r(50.0)),
            (bin(BinaryOp::Lt, lref("u"), r(-50.0)), r(-50.0)),
        ],
        else_value: Box::new(lref("u")),
    });
    // The printer inserts self-parens + negative-literal operand parens.
    assert_eq!(
        printed_expr(&limiter),
        "(if u > 50.0 then 50.0 elseif u < (-50.0) then -50.0 else u)"
    );
    assert_expr_text_stable(&limiter);
}

// ===========================================================================
// F4 — builtin calls: any-name callee, subscripted argument, zero-arg
// ===========================================================================

#[test]
fn f4_builtin_calls() {
    assert_expr_ast_stable(&ecall("absolute", vec![lref("v")]));
    assert_expr_ast_stable(&ecall(
        "isNaN",
        vec![Expression::Ref(local_idx("solution", 1))],
    ));
    assert_expr_ast_stable(&ecall("sqrt", vec![lref("x")]));
    assert_expr_ast_stable(&ecall("now", vec![]));
}

// ===========================================================================
// F5 — quoted hierarchical names: atomic quoted lexeme (Flag G),
//      structural vs literal `[ ]` / `.`
// ===========================================================================

#[test]
fn f5_quoted_names() {
    // self.'shaft[2].gear'[1].w — inner [2] is literal lexeme text; outer [1]
    // and .w are structural.
    let nested = Expression::Ref(Reference::State(vec![
        RefPart {
            name: Name::quoted("shaft[2].gear"),
            subscripts: vec![int(1)],
        },
        RefPart::plain(n("w")),
    ]));
    assert_eq!(printed_expr(&nested), "self.'shaft[2].gear'[1].w");
    assert_expr_ast_stable(&nested);

    assert_expr_ast_stable(&Expression::Ref(Reference::state(Name::quoted(
        "previous(feedback.y)",
    ))));
    assert_expr_ast_stable(&Expression::Ref(Reference::local(Name::quoted(
        "derivative(PID.I.x)",
    ))));
}

// ===========================================================================
// F6 — whole-array (matrix) assignment
// ===========================================================================

#[test]
fn f6_matrix_assignment() {
    let zero_row = || Expression::Array(vec![r(0.0), r(0.0), r(0.0)]);
    let block = Block {
        protected: vec![entity(
            ProtectedKind::State,
            dims(real_decl(n("accumulator")), &[2, 3]),
            None,
        )],
        startup: BlockMethod {
            statements: vec![assign(
                state("accumulator"),
                Expression::Array(vec![zero_row(), zero_row()]),
            )],
            ..Default::default()
        },
        ..Block::new(n("Matrix"))
    };
    assert_block_round_trips(&block);
}

// ===========================================================================
// F7 — full projected PID-shaped block
// ===========================================================================

fn dependent_gain_update() -> Statement {
    assign(
        state("integralGain"),
        bin(BinaryOp::Div, sref("gain"), sref("integralTime")),
    )
}

fn pid_do_step() -> BlockMethod {
    let first_tick_branch = IfBranch {
        condition: Condition::Expression(sref("firstTick")),
        body: vec![
            assign(state("firstTick"), Expression::Bool(false)),
            assign(local("derivativeEstimate"), r(0.0)),
        ],
    };
    let error_delta = bin(
        BinaryOp::Sub,
        lref("trackingError"),
        Expression::Ref(stateq("previous(trackingError)")),
    );
    let integrate = vec![
        assign(
            local("derivativeEstimate"),
            bin(
                BinaryOp::Div,
                Expression::Paren(Box::new(error_delta)),
                sref("period"),
            ),
        ),
        assign(
            state("integralState"),
            bin(
                BinaryOp::Add,
                sref("integralState"),
                bin(
                    BinaryOp::Mul,
                    bin(BinaryOp::Mul, sref("integralGain"), lref("trackingError")),
                    sref("period"),
                ),
            ),
        ),
    ];
    let limiter = Expression::If(IfExpression {
        branches: vec![
            (bin(BinaryOp::Gt, lref("unboundedDrive"), r(50.0)), r(50.0)),
            (
                bin(BinaryOp::Lt, lref("unboundedDrive"), r(-50.0)),
                r(-50.0),
            ),
        ],
        else_value: Box::new(lref("unboundedDrive")),
    });
    BlockMethod {
        signals: vec![],
        locals: vec![
            real_decl(n("trackingError")),
            real_decl(n("derivativeEstimate")),
            real_decl(n("unboundedDrive")),
        ],
        statements: vec![
            assign(
                local("trackingError"),
                bin(BinaryOp::Sub, sref("speedSetpoint"), sref("speedMeasured")),
            ),
            Statement::If(IfStatement {
                branches: vec![first_tick_branch],
                else_body: Some(integrate),
            }),
            Statement::Limit(vec![LimitTarget::Reference(state("integralState"))]),
            assign(
                local("unboundedDrive"),
                bin(
                    BinaryOp::Add,
                    bin(
                        BinaryOp::Add,
                        bin(BinaryOp::Mul, sref("gain"), lref("trackingError")),
                        sref("integralState"),
                    ),
                    bin(
                        BinaryOp::Mul,
                        sref("derivativeTime"),
                        lref("derivativeEstimate"),
                    ),
                ),
            ),
            assign(state("drive"), limiter),
            assign(stateq("previous(trackingError)"), lref("trackingError")),
        ],
    }
}

fn pid_block() -> Block {
    Block {
        interface: vec![
            interface(
                InterfaceKind::Input,
                ranged(real_decl(n("speedSetpoint")), -500.0, 500.0),
                None,
            ),
            interface(
                InterfaceKind::Input,
                ranged(real_decl(n("speedMeasured")), -500.0, 500.0),
                None,
            ),
            interface(
                InterfaceKind::Output,
                ranged(real_decl(n("drive")), -50.0, 50.0),
                Some(r(0.0)),
            ),
            interface(
                InterfaceKind::TunableParameter,
                ranged(real_decl(n("gain")), 0.0, 100.0),
                Some(r(2.5)),
            ),
            interface(
                InterfaceKind::TunableParameter,
                ranged(real_decl(n("integralTime")), 0.000_001, 1000.0),
                Some(r(0.5)),
            ),
            interface(
                InterfaceKind::TunableParameter,
                ranged(real_decl(n("derivativeTime")), 0.0, 10.0),
                Some(r(0.01)),
            ),
            interface(
                InterfaceKind::TunableParameter,
                ranged(real_decl(n("period")), 0.000_001, 1.0),
                Some(r(0.005)),
            ),
        ],
        protected: vec![
            entity(
                ProtectedKind::DependentParameter,
                real_decl(n("integralGain")),
                None,
            ),
            entity(
                ProtectedKind::State,
                ranged(real_decl(n("integralState")), -100.0, 100.0),
                Some(r(0.0)),
            ),
            entity(
                ProtectedKind::State,
                real_decl(Name::quoted("previous(trackingError)")),
                Some(r(0.0)),
            ),
            entity(
                ProtectedKind::State,
                VariableDeclaration::scalar(ScalarType::Boolean, n("firstTick")),
                Some(Expression::Bool(true)),
            ),
        ],
        startup: BlockMethod {
            statements: vec![
                assign(state("gain"), r(2.5)),
                assign(state("integralTime"), r(0.5)),
                dependent_gain_update(),
                assign(state("integralState"), r(0.0)),
                assign(stateq("previous(trackingError)"), r(0.0)),
                assign(state("firstTick"), Expression::Bool(true)),
                assign(state("drive"), r(0.0)),
            ],
            ..Default::default()
        },
        recalibrate: BlockMethod {
            statements: vec![dependent_gain_update()],
            ..Default::default()
        },
        do_step: pid_do_step(),
        ..Block::new(n("RateController"))
    }
}

#[test]
fn f7_pid_block() {
    assert_block_round_trips(&pid_block());
}

// ===========================================================================
// F8 — matrix averager with nested for-loops and a state compartment
// ===========================================================================

fn matrix_do_step() -> BlockMethod {
    let size_dim = |d: i64| Expression::Size {
        array: state("accumulator"),
        dimension: Box::new(Expression::Integer(d)),
    };
    let ij = || vec![lref("i"), lref("j")];
    let inner = Statement::For(ForLoop {
        iterator: Some(n("j")),
        start: int(1),
        step: None,
        stop: size_dim(2),
        body: vec![
            assign(
                state_idx("accumulator", ij()),
                Expression::Ref(state_idx("samples", ij())),
            ),
            assign(
                local("total"),
                bin(
                    BinaryOp::Add,
                    lref("total"),
                    Expression::Ref(state_idx("accumulator", ij())),
                ),
            ),
        ],
    });
    let outer = Statement::For(ForLoop {
        iterator: Some(n("i")),
        start: int(1),
        step: Some(int(1)),
        stop: size_dim(1),
        body: vec![
            assign(local("total"), r(0.0)),
            inner,
            assign(
                state_idx("rowMean", vec![lref("i")]),
                bin(BinaryOp::Div, lref("total"), r(3.0)),
            ),
        ],
    });
    BlockMethod {
        locals: vec![real_decl(n("total"))],
        statements: vec![outer],
        ..Default::default()
    }
}

fn matrix_averager() -> Block {
    Block {
        interface: vec![
            interface(
                InterfaceKind::Input,
                ranged(dims(real_decl(n("samples")), &[2, 3]), -1000.0, 1000.0),
                None,
            ),
            interface(
                InterfaceKind::Output,
                dims(real_decl(n("rowMean")), &[2]),
                Some(Expression::Array(vec![r(0.0), r(0.0)])),
            ),
        ],
        compartments: vec![StateCompartment {
            name: n("Cfg"),
            entities: vec![
                entity(
                    ProtectedKind::Constant,
                    real_decl(n("divisor")),
                    Some(r(3.0)),
                ),
                entity(
                    ProtectedKind::DependentParameter,
                    real_decl(n("scale")),
                    None,
                ),
            ],
        }],
        protected: vec![entity(
            ProtectedKind::State,
            dims(real_decl(n("accumulator")), &[2, 3]),
            None,
        )],
        startup: BlockMethod {
            statements: vec![assign(
                state("accumulator"),
                Expression::Array(vec![
                    Expression::Array(vec![r(0.0), r(0.0), r(0.0)]),
                    Expression::Array(vec![r(0.0), r(0.0), r(0.0)]),
                ]),
            )],
            ..Default::default()
        },
        do_step: matrix_do_step(),
        ..Block::new(n("MatrixAverager"))
    }
}

#[test]
fn f8_matrix_averager() {
    assert_block_round_trips(&matrix_averager());
}

// ===========================================================================
// F9 — signal-guard block: user function, MultiAssignment, signal checks
// ===========================================================================

fn classify_function() -> UserFunction {
    UserFunction {
        kind: FunctionKind::Stateless,
        name: n("classify"),
        signals: vec![id("sensorFault")],
        parameters: vec![
            in_real("v"),
            Parameter {
                direction: Direction::Output,
                decl: VariableDeclaration::scalar(ScalarType::Boolean, n("ok")),
            },
        ],
        locals: vec![],
        statements: vec![Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(bin(
                    BinaryOp::Gt,
                    ecall("absolute", vec![lref("v")]),
                    r(99.0),
                )),
                body: vec![
                    Statement::Signal(vec![id("sensorFault")]),
                    assign(local("ok"), Expression::Bool(false)),
                ],
            }],
            else_body: Some(vec![assign(local("ok"), Expression::Bool(true))]),
        })],
    }
}

fn signal_guard_statements() -> Vec<Statement> {
    let identity = Expression::Array(vec![
        Expression::Array(vec![r(2.0), r(0.0)]),
        Expression::Array(vec![r(0.0), r(4.0)]),
    ]);
    let nan_raise = IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(ecall(
                "isNaN",
                vec![Expression::Ref(local_idx("solution", 1))],
            )),
            body: vec![Statement::Signal(vec![id("NAN")])],
        }],
        else_body: None,
    };
    let guard = IfStatement {
        branches: vec![
            IfBranch {
                condition: Condition::SignalCheck(SignalCheck {
                    closure: Some(id("s")),
                    test: Some(SignalTest {
                        negated: false,
                        signals: vec![id("sensorFault"), id("SOLVE_LINEAR_EQUATIONS_FAILED")],
                    }),
                    fallback: Some(Expression::Not(Box::new(lref("ok")))),
                }),
                body: vec![assign(state("filtered"), r(0.0))],
            },
            IfBranch {
                condition: Condition::SignalCheck(SignalCheck {
                    closure: None,
                    test: Some(SignalTest {
                        negated: true,
                        signals: vec![id("sensorFault")],
                    }),
                    fallback: None,
                }),
                body: vec![assign(
                    state("filtered"),
                    Expression::Ref(local_idx("solution", 1)),
                )],
            },
        ],
        else_body: Some(vec![assign(
            state("filtered"),
            bin(
                BinaryOp::Div,
                Expression::Ref(local_idx("solution", 1)),
                r(2.0),
            ),
        )]),
    };
    let final_catch = IfStatement {
        branches: vec![IfBranch {
            condition: Condition::SignalCheck(SignalCheck {
                closure: None,
                test: None,
                fallback: None,
            }),
            body: vec![
                assign(state("filtered"), r(0.0)),
                Statement::Signal(vec![id("NAN")]),
            ],
        }],
        else_body: None,
    };
    vec![
        assign(local("ok"), ecall("classify", vec![sref("reading")])),
        Statement::MultiAssignment {
            targets: vec![local("lu"), local("pivots")],
            call: fcall("luFactorize", vec![identity]),
        },
        Statement::MultiAssignment {
            targets: vec![local("solution")],
            call: fcall(
                "luSolve",
                vec![
                    lref("lu"),
                    lref("pivots"),
                    Expression::Array(vec![sref("reading"), r(0.0)]),
                ],
            ),
        },
        Statement::If(nan_raise),
        Statement::If(guard),
        Statement::If(final_catch),
        Statement::Limit(vec![LimitTarget::SelfState]),
    ]
}

fn signal_guard() -> Block {
    Block {
        interface: vec![
            interface(
                InterfaceKind::Input,
                ranged(real_decl(n("reading")), -100.0, 100.0),
                None,
            ),
            interface(
                InterfaceKind::Output,
                real_decl(n("filtered")),
                Some(r(0.0)),
            ),
        ],
        error_signals: vec![id("sensorFault")],
        protected_functions: vec![classify_function()],
        startup: BlockMethod {
            statements: vec![assign(state("filtered"), r(0.0))],
            ..Default::default()
        },
        do_step: BlockMethod {
            signals: vec![PredefinedSignal::Nan],
            locals: vec![
                VariableDeclaration::scalar(ScalarType::Boolean, n("ok")),
                dims(real_decl(n("lu")), &[2, 2]),
                dims(
                    VariableDeclaration::scalar(ScalarType::Integer, n("pivots")),
                    &[2],
                ),
                dims(real_decl(n("solution")), &[2]),
            ],
            statements: signal_guard_statements(),
        },
        ..Block::new(n("SignalGuard"))
    }
}

#[test]
fn f9_signal_guard() {
    assert_block_round_trips(&signal_guard());
}

// ===========================================================================
// F10 — T7 real-literal accept / reject table
// ===========================================================================

/// The lexer accepts each conformant spelling and yields the exact `f64`, which
/// then prints stably.
#[test]
fn f10_real_literals_accepted() {
    let accepted = [
        "1.0",
        "0.0",
        "-1.5",
        "3.14159",
        "0.5",
        "1.0e+5",
        "2.5e-3",
        "-0.0",
        "123.456e+10",
        "-7.25e-30",
        "0.000001",
    ];
    for text in accepted {
        let expected = text.parse::<f64>().expect("host f64 parse");
        assert_eq!(
            reparse_expr(text),
            Expression::Real(expected),
            "expected real accept: {text}"
        );
        assert_expr_text_stable(&Expression::Real(expected));
    }
}

/// A non-conformant spelling never parses as a single Real carrying its
/// host-parsed value (the lexer stops early or rejects).
#[test]
fn f10_real_literals_rejected() {
    let rejected = ["1e5", "1.", "1.0e5", "01.0", "1.0E+5", "+1.0", "-.5"];
    for text in rejected {
        let intended = Expression::Real(text.parse::<f64>().expect("host f64 parse"));
        assert_ne!(
            parse_expression(text, "roundtrip").ok(),
            Some(intended),
            "non-conformant real must not parse as its full value: {text}"
        );
    }
}

// ===========================================================================
// F11 — expression micro-table
// ===========================================================================

#[test]
fn f11_expression_micro_table() {
    // -b : injective.
    assert_expr_ast_stable(&Expression::Neg(local("b")));

    // (a ^ 2) * b : cross-class, printer inserts parens → text-level only.
    assert_expr_text_stable(&bin(
        BinaryOp::Mul,
        bin(BinaryOp::Pow, lref("a"), int(2)),
        lref("b"),
    ));

    // (a or b) and c : cross-class → text-level only.
    assert_expr_text_stable(&bin(
        BinaryOp::And,
        bin(BinaryOp::Or, lref("a"), lref("b")),
        lref("c"),
    ));

    // a + b + c (left) vs a + (b + c) (right).
    assert_expr_ast_stable(&bin(
        BinaryOp::Add,
        bin(BinaryOp::Add, lref("a"), lref("b")),
        lref("c"),
    ));
    assert_expr_text_stable(&bin(
        BinaryOp::Add,
        lref("a"),
        bin(BinaryOp::Add, lref("b"), lref("c")),
    ));

    // a ^ b ^ c (right) vs (a ^ b) ^ c (left).
    assert_expr_ast_stable(&bin(
        BinaryOp::Pow,
        lref("a"),
        bin(BinaryOp::Pow, lref("b"), lref("c")),
    ));
    assert_expr_text_stable(&bin(
        BinaryOp::Pow,
        bin(BinaryOp::Pow, lref("a"), lref("b")),
        lref("c"),
    ));

    // not (b1 or b2) : parser re-parenthesizes into a Paren node → text-level.
    assert_expr_text_stable(&Expression::Not(Box::new(bin(
        BinaryOp::Or,
        lref("b1"),
        lref("b2"),
    ))));

    // size(self.table, 1) : injective.
    assert_expr_ast_stable(&Expression::Size {
        array: state("table"),
        dimension: Box::new(int(1)),
    });
}

// ===========================================================================
// Secondary AST-level property over the canonical generator (§5.1, §5.2)
//
// `tests/validate.rs`-style valid blocks are canonical by construction:
// `start: None`, finite Reals, and only observable parentheses. They must
// round-trip cleanly at AST level (after `normalize`, which is a no-op on
// `start` here and only strips parens the printer re-inserts).
// ===========================================================================

/// Smallest interesting valid block: one input, one output, empty methods.
fn canonical_minimal() -> Block {
    let mut block = Block::new(n("Minimal"));
    block.interface = vec![
        interface(InterfaceKind::Input, real_decl(n("u")), None),
        interface(InterfaceKind::Output, real_decl(n("y")), None),
    ];
    block
}

/// A stateless classifier reachable from `DoStep`, declaring + raising a signal.
fn canonical_classify() -> UserFunction {
    UserFunction {
        kind: FunctionKind::Stateless,
        name: n("classify"),
        signals: vec![id("probeFault")],
        parameters: vec![in_real("v"), out_real("w")],
        locals: vec![],
        statements: vec![Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(bin(BinaryOp::Gt, lref("v"), r(100.0))),
                body: vec![
                    Statement::Signal(vec![id("probeFault")]),
                    assign(local("w"), r(0.0)),
                ],
            }],
            else_body: Some(vec![assign(local("w"), lref("v"))]),
        })],
    }
}

/// A stateful accumulator method writing block state.
fn canonical_accumulate() -> UserFunction {
    UserFunction {
        kind: FunctionKind::Stateful,
        name: n("accumulate"),
        signals: vec![],
        parameters: vec![in_real("w")],
        locals: vec![],
        statements: vec![assign(
            state("total"),
            bin(BinaryOp::Add, sref("total"), lref("w")),
        )],
    }
}

/// Full-featured canonical block: interface ordering, dependent parameter,
/// state, user signal, stateless + stateful user functions, and a catching
/// signal check.
fn canonical_estimator() -> Block {
    let mut block = Block::new(n("Estimator"));
    block.interface = vec![
        interface(InterfaceKind::Input, real_decl(n("u")), None),
        interface(InterfaceKind::Output, real_decl(n("y")), None),
        interface(InterfaceKind::TunableParameter, real_decl(n("gain")), None),
    ];
    block.protected = vec![
        entity(
            ProtectedKind::DependentParameter,
            real_decl(n("invGain")),
            None,
        ),
        entity(ProtectedKind::State, real_decl(n("total")), None),
    ];
    block.error_signals = vec![id("probeFault")];
    block.protected_functions = vec![canonical_classify(), canonical_accumulate()];
    block.startup.statements = vec![
        assign(state("gain"), r(2.0)),
        assign(state("invGain"), r(0.5)),
        assign(state("total"), r(0.0)),
        assign(state("y"), r(0.0)),
    ];
    block.recalibrate.statements = vec![assign(
        state("invGain"),
        bin(BinaryOp::Div, r(1.0), sref("gain")),
    )];
    block.do_step.locals = vec![real_decl(n("w"))];
    block.do_step.statements = vec![
        assign(local("w"), ecall("classify", vec![sref("u")])),
        Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::SignalCheck(SignalCheck {
                    closure: None,
                    test: Some(SignalTest {
                        negated: false,
                        signals: vec![id("probeFault")],
                    }),
                    fallback: None,
                }),
                body: vec![assign(local("w"), r(0.0))],
            }],
            else_body: None,
        }),
        call_stmt("accumulate", vec![lref("w")]),
        assign(
            state("y"),
            bin(BinaryOp::Mul, sref("total"), sref("invGain")),
        ),
    ];
    block
}

#[test]
fn canonical_generator_minimal() {
    assert_block_round_trips(&canonical_minimal());
}

#[test]
fn canonical_generator_estimator() {
    assert_block_round_trips(&canonical_estimator());
}

// ===========================================================================
// Negative parse cases (§7.3): each malformed input → a specific typed
// `GalecParseError` variant carrying its stable EG050–EG055 code.
// ===========================================================================

/// A well-formed minimal block whose text the negative cases perturb.
const OK: &str = "block M
protected
public
method Startup
algorithm
end Startup;
method Recalibrate
algorithm
end Recalibrate;
method DoStep
algorithm
end DoStep;
end M;
";

fn err(source: &str) -> GalecParseError {
    parse(source, "roundtrip").expect_err("input must be rejected")
}

/// The unperturbed reference block parses, and its diagnostics baseline is
/// green (guards against a stale `OK` template silently masking a case).
#[test]
fn negative_reference_block_parses() {
    parse(OK, "roundtrip").expect("reference block must parse");
}

/// EG050 — a `//` line comment is never legal GALEC (trap T7); it surfaces as a
/// normalized `Syntax` error, never a panic.
#[test]
fn eg050_line_comment_is_syntax_error() {
    let bad = OK.replace("algorithm\nend DoStep;", "algorithm\n// nope\nend DoStep;");
    let e = err(&bad);
    assert!(matches!(e, GalecParseError::Syntax { .. }), "got {e:?}");
    assert!(e.to_string().contains("[EG050]"), "{e}");
}

/// EG050 — a non-T7 real spelling in a statement position is a syntax error.
#[test]
fn eg050_non_t7_real_is_syntax_error() {
    // `1e5` (no decimal point) is not a conformant real; as a bare expression
    // it fails to parse into a Real value.
    for bad in ["1e5", "1.", "1.0E+5", "+1.0"] {
        let e = parse_expression(bad, "roundtrip").expect_err("non-T7 real must be rejected");
        assert!(matches!(e, GalecParseError::Syntax { .. }), "{bad}: {e:?}");
    }
}

/// EG051 — a `block`/`method` terminator name that does not match its header.
#[test]
fn eg051_terminator_mismatch() {
    let block_bad = OK.replace("end M;", "end Other;");
    let e = err(&block_bad);
    assert!(
        matches!(e, GalecParseError::TerminatorMismatch { .. }),
        "block: {e:?}"
    );
    assert!(e.to_string().contains("[EG051]"), "{e}");

    let method_bad = OK.replace("end DoStep;", "end Wrong;");
    assert!(matches!(
        err(&method_bad),
        GalecParseError::TerminatorMismatch { .. }
    ));
}

/// EG052 — a fixed block method that declares parameters (trap T1).
#[test]
fn eg052_method_has_parameters() {
    let bad = OK.replace(
        "method DoStep\nalgorithm",
        "method DoStep\ninput Real x;\nalgorithm",
    );
    let e = err(&bad);
    assert!(
        matches!(&e, GalecParseError::MethodHasParameters { name } if name == "DoStep"),
        "got {e:?}"
    );
    assert!(e.to_string().contains("[EG052]"), "{e}");
}

/// EG053 — a block missing a mandatory method.
#[test]
fn eg053_missing_method() {
    let bad = OK.replace("method DoStep\nalgorithm\nend DoStep;\n", "");
    let e = err(&bad);
    assert!(
        matches!(&e, GalecParseError::MissingMethod { name } if name == "DoStep"),
        "got {e:?}"
    );
    assert!(e.to_string().contains("[EG053]"), "{e}");
}

/// EG054 — a duplicated block method.
#[test]
fn eg054_duplicate_method() {
    let bad = OK.replace(
        "method Recalibrate",
        "method Startup\nalgorithm\nend Startup;\nmethod Recalibrate",
    );
    let e = err(&bad);
    assert!(
        matches!(&e, GalecParseError::DuplicateMethod { name } if name == "Startup"),
        "got {e:?}"
    );
    assert!(e.to_string().contains("[EG054]"), "{e}");
}

/// EG055 — an unknown predefined signal in a method `signals` clause; a known
/// one (`NAN`) parses.
#[test]
fn eg055_unknown_predefined_signal() {
    let bad = OK.replace(
        "method DoStep\nalgorithm",
        "method DoStep\nsignals BOGUS;\nalgorithm",
    );
    let e = err(&bad);
    assert!(
        matches!(&e, GalecParseError::UnknownPredefinedSignal { name } if name == "BOGUS"),
        "got {e:?}"
    );
    assert!(e.to_string().contains("[EG055]"), "{e}");

    let good = OK.replace(
        "method DoStep\nalgorithm",
        "method DoStep\nsignals NAN;\nalgorithm",
    );
    parse(&good, "roundtrip").expect("NAN is a predefined signal");
}
