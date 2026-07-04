//! Validator tests: the six analyses (name / type / dimensionality /
//! termination / side-effect / signals) per SPEC_0034 "Validator Scope" and
//! GAL-018, with a full-featured valid fixture and crafted invalid fixtures
//! (at least two negative cases per analysis). All fixtures are authored
//! independently — no standard text reproduced (GAL-023).
//!
//! The golden PID / array fixtures also pass `validate` — asserted next to
//! their definitions in `tests/block_print.rs`.

use rumoca_ir_galec::ast::{
    BinaryOp, Block, Condition, Dimension, Direction, Expression, ForLoop, FunctionCall,
    FunctionKind, Identifier, IfBranch, IfExpression, IfStatement, InterfaceKind,
    InterfaceVariable, Name, Parameter, PredefinedSignal, ProtectedEntity, ProtectedKind,
    RangeAttributes, RefPart, Reference, ScalarType, SignalCheck, SignalTest, Span, Spanned,
    StateCompartment, Statement, TypeRef, UserFunction, VariableDeclaration,
};
use rumoca_ir_galec::validate;

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

fn n(name: &str) -> Name {
    Name::ident(name)
}

fn id(name: &str) -> Identifier {
    Identifier::new(name)
}

fn real_decl(name: &str) -> VariableDeclaration {
    VariableDeclaration::scalar(ScalarType::Real, n(name))
}

fn int_decl(name: &str) -> VariableDeclaration {
    VariableDeclaration::scalar(ScalarType::Integer, n(name))
}

fn arr_decl(name: &str, sizes: &[i64]) -> VariableDeclaration {
    let mut decl = real_decl(name);
    decl.dimensions = sizes
        .iter()
        .map(|s| Dimension::Expr(Expression::Integer(*s)))
        .collect();
    decl
}

fn state(name: &str) -> Reference {
    Reference::state(n(name))
}

fn stateq(content: &str) -> Reference {
    Reference::state(Name::quoted(content))
}

/// A 2x2 Real matrix literal.
fn matrix2() -> Expression {
    Expression::Array(vec![
        Expression::Array(vec![r(3.0), r(0.0)]),
        Expression::Array(vec![r(0.0), r(5.0)]),
    ])
}

/// A block with one compartment `Cfg { Real a; }`.
fn with_compartment(mut block: Block) -> Block {
    block.compartments = vec![StateCompartment {
        name: n("Cfg"),
        entities: vec![protected_entity(ProtectedKind::State, real_decl("a"))],
        span: Span::DUMMY,
    }];
    block
}

fn state_at(name: &str, subscripts: Vec<Expression>) -> Reference {
    Reference::State(vec![RefPart {
        name: n(name),
        subscripts,
        span: Span::DUMMY,
    }])
}

fn local(name: &str) -> Reference {
    Reference::local(n(name))
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

fn assign(target: Reference, value: Expression) -> Spanned<Statement> {
    Spanned::dummy(Statement::Assignment { target, value })
}

fn fcall(function: &str, arguments: Vec<Expression>) -> FunctionCall {
    FunctionCall {
        function: n(function),
        arguments,
    }
}

fn call_stmt(function: &str, arguments: Vec<Expression>) -> Spanned<Statement> {
    Spanned::dummy(Statement::Call(fcall(function, arguments)))
}

fn in_real(name: &str) -> Parameter {
    Parameter {
        direction: Direction::Input,
        decl: real_decl(name),
    }
}

fn out_real(name: &str) -> Parameter {
    Parameter {
        direction: Direction::Output,
        decl: real_decl(name),
    }
}

fn interface_var(kind: InterfaceKind, decl: VariableDeclaration) -> InterfaceVariable {
    InterfaceVariable {
        kind,
        decl,
        start: None,
    }
}

fn protected_entity(kind: ProtectedKind, decl: VariableDeclaration) -> ProtectedEntity {
    ProtectedEntity {
        kind,
        decl,
        start: None,
    }
}

fn function(kind: FunctionKind, name: &str, statements: Vec<Spanned<Statement>>) -> UserFunction {
    UserFunction {
        kind,
        name: n(name),
        signals: Vec::new(),
        parameters: Vec::new(),
        locals: Vec::new(),
        statements,
        span: Span::DUMMY,
    }
}

/// `if signal in s1, … then body end if;` — a catching check.
fn catch_cond(signals: &[&str]) -> Condition {
    Condition::SignalCheck(SignalCheck {
        closure: None,
        test: Some(SignalTest {
            negated: false,
            signals: signals.iter().map(|s| id(s)).collect(),
        }),
        fallback: None,
    })
}

fn catch_stmt(signals: &[&str], body: Vec<Spanned<Statement>>) -> Spanned<Statement> {
    Spanned::dummy(Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: catch_cond(signals),
            body,
            span: Span::DUMMY,
        }],
        else_body: None,
    }))
}

#[track_caller]
fn expect_codes(block: &Block, expected: &[&str]) {
    let errors = validate(block).expect_err("block must be invalid");
    let codes: Vec<&str> = errors.iter().map(|e| e.code()).collect();
    assert_eq!(codes, expected, "diagnostics: {errors:#?}");
}

#[track_caller]
fn expect_valid(block: &Block) {
    if let Err(errors) = validate(block) {
        panic!("expected a valid block, got: {errors:#?}");
    }
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

/// Smallest interesting valid block: one input, one output, empty methods.
fn minimal() -> Block {
    let mut block = Block::new(n("Minimal"));
    block.interface = vec![
        interface_var(InterfaceKind::Input, real_decl("u")),
        interface_var(InterfaceKind::Output, real_decl("y")),
    ];
    block
}

/// A stateless classifier: `w := 0.0` (optionally signaling `probeFault`)
/// for out-of-range readings, `w := v` otherwise.
fn classify_fn(declared: bool, raises: bool) -> UserFunction {
    let mut then_body = vec![assign(local("w"), r(0.0))];
    if raises {
        then_body.insert(0, Spanned::dummy(Statement::Signal(vec![id("probeFault")])));
    }
    UserFunction {
        kind: FunctionKind::Stateless,
        name: n("classify"),
        signals: if declared {
            vec![id("probeFault")]
        } else {
            Vec::new()
        },
        parameters: vec![in_real("v"), out_real("w")],
        locals: Vec::new(),
        statements: vec![Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(bin(BinaryOp::Gt, lref("v"), r(100.0))),
                body: then_body,
                span: Span::DUMMY,
            }],
            else_body: Some(vec![assign(local("w"), lref("v"))]),
        }))],
        span: Span::DUMMY,
    }
}

/// A stateful accumulator method writing block state.
fn accumulate_fn() -> UserFunction {
    let mut accumulate = function(
        FunctionKind::Stateful,
        "accumulate",
        vec![assign(
            state("total"),
            bin(BinaryOp::Add, sref("total"), lref("w")),
        )],
    );
    accumulate.parameters = vec![in_real("w")];
    accumulate
}

/// Full-featured valid fixture: interface ordering, dependent parameter,
/// state, user signal, stateless + stateful functions reachable from
/// `DoStep`, and a catching signal check making the `DoStep` escape set
/// empty.
fn estimator() -> Block {
    let mut block = Block::new(n("Estimator"));
    block.interface = vec![
        interface_var(InterfaceKind::Input, real_decl("u")),
        interface_var(InterfaceKind::Output, real_decl("y")),
        interface_var(InterfaceKind::TunableParameter, real_decl("gain")),
    ];
    block.protected = vec![
        protected_entity(ProtectedKind::DependentParameter, real_decl("invGain")),
        protected_entity(ProtectedKind::State, real_decl("total")),
    ];
    block.error_signals = vec![id("probeFault")];
    block.protected_functions = vec![classify_fn(true, true), accumulate_fn()];
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
    block.do_step.locals = vec![real_decl("w")];
    block.do_step.statements = vec![
        assign(
            local("w"),
            Expression::Call(fcall("classify", vec![sref("u")])),
        ),
        catch_stmt(&["probeFault"], vec![assign(local("w"), r(0.0))]),
        call_stmt("accumulate", vec![lref("w")]),
        assign(
            state("y"),
            bin(BinaryOp::Mul, sref("total"), sref("invGain")),
        ),
    ];
    block
}

/// Stateful function with one output, usable inside expressions.
fn bump_fn() -> UserFunction {
    let mut bump = function(
        FunctionKind::Stateful,
        "bump",
        vec![assign(state("y"), r(1.0)), assign(local("rv"), r(1.0))],
    );
    bump.parameters = vec![out_real("rv")];
    bump
}

#[test]
fn minimal_block_is_valid() {
    expect_valid(&minimal());
}

#[test]
fn full_featured_block_is_valid() {
    expect_valid(&estimator());
}

// ---------------------------------------------------------------------------
// 1. Name analysis
// ---------------------------------------------------------------------------

#[test]
fn reserved_names_rejected() {
    let mut block = minimal();
    // Builtin collision and reserved keyword, both in the function
    // namespace (locals).
    block.do_step.locals = vec![real_decl("sin"), real_decl("while")];
    expect_codes(&block, &["EG011", "EG011"]);
}

#[test]
fn state_entities_are_a_separate_namespace() {
    // S-2.5 R-2: state entities (always accessed via `self.`) MAY share
    // names with functions — including builtins and predefined signals.
    let mut block = minimal();
    block.protected = vec![
        protected_entity(ProtectedKind::State, real_decl("sign")),
        protected_entity(ProtectedKind::State, real_decl("max")),
        protected_entity(ProtectedKind::State, real_decl("NAN")),
    ];
    expect_valid(&block);
}

#[test]
fn keywords_rejected_even_for_state_entities() {
    let mut block = minimal();
    block
        .protected
        .push(protected_entity(ProtectedKind::State, real_decl("while")));
    expect_codes(&block, &["EG011"]);
}

#[test]
fn empty_identifier_rejected() {
    let mut block = minimal();
    block.do_step.locals = vec![real_decl("")];
    expect_codes(&block, &["EG002"]);
}

#[test]
fn malformed_quoted_identifiers_rejected() {
    let mut block = minimal();
    block.protected = vec![
        protected_entity(
            ProtectedKind::State,
            VariableDeclaration::scalar(ScalarType::Real, Name::quoted("has space")),
        ),
        protected_entity(
            ProtectedKind::State,
            VariableDeclaration::scalar(ScalarType::Real, Name::quoted("x[0]")),
        ),
    ];
    expect_codes(&block, &["EG003", "EG003"]);
}

#[test]
fn illegal_identifier_shapes_rejected() {
    let mut block = minimal();
    // Digit-first and `__`-prefixed names both fail the lexical shape rule.
    block.do_step.locals = vec![real_decl("2fast"), real_decl("__x")];
    expect_codes(&block, &["EG010", "EG010"]);
}

#[test]
fn interface_ordering_enforced() {
    let mut block = minimal();
    block.interface = vec![
        interface_var(InterfaceKind::Output, real_decl("y")),
        interface_var(InterfaceKind::Input, real_decl("u")),
    ];
    expect_codes(&block, &["EG013"]);
}

#[test]
fn duplicate_state_entities_rejected() {
    let mut block = minimal();
    block.protected = vec![
        protected_entity(ProtectedKind::State, real_decl("total")),
        protected_entity(ProtectedKind::State, real_decl("total")),
    ];
    expect_codes(&block, &["EG012"]);
}

#[test]
fn duplicate_compartments_rejected() {
    let mut block = minimal();
    block.compartments = vec![
        StateCompartment {
            name: n("cfg"),
            entities: vec![protected_entity(ProtectedKind::State, real_decl("a"))],
            span: Span::DUMMY,
        },
        StateCompartment {
            name: n("cfg"),
            entities: vec![protected_entity(ProtectedKind::State, real_decl("b"))],
            span: Span::DUMMY,
        },
    ];
    expect_codes(&block, &["EG012"]);
}

// ---------------------------------------------------------------------------
// 2. Type analysis
// ---------------------------------------------------------------------------

#[test]
fn mixed_integer_real_operands_rejected() {
    let mut block = minimal();
    // No implicit Integer<->Real promotion (trap T5).
    block.do_step.statements = vec![assign(state("y"), bin(BinaryOp::Add, int(1), r(0.5)))];
    expect_codes(&block, &["EG016"]);
}

#[test]
fn integer_division_rejected() {
    let mut block = minimal();
    block.do_step.locals = vec![int_decl("k")];
    block.do_step.statements = vec![assign(state("y"), bin(BinaryOp::Div, lref("k"), lref("k")))];
    expect_codes(&block, &["EG016"]);
}

#[test]
fn if_expression_branches_must_be_equally_typed() {
    let mut block = minimal();
    block.do_step.locals = vec![int_decl("k")];
    block.do_step.statements = vec![assign(
        local("k"),
        Expression::If(IfExpression {
            branches: vec![(bin(BinaryOp::Gt, sref("u"), r(0.0)), int(1))],
            else_value: Box::new(r(2.0)),
        }),
    )];
    expect_codes(&block, &["EG017"]);
}

#[test]
fn conditions_must_be_boolean() {
    let mut block = minimal();
    block.do_step.statements = vec![Spanned::dummy(Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(sref("u")),
            body: vec![],
            span: Span::DUMMY,
        }],
        else_body: None,
    }))];
    expect_codes(&block, &["EG017"]);
}

#[test]
fn unresolved_reference_reported_once() {
    let mut block = minimal();
    block.do_step.statements = vec![assign(state("y"), sref("missing"))];
    expect_codes(&block, &["EG014"]);
}

#[test]
fn unknown_function_reported() {
    let mut block = minimal();
    block.do_step.statements = vec![call_stmt("bogus", vec![r(1.0)])];
    expect_codes(&block, &["EG015"]);
}

#[test]
fn declared_dimensions_must_be_integer_typed() {
    let mut block = minimal();
    // `Real v[2.5];` and `Real w[true];` are not derivable from the grammar
    // (dimensions are constant-scalar-INTEGER-expressions, S-3.1).
    let mut v = real_decl("v");
    v.dimensions = vec![Dimension::Expr(r(2.5))];
    let mut w = real_decl("w");
    w.dimensions = vec![Dimension::Expr(Expression::Bool(true))];
    block.do_step.locals = vec![v, w];
    expect_codes(&block, &["EG017", "EG017"]);
}

/// The normative §3.2.7 Euler idiom: vector + scalar * vector (GAL-026).
#[test]
fn vector_arithmetic_with_scalar_broadcast_is_legal() {
    let mut block = minimal();
    let mut derivative =
        VariableDeclaration::scalar(ScalarType::Real, Name::quoted("derivative(x)"));
    derivative.dimensions = vec![Dimension::Expr(int(4))];
    block.protected = vec![
        protected_entity(ProtectedKind::Constant, real_decl("stepSize")),
        protected_entity(ProtectedKind::State, arr_decl("x", &[4])),
        protected_entity(ProtectedKind::State, derivative),
    ];
    block.do_step.statements = vec![assign(
        state("x"),
        bin(
            BinaryOp::Add,
            sref("x"),
            bin(
                BinaryOp::Mul,
                sref("stepSize"),
                Expression::Ref(stateq("derivative(x)")),
            ),
        ),
    )];
    expect_valid(&block);
}

#[test]
fn mismatched_array_ranks_rejected() {
    let mut block = minimal();
    block.protected = vec![
        protected_entity(ProtectedKind::State, arr_decl("v", &[3])),
        protected_entity(ProtectedKind::State, arr_decl("m", &[2, 2])),
    ];
    // vector + matrix: no rank agreement and neither side is scalar.
    block.do_step.statements = vec![assign(state("v"), bin(BinaryOp::Add, sref("v"), sref("m")))];
    expect_codes(&block, &["EG016"]);
}

#[test]
fn relational_operators_are_scalar_only() {
    let mut block = minimal();
    block
        .protected
        .push(protected_entity(ProtectedKind::State, arr_decl("v", &[3])));
    block.do_step.statements = vec![Spanned::dummy(Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(bin(BinaryOp::Lt, sref("v"), sref("v"))),
            body: vec![],
            span: Span::DUMMY,
        }],
        else_body: None,
    }))];
    expect_codes(&block, &["EG016"]);
}

#[test]
fn call_input_arity_checked() {
    let mut block = minimal();
    block.do_step.statements = vec![assign(
        state("y"),
        Expression::Call(fcall("sqrt", vec![r(1.0), r(2.0)])),
    )];
    expect_codes(&block, &["EG018"]);
}

/// `luFactorize` signals SOLVE_LINEAR_EQUATIONS_FAILED; declaring it keeps
/// DoStep's escape set coherent so the tests below isolate their defect.
fn declare_solver_signal(block: &mut Block) {
    block.do_step.signals = vec![PredefinedSignal::SolveLinearEquationsFailed];
}

#[test]
fn multi_output_call_rejected_in_expression_context() {
    let mut block = minimal();
    declare_solver_signal(&mut block);
    // luFactorize has output-arity 2; expressions require exactly 1.
    block.do_step.statements = vec![assign(
        state("y"),
        Expression::Call(fcall("luFactorize", vec![matrix2()])),
    )];
    expect_codes(&block, &["EG019"]);
}

#[test]
fn multi_assignment_output_arity_checked() {
    let mut block = minimal();
    declare_solver_signal(&mut block);
    let mut lu = real_decl("lu");
    lu.dimensions = vec![Dimension::Expr(int(2)), Dimension::Expr(int(2))];
    block.do_step.locals = vec![lu];
    // One target for luFactorize's two outputs.
    block.do_step.statements = vec![Spanned::dummy(Statement::MultiAssignment {
        targets: vec![local("lu")],
        call: fcall("luFactorize", vec![matrix2()]),
    })];
    expect_codes(&block, &["EG019"]);
}

#[test]
fn multi_assignment_mismatch_reports_target_as_expected() {
    let mut block = minimal();
    declare_solver_signal(&mut block);
    let mut lu = real_decl("lu");
    lu.dimensions = vec![Dimension::Expr(int(2)), Dimension::Expr(int(2))];
    // Should be Integer[2] (luFactorize's pivots output).
    block.do_step.locals = vec![lu, arr_decl("pivots", &[2])];
    block.do_step.statements = vec![Spanned::dummy(Statement::MultiAssignment {
        targets: vec![local("lu"), local("pivots")],
        call: fcall("luFactorize", vec![matrix2()]),
    })];
    let errors = validate(&block).expect_err("mismatched target must fail");
    assert_eq!(errors.len(), 1, "diagnostics: {errors:#?}");
    assert_eq!(errors[0].code(), "EG017");
    // Same orientation as single assignments: target is expected.
    let message = errors[0].to_string();
    assert!(
        message.contains("expected Real[:], found Integer[:]"),
        "orientation wrong in: {message}"
    );
}

#[test]
fn component_typed_locals_rejected() {
    let mut block = with_compartment(minimal());
    block.do_step.locals = vec![VariableDeclaration {
        ty: TypeRef::Compartment(n("Cfg")),
        name: n("c"),
        dimensions: vec![],
        range: RangeAttributes::default(),
        span: Span::DUMMY,
    }];
    expect_codes(&block, &["EG020"]);
}

#[test]
fn components_may_not_be_used_as_values() {
    let mut block = with_compartment(minimal());
    block.protected.push(protected_entity(
        ProtectedKind::State,
        VariableDeclaration {
            ty: TypeRef::Compartment(n("Cfg")),
            name: n("box"),
            dimensions: vec![],
            range: RangeAttributes::default(),
            span: Span::DUMMY,
        },
    ));
    block.do_step.statements = vec![assign(state("y"), Expression::Ref(state("box")))];
    expect_codes(&block, &["EG021"]);
}

// ---------------------------------------------------------------------------
// 3. Dimensionality analysis
// ---------------------------------------------------------------------------

#[test]
fn subscripts_must_be_static() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    block.do_step.locals = vec![int_decl("k")];
    // `k` is a plain local, not a loop iterator: not statically evaluable.
    block.do_step.statements = vec![assign(state_at("arr", vec![lref("k")]), r(0.0))];
    expect_codes(&block, &["EG022"]);
}

#[test]
fn loop_bounds_must_be_static() {
    let mut block = minimal();
    block.do_step.locals = vec![int_decl("k")];
    block.do_step.statements = vec![Spanned::dummy(Statement::For(ForLoop {
        iterator: Some(n("i")),
        start: int(1),
        step: None,
        stop: lref("k"),
        body: vec![],
    }))];
    expect_codes(&block, &["EG022"]);
}

#[test]
fn subscript_arity_must_match_declared_dimensions() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    block.do_step.statements = vec![assign(state_at("arr", vec![int(1), int(2)]), r(0.0))];
    expect_codes(&block, &["EG023"]);
}

#[test]
fn block_dimensions_must_be_literals_of_at_least_one() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("bad", &[0]),
    ));
    expect_codes(&block, &["EG024"]);
}

#[test]
fn signaling_builtins_rejected_in_static_positions() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    // §3.2.6 L-2: `integer` can signal NAN/OVERFLOW, which is not permitted
    // in statically-evaluated expressions.
    block.do_step.statements = vec![assign(
        state("y"),
        Expression::Ref(state_at(
            "arr",
            vec![Expression::Call(fcall("integer", vec![r(1.5)]))],
        )),
    )];
    expect_codes(&block, &["EG022"]);
}

#[test]
fn non_signaling_builtins_allowed_in_static_positions() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    block.do_step.statements = vec![assign(
        state("y"),
        Expression::Ref(state_at(
            "arr",
            vec![Expression::Call(fcall("imin", vec![int(2), int(3)]))],
        )),
    )];
    expect_valid(&block);
}

#[test]
fn literal_subscripts_checked_against_literal_dimensions() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    block.do_step.statements = vec![
        assign(state_at("arr", vec![int(5)]), r(0.0)),
        assign(state_at("arr", vec![int(0)]), r(0.0)),
    ];
    expect_codes(&block, &["EG040", "EG040"]);
}

#[test]
fn in_bounds_literal_subscripts_accepted() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    block.do_step.statements = vec![
        assign(state_at("arr", vec![int(1)]), r(0.0)),
        assign(state_at("arr", vec![int(3)]), r(0.0)),
    ];
    expect_valid(&block);
}

#[test]
fn derived_dimensions_only_on_function_inputs() {
    let mut block = minimal();
    block.do_step.locals = vec![VariableDeclaration {
        ty: TypeRef::Primitive(ScalarType::Real),
        name: n("v"),
        dimensions: vec![Dimension::Derived],
        range: RangeAttributes::default(),
        span: Span::DUMMY,
    }];
    expect_codes(&block, &["EG025"]);
}

// ---------------------------------------------------------------------------
// 4. Termination analysis
// ---------------------------------------------------------------------------

#[test]
fn recursive_call_cycles_rejected() {
    let mut block = minimal();
    block.protected_functions = vec![
        function(
            FunctionKind::Stateless,
            "ping",
            vec![call_stmt("pong", vec![])],
        ),
        function(
            FunctionKind::Stateless,
            "pong",
            vec![call_stmt("ping", vec![])],
        ),
    ];
    block.do_step.statements = vec![call_stmt("ping", vec![])];
    expect_codes(&block, &["EG026"]);
}

#[test]
fn functions_unreachable_from_do_step_rejected() {
    let mut block = minimal();
    let mut orphan = function(
        FunctionKind::Stateless,
        "orphan",
        vec![assign(local("w"), r(1.0))],
    );
    orphan.parameters = vec![out_real("w")];
    block.protected_functions = vec![orphan];
    expect_codes(&block, &["EG027"]);
}

#[test]
fn startup_may_not_call_user_functions() {
    let mut block = minimal();
    block.protected_functions = vec![function(FunctionKind::Stateless, "helper", vec![])];
    block.startup.statements = vec![call_stmt("helper", vec![])];
    // Also reached from DoStep, so the only defect is the Startup call.
    block.do_step.statements = vec![call_stmt("helper", vec![])];
    expect_codes(&block, &["EG028"]);
}

#[test]
fn calls_inside_subscripts_count_for_reachability() {
    let mut block = minimal();
    block.protected.push(protected_entity(
        ProtectedKind::State,
        arr_decl("arr", &[3]),
    ));
    let mut index_fn = UserFunction {
        kind: FunctionKind::Stateless,
        name: n("pick"),
        signals: Vec::new(),
        parameters: vec![Parameter {
            direction: Direction::Output,
            decl: int_decl("k"),
        }],
        locals: Vec::new(),
        statements: vec![assign(local("k"), int(1))],
        span: Span::DUMMY,
    };
    index_fn.signals = Vec::new();
    block.protected_functions = vec![index_fn];
    // The user call in the subscript is invalid (static positions are
    // builtin-only, EG022) but must still make `pick` DoStep-reachable —
    // no spurious EG027.
    block.do_step.statements = vec![assign(
        state_at("arr", vec![Expression::Call(fcall("pick", vec![]))]),
        r(0.0),
    )];
    expect_codes(&block, &["EG022"]);
}

#[test]
fn startup_may_call_builtins() {
    let mut block = minimal();
    block.startup.statements = vec![assign(
        state("y"),
        Expression::Call(fcall("sqrt", vec![r(2.0)])),
    )];
    expect_valid(&block);
}

// ---------------------------------------------------------------------------
// 5. Side-effect analysis
// ---------------------------------------------------------------------------

#[test]
fn stateless_functions_may_not_write_state() {
    let mut block = minimal();
    block.protected_functions = vec![function(
        FunctionKind::Stateless,
        "poke",
        vec![assign(state("y"), r(1.0))],
    )];
    block.do_step.statements = vec![call_stmt("poke", vec![])];
    expect_codes(&block, &["EG029"]);
}

#[test]
fn stateless_functions_may_not_limit_state() {
    use rumoca_ir_galec::ast::LimitTarget;
    let mut block = minimal();
    // `limit` saturates (writes) its targets, so a stateless function may
    // neither limit a state variable nor the whole control-state.
    block.protected_functions = vec![
        function(
            FunctionKind::Stateless,
            "clampY",
            vec![Spanned::dummy(Statement::Limit(vec![
                LimitTarget::Reference(state("y")),
            ]))],
        ),
        function(
            FunctionKind::Stateless,
            "clampAll",
            vec![Spanned::dummy(Statement::Limit(vec![
                LimitTarget::SelfState,
            ]))],
        ),
    ];
    block.do_step.statements = vec![call_stmt("clampY", vec![]), call_stmt("clampAll", vec![])];
    expect_codes(&block, &["EG029", "EG029"]);
}

#[test]
fn control_inputs_may_not_be_limited() {
    use rumoca_ir_galec::ast::LimitTarget;
    let mut block = minimal();
    block.do_step.statements = vec![Spanned::dummy(Statement::Limit(vec![
        LimitTarget::Reference(state("u")),
    ]))];
    expect_codes(&block, &["EG033"]);
}

#[test]
fn stateless_functions_may_not_call_stateful_functions() {
    let mut block = minimal();
    block.protected_functions = vec![
        function(
            FunctionKind::Stateful,
            "advance",
            vec![assign(state("y"), r(1.0))],
        ),
        function(
            FunctionKind::Stateless,
            "wrap",
            vec![call_stmt("advance", vec![])],
        ),
    ];
    block.do_step.statements = vec![call_stmt("wrap", vec![])];
    expect_codes(&block, &["EG030"]);
}

#[test]
fn stateful_call_must_be_isolated_in_its_expression() {
    let mut block = minimal();
    block.protected_functions = vec![bump_fn()];
    block.do_step.locals = vec![real_decl("w")];
    // Sibling state reference next to the stateful call.
    block.do_step.statements = vec![assign(
        local("w"),
        bin(
            BinaryOp::Add,
            Expression::Call(fcall("bump", vec![])),
            sref("y"),
        ),
    )];
    expect_codes(&block, &["EG031"]);
}

#[test]
fn stateful_calls_banned_inside_if_expressions() {
    let mut block = minimal();
    block.protected_functions = vec![bump_fn()];
    block.do_step.locals = vec![real_decl("w")];
    block.do_step.statements = vec![assign(
        local("w"),
        Expression::If(IfExpression {
            branches: vec![(
                bin(BinaryOp::Gt, sref("u"), r(0.0)),
                Expression::Call(fcall("bump", vec![])),
            )],
            else_value: Box::new(r(0.0)),
        }),
    )];
    expect_codes(&block, &["EG032"]);
}

#[test]
fn control_inputs_are_read_only() {
    let mut block = minimal();
    block.do_step.statements = vec![assign(state("u"), r(1.0))];
    expect_codes(&block, &["EG033"]);
}

// ---------------------------------------------------------------------------
// 6. Signal analysis
// ---------------------------------------------------------------------------

#[test]
fn undeclared_escape_rejected() {
    let mut block = minimal();
    block.error_signals = vec![id("probeFault")];
    // Raises `probeFault` without declaring it in the signals clause.
    block.protected_functions = vec![classify_fn(false, true)];
    block.do_step.locals = vec![real_decl("w")];
    block.do_step.statements = vec![assign(
        local("w"),
        Expression::Call(fcall("classify", vec![sref("u")])),
    )];
    expect_codes(&block, &["EG036"]);
}

#[test]
fn overdeclared_signals_clause_rejected() {
    let mut block = estimator();
    // Declares `probeFault` but never raises it.
    block.protected_functions[0] = classify_fn(true, false);
    expect_codes(&block, &["EG037"]);
}

#[test]
fn testing_an_unsettable_signal_rejected() {
    let mut block = minimal();
    // Nothing before the check can set NAN.
    block.do_step.statements = vec![catch_stmt(&["NAN"], vec![])];
    expect_codes(&block, &["EG038"]);
}

#[test]
fn testing_an_already_caught_signal_rejected() {
    let mut block = estimator();
    // Second branch re-tests a signal the first branch already caught.
    block.do_step.statements[1] = Spanned::dummy(Statement::If(IfStatement {
        branches: vec![
            IfBranch {
                condition: catch_cond(&["probeFault"]),
                body: vec![assign(local("w"), r(0.0))],
                span: Span::DUMMY,
            },
            IfBranch {
                condition: catch_cond(&["probeFault"]),
                body: vec![assign(local("w"), r(1.0))],
                span: Span::DUMMY,
            },
        ],
        else_body: None,
    }));
    expect_codes(&block, &["EG038"]);
}

#[test]
fn unrestricted_check_needs_a_settable_signal() {
    let mut block = minimal();
    block.do_step.statements = vec![Spanned::dummy(Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::SignalCheck(SignalCheck {
                closure: None,
                test: None,
                fallback: None,
            }),
            body: vec![],
            span: Span::DUMMY,
        }],
        else_body: None,
    }))];
    expect_codes(&block, &["EG039"]);
}

#[test]
fn at_most_sixteen_user_signals() {
    let mut block = minimal();
    block.error_signals = (0..17).map(|i| id(&format!("sig{i}"))).collect();
    expect_codes(&block, &["EG035"]);
}

#[test]
fn unknown_signal_names_rejected() {
    let mut block = minimal();
    block.do_step.statements = vec![Spanned::dummy(Statement::Signal(vec![id("ghost")]))];
    expect_codes(&block, &["EG034"]);
}

#[test]
fn signal_closure_reraise_is_accounted() {
    let mut block = minimal();
    block.error_signals = vec![id("probeFault")];
    // relabel catches its own raise into closure `c` and re-raises it, so
    // its escape set is exactly {probeFault} — matching its clause.
    let mut relabel = function(
        FunctionKind::Stateless,
        "relabel",
        vec![
            Spanned::dummy(Statement::Signal(vec![id("probeFault")])),
            Spanned::dummy(Statement::If(IfStatement {
                branches: vec![IfBranch {
                    condition: Condition::SignalCheck(SignalCheck {
                        closure: Some(id("c")),
                        test: None,
                        fallback: None,
                    }),
                    body: vec![Spanned::dummy(Statement::Signal(vec![id("c")]))],
                    span: Span::DUMMY,
                }],
                else_body: None,
            })),
        ],
    );
    relabel.signals = vec![id("probeFault")];
    block.protected_functions = vec![relabel];
    block.do_step.statements = vec![
        call_stmt("relabel", vec![]),
        catch_stmt(&["probeFault"], vec![assign(state("y"), r(0.0))]),
    ];
    expect_valid(&block);
}
