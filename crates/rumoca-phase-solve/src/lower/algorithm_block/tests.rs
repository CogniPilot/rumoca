use super::*;
use rumoca_core::SourceId;
use rumoca_ir_galec::ast::{
    Dimension, IfStatement, InterfaceVariable, Name, ProtectedEntity, RangeAttributes,
};
use rumoca_ir_solve::{SolveIntegerDomain, SolveRealFormat};

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("controller.alg"),
        start,
        start + 1,
    )
}

fn binary32() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

fn decl(ty: ScalarType, name: &str, at: usize) -> VariableDeclaration {
    let mut decl = VariableDeclaration::scalar(ty, Name::ident(name));
    decl.span = span(at);
    decl
}

fn ranged(mut decl: VariableDeclaration, min: f64, max: f64) -> VariableDeclaration {
    decl.range = RangeAttributes {
        min: Some(Expression::Real(min)),
        max: Some(Expression::Real(max)),
    };
    decl
}

fn state(name: &str) -> Reference {
    Reference::state(Name::ident(name))
}

fn local(name: &str) -> Reference {
    Reference::local(Name::ident(name))
}

fn at(statement: Statement, start: usize) -> Spanned<Statement> {
    Spanned {
        node: statement,
        span: span(start),
    }
}

fn assign(target: Reference, value: Expression, start: usize) -> Spanned<Statement> {
    at(Statement::Assignment { target, value }, start)
}

/// A RateController-shaped block: a tunable gain, a ranged input, output and
/// integrator state, a first-tick flag, an `if` statement assigning state in
/// one branch, an explicit `limit`, and a saturating `if` expression.
fn controller() -> Block {
    let mut block = Block::new(Name::ident("Controller"));
    block.span = span(1);
    block.interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: ranged(decl(ScalarType::Real, "u", 2), -500.0, 500.0),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Output,
            decl: ranged(decl(ScalarType::Real, "y", 3), -50.0, 50.0),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: ranged(decl(ScalarType::Real, "gain", 4), 0.0, 100.0),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: decl(ScalarType::Real, "period", 5),
            start: None,
        },
    ];
    block.protected = vec![
        ProtectedEntity {
            kind: ProtectedKind::State,
            decl: ranged(decl(ScalarType::Real, "x", 6), -100.0, 100.0),
            start: None,
        },
        ProtectedEntity {
            kind: ProtectedKind::State,
            decl: decl(ScalarType::Boolean, "first", 7),
            start: None,
        },
    ];
    block.startup.span = span(10);
    block.startup.statements = vec![
        assign(state("gain"), Expression::Real(2.5), 11),
        assign(state("period"), Expression::Real(0.005), 12),
        assign(state("x"), Expression::Real(0.0), 13),
        assign(state("first"), Expression::Bool(true), 14),
        assign(state("y"), Expression::Real(0.0), 15),
    ];
    block.recalibrate.span = span(20);
    block.do_step.span = span(30);
    block.do_step.locals = vec![decl(ScalarType::Real, "e", 31)];
    let error = || Expression::Ref(local("e"));
    block.do_step.statements = vec![
        assign(local("e"), Expression::Ref(state("u")), 32),
        at(
            Statement::If(IfStatement {
                branches: vec![IfBranch {
                    condition: Condition::Expression(Expression::Ref(state("first"))),
                    body: vec![assign(state("first"), Expression::Bool(false), 34)],
                    span: span(33),
                }],
                else_body: Some(vec![assign(
                    state("x"),
                    Expression::binary(
                        BinaryOp::Add,
                        Expression::Ref(state("x")),
                        Expression::binary(
                            BinaryOp::Mul,
                            Expression::binary(
                                BinaryOp::Mul,
                                Expression::Ref(state("gain")),
                                error(),
                            ),
                            Expression::Ref(state("period")),
                        ),
                    ),
                    35,
                )]),
            }),
            33,
        ),
        at(
            Statement::Limit(vec![LimitTarget::Reference(state("x"))]),
            36,
        ),
        assign(
            state("y"),
            Expression::If(IfExpression::new(
                vec![(
                    Expression::binary(BinaryOp::Gt, error(), Expression::Real(50.0)),
                    Expression::Real(50.0),
                )],
                Expression::binary(BinaryOp::Add, Expression::Ref(state("x")), error()),
            )),
            37,
        ),
    ];
    block
}

fn package(block: Block) -> AlgorithmCodePackage {
    let nominals = vec![None; block.interface.len() + block.protected.len()];
    AlgorithmCodePackage::construct(block, nominals, "period").expect("checked controller block")
}

#[test]
fn a_controller_block_refines_into_a_solve_block() {
    let block = lower_algorithm_block(&package(controller()), binary32()).expect("refined");
    // Six declarations, then the six predefined signal status bits.
    assert_eq!(block.slots().len(), 6 + SIGNALS.len());
    let names = block
        .slots()
        .iter()
        .map(|slot| slot.source().qualified_name())
        .collect::<Vec<_>>();
    assert_eq!(&names[..6], ["u", "y", "gain", "period", "x", "first"]);
    assert_eq!(names[6 + NAN_SIGNAL], "ErrorSignalStatus.NAN");
    assert_eq!(
        block
            .do_step()
            .local_sources()
            .iter()
            .map(SolveAlgorithmCodeRef::qualified_name)
            .collect::<Vec<_>>(),
        ["DoStep.e"]
    );
    assert!(block.startup().local_sources().is_empty());
}

#[test]
fn a_tunable_parameter_is_stored_only_by_startup() {
    let package = package(controller());
    let scope = block_scope(package.block(), binary32()).unwrap();
    let do_step = method_scope(&scope, Method::DoStep, &package.block().do_step).unwrap();
    let startup = method_scope(&scope, Method::Startup, &package.block().startup).unwrap();
    let gain = startup.resolve(&state("gain")).unwrap();
    assert!(startup.check_store(gain, span(1)).is_ok());
    assert!(matches!(
        do_step.check_store(gain, span(1)),
        Err(AlgorithmBlockLowerError::LifecycleStore { name, .. }) if name == "gain"
    ));
    let input = do_step.resolve(&state("u")).unwrap();
    assert!(startup.check_store(input, span(1)).is_err());
}

#[test]
fn bounds_are_decided_in_the_binary32_domain() {
    let mut block = controller();
    block.protected[0].decl = ranged(decl(ScalarType::Real, "x", 6), -1.0e60, 0.000001);
    let package = package(block);
    let scope = block_scope(package.block(), binary32()).unwrap();
    let x = &scope.block[scope.by_name["x"]];
    // No binary32 value lies below -1e60, so that clamp is elided.
    assert_eq!(x.low, None);
    assert_eq!(x.high, Some(Bound::Real(0.000001)));
}

#[test]
fn an_array_variable_refines_and_an_unsupported_statement_is_refused() {
    let mut block = controller();
    let mut buffer = decl(ScalarType::Real, "buffer", 8);
    buffer.dimensions = vec![Dimension::Expr(Expression::Integer(3))];
    block.protected.push(ProtectedEntity {
        kind: ProtectedKind::State,
        decl: buffer,
        start: None,
    });
    // Startup defines every state slot.
    block.startup.statements.push(assign(
        state("buffer"),
        Expression::Array(vec![Expression::Real(0.0); 3]),
        9,
    ));
    let package = package(block);
    let refined = lower_algorithm_block(&package, binary32()).expect("arrays are refined");
    let buffer = refined
        .slots()
        .iter()
        .find(|slot| slot.source().qualified_name() == "buffer")
        .unwrap();
    assert_eq!(buffer.value_type().dimensions(), [3]);
    let scope = block_scope(package.block(), binary32()).unwrap();
    let do_step = method_scope(&scope, Method::DoStep, &package.block().do_step).unwrap();
    let call = rumoca_ir_galec::ast::FunctionCall {
        function: n("solve"),
        arguments: Vec::new(),
    };
    let statement = Statement::MultiAssignment {
        targets: vec![state("x")],
        call,
    };
    let Err(error) = check_statement(&do_step, &statement, span(1)) else {
        panic!("multiple assignment is not refined in this slice");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(
        error.to_string().contains("built-in call statement"),
        "{error}"
    );
}

// The golden RateController (rumoca-phase-codegen galec_golden_tests.rs),
// built the same way, then given real spans: Solve provenance is never dummy.

fn n(name: &str) -> Name {
    Name::ident(name)
}

fn real_decl(name: Name) -> VariableDeclaration {
    VariableDeclaration::scalar(ScalarType::Real, name)
}

fn stateq(content: &str) -> Reference {
    Reference::state(Name::quoted(content))
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

fn bin(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::binary(op, lhs, rhs)
}

fn plain(target: Reference, value: Expression) -> Spanned<Statement> {
    Spanned::dummy(Statement::Assignment { target, value })
}

fn rate_controller_interface() -> Vec<InterfaceVariable> {
    let tunable = |name: &str, min: f64, max: f64, start: f64| InterfaceVariable {
        kind: InterfaceKind::TunableParameter,
        decl: ranged(real_decl(n(name)), min, max),
        start: Some(r(start)),
    };
    vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: ranged(real_decl(n("speedSetpoint")), -500.0, 500.0),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: ranged(real_decl(n("speedMeasured")), -500.0, 500.0),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Output,
            decl: ranged(real_decl(n("drive")), -50.0, 50.0),
            start: Some(r(0.0)),
        },
        tunable("gain", 0.0, 100.0, 2.5),
        tunable("integralTime", 0.000_001, 1000.0, 0.5),
        tunable("derivativeTime", 0.0, 10.0, 0.01),
        tunable("period", 0.000_001, 1.0, 0.005),
    ]
}

fn rate_controller_protected() -> Vec<ProtectedEntity> {
    vec![
        ProtectedEntity {
            kind: ProtectedKind::DependentParameter,
            decl: real_decl(n("integralGain")),
            start: None,
        },
        ProtectedEntity {
            kind: ProtectedKind::State,
            decl: ranged(real_decl(n("integralState")), -100.0, 100.0),
            start: Some(r(0.0)),
        },
        ProtectedEntity {
            kind: ProtectedKind::State,
            decl: real_decl(Name::quoted("previous(trackingError)")),
            start: Some(r(0.0)),
        },
        ProtectedEntity {
            kind: ProtectedKind::State,
            decl: VariableDeclaration::scalar(ScalarType::Boolean, n("firstTick")),
            start: Some(Expression::Bool(true)),
        },
    ]
}

fn dependent_gain_update() -> Spanned<Statement> {
    plain(
        state("integralGain"),
        bin(BinaryOp::Div, sref("gain"), sref("integralTime")),
    )
}

fn rate_controller_do_step() -> BlockMethod {
    let first_tick_branch = IfBranch {
        condition: Condition::Expression(sref("firstTick")),
        body: vec![
            plain(state("firstTick"), Expression::Bool(false)),
            plain(local("derivativeEstimate"), r(0.0)),
        ],
        span: Span::DUMMY,
    };
    let error_delta = bin(
        BinaryOp::Sub,
        lref("trackingError"),
        Expression::Ref(stateq("previous(trackingError)")),
    );
    let integrate = vec![
        plain(
            local("derivativeEstimate"),
            bin(
                BinaryOp::Div,
                Expression::Paren(Box::new(error_delta)),
                sref("period"),
            ),
        ),
        plain(
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
    let limiter = Expression::If(IfExpression::new(
        vec![
            (bin(BinaryOp::Gt, lref("unboundedDrive"), r(50.0)), r(50.0)),
            (
                bin(BinaryOp::Lt, lref("unboundedDrive"), r(-50.0)),
                r(-50.0),
            ),
        ],
        lref("unboundedDrive"),
    ));
    BlockMethod {
        signals: vec![PredefinedSignal::Nan],
        locals: vec![
            real_decl(n("trackingError")),
            real_decl(n("derivativeEstimate")),
            real_decl(n("unboundedDrive")),
        ],
        statements: vec![
            plain(
                local("trackingError"),
                bin(BinaryOp::Sub, sref("speedSetpoint"), sref("speedMeasured")),
            ),
            Spanned::dummy(Statement::If(IfStatement {
                branches: vec![first_tick_branch],
                else_body: Some(integrate),
            })),
            Spanned::dummy(Statement::Limit(vec![LimitTarget::Reference(state(
                "integralState",
            ))])),
            plain(
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
            plain(state("drive"), limiter),
            plain(stateq("previous(trackingError)"), lref("trackingError")),
        ],
        span: Span::DUMMY,
    }
}

fn rate_controller() -> Block {
    let mut block = Block {
        interface: rate_controller_interface(),
        protected: rate_controller_protected(),
        startup: BlockMethod {
            statements: vec![
                plain(state("gain"), r(2.5)),
                plain(state("integralTime"), r(0.5)),
                plain(state("derivativeTime"), r(0.01)),
                plain(state("period"), r(0.005)),
                dependent_gain_update(),
                plain(state("integralState"), r(0.0)),
                plain(stateq("previous(trackingError)"), r(0.0)),
                plain(state("firstTick"), Expression::Bool(true)),
                plain(state("drive"), r(0.0)),
            ],
            ..Default::default()
        },
        recalibrate: BlockMethod {
            statements: vec![dependent_gain_update()],
            ..Default::default()
        },
        do_step: rate_controller_do_step(),
        ..Block::new(n("RateController"))
    };
    respan(&mut block);
    block
}

/// Give every declaration, statement, branch and method a distinct real span.
fn respan(block: &mut Block) {
    let mut next = 100;
    let mut fresh = || {
        next += 1;
        span(next)
    };
    block.span = fresh();
    for variable in &mut block.interface {
        variable.decl.span = fresh();
    }
    for entity in &mut block.protected {
        entity.decl.span = fresh();
    }
    for method in [
        &mut block.startup,
        &mut block.recalibrate,
        &mut block.do_step,
    ] {
        method.span = fresh();
        for local in &mut method.locals {
            local.span = fresh();
        }
        respan_statements(&mut method.statements, &mut fresh);
    }
    for function in block
        .protected_functions
        .iter_mut()
        .chain(&mut block.public_functions)
    {
        function.span = fresh();
        for parameter in &mut function.parameters {
            parameter.decl.span = fresh();
        }
        for local in &mut function.locals {
            local.span = fresh();
        }
        respan_statements(&mut function.statements, &mut fresh);
    }
}

fn respan_statements(statements: &mut [Spanned<Statement>], fresh: &mut impl FnMut() -> Span) {
    for statement in statements {
        statement.span = fresh();
        match &mut statement.node {
            Statement::If(if_statement) => {
                for branch in &mut if_statement.branches {
                    branch.span = fresh();
                    respan_statements(&mut branch.body, fresh);
                }
                if let Some(body) = &mut if_statement.else_body {
                    respan_statements(body, fresh);
                }
            }
            Statement::For(for_loop) => respan_statements(&mut for_loop.body, fresh),
            _ => {}
        }
    }
}

#[test]
fn the_golden_rate_controller_refines_into_a_solve_block() {
    let package = AlgorithmCodePackage::construct(rate_controller(), vec![None; 11], "period")
        .expect("the golden RateController validates");
    let block = lower_algorithm_block(&package, binary32()).expect("refined");
    let names = block
        .slots()
        .iter()
        .take(11)
        .map(|slot| slot.source().qualified_name())
        .collect::<Vec<_>>();
    assert_eq!(names[9], "previous(trackingError)");
    assert_eq!(
        block
            .do_step()
            .local_sources()
            .iter()
            .map(SolveAlgorithmCodeRef::qualified_name)
            .collect::<Vec<_>>(),
        [
            "DoStep.trackingError",
            "DoStep.derivativeEstimate",
            "DoStep.unboundedDrive"
        ]
    );
    // Recalibrate may store the dependent gain; DoStep may not.
    let scope = block_scope(package.block(), binary32()).unwrap();
    let gain = VarId::Block(scope.by_name["integralGain"]);
    let recalibrate =
        method_scope(&scope, Method::Recalibrate, &package.block().recalibrate).unwrap();
    let do_step = method_scope(&scope, Method::DoStep, &package.block().do_step).unwrap();
    assert!(recalibrate.check_store(gain, span(1)).is_ok());
    assert!(do_step.check_store(gain, span(1)).is_err());
}

// The refined block computes what the GALEC evaluator computes.

fn binary64() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

fn real64(value: f64) -> rumoca_eval_solve::TypedValue {
    let value = SolveValue::real(binary64(), value);
    rumoca_eval_solve::TypedValue::construct(value.value_type().clone(), vec![value.kind()])
        .unwrap()
}

/// One block value, as bits so NaN payloads and signed zeros compare exactly.
#[derive(Debug, PartialEq)]
enum Bits {
    Real(u64),
    Integer(i64),
    Boolean(bool),
    Unset,
}

fn solve_bits(value: Option<&rumoca_eval_solve::TypedValue>) -> Bits {
    match value.map(|value| value.elements()[0]) {
        Some(rumoca_ir_solve::SolveValueKind::Real64(bits)) => Bits::Real(bits),
        Some(rumoca_ir_solve::SolveValueKind::Integer(value)) => Bits::Integer(value),
        Some(rumoca_ir_solve::SolveValueKind::Boolean(value)) => Bits::Boolean(value),
        None => Bits::Unset,
        Some(other) => panic!("unexpected element {other:?}"),
    }
}

fn oracle_bits(value: &rumoca_eval_galec::Value) -> Bits {
    match value {
        rumoca_eval_galec::Value::Real(value) => Bits::Real(value.to_bits()),
        rumoca_eval_galec::Value::Integer(value) => Bits::Integer(*value),
        rumoca_eval_galec::Value::Boolean(value) => Bits::Boolean(*value),
        other => panic!("unexpected oracle value {other:?}"),
    }
}

#[test]
fn the_rate_controller_solve_block_matches_the_galec_evaluator() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let package = AlgorithmCodePackage::construct(rate_controller(), vec![None; 11], "period")
        .expect("the golden RateController validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    let names = solve
        .slots()
        .iter()
        .take(11)
        .map(|slot| slot.source().qualified_name().to_owned())
        .collect::<Vec<_>>();
    let index = |name: &str| {
        names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap()
    };
    let nan_status = 11 + NAN_SIGNAL;
    let mut state = vec![None; solve.slots().len()];
    let set = |state: &mut Vec<_>, oracle: &mut Evaluator<'_>, name: &str, value: f64| {
        oracle.set_state(name, Value::Real(value)).unwrap();
        state[index(name)] = Some(real64(value));
    };
    // External state before Startup, in range (the oracle saturates an
    // initialized input at Startup's return, the C path does not).
    for (name, value) in [
        ("speedSetpoint", 0.0),
        ("speedMeasured", 0.0),
        ("gain", 1.0),
        ("integralTime", 1.0),
        ("derivativeTime", 0.0),
        ("period", 0.01),
    ] {
        set(&mut state, &mut oracle, name, value);
    }
    let compare =
        |state: &[Option<rumoca_eval_solve::TypedValue>], oracle: &Evaluator<'_>, step: &str| {
            for (slot, name) in names.iter().enumerate() {
                assert_eq!(
                    solve_bits(state[slot].as_ref()),
                    oracle_bits(oracle.state(name).unwrap()),
                    "{step}: {name}"
                );
            }
            assert_eq!(
                solve_bits(state[nan_status].as_ref()),
                Bits::Boolean(oracle.active_signals().contains("NAN")),
                "{step}: NAN signal"
            );
        };
    oracle.startup().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    compare(&state, &oracle, "Startup");
    let steps = [
        (10.0, 0.0),
        (10.0, 2.5),
        (600.0, 0.0),
        (-450.0, 400.0),
        (f64::NAN, 1.0),
        (3.0, f64::NAN),
        (0.125, -0.0),
    ];
    for (step, &(setpoint, measured)) in steps.iter().enumerate() {
        if step == 3 {
            set(&mut state, &mut oracle, "gain", 7.5);
            set(&mut state, &mut oracle, "integralTime", 0.25);
            oracle.recalibrate().unwrap();
            rumoca_eval_solve::eval_algorithm_method(
                &solve,
                SolveBlockMethodKind::Recalibrate,
                &mut state,
            )
            .unwrap();
            compare(&state, &oracle, "Recalibrate");
        }
        set(&mut state, &mut oracle, "speedSetpoint", setpoint);
        set(&mut state, &mut oracle, "speedMeasured", measured);
        oracle.do_step().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .unwrap();
        compare(
            &state,
            &oracle,
            &format!("DoStep {step} ({setpoint}, {measured})"),
        );
    }
}

// Arrays, element reads and writes, nested `for` loops, `size`, array
// literals and element-wise saturation, on the golden MatrixAverager.

fn dims(mut decl: VariableDeclaration, sizes: &[i64]) -> VariableDeclaration {
    decl.dimensions = sizes
        .iter()
        .map(|&size| Dimension::Expr(Expression::Integer(size)))
        .collect();
    decl
}

fn element(name: &str, subscripts: Vec<Expression>) -> Reference {
    Reference::State(vec![rumoca_ir_galec::ast::RefPart {
        name: n(name),
        subscripts,
        span: Span::DUMMY,
    }])
}

fn matrix_averager() -> Block {
    use rumoca_ir_galec::ast::ForLoop;
    let zero_row = || Expression::Array(vec![r(0.0), r(0.0), r(0.0)]);
    let size_dim = |dimension: i64| Expression::Size {
        array: state("accumulator"),
        dimension: Box::new(Expression::Integer(dimension)),
    };
    let ij = || vec![lref("i"), lref("j")];
    let inner = Spanned::dummy(Statement::for_loop(ForLoop::new(
        Some(n("j")),
        Expression::Integer(1),
        None,
        size_dim(2),
        vec![
            plain(
                element("accumulator", ij()),
                Expression::Ref(element("samples", ij())),
            ),
            plain(
                local("total"),
                bin(
                    BinaryOp::Add,
                    lref("total"),
                    Expression::Ref(element("accumulator", ij())),
                ),
            ),
        ],
    )));
    let outer = Spanned::dummy(Statement::for_loop(ForLoop::new(
        Some(n("i")),
        Expression::Integer(1),
        None,
        size_dim(1),
        vec![
            plain(local("total"), r(0.0)),
            inner,
            plain(
                element("rowMean", vec![lref("i")]),
                bin(BinaryOp::Div, lref("total"), r(3.0)),
            ),
        ],
    )));
    let mut block = Block {
        interface: vec![
            InterfaceVariable {
                kind: InterfaceKind::Input,
                decl: ranged(dims(real_decl(n("samples")), &[2, 3]), -1000.0, 1000.0),
                start: None,
            },
            InterfaceVariable {
                kind: InterfaceKind::Output,
                decl: dims(real_decl(n("rowMean")), &[2]),
                start: Some(Expression::Array(vec![r(0.0), r(0.0)])),
            },
            InterfaceVariable {
                kind: InterfaceKind::TunableParameter,
                decl: ranged(real_decl(n("period")), 0.001, 1.0),
                start: Some(r(0.01)),
            },
        ],
        protected: vec![ProtectedEntity {
            kind: ProtectedKind::State,
            decl: dims(real_decl(n("accumulator")), &[2, 3]),
            start: None,
        }],
        startup: BlockMethod {
            statements: vec![
                plain(state("period"), r(0.01)),
                plain(
                    state("accumulator"),
                    Expression::Array(vec![zero_row(), zero_row()]),
                ),
                plain(state("rowMean"), Expression::Array(vec![r(0.0), r(0.0)])),
            ],
            ..Default::default()
        },
        do_step: BlockMethod {
            locals: vec![real_decl(n("total"))],
            statements: vec![outer],
            ..Default::default()
        },
        ..Block::new(n("MatrixAverager"))
    };
    respan(&mut block);
    block
}

/// Every element of a block value as bits, in row-major order.
fn solve_elements(value: Option<&rumoca_eval_solve::TypedValue>) -> Vec<Bits> {
    let Some(value) = value else {
        return vec![Bits::Unset];
    };
    value
        .elements()
        .iter()
        .map(|element| match element {
            rumoca_ir_solve::SolveValueKind::Real64(bits) => Bits::Real(*bits),
            rumoca_ir_solve::SolveValueKind::Boolean(value) => Bits::Boolean(*value),
            other => panic!("unexpected element {other:?}"),
        })
        .collect()
}

fn oracle_elements(value: &rumoca_eval_galec::Value, out: &mut Vec<Bits>) {
    match value {
        rumoca_eval_galec::Value::Array(items) => {
            for item in items {
                oracle_elements(item, out);
            }
        }
        scalar => out.push(oracle_bits(scalar)),
    }
}

fn real_tensor(values: &[f64], dims: Vec<u32>) -> rumoca_eval_solve::TypedValue {
    let value_type = SolveValueType::tensor(
        SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        },
        dims,
    )
    .unwrap();
    rumoca_eval_solve::TypedValue::construct(
        value_type,
        values
            .iter()
            .map(|value| rumoca_ir_solve::SolveValueKind::Real64(value.to_bits()))
            .collect(),
    )
    .unwrap()
}

#[test]
fn the_matrix_averager_solve_block_matches_the_galec_evaluator() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let package = AlgorithmCodePackage::construct(matrix_averager(), vec![None; 4], "period")
        .expect("the golden MatrixAverager validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    let names = ["samples", "rowMean", "period", "accumulator"];
    let set_samples = |state: &mut Vec<Option<rumoca_eval_solve::TypedValue>>,
                       oracle: &mut Evaluator<'_>,
                       values: [f64; 6]| {
        let rows = values
            .chunks(3)
            .map(|row| Value::Array(row.iter().copied().map(Value::Real).collect()))
            .collect();
        oracle.set_state("samples", Value::Array(rows)).unwrap();
        state[0] = Some(real_tensor(&values, vec![2, 3]));
    };
    let mut state = vec![None; solve.slots().len()];
    set_samples(&mut state, &mut oracle, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
    oracle.set_state("period", Value::Real(0.02)).unwrap();
    state[2] = Some(real64(0.02));
    let compare =
        |state: &[Option<rumoca_eval_solve::TypedValue>], oracle: &Evaluator<'_>, step: &str| {
            for (slot, name) in names.iter().enumerate() {
                let mut expected = Vec::new();
                oracle_elements(oracle.state(name).unwrap(), &mut expected);
                assert_eq!(
                    solve_elements(state[slot].as_ref()),
                    expected,
                    "{step}: {name}"
                );
            }
        };
    oracle.startup().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    compare(&state, &oracle, "Startup");
    for (step, samples) in [
        [1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
        [2000.0, -0.0, 0.5, -3000.0, 7.25, 1e-300],
        [f64::NAN, 1.0, 1.0, 1.0, f64::NAN, 1.0],
    ]
    .into_iter()
    .enumerate()
    {
        set_samples(&mut state, &mut oracle, samples);
        oracle.do_step().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .unwrap();
        compare(&state, &oracle, &format!("DoStep {step}"));
    }
}

/// Regression (#483): an if-expression arm GALEC does not select must not
/// raise a signal; and Startup saturates the inputs set before it.
fn lazy_selector() -> Block {
    let mut block = Block::new(n("LazySelector"));
    block.interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: real_decl(n("x")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: ranged(real_decl(n("u")), -1.0, 1.0),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Output,
            decl: real_decl(n("y")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: real_decl(n("period")),
            start: Some(r(0.01)),
        },
    ];
    block.protected = vec![ProtectedEntity {
        kind: ProtectedKind::State,
        decl: VariableDeclaration::scalar(ScalarType::Boolean, n("flag")),
        start: None,
    }];
    block.startup.statements = vec![
        plain(state("flag"), Expression::Bool(true)),
        plain(state("y"), r(0.0)),
    ];
    block.do_step.statements = vec![
        plain(
            state("y"),
            Expression::If(IfExpression::new(
                vec![
                    (sref("flag"), r(1.0)),
                    (bin(BinaryOp::Gt, sref("x"), r(0.0)), r(2.0)),
                ],
                r(3.0),
            )),
        ),
        plain(state("flag"), Expression::Not(Box::new(sref("flag")))),
    ];
    respan(&mut block);
    block
}

#[test]
fn an_unselected_if_arm_raises_nothing_and_startup_saturates_set_inputs() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let package = AlgorithmCodePackage::construct(lazy_selector(), vec![None; 5], "period")
        .expect("the selector validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    let names = ["x", "u", "y", "period", "flag"];
    let nan_status = 5 + NAN_SIGNAL;
    let mut state = vec![None; solve.slots().len()];
    for (slot, name, value) in [(0, "x", f64::NAN), (1, "u", 5.0), (3, "period", 0.01)] {
        oracle.set_state(name, Value::Real(value)).unwrap();
        state[slot] = Some(real64(value));
    }
    let compare =
        |state: &[Option<rumoca_eval_solve::TypedValue>], oracle: &Evaluator<'_>, step: &str| {
            for (slot, name) in names.iter().enumerate() {
                assert_eq!(
                    solve_bits(state[slot].as_ref()),
                    oracle_bits(oracle.state(name).unwrap()),
                    "{step}: {name}"
                );
            }
            assert_eq!(
                solve_bits(state[nan_status].as_ref()),
                Bits::Boolean(oracle.active_signals().contains("NAN")),
                "{step}: NAN signal"
            );
        };
    oracle.startup().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    // u = 5 was set before Startup: both sides clamp it to 1 at its return.
    compare(&state, &oracle, "Startup");
    // flag is true, so the NaN comparison is not evaluated: no NAN raised.
    // Then flag is false and the comparison runs on NaN: NAN raised.
    for step in 0..2 {
        oracle.do_step().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .unwrap();
        compare(&state, &oracle, &format!("DoStep {step}"));
    }
    assert_eq!(solve_bits(state[nan_status].as_ref()), Bits::Boolean(true));
}

#[test]
fn integer_bounds_are_decided_exactly_in_a_signed64_domain() {
    let signed64 = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let mut block = controller();
    let mut count = decl(ScalarType::Integer, "count", 6);
    count.range = RangeAttributes {
        min: Some(Expression::Integer(-9_223_372_036_854_775_807)),
        max: Some(Expression::Integer(9_007_199_254_740_993)),
    };
    let mut wide = decl(ScalarType::Integer, "wide", 7);
    wide.range = RangeAttributes {
        min: Some(Expression::Integer(i64::MIN)),
        max: Some(Expression::Real(0.5)),
    };
    block.protected.push(ProtectedEntity {
        kind: ProtectedKind::State,
        decl: count,
        start: None,
    });
    let exact = package(block.clone());
    let scope = block_scope(exact.block(), signed64).unwrap();
    let count = &scope.block[scope.by_name["count"]];
    // Neither bound survives an f64 round trip; both stay exact.
    assert_eq!(count.low, Some(Bound::Integer(-9_223_372_036_854_775_807)));
    assert_eq!(count.high, Some(Bound::Integer(9_007_199_254_740_993)));
    block.protected.push(ProtectedEntity {
        kind: ProtectedKind::State,
        decl: wide,
        start: None,
    });
    let fractional = package(block);
    let Err(error) = block_scope(fractional.block(), signed64) else {
        panic!("a fractional Integer bound is refused");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
}

#[test]
fn bounds_outside_the_target_domain_are_refused_with_their_feature() {
    let mut block = controller();
    let mut edge = decl(ScalarType::Integer, "edge", 6);
    edge.range = RangeAttributes {
        min: Some(Expression::Integer(i32::MIN.into())),
        max: Some(Expression::Integer(i32::MAX.into())),
    };
    block.protected.push(ProtectedEntity {
        kind: ProtectedKind::State,
        decl: edge,
        start: None,
    });
    let at_edge = package(block.clone());
    let scope = block_scope(at_edge.block(), binary32()).unwrap();
    let edge = &scope.block[scope.by_name["edge"]];
    // A bound at the int32 edge is unviolable and elided.
    assert_eq!((edge.low, edge.high), (None, None));

    let mut wide = decl(ScalarType::Integer, "wide", 7);
    wide.range = RangeAttributes {
        min: None,
        max: Some(Expression::Integer(3_000_000_000)),
    };
    let mut outside = block.clone();
    outside.protected.push(ProtectedEntity {
        kind: ProtectedKind::State,
        decl: wide,
        start: None,
    });
    let Err(error) = block_scope(package(outside).block(), binary32()) else {
        panic!("an Integer bound beyond the int32 domain is refused");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:target-integer-range"),
        "{error}"
    );

    block.protected[0].decl = ranged(decl(ScalarType::Real, "x", 6), 1.0e60, 2.0e60);
    let Err(error) = block_scope(package(block).block(), binary32()) else {
        panic!("a Real min beyond binary32 is refused");
    };
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:target-real-range"),
        "{error}"
    );
}

fn integer_selector() -> Block {
    let mut block = Block::new(n("IntegerSelector"));
    block.interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: VariableDeclaration::scalar(ScalarType::Integer, n("k")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Output,
            decl: VariableDeclaration::scalar(ScalarType::Integer, n("m")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: real_decl(n("period")),
            start: Some(r(0.01)),
        },
    ];
    block.protected = vec![ProtectedEntity {
        kind: ProtectedKind::State,
        decl: VariableDeclaration::scalar(ScalarType::Boolean, n("flag")),
        start: None,
    }];
    block.startup.statements = vec![
        plain(state("flag"), Expression::Bool(true)),
        plain(state("m"), Expression::Integer(0)),
    ];
    block.do_step.statements = vec![
        plain(
            state("m"),
            Expression::If(IfExpression::new(
                vec![(sref("flag"), Expression::Integer(0))],
                bin(BinaryOp::Mul, sref("k"), sref("k")),
            )),
        ),
        plain(state("flag"), Expression::Not(Box::new(sref("flag")))),
    ];
    respan(&mut block);
    block
}

#[test]
fn an_unselected_integer_arm_does_not_overflow() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let package = AlgorithmCodePackage::construct(integer_selector(), vec![None; 4], "period")
        .expect("the selector validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    let names = ["k", "m", "period", "flag"];
    let mut state = vec![None; solve.slots().len()];
    // k * k overflows signed32.
    let k = SolveValue::integer(binary64(), 100_000).unwrap();
    oracle.set_state("k", Value::Integer(100_000)).unwrap();
    state[0] = Some(
        rumoca_eval_solve::TypedValue::construct(k.value_type().clone(), vec![k.kind()]).unwrap(),
    );
    oracle.set_state("period", Value::Real(0.01)).unwrap();
    state[2] = Some(real64(0.01));
    oracle.startup().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    // flag is true: the overflowing arm is not evaluated on either side.
    oracle.do_step().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
        .expect("the unselected arm does not fault");
    for (slot, name) in names.iter().enumerate() {
        assert_eq!(
            solve_bits(state[slot].as_ref()),
            oracle_bits(oracle.state(name).unwrap()),
            "DoStep 0: {name}"
        );
    }
    // flag is false: the selected arm overflows on both sides.
    assert!(oracle.do_step().is_err());
    assert!(
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .is_err()
    );
}

#[test]
fn lifecycle_methods_require_initialized_state() {
    use rumoca_ir_solve::SolveBlockMethodKind;

    // Startup that leaves the output unassigned is refused at lowering.
    let mut block = lazy_selector();
    block.startup.statements.pop();
    let package = AlgorithmCodePackage::construct(block, vec![None; 5], "period")
        .expect("the selector validates");
    let Err(error) = lower_algorithm_block(&package, binary64()) else {
        panic!("Startup must define the output");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(error.to_string().contains("Startup"), "{error}");

    // DoStep before Startup is refused by the method evaluator.
    let package = AlgorithmCodePackage::construct(lazy_selector(), vec![None; 5], "period")
        .expect("the selector validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let mut state = vec![None; solve.slots().len()];
    for (slot, value) in [(0, 0.5), (1, 0.5), (3, 0.01)] {
        state[slot] = Some(real64(value));
    }
    let before = state.clone();
    assert!(
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .is_err()
    );
    assert_eq!(state, before, "a refused method leaves the state unchanged");

    // Startup with a missing input is refused, not run unclamped.
    let mut missing = before.clone();
    missing[1] = None;
    assert!(
        rumoca_eval_solve::eval_algorithm_method(
            &solve,
            SolveBlockMethodKind::Startup,
            &mut missing
        )
        .is_err()
    );
    assert_eq!(missing[2], None, "Startup did not run");
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
        .expect("DoStep after a successful Startup");
}

#[test]
fn declared_signals_become_the_method_escape_set() {
    let package = AlgorithmCodePackage::construct(rate_controller(), vec![None; 11], "period")
        .expect("the golden RateController validates");
    let block = lower_algorithm_block(&package, binary32()).expect("refined");
    let escapes = |method: &rumoca_ir_solve::SolveBlockMethod| {
        method
            .escapes()
            .iter()
            .map(|&slot| block.slots()[slot].source().qualified_name().to_owned())
            .collect::<Vec<_>>()
    };
    assert_eq!(escapes(block.do_step()), ["ErrorSignalStatus.NAN"]);
    assert!(escapes(block.startup()).is_empty());

    // The checked package derives each clause from GALEC's escape analysis;
    // lowering cross-checks it against what it actually raises.
    let scope = block_scope(package.block(), binary32()).unwrap();
    let mut undeclared = package.block().do_step.clone();
    undeclared.signals.clear();
    let do_step = method_scope(&scope, Method::DoStep, &undeclared).unwrap();
    do_step.raised.borrow_mut().insert(NAN_SIGNAL);
    let Err(error) = declared_signals(&do_step, &undeclared) else {
        panic!("an undeclared NAN is refused");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
}

const UNARY_BUILTINS: [&str; 17] = [
    "roundDown",
    "roundUp",
    "sign",
    "absolute",
    "sqrt",
    "exp",
    "ln",
    "lg",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "sinh",
    "cosh",
    "tanh",
];
const BINARY_BUILTINS: [&str; 3] = ["atan2", "min", "max"];

fn builtin_call(name: &str, arguments: Vec<Expression>) -> Expression {
    Expression::Call(rumoca_ir_galec::ast::FunctionCall {
        function: n(name),
        arguments,
    })
}

/// One output per supported built-in over inputs `a` and `b`.
fn builtins_block() -> (Block, Vec<String>) {
    let mut block = Block::new(n("Builtins"));
    let mut interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: real_decl(n("a")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: real_decl(n("b")),
            start: None,
        },
    ];
    let mut calls = Vec::new();
    for name in UNARY_BUILTINS {
        calls.push((format!("{name}_a"), builtin_call(name, vec![sref("a")])));
    }
    for name in BINARY_BUILTINS {
        calls.push((
            format!("{name}_ab"),
            builtin_call(name, vec![sref("a"), sref("b")]),
        ));
        calls.push((
            format!("{name}_ba"),
            builtin_call(name, vec![sref("b"), sref("a")]),
        ));
    }
    calls.push(("pi".into(), builtin_call("pi", Vec::new())));
    calls.push(("euler".into(), builtin_call("euler", Vec::new())));
    calls.push(("isNaN_a".into(), builtin_call("isNaN", vec![sref("a")])));
    let mut names = vec!["a".to_owned(), "b".to_owned()];
    for (output, call) in &calls {
        let ty = if output == "isNaN_a" {
            ScalarType::Boolean
        } else {
            ScalarType::Real
        };
        interface.push(InterfaceVariable {
            kind: InterfaceKind::Output,
            decl: VariableDeclaration::scalar(ty, n(output)),
            start: None,
        });
        block.startup.statements.push(plain(
            state(output),
            if output == "isNaN_a" {
                Expression::Bool(false)
            } else {
                r(0.0)
            },
        ));
        block
            .do_step
            .statements
            .push(plain(state(output), call.clone()));
        names.push(output.clone());
    }
    interface.push(InterfaceVariable {
        kind: InterfaceKind::TunableParameter,
        decl: real_decl(n("period")),
        start: Some(r(0.01)),
    });
    names.push("period".to_owned());
    block.interface = interface;
    respan(&mut block);
    (block, names)
}

#[test]
fn builtin_calls_match_the_galec_evaluator() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let (block, names) = builtins_block();
    let package = AlgorithmCodePackage::construct(block, vec![None; names.len()], "period")
        .expect("the built-ins block validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let inputs = [
        (0.5, -0.25),
        (-0.0, 0.0),
        (0.0, -0.0),
        (f64::NAN, 1.0),
        (1.0, f64::NAN),
        (f64::INFINITY, f64::NEG_INFINITY),
        (-3.7, 2.0),
        (1.0e300, -1.0e-300),
        (2.0, 2.0),
    ];
    for (a, b) in inputs {
        let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
        let mut state = vec![None; solve.slots().len()];
        let period = names.len() - 1;
        for (slot, name, value) in [(0, "a", a), (1, "b", b), (period, "period", 0.01)] {
            oracle.set_state(name, Value::Real(value)).unwrap();
            state[slot] = Some(real64(value));
        }
        oracle.startup().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
            .unwrap();
        oracle.do_step().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .unwrap();
        for (slot, name) in names.iter().enumerate() {
            assert_eq!(
                solve_bits(state[slot].as_ref()),
                oracle_bits(oracle.state(name).unwrap()),
                "a={a:?} b={b:?}: {name}"
            );
        }
        assert!(
            oracle.active_signals().is_empty(),
            "built-ins raise nothing"
        );
    }
}

#[test]
fn an_unsupported_builtin_is_refused() {
    let (mut block, names) = builtins_block();
    block.do_step.statements.push(plain(
        state("pi"),
        builtin_call("roundHalfToEven", vec![sref("a")]),
    ));
    respan(&mut block);
    let package = AlgorithmCodePackage::construct(block, vec![None; names.len()], "period")
        .expect("the block validates");
    let Err(error) = lower_algorithm_block(&package, binary64()) else {
        panic!("roundHalfToEven is not refined in this slice");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(error.to_string().contains("built-in"), "{error}");
}

const INTEGER_BUILTIN_NAMES: [&str; 11] = [
    "x", "i", "j", "flag", "ix", "ri", "mn", "mx", "dq", "dr", "period",
];

/// Integer built-ins, `integer()` and a `signal` statement over inputs
/// `x`, `i`, `j` and `flag`; the faulting divisions come last.
fn integer_builtins_block() -> Block {
    let input = |name: &str, ty| InterfaceVariable {
        kind: InterfaceKind::Input,
        decl: VariableDeclaration::scalar(ty, n(name)),
        start: None,
    };
    let output = |name: &str, ty| InterfaceVariable {
        kind: InterfaceKind::Output,
        decl: VariableDeclaration::scalar(ty, n(name)),
        start: None,
    };
    let mut block = Block::new(n("IntegerBuiltins"));
    block.interface = vec![
        input("x", ScalarType::Real),
        input("i", ScalarType::Integer),
        input("j", ScalarType::Integer),
        input("flag", ScalarType::Boolean),
        output("ix", ScalarType::Integer),
        output("ri", ScalarType::Real),
        output("mn", ScalarType::Integer),
        output("mx", ScalarType::Integer),
        output("dq", ScalarType::Integer),
        output("dr", ScalarType::Integer),
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: real_decl(n("period")),
            start: Some(r(0.01)),
        },
    ];
    for name in ["ix", "mn", "mx", "dq", "dr"] {
        block
            .startup
            .statements
            .push(plain(state(name), Expression::Integer(0)));
    }
    block.startup.statements.push(plain(state("ri"), r(0.0)));
    let ij = || vec![sref("i"), sref("j")];
    block.do_step.statements = vec![
        plain(state("ix"), builtin_call("integer", vec![sref("x")])),
        plain(state("ri"), builtin_call("real", vec![sref("i")])),
        plain(state("mn"), builtin_call("imin", ij())),
        plain(state("mx"), builtin_call("imax", ij())),
        Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(sref("flag")),
                body: vec![Spanned::dummy(Statement::Signal(vec![
                    rumoca_ir_galec::ast::Identifier::new("UNSPECIFIED_ERROR"),
                ]))],
                span: Span::DUMMY,
            }],
            else_body: None,
        })),
        plain(state("dq"), builtin_call("divisionTowardsZero", ij())),
        plain(state("dr"), builtin_call("remainderTowardsZero", ij())),
    ];
    respan(&mut block);
    block
}

fn int32(value: i64) -> rumoca_eval_solve::TypedValue {
    let value = SolveValue::integer(binary64(), value).unwrap();
    rumoca_eval_solve::TypedValue::construct(value.value_type().clone(), vec![value.kind()])
        .unwrap()
}

fn boolean(value: bool) -> rumoca_eval_solve::TypedValue {
    let value = SolveValue::boolean(value);
    rumoca_eval_solve::TypedValue::construct(value.value_type().clone(), vec![value.kind()])
        .unwrap()
}

#[test]
fn integer_builtins_and_signal_statements_match_the_galec_evaluator() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let names = INTEGER_BUILTIN_NAMES;
    let package = AlgorithmCodePackage::construct(
        integer_builtins_block(),
        vec![None; names.len()],
        "period",
    )
    .expect("the Integer built-ins block validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let signals = [
        "INVALID_ARGUMENT",
        "OVERFLOW",
        "NAN",
        "SOLVE_LINEAR_EQUATIONS_FAILED",
        "NO_SOLUTION_FOUND",
        "UNSPECIFIED_ERROR",
    ];
    let xs = [
        f64::NAN,
        2.7,
        -2.7,
        -0.0,
        2_147_483_647.9,
        2_147_483_648.0,
        -2_147_483_648.5,
        -2_147_483_649.0,
        f64::INFINITY,
    ];
    let pairs = [(7, -2), (-7, 2), (5, 0), (i64::from(i32::MIN), -1)];
    let mut cases = 0;
    for (case, x) in xs.into_iter().enumerate() {
        for (i, j) in pairs {
            let flag = case % 2 == 0;
            let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
            let mut state = vec![None; solve.slots().len()];
            oracle.set_state("x", Value::Real(x)).unwrap();
            oracle.set_state("i", Value::Integer(i)).unwrap();
            oracle.set_state("j", Value::Integer(j)).unwrap();
            oracle.set_state("flag", Value::Boolean(flag)).unwrap();
            oracle.set_state("period", Value::Real(0.01)).unwrap();
            state[0] = Some(real64(x));
            state[1] = Some(int32(i));
            state[2] = Some(int32(j));
            state[3] = Some(boolean(flag));
            state[10] = Some(real64(0.01));
            oracle.startup().unwrap();
            rumoca_eval_solve::eval_algorithm_method(
                &solve,
                SolveBlockMethodKind::Startup,
                &mut state,
            )
            .unwrap();
            let expected = oracle.do_step();
            let actual = rumoca_eval_solve::eval_algorithm_method(
                &solve,
                SolveBlockMethodKind::DoStep,
                &mut state,
            );
            let at = format!("x={x:?} i={i} j={j} flag={flag}");
            assert_eq!(
                actual.is_ok(),
                expected.is_ok(),
                "{at}: {actual:?} vs {expected:?}"
            );
            // A failed DoStep keeps what it stored before the fault, on both
            // sides.
            if expected.is_ok() {
                cases += 1;
            }
            for (slot, name) in names.iter().enumerate() {
                assert_eq!(
                    solve_bits(state[slot].as_ref()),
                    oracle_bits(oracle.state(name).unwrap()),
                    "{at}: {name}"
                );
            }
            for (bit, signal) in signals.iter().enumerate() {
                assert_eq!(
                    solve_bits(state[names.len() + bit].as_ref()),
                    Bits::Boolean(oracle.active_signals().contains(*signal)),
                    "{at}: {signal}"
                );
            }
        }
    }
    assert_eq!(
        cases,
        2 * xs.len(),
        "the two non-faulting pairs run everywhere"
    );
}

fn user_function(
    name: &str,
    inputs: &[&str],
    outputs: &[&str],
    statements: Vec<Spanned<Statement>>,
) -> rumoca_ir_galec::ast::UserFunction {
    use rumoca_ir_galec::ast::{Direction, FunctionKind, Parameter};
    let parameter = |direction, name: &str| Parameter {
        direction,
        decl: real_decl(n(name)),
    };
    rumoca_ir_galec::ast::UserFunction {
        kind: FunctionKind::Stateless,
        name: n(name),
        signals: Vec::new(),
        parameters: inputs
            .iter()
            .map(|name| parameter(Direction::Input, name))
            .chain(
                outputs
                    .iter()
                    .map(|name| parameter(Direction::Output, name)),
            )
            .collect(),
        locals: Vec::new(),
        statements,
        span: Span::DUMMY,
    }
}

fn lsref(name: &str) -> Expression {
    Expression::Ref(local(name))
}

/// `ratio` compares (so may raise NAN); `pair` calls it and returns two
/// outputs; DoStep calls both, one in an expression and one in a multiple
/// assignment.
fn function_caller() -> Block {
    let mut block = Block::new(n("FunctionCaller"));
    let real_io = |kind, name: &str| InterfaceVariable {
        kind,
        decl: real_decl(n(name)),
        start: None,
    };
    block.interface = vec![
        real_io(InterfaceKind::Input, "u"),
        real_io(InterfaceKind::Input, "v"),
        real_io(InterfaceKind::Output, "y"),
        real_io(InterfaceKind::Output, "s"),
        real_io(InterfaceKind::Output, "d"),
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: real_decl(n("period")),
            start: Some(r(0.01)),
        },
    ];
    let ratio = user_function(
        "ratio",
        &["a", "b"],
        &["q"],
        vec![plain(
            local("q"),
            Expression::If(IfExpression::new(
                vec![(
                    bin(BinaryOp::Gt, lsref("a"), lsref("b")),
                    bin(BinaryOp::Div, lsref("a"), lsref("b")),
                )],
                bin(BinaryOp::Sub, lsref("b"), lsref("a")),
            )),
        )],
    );
    let pair = user_function(
        "pair",
        &["x"],
        &["sum", "twice"],
        vec![
            plain(
                local("sum"),
                builtin_call("ratio", vec![lsref("x"), r(2.0)]),
            ),
            plain(local("twice"), bin(BinaryOp::Mul, lsref("x"), r(2.0))),
        ],
    );
    block.protected_functions = vec![ratio, pair];
    for name in ["y", "s", "d"] {
        block.startup.statements.push(plain(state(name), r(0.0)));
    }
    block.do_step.statements = vec![
        plain(
            state("y"),
            builtin_call("ratio", vec![sref("u"), sref("v")]),
        ),
        Spanned::dummy(Statement::MultiAssignment {
            targets: vec![state("s"), state("d")],
            call: rumoca_ir_galec::ast::FunctionCall {
                function: n("pair"),
                arguments: vec![sref("v")],
            },
        }),
    ];
    respan(&mut block);
    block
}

#[test]
fn user_function_calls_match_the_galec_evaluator() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let names = ["u", "v", "y", "s", "d", "period"];
    let package =
        AlgorithmCodePackage::construct(function_caller(), vec![None; names.len()], "period")
            .expect("the function caller validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    assert_eq!(solve.functions().owners().len(), 2);
    assert_eq!(
        solve
            .function_sources()
            .iter()
            .map(|source| source.qualified_name())
            .collect::<Vec<_>>(),
        ["ratio", "pair"],
        "callee first"
    );
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let nan_status = names.len() + NAN_SIGNAL;
    for (u, v) in [
        (3.0, 1.5),
        (1.0, 4.0),
        (f64::NAN, 1.0),
        (2.0, f64::NAN),
        (-0.0, 0.0),
    ] {
        let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
        let mut state = vec![None; solve.slots().len()];
        for (slot, name, value) in [(0, "u", u), (1, "v", v), (5, "period", 0.01)] {
            oracle.set_state(name, Value::Real(value)).unwrap();
            state[slot] = Some(real64(value));
        }
        oracle.startup().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
            .unwrap();
        oracle.do_step().unwrap();
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .unwrap();
        for (slot, name) in names.iter().enumerate() {
            assert_eq!(
                solve_bits(state[slot].as_ref()),
                oracle_bits(oracle.state(name).unwrap()),
                "u={u:?} v={v:?}: {name}"
            );
        }
        assert_eq!(
            solve_bits(state[nan_status].as_ref()),
            Bits::Boolean(oracle.active_signals().contains("NAN")),
            "u={u:?} v={v:?}: NAN"
        );
    }
}

/// DoStep stores `out := 1.0`, then `count := k + 1`, which overflows for
/// `k = i32::MAX`; `looped` puts both in a two-iteration loop, else they sit
/// under `if flag`.
fn guarded_block(looped: bool) -> Block {
    use rumoca_ir_galec::ast::ForLoop;
    let mut block = Block::new(n("Guarded"));
    block.interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: VariableDeclaration::scalar(ScalarType::Boolean, n("flag")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: VariableDeclaration::scalar(ScalarType::Integer, n("k")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Output,
            decl: real_decl(n("out")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: real_decl(n("period")),
            start: Some(r(0.01)),
        },
    ];
    block.protected = vec![ProtectedEntity {
        kind: ProtectedKind::State,
        decl: VariableDeclaration::scalar(ScalarType::Integer, n("count")),
        start: None,
    }];
    block.startup.statements = vec![
        plain(state("out"), r(0.0)),
        plain(state("count"), Expression::Integer(0)),
    ];
    let body = vec![
        plain(state("out"), r(1.0)),
        plain(
            state("count"),
            bin(BinaryOp::Add, sref("k"), Expression::Integer(1)),
        ),
    ];
    let statement = if looped {
        Statement::for_loop(ForLoop::new(
            Some(n("step")),
            Expression::Integer(1),
            None,
            Expression::Integer(2),
            body,
        ))
    } else {
        Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(sref("flag")),
                body,
                span: Span::DUMMY,
            }],
            else_body: None,
        })
    };
    block.do_step.statements = vec![Spanned::dummy(statement)];
    respan(&mut block);
    block
}

#[test]
fn an_if_body_publishes_each_statement_and_a_loop_that_could_lose_one_is_refused() {
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    // Split `if`: `out` is stored before `count` overflows, on both sides.
    let package = AlgorithmCodePackage::construct(guarded_block(false), vec![None; 5], "period")
        .expect("the guarded block validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("a split if is exact");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    let names = ["flag", "k", "out", "period", "count"];
    let mut state = vec![None; solve.slots().len()];
    oracle.set_state("flag", Value::Boolean(true)).unwrap();
    oracle
        .set_state("k", Value::Integer(i32::MAX.into()))
        .unwrap();
    oracle.set_state("period", Value::Real(0.01)).unwrap();
    state[0] = Some(boolean(true));
    state[1] = Some(int32(i32::MAX.into()));
    state[3] = Some(real64(0.01));
    oracle.startup().unwrap();
    rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
        .unwrap();
    assert!(oracle.do_step().is_err());
    assert!(
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::DoStep, &mut state)
            .is_err()
    );
    for (slot, name) in names.iter().enumerate() {
        assert_eq!(
            solve_bits(state[slot].as_ref()),
            oracle_bits(oracle.state(name).unwrap()),
            "after the fault: {name}"
        );
    }
    assert_eq!(solve_bits(state[2].as_ref()), Bits::Real(1.0f64.to_bits()));

    // In a loop the first iteration's store precedes the second's fault, and
    // the loop is one region: refused.
    let package = AlgorithmCodePackage::construct(guarded_block(true), vec![None; 5], "period")
        .expect("the looped block validates");
    let Err(error) = lower_algorithm_block(&package, binary64()) else {
        panic!("an effect before a fault inside one loop region is refused");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(error.to_string().contains("fault after"), "{error}");
}

mod refusals;
