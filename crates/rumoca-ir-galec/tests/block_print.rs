//! Golden `.alg` printer tests: an independently authored fixed-sample
//! discrete PID-shaped block, a 2D-array/for-loop block proving
//! array-nativeness (GAL-026), and a signal-machinery block (GAL-018).
//! All fixtures are original constructions — no standard text reproduced.

use rumoca_ir_galec::ast::{
    BinaryOp, Block, BlockMethod, Condition, Dimension, Direction, Expression, ForLoop,
    FunctionCall, FunctionKind, Identifier, IfBranch, IfExpression, IfStatement, InterfaceKind,
    InterfaceVariable, LimitTarget, Name, Parameter, PredefinedSignal, ProtectedEntity,
    ProtectedKind, RangeAttributes, RefPart, Reference, ScalarType, SignalCheck, SignalTest,
    Statement, UserFunction, VariableDeclaration,
};
use rumoca_ir_galec::print_block;

// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------

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

fn bin(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::binary(op, lhs, rhs)
}

fn assign(target: Reference, value: Expression) -> Statement {
    Statement::Assignment { target, value }
}

fn call(function: &str, arguments: Vec<Expression>) -> FunctionCall {
    FunctionCall {
        function: n(function),
        arguments,
    }
}

// ---------------------------------------------------------------------------
// PID-shaped golden fixture (independently authored)
// ---------------------------------------------------------------------------

fn rate_controller_interface() -> Vec<InterfaceVariable> {
    vec![
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
    ]
}

fn rate_controller_protected() -> Vec<ProtectedEntity> {
    vec![
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
    ]
}

/// Dependent parameter recomputation shared by `Startup` (inline) and
/// `Recalibrate` (GAL-020).
fn dependent_gain_update() -> Statement {
    assign(
        state("integralGain"),
        bin(BinaryOp::Div, sref("gain"), sref("integralTime")),
    )
}

fn rate_controller_startup() -> BlockMethod {
    BlockMethod {
        statements: vec![
            assign(state("gain"), r(2.5)),
            assign(state("integralTime"), r(0.5)),
            assign(state("derivativeTime"), r(0.01)),
            assign(state("period"), r(0.005)),
            dependent_gain_update(),
            assign(state("integralState"), r(0.0)),
            assign(stateq("previous(trackingError)"), r(0.0)),
            assign(state("firstTick"), Expression::Bool(true)),
            assign(state("drive"), r(0.0)),
        ],
        ..Default::default()
    }
}

fn rate_controller_do_step() -> BlockMethod {
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
            // `'previous(x)'` state committed at the END of DoStep (trap T2).
            assign(stateq("previous(trackingError)"), lref("trackingError")),
        ],
    }
}

fn rate_controller() -> Block {
    Block {
        interface: rate_controller_interface(),
        protected: rate_controller_protected(),
        startup: rate_controller_startup(),
        recalibrate: BlockMethod {
            statements: vec![dependent_gain_update()],
            ..Default::default()
        },
        do_step: rate_controller_do_step(),
        ..Block::new(n("RateController"))
    }
}

const RATE_CONTROLLER_ALG: &str = "block RateController
    input Real speedSetpoint(min = -500.0, max = 500.0);
    input Real speedMeasured(min = -500.0, max = 500.0);
    output Real drive(min = -50.0, max = 50.0);
    parameter Real gain(min = 0.0, max = 100.0);
    parameter Real integralTime(min = 0.000001, max = 1000.0);
    parameter Real derivativeTime(min = 0.0, max = 10.0);
    parameter Real period(min = 0.000001, max = 1.0);
protected
    parameter Real integralGain;
    Real integralState(min = -100.0, max = 100.0);
    Real 'previous(trackingError)';
    Boolean firstTick;
public
    method Startup
    algorithm
        self.gain := 2.5;
        self.integralTime := 0.5;
        self.derivativeTime := 0.01;
        self.period := 0.005;
        self.integralGain := self.gain / self.integralTime;
        self.integralState := 0.0;
        self.'previous(trackingError)' := 0.0;
        self.firstTick := true;
        self.drive := 0.0;
    end Startup;
    method Recalibrate
    algorithm
        self.integralGain := self.gain / self.integralTime;
    end Recalibrate;
    method DoStep
    protected
        Real trackingError;
        Real derivativeEstimate;
        Real unboundedDrive;
    algorithm
        trackingError := self.speedSetpoint - self.speedMeasured;
        if self.firstTick then
            self.firstTick := false;
            derivativeEstimate := 0.0;
        else
            derivativeEstimate := (trackingError - self.'previous(trackingError)') / self.period;
            self.integralState := self.integralState + (self.integralGain * trackingError * self.period);
        end if;
        limit self.integralState;
        unboundedDrive := (self.gain * trackingError) + self.integralState + (self.derivativeTime * derivativeEstimate);
        self.drive := (if unboundedDrive > 50.0 then 50.0 elseif unboundedDrive < (-50.0) then -50.0 else unboundedDrive);
        self.'previous(trackingError)' := trackingError;
    end DoStep;
end RateController;
";

#[test]
fn golden_pid_shaped_block() {
    let printed = print_block(&rate_controller()).expect("fixture must print");
    assert_eq!(printed, RATE_CONTROLLER_ALG);
}

#[test]
fn golden_pid_shaped_block_validates() {
    rumoca_ir_galec::validate(&rate_controller()).expect("golden PID fixture must validate");
}

// ---------------------------------------------------------------------------
// 2D array + for-loop fixture (array-nativeness, GAL-026)
// ---------------------------------------------------------------------------

fn matrix_startup() -> BlockMethod {
    let zero_row = || Expression::Array(vec![r(0.0), r(0.0), r(0.0)]);
    BlockMethod {
        statements: vec![
            assign(state("period"), r(0.01)),
            assign(
                state("accumulator"),
                Expression::Array(vec![zero_row(), zero_row()]),
            ),
            assign(state("rowMean"), Expression::Array(vec![r(0.0), r(0.0)])),
        ],
        ..Default::default()
    }
}

fn matrix_do_step() -> BlockMethod {
    let size_dim = |d: i64| Expression::Size {
        array: state("accumulator"),
        dimension: Box::new(Expression::Integer(d)),
    };
    let ij = || vec![lref("i"), lref("j")];
    let inner = Statement::For(ForLoop {
        iterator: Some(n("j")),
        start: Expression::Integer(1),
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
        start: Expression::Integer(1),
        step: None,
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
            interface(
                InterfaceKind::TunableParameter,
                ranged(real_decl(n("period")), 0.001, 1.0),
                Some(r(0.01)),
            ),
        ],
        protected: vec![entity(
            ProtectedKind::State,
            dims(real_decl(n("accumulator")), &[2, 3]),
            None,
        )],
        startup: matrix_startup(),
        do_step: matrix_do_step(),
        ..Block::new(n("MatrixAverager"))
    }
}

const MATRIX_AVERAGER_ALG: &str = "block MatrixAverager
    input Real samples[2, 3](min = -1000.0, max = 1000.0);
    output Real rowMean[2];
    parameter Real period(min = 0.001, max = 1.0);
protected
    Real accumulator[2, 3];
public
    method Startup
    algorithm
        self.period := 0.01;
        self.accumulator := {{0.0, 0.0, 0.0}, {0.0, 0.0, 0.0}};
        self.rowMean := {0.0, 0.0};
    end Startup;
    method Recalibrate
    algorithm
    end Recalibrate;
    method DoStep
    protected
        Real total;
    algorithm
        for i in 1:size(self.accumulator, 1) loop
            total := 0.0;
            for j in 1:size(self.accumulator, 2) loop
                self.accumulator[i, j] := self.samples[i, j];
                total := total + self.accumulator[i, j];
            end for;
            self.rowMean[i] := total / 3.0;
        end for;
    end DoStep;
end MatrixAverager;
";

#[test]
fn golden_array_native_block() {
    let printed = print_block(&matrix_averager()).expect("fixture must print");
    assert_eq!(printed, MATRIX_AVERAGER_ALG);
}

#[test]
fn golden_array_native_block_validates() {
    rumoca_ir_galec::validate(&matrix_averager()).expect("array-native fixture must validate");
}

// ---------------------------------------------------------------------------
// Signal machinery fixture (GAL-018)
// ---------------------------------------------------------------------------

fn classify_function() -> UserFunction {
    UserFunction {
        kind: FunctionKind::Stateless,
        name: n("classify"),
        signals: vec![id("sensorFault")],
        parameters: vec![
            Parameter {
                direction: Direction::Input,
                decl: real_decl(n("v")),
            },
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
                    Expression::Call(call("absolute", vec![lref("v")])),
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

/// DoStep body with a coherent §3.2.5 signal dataflow: after the solves,
/// {sensorFault, SOLVE_LINEAR_EQUATIONS_FAILED} may be pending and a
/// conditional `signal NAN;` adds NAN; the guard's first branch catches the
/// two fault signals (leaving NAN), the `not in` branch catches NAN, the
/// unrestricted final check catches any remainder and deliberately
/// re-raises NAN so DoStep's declared escape set is exactly {NAN}.
fn signal_guard_statements() -> Vec<Statement> {
    let identity = Expression::Array(vec![
        Expression::Array(vec![r(2.0), r(0.0)]),
        Expression::Array(vec![r(0.0), r(4.0)]),
    ]);
    let nan_raise = IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(Expression::Call(call(
                "isNaN",
                vec![Expression::Ref(local_idx("solution", 1))],
            ))),
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
        assign(
            local("ok"),
            Expression::Call(call("classify", vec![sref("reading")])),
        ),
        Statement::MultiAssignment {
            targets: vec![local("lu"), local("pivots")],
            call: call("luFactorize", vec![identity]),
        },
        Statement::MultiAssignment {
            targets: vec![local("solution")],
            call: call(
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

const SIGNAL_GUARD_ALG: &str = "block SignalGuard
    input Real reading(min = -100.0, max = 100.0);
    output Real filtered;
protected
    signal sensorFault;
    function classify
        signals sensorFault;
        input Real v;
        output Boolean ok;
    algorithm
        if absolute(v) > 99.0 then
            signal sensorFault;
            ok := false;
        else
            ok := true;
        end if;
    end classify;
public
    method Startup
    algorithm
        self.filtered := 0.0;
    end Startup;
    method Recalibrate
    algorithm
    end Recalibrate;
    method DoStep
        signals NAN;
    protected
        Boolean ok;
        Real lu[2, 2];
        Integer pivots[2];
        Real solution[2];
    algorithm
        ok := classify(self.reading);
        (lu, pivots) := luFactorize({{2.0, 0.0}, {0.0, 4.0}});
        (solution) := luSolve(lu, pivots, {self.reading, 0.0});
        if isNaN(solution[1]) then
            signal NAN;
        end if;
        if signal s in sensorFault, SOLVE_LINEAR_EQUATIONS_FAILED or not (ok) then
            self.filtered := 0.0;
        elseif signal not in sensorFault then
            self.filtered := solution[1];
        else
            self.filtered := solution[1] / 2.0;
        end if;
        if signal then
            self.filtered := 0.0;
            signal NAN;
        end if;
        limit self;
    end DoStep;
end SignalGuard;
";

#[test]
fn golden_signal_machinery_block() {
    let printed = print_block(&signal_guard()).expect("fixture must print");
    assert_eq!(printed, SIGNAL_GUARD_ALG);
}

#[test]
fn golden_signal_machinery_block_validates() {
    rumoca_ir_galec::validate(&signal_guard()).expect("signal fixture must validate");
}

// ---------------------------------------------------------------------------
// Structural errors and layout details
// ---------------------------------------------------------------------------

fn minimal_block(statements: Vec<Statement>) -> Block {
    let mut block = Block::new(n("Minimal"));
    block.do_step.statements = statements;
    block
}

#[test]
fn for_loop_with_step_prints_start_step_stop() {
    let block = minimal_block(vec![Statement::For(ForLoop {
        iterator: Some(n("k")),
        start: Expression::Integer(8),
        step: Some(Expression::Integer(-2)),
        stop: Expression::Integer(2),
        body: vec![assign(local("k2"), lref("k"))],
    })]);
    let printed = print_block(&block).expect("block must print");
    assert!(
        printed.contains("for k in 8:-2:2 loop"),
        "missing stepped loop head in:\n{printed}"
    );
}

#[test]
fn empty_limit_statement_is_a_stable_error() {
    let block = minimal_block(vec![Statement::Limit(vec![])]);
    let error = print_block(&block).expect_err("empty limit must fail");
    assert_eq!(error.code(), "EG005");
    let message = error.to_string();
    assert!(
        message.contains("block Minimal / method DoStep / statement 0"),
        "location path missing in: {message}"
    );
}

#[test]
fn empty_signal_statement_is_a_stable_error() {
    let block = minimal_block(vec![Statement::Signal(vec![])]);
    let error = print_block(&block).expect_err("empty signal must fail");
    assert_eq!(error.code(), "EG004");
}

#[test]
fn if_statement_without_branches_is_a_stable_error() {
    let block = minimal_block(vec![Statement::If(IfStatement {
        branches: vec![],
        else_body: None,
    })]);
    let error = print_block(&block).expect_err("branchless if must fail");
    assert_eq!(error.code(), "EG006");
}

#[test]
fn empty_signal_test_list_is_a_stable_error() {
    let block = minimal_block(vec![Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::SignalCheck(SignalCheck {
                closure: None,
                test: Some(SignalTest {
                    negated: false,
                    signals: vec![],
                }),
                fallback: None,
            }),
            body: vec![],
        }],
        else_body: None,
    })]);
    let error = print_block(&block).expect_err("empty signal test list must fail");
    assert_eq!(error.code(), "EG008");
}

#[test]
fn compartment_entities_print_kind_prefixes() {
    let mut block = minimal_block(vec![]);
    block.compartments = vec![rumoca_ir_galec::ast::StateCompartment {
        name: n("Cfg"),
        entities: vec![
            entity(
                ProtectedKind::DependentParameter,
                real_decl(n("k")),
                Some(r(1.0)),
            ),
            entity(ProtectedKind::Constant, real_decl(n("c")), Some(r(2.0))),
            entity(ProtectedKind::State, real_decl(n("s")), Some(r(0.0))),
        ],
    }];
    let printed = print_block(&block).expect("block must print");
    let expected = "    record Cfg\n        parameter Real k;\n        constant Real c;\n        Real s;\n    end Cfg;\n";
    assert!(
        printed.contains(expected),
        "compartment section missing in:\n{printed}"
    );
}

#[test]
fn empty_recalibrate_is_still_emitted() {
    let block = minimal_block(vec![]);
    let printed = print_block(&block).expect("block must print");
    assert!(
        printed.contains("method Recalibrate\n    algorithm\n    end Recalibrate;"),
        "empty Recalibrate missing in:\n{printed}"
    );
}
