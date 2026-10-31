//! Refusals and their admitted neighbours: definedness, fault-visible state,
//! element coverage, and the power-of-two lowering.

use super::*;

/// Inputs `flag` (Boolean) and `v` (Real), output `y`, a Real DoStep local
/// `z`; DoStep is `statements`.
fn local_reader(statements: Vec<Spanned<Statement>>) -> Block {
    let mut block = Block::new(n("LocalReader"));
    block.interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: VariableDeclaration::scalar(ScalarType::Boolean, n("flag")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: real_decl(n("v")),
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
    block.startup.statements = vec![plain(state("y"), r(0.0))];
    block.do_step.locals = vec![real_decl(n("z"))];
    block.do_step.statements = statements;
    respan(&mut block);
    block
}

fn when_flag(body: Vec<Spanned<Statement>>) -> Spanned<Statement> {
    Spanned::dummy(Statement::If(IfStatement {
        branches: vec![IfBranch {
            condition: Condition::Expression(sref("flag")),
            body,
            span: Span::DUMMY,
        }],
        else_body: None,
    }))
}

fn refine(block: Block) -> Result<SolveAlgorithmBlock, AlgorithmBlockLowerError> {
    let package = AlgorithmCodePackage::construct(block, vec![None; 4], "period")
        .expect("the block validates");
    lower_algorithm_block(&package, binary64())
}

#[test]
fn a_read_of_a_possibly_unassigned_local_is_refused() {
    // Regression: z is never assigned, and the oracle faults on the read.
    let Err(error) = refine(local_reader(vec![plain(state("y"), lref("z"))])) else {
        panic!("an unassigned read is refused");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");

    // Assigned under `flag`, read under the same `flag`: proven.
    refine(local_reader(vec![
        when_flag(vec![plain(local("z"), sref("v"))]),
        when_flag(vec![plain(state("y"), lref("z"))]),
    ]))
    .expect("a read under the guard that defined it is proven");

    // Read outside the guard: refused.
    assert!(
        refine(local_reader(vec![
            when_flag(vec![plain(local("z"), sref("v"))]),
            plain(state("y"), lref("z")),
        ]))
        .is_err()
    );
}

#[test]
fn a_function_output_unassigned_on_some_path_is_refused() {
    use rumoca_ir_galec::ast::{Direction, FunctionKind, Parameter};
    let mut block = local_reader(vec![plain(
        state("y"),
        builtin_call("partial", vec![sref("v")]),
    )]);
    block.do_step.locals.clear();
    block.protected_functions = vec![rumoca_ir_galec::ast::UserFunction {
        kind: FunctionKind::Stateless,
        name: n("partial"),
        signals: vec![rumoca_ir_galec::ast::Identifier::new("NAN")],
        parameters: vec![
            Parameter {
                direction: Direction::Input,
                decl: real_decl(n("a")),
            },
            Parameter {
                direction: Direction::Output,
                decl: real_decl(n("b")),
            },
        ],
        locals: Vec::new(),
        statements: vec![Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(bin(BinaryOp::Gt, lsref("a"), r(0.0))),
                body: vec![plain(local("b"), lsref("a"))],
                span: Span::DUMMY,
            }],
            else_body: None,
        }))],
        span: Span::DUMMY,
    }];
    respan(&mut block);
    let Err(error) = refine(block) else {
        panic!("an output missing on one path is refused");
    };
    assert!(error.to_string().contains("output"), "{error}");
}

#[test]
fn a_limit_between_two_guards_forgets_the_guard() {
    // Regression: g := 2; if g < 1.5 then z := 7; limit g; if g < 1.5 then y := z.
    // The first guard is false, the limit makes the second true, and the
    // oracle faults on z.
    let guard = || Condition::Expression(bin(BinaryOp::Lt, lsref("g"), r(1.5)));
    let mut block = local_reader(vec![
        plain(local("g"), r(2.0)),
        Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: guard(),
                body: vec![plain(local("z"), r(7.0))],
                span: Span::DUMMY,
            }],
            else_body: None,
        })),
        Spanned::dummy(Statement::Limit(vec![LimitTarget::Reference(local("g"))])),
        Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: guard(),
                body: vec![plain(state("y"), lref("z"))],
                span: Span::DUMMY,
            }],
            else_body: None,
        })),
    ]);
    block
        .do_step
        .locals
        .push(ranged(real_decl(n("g")), 0.0, 1.0));
    block.do_step.signals = vec![PredefinedSignal::Nan];
    respan(&mut block);
    let Err(error) = refine(block) else {
        panic!("the limit invalidates the guard");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");
}

#[test]
fn a_later_condition_that_faults_after_an_earlier_raise_is_refused() {
    // Regression: the first condition is false, `v > 0` raises NAN on the false
    // path, then the next condition divides by zero. In one lazy region the
    // raise would be lost.
    let division = builtin_call(
        "divisionTowardsZero",
        vec![Expression::Integer(1), Expression::Integer(0)],
    );
    let selected = Expression::If(IfExpression::new(
        vec![
            (sref("flag"), r(1.0)),
            (bin(BinaryOp::Gt, sref("v"), r(0.0)), r(2.0)),
            (bin(BinaryOp::Gt, division, Expression::Integer(0)), r(3.0)),
        ],
        r(4.0),
    ));
    let mut block = local_reader(vec![plain(state("y"), selected)]);
    block.do_step.locals.clear();
    block.do_step.signals = vec![PredefinedSignal::Nan];
    respan(&mut block);
    let Err(error) = refine(block) else {
        panic!("a raise before a later fault in one lazy region is refused");
    };
    assert!(error.to_string().contains("fault after"), "{error}");
}

#[test]
fn a_guard_on_a_rebound_iterator_proves_nothing() {
    // Regression: for i in 1:1 { if i == 2 { z := 7 } }; for i in 2:2 { if i == 2
    // { y := z } }. The first loop never assigns z.
    use rumoca_ir_galec::ast::ForLoop;
    let on_two = |body| {
        Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(bin(
                    BinaryOp::Eq,
                    lsref("i"),
                    Expression::Integer(2),
                )),
                body,
                span: Span::DUMMY,
            }],
            else_body: None,
        }))
    };
    let over = |from: i64, body| {
        Spanned::dummy(Statement::for_loop(ForLoop::new(
            Some(n("i")),
            Expression::Integer(from),
            None,
            Expression::Integer(from),
            vec![body],
        )))
    };
    let block = local_reader(vec![
        over(1, on_two(vec![plain(local("z"), r(7.0))])),
        over(2, on_two(vec![plain(state("y"), lref("z"))])),
    ]);
    let Err(error) = refine(block) else {
        panic!("a fact under a guard on an earlier iterator binding is not reused");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");
}

#[test]
fn a_limited_local_has_no_assigned_range() {
    // Regression: k in [10, 20], k := 1, limit k (k becomes 10). With k's assigned
    // range [1, 1], k * 1000000000 would fit signed32; after the limit it
    // overflows, and inside the loop that fault follows the store of y.
    // (GALEC subscripts must be static (EG022), so the overflow carries it.)
    use rumoca_ir_galec::ast::ForLoop;
    let mut block = local_reader(vec![
        plain(local("k"), Expression::Integer(1)),
        Spanned::dummy(Statement::Limit(vec![LimitTarget::Reference(local("k"))])),
        Spanned::dummy(Statement::for_loop(ForLoop::new(
            Some(n("i")),
            Expression::Integer(1),
            None,
            Expression::Integer(1),
            vec![
                plain(state("y"), r(1.0)),
                plain(
                    local("m"),
                    bin(
                        BinaryOp::Mul,
                        lsref("k"),
                        Expression::Integer(1_000_000_000),
                    ),
                ),
            ],
        ))),
    ]);
    block.do_step.locals = vec![
        ranged(decl(ScalarType::Integer, "k", 1), 10.0, 20.0),
        decl(ScalarType::Integer, "m", 2),
    ];
    respan(&mut block);
    let Err(error) = refine(block) else {
        panic!("the limit leaves k without the assigned range");
    };
    assert!(error.to_string().contains("fault after"), "{error}");
}

#[test]
fn a_failed_startup_keeps_the_signals_it_raised() {
    // Regression: Startup compares a NaN input (raising NAN),
    // then overflows; the interpreter restores state but not its signals.
    use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
    use rumoca_ir_solve::SolveBlockMethodKind;

    let mut block = Block::new(n("StartupSignal"));
    block.interface = vec![
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: real_decl(n("x")),
            start: None,
        },
        InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: VariableDeclaration::scalar(ScalarType::Integer, n("k")),
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
        decl: VariableDeclaration::scalar(ScalarType::Integer, n("count")),
        start: None,
    }];
    block.startup.statements = vec![
        plain(
            state("y"),
            Expression::If(IfExpression::new(
                vec![(bin(BinaryOp::Gt, sref("x"), r(0.0)), r(1.0))],
                r(2.0),
            )),
        ),
        plain(
            state("count"),
            bin(BinaryOp::Add, sref("k"), Expression::Integer(1)),
        ),
    ];
    respan(&mut block);
    let package = AlgorithmCodePackage::construct(block, vec![None; 5], "period")
        .expect("the block validates");
    let solve = lower_algorithm_block(&package, binary64()).expect("refined");
    let checked =
        rumoca_ir_galec::package::CheckedAlgorithmBlock::construct(package.block().clone())
            .unwrap();
    let mut oracle = Evaluator::new(&checked, IntegerDomain::signed_32()).unwrap();
    oracle.set_state("x", Value::Real(f64::NAN)).unwrap();
    oracle
        .set_state("k", Value::Integer(i32::MAX.into()))
        .unwrap();
    oracle.set_state("period", Value::Real(0.01)).unwrap();
    let mut state = vec![None; solve.slots().len()];
    state[0] = Some(real64(f64::NAN));
    state[1] = Some(int32(i32::MAX.into()));
    state[3] = Some(real64(0.01));
    let before = state.clone();
    assert!(oracle.startup().is_err());
    assert!(
        rumoca_eval_solve::eval_algorithm_method(&solve, SolveBlockMethodKind::Startup, &mut state)
            .is_err()
    );
    assert!(oracle.active_signals().contains("NAN"));
    let nan = 5 + NAN_SIGNAL;
    assert_eq!(solve_bits(state[nan].as_ref()), Bits::Boolean(true));
    for slot in 0..5 {
        assert_eq!(state[slot], before[slot], "slot {slot} is restored");
    }
}

/// `name[index]` as a local element reference.
fn element_ref(name: &str, index: Expression) -> Reference {
    Reference::Local(RefPart {
        name: Name::ident(name),
        subscripts: vec![index],
        span: Span::DUMMY,
    })
}

/// `local_reader` with a local `a[2]` beside `z`.
fn array_reader(statements: Vec<Spanned<Statement>>) -> Block {
    let mut block = local_reader(statements);
    let mut array = real_decl(n("a"));
    array.dimensions = vec![Dimension::Expr(Expression::Integer(2))];
    block.do_step.locals.push(array);
    respan(&mut block);
    block
}

fn over_i(stop: i64, body: Vec<Spanned<Statement>>) -> Spanned<Statement> {
    Spanned::dummy(Statement::for_loop(rumoca_ir_galec::ast::ForLoop::new(
        Some(n("i")),
        Expression::Integer(1),
        None,
        Expression::Integer(stop),
        body,
    )))
}

#[test]
fn literal_element_writes_on_every_arm_define_the_array() {
    let a = |index: i64| element_ref("a", Expression::Integer(index));
    let a_read = |index: i64| Expression::Ref(a(index));
    let chain = |else_body| {
        Spanned::dummy(Statement::If(IfStatement {
            branches: vec![IfBranch {
                condition: Condition::Expression(sref("flag")),
                body: vec![plain(a(1), sref("v")), plain(a(2), sref("v"))],
                span: Span::DUMMY,
            }],
            else_body: Some(else_body),
        }))
    };
    refine(array_reader(vec![
        chain(vec![plain(a(2), r(1.0)), plain(a(1), r(0.0))]),
        plain(state("y"), bin(BinaryOp::Add, a_read(1), a_read(2))),
    ]))
    .expect("every arm writes both elements");

    let Err(error) = refine(array_reader(vec![
        chain(vec![plain(a(1), r(0.0))]),
        plain(state("y"), a_read(2)),
    ])) else {
        panic!("the else arm leaves a[2] unassigned");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");
}

#[test]
fn an_element_written_this_iteration_is_readable_this_iteration() {
    let a_i = || element_ref("a", lsref("i"));
    // The tensor-contraction accumulator: a[i] := 0; a[i] := a[i] + v.
    refine(array_reader(vec![
        over_i(
            2,
            vec![
                plain(a_i(), r(0.0)),
                plain(a_i(), bin(BinaryOp::Add, Expression::Ref(a_i()), sref("v"))),
            ],
        ),
        plain(
            state("y"),
            Expression::Ref(element_ref("a", Expression::Integer(1))),
        ),
    ]))
    .expect("the element is read where its write is proven");

    // Written under a guard, read outside it: refused.
    assert!(
        refine(array_reader(vec![over_i(
            2,
            vec![
                when_flag(vec![plain(a_i(), r(0.0))]),
                plain(state("y"), Expression::Ref(a_i())),
            ],
        )]))
        .is_err()
    );

    // A fact never outlives its iterator's binding: the first loop wrote
    // only a[1], and the second loop's `a[i]` is a[2].
    let over_two = |body| {
        Spanned::dummy(Statement::for_loop(rumoca_ir_galec::ast::ForLoop::new(
            Some(n("i")),
            Expression::Integer(2),
            None,
            Expression::Integer(2),
            body,
        )))
    };
    let Err(error) = refine(array_reader(vec![
        over_i(1, vec![plain(a_i(), sref("v"))]),
        over_two(vec![plain(state("y"), Expression::Ref(a_i()))]),
    ])) else {
        panic!("an element fact does not cross loops");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");

    // What a loop wrote is known element by element afterwards: a[1] here.
    refine(array_reader(vec![
        over_i(1, vec![plain(a_i(), sref("v"))]),
        over_i(1, vec![plain(state("y"), Expression::Ref(a_i()))]),
    ]))
    .expect("the second loop reads only the element the first one wrote");
}

/// `y := value` in DoStep over Real inputs `a`, `b`; the refined DoStep's
/// operations as serialized JSON.
fn power_operations(value: Expression) -> Result<Vec<serde_json::Value>, AlgorithmBlockLowerError> {
    let mut block = Block::new(n("Power"));
    let real_io = |kind, name: &str| InterfaceVariable {
        kind,
        decl: real_decl(n(name)),
        start: None,
    };
    block.interface = vec![
        real_io(InterfaceKind::Input, "a"),
        real_io(InterfaceKind::Input, "b"),
        real_io(InterfaceKind::Output, "y"),
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: real_decl(n("period")),
            start: Some(r(0.01)),
        },
    ];
    block.startup.statements = vec![plain(state("y"), r(0.0))];
    block.do_step.statements = vec![plain(state("y"), value)];
    respan(&mut block);
    let refined = refine(block)?;
    let program = serde_json::to_value(refined.do_step().program()).expect("serializable");
    Ok(program["operations"]
        .as_array()
        .expect("operations")
        .iter()
        .map(|issued| issued["operation"].clone())
        .collect())
}

fn count_kind(operations: &[serde_json::Value], kind: &str) -> usize {
    operations
        .iter()
        .filter(|operation| operation["operation"] == kind)
        .count()
}

/// The single square multiply: `destination := base * base`.
fn square_of(operations: &[serde_json::Value]) -> Option<&serde_json::Value> {
    operations.iter().find(|operation| {
        operation["operation"] == "binary"
            && operation["operator"] == "multiply"
            && operation["lhs"] == operation["rhs"]
    })
}

fn pow(base: Expression, exponent: Expression) -> Expression {
    bin(BinaryOp::Pow, base, exponent)
}

#[test]
fn a_real_square_is_one_multiply_of_one_base_for_every_spelling_of_two() {
    let real_two = Expression::Call(rumoca_ir_galec::ast::FunctionCall {
        function: n("real"),
        arguments: vec![Expression::Integer(2)],
    });
    for exponent in [r(2.0), real_two] {
        let operations = power_operations(pow(sref("a"), exponent.clone())).unwrap();
        assert!(
            square_of(&operations).is_some(),
            "{exponent:?}: {operations:#?}"
        );
        assert!(
            !operations
                .iter()
                .any(|operation| operation["operator"] == "power"),
            "{exponent:?} must not reach Power: {operations:#?}"
        );
    }
}

#[test]
fn a_squared_base_is_evaluated_once() {
    let real_two = || {
        Expression::Call(rumoca_ir_galec::ast::FunctionCall {
            function: n("real"),
            arguments: vec![Expression::Integer(2)],
        })
    };
    // Compound base: one Add, squared by multiplying its own result.
    let compound = power_operations(pow(
        Expression::Paren(Box::new(bin(BinaryOp::Add, sref("a"), sref("b")))),
        real_two(),
    ))
    .unwrap();
    let add = compound
        .iter()
        .filter(|operation| operation["operator"] == "add")
        .collect::<Vec<_>>();
    assert_eq!(add.len(), 1, "{compound:#?}");
    assert_eq!(
        square_of(&compound).map(|square| &square["lhs"]),
        Some(&add[0]["destination"]),
        "the square must multiply the one sum by itself: {compound:#?}"
    );

    // Call base: `max` selects once, and that selection is squared.
    let call = power_operations(pow(
        Expression::Call(rumoca_ir_galec::ast::FunctionCall {
            function: n("max"),
            arguments: vec![sref("a"), r(1.0)],
        }),
        real_two(),
    ))
    .unwrap();
    assert_eq!(count_kind(&call, "select"), 1, "{call:#?}");
    let select = call
        .iter()
        .find(|operation| operation["operation"] == "select")
        .unwrap();
    assert_eq!(
        square_of(&call).map(|square| &square["lhs"]),
        Some(&select["destination"]),
        "the square must multiply the one selection by itself: {call:#?}"
    );
}

#[test]
fn a_non_square_power_keeps_power_and_mixed_operands_are_refused() {
    let cube = power_operations(pow(
        sref("a"),
        Expression::Call(rumoca_ir_galec::ast::FunctionCall {
            function: n("real"),
            arguments: vec![Expression::Integer(3)],
        }),
    ))
    .unwrap();
    assert!(
        cube.iter()
            .any(|operation| operation["operator"] == "power"),
        "{cube:#?}"
    );
    let variable = power_operations(pow(sref("a"), sref("b"))).unwrap();
    assert!(
        variable
            .iter()
            .any(|operation| operation["operator"] == "power"),
        "{variable:#?}"
    );
    // GALEC has no Real ^ Integer (the evaluator rejects it; the projection
    // writes `real(n)`), not even for the square.
    for exponent in [2, 3] {
        let Err(error) = power_operations(pow(sref("a"), Expression::Integer(exponent))) else {
            panic!("a Real base with Integer exponent {exponent} is refused");
        };
        assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
        assert!(
            error.to_string().contains("mixed Real and Integer"),
            "{error}"
        );
    }
}

#[test]
fn a_state_written_only_conditionally_by_startup_is_refused() {
    // A write under `if` leaves the other arm's value indeterminate, so it
    // does not count as Startup defining the output.
    let mut block = local_reader(vec![plain(state("y"), r(1.0))]);
    block.startup.statements = vec![when_flag(vec![plain(state("y"), r(0.0))])];
    respan(&mut block);
    let Err(error) = refine(block) else {
        panic!("a conditional-only Startup write is refused");
    };
    assert_eq!(error.code(), EL007_UNSUPPORTED_ALGORITHM_CODE);
    assert!(
        error
            .to_string()
            .contains("not definitely assigned by Startup"),
        "{error}"
    );
}

/// `local_reader` with a local `m[2, 2]` beside `z`.
fn matrix_reader(statements: Vec<Spanned<Statement>>) -> Block {
    let mut block = local_reader(statements);
    let mut matrix = real_decl(n("m"));
    matrix.dimensions = vec![
        Dimension::Expr(Expression::Integer(2)),
        Dimension::Expr(Expression::Integer(2)),
    ];
    block.do_step.locals.push(matrix);
    respan(&mut block);
    block
}

fn over(name: &str, start: i64, stop: i64, body: Vec<Spanned<Statement>>) -> Spanned<Statement> {
    Spanned::dummy(Statement::for_loop(rumoca_ir_galec::ast::ForLoop::new(
        Some(n(name)),
        Expression::Integer(start),
        None,
        Expression::Integer(stop),
        body,
    )))
}

fn m_at(row: Expression, column: Expression) -> Reference {
    Reference::Local(RefPart {
        name: Name::ident("m"),
        subscripts: vec![row, column],
        span: Span::DUMMY,
    })
}

#[test]
fn a_shared_iterator_writes_only_its_diagonal() {
    // Regression: `m[i, i]` over i = 1:2 writes the
    // diagonal, not the Cartesian product of the two axes.
    let diagonal = || {
        over(
            "i",
            1,
            2,
            vec![plain(m_at(lsref("i"), lsref("i")), sref("v"))],
        )
    };
    refine(matrix_reader(vec![
        diagonal(),
        plain(
            state("y"),
            Expression::Ref(m_at(Expression::Integer(2), Expression::Integer(2))),
        ),
    ]))
    .expect("m[2, 2] is on the written diagonal");
    let Err(error) = refine(matrix_reader(vec![
        diagonal(),
        plain(
            state("y"),
            Expression::Ref(m_at(Expression::Integer(1), Expression::Integer(2))),
        ),
    ])) else {
        panic!("m[1, 2] is off the diagonal and never written");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");
}

#[test]
fn a_zero_trip_nested_loop_writes_nothing() {
    // Regression: the inner loop over j = 2:1 never runs.
    let Err(error) = refine(matrix_reader(vec![
        over(
            "i",
            1,
            1,
            vec![over(
                "j",
                2,
                1,
                vec![plain(
                    m_at(Expression::Integer(1), Expression::Integer(1)),
                    sref("v"),
                )],
            )],
        ),
        plain(
            state("y"),
            Expression::Ref(m_at(Expression::Integer(1), Expression::Integer(1))),
        ),
    ])) else {
        panic!("a write inside a zero-trip loop is not a write");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");
}

#[test]
fn a_shadowed_iterator_never_covers_a_whole_array() {
    // Regression: in `for i { for i { m[i, i] } }` both
    // subscripts name the inner i, so only the diagonal is written.
    let shadowed = || {
        over(
            "i",
            1,
            2,
            vec![over(
                "i",
                1,
                2,
                vec![plain(m_at(lsref("i"), lsref("i")), sref("v"))],
            )],
        )
    };
    let off_diagonal = || {
        plain(
            state("y"),
            Expression::Ref(m_at(Expression::Integer(1), Expression::Integer(2))),
        )
    };
    let Err(error) = refine(matrix_reader(vec![shadowed(), off_diagonal()])) else {
        panic!("a shadowed diagonal does not write m[1, 2]");
    };
    assert!(error.to_string().contains("unassigned local"), "{error}");

    // Positive control: independent i and j write every element.
    let full = over(
        "i",
        1,
        2,
        vec![over(
            "j",
            1,
            2,
            vec![plain(m_at(lsref("i"), lsref("j")), sref("v"))],
        )],
    );
    refine(matrix_reader(vec![full, off_diagonal()]))
        .expect("independent loops over both axes define the matrix");
}

/// EL007: inside one region of a function a raise precedes a
/// fault. The region holds the raised bit until it completes, so the fault
/// would lose it: refused, although a function's top level publishes as it
/// goes.
#[test]
fn a_raise_a_function_region_still_holds_at_a_fault_is_refused() {
    use rumoca_ir_galec::ast::Identifier;
    let mut function = user_function(
        "held",
        &["a"],
        &["b"],
        vec![
            plain(local("b"), lsref("a")),
            Spanned::dummy(Statement::If(IfStatement {
                branches: vec![IfBranch {
                    condition: Condition::Expression(bin(BinaryOp::Gt, lsref("a"), r(0.0))),
                    body: vec![
                        Spanned::dummy(Statement::Signal(vec![Identifier::new("NAN")])),
                        plain(
                            local("k"),
                            builtin_call(
                                "divisionTowardsZero",
                                vec![Expression::Integer(1), Expression::Integer(0)],
                            ),
                        ),
                    ],
                    span: Span::DUMMY,
                }],
                else_body: None,
            })),
        ],
    );
    function.signals = vec![Identifier::new("NAN")];
    function.locals = vec![decl(ScalarType::Integer, "k", 0)];
    let mut block = local_reader(vec![plain(
        state("y"),
        builtin_call("held", vec![sref("v")]),
    )]);
    block.do_step.locals.clear();
    block.do_step.signals = vec![PredefinedSignal::Nan];
    block.protected_functions = vec![function];
    respan(&mut block);
    let Err(error) = refine(block) else {
        panic!("a raise held by a function region at a fault is refused");
    };
    assert!(error.to_string().contains("fault after"), "{error}");
}

#[test]
fn a_refused_reference_is_located_at_its_first_part_or_reported_unlocated() {
    let located = unsupported_reference("subscripted reference", &local("z"));
    assert!(matches!(
        located,
        AlgorithmBlockLowerError::Unsupported {
            construct: "subscripted reference",
            ..
        }
    ));
    let unlocated = unsupported_reference(
        "compartment member reference",
        &Reference::State(Vec::new()),
    );
    assert!(matches!(
        unlocated,
        AlgorithmBlockLowerError::MissingProvenance { .. }
    ));
    assert_eq!(
        unlocated.code(),
        crate::diagnostic_codes::EL005_INVALID_SOLVE_CONTRACT
    );
    assert!(
        unlocated.to_string().contains("carries no source span"),
        "{unlocated}"
    );
}
