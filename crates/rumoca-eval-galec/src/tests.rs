use rumoca_ir_galec::ast::{
    BinaryOp, Block, Expression, InterfaceKind, InterfaceVariable, Name, Reference, ScalarType,
    Spanned, Statement, VariableDeclaration,
};
use rumoca_ir_galec::package::{
    AlgorithmCodeIntegerFormat, AlgorithmCodePackage, AlgorithmCodeRealFormat,
};

use crate::test_support::{binary64_i32, package};
use crate::{EvaluationError, Evaluator, Value};

fn declaration(kind: ScalarType, name: &str) -> VariableDeclaration {
    VariableDeclaration::scalar(kind, Name::ident(name))
}

fn interface(
    kind: InterfaceKind,
    scalar: ScalarType,
    name: &str,
    start: Option<Expression>,
) -> InterfaceVariable {
    InterfaceVariable {
        kind,
        decl: declaration(scalar, name),
        start,
    }
}

fn assignment(name: &str, value: Expression) -> Spanned<Statement> {
    Spanned::dummy(Statement::Assignment {
        target: Reference::state(Name::ident(name)),
        value,
    })
}

fn lifecycle_block(input_start: Option<Expression>) -> AlgorithmCodePackage {
    let mut block = Block::new(Name::ident("Lifecycle"));
    block.interface = vec![
        interface(InterfaceKind::Input, ScalarType::Real, "u", input_start),
        interface(
            InterfaceKind::Output,
            ScalarType::Real,
            "y",
            Some(Expression::Real(0.0)),
        ),
    ];
    block.startup.statements = vec![assignment("y", Expression::Real(2.0))];
    binary64_i32(block)
}

#[test]
fn created_block_rejects_state_reads_and_runtime_methods() {
    let block = lifecycle_block(Some(Expression::Real(1.0)));
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    assert!(matches!(
        evaluator.state("u"),
        Err(EvaluationError::Lifecycle {
            operation: "read state",
            state: "created",
        })
    ));
    assert!(matches!(
        evaluator.recalibrate(),
        Err(EvaluationError::Lifecycle {
            operation: "invoke Recalibrate",
            state: "created",
        })
    ));
    assert!(matches!(
        evaluator.do_step(),
        Err(EvaluationError::Lifecycle {
            operation: "invoke DoStep",
            state: "created",
        })
    ));
}

#[test]
fn startup_requires_external_initialization_and_cannot_repeat() {
    let block = lifecycle_block(None);
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    assert_eq!(
        evaluator.startup(),
        Err(EvaluationError::Uninitialized("u".to_owned()))
    );
    evaluator
        .set_state("u", Value::Real(3.0))
        .expect("initialize control input");
    evaluator.startup().expect("first Startup succeeds");

    assert_eq!(evaluator.state("u"), Ok(&Value::Real(3.0)));
    assert_eq!(evaluator.state("y"), Ok(&Value::Real(2.0)));
    assert!(matches!(
        evaluator.startup(),
        Err(EvaluationError::Lifecycle {
            operation: "invoke Startup",
            state: "started",
        })
    ));
    evaluator.recalibrate().expect("Recalibrate after Startup");
    evaluator.do_step().expect("DoStep after Startup");
}

#[test]
fn declared_external_start_is_not_exposed_before_startup() {
    let block = lifecycle_block(Some(Expression::Real(4.0)));
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    assert!(matches!(
        evaluator.state("u"),
        Err(EvaluationError::Lifecycle { .. })
    ));
    evaluator
        .startup()
        .expect("Startup uses declared input start");
    assert_eq!(evaluator.state("u"), Ok(&Value::Real(4.0)));
}

#[test]
fn startup_cannot_publish_partially_initialized_block_state() {
    let mut block = Block::new(Name::ident("IncompleteStartup"));
    block.interface = vec![
        interface(
            InterfaceKind::Input,
            ScalarType::Real,
            "u",
            Some(Expression::Real(1.0)),
        ),
        interface(
            InterfaceKind::Output,
            ScalarType::Real,
            "y",
            Some(Expression::Real(0.0)),
        ),
    ];
    let block = binary64_i32(block);
    let mut evaluator = Evaluator::new(&block).expect("create evaluator");

    assert_eq!(
        evaluator.startup(),
        Err(EvaluationError::Uninitialized("y".to_owned()))
    );
    assert!(matches!(
        evaluator.state("u"),
        Err(EvaluationError::Lifecycle {
            state: "created",
            ..
        })
    ));
}

fn arithmetic_package(real: AlgorithmCodeRealFormat) -> AlgorithmCodePackage {
    let mut block = Block::new(Name::ident("ArithmeticProfile"));
    block.interface = vec![interface(
        InterfaceKind::Output,
        ScalarType::Real,
        "y",
        Some(Expression::Real(0.0)),
    )];
    let boundary = Expression::binary(
        BinaryOp::Sub,
        Expression::binary(
            BinaryOp::Add,
            Expression::Real(16_777_216.0),
            Expression::Real(1.0),
        ),
        Expression::Real(16_777_216.0),
    );
    block.startup.statements = vec![assignment("y", boundary)];
    package(block, real, AlgorithmCodeIntegerFormat::I32)
}

#[test]
fn package_real_profile_rounds_every_primitive_operation() {
    let binary32 = arithmetic_package(AlgorithmCodeRealFormat::Binary32);
    let binary64 = arithmetic_package(AlgorithmCodeRealFormat::Binary64);
    let mut binary32_eval = Evaluator::new(&binary32).expect("create Binary32 evaluator");
    let mut binary64_eval = Evaluator::new(&binary64).expect("create Binary64 evaluator");

    binary32_eval.startup().expect("Binary32 Startup");
    binary64_eval.startup().expect("Binary64 Startup");

    assert_eq!(binary32_eval.state("y"), Ok(&Value::Real(0.0)));
    assert_eq!(binary64_eval.state("y"), Ok(&Value::Real(1.0)));
}

#[test]
fn checked_integer_power_uses_package_real_semantics() {
    let mut block = Block::new(Name::ident("IntegerPower"));
    block.interface = vec![interface(
        InterfaceKind::Output,
        ScalarType::Real,
        "y",
        Some(Expression::Real(0.0)),
    )];
    block.startup.statements = vec![assignment(
        "y",
        Expression::binary(
            BinaryOp::Pow,
            Expression::Integer(2),
            Expression::Integer(3),
        ),
    )];
    let package = package(
        block,
        AlgorithmCodeRealFormat::Binary32,
        AlgorithmCodeIntegerFormat::I32,
    );
    let mut evaluator = Evaluator::new(&package).expect("create evaluator");

    evaluator.startup().expect("Startup succeeds");

    assert_eq!(evaluator.state("y"), Ok(&Value::Real(8.0)));
}

#[test]
fn binary32_external_real_is_rounded_before_storage() {
    let mut block = Block::new(Name::ident("Binary32ExternalInput"));
    block.interface = vec![
        interface(InterfaceKind::Input, ScalarType::Real, "u", None),
        interface(
            InterfaceKind::Output,
            ScalarType::Real,
            "y",
            Some(Expression::Real(0.0)),
        ),
    ];
    block.startup.statements = vec![assignment("y", Expression::Real(0.0))];
    let block = package(
        block,
        AlgorithmCodeRealFormat::Binary32,
        AlgorithmCodeIntegerFormat::I32,
    );
    let mut evaluator = Evaluator::new(&block).expect("create Binary32 evaluator");

    evaluator
        .set_state("u", Value::Real(16_777_217.0))
        .expect("set Binary32 input");
    evaluator.startup().expect("Startup succeeds");

    assert_eq!(evaluator.state("u"), Ok(&Value::Real(16_777_216.0)));
}
