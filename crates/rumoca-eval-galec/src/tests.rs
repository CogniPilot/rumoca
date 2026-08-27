use rumoca_ir_galec::ast::{
    Block, Expression, InterfaceKind, InterfaceVariable, Name, Reference, ScalarType, Spanned,
    Statement, VariableDeclaration,
};
use rumoca_ir_galec::package::CheckedAlgorithmBlock;

use crate::{EvaluationError, Evaluator, IntegerDomain, Value};

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

fn lifecycle_block(input_start: Option<Expression>) -> CheckedAlgorithmBlock {
    let mut block = Block::new(Name::ident("Lifecycle"));
    block.interface = vec![
        interface(InterfaceKind::Input, ScalarType::Real, "u", input_start),
        interface(InterfaceKind::Output, ScalarType::Real, "y", None),
    ];
    block.startup.statements = vec![assignment("y", Expression::Real(2.0))];
    CheckedAlgorithmBlock::construct(block).expect("lifecycle fixture must be checked")
}

#[test]
fn created_block_rejects_state_reads_and_runtime_methods() {
    let block = lifecycle_block(Some(Expression::Real(1.0)));
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");

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
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");

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
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");

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
        interface(InterfaceKind::Output, ScalarType::Real, "y", None),
    ];
    let block =
        CheckedAlgorithmBlock::construct(block).expect("fixture remains structurally valid");
    let mut evaluator =
        Evaluator::new(&block, IntegerDomain::signed_32()).expect("create evaluator");

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
