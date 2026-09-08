//! The Algorithm Code emitter is a passive spelling of one checked GALEC
//! root. Rendering and reparsing preserve bytes; evaluation consumes the
//! originating package's retained numeric profile.

use rumoca_eval_galec::{Evaluator, Value};
use rumoca_ir_galec::ast::{
    BinaryOp, Block, Expression, InterfaceKind, InterfaceVariable, Name, Reference, ScalarType,
    Spanned, Statement, VariableDeclaration,
};
use rumoca_ir_galec::package::{
    AlgorithmCodeArithmeticProfile, AlgorithmCodeIntegerFormat, AlgorithmCodePackage,
    AlgorithmCodePackageMetadata, AlgorithmCodeRealFormat,
};

fn interface(kind: InterfaceKind, name: &str, start: Expression) -> InterfaceVariable {
    InterfaceVariable {
        kind,
        decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident(name)),
        start: Some(start),
    }
}

fn state(name: &str) -> Reference {
    Reference::state(Name::ident(name))
}

fn assignment(name: &str, value: Expression) -> Spanned<Statement> {
    Spanned::dummy(Statement::Assignment {
        target: state(name),
        value,
    })
}

fn checked_fixture() -> AlgorithmCodePackage {
    let mut block = Block::new(Name::ident("PassiveEmitter"));
    block.interface = vec![
        interface(InterfaceKind::Input, "u", Expression::Real(4.0)),
        interface(InterfaceKind::Output, "y", Expression::Real(0.0)),
    ];
    block.startup.statements = vec![assignment("y", Expression::Real(0.0))];
    block.do_step.statements = vec![assignment(
        "y",
        Expression::binary(
            BinaryOp::Mul,
            Expression::binary(
                BinaryOp::Add,
                Expression::Ref(state("u")),
                Expression::Real(3.0),
            ),
            Expression::Real(2.0),
        ),
    )];
    block.protected.push(rumoca_ir_galec::ast::ProtectedEntity {
        kind: rumoca_ir_galec::ast::ProtectedKind::Constant,
        decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident("period")),
        start: Some(Expression::Real(0.01)),
    });
    block
        .startup
        .statements
        .insert(0, assignment("period", Expression::Real(0.01)));
    AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None; 3],
            "period",
            Vec::new(),
            AlgorithmCodeArithmeticProfile::construct(
                AlgorithmCodeRealFormat::Binary64,
                AlgorithmCodeIntegerFormat::I32,
                rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        ),
    )
    .expect("fixture closes as an Algorithm Code package")
}

fn evaluate(package: &AlgorithmCodePackage) -> Value {
    let mut evaluator = Evaluator::new(package).expect("checked package creates an evaluator");
    evaluator
        .set_state("u", Value::Real(4.0))
        .expect("initialize the external input identically for both roots");
    evaluator.startup().expect("Startup succeeds");
    evaluator.do_step().expect("DoStep succeeds");
    evaluator.state("y").expect("output exists").clone()
}

#[test]
fn checked_algorithm_code_render_is_byte_stable_and_package_evaluable() {
    let original = checked_fixture();
    let first =
        rumoca_phase_codegen::render_checked_algorithm_block_source(original.checked_block())
            .expect("checked source renders");
    let reparsed = rumoca_phase_parse_galec::parse(&first, "PassiveEmitter.alg")
        .expect("rendered source reparses as checked Algorithm Code");
    let second = rumoca_phase_codegen::render_checked_algorithm_block_source(&reparsed)
        .expect("reparsed checked source renders");

    assert_eq!(first, second, "checked GALEC spelling must be canonical");
    assert_eq!(evaluate(&original), Value::Real(14.0));
}
