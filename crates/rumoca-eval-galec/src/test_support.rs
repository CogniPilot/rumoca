use rumoca_ir_galec::ast;
use rumoca_ir_galec::package::{
    AlgorithmCodeArithmeticProfile, AlgorithmCodeIntegerFormat, AlgorithmCodePackage,
    AlgorithmCodePackageMetadata, AlgorithmCodeRealFormat,
};

pub(crate) fn package(
    mut block: ast::Block,
    real: AlgorithmCodeRealFormat,
    integer: AlgorithmCodeIntegerFormat,
) -> AlgorithmCodePackage {
    let period_name = "evaluatorPeriod";
    block.protected.push(ast::ProtectedEntity {
        kind: ast::ProtectedKind::Constant,
        decl: ast::VariableDeclaration::scalar(
            ast::ScalarType::Real,
            ast::Name::ident(period_name),
        ),
        start: Some(ast::Expression::Real(0.01)),
    });
    block.startup.statements.insert(
        0,
        ast::Spanned::dummy(ast::Statement::Assignment {
            target: ast::Reference::state(ast::Name::ident(period_name)),
            value: ast::Expression::Real(0.01),
        }),
    );
    let variable_count = block.interface.len() + block.protected.len();
    AlgorithmCodePackage::construct(
        block,
        AlgorithmCodePackageMetadata::new(
            vec![None; variable_count],
            period_name,
            Vec::new(),
            AlgorithmCodeArithmeticProfile::construct(
                real,
                integer,
                rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        ),
    )
    .expect("evaluator fixture must close as an Algorithm Code package")
}

pub(crate) fn binary64_i32(block: ast::Block) -> AlgorithmCodePackage {
    package(
        block,
        AlgorithmCodeRealFormat::Binary64,
        AlgorithmCodeIntegerFormat::I32,
    )
}
