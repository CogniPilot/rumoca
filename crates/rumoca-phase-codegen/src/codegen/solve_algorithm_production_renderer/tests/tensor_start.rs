use super::*;

fn rank_two_uniform_start_product<'inv>(
    brand: TargetInvocationBrand<'inv>,
) -> solve::SolveAlgorithmProduct<'inv> {
    let source = SourceId::from_source_name("production-c-tensor-start.mo");
    let mut output = galec::ast::VariableDeclaration::scalar(
        galec::ast::ScalarType::Real,
        galec::ast::Name::ident("matrix"),
    );
    output.dimensions = vec![
        galec::ast::Dimension::Expr(galec::ast::Expression::Integer(2)),
        galec::ast::Dimension::Expr(galec::ast::Expression::Integer(3)),
    ];
    output.span = Span::from_offsets(source, 1, 12);
    let start = galec::ast::Expression::Array(vec![
        galec::ast::Expression::Array(vec![
            galec::ast::Expression::Real(-0.0),
            galec::ast::Expression::Real(-0.0),
            galec::ast::Expression::Real(-0.0),
        ]),
        galec::ast::Expression::Array(vec![
            galec::ast::Expression::Real(-0.0),
            galec::ast::Expression::Real(-0.0),
            galec::ast::Expression::Real(-0.0),
        ]),
    ]);
    let mut block = galec::Block::new(galec::ast::Name::ident("TensorProduction"));
    block.interface.push(galec::ast::InterfaceVariable {
        kind: galec::ast::InterfaceKind::Output,
        decl: output,
        start: Some(start.clone()),
    });
    let mut period = galec::ast::VariableDeclaration::scalar(
        galec::ast::ScalarType::Real,
        galec::ast::Name::ident("period"),
    );
    period.span = Span::from_offsets(source, 1, 12);
    block.protected.push(galec::ast::ProtectedEntity {
        kind: galec::ast::ProtectedKind::Constant,
        decl: period,
        start: Some(galec::ast::Expression::Real(0.01)),
    });
    block.startup.statements.extend([
        galec::ast::Spanned {
            node: galec::ast::Statement::Assignment {
                target: galec::ast::Reference::state(galec::ast::Name::ident("matrix")),
                value: start,
            },
            span: Span::from_offsets(source, 13, 24),
        },
        galec::ast::Spanned {
            node: galec::ast::Statement::Assignment {
                target: galec::ast::Reference::state(galec::ast::Name::ident("period")),
                value: galec::ast::Expression::Real(0.01),
            },
            span: Span::from_offsets(source, 25, 32),
        },
    ]);
    let mut sources = rumoca_core::SourceMap::new();
    assert_eq!(
        sources.add("production-c-tensor-start.mo", &" ".repeat(64)),
        source,
    );
    let package = galec::TracedAlgorithmCodeProduct::project_from_origin(
        brand,
        &sources,
        "TensorProduction",
        |issuer| {
            issuer.construct(
                block,
                galec::package::AlgorithmCodePackageMetadata::new(
                    vec![None; 2],
                    "period",
                    Vec::new(),
                    galec::package::AlgorithmCodeArithmeticProfile::construct(
                        galec::package::AlgorithmCodeRealFormat::Binary32,
                        galec::package::AlgorithmCodeIntegerFormat::I32,
                        MATRIX_ARITHMETIC,
                    ),
                ),
            )
        },
    )
    .expect("tensor fixture retains its exact trace origin");
    rumoca_phase_solve::lower_solve_algorithm_product(package)
        .expect("rank-two uniform start has one compact Solve action")
}

#[test]
fn rank_two_uniform_negative_zero_start_renders_compiles_and_executes() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive");
        let prepared = prepare_solve_algorithm_production(
            rank_two_uniform_start_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect("rank-two uniform fill is Production C-ready");
        assert_eq!(prepared.declarations()[0].dimensions().len(), 2);
        assert!(matches!(
            prepared.declarations()[0].initialization(),
            crate::PreparedSolveAlgorithmInitialization::Internal(
                crate::PreparedSolveAlgorithmInitializationValue::UniformTensorFill(_)
            )
        ));
        let storage_type = prepared.presentation().storage_type().as_str().to_owned();
        let startup = prepared
            .presentation()
            .method(solve::SolveAlgorithmMethodKind::Startup)
            .function()
            .as_str()
            .to_owned();
        let status_ok = prepared
            .presentation()
            .success_status()
            .identifier()
            .as_str()
            .to_owned();
        let component = prepared.presentation().declarations()[0]
            .component()
            .as_str()
            .to_owned();
        let identities = BTreeMap::new();
        let checksums = BTreeMap::new();
        let artifact = crate::codegen::codegen_test_support::artifact_bindings(
            brand,
            "2026-08-30T00:00:00Z",
            "rumoca-tensor-test",
            "model",
            &identities,
            &checksums,
        );
        let (header_path, header_content) = render_production_test_file(
            brand,
            &prepared,
            &artifact,
            ProductionCodeFileRole::Header,
            TemplateArtifactKind::CHeader,
            include_str!("../../../templates/efmu/production.h.jinja"),
        );
        assert!(header_content.contains(&format!("{component}[2][3];")));

        let (source_path, source_content) = render_production_test_file(
            brand,
            &prepared,
            &artifact,
            ProductionCodeFileRole::Source,
            TemplateArtifactKind::CSource,
            include_str!("../../../templates/efmu/production.c.jinja"),
        );
        assert_eq!(source_content.matches("for (").count(), 2);
        assert_eq!(source_content.matches(" = -0.0F;").count(), 1);

        let driver = format!(
            "#include <math.h>\n#include \"ProductionCode/sources/production.h\"\nint main(void) {{\n    {storage_type} value;\n    if ({startup}(&value) != {status_ok}) return 2;\n    for (unsigned i = 0; i < 2; ++i) {{\n        for (unsigned j = 0; j < 3; ++j) {{\n            if (!signbit(value.{component}[i][j])) return 1;\n        }}\n    }}\n    return 0;\n}}\n"
        );
        compile_and_run_rendered_c(
            &source_path,
            &header_path,
            &source_content,
            &header_content,
            &driver,
            "production-c-tensor-probe",
        );
    });
}
