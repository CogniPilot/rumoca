use std::sync::Arc;

use rumoca_core::{
    RealMatrixMultiplySemantics, TargetInvocationBrand, with_target_invocation_brand,
};
use rumoca_ir_galec as galec;
use rumoca_ir_solve as solve;

use super::*;

const MATRIX_ARITHMETIC: RealMatrixMultiplySemantics =
    RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;

fn package_parts(
    real_format: galec::package::AlgorithmCodeRealFormat,
) -> (galec::Block, galec::package::AlgorithmCodePackageMetadata) {
    let mut block = galec::Block::new(galec::ast::Name::ident("CheckedController"));
    block.protected.push(galec::ast::ProtectedEntity {
        kind: galec::ast::ProtectedKind::Constant,
        decl: galec::ast::VariableDeclaration::scalar(
            galec::ast::ScalarType::Real,
            galec::ast::Name::ident("period"),
        ),
        start: Some(galec::ast::Expression::Real(0.01)),
    });
    block.startup.statements.push(galec::ast::Spanned {
        node: galec::ast::Statement::Assignment {
            target: galec::ast::Reference::state(galec::ast::Name::ident("period")),
            value: galec::ast::Expression::Real(0.01),
        },
        span: rumoca_core::Span::DUMMY,
    });
    (
        block,
        galec::package::AlgorithmCodePackageMetadata::new(
            vec![None],
            "period",
            Vec::new(),
            galec::package::AlgorithmCodeArithmeticProfile::construct(
                real_format,
                galec::package::AlgorithmCodeIntegerFormat::I32,
                MATRIX_ARITHMETIC,
            ),
        ),
    )
}

fn traced_package<'inv>(
    brand: TargetInvocationBrand<'inv>,
    real_format: galec::package::AlgorithmCodeRealFormat,
) -> galec::TracedAlgorithmCodeProduct<'inv> {
    let mut sources = rumoca_core::SourceMap::new();
    assert!(sources.register_id(
        rumoca_core::Span::DUMMY.source,
        &rumoca_core::placeholder_source_name(rumoca_core::Span::DUMMY.source),
        Arc::<str>::from(" "),
    ));
    galec::TracedAlgorithmCodeProduct::project_from_origin(
        brand,
        &sources,
        "CheckedController",
        |issuer| {
            let (block, metadata) = package_parts(real_format);
            issuer.construct(block, metadata)
        },
    )
    .expect("fixture package retains its exact trace origin")
}

fn solve_lifecycle_product<'inv>(
    brand: TargetInvocationBrand<'inv>,
    real_format: galec::package::AlgorithmCodeRealFormat,
) -> solve::SolveAlgorithmProduct<'inv> {
    solve::SolveAlgorithmBlock::construct(traced_package(brand, real_format), |inspection, root| {
        for subject in inspection.subjects() {
            match subject {
                galec::package::AlgorithmCodeSubject::Declaration(declaration) => {
                    root.issue_declaration(inspection, declaration)?;
                }
                galec::package::AlgorithmCodeSubject::LifecycleMethod(method) => {
                    root.issue_lifecycle_method(inspection, method)?;
                }
                galec::package::AlgorithmCodeSubject::Expression(expression) => {
                    root.issue_expression(inspection, expression)?;
                }
                galec::package::AlgorithmCodeSubject::Statement(statement) => {
                    root.issue_statement(inspection, statement)?;
                }
                galec::package::AlgorithmCodeSubject::Reference(reference) => {
                    root.issue_reference(inspection, reference)?;
                }
                galec::package::AlgorithmCodeSubject::UserFunction(_)
                | galec::package::AlgorithmCodeSubject::LoopBinder(_)
                | galec::package::AlgorithmCodeSubject::Call(_)
                | galec::package::AlgorithmCodeSubject::CallResultProjection(_) => {
                    unreachable!("empty fixture has only lifecycle subjects")
                }
            }
        }
        let transfers = solve::CallTransferPlanSet::construct(Vec::new(), |_| Ok(()))?;
        root.issue_call_transfers(transfers)
    })
    .expect("minimal package refines into one checked Solve lifecycle root")
}

fn production_profile() -> SolveAlgorithmProductionProfile {
    SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
        .expect("the fixture stack budget is positive")
}

fn production_artifact_layout() -> ProductionArtifactLayout {
    ProductionArtifactLayout::construct([
        (ProductionArtifactRole::PackageManifest, "__content.xml"),
        (
            ProductionArtifactRole::AlgorithmCodeManifest,
            "AlgorithmCode/manifest.xml",
        ),
        (
            ProductionArtifactRole::AlgorithmCodeSource,
            "AlgorithmCode/model.alg",
        ),
        (
            ProductionArtifactRole::ProductionManifest,
            "ProductionCode/manifest.xml",
        ),
        (
            ProductionArtifactRole::ProductionHeader,
            "ProductionCode/sources/production.h",
        ),
        (
            ProductionArtifactRole::ProductionSource,
            "ProductionCode/sources/production.c",
        ),
        (
            ProductionArtifactRole::Schema,
            "schemas/ProductionCode/schema.xsd",
        ),
    ])
    .expect("the test target has one complete static artifact layout")
}

#[test]
fn only_the_sealed_solve_root_enters_production_preparation() {
    with_target_invocation_brand(|brand| {
        let prepare: for<'inv> fn(
            solve::SolveAlgorithmProduct<'inv>,
            SolveAlgorithmProductionProfile,
            ProductionArtifactLayout,
        ) -> Result<
            PreparedSolveAlgorithmProduction<'inv>,
            SolveAlgorithmProductionPreparationError,
        > = prepare_solve_algorithm_production;
        let product =
            solve_lifecycle_product(brand, galec::package::AlgorithmCodeRealFormat::Binary32);

        let prepared = prepare(product, production_profile(), production_artifact_layout())
            .expect("the sealed scalar lifecycle subset is production-ready");
        assert_eq!(prepared.block().declarations().len(), 1);
        assert_eq!(prepared.algorithm_code().block().protected.len(), 1);
    });
}

#[test]
fn preparation_carries_all_three_complete_lifecycle_methods() {
    with_target_invocation_brand(|brand| {
        let product =
            solve_lifecycle_product(brand, galec::package::AlgorithmCodeRealFormat::Binary32);
        let prepared = prepare_solve_algorithm_production(
            product,
            production_profile(),
            production_artifact_layout(),
        )
        .expect("the checked subset prepares without fallback");

        for kind in [
            solve::SolveAlgorithmMethodKind::Startup,
            solve::SolveAlgorithmMethodKind::Recalibrate,
            solve::SolveAlgorithmMethodKind::DoStep,
        ] {
            let method = prepared.method(kind);
            assert_eq!(method.kind(), kind);
            assert_eq!(
                method.status(),
                PreparedSolveAlgorithmMethodStatus::InfallibleReturnedStatusI32
            );
            assert!(method.registers().is_empty());
            if kind == solve::SolveAlgorithmMethodKind::Startup {
                assert_eq!(method.operations().len(), 1);
                assert!(matches!(
                    method.operations()[0],
                    PreparedSolveAlgorithmOperation::InitializeScalar { .. }
                ));
            } else {
                assert!(method.operations().is_empty());
            }
        }
    });
}

#[test]
fn production_real_abi_is_derived_from_the_package_profile() {
    for (format, c_type, suffix, bytes, efmi_type, precision) in [
        (
            galec::package::AlgorithmCodeRealFormat::Binary32,
            "float",
            "F",
            4,
            "efmiFloat32",
            "32-bit",
        ),
        (
            galec::package::AlgorithmCodeRealFormat::Binary64,
            "double",
            "",
            8,
            "efmiFloat64",
            "64-bit",
        ),
    ] {
        with_target_invocation_brand(|brand| {
            let prepared = prepare_solve_algorithm_production(
                solve_lifecycle_product(brand, format),
                production_profile(),
                production_artifact_layout(),
            )
            .expect("both package-selected Real formats have a closed lexical ABI");
            let abi = prepared.real_abi();
            assert_eq!(abi.c_scalar_type, c_type);
            assert_eq!(abi.literal_suffix, suffix);
            assert_eq!(abi.byte_width, bytes);
            assert_eq!(abi.efmi_datatype, efmi_type);
            assert_eq!(abi.float_precision, precision);
        });
    }
}

#[test]
fn production_code_container_profile_is_closed_and_explicit() {
    let profile = production_profile().code_container();
    assert_eq!(profile.language(), ProductionCodeLanguage::C);
    assert_eq!(profile.standard(), ProductionCodeLanguageStandard::C99);
    assert_eq!(profile.platform(), ProductionCodePlatform::Legacy);
}

#[test]
fn real_literals_retain_exact_round_trip_c_tokens() {
    let binary32 =
        ProductionRealAbi::from_package_format(galec::package::AlgorithmCodeRealFormat::Binary32);
    for (value, expected) in [(0.1_f32, "0.1"), (-0.0_f32, "-0.0"), (1.0_f32, "1.0")] {
        let literal = PreparedSolveAlgorithmRealLiteral::from_f32_bits(value.to_bits(), binary32)
            .expect("every finite binary32 value has an exact round-trip decimal token");
        assert_eq!(literal.as_str(), format!("{expected}F"));
        assert_eq!(
            literal
                .as_str()
                .trim_end_matches('F')
                .parse::<f32>()
                .expect("the construction-issued decimal is parseable")
                .to_bits(),
            value.to_bits()
        );
    }
    assert!(
        PreparedSolveAlgorithmRealLiteral::from_f32_bits(f32::NAN.to_bits(), binary32).is_none()
    );
    assert!(
        PreparedSolveAlgorithmRealLiteral::from_f32_bits(f32::INFINITY.to_bits(), binary32)
            .is_none()
    );

    let serialized = serde_json::to_value(PreparedSolveAlgorithmLiteral::Real {
        token: PreparedSolveAlgorithmRealLiteral::from_f32_bits(0.1_f32.to_bits(), binary32)
            .expect("0.1f32 is finite"),
    })
    .expect("the prepared literal is serialization-only input");
    assert_eq!(serialized["kind"], "real");
    assert_eq!(serialized["token"], "0.1F");
    assert!(serialized.get("value").is_none());

    let binary64 =
        ProductionRealAbi::from_package_format(galec::package::AlgorithmCodeRealFormat::Binary64);
    for (value, expected) in [(0.1_f64, "0.1"), (-0.0_f64, "-0.0"), (1.0_f64, "1.0")] {
        let literal = PreparedSolveAlgorithmRealLiteral::from_f64_bits(value.to_bits(), binary64)
            .expect("every finite binary64 value has an exact round-trip C token");
        assert_eq!(literal.as_str(), expected);
        assert_eq!(
            literal
                .as_str()
                .parse::<f64>()
                .expect("the construction-issued binary64 token is parseable")
                .to_bits(),
            value.to_bits()
        );
    }
    assert!(
        PreparedSolveAlgorithmRealLiteral::from_f64_bits(f64::NAN.to_bits(), binary64).is_none()
    );
    assert!(
        PreparedSolveAlgorithmRealLiteral::from_f64_bits(f64::INFINITY.to_bits(), binary64)
            .is_none()
    );
}

#[test]
fn production_boundary_contains_no_template_or_raw_operation_surface() {
    let boundary_source = include_str!("../solve_algorithm_production.rs");

    assert!(!boundary_source.contains("minijinja"));
    assert!(!boundary_source.contains("fn render"));
    assert!(!boundary_source.contains("AlgorithmCodeSubject"));
    assert!(!boundary_source.contains("\".alg\""));
    assert!(!boundary_source.contains("Real32 { value: f32 }"));
    assert!(
        !boundary_source.contains(".zip("),
        "prepared Solve production must index through retained cardinality proofs, never use truncating zip"
    );
}
