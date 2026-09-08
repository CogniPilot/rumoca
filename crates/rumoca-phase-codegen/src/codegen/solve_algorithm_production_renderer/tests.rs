use std::collections::BTreeMap;
use std::process::Command;

use rumoca_core::{
    RealMatrixMultiplySemantics, SourceId, Span, TargetInvocationBrand,
    with_target_invocation_brand,
};
use rumoca_ir_galec as galec;
use rumoca_ir_solve as solve;

use super::*;
use crate::{
    CorrelatedAlgorithmCodeArtifactRole, CorrelatedAlgorithmCodeTemplateFile,
    ProductionArtifactLayout, ProductionArtifactRole, ProductionCodeFileRole,
    SolveAlgorithmTemplateFile, TemplateArtifactKind, TemplateSemanticContext,
    prepare_correlated_algorithm_code_template_file, prepare_solve_algorithm_production,
    prepare_solve_algorithm_template_file, render_correlated_algorithm_code_file,
};

mod tensor_start;

const MATRIX_ARITHMETIC: RealMatrixMultiplySemantics =
    RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;

fn scalar_assignment_product<'inv>(
    brand: TargetInvocationBrand<'inv>,
) -> solve::SolveAlgorithmProduct<'inv> {
    scalar_assignment_product_with_value(
        brand,
        galec::ast::Expression::Real(1.25),
        galec::package::AlgorithmCodeRealFormat::Binary32,
    )
}

fn scalar_assignment_product_with_value<'inv>(
    brand: TargetInvocationBrand<'inv>,
    assigned_value: galec::ast::Expression,
    real_format: galec::package::AlgorithmCodeRealFormat,
) -> solve::SolveAlgorithmProduct<'inv> {
    let source = SourceId::from_source_name("production-c-scalar.mo");
    let mut output = galec::ast::VariableDeclaration::scalar(
        galec::ast::ScalarType::Real,
        galec::ast::Name::ident("y"),
    );
    output.span = Span::from_offsets(source, 1, 8);
    let mut block = galec::Block::new(galec::ast::Name::ident("ScalarProduction"));
    block.interface.push(galec::ast::InterfaceVariable {
        kind: galec::ast::InterfaceKind::Output,
        decl: output,
        start: Some(galec::ast::Expression::Real(0.0)),
    });
    let mut period = galec::ast::VariableDeclaration::scalar(
        galec::ast::ScalarType::Real,
        galec::ast::Name::ident("period"),
    );
    period.span = Span::from_offsets(source, 1, 8);
    block.protected.push(galec::ast::ProtectedEntity {
        kind: galec::ast::ProtectedKind::Constant,
        decl: period,
        start: Some(galec::ast::Expression::Real(0.01)),
    });
    for (name, value) in [("y", 0.0), ("period", 0.01)] {
        block.startup.statements.push(galec::ast::Spanned {
            node: galec::ast::Statement::Assignment {
                target: galec::ast::Reference::state(galec::ast::Name::ident(name)),
                value: galec::ast::Expression::Real(value),
            },
            span: Span::from_offsets(source, 8, 10),
        });
    }
    block.do_step.statements.push(galec::ast::Spanned {
        node: galec::ast::Statement::Assignment {
            target: galec::ast::Reference::state(galec::ast::Name::ident("y")),
            value: assigned_value,
        },
        span: Span::from_offsets(source, 10, 24),
    });
    let mut sources = rumoca_core::SourceMap::new();
    assert_eq!(
        sources.add("production-c-scalar.mo", &" ".repeat(64)),
        source,
    );
    let package = galec::TracedAlgorithmCodeProduct::project_from_origin(
        brand,
        &sources,
        "ScalarProduction",
        |issuer| {
            issuer.construct(
                block,
                galec::package::AlgorithmCodePackageMetadata::new(
                    vec![None; 2],
                    "period",
                    Vec::new(),
                    galec::package::AlgorithmCodeArithmeticProfile::construct(
                        real_format,
                        galec::package::AlgorithmCodeIntegerFormat::I32,
                        MATRIX_ARITHMETIC,
                    ),
                ),
            )
        },
    )
    .expect("scalar fixture retains its exact trace origin");
    rumoca_phase_solve::lower_solve_algorithm_product(package)
        .expect("scalar package has a total Solve Algorithm refinement")
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
    .expect("the renderer fixture has one complete static artifact layout")
}

fn render_production_test_file<'inv>(
    brand: TargetInvocationBrand<'inv>,
    prepared: &crate::PreparedSolveAlgorithmProduction<'inv>,
    artifact: &crate::TemplateBindings<'inv>,
    role: ProductionCodeFileRole,
    kind: TemplateArtifactKind,
    template: &str,
) -> (String, String) {
    let file = SolveAlgorithmTemplateFile::construct(
        brand,
        role,
        kind,
        TemplateSemanticContext::Solve,
        template,
    )
    .expect("the Production file declaration is valid");
    let bound = prepare_solve_algorithm_template_file(prepared, &file);
    let rendered = render_solve_algorithm_production_file(bound, artifact)
        .expect("the passive template renders prepared lexical facts");
    (
        rendered.member().member_path().as_str().to_owned(),
        rendered.content().to_owned(),
    )
}

fn compile_and_run_rendered_c(
    source_member: &str,
    header_member: &str,
    source_content: &str,
    header_content: &str,
    driver_content: &str,
    executable_name: &str,
) {
    let directory = tempfile::tempdir().expect("create isolated C test directory");
    let source_path = directory.path().join(source_member);
    let header_path = directory.path().join(header_member);
    let driver_path = directory.path().join("driver.c");
    let executable_path = directory.path().join(executable_name);
    std::fs::create_dir_all(
        source_path
            .parent()
            .expect("the checked source member has a parent directory"),
    )
    .expect("create checked Production Code member directories");
    std::fs::write(&source_path, source_content).expect("write rendered C test source");
    std::fs::write(&header_path, header_content).expect("write rendered C test header");
    std::fs::write(&driver_path, driver_content).expect("write Production C test driver");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror"])
        .arg(&driver_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&executable_path)
        .output()
        .expect("invoke the host C compiler");
    assert!(
        compile.status.success(),
        "C compiler rejected checked output: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        Command::new(executable_path)
            .status()
            .expect("run compiled Production C probe")
            .success()
    );
}

#[test]
fn checked_product_renders_compiles_and_runs_production_c() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive");
        let prepared = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect("scalar Solve Algorithm is Production C-ready");
        let presentation = prepared.presentation();
        let integer_type = presentation.integer32_type().as_str().to_owned();
        let status_type = presentation.status_type().as_str().to_owned();
        let status_ok = presentation
            .success_status()
            .identifier()
            .as_str()
            .to_owned();
        let storage_type = presentation.storage_type().as_str().to_owned();
        let startup = presentation
            .method(solve::SolveAlgorithmMethodKind::Startup)
            .function()
            .as_str()
            .to_owned();
        let do_step = presentation
            .method(solve::SolveAlgorithmMethodKind::DoStep)
            .function()
            .as_str()
            .to_owned();
        let output_component = presentation.declarations()[0]
            .component()
            .as_str()
            .to_owned();
        let identities = BTreeMap::new();
        let checksums = BTreeMap::new();
        let artifact = crate::codegen::codegen_test_support::artifact_bindings(
            brand,
            "2026-08-30T00:00:00Z",
            "rumoca-test",
            "model",
            &identities,
            &checksums,
        );
        let (header_path, header_content) = render_production_test_file(
            brand,
            &prepared,
            &artifact,
            ProductionCodeFileRole::Header,
            crate::TemplateArtifactKind::CHeader,
            include_str!("../../templates/efmu/production.h.jinja"),
        );
        assert!(header_content.contains(&format!("typedef int32_t {integer_type};")));
        assert!(header_content.contains(&format!("typedef int32_t {status_type};")));
        assert!(header_content.contains(&format!(
            "static const {status_type} {status_ok} = INT32_C(0);"
        )));
        assert!(!header_content.contains("typedef enum"));
        assert!(header_content.contains(&format!("{status_type} {do_step}(")));
        assert!(!header_content.contains(&format!("void {do_step}(")));
        assert!(!header_content.contains("int64_t"));

        let (path, content) = render_production_test_file(
            brand,
            &prepared,
            &artifact,
            ProductionCodeFileRole::Source,
            crate::TemplateArtifactKind::CSource,
            include_str!("../../templates/efmu/production.c.jinja"),
        );
        assert_eq!(header_path, "ProductionCode/sources/production.h");
        assert_eq!(path, "ProductionCode/sources/production.c");
        assert!(content.contains("#include \"production.h\""));
        assert!(!content.contains("algorithm_code"));
        assert_eq!(content.matches(&format!("return {status_ok};")).count(), 3);
        assert_eq!(content.matches(" = UINT32_C(0);").count(), 3);

        let driver = format!(
            "#include \"ProductionCode/sources/production.h\"\n_Static_assert(sizeof({status_type}) == 4, \"status must be signed i32\");\nint main(void) {{\n    {storage_type} value;\n    if ({startup}(&value) != {status_ok}) return 2;\n    if ({do_step}(&value) != {status_ok}) return 3;\n    return value.{output_component} == 1.25F ? 0 : 1;\n}}\n"
        );
        compile_and_run_rendered_c(
            &path,
            &header_path,
            &content,
            &header_content,
            &driver,
            "production-c-probe",
        );
    });
}

#[test]
fn prepared_product_renders_correlated_production_code_manifest() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive");
        let prepared = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect("scalar Solve Algorithm is Production Code manifest-ready");
        let file = SolveAlgorithmTemplateFile::construct(
            brand,
            ProductionCodeFileRole::Manifest,
            crate::TemplateArtifactKind::Xml,
            TemplateSemanticContext::Solve,
            include_str!("../../templates/efmu/pc_manifest.xml.jinja"),
        )
        .expect("the Production Code manifest declaration is valid");
        let bound = prepare_solve_algorithm_template_file(&prepared, &file);
        let identities = [
            (
                "ac_manifest".to_owned(),
                "11111111-1111-4111-8111-111111111111".to_owned(),
            ),
            (
                "pc_manifest".to_owned(),
                "22222222-2222-4222-8222-222222222222".to_owned(),
            ),
        ]
        .into_iter()
        .collect();
        let checksums = [
            ("ac_manifest_sha1".to_owned(), "a".repeat(40)),
            ("production_header_sha1".to_owned(), "b".repeat(40)),
            ("production_source_sha1".to_owned(), "c".repeat(40)),
        ]
        .into_iter()
        .collect();
        let artifact = crate::codegen::codegen_test_support::artifact_bindings(
            brand,
            "2026-08-30T00:00:00Z",
            "rumoca-test",
            "model",
            &identities,
            &checksums,
        );
        let rendered = render_solve_algorithm_production_file(bound, &artifact)
            .expect("passive XML template renders checked Production facts");
        let path = rendered.member().member_path().as_str();
        let content = rendered.content();
        assert_eq!(path, "ProductionCode/manifest.xml");
        assert!(content.contains("kind=\"ProductionCode\""));
        assert!(content.contains(
        "<CodeContainer language=\"C\" standard=\"C99\" platform=\"Legacy\" floatPrecision=\"32-bit\">"
    ));
        assert!(content.contains("name=\"production.h\" path=\"./sources/\""));
        assert!(content.contains("name=\"production.c\" path=\"./sources/\""));
        assert_eq!(content.matches("<DataReference>").count(), 3);
        assert_eq!(content.matches("<FunctionReference>").count(), 3);
        for method in prepared.methods() {
            assert!(content.contains(method.presentation().algorithm_code_identity().as_str()));
            assert!(content.contains(method.presentation().function().as_str()));
        }
    });
}

fn compile_rendered_width(header: &str, source: &str, precision: &str) {
    let directory = tempfile::tempdir().expect("create isolated C test directory");
    let header_path = directory.path().join("production.h");
    let source_path = directory.path().join("production.c");
    let object_path = directory.path().join("production.o");
    std::fs::write(&header_path, header).expect("write rendered width-specific header");
    std::fs::write(&source_path, source).expect("write rendered width-specific source");
    let compile = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg(&source_path)
        .arg("-o")
        .arg(object_path)
        .output()
        .expect("invoke the host C compiler");
    assert!(
        compile.status.success(),
        "C compiler rejected {precision} lexical output: {}",
        String::from_utf8_lossy(&compile.stderr)
    );
}

#[test]
fn package_selected_real_width_drives_header_source_and_manifest_together() {
    for (format, c_type, typedef_name, literal, precision, efmi_type) in [
        (
            galec::package::AlgorithmCodeRealFormat::Binary32,
            "float",
            "rumoca_real_rumoca_model",
            "1.25F",
            "32-bit",
            "efmiFloat32",
        ),
        (
            galec::package::AlgorithmCodeRealFormat::Binary64,
            "double",
            "rumoca_real_rumoca_model",
            "1.25",
            "64-bit",
            "efmiFloat64",
        ),
    ] {
        with_target_invocation_brand(|brand| {
            let product = scalar_assignment_product_with_value(
                brand,
                galec::ast::Expression::Real(1.25),
                format,
            );
            let profile =
                crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(
                    65_536,
                )
                .expect("the fixture automatic-payload budget is positive");
            let prepared =
                prepare_solve_algorithm_production(product, profile, production_artifact_layout())
                    .expect("the package-selected Real format has one closed Production ABI");
            let identities = [
                (
                    "ac_manifest".to_owned(),
                    "11111111-1111-4111-8111-111111111111".to_owned(),
                ),
                (
                    "pc_manifest".to_owned(),
                    "22222222-2222-4222-8222-222222222222".to_owned(),
                ),
            ]
            .into_iter()
            .collect();
            let checksums = [
                ("ac_manifest_sha1".to_owned(), "a".repeat(40)),
                ("production_header_sha1".to_owned(), "b".repeat(40)),
                ("production_source_sha1".to_owned(), "c".repeat(40)),
            ]
            .into_iter()
            .collect();
            let artifact = crate::codegen::codegen_test_support::artifact_bindings(
                brand,
                "2026-08-30T00:00:00Z",
                "rumoca-width-test",
                "model",
                &identities,
                &checksums,
            );
            let (_, header) = render_production_test_file(
                brand,
                &prepared,
                &artifact,
                ProductionCodeFileRole::Header,
                TemplateArtifactKind::CHeader,
                include_str!("../../templates/efmu/production.h.jinja"),
            );
            let (_, source) = render_production_test_file(
                brand,
                &prepared,
                &artifact,
                ProductionCodeFileRole::Source,
                TemplateArtifactKind::CSource,
                include_str!("../../templates/efmu/production.c.jinja"),
            );
            let (_, manifest) = render_production_test_file(
                brand,
                &prepared,
                &artifact,
                ProductionCodeFileRole::Manifest,
                TemplateArtifactKind::Xml,
                include_str!("../../templates/efmu/pc_manifest.xml.jinja"),
            );

            assert!(header.contains(&format!("typedef {c_type} {typedef_name};")));
            assert!(source.contains(&format!(" = {literal};")));
            assert!(manifest.contains(&format!("floatPrecision=\"{precision}\"")));
            assert!(manifest.contains(&format!("kind=\"{efmi_type}\" codedType=\"{c_type}\"")));
            assert!(manifest.contains(&format!(
                "<Typedef id=\"{typedef_name}\" name=\"{typedef_name}\""
            )));
            compile_rendered_width(&header, &source, precision);
        });
    }
}

#[test]
fn production_view_does_not_forge_a_toolchain_or_refinement_receipt() {
    with_target_invocation_brand(|brand| {
        let prepared = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive"),
            production_artifact_layout(),
        )
        .expect("source rendering remains available without a false proof claim");
        let serialized = serde_json::to_string(&ProductionView::construct(&prepared))
            .expect("the renderer view is serialization-only input");
        for forged_claim in [
            "toolchain_receipt",
            "refinement_receipt",
            "ecm_003_conformant",
            "authenticated_production_c",
        ] {
            assert!(!serialized.contains(forged_claim));
        }

        let boundary = include_str!("../../views/solve_algorithm_production.rs");
        for missing_obligation in [
            "FLT_EVAL_METHOD == 0",
            "no excess precision",
            "disabled contraction",
            "floating-environment/status",
            "trap behavior",
        ] {
            assert!(boundary.contains(missing_obligation));
        }
    });
}

#[test]
fn prepared_product_renders_correlated_algorithm_code_manifest() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive");
        let prepared = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect("scalar Solve Algorithm is correlated Algorithm Code manifest-ready");
        let file = CorrelatedAlgorithmCodeTemplateFile::construct(
            brand,
            CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest,
            TemplateArtifactKind::Xml,
            TemplateSemanticContext::Galec,
            include_str!("../../templates/efmu/ac_manifest.xml.jinja"),
        )
        .expect("the Algorithm Code manifest declaration is valid");
        let file = prepare_correlated_algorithm_code_template_file(&prepared, &file);
        let identities = [(
            "ac_manifest".to_owned(),
            "11111111-1111-4111-8111-111111111111".to_owned(),
        )]
        .into_iter()
        .collect();
        let checksums = [("alg_sha1".to_owned(), "a".repeat(40))]
            .into_iter()
            .collect();
        let artifact = crate::codegen::codegen_test_support::artifact_bindings(
            brand,
            "2026-08-30T00:00:00Z",
            "rumoca-test",
            "model",
            &identities,
            &checksums,
        );
        let rendered = render_correlated_algorithm_code_file(file, &artifact)
            .expect("passive XML template renders correlated Algorithm Code facts");
        let path = rendered.member().member_path().as_str();
        let content = rendered.content();
        assert_eq!(path, "AlgorithmCode/manifest.xml");
        assert!(content.contains("name=\"y\" blockCausality=\"output\" start=\"0.0\""));
        let presentation = prepared.presentation();
        assert!(content.contains(presentation.manifest().algorithm_code_file().as_str()));
        assert!(content.contains(presentation.manifest().algorithm_code_clock().as_str()));
        assert!(content.contains(presentation.clock().algorithm_code_identity().as_str()));
        for declaration in presentation.declarations() {
            assert!(content.contains(declaration.algorithm_code_identity().as_str()));
        }
        for method in prepared.methods() {
            assert!(content.contains(method.presentation().algorithm_code_identity().as_str()));
        }

        let template = include_str!("../../templates/efmu/ac_manifest.xml.jinja");
        for forbidden_reconstruction in [
            "loop.index",
            "model_name",
            "BM_STARTUP",
            "BM_RECALIBRATE",
            "BM_DOSTEP",
            "ESS",
            "V1",
        ] {
            assert!(
                !template.contains(forbidden_reconstruction),
                "Algorithm Code manifest template must not reconstruct `{forbidden_reconstruction}`"
            );
        }
    });
}

#[test]
fn correlated_algorithm_code_roles_select_only_their_retained_layout_members() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive");
        let prepared = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect("scalar Solve Algorithm has a complete correlated layout");
        for (role, kind, expected) in [
            (
                CorrelatedAlgorithmCodeArtifactRole::PackageManifest,
                TemplateArtifactKind::Xml,
                "__content.xml",
            ),
            (
                CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                TemplateArtifactKind::Xml,
                "AlgorithmCode/manifest.xml",
            ),
            (
                CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource,
                TemplateArtifactKind::AlgorithmCode,
                "AlgorithmCode/model.alg",
            ),
        ] {
            let file = CorrelatedAlgorithmCodeTemplateFile::construct(
                brand,
                role,
                kind,
                TemplateSemanticContext::Galec,
                "body",
            )
            .expect("the role/kind pair is admitted at declaration construction");
            let bound = prepare_correlated_algorithm_code_template_file(&prepared, &file);
            assert_eq!(bound.member().member_path().as_str(), expected);
        }

        for (role, kind) in [
            (
                CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource,
                TemplateArtifactKind::Xml,
            ),
            (
                CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                TemplateArtifactKind::AlgorithmCode,
            ),
            (
                CorrelatedAlgorithmCodeArtifactRole::PackageManifest,
                TemplateArtifactKind::AlgorithmCode,
            ),
        ] {
            let error = CorrelatedAlgorithmCodeTemplateFile::construct(
                brand,
                role,
                kind,
                TemplateSemanticContext::Galec,
                "body",
            )
            .expect_err("a correlated role cannot be paired with another artifact kind");
            assert_eq!(
                error,
                crate::TemplateFilePlanError::CorrelatedAlgorithmCodeArtifactRoleMismatch {
                    role,
                    artifact_kind: kind,
                }
            );
        }
    });
}

#[test]
fn production_code_roles_select_only_their_retained_layout_members() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(65_536)
                .expect("the fixture automatic-payload budget is positive");
        let prepared = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect("scalar Solve Algorithm has a complete Production Code layout");
        for (role, kind, expected) in [
            (
                ProductionCodeFileRole::Manifest,
                TemplateArtifactKind::Xml,
                "ProductionCode/manifest.xml",
            ),
            (
                ProductionCodeFileRole::Header,
                TemplateArtifactKind::CHeader,
                "ProductionCode/sources/production.h",
            ),
            (
                ProductionCodeFileRole::Source,
                TemplateArtifactKind::CSource,
                "ProductionCode/sources/production.c",
            ),
        ] {
            let file = SolveAlgorithmTemplateFile::construct(
                brand,
                role,
                kind,
                TemplateSemanticContext::Solve,
                "body",
            )
            .expect("the closed Production Code role/kind pair is admitted");
            let bound = prepare_solve_algorithm_template_file(&prepared, &file);
            assert_eq!(bound.member().member_path().as_str(), expected);
        }
        for (role, kind) in [
            (
                ProductionCodeFileRole::Header,
                TemplateArtifactKind::CSource,
            ),
            (ProductionCodeFileRole::Source, TemplateArtifactKind::Xml),
        ] {
            assert!(matches!(
                SolveAlgorithmTemplateFile::construct(
                    brand,
                    role,
                    kind,
                    TemplateSemanticContext::Solve,
                    "body",
                ),
                Err(crate::TemplateFilePlanError::ProductionCodeFileRoleMismatch { .. })
            ));
        }
    });
}

#[test]
fn exact_real32_tokens_render_and_compile_without_integer_suffix_forms() {
    with_target_invocation_brand(|brand| {
        for (value, expected_assignment) in [(0.1_f32, " = 0.1F;"), (-0.0_f32, " = -0.0F;")] {
            let profile =
                crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(
                    65_536,
                )
                .expect("the fixture automatic-payload budget is positive");
            let prepared = prepare_solve_algorithm_production(
                scalar_assignment_product_with_value(
                    brand,
                    galec::ast::Expression::Real(f64::from(value)),
                    galec::package::AlgorithmCodeRealFormat::Binary32,
                ),
                profile,
                production_artifact_layout(),
            )
            .expect("finite binary32 literals are Production C-ready");
            let identities = BTreeMap::new();
            let checksums = BTreeMap::new();
            let artifact = crate::codegen::codegen_test_support::artifact_bindings(
                brand,
                "2026-08-30T00:00:00Z",
                "rumoca-test",
                "model",
                &identities,
                &checksums,
            );
            let header = SolveAlgorithmTemplateFile::construct(
                brand,
                ProductionCodeFileRole::Header,
                TemplateArtifactKind::CHeader,
                TemplateSemanticContext::Solve,
                include_str!("../../templates/efmu/production.h.jinja"),
            )
            .expect("the C header declaration is valid");
            let header = prepare_solve_algorithm_template_file(&prepared, &header);
            let rendered_header = render_solve_algorithm_production_file(header, &artifact)
                .expect("the checked header renders");
            let header_path = rendered_header.member().member_path().as_str().to_owned();
            let header_content = rendered_header.content().to_owned();
            let source = SolveAlgorithmTemplateFile::construct(
                brand,
                ProductionCodeFileRole::Source,
                TemplateArtifactKind::CSource,
                TemplateSemanticContext::Solve,
                include_str!("../../templates/efmu/production.c.jinja"),
            )
            .expect("the C source declaration is valid");
            let source = prepare_solve_algorithm_template_file(&prepared, &source);
            let rendered_source = render_solve_algorithm_production_file(source, &artifact)
                .expect("the checked source renders");
            let source_path = rendered_source.member().member_path().as_str().to_owned();
            let source_content = rendered_source.content().to_owned();
            assert!(source_content.contains(expected_assignment));
            assert!(!source_content.contains(" = 0F;"));
            assert!(!source_content.contains(" = -0F;"));

            let directory = tempfile::tempdir().expect("create isolated C test directory");
            let header_path = directory.path().join(header_path);
            let source_path = directory.path().join(source_path);
            std::fs::create_dir_all(
                source_path
                    .parent()
                    .expect("the checked source member has a parent directory"),
            )
            .expect("create checked Production Code member directories");
            std::fs::write(header_path, header_content).expect("write rendered C header");
            std::fs::write(&source_path, source_content).expect("write rendered C source");
            let object_path = directory.path().join("production.o");
            let compile = Command::new("cc")
                .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
                .arg(&source_path)
                .arg("-o")
                .arg(object_path)
                .output()
                .expect("invoke the host C compiler");
            assert!(
                compile.status.success(),
                "C compiler rejected exact binary32 token `{expected_assignment}`: {}",
                String::from_utf8_lossy(&compile.stderr)
            );
        }
    });
}

#[test]
fn declared_method_automatic_payload_budget_is_enforced_during_preparation() {
    with_target_invocation_brand(|brand| {
        let profile =
            crate::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(1)
                .expect("one byte is a positive declared automatic-payload budget");
        let error = prepare_solve_algorithm_production(
            scalar_assignment_product(brand),
            profile,
            production_artifact_layout(),
        )
        .expect_err("the scalar assignment needs one four-byte Real register");
        assert_eq!(
            error.requirement(),
            crate::SolveAlgorithmProductionRequirement::CallOwnerAbiAndWorkingMemory
        );
        assert_eq!(
            error.method(),
            Some(solve::SolveAlgorithmMethodKind::DoStep)
        );
    });
}

#[test]
fn integer_to_real_has_no_prepared_or_template_emission_arm() {
    let preparation = include_str!("../../views/solve_algorithm_production.rs");
    let template = include_str!("../../templates/efmu/production.c.jinja");

    assert!(preparation.contains("SolveOperation::Convert { .. }"));
    assert!(!preparation.contains("ConvertIntegerToReal"));
    assert!(!template.contains("convert_integer_to_real"));
    assert!(!template.contains("(efmiFloat32)"));
}

#[test]
fn production_renderer_serializes_facts_and_never_spells_c() {
    let renderer = include_str!("../solve_algorithm_production_renderer.rs");
    for forbidden in [
        "production_c:",
        "render_header",
        "render_source",
        "render_method",
        "render_operation",
        "#include",
        "typedef ",
        "INT64_C",
        "UINT32_C",
    ] {
        assert!(
            !renderer.contains(forbidden),
            "Rust Production renderer must not contain C spelling '{forbidden}'"
        );
    }
    assert!(renderer.contains("Value::from_serialize"));
}

#[test]
fn production_templates_only_spell_prepared_facts() {
    let header = include_str!("../../templates/efmu/production.h.jinja");
    let source = include_str!("../../templates/efmu/production.c.jinja");
    let manifest = include_str!("../../templates/efmu/pc_manifest.xml.jinja");
    for forbidden in [
        "loop.index",
        "loop.index0",
        "method.kind",
        "file_stem",
        "model_name",
        "_initialize(",
        "BM_",
        "V{{",
        concat!("render_", "expr"),
        concat!("render_", "stms"),
        "binary32",
        "binary64",
        "efmiFloat32",
        "efmiFloat64",
        "typedef float",
        "typedef double",
        "32-bit",
        "64-bit",
    ] {
        assert!(
            !header.contains(forbidden)
                && !source.contains(forbidden)
                && !manifest.contains(forbidden),
            "Production templates must not reconstruct prepared fact `{forbidden}`"
        );
    }
    assert!(source.contains("production.presentation.artifact_layout.header_include"));
}
