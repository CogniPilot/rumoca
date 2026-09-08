use std::collections::BTreeMap;
use std::path::Path;

use rumoca_core::{
    RealMatrixMultiplySemantics, TargetInvocationBrand, with_target_invocation_brand,
};
use rumoca_ir_galec as galec;

use super::AlgorithmCodeTemplateRenderer;
use crate::{
    AlgorithmCodeArtifactLayout, AlgorithmCodeArtifactRole, AlgorithmCodeSourceOutputPathTemplate,
    AlgorithmCodeTemplateFile, PackagedAlgorithmCodeTemplateFile, TemplateArtifactKind,
    TemplateSemanticContext, prepare_algorithm_code_package,
    prepare_packaged_algorithm_code_template_file, render_packaged_algorithm_code_file,
};

fn renderer<'inv>(brand: TargetInvocationBrand<'inv>) -> AlgorithmCodeTemplateRenderer<'inv> {
    AlgorithmCodeTemplateRenderer {
        _brand: brand,
        context: minijinja::context! {
            algorithm_code => minijinja::context! { checked_value => "from-checked-root" },
            ir_kind => "algorithm_code",
        },
    }
}

fn file<'inv, 'path>(
    brand: TargetInvocationBrand<'inv>,
    path: &'path AlgorithmCodeSourceOutputPathTemplate,
    body: &str,
) -> AlgorithmCodeTemplateFile<'inv, 'path> {
    AlgorithmCodeTemplateFile::construct(
        brand,
        TemplateArtifactKind::AlgorithmCode,
        TemplateSemanticContext::Galec,
        path,
        body,
    )
    .unwrap()
}

fn package_parts() -> (galec::Block, galec::package::AlgorithmCodePackageMetadata) {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("packaged-renderer.mo"),
        0,
        1,
    );
    let mut block = galec::Block::new(galec::ast::Name::ident("PackagedModel"));
    let mut output = galec::ast::VariableDeclaration::scalar(
        galec::ast::ScalarType::Real,
        galec::ast::Name::ident("y"),
    );
    output.span = span;
    block.interface.push(galec::ast::InterfaceVariable {
        kind: galec::ast::InterfaceKind::Output,
        decl: output,
        start: Some(galec::ast::Expression::Real(0.0)),
    });
    let mut period = galec::ast::VariableDeclaration::scalar(
        galec::ast::ScalarType::Real,
        galec::ast::Name::ident("period"),
    );
    period.span = span;
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
            span,
        });
    }
    (
        block,
        galec::package::AlgorithmCodePackageMetadata::new(
            vec![None; 2],
            "period",
            Vec::new(),
            galec::package::AlgorithmCodeArithmeticProfile::construct(
                galec::package::AlgorithmCodeRealFormat::Binary64,
                galec::package::AlgorithmCodeIntegerFormat::I32,
                RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        ),
    )
}

fn traced_package<'inv>(
    brand: TargetInvocationBrand<'inv>,
    source_text: &str,
) -> galec::TracedAlgorithmCodeProduct<'inv> {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("packaged-renderer.mo", source_text);
    assert_eq!(
        source,
        rumoca_core::SourceId::from_source_name("packaged-renderer.mo")
    );
    galec::TracedAlgorithmCodeProduct::project_from_origin(
        brand,
        &sources,
        "PackagedModel",
        |issuer| {
            let (block, metadata) = package_parts();
            issuer.construct(block, metadata)
        },
    )
    .expect("renderer fixture retains its exact trace origin")
}

fn artifact_layout() -> AlgorithmCodeArtifactLayout {
    AlgorithmCodeArtifactLayout::construct([
        (AlgorithmCodeArtifactRole::PackageManifest, "__content.xml"),
        (
            AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
            "AlgorithmCode/manifest.xml",
        ),
        (
            AlgorithmCodeArtifactRole::AlgorithmCodeSource,
            "AlgorithmCode/model.alg",
        ),
    ])
    .unwrap()
}

#[test]
fn content_receives_only_the_exact_artifact_facts_and_checked_root() {
    with_target_invocation_brand(|brand| {
        let identities = BTreeMap::from([("model".to_owned(), "identity".to_owned())]);
        let checksums = BTreeMap::from([("source".to_owned(), "digest".to_owned())]);
        let facts = super::super::codegen_test_support::artifact_bindings(
            brand,
            "1970-01-01T00:00:00Z",
            "test-tool",
            "model",
            &identities,
            &checksums,
        );
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked.alg".into()).unwrap();
        let file = file(
            brand,
            &output_path,
            "{{ algorithm_code.checked_value }}|{{ artifact.generation_tool }}|{{ __rumoca_artifact_identity_v1_model }}|{{ artifact.checksums.source }}",
        );

        let rendered = renderer(brand).render_content(&file, &facts).unwrap();
        assert_eq!(rendered, "from-checked-root|test-tool|identity|digest");
    });
}

#[test]
fn output_path_receives_no_algorithm_code_semantics() {
    for path in [
        "{{ algorithm_code.checked_value }}.alg",
        "{{ model_name }}.alg",
        "AlgorithmCode/model.alg",
        "../model.alg",
        "/model.alg",
        "model\\nested.alg",
        ".alg",
        ".hidden.alg",
        "model.ALG",
        "CON.alg",
        "lpt1.trace.alg",
    ] {
        assert!(
            AlgorithmCodeSourceOutputPathTemplate::construct(path.into()).is_err(),
            "unsafe, dynamic, packaged, or noncanonical source-only path must reject: {path}"
        );
    }
    with_target_invocation_brand(|brand| {
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked-model.alg".into())
                .expect("one portable static basename is admitted");
        let file = file(brand, &output_path, "body");
        assert_eq!(
            renderer(brand).render_output_path(&file).unwrap(),
            "checked-model.alg"
        );
    });
}

#[test]
fn one_checked_file_renders_each_exact_checksum_context() {
    with_target_invocation_brand(|brand| {
        let identities = BTreeMap::new();
        let first_checksums = BTreeMap::from([("source".to_owned(), "first-digest".to_owned())]);
        let final_checksums = BTreeMap::from([("source".to_owned(), "final-digest".to_owned())]);
        let first_facts = super::super::codegen_test_support::artifact_bindings(
            brand,
            "timestamp",
            "tool",
            "model",
            &identities,
            &first_checksums,
        );
        let final_facts = super::super::codegen_test_support::artifact_bindings(
            brand,
            "timestamp",
            "tool",
            "model",
            &identities,
            &final_checksums,
        );
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked.alg".into()).unwrap();
        let file = file(brand, &output_path, "{{ artifact.checksums.source }}");

        assert_eq!(
            renderer(brand).render_output_path(&file).unwrap(),
            "checked.alg"
        );
        assert_eq!(
            renderer(brand).render_content(&file, &first_facts).unwrap(),
            "first-digest"
        );
        assert_eq!(
            renderer(brand).render_content(&file, &final_facts).unwrap(),
            "final-digest"
        );
    });
}

#[test]
fn checked_renderer_surface_has_no_raw_or_generic_escape() {
    let source = include_str!("../algorithm_code_renderer.rs");
    assert!(!source.contains("render_with_name_and_artifact"));
    assert!(!source.contains("new_correlated"));
    assert!(!source.contains("T: serde::Serialize"));
    assert!(!source.contains("pub fn render_template"));
    assert!(!source.contains("pub fn render_content(\n        &self,\n        template: &str"));
    let correlated = source
        .split("pub fn render_correlated_algorithm_code_file")
        .nth(1)
        .expect("one closed correlated renderer function");
    let signature = correlated
        .split("Result<RenderedCorrelatedAlgorithmCodeFile")
        .next()
        .expect("correlated renderer return boundary");
    assert!(!signature.contains("model_name"));
    assert!(!signature.contains("AlgorithmCodePackage"));
    assert!(!signature.contains("SourceMap"));
    let packaged = source
        .split("pub fn render_packaged_algorithm_code_file")
        .nth(1)
        .expect("one closed packaged renderer function");
    let packaged_signature = packaged
        .split("Result<RenderedPackagedAlgorithmCodeFile")
        .next()
        .expect("packaged renderer return boundary");
    assert!(!packaged_signature.contains("SourceMap"));
    assert!(!source.contains("SourceMap::new()"));

    let rendered = include_str!("../../rendered_template_file.rs");
    assert!(!rendered.contains("into_parts"));
    assert!(!rendered.contains("pub fn construct"));
    assert!(!rendered.contains("pub fn path"));
}

#[test]
fn same_source_id_in_a_different_map_cannot_replace_the_origin_trace() {
    with_target_invocation_brand(|brand| {
        let product = traced_package(brand, "origin statement");
        let source_id = rumoca_core::SourceId::from_source_name("packaged-renderer.mo");
        let mut wrong_map = rumoca_core::SourceMap::new();
        assert!(wrong_map.register_id(
            source_id,
            "different-name.mo",
            std::sync::Arc::<str>::from("\nwrong statement"),
        ));
        assert_eq!(wrong_map.first_source_id(), Some(source_id));
        assert_ne!(
            product.sources().get_source(source_id),
            wrong_map.get_source(source_id),
        );

        let renderer = AlgorithmCodeTemplateRenderer::new(&product)
            .expect("the traced product alone constructs its renderer");
        let output_path =
            AlgorithmCodeSourceOutputPathTemplate::construct("checked.alg".into()).unwrap();
        let file = file(
            brand,
            &output_path,
            "{{ algorithm_code.traces.files[0].path }}",
        );
        let identities = BTreeMap::new();
        let checksums = BTreeMap::new();
        let facts = super::super::codegen_test_support::artifact_bindings(
            brand,
            "timestamp",
            "tool",
            "model",
            &identities,
            &checksums,
        );

        assert_eq!(
            renderer.render_content(&file, &facts).unwrap(),
            "packaged-renderer.mo",
            "rendering resolves the retained origin; the same SourceId in the foreign map has no input slot",
        );
    });
}

#[test]
fn packaged_roles_render_only_the_exact_retained_members() {
    with_target_invocation_brand(|brand| {
        let layout = artifact_layout();
        let package = prepare_algorithm_code_package(traced_package(brand, "x"), layout);
        let identities = BTreeMap::new();
        let checksums = BTreeMap::new();
        let facts = super::super::codegen_test_support::artifact_bindings(
            brand,
            "timestamp",
            "tool",
            "model",
            &identities,
            &checksums,
        );
        let cases = [
            (
                AlgorithmCodeArtifactRole::PackageManifest,
                TemplateArtifactKind::Xml,
                "root",
                "__content.xml",
                "root",
            ),
            (
                AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                TemplateArtifactKind::Xml,
                "{{ algorithm_code.artifact_layout.algorithm_code_source.file_name }}|{{ algorithm_code.artifact_layout.algorithm_code_source.file_path }}",
                "AlgorithmCode/manifest.xml",
                "model.alg|./",
            ),
            (
                AlgorithmCodeArtifactRole::AlgorithmCodeSource,
                TemplateArtifactKind::AlgorithmCode,
                "source",
                "AlgorithmCode/model.alg",
                "source",
            ),
        ];
        for (role, kind, body, expected_path, expected_content) in cases {
            let file = PackagedAlgorithmCodeTemplateFile::construct(
                brand,
                role,
                kind,
                TemplateSemanticContext::Galec,
                body,
            )
            .unwrap();
            let file = prepare_packaged_algorithm_code_template_file(&package, &file);
            let rendered = render_packaged_algorithm_code_file(file, &facts).unwrap();
            let path = rendered.member().member_path().as_str();
            let content = rendered.content();
            assert_eq!(path, expected_path);
            assert_eq!(content, expected_content);
        }
    });
}

#[test]
fn packaged_role_kind_mismatches_are_unrepresentable() {
    with_target_invocation_brand(|brand| {
        for (role, kind) in [
            (
                AlgorithmCodeArtifactRole::AlgorithmCodeSource,
                TemplateArtifactKind::Xml,
            ),
            (
                AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                TemplateArtifactKind::AlgorithmCode,
            ),
            (
                AlgorithmCodeArtifactRole::PackageManifest,
                TemplateArtifactKind::AlgorithmCode,
            ),
        ] {
            let error = PackagedAlgorithmCodeTemplateFile::construct(
                brand,
                role,
                kind,
                TemplateSemanticContext::Galec,
                "body",
            )
            .unwrap_err();
            assert_eq!(
                error,
                crate::TemplateFilePlanError::PackagedAlgorithmCodeArtifactRoleMismatch {
                    role,
                    artifact_kind: kind,
                }
            );
        }
    });
}

#[test]
fn algorithm_code_projection_has_no_omission_shaped_nominal_zip() {
    let source = include_str!("../../views/algorithm_code.rs");
    assert!(!source.contains(".zip(package.variable_nominals())"));
    assert!(source.contains("variable_nominals[index]"));
    for forbidden_start_access in ["values.try_into", "values[", ".first(", ".next("] {
        assert!(
            !source.contains(forbidden_start_access),
            "Algorithm Code start projection must not recheck or truncate through `{forbidden_start_access}`"
        );
    }
    let correlated = source
        .split("impl<'a> CorrelatedAlgorithmCodeView<'a>")
        .nth(1)
        .and_then(|tail| tail.split("struct VariableView").next())
        .expect("correlated view construction remains visible to the gate");
    assert!(!correlated.contains("AlgorithmCodeManifestPresentation::construct"));
    assert!(!correlated.contains("AlgorithmCodeManifestIdentifier"));
}

#[test]
fn algorithm_code_renderer_has_one_closed_implementation_surface() {
    let mut sources = Vec::new();
    collect_rust_sources(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("src").as_path(),
        &mut sources,
    );
    let implementation_marker = ["impl<'inv> AlgorithmCode", "TemplateRenderer<'inv>"].concat();
    let implementation_count = sources
        .iter()
        .map(|source| source.matches(&implementation_marker).count())
        .sum::<usize>();
    assert_eq!(implementation_count, 1);
}

fn collect_rust_sources(directory: &Path, sources: &mut Vec<String>) {
    let mut entries = std::fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("read {}: {error}", directory.display()))
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|error| panic!("read entry below {}: {error}", directory.display()));
    entries.sort_by_key(std::fs::DirEntry::path);
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_rust_sources(&path, sources);
        } else if path.extension().and_then(|value| value.to_str()) == Some("rs") {
            sources.push(
                std::fs::read_to_string(&path)
                    .unwrap_or_else(|error| panic!("read {}: {error}", path.display())),
            );
        }
    }
}
