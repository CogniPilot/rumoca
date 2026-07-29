//! Architecture checks for the checked Algorithm Code template surface.

use super::create_environment;
use crate::templates;
use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use serde_json::json;

#[test]
fn galec_templates_parse_in_the_strict_shared_environment() {
    let mut env = create_environment();
    for target in ["galec", "galec-production", "embedded-c-galec"] {
        let bundle = templates::builtin_target(target).expect("built-in GALEC target");
        for template in bundle.templates {
            env.add_template_owned(
                format!("{target}/{}", template.path),
                template.source.to_owned(),
            )
            .unwrap_or_else(|error| panic!("{target}/{}: {error}", template.path));
        }
    }
}

#[test]
fn galec_templates_consume_checked_algorithm_code_and_artifact_facts() {
    for target in ["galec", "galec-production", "embedded-c-galec"] {
        let bundle = templates::builtin_target(target).expect("built-in GALEC target");
        for template in bundle.templates {
            assert!(
                !template.source.contains("ctx."),
                "{target}/{} retains the removed dynamic manifest context",
                template.path
            );
            assert!(
                !template.source.contains("galec_alg_source")
                    && !template.source.contains("galec_c_source")
                    && !template.source.contains("galec_c_header"),
                "{target}/{} retains a pre-rendered target-language passthrough",
                template.path
            );
        }
    }
}

fn collision_block() -> CheckedAlgorithmBlock {
    let input = |name| galec::InterfaceVariable {
        kind: galec::InterfaceKind::Input,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, name),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident("constexpr"));
    block.interface = vec![
        input(galec::Name::quoted("a.b")),
        input(galec::Name::ident("a_b")),
        input(galec::Name::ident("volatile")),
    ];
    CheckedAlgorithmBlock::construct(block).expect("valid collision fixture")
}

fn render_fixture(template: &str) -> String {
    let artifact = json!({
        "generated_at": "2026-01-01T00:00:00Z",
        "generation_tool": "rumoca-test",
        "identities": {
            "pc_manifest": "10000000-0000-0000-0000-000000000001",
            "ac_manifest": "10000000-0000-0000-0000-000000000002"
        },
        "checksums": {
            "ac_manifest_sha1": "0000000000000000000000000000000000000000",
            "c_header_sha1": "0000000000000000000000000000000000000000",
            "c_source_sha1": "0000000000000000000000000000000000000000"
        }
    });
    crate::render_checked_algorithm_block_template_with_artifact(
        &collision_block(),
        &artifact,
        template,
        "fixture",
    )
    .expect("render collision fixture")
}

#[test]
fn galec_c_symbols_are_collision_safe_reserved_disjoint_and_consistent() {
    let header = render_fixture(
        templates::builtin_template_source("embedded-c-galec", "model.h.jinja")
            .expect("C header template"),
    );
    let source = render_fixture(
        templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("C source template"),
    );
    let manifest = render_fixture(
        templates::builtin_template_source("galec-production", "pc_manifest.xml.jinja")
            .expect("Production Code manifest template"),
    );

    assert_eq!(header.matches("double b; /* declaration 1: a.b").count(), 1);
    assert_eq!(
        header.matches("double a_b; /* declaration 2: a_b").count(),
        1
    );
    assert_eq!(
        header
            .matches("double volatile_2; /* declaration 3: volatile")
            .count(),
        1
    );
    assert!(header.contains("constexpr_2State"), "{header}");
    assert!(source.contains("void constexpr_2_startup"), "{source}");
    assert!(manifest.contains("name=\"constexpr_2State\""), "{manifest}");
    assert!(
        manifest.contains("<Component id=\"CO_1\" name=\"b\" typeDefRefId=\"TD_F64\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("<Component id=\"CO_2\" name=\"a_b\" typeDefRefId=\"TD_F64\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("<Component id=\"CO_3\" name=\"volatile_2\" typeDefRefId=\"TD_F64\""),
        "{manifest}"
    );
    assert!(
        manifest.contains("componentIdentifier=\"a_b\""),
        "{manifest}"
    );
    assert!(manifest.contains("componentIdentifier=\"b\""), "{manifest}");
    assert!(
        manifest.contains("componentIdentifier=\"volatile_2\""),
        "{manifest}"
    );
}

#[test]
fn galec_c_templates_do_not_use_lossy_sanitization() {
    for target in ["embedded-c-galec", "galec-production"] {
        let bundle = templates::builtin_target(target).expect("built-in C target");
        for template in bundle.templates {
            assert!(
                !template.source.contains("| sanitize"),
                "{target}/{} uses lossy symbol sanitization",
                template.path
            );
        }
    }
}
