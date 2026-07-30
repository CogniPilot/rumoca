//! Architecture checks for the checked Algorithm Code template surface.

use super::{create_environment, xs_double_str};
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

fn is_conformant_real_literal(text: &str) -> bool {
    let text = text.strip_prefix('-').unwrap_or(text);
    let Some((integer, fraction)) = text.split_once('.') else {
        return false;
    };
    if integer.is_empty()
        || !integer.bytes().all(|byte| byte.is_ascii_digit())
        || integer.starts_with('0') && integer.len() != 1
    {
        return false;
    }
    let (fraction, exponent) = fraction.split_once('e').unwrap_or((fraction, ""));
    if fraction.is_empty() || !fraction.bytes().all(|byte| byte.is_ascii_digit()) {
        return false;
    }
    exponent.is_empty()
        || exponent.strip_prefix(['+', '-']).is_some_and(|digits| {
            !digits.is_empty() && digits.bytes().all(|byte| byte.is_ascii_digit())
        })
}

#[test]
fn portable_real_filter_preserves_expected_galec_spellings() {
    for (value, expected) in [
        (0.0, "0.0"),
        (-0.0, "-0.0"),
        (0.5, "0.5"),
        (-2.5, "-2.5"),
        (100_000.0, "100000.0"),
        (0.000_001, "0.000001"),
        (1.0e300, "1.0e+300"),
        (-1.5e300, "-1.5e+300"),
        (1.0e-300, "1.0e-300"),
        (1.0e21, "1.0e+21"),
    ] {
        assert_eq!(xs_double_str(value).unwrap(), expected);
    }
}

#[test]
fn portable_real_filter_is_conformant_and_round_trips() {
    for value in [
        0.0,
        -0.0,
        1.0,
        -1.0,
        0.1 + 0.2,
        std::f64::consts::PI,
        1.0e-42,
        -3.25e17,
        f64::MAX,
        f64::MIN_POSITIVE,
        5e-324,
    ] {
        let rendered = xs_double_str(value).unwrap();
        assert!(is_conformant_real_literal(&rendered), "{rendered}");
        assert_eq!(rendered.parse::<f64>().unwrap(), value);
    }
}

#[test]
fn portable_real_filter_rejects_non_finite_values() {
    for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(xs_double_str(value).is_err());
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

#[test]
fn galec_real_min_max_are_relational_target_helpers() {
    let source = templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
        .expect("C source template");

    assert!(!source.contains(r#"function == "min" -%}fmin"#), "{source}");
    assert!(!source.contains(r#"function == "max" -%}fmax"#), "{source}");
    assert!(
        source.contains(r#"function == "min" -%}rumoca_galec_min"#),
        "{source}"
    );
    assert!(
        source.contains(r#"function == "max" -%}rumoca_galec_max"#),
        "{source}"
    );
    assert!(source.contains("return u1 < u2 ? u1 : u2;"), "{source}");
    assert!(source.contains("return u1 > u2 ? u1 : u2;"), "{source}");
    assert!(
        source.contains("#define rumoca_galec_imin"),
        "Integer min must retain its distinct builtin mapping"
    );
    assert!(
        source.contains("#define rumoca_galec_imax"),
        "Integer max must retain its distinct builtin mapping"
    );
}
