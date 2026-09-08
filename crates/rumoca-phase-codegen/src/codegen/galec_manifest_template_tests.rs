//! Architecture checks for the checked Algorithm Code template surface.

use super::{target_template_environment, xs_double_str};
use crate::templates;

#[test]
fn galec_templates_parse_in_the_strict_environment() {
    let mut env = target_template_environment();
    for target in ["galec"] {
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
    for target in ["galec"] {
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

/// Operator parenthesization facts are typed IR data, never template
/// classification (SPEC_0042 T6, GAL-019).
///
/// The load-bearing guarantee is type-level and lives beside the operator
/// enum: `BinaryOp::precedence_class` and `PrecedenceClass::associativity`
/// are exhaustive `match`es, and `BinaryOp`'s hand-written `Serialize`
/// consumes them, so an operator without a class or a class without an
/// associativity is a compile failure and every serialized operator carries
/// both facts. The strict render environment then fails any template read of
/// a missing field. This test is the narrow textual tombstone on top: the
/// GALEC source template must consume those serialized facts and must not
/// grow back an operator-classification branch or spell precedence-class
/// names itself.
#[test]
fn algorithm_code_source_template_consumes_typed_precedence_facts() {
    let bundle = templates::builtin_target("galec").expect("built-in Algorithm Code target");
    let source = &bundle
        .templates
        .iter()
        .find(|template| template.path == "model.alg.jinja")
        .expect("Algorithm Code source template")
        .source;
    for required in [
        "value.value.op.precedence_class.kind",
        "parent_op.precedence_class.kind",
        "parent_op.associativity.kind",
    ] {
        assert!(
            source.contains(required),
            "model.alg.jinja must consume the serialized operator fact `{required}`"
        );
    }
    for forbidden in [
        // The deleted in-template classifier and its inputs.
        "macro precedence_class",
        "parent_op.kind",
        // Precedence-class spellings: classes reach the template as opaque
        // values compared for equality, so their names never appear in it.
        "power",
        "multiplicative",
        "additive",
        "relational",
        "equality",
        "logical_and",
        "logical_or",
    ] {
        assert!(
            !source.contains(forbidden),
            "model.alg.jinja must not re-derive operator precedence via `{forbidden}`"
        );
    }
}

/// The serialized operator shape the strict template environment relies on:
/// `kind` plus construction-issued `precedence_class` and `associativity`.
#[test]
fn binary_operator_serialization_carries_typed_precedence_facts() {
    use rumoca_ir_galec::ast::BinaryOp;
    for (op, kind, class, associativity) in [
        (BinaryOp::Pow, "pow", "power", "right"),
        (BinaryOp::Div, "div", "multiplicative", "left"),
        (BinaryOp::Sub, "sub", "additive", "left"),
        (BinaryOp::Le, "le", "relational", "left"),
        (BinaryOp::Ne, "ne", "equality", "left"),
        (BinaryOp::And, "and", "logical_and", "left"),
        (BinaryOp::Or, "or", "logical_or", "left"),
    ] {
        let value = serde_json::to_value(op).expect("operator must serialize");
        assert_eq!(value["kind"], kind);
        assert_eq!(value["precedence_class"]["kind"], class);
        assert_eq!(value["associativity"]["kind"], associativity);
    }
}

#[test]
fn algorithm_code_manifest_starts_have_no_truncating_value_access() {
    for target in ["galec", "efmu"] {
        let bundle = templates::builtin_target(target).expect("built-in Algorithm Code target");
        for path in ["manifest.xml.jinja", "ac_manifest.xml.jinja"] {
            let Some(template) = bundle
                .templates
                .iter()
                .find(|template| template.path == path)
            else {
                continue;
            };
            for forbidden in ["values[", ".first(", ".next(", "|default"] {
                assert!(
                    !template.source.contains(forbidden),
                    "{target}/{path} may silently truncate a start payload via `{forbidden}`"
                );
            }
            assert!(template.source.contains("value.start.form"));
            assert!(template.source.contains("value.start.value"));
            assert!(template.source.contains("value.start.values"));
        }
    }
}

#[test]
fn standalone_manifest_borrows_only_construction_issued_identifiers_and_file_facts() {
    let bundle = templates::builtin_target("galec").expect("built-in Algorithm Code target");
    let manifest = bundle
        .templates
        .iter()
        .find(|template| template.path == "manifest.xml.jinja")
        .expect("standalone Algorithm Code manifest template");
    for forbidden in [
        "\"V\" ~",
        "BM_STARTUP",
        "BM_RECALIBRATE",
        "BM_DOSTEP",
        "ESS",
        "model_name",
        "clock_variable_ordinal",
        "variable.ordinal",
        "fail_closed_",
    ] {
        assert!(
            !manifest.source.contains(forbidden),
            "standalone manifest must not reconstruct or weakly reject `{forbidden}`"
        );
    }
    for required in [
        "algorithm_code.presentation.algorithm_code_file",
        "algorithm_code.artifact_layout.algorithm_code_source.file_name",
        "algorithm_code.artifact_layout.algorithm_code_source.file_path",
        "algorithm_code.presentation.clock_variable",
        "variable.manifest.algorithm_code_identity",
        "fail(\"unprepared Algorithm Code",
    ] {
        assert!(
            manifest.source.contains(required),
            "standalone manifest must consume construction fact `{required}`"
        );
    }
    let target = include_str!("../templates/galec/target.toml");
    assert!(target.contains("path = \"AlgorithmCode/model.alg\""));
    assert!(!target.contains("AlgorithmCode/{{ model_name }}.alg"));
}

fn is_conformant_real_literal(text: &str) -> bool {
    let text = text.strip_prefix('-').unwrap_or(text);
    let Some(decimal_index) = text.find('.') else {
        return false;
    };
    let (integer, fraction_with_separator) = text.split_at(decimal_index);
    let fraction = &fraction_with_separator[1..];
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
