//! Unit tests for the serializable manifest context views + filters (WI-1).

use serde_json::json;

use super::views::*;
use crate::manifest_context::algorithm_code_manifest::BlockCausality;
use crate::manifest_context::algorithm_code_manifest::{
    AlgorithmCodeManifest, AlgorithmCodeManifestParts, BlockMethod, BlockMethods, Clock,
    ErrorSignalStatus, RealVariable, StartValue, Variable, VariableCommon,
};
use crate::manifest_context::checksum::Sha1Hex;
use crate::manifest_context::content::{
    Content, ContentParts, ModelRepresentation, ModelRepresentationKind,
};
use crate::manifest_context::ids::{
    FilePath, Identifier, ManifestId, NameWithoutSlashes, NormalizedText, UtcTimestamp,
};
use crate::manifest_context::manifest_common::{
    BaseUnit, File, FileChecksum, FileRole, ManifestAttributes, Unit,
};

fn ident(value: &str) -> Identifier {
    Identifier::new(value).unwrap()
}

fn text(value: &str) -> NormalizedText {
    NormalizedText::new(value).unwrap()
}

fn ac_attributes() -> ManifestAttributes {
    ManifestAttributes {
        id: ManifestId::parse("{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}").unwrap(),
        name: text("TestBlock"),
        description: None,
        version: None,
        generation_date_and_time: UtcTimestamp::parse("2026-07-03T12:00:00Z").unwrap(),
        generation_tool: None,
        copyright: None,
        license: None,
    }
}

/// A minimal validated AC manifest: one Real `constant` clock variable in
/// seconds, three void block methods, one seconds unit.
fn ac_manifest() -> AlgorithmCodeManifest {
    AlgorithmCodeManifest::new(AlgorithmCodeManifestParts {
        attributes: ac_attributes(),
        file_ref_id: ident("F_ALG"),
        files: vec![File {
            id: ident("F_ALG"),
            name: NameWithoutSlashes::new("TestBlock.alg").unwrap(),
            path: FilePath::root(),
            checksum: FileChecksum::Sha1(Sha1Hex::of_bytes(b"alg")),
            role: FileRole::Code,
            description: None,
        }],
        clock: Clock {
            id: ident("CLK"),
            variable_ref_id: ident("V_T"),
        },
        block_methods: BlockMethods {
            startup: BlockMethod {
                id: ident("BM_STARTUP"),
                signals: vec![],
            },
            recalibrate: BlockMethod {
                id: ident("BM_RECALIBRATE"),
                signals: vec![],
            },
            do_step: BlockMethod {
                id: ident("BM_DOSTEP"),
                signals: vec![],
            },
        },
        error_signal_status: ErrorSignalStatus { id: ident("ESS") },
        units: vec![Unit {
            id: ident("U_S"),
            name: text("s"),
            base_unit: Some(BaseUnit {
                s: 1,
                ..BaseUnit::default()
            }),
        }],
        variables: vec![Variable::Real(RealVariable {
            common: VariableCommon {
                id: ident("V_T"),
                name: text("samplePeriod"),
                description: Some(text("period <s>")),
                block_causality: BlockCausality::Constant,
                dimensions: vec![],
                annotations: vec![],
            },
            start: StartValue::Scalar(0.001),
            unit_ref_id: Some(ident("U_S")),
            relative_quantity: false,
            min: None,
            max: None,
            nominal: Some(0.001),
        })],
        annotations: vec![],
    })
    .unwrap()
}

fn content() -> Content {
    Content::new(ContentParts {
        attributes: ManifestAttributes {
            id: ManifestId::parse("{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}").unwrap(),
            ..ac_attributes()
        },
        active_fmu: None,
        model_representations: vec![ModelRepresentation {
            name: NameWithoutSlashes::new("AlgorithmCode").unwrap(),
            kind: ModelRepresentationKind::AlgorithmCode,
            manifest: NameWithoutSlashes::new("manifest.xml").unwrap(),
            checksum: Sha1Hex::of_bytes(b"ac-bytes"),
            manifest_ref_id: ManifestId::parse("{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}").unwrap(),
        }],
    })
    .unwrap()
}

#[test]
fn ac_view_serializes_the_validated_shape() {
    let manifest = ac_manifest();
    let ac = AcManifestCtx::from_manifest(&manifest);
    let value = serde_json::to_value(&ac).unwrap();

    assert_eq!(
        value["attributes"]["id"],
        "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}"
    );
    assert_eq!(value["file_ref_id"], "F_ALG");
    assert_eq!(value["files"][0]["needs_checksum"], true);
    assert_eq!(value["clock"]["variable_ref_id"], "V_T");
    assert_eq!(value["block_methods"]["do_step"]["kind"], "DoStep");
    assert_eq!(value["error_signal_status_id"], "ESS");

    let variable = &value["variables"][0];
    assert_eq!(variable["kind"], "Real");
    assert_eq!(variable["block_causality"], "constant");
    // start / nominal are pre-formatted xs:double lexicals.
    assert_eq!(variable["start"], "0.001");
    assert_eq!(variable["nominal"], "0.001");
    assert_eq!(variable["unit_ref_id"], "U_S");
    // Raw description carries the un-escaped angle brackets; escaping is the
    // template's job.
    assert_eq!(variable["description"], "period <s>");
}

#[test]
fn units_carry_raw_factor_for_the_xs_double_filter() {
    let manifest = ac_manifest();
    let units = AcManifestCtx::units(&manifest);
    let value = serde_json::to_value(&units).unwrap();
    assert_eq!(value[0]["name"], "s");
    // Exponent stays i32; factor/offset stay raw f64 so the template runs
    // them through xs_double / the only-when-nondefault guard.
    assert_eq!(value[0]["base_unit"]["s"], 1);
    assert_eq!(value[0]["base_unit"]["factor"], 1.0);
    assert_eq!(value[0]["base_unit"]["offset"], 0.0);
}

#[test]
fn content_view_serializes_representations() {
    let value = serde_json::to_value(ContentCtx::from_content(&content())).unwrap();
    assert!(value["active_fmu"].is_null());
    let rep = &value["representations"][0];
    assert_eq!(rep["kind"], "AlgorithmCode");
    assert_eq!(rep["checksum"], Sha1Hex::of_bytes(b"ac-bytes").as_str());
}

#[test]
fn model_export_is_serializable_and_neutral() {
    // Layer A carries neutral keys and RAW values only — no eFMI id strings.
    let export = ModelExport {
        model_name: "TestBlock".to_owned(),
        block_name: "TestBlock".to_owned(),
        description: None,
        struct_name: "TestBlockState".to_owned(),
        function_prefix: "TestBlock".to_owned(),
        include_guard: "TESTBLOCK_H".to_owned(),
        variables: vec![VarEntry {
            key: 1,
            name: "samplePeriod".to_owned(),
            description: None,
            scalar_type: ScalarKind::Real,
            causality: Causality::Constant,
            dimensions: vec![],
            start: StartView::Scalar {
                value: NumberView::Real(0.001),
            },
            min: None,
            max: None,
            nominal: Some(NumberView::Real(0.001)),
            unit: Some(UnitDecomp {
                name: "s".to_owned(),
                base_unit: None,
            }),
            relative_quantity: false,
            storage_name: "samplePeriod".to_owned(),
            storage_type: StorageType::Double,
        }],
        clock_variable_key: 1,
    };
    let value = serde_json::to_value(&export).unwrap();
    assert_eq!(value["variables"][0]["key"], 1);
    assert_eq!(value["variables"][0]["scalar_type"], "Real");
    assert_eq!(value["variables"][0]["causality"], "constant");
    assert_eq!(value["variables"][0]["storage_type"], "double");
    assert_eq!(
        value["variables"][0]["start"],
        json!({"kind": "scalar", "value": 0.001})
    );
}

#[test]
fn xml_escape_covers_the_five_predefined_entities() {
    assert_eq!(
        xml_escape(r#"<a & b "c" 'd'>"#),
        "&lt;a &amp; b &quot;c&quot; &apos;d&apos;&gt;"
    );
    // Idempotence is not claimed on already-escaped text, but plain text is
    // returned unchanged.
    assert_eq!(xml_escape("plain text"), "plain text");
}

#[test]
fn xs_double_is_a_valid_lexical_form_at_magnitude_extremes() {
    // A naive `{{ value }}` would emit a bare int or exponent notation; the
    // filter guarantees an explicit decimal point and no exponent.
    assert_eq!(xs_double(2.0), "2.0");
    assert_eq!(xs_double(-10.0), "-10.0");
    for value in [1.0e300, -1.0e300, 5.0e-324, f64::MAX, f64::MIN_POSITIVE] {
        let rendered = xs_double(value);
        let digits = rendered.strip_prefix('-').unwrap_or(&rendered);
        assert_eq!(
            digits.matches('.').count(),
            1,
            "`{rendered}` must contain exactly one decimal point"
        );
        assert!(
            digits.chars().all(|c| c.is_ascii_digit() || c == '.'),
            "`{rendered}` must contain only digits and a decimal point"
        );
    }
}
