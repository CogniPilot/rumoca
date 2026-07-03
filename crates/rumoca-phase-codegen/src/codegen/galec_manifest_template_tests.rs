//! Render tests for the real eFMI manifest jinja templates (WI-2).
//!
//! These are the shipping `galec-production` manifest templates
//! (`src/templates/galec-production/`), which the switch-dispatch build step
//! renders for the `galec`/`galec-production` targets (contract §9 WI-5; the
//! AC-only `galec` target ships a byte-identical copy as `manifest.xml.jinja`).
//! They are loaded here with `include_str!` and rendered through the shared
//! [`create_environment`] (which registers the `xml_escape` / `xs_double`
//! filters), against a serde context whose shape matches
//! `rumoca_galec_codegen::manifest_context::EfmiManifestContext`.
//!
//! These are structural checks (child order, wrapper cardinality, 1-based-AC
//! vs 0-based-PC `Dimension/@number`, `origin="true"`, only-when-true
//! booleans, `<Target>` as text, escaping, `xs_double`). Full XSD validation
//! and checksum-web correctness are the container/CLI tests of later work
//! items.

use super::*;
use serde_json::json;

const AC_TEMPLATE: &str = include_str!("../templates/galec-production/ac_manifest.xml.jinja");
const PC_TEMPLATE: &str = include_str!("../templates/galec-production/pc_manifest.xml.jinja");
const CONTENT_TEMPLATE: &str = include_str!("../templates/galec-production/__content.xml.jinja");

/// The AC-only `galec` target ships its own copy of the Algorithm Code
/// manifest template (each `target.toml` bundle is self-contained). It must
/// stay byte-identical to the `galec-production` copy this file validates, so
/// the equivalence proof here covers both — a divergence is a drift bug.
const GALEC_AC_TEMPLATE: &str = include_str!("../templates/galec/manifest.xml.jinja");
const GALEC_CONTENT_TEMPLATE: &str = include_str!("../templates/galec/__content.xml.jinja");

#[test]
fn galec_and_galec_production_share_identical_manifest_templates() {
    assert_eq!(
        GALEC_AC_TEMPLATE, AC_TEMPLATE,
        "galec/manifest.xml.jinja must stay byte-identical to \
         galec-production/ac_manifest.xml.jinja"
    );
    assert_eq!(
        GALEC_CONTENT_TEMPLATE, CONTENT_TEMPLATE,
        "galec/__content.xml.jinja must stay byte-identical to \
         galec-production/__content.xml.jinja"
    );
}

fn render(template: &str, ctx: serde_json::Value) -> String {
    let env = create_environment();
    let ctx_value = minijinja::Value::from_serialize(&ctx);
    env.render_str(template, minijinja::context! { ctx => ctx_value })
        .unwrap_or_else(|error| panic!("template render failed: {error:#}"))
}

fn assert_before(haystack: &str, first: &str, second: &str) {
    let a = haystack
        .find(first)
        .unwrap_or_else(|| panic!("missing {first:?}\n{haystack}"));
    let b = haystack
        .find(second)
        .unwrap_or_else(|| panic!("missing {second:?}\n{haystack}"));
    assert!(a < b, "{first:?} must precede {second:?}\n{haystack}");
}

fn attributes(id: &str) -> serde_json::Value {
    json!({
        "id": id,
        "name": "TestBlock",
        "description": serde_json::Value::Null,
        "version": serde_json::Value::Null,
        "generation_date_and_time": "2026-07-03T12:00:00Z",
        "generation_tool": serde_json::Value::Null,
        "copyright": serde_json::Value::Null,
        "license": serde_json::Value::Null,
    })
}

fn ac_context() -> serde_json::Value {
    json!({
        "ac": {
            "attributes": {
                "id": "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}",
                "name": "TestBlock",
                // Angle brackets exercise xml_escape.
                "description": "period <b>",
                "version": serde_json::Value::Null,
                "generation_date_and_time": "2026-07-03T12:00:00Z",
                "generation_tool": serde_json::Value::Null,
                "copyright": serde_json::Value::Null,
                "license": serde_json::Value::Null,
            },
            "file_ref_id": "F_ALG",
            "files": [{
                "id": "F_ALG", "name": "TestBlock.alg", "path": "",
                "needs_checksum": true, "checksum": "abc123",
                "role": "Code", "description": serde_json::Value::Null,
            }],
            "clock": { "id": "CLK", "variable_ref_id": "V_T" },
            "block_methods": {
                "startup": { "id": "BM_STARTUP", "kind": "Startup", "signals": [] },
                "recalibrate": { "id": "BM_RECALIBRATE", "kind": "Recalibrate", "signals": [] },
                "do_step": { "id": "BM_DOSTEP", "kind": "DoStep", "signals": ["NAN"] },
            },
            "error_signal_status_id": "ESS",
            "variables": [
                {
                    "kind": "Real", "id": "V_T", "name": "samplePeriod",
                    "description": serde_json::Value::Null,
                    "block_causality": "constant", "dimensions": [],
                    "start": "0.001", "annotations": [],
                    "unit_ref_id": "U_S", "relative_quantity": false,
                    "min": serde_json::Value::Null, "max": serde_json::Value::Null,
                    "nominal": "0.001",
                },
                {
                    "kind": "Integer", "id": "V_A", "name": "arr",
                    "description": serde_json::Value::Null,
                    "block_causality": "state", "dimensions": [2, 3],
                    "start": "0 0 0 0 0 0",
                    "annotations": [{ "annotation_type": "com.example" }],
                    "unit_ref_id": serde_json::Value::Null,
                    "relative_quantity": false,
                    "min": serde_json::Value::Null, "max": "10",
                    "nominal": serde_json::Value::Null,
                },
            ],
            "annotations": [],
        },
        "units": [{
            "id": "U_S", "name": "s",
            // factor != 1.0 exercises xs_double; offset 0.0 stays omitted.
            "base_unit": {
                "kg": 0, "m": 0, "s": 1, "ampere": 0, "kelvin": 0,
                "mol": 0, "cd": 0, "rad": 0, "factor": 2.0, "offset": 0.0,
            },
        }],
    })
}

#[test]
fn ac_manifest_reproduces_the_serializer_structure() {
    let out = render(AC_TEMPLATE, ac_context());

    assert!(out.starts_with("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
    // xs:sequence child order is load-bearing.
    assert_before(&out, "<Files", "<Clock");
    assert_before(&out, "<Clock", "<BlockMethods");
    assert_before(&out, "<BlockMethods", "<ErrorSignalStatus");
    assert_before(&out, "<ErrorSignalStatus", "<Units");
    assert_before(&out, "<Units", "<Variables");

    // Escaping (autoescape OFF -> explicit filter on every text value).
    assert!(out.contains("description=\"period &lt;b&gt;\""));

    // AC Dimension/@number is 1-based.
    assert!(out.contains("<Dimension number=\"1\" size=\"2\"/>"));
    assert!(out.contains("<Dimension number=\"2\" size=\"3\"/>"));

    // xs_double on the raw BaseUnit factor; nondefault-only exponents/offset.
    assert!(out.contains("factor=\"2.0\""));
    assert!(out.contains(" s=\"1\""));
    assert!(!out.contains("offset="));
    assert!(!out.contains(" kg="));

    // Scalar Real is self-closing; unitRefId + nominal present.
    assert!(out.contains("unitRefId=\"U_S\""));
    assert!(out.contains("nominal=\"0.001\""));

    // BlockMethod self-closes without signals, opens with them.
    assert!(out.contains("kind=\"Startup\"/>"));
    assert!(out.contains("<Signal value=\"NAN\"/>"));

    // Integer array variable carries its state causality + max only.
    assert!(out.contains("<IntegerVariable"));
    assert!(out.contains("max=\"10\""));
}

#[test]
fn ac_manifest_always_emits_empty_files_and_units_wrappers() {
    let mut ctx = ac_context();
    ctx["ac"]["files"] = json!([]);
    ctx["units"] = json!([]);
    let out = render(AC_TEMPLATE, ctx);
    assert!(
        out.contains("<Files/>"),
        "empty Files must self-close:\n{out}"
    );
    assert!(
        out.contains("<Units/>"),
        "empty Units must self-close:\n{out}"
    );
}

#[test]
fn content_registry_loops_representations() {
    let ctx = json!({
        "content": {
            "attributes": attributes("{2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11}"),
            "active_fmu": serde_json::Value::Null,
            "representations": [{
                "name": "AlgorithmCode", "kind": "AlgorithmCode",
                "manifest": "manifest.xml", "checksum": "deadbeef",
                "manifest_ref_id": "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}",
            }],
        }
    });
    let out = render(CONTENT_TEMPLATE, ctx);
    assert!(out.contains("<Content "));
    assert!(
        !out.contains("activeFmu"),
        "rumoca never emits activeFmu:\n{out}"
    );
    assert!(out.contains("kind=\"AlgorithmCode\""));
    assert!(out.contains("checksum=\"deadbeef\""));
    assert!(out.contains("manifestRefId=\"{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}\""));
}

fn pc_context() -> serde_json::Value {
    json!({
        "pc": {
            "attributes": attributes("{3c9aa74e-1d2f-4b6a-9e07-51c4f0d88b31}"),
            "manifest_reference": {
                "id": "MR_AC",
                "manifest_ref_id": "{7d6b1a52-4f0e-4d59-9d3b-215f8e5b6a20}",
                "checksum": "acsha1",
            },
            "files": [{
                "id": "F_H", "name": "TestBlock.h", "path": "",
                "needs_checksum": true, "checksum": "hsha",
                "role": "Code", "description": serde_json::Value::Null,
            }],
            "code_container": {
                "language": "C", "standard": "C99", "platform": "Legacy",
                "float_precision": "64-bit",
                "description": serde_json::Value::Null,
                "target": "Generic",
                "target_types": [
                    { "id": "TT_F64", "kind": "efmiFloat64", "coded_type": "double" },
                    { "id": "TT_VOID", "kind": "efmiVoid", "coded_type": "void" },
                ],
                "code_files": [
                    {
                        "id": "CF_H", "file_type": "ProductionCode",
                        "code_type": "HeaderFile", "file_ref_id": "F_H",
                        "file_ref_kind": serde_json::Value::Null,
                        "includes": [],
                        "typedefs": [
                            {
                                "id": "TD_F64", "name": "double",
                                "alias": { "target_type_ref_id": "TT_F64",
                                           "type_def_ref_id": serde_json::Value::Null },
                                "components": serde_json::Value::Null,
                            },
                            {
                                "id": "TD_STATE", "name": "TestBlock_state",
                                "alias": serde_json::Value::Null,
                                "components": [
                                    { "id": "CO_0", "name": "x",
                                      "type_def_ref_id": "TD_F64",
                                      "dimensions": [], "pointer": false },
                                    { "id": "CO_1", "name": "arr",
                                      "type_def_ref_id": "TD_F64",
                                      "dimensions": [2, 3], "pointer": false },
                                ],
                            },
                        ],
                        "functions": [],
                    },
                    {
                        "id": "CF_C", "file_type": "ProductionCode",
                        "code_type": "SourceFile", "file_ref_id": "F_H",
                        "file_ref_kind": serde_json::Value::Null,
                        "includes": ["CF_H"],
                        "typedefs": [],
                        "functions": [{
                            "id": "FN_DOSTEP", "name": "TestBlock_dostep",
                            "return_parameter": {
                                "id": "RP", "type_def_ref_id": "TD_F64",
                                "constant": false, "pointer": false,
                                "const_pointer": false, "dimensions": [],
                            },
                            "formal_parameters": [{
                                "name": "self",
                                "core": {
                                    "id": "FP_SELF", "type_def_ref_id": "TD_STATE",
                                    "constant": false, "pointer": true,
                                    "const_pointer": false, "dimensions": [],
                                },
                            }],
                        }],
                    },
                ],
                "logical_data": {
                    "data_references": [{
                        "manifest_reference_ref_id": "MR_AC",
                        "foreign_ref_id": "V_T",
                        "formal_parameter_ref_id": "FP_SELF",
                        "component_identifier": "x",
                    }],
                    "function_references": [{
                        "manifest_reference_ref_id": "MR_AC",
                        "foreign_ref_id": "BM_DOSTEP",
                        "function_ref_id": "FN_DOSTEP",
                    }],
                },
            },
            "annotations": [],
        }
    })
}

#[test]
fn pc_manifest_reproduces_the_serializer_structure() {
    let out = render(PC_TEMPLATE, pc_context());

    assert_before(&out, "<ManifestReferences", "<Files");
    assert_before(&out, "<Files", "<CodeContainer");
    assert_before(&out, "<Target>", "<TargetTypes");
    assert_before(&out, "<TargetTypes", "<CodeFiles");
    assert_before(&out, "<CodeFiles", "<LogicalData");

    // origin is unconditional on the single ManifestReference.
    assert!(out.contains("origin=\"true\""));
    // Target is a text element, not an attribute.
    assert!(out.contains("<Target>Generic</Target>"));

    // PC Dimension/@number is 0-based (CO_1 arr[2,3]).
    assert!(out.contains("<Dimension number=\"0\" size=\"2\"/>"));
    assert!(out.contains("<Dimension number=\"1\" size=\"3\"/>"));

    // FormalParameter/@number is 0-based; pointer emitted only when true.
    assert!(out.contains("number=\"0\""));
    assert!(out.contains("pointer=\"true\""));
    // const / constPointer are false here, so must not appear.
    assert!(!out.contains("const=\"true\""));
    assert!(!out.contains("constPointer=\"true\""));

    // Includes wrapper appears (CF_C has one) and componentIdentifier passes.
    assert!(out.contains("<Include codeFileRefId=\"CF_H\"/>"));
    assert!(out.contains("componentIdentifier=\"x\""));

    // LogicalData inner wrappers present with children.
    assert!(out.contains("<DataReferences>"));
    assert!(out.contains("<FunctionReferences>"));
    assert!(out.contains("<GlobalFunction functionRefId=\"FN_DOSTEP\"/>"));
}

#[test]
fn pc_manifest_always_emits_empty_logical_data_wrappers() {
    let mut ctx = pc_context();
    ctx["pc"]["code_container"]["logical_data"]["data_references"] = json!([]);
    ctx["pc"]["code_container"]["logical_data"]["function_references"] = json!([]);
    let out = render(PC_TEMPLATE, ctx);
    assert!(
        out.contains("<DataReferences/>"),
        "empty DataReferences must self-close:\n{out}"
    );
    assert!(
        out.contains("<FunctionReferences/>"),
        "empty FunctionReferences must self-close:\n{out}"
    );
}
