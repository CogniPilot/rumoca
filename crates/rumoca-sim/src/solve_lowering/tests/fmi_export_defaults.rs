//! FMI publication defaults preserve source absence and exact initialization.
mod native_defaults;

use super::*;
use rumoca_ir_solve::{SolveDeclarationStart as Start, SolveInitialValues, fmi::FmiComponent};
use serde::de::IntoDeserializer;

fn variable<'a>(component: &'a FmiComponent, name: &str) -> &'a rumoca_ir_solve::fmi::FmiVariable {
    component
        .variables()
        .iter()
        .find(|v| v.name() == name)
        .expect("source variable")
}

fn export_with_options(source: &str, name: &str, options: &SimOptions) -> FmiComponent {
    let dae = compile(source, name);
    let lowered = super::super::entry::lower_correlated_for_fmi_export(
        &dae,
        options,
        rumoca_ir_solve::RefreshProfile::default(),
    )
    .unwrap();
    rumoca_phase_solve::fmi::finish_fmi_component(lowered).unwrap()
}

#[test]
fn fmi_export_defaults_preserve_missing_source_and_type_bounds() {
    let component = export_with_options(
        "model Defaults type Mode=enumeration(a,b,c); input Real r[2](min={2,-5},max={5,-2}); input Integer i(min=7); input Boolean b; input Mode mode(min=Mode.b); input Real z(start=-0.0); Real x(start=0,fixed=true); equation der(x)=sum(r); end Defaults;",
        "Defaults",
        &SimOptions::default(),
    );
    for name in ["r", "i", "b", "mode"] {
        assert!(variable(&component, name).declaration_start().is_missing());
    }
    let Start::Real(r) = variable(&component, "r").instantiation_start() else {
        panic!("Real")
    };
    assert_eq!((r.value(0), r.value(1)), (Some(2.0), Some(-2.0)));
    let Start::Integer(i) = variable(&component, "i").instantiation_start() else {
        panic!("Integer")
    };
    assert_eq!(i.value(0), Some(&7));
    let Start::Boolean(b) = variable(&component, "b").instantiation_start() else {
        panic!("Boolean")
    };
    assert_eq!(b.value(0), Some(&false));
    let Start::Enumeration(mode) = variable(&component, "mode").instantiation_start() else {
        panic!("Enumeration")
    };
    assert_eq!(mode.value(0), Some(&2));
    let Start::Real(z) = variable(&component, "z").declaration_start() else {
        panic!("authored start")
    };
    assert_eq!(z.value(0).unwrap().to_bits(), (-0.0_f64).to_bits());
    component.into_codegen_view().try_c().unwrap();
}

#[test]
fn fmi_export_defaults_retain_actual_supplied_presence_and_parameter_context() {
    let component = export_with_options(
        "model Overrides parameter Real offset=.5; input Real u[2,3](each start=offset); input Real missing[2](each min=offset); Real x(start=0,fixed=true); equation der(x)=sum(u)+sum(missing); end Overrides;",
        "Overrides",
        &SimOptions {
            param_overrides: vec![("offset".into(), 0.75)],
            initial_inputs: vec![("u[1,2]".into(), 1.25), ("missing[1]".into(), 0.0)],
            ..SimOptions::default()
        },
    );
    let u = variable(&component, "u");
    // This input repair does not change the existing Parameter source facet.
    // Explicit export with startup Parameter overrides still exposes that
    // separate metadata mismatch; normal packaging supplies no overrides.
    let offset = variable(&component, "offset");
    let Start::Real(authored_offset) = offset.declaration_start() else {
        panic!("Parameter source")
    };
    assert_eq!(authored_offset.value(0), Some(0.5));
    let storage = offset.storage().unwrap();
    assert_eq!(storage.column(), rumoca_ir_solve::fmi::FmiStorageColumn::P);
    assert_eq!(
        component
            .runtime_view()
            .model()
            .parameters
            .value(storage.base()),
        Some(0.75)
    );
    let Start::Real(source) = u.declaration_start() else {
        panic!("authored source")
    };
    let Start::Real(actual) = u.instantiation_start() else {
        panic!("effective default")
    };
    assert_eq!(source.value(1), Some(0.75));
    assert_eq!(actual.value(1), Some(1.25));
    assert_eq!(actual.value(5), Some(0.75));
    let missing = variable(&component, "missing");
    assert!(missing.declaration_start().is_missing());
    let Start::Real(actual) = missing.instantiation_start() else {
        panic!("effective default")
    };
    assert_eq!((actual.value(0), actual.value(1)), (Some(0.0), Some(0.75)));
    assert_eq!(
        missing.effective_input_default().unwrap().origins()[0].origin,
        rumoca_ir_solve::fmi::FmiInputDefaultOrigin::Supplied
    );
    component.into_codegen_view().try_c().unwrap();
}

#[test]
fn fmi_export_defaults_keep_broadcasts_compact_through_wire_replay() {
    let component = export_with_options(
        "model Compact input Real u[2,6000](each min=2); Real x(start=0,fixed=true); equation der(x)=sum(u); end Compact;",
        "Compact",
        &SimOptions::default(),
    );
    let input = variable(&component, "u");
    assert!(input.declaration_start().is_missing());
    let default = input.effective_input_default().unwrap();
    assert_eq!(default.origins().len(), 1);
    assert!(default.values().is_repeat());
    assert_eq!(default.values().len(), 12_000);
    assert!(!component.runtime_view().model().parameters.has_dense_view());
    let wire = wire(&component);
    let input_wire = wire["variables"]
        .as_array()
        .unwrap()
        .iter()
        .find(|v| v["name"] == "u")
        .unwrap();
    let ingredients = &input_wire["effective_input_default"];
    assert_eq!(
        ingredients["values"]["values"]["runs"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert_eq!(
        ingredients["fallback_minimum"]["values"]["runs"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert_eq!(ingredients["origins"].as_array().unwrap().len(), 1);
    let replayed = replay(wire).unwrap();
    assert!(variable(&replayed, "u").declaration_start().is_missing());
    assert!(variable(&replayed, "u").instantiation_start().is_repeat());
    assert!(!replayed.runtime_view().model().parameters.has_dense_view());
    replayed.into_codegen_view().try_c().unwrap();
}

fn wire(component: &FmiComponent) -> serde_json::Value {
    let root = component.runtime_view().retain_assertion_root();
    serde_json::to_value(rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root).unwrap())
        .unwrap()
}

fn replay(wire: serde_json::Value) -> Result<FmiComponent, serde_json::Error> {
    rumoca_eval_solve::fmi_wire::deserialize_fmi_component(wire.into_deserializer())
}

fn replace_scalar_real_default_values(default: &mut serde_json::Value, value: f64) {
    let Start::Real(original) = serde_json::from_value(default["values"].clone())
        .expect("the original default uses the current checked start wire")
    else {
        panic!("the fixture must have a Real default")
    };
    assert_eq!(original.len(), 1, "the fixture mutates one scalar default");
    default["values"] = serde_json::to_value(Start::Real(
        SolveInitialValues::repeat(value, 1).expect("valid replacement value inventory"),
    ))
    .expect("serialize through the canonical checked start owner");
}

#[test]
fn fmi_export_defaults_wire_rejects_foreign_incomplete_and_inconsistent_facts() {
    let component = export_with_options(
        "model Wire input Real u(min=2); Real x(start=0,fixed=true); equation der(x)=u; end Wire;",
        "Wire",
        &SimOptions::default(),
    );
    let original = wire(&component);
    let input = original["variables"]
        .as_array()
        .unwrap()
        .iter()
        .position(|v| v["name"] == "u")
        .unwrap();
    type Mutation = (fn(&mut serde_json::Value), &'static str);
    let mutations: [Mutation; 3] = [
        (
            |v: &mut serde_json::Value| v["declaration"] = serde_json::json!(999),
            "default identity, kind or extent mismatch",
        ),
        (
            |v: &mut serde_json::Value| v["origins"][0]["start"] = serde_json::json!(1),
            "default origins are not a complete ordered partition",
        ),
        (
            |v: &mut serde_json::Value| replace_scalar_real_default_values(v, 3.0),
            "invalid language fallback witness",
        ),
    ];
    for (mutate, expected) in mutations {
        let mut bad = original.clone();
        mutate(&mut bad["variables"][input]["effective_input_default"]);
        let error = replay(bad).expect_err("invalid default ingredients must be refused");
        assert!(error.to_string().contains(expected), "{error}");
    }
    let mut old = original.clone();
    old["schema_version"] = serde_json::json!(6);
    assert!(replay(old).is_err());
    let mut host_only = original;
    host_only["variables"][input]["effective_input_default"] = serde_json::Value::Null;
    let component = replay(host_only).unwrap();
    assert!(variable(&component, "u").declaration_start().is_missing());
    assert!(
        component.into_codegen_view().try_c().is_err(),
        "reservation cannot authorize publication"
    );
}

#[test]
fn fmi_export_defaults_storage_agreement_preserves_host_and_nonpublic_cases() {
    let component = export_with_options(
        "model Agreement input Real u(start=2); Real x(start=0,fixed=true); equation der(x)=u; end Agreement;",
        "Agreement",
        &SimOptions {
            initial_inputs: vec![("u".into(), 3.0)],
            ..SimOptions::default()
        },
    );
    let original = wire(&component);
    component.into_codegen_view().try_c().unwrap();
    let input = original["variables"]
        .as_array()
        .unwrap()
        .iter()
        .position(|v| v["name"] == "u")
        .unwrap();
    let mut host = original.clone();
    host["variables"][input]["effective_input_default"] = serde_json::Value::Null;
    let host = replay(host).expect("host source start may differ from actual supplied storage");
    assert!(
        matches!(variable(&host, "u").declaration_start(), Start::Real(v) if v.value(0) == Some(2.0))
    );
    assert!(host.into_codegen_view().try_c().is_err());

    let mut nonpublic = original.clone();
    nonpublic["variables"][input]["causality"] = serde_json::json!("local");
    nonpublic["variables"][input]["declared_causality"] = serde_json::json!("input");
    replay(nonpublic.clone()).expect("a local declared input keeps its issued default");
    for mut inconsistent in [original, nonpublic] {
        replace_scalar_real_default_values(
            &mut inconsistent["variables"][input]["effective_input_default"],
            4.0,
        );
        let error = replay(inconsistent).expect_err("issued default must match actual storage");
        assert!(
            error
                .to_string()
                .contains("default disagrees with original instantiation storage"),
            "{error}"
        );
    }
}

#[test]
fn fmi_export_defaults_keep_wide_integer_exact_and_target_refusal_explicit() {
    let component = export_with_options(
        "model Wide input Integer u(start=9007199254740993); Real x(start=0,fixed=true); equation der(x)=0; end Wide;",
        "Wide",
        &SimOptions::default(),
    );
    for start in [
        variable(&component, "u").declaration_start(),
        variable(&component, "u").instantiation_start(),
    ] {
        let Start::Integer(values) = start else {
            panic!("exact Integer")
        };
        assert_eq!(values.value(0), Some(&9_007_199_254_740_993));
    }
    let replayed = replay(wire(&component)).unwrap();
    let Start::Integer(values) = variable(&replayed, "u").instantiation_start() else {
        panic!("exact Integer")
    };
    assert_eq!(values.value(0), Some(&9_007_199_254_740_993));
    let mut bad_bound = wire(&component);
    let input = bad_bound["variables"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|v| v["name"] == "u")
        .unwrap();
    let default = &mut input["effective_input_default"];
    default["fallback_minimum"] = default["values"].clone();
    default["fallback_minimum_span"] = default["origins"][0]["span"].clone();
    input["minimum"] = serde_json::to_value(
        rumoca_ir_solve::SolveInitialValues::repeat(9_007_199_254_740_993_i64 as f64, 1).unwrap(),
    )
    .unwrap();
    assert!(
        replay(bad_bound).is_err(),
        "rounded Integer bound witness is unsupported"
    );
    assert!(
        component.into_codegen_view().try_c().is_err(),
        "current C profile cannot represent this Integer"
    );
}

#[test]
fn fmi_export_defaults_do_not_issue_invalid_bounds_or_headless_drivers() {
    let absent = compile(
        "model Absent input Real u; Real x(start=0,fixed=true); equation der(x)=u; end Absent;",
        "Absent",
    );
    assert!(lower_dae_for_simulation(&absent, &SimOptions::default()).is_err());
    let host = super::super::fmi::lower_fmi_component(&absent).unwrap();
    assert!(variable(&host, "u").declaration_start().is_missing());
    assert!(variable(&host, "u").effective_input_default().is_none());
    assert!(host.into_codegen_view().try_c().is_err());
    let exported = super::super::fmi::lower_fmi_export_component(&absent).unwrap();
    assert!(variable(&exported, "u").declaration_start().is_missing());
    assert!(
        matches!(variable(&exported, "u").instantiation_start(), Start::Real(v) if v.value(0) == Some(0.0))
    );
    exported.into_codegen_view().try_c().unwrap();
    for bounds in ["min=3,max=2", "min=-1e999,max=1e999"] {
        let source = format!(
            "model Bad input Real u({bounds}); Real x(start=0,fixed=true); equation der(x)=u; end Bad;"
        );
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("invalid_input_default.mo", &source)
            .unwrap();
        match session.compile_model("Bad") {
            Ok(compiled) => {
                assert!(super::super::fmi::lower_fmi_export_component(&compiled.dae).is_err())
            }
            Err(_) => assert!(
                bounds.contains("1e999"),
                "finite source bounds should reach export selection"
            ),
        }
    }
}

#[test]
fn fmi_export_defaults_incomplete_public_input_does_not_hide_later_invalid_default() {
    let component = export_with_options(
        "model LaterDefault input Real a(min=2); input Real b(start=3); Real x(start=0,fixed=true); equation der(x)=a+b; end LaterDefault;",
        "LaterDefault",
        &SimOptions {
            initial_inputs: vec![("b".into(), 5.0)],
            ..SimOptions::default()
        },
    );
    let mut host = wire(&component);
    component.into_codegen_view().try_c().unwrap();
    let variables = host["variables"].as_array().unwrap();
    let first = variables.iter().position(|v| v["name"] == "a").unwrap();
    let later = variables.iter().position(|v| v["name"] == "b").unwrap();
    assert!(
        first < later,
        "the incomplete input must precede the invalid default"
    );
    host["variables"][first]["effective_input_default"] = serde_json::Value::Null;
    let valid = replay(host.clone()).expect("incomplete host input alone remains valid");
    assert!(variable(&valid, "a").instantiation_start().is_missing());
    assert!(valid.into_codegen_view().try_c().is_err());

    replace_scalar_real_default_values(
        &mut host["variables"][later]["effective_input_default"],
        6.0,
    );
    let error = replay(host).expect_err("an earlier incomplete input cannot hide invalid storage");
    assert!(
        error
            .to_string()
            .contains("default disagrees with original instantiation storage"),
        "{error}"
    );
}
