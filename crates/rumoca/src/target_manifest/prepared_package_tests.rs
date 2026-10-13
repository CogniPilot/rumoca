//! Prepared packages retain their rendered inventory and issue only their own
//! build requests and build bindings.

use super::*;
use crate::Compiler;

fn decay() -> CompilationResult {
    Compiler::new()
        .model("Decay")
        .compile_str(
            "model Decay input Real gain = 2; output Real y; equation y = gain * time; end Decay;",
            "Decay.mo",
        )
        .unwrap()
}

#[test]
fn a_prepared_component_package_retains_metadata_and_declares_one_build_slot() {
    let prepared = prepare_packaged_target(&decay(), "Decay", "fmi-ls-wasm").unwrap();
    assert_eq!(prepared.build_slots(), ["component"]);
    let request = prepared.build_request("component").unwrap();
    assert!(
        request
            .files()
            .contains_key(Path::new("modelDescription.xml"))
    );
    let error = prepared.build_request("undeclared").err().unwrap();
    assert!(!error.to_string().is_empty());
}

#[test]
fn a_build_binding_is_accepted_only_by_the_inventory_that_issued_it() {
    let result = decay();
    let prepared = prepare_packaged_target(&result, "Decay", "fmi-ls-wasm").unwrap();
    let foreign = prepare_packaged_target(&result, "Decay", "fmi-ls-wasm").unwrap();
    let binding = prepared.build_request("component").unwrap().binding();
    assert!(prepared.accepts_build(&binding));
    assert!(!foreign.accepts_build(&binding));
    assert_eq!(sealed_slot(&prepared, &binding).unwrap(), "component");
    let error = sealed_slot(&foreign, &binding).unwrap_err();
    assert!(error.to_string().contains("foreign prepared inventory"));
}

#[test]
fn packaging_refuses_raw_templates_and_targets_without_a_package() {
    let result = decay();
    let raw = prepare_packaged_target(&result, "Decay", "model.jinja")
        .err()
        .unwrap();
    assert!(raw.to_string().contains("raw template"));
    let unpackaged = prepare_packaged_target(&result, "Decay", "base-modelica")
        .err()
        .unwrap();
    assert!(
        unpackaged
            .to_string()
            .contains("does not declare a package")
    );
}
