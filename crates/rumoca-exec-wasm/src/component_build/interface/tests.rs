use super::*;
use rumoca_core::artifact_build::PreparedBuildInventory;
use std::collections::BTreeMap;

fn inventory(extra: &str) -> PreparedBuildInventory<()> {
    let files = [
        (
            "world.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/world.wit"
            ),
        ),
        (
            "types.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/fmi3-types.wit"
            ),
        ),
        (
            "callbacks.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/fmi3-callbacks.wit"
            ),
        ),
        (
            "common.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/fmi3-common.wit"
            ),
        ),
        (
            "cs.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/fmi3-co-simulation.wit"
            ),
        ),
        (
            "me.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/fmi3-model-exchange.wit"
            ),
        ),
        (
            "se.wit",
            include_str!(
                "../../../../rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/fmi3-scheduled-execution.wit"
            ),
        ),
    ];
    PreparedBuildInventory::construct(
        files
            .into_iter()
            .map(|(name, bytes)| (Path::new("wit").join(name), bytes.as_bytes().to_vec()))
            .collect(),
        (),
        BTreeMap::from([(
            "component".into(),
            BTreeMap::from([
                ("wit-directory".into(), "wit".into()),
                ("world".into(), "co-simulation-fmu".into()),
                ("allowed-extra-imports".into(), extra.into()),
            ]),
        )]),
    )
    .unwrap()
}

fn expected(inventory: &PreparedBuildInventory<()>) -> ExpectedInterface {
    ExpectedInterface::from_request(&inventory.request("component").unwrap()).unwrap()
}

fn component(resolve: &Resolve, world: WorldId) -> Vec<u8> {
    let mut module =
        wit_component::dummy_module(resolve, world, wit_parser::ManglingAndAbi::Standard32);
    wit_component::embed_component_metadata(
        &mut module,
        resolve,
        world,
        wit_component::StringEncoding::UTF8,
    )
    .unwrap();
    wit_component::ComponentEncoder::default()
        .module(&module)
        .unwrap()
        .validate(true)
        .encode()
        .unwrap()
}

#[test]
fn retained_fmi_world_accepts_an_actual_conforming_component() {
    let inventory = inventory("[]");
    let expected = expected(&inventory);
    let bytes = component(&expected.resolve, expected.world);
    expected.check(&bytes).unwrap();
}

#[test]
fn actual_valid_unrelated_world_and_core_module_are_refused() {
    let inventory = inventory("[]");
    let mut resolve = Resolve::default();
    let package = resolve
        .push_str(
            "unrelated.wit",
            "package other:product; world unrelated { export run: func(); }",
        )
        .unwrap();
    let world = resolve.select_world(&[package], None).unwrap();
    let bytes = component(&resolve, world);
    assert!(expected(&inventory).check(&bytes).is_err());
    assert!(
        expected(&inventory)
            .check(&wasm_encoder::Module::new().finish())
            .is_err()
    );
}

#[test]
fn actual_valid_component_missing_cs_or_with_wrong_signature_is_refused() {
    let inventory = inventory("[]");
    let contract = expected(&inventory);
    let mut missing = contract.resolve.clone();
    let cs = missing.worlds[contract.world]
        .exports
        .keys()
        .find(|key| missing.name_world_key(key) == "fmi:fmi3/co-simulation@3.0.0")
        .unwrap()
        .clone();
    missing.worlds[contract.world].exports.swap_remove(&cs);
    assert!(
        expected(&inventory)
            .check(&component(&missing, contract.world))
            .is_err()
    );
    let mut wrong = contract.resolve.clone();
    let common = wrong.worlds[contract.world]
        .exports
        .values()
        .find_map(|item| match item {
            WorldItem::Interface { id, .. }
                if wrong.id_of(*id).as_deref() == Some("fmi:fmi3/common@3.0.0") =>
            {
                Some(*id)
            }
            _ => None,
        })
        .unwrap();
    wrong.interfaces[common].functions["get-version"].result = Some(wit_parser::Type::U32);
    assert!(
        expected(&inventory)
            .check(&component(&wrong, contract.world))
            .is_err()
    );
}

#[test]
fn extra_imports_require_exact_target_declaration_and_version() {
    let undeclared = inventory("[]");
    let mut contract = expected(&undeclared);
    let package = contract.resolve.push_str("host.wit", "package host:runtime@1.0.0; interface io { read: func() -> u32; } world runtime { import io; }").unwrap();
    let host_world = contract.resolve.select_world(&[package], None).unwrap();
    let imports = contract.resolve.worlds[host_world].imports.clone();
    contract.resolve.worlds[contract.world]
        .imports
        .extend(imports);
    let bytes = component(&contract.resolve, contract.world);
    assert!(expected(&undeclared).check(&bytes).is_err());
    expected(&inventory("[\"host:runtime/io@1.0.0\"]"))
        .check(&bytes)
        .unwrap();
    assert!(
        expected(&inventory("[\"host:runtime/io@2.0.0\"]"))
            .check(&bytes)
            .is_err()
    );
}

#[test]
fn contract_preflight_refuses_missing_world_duplicate_or_replaced_imports() {
    for extra in [
        "not json",
        "[\"\"]",
        "[\"same\",\"same\"]",
        "[\"fmi:fmi3/callbacks@3.0.0\"]",
    ] {
        let inventory = inventory(extra);
        assert!(ExpectedInterface::from_request(&inventory.request("component").unwrap()).is_err());
    }
    let inventory = PreparedBuildInventory::construct(
        vec![],
        (),
        BTreeMap::from([("component".into(), BTreeMap::new())]),
    )
    .unwrap();
    assert!(ExpectedInterface::from_request(&inventory.request("component").unwrap()).is_err());
}

#[cfg(feature = "fixed-native-guest-build")]
fn fixed_source_platform_bindings(
    newest: &str,
    interface: &str,
    result: &str,
) -> wit_component::metadata::Bindgen {
    let mut resolve = Resolve::default();
    let old = resolve
        .push_str(
            "old.wit",
            "package wasi:cli@0.2.0; interface environment { get-environment: func() -> u32; unused: func(); } world old { import environment; }",
        )
        .unwrap();
    let world = resolve.select_world(&[old], None).unwrap();
    let new = resolve
        .push_str(
            "new.wit",
            &format!("package wasi:cli@{newest}; interface {interface} {{ get-environment: func() -> {result}; unused: func(); }} world new {{ import {interface}; }}"),
        )
        .unwrap();
    let new_world = resolve.select_world(&[new], None).unwrap();
    let imports = resolve.worlds[new_world].imports.clone();
    resolve.worlds[world].imports.extend(imports);
    let metadata =
        wit_component::metadata::encode(&resolve, world, wit_component::StringEncoding::UTF8, None)
            .unwrap();
    let mut module = wasm_encoder::Module::new();
    let mut types = wasm_encoder::TypeSection::new();
    types.ty().function([], [wasm_encoder::ValType::I32]);
    module.section(&types);
    let mut imports = wasm_encoder::ImportSection::new();
    imports.import(
        "wasi:cli/environment@0.2.0",
        "get-environment",
        wasm_encoder::EntityType::Function(0),
    );
    module.section(&imports);
    module.section(&wasm_encoder::CustomSection {
        name: "component-type".into(),
        data: metadata.into(),
    });
    wit_component::metadata::decode(&module.finish()).unwrap().1
}

#[cfg(feature = "fixed-native-guest-build")]
#[test]
fn fixed_source_semver_imports_preserve_exact_interface_schema() {
    let inventory = inventory("[]");
    let source = fixed_source_platform_bindings("0.2.4", "environment", "u32");
    let contract = expected(&inventory).with_source_imports(&source).unwrap();
    let names: BTreeSet<_> = contract.resolve.worlds[contract.world]
        .imports
        .keys()
        .map(|key| contract.resolve.name_world_key(key))
        .collect();
    assert!(names.contains("wasi:cli/environment@0.2.4"));
    assert!(!names.contains("wasi:cli/environment@0.2.0"));
    let bytes = component(&contract.resolve, contract.world);
    expected(&inventory)
        .with_source_imports(&source)
        .unwrap()
        .check(&bytes)
        .unwrap();
    for (version, interface) in [("0.3.0", "environment"), ("0.2.4", "foreign")] {
        let altered = fixed_source_platform_bindings(version, interface, "u32");
        let candidate = expected(&inventory).with_source_imports(&altered).unwrap();
        assert!(
            expected(&inventory)
                .with_source_imports(&source)
                .unwrap()
                .check(&component(&candidate.resolve, candidate.world))
                .is_err()
        );
    }
    let mut wrong = contract.resolve.clone();
    let environment = wrong.worlds[contract.world]
        .imports
        .values()
        .find_map(|item| match item {
            WorldItem::Interface { id, .. }
                if wrong.id_of(*id).as_deref() == Some("wasi:cli/environment@0.2.4") =>
            {
                Some(*id)
            }
            _ => None,
        })
        .unwrap();
    wrong.interfaces[environment].functions["get-environment"].result = Some(wit_parser::Type::U64);
    assert!(
        expected(&inventory)
            .with_source_imports(&source)
            .unwrap()
            .check(&component(&wrong, contract.world))
            .is_err()
    );
    let mut wrong = contract.resolve.clone();
    let extra = wrong
        .push_str("extra.wit", "package other:provider; interface extra { read: func(); } world extra-world { import extra; }")
        .unwrap();
    let extra_world = wrong.select_world(&[extra], None).unwrap();
    let extra_imports = wrong.worlds[extra_world].imports.clone();
    wrong.worlds[contract.world].imports.extend(extra_imports);
    assert!(
        expected(&inventory)
            .with_source_imports(&source)
            .unwrap()
            .check(&component(&wrong, contract.world))
            .is_err()
    );
}

#[cfg(feature = "fixed-native-guest-build")]
#[test]
fn source_metadata_unused_imports_and_functions_are_not_required_at_runtime() {
    let inventory = inventory("[]");
    let source = fixed_source_platform_bindings("0.2.4", "environment", "u32");
    let contract = expected(&inventory).with_source_imports(&source).unwrap();
    let key = contract.resolve.worlds[contract.world]
        .imports
        .keys()
        .find(|key| contract.resolve.name_world_key(key) == "wasi:cli/environment@0.2.4")
        .unwrap()
        .clone();
    let WorldKey::Interface(environment) = &key else {
        panic!("source platform import must retain its interface identity");
    };
    let mut omitted = contract.resolve.clone();
    omitted.worlds[contract.world].imports.swap_remove(&key);
    expected(&inventory)
        .with_source_imports(&source)
        .unwrap()
        .check(&component(&omitted, contract.world))
        .unwrap();
    let mut subset = contract.resolve.clone();
    assert!(
        subset.interfaces[*environment]
            .functions
            .swap_remove("unused")
            .is_some()
    );
    expected(&inventory)
        .with_source_imports(&source)
        .unwrap()
        .check(&component(&subset, contract.world))
        .unwrap();
}
