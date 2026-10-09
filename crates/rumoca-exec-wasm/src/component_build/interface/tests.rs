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
