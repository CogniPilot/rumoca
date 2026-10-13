//! Real fixed-source core + original checked typed model, before FMU publication.

use super::*;
use rumoca_core::artifact_build::PreparedBuildInventory;
use std::{collections::BTreeMap, path::Path};

mod metadata;

#[test]
fn native_fmi_ls_actual_fixed_guest_links_the_original_typed_component() {
    let _lock = session_test_guard();
    let output = output_directory();
    std::fs::create_dir(output.join("fixed-source")).unwrap();
    std::fs::create_dir(output.join("fixed-target")).unwrap();
    // This is the canonical fixed-source builder, once. No per-model Rust/C
    // generation, arbitrary core bytes/path, staging artifact or raw issuer.
    let core = rumoca_exec_wasm::build_fixed_native_guest_core(
        &output.join("fixed-source"),
        &output.join("fixed-target"),
    )
    .expect("actual fixed guest source must produce its complete core contract");
    check_actual_fixed_core_abi(&core);
    with_fixture_component(|component| {
        let code = preparation(&component);
        let root = component.runtime_view().retain_assertion_root();
        let manifest = metadata::case_manifest(&component, &code);
        let token = manifest["instantiationToken"].as_str().unwrap();
        let xml = metadata::fixture_xml(&component, token);
        let library = NativeModelLibrary::from_preparation(code.clone(), deployment(component))
            .expect("actual emitted code and original deployment share their root");
        let pair = PreparedNativeFmiLsModel::from_library(library).unwrap();
        let inventory = PreparedBuildInventory::construct(
            vec![("modelDescription.xml".into(), xml.as_bytes().to_vec())],
            (),
            BTreeMap::from([("component".into(), BTreeMap::new())]),
        )
        .unwrap();
        let request = inventory.request("component").unwrap();
        let linked = pair
            .link_fixed_guest(core, &request)
            .expect("the real original model/data/fixed guest graph must link first");
        assert!(inventory.accepts(linked.binding(), "component"));
        let retained = linked.original_model().original().inputs().root();
        assert!(retained.shares_root_owner(&root));
        assert!(std::ptr::eq(retained.metadata(), root.metadata()));
        let refusal = missing_source_provider(&linked);
        write_new(
            &output.join("missing-source-provider.log"),
            refusal.as_bytes(),
        );
        let replay_bytes =
            serde_json::to_vec(&rumoca_sim::retained_fmi_component_wire(retained).unwrap())
                .unwrap();
        let mut replay = serde_json::Deserializer::from_slice(&replay_bytes);
        let replayed = rumoca_sim::deserialize_fmi_component(&mut replay).unwrap();
        replay.end().unwrap();
        assert!(matches!(
            NativeModelLibrary::from_preparation(code.clone(), deployment(replayed)),
            Err(RetainedNativeScheduleError::ForeignPreparation)
        ));
        write_new(&output.join("component.wasm"), linked.bytes());
        write_new(&output.join("modelDescription.xml"), xml.as_bytes());
        write_new(
            &output.join("case.json"),
            &serde_json::to_vec_pretty(&manifest).unwrap(),
        );
        write_new(
            &output.join("model-original.wasm"),
            code.compiled().module_bytes(),
        );
        write_new(&output.join("fixed-core.wasm"), linked.fixed_core().bytes());
        write_new(
            &output.join("fixed-core-cargo.jsonl"),
            linked.fixed_core().build_messages().as_bytes(),
        );
        let (source, target) = linked.fixed_core().build_directories();
        println!(
            "actual fixed guest source={} target={}",
            source.display(),
            target.display()
        );
        println!(
            "actual paired component={} bytes={}",
            output.join("component.wasm").display(),
            linked.bytes().len()
        );
        Ok(String::new())
    })
    .unwrap();
}

fn core_exports(bytes: &[u8]) -> Vec<(String, wasmparser::ExternalKind)> {
    wasmparser::Parser::new(0)
        .parse_all(bytes)
        .filter_map(|payload| match payload.unwrap() {
            wasmparser::Payload::ExportSection(exports) => Some(exports),
            _ => None,
        })
        .flatten()
        .map(|export| {
            let export = export.unwrap();
            (export.name.to_string(), export.kind)
        })
        .collect()
}

fn check_actual_fixed_core_abi(core: &rumoca_exec_wasm::FixedNativeGuestCore) {
    for name in [
        "memory",
        "__indirect_function_table",
        "eval_assignments",
        "rumoca_source_capsule_len",
        "rumoca_source_capsule_copy",
        "rumoca_publication_token_len",
        "rumoca_publication_token_copy",
    ] {
        assert!(core.imports().any(|import| import == ("env", name)));
    }
    assert!(!core.imports().any(|(_, name)| name == "cabi_realloc"));
    let exports = core_exports(core.bytes());
    let exports_function = |wanted: &str| {
        exports
            .iter()
            .filter(|(name, _)| name == wanted)
            .inspect(|(_, kind)| assert_eq!(*kind, wasmparser::ExternalKind::Func))
            .count()
            > 0
    };
    let allocator = exports_function("cabi_realloc");
    let canonical_version = exports_function("fmi:fmi3/common@3.0.0#get-version");
    assert!(allocator && canonical_version);
}

fn missing_source_provider(linked: &rumoca_sim::LinkedNativeFmiLsComponent<()>) -> String {
    let result = wit_component::Linker::default()
        .validate(true)
        .stub_missing_functions(false)
        .use_built_in_libdl(false)
        .library(
            "rumoca-model",
            linked.original_model().module_bytes(),
            false,
        )
        .and_then(|linker| linker.library("rumoca-runtime", linked.fixed_core().bytes(), false))
        .and_then(wit_component::Linker::encode);
    let error = match result {
        Ok(_) => panic!("the same actual core cannot resolve omitted source data"),
        Err(error) => format!("{error:#}"),
    };
    assert!(error.contains("unresolved symbol(s)"));
    for name in [
        "rumoca_source_capsule_len",
        "rumoca_source_capsule_copy",
        "rumoca_publication_token_len",
        "rumoca_publication_token_copy",
    ] {
        assert!(
            error.contains(name),
            "the omitted actual data slot must be named: {error}"
        );
    }
    error
}

fn output_directory() -> std::path::PathBuf {
    let config = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/verification/native-fixed-wholegraph.json");
    let text = std::fs::read_to_string(config)
        .expect("wholegraph qualification requires its fixed-path fixture config");
    let config: serde_json::Value = serde_json::from_str(&text).unwrap();
    let output = std::path::PathBuf::from(config["output"].as_str().unwrap());
    let scratch = std::path::PathBuf::from(std::env::var_os("HOME").unwrap())
        .join("scratch")
        .canonicalize()
        .unwrap();
    let output = output
        .canonicalize()
        .expect("ROOT must create its fresh output namespace");
    assert!(output.starts_with(&scratch) && output != scratch);
    assert!(
        output.read_dir().unwrap().next().is_none(),
        "first outcome requires an empty namespace"
    );
    output
}

fn write_new(path: &Path, bytes: &[u8]) {
    use std::io::Write;
    let mut file = std::fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(path)
        .unwrap();
    file.write_all(bytes).unwrap();
    file.sync_all().unwrap();
}
