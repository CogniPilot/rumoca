//! A scalar WIT linking fixture, not an admitted FMI implementation.

use super::*;
use crate::component_build::interface::ExpectedInterface;
use rumoca_core::artifact_build::PreparedBuildInventory;
use std::collections::BTreeMap;

const WORLD: &str =
    "package rumoca:linkage; world native { export calculate: func(value: f64) -> f64; }";

fn inventory(world: &str) -> PreparedBuildInventory<()> {
    PreparedBuildInventory::construct(
        vec![("wit/linkage.wit".into(), world.as_bytes().to_vec())],
        (),
        BTreeMap::from([(
            "structural".into(),
            BTreeMap::from([
                ("wit-directory".into(), "wit".into()),
                ("world".into(), "native".into()),
            ]),
        )]),
    )
    .unwrap()
}

fn scalar_runtime() -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
        ],
        [ValType::I32],
    );
    types.ty().function([ValType::F64], [ValType::F64]);
    module.section(&types);
    let mut imports = ImportSection::new();
    let mut public_memory = memory(false);
    public_memory.maximum = None;
    imports.import("env", "memory", EntityType::Memory(public_memory));
    // Linker resolves ordinary library symbols through env, not WIT imports.
    imports.import("env", "eval_assignments", EntityType::Function(0));
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(1);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("calculate", ExportKind::Func, 1);
    module.section(&exports);
    let mut function = Function::new([]);
    for instruction in [
        Instruction::I32Const(0),
        Instruction::I32Const(8),
        Instruction::LocalGet(0),
        Instruction::I32Const(16),
        Instruction::I32Const(0),
        Instruction::Call(0),
        Instruction::If(wasm_encoder::BlockType::Empty),
        Instruction::Unreachable,
        Instruction::End,
        Instruction::I32Const(0),
        Instruction::F64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }),
        Instruction::End,
    ] {
        function.instruction(&instruction);
    }
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    let mut bytes = module.finish();
    let mut resolve = wit_parser::Resolve::default();
    let package = resolve.push_str("linkage.wit", WORLD).unwrap();
    let world = resolve.select_world(&[package], Some("native")).unwrap();
    wit_component::embed_component_metadata(
        &mut bytes,
        &resolve,
        world,
        wit_component::StringEncoding::UTF8,
    )
    .unwrap();
    bytes
}

fn linked(include_model: bool) -> Result<Vec<u8>, String> {
    let mut linker = wit_component::Linker::default()
        .validate(true)
        .stub_missing_functions(false)
        .use_built_in_libdl(false)
        .library("math", &support_without_memory(), false)
        .map_err(|error| error.to_string())?;
    if include_model {
        linker = linker
            .library("model", &model_with_unbounded_public_memory(), false)
            .map_err(|error| error.to_string())?;
    }
    linker
        .library("runtime", &scalar_runtime(), false)
        .and_then(wit_component::Linker::encode)
        .map_err(|error| format!("link actual WIT libraries: {error:#}"))
}

#[test]
fn public_linker_preserves_actual_scalar_wit_world_without_stubs() {
    let bytes = linked(true).unwrap();
    let inventory = inventory(WORLD);
    ExpectedInterface::from_request(&inventory.request("structural").unwrap())
        .unwrap()
        .check(&bytes)
        .unwrap();
    export_fixture_if_requested(&bytes);
}

#[test]
fn public_linker_refuses_missing_model_and_foreign_world() {
    assert!(linked(false).is_err());
    let bytes = linked(true).unwrap();
    let inventory = inventory(
        "package rumoca:foreign; world native { export calculate: func(value: f64) -> u32; }",
    );
    assert!(
        ExpectedInterface::from_request(&inventory.request("structural").unwrap())
            .unwrap()
            .check(&bytes)
            .is_err()
    );
}

fn export_fixture_if_requested(bytes: &[u8]) {
    use std::io::Write;
    let config = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/verification/component-linkage.json");
    let text = match std::fs::read_to_string(config) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return,
        Err(error) => panic!("read component linkage fixture config: {error}"),
    };
    let config: serde_json::Value = serde_json::from_str(&text).unwrap();
    let output = std::path::PathBuf::from(config["output"].as_str().unwrap());
    let scratch = std::path::PathBuf::from(std::env::var_os("HOME").unwrap()).join("scratch");
    assert!(
        output.is_absolute() && output.starts_with(&scratch),
        "fixture export requires owned HOME scratch path"
    );
    let source = config["reportedSource"]
        .as_str()
        .expect("export requires source receipt SHA");
    assert!(source.len() == 40 && source.bytes().all(|byte| byte.is_ascii_hexdigit()));
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&output)
        .unwrap();
    file.write_all(bytes).unwrap();
    file.sync_all().unwrap();
    let metadata = serde_json::json!({
        "scope": "scalar-linkage-fixture-only",
        "reportedSource": source,
        "sourceBinding": "central recipe must verify issuer/source/.d; caller value alone grants no proof",
        "bytes": bytes.len(),
        "digestBinding": "central evidence must retain SHA256 of these exact exported bytes",
        "world": WORLD,
    });
    let mut sidecar = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(output.with_extension("json"))
        .unwrap();
    serde_json::to_writer_pretty(&mut sidecar, &metadata).unwrap();
    sidecar.write_all(b"\n").unwrap();
    sidecar.sync_all().unwrap();
    println!(
        "exported scalar linkage fixture: {} ({} bytes)",
        output.display(),
        bytes.len()
    );
}
