//! Actual emitter linkage control; this fixture is not a production FMI guest.

use super::*;
use rumoca_exec_wasm::NativeLinkableModelLibrary;
use wasm_encoder::{Encode, EntityType, ExportKind, Instruction, Module, ValType};

mod runtime;

const WORLD: &str = "package rumoca:actual-native; world native { export invoke: func(region: u32, first: bool, sample: f64, k: s64) -> f64; export last-status: func(region: u32) -> u32; export input-sample: func(region: u32) -> f64; export input-integer: func(region: u32) -> s64; export input-boolean: func(region: u32) -> bool; export accepted-result: func(region: u32) -> f64; }";

#[derive(Clone, Copy)]
enum LinkCase {
    Correct,
    MissingEntry,
    WrongEntryType,
    SharedMemory,
}

struct Layout {
    p: usize,
    scratch: usize,
    typed: usize,
    status: usize,
    stride: usize,
    first: usize,
    k: usize,
    sample: usize,
    result: usize,
    parameters: Vec<f64>,
}

impl Layout {
    fn from_library(library: &NativeModelLibrary) -> Self {
        let model = library.inputs().root().model();
        let layout = &model.problem.layout;
        let schedule = library.inputs().schedule();
        assert_eq!(
            layout.y_scalars(),
            1,
            "this original fixture's complete Y is result"
        );
        let p_index = |name: &str| match layout.binding(name).unwrap() {
            rumoca_ir_solve::ScalarSlot::P { index, .. } => index,
            _ => panic!("original fixture input has a P coordinate"),
        };
        let lane = |name| schedule.input_lane(p_index(name)).unwrap().lane_offset();
        let p = layout.y_scalars() * 8;
        let scratch = p + layout.p_scalars() * 8;
        let typed = (scratch + library.compiled().scratch_bytes() as usize).next_multiple_of(8);
        let status =
            (typed + schedule.input_lane_bytes() + schedule.lane_bytes()).next_multiple_of(8);
        assert!(schedule.input_lane_bytes() > 0 && typed > 0);
        Self {
            p,
            scratch,
            typed,
            status,
            stride: (status + 4).next_multiple_of(8),
            first: lane("first"),
            k: lane("k"),
            sample: p_index("samples") * 8,
            result: match layout.binding("result").unwrap() {
                rumoca_ir_solve::ScalarSlot::Y { index, .. } => index * 8,
                _ => panic!("original result has a Y coordinate"),
            },
            parameters: model.parameters.to_vec(),
        }
    }
}

fn linked(target: &NativeLinkableModelLibrary, case: LinkCase) -> Result<Vec<u8>, String> {
    let library = target.original();
    assert!(
        library.compiled().math_imports().is_empty(),
        "this actual fixture requires no math provider"
    );
    let layout = Layout::from_library(library);
    let runtime = runtime::module(&layout, case);
    wit_component::Linker::default()
        .validate(true)
        .stub_missing_functions(false)
        .use_built_in_libdl(false)
        .library("model", target.module_bytes(), false)
        .and_then(|linker| linker.library("runtime", &runtime, false))
        .and_then(wit_component::Linker::encode)
        .map_err(|error| format!("link actual emitted native model: {error:#}"))
}

#[test]
fn native_model_library_actual_emission_links_typed_resource_regions() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let library =
            NativeModelLibrary::from_preparation(preparation(&component), deployment(component))
                .unwrap();
        library.check_linkage_contract().unwrap();
        let target = library.into_linkable().unwrap();
        let bytes = linked(&target, LinkCase::Correct).unwrap();
        let wit_component::DecodedWasm::Component(resolve, world) =
            wit_component::decode(&bytes).unwrap()
        else {
            panic!("actual linkage must be a component")
        };
        assert!(resolve.worlds[world].imports.is_empty());
        assert_eq!(resolve.worlds[world].exports.len(), 6);
        export_if_requested(&bytes, target.original());
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn native_model_library_actual_emission_refuses_foreign_entry_and_memory() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let library =
            NativeModelLibrary::from_preparation(preparation(&component), deployment(component))
                .unwrap();
        let target = library.into_linkable().unwrap();
        // Establish that this SAME actual model can link before assigning any
        // altered receiver's refusal to its intended owning boundary.
        linked(&target, LinkCase::Correct).unwrap();
        for case in [
            LinkCase::MissingEntry,
            LinkCase::WrongEntryType,
            LinkCase::SharedMemory,
        ] {
            let error = linked(&target, case).unwrap_err();
            match case {
                LinkCase::MissingEntry => assert!(
                    error.contains("unresolved symbol(s)")
                        && error.contains("missing_eval_assignments"),
                    "{error}"
                ),
                LinkCase::WrongEntryType => assert!(
                    error.contains("unresolved symbol(s)")
                        && error.contains("eval_assignments")
                        && error.contains("I64"),
                    "{error}"
                ),
                LinkCase::SharedMemory => assert!(
                    error.contains("memory") && error.contains("shared"),
                    "{error}"
                ),
                LinkCase::Correct => unreachable!("only altered cases are negative"),
            }
        }
        target.original().check_linkage_contract().unwrap();
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn native_model_library_linkable_preserves_original_source_and_nonexport_sections() {
    let _lock = session_test_guard();
    with_fixture_component(|component| {
        let library =
            NativeModelLibrary::from_preparation(preparation(&component), deployment(component))
                .unwrap();
        let root = library.inputs().root().clone();
        let original_bytes = library.compiled().module_bytes().to_vec();
        let target = library.into_linkable().unwrap();
        assert!(root.shares_root_owner(target.original().inputs().root()));
        assert!(std::ptr::eq(
            root.metadata(),
            target.original().inputs().root().metadata()
        ));
        assert_eq!(original_bytes, target.original().compiled().module_bytes());
        assert_eq!(
            nonexport_sections(&original_bytes),
            nonexport_sections(target.module_bytes())
        );
        assert_eq!(exports(&original_bytes), ["memory", "eval_assignments"]);
        assert_eq!(exports(target.module_bytes()), ["eval_assignments"]);
        Ok(String::new())
    })
    .unwrap();
}

fn nonexport_sections(bytes: &[u8]) -> Vec<(u8, &[u8])> {
    wasmparser::Parser::new(0)
        .parse_all(bytes)
        .filter_map(|payload| {
            let payload = payload.unwrap();
            if matches!(payload, wasmparser::Payload::ExportSection(_)) {
                return None;
            }
            payload.as_section().map(|(id, range)| (id, &bytes[range]))
        })
        .collect()
}

fn exports(bytes: &[u8]) -> Vec<String> {
    wasmparser::Parser::new(0)
        .parse_all(bytes)
        .flat_map(|payload| match payload.unwrap() {
            wasmparser::Payload::ExportSection(exports) => exports
                .into_iter()
                .map(|export| export.unwrap().name.to_owned())
                .collect(),
            _ => Vec::new(),
        })
        .collect()
}

fn export_if_requested(bytes: &[u8], library: &NativeModelLibrary) {
    use std::io::Write;
    let config = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/verification/native-model-linkage.json");
    let text = match std::fs::read_to_string(config) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return,
        Err(error) => panic!("read native linkage fixture config: {error}"),
    };
    let config: serde_json::Value = serde_json::from_str(&text).unwrap();
    let path = std::path::PathBuf::from(config["output"].as_str().unwrap());
    let scratch = std::path::PathBuf::from(std::env::var_os("HOME").unwrap()).join("scratch");
    assert!(path.is_absolute() && path.starts_with(scratch));
    let mut output = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&path)
        .unwrap();
    output.write_all(bytes).unwrap();
    output.sync_all().unwrap();
    let layout = Layout::from_library(library);
    for fault in library.compiled().faults() {
        fault
            .provenance
            .require_provenance("actual library fault inventory")
            .unwrap();
    }
    let metadata = serde_json::json!({"scope":"actual-native-linkage-control-only", "world":WORLD,
        "componentBytes":bytes.len(), "modelBytes":library.compiled().module_bytes().len(),
        "pOffset":layout.p, "scratchOffset":layout.scratch, "typedOffset":layout.typed,
        "statusOffset":layout.status, "resourceStride":layout.stride, "resources":2,
        "indexBoundsStatuses":library.compiled().faults().iter().filter(|fault| fault.kind == rumoca_exec_wasm::TypedCallFaultKind::IndexBounds).map(|fault| fault.status).collect::<Vec<_>>(),
        "allocator":"actual fixture dylink memory-size/alignment plus imported __memory_base",
        "authority":"no production guest, linked entry, FMI publication or CPU/WASIP2 admission"});
    let mut output = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path.with_extension("json"))
        .unwrap();
    serde_json::to_writer_pretty(&mut output, &metadata).unwrap();
    output.write_all(b"\n").unwrap();
    output.sync_all().unwrap();
}
