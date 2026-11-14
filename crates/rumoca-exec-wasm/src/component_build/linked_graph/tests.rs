//! The scalar-linked graph's shape in memory: source data slots, a linked
//! kernel module with its own private arena memory, the dispatch over it and
//! a runtime that reaches the kernel only through the dispatch. Nothing here
//! builds the fixed guest; the shapes follow the issued linked profile.

use super::link_single_provider_graph;
use crate::component_build::check_embedded_core_modules;
use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemorySection, MemoryType, Module, TypeSection, ValType,
};

const KERNEL: &str = "rumoca_k_exact_0";

fn memory(minimum: u64) -> MemoryType {
    MemoryType {
        minimum,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    }
}

/// `(a0 a1 t a3 a4) -> i32`: the five-argument entry convention.
fn kernel_type() -> (Vec<ValType>, Vec<ValType>) {
    (
        vec![
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
        ],
        vec![ValType::I32],
    )
}

/// `rumoca_dispatch`: kind, index, four key words, `a0 a1 t a3 .. a8`.
fn dispatch_parameters() -> Vec<ValType> {
    [ValType::I32, ValType::I32]
        .into_iter()
        .chain([ValType::I64; 4])
        .chain([ValType::I32, ValType::I32, ValType::F64])
        .chain([ValType::I32; 6])
        .collect()
}

fn finish(
    mut module: Module,
    types: TypeSection,
    imports: ImportSection,
    functions: FunctionSection,
    memories: Option<MemorySection>,
    exports: ExportSection,
    code: CodeSection,
) -> Vec<u8> {
    module.section(&types);
    module.section(&imports);
    module.section(&functions);
    if let Some(memories) = memories {
        module.section(&memories);
    }
    module.section(&exports);
    module.section(&code);
    module.finish()
}

/// A linked kernel: imports the shared memory, defines its private arena,
/// exports `name` and returns status 0.
fn kernel(name: &str) -> Vec<u8> {
    let mut types = TypeSection::new();
    let (parameters, results) = kernel_type();
    types.ty().function(parameters, results);
    let mut imports = ImportSection::new();
    imports.import("env", "memory", EntityType::Memory(memory(0)));
    let mut functions = FunctionSection::new();
    functions.function(0);
    let mut memories = MemorySection::new();
    memories.memory(memory(1));
    let mut exports = ExportSection::new();
    exports.export(name, ExportKind::Func, 0);
    let mut code = CodeSection::new();
    let mut body = Function::new([]);
    body.instruction(&Instruction::I32Const(0));
    body.instruction(&Instruction::End);
    code.function(&body);
    finish(
        Module::new(),
        types,
        imports,
        functions,
        Some(memories),
        exports,
        code,
    )
}

/// A dispatch over the one kernel: no memory, forwards `a0 a1 t a3 a4`.
fn dispatch(export: &str) -> Vec<u8> {
    let mut types = TypeSection::new();
    let (parameters, results) = kernel_type();
    types.ty().function(parameters, results);
    types.ty().function(dispatch_parameters(), [ValType::I32]);
    let mut imports = ImportSection::new();
    imports.import("env", KERNEL, EntityType::Function(0));
    let mut functions = FunctionSection::new();
    functions.function(1);
    let mut exports = ExportSection::new();
    exports.export(export, ExportKind::Func, 1);
    let mut code = CodeSection::new();
    let mut body = Function::new([]);
    for argument in 6..11 {
        body.instruction(&Instruction::LocalGet(argument));
    }
    body.instruction(&Instruction::Call(0));
    body.instruction(&Instruction::End);
    code.function(&body);
    finish(
        Module::new(),
        types,
        imports,
        functions,
        None,
        exports,
        code,
    )
}

/// Data-library-shaped slots: `<slot>_len` exports returning a constant.
fn data(slots: &[&str]) -> Vec<u8> {
    let mut types = TypeSection::new();
    types.ty().function([], [ValType::I32]);
    let mut functions = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut code = CodeSection::new();
    for (index, slot) in slots.iter().enumerate() {
        functions.function(0);
        exports.export(&format!("{slot}_len"), ExportKind::Func, index as u32);
        let mut body = Function::new([]);
        body.instruction(&Instruction::I32Const(1));
        body.instruction(&Instruction::End);
        code.function(&body);
    }
    finish(
        Module::new(),
        types,
        ImportSection::new(),
        functions,
        None,
        exports,
        code,
    )
}

/// A runtime exporting the world function `run`, which reads both data slots
/// and calls the dispatch; it imports the dispatch and never a kernel.
fn runtime() -> Vec<u8> {
    let mut types = TypeSection::new();
    types.ty().function([], [ValType::I32]);
    types.ty().function(dispatch_parameters(), [ValType::I32]);
    let mut imports = ImportSection::new();
    imports.import("env", "memory", EntityType::Memory(memory(0)));
    imports.import("env", "rumoca_saved_product_len", EntityType::Function(0));
    imports.import("env", "rumoca_linked_artifact_len", EntityType::Function(0));
    imports.import(
        "env",
        crate::LINKED_DISPATCH_EXPORT,
        EntityType::Function(1),
    );
    let mut functions = FunctionSection::new();
    functions.function(0);
    let mut exports = ExportSection::new();
    exports.export("run", ExportKind::Func, 3);
    let mut code = CodeSection::new();
    // Locals start at zero, so the f64 argument needs no constant encoding.
    let mut body = Function::new([(1, ValType::I64), (1, ValType::F64)]);
    body.instruction(&Instruction::Call(0));
    body.instruction(&Instruction::Call(1));
    body.instruction(&Instruction::I32Add);
    body.instruction(&Instruction::I32Const(3));
    body.instruction(&Instruction::I32Const(0));
    for _ in 0..4 {
        body.instruction(&Instruction::LocalGet(0));
    }
    body.instruction(&Instruction::I32Const(0));
    body.instruction(&Instruction::I32Const(0));
    body.instruction(&Instruction::LocalGet(1));
    for _ in 0..6 {
        body.instruction(&Instruction::I32Const(0));
    }
    body.instruction(&Instruction::Call(2));
    body.instruction(&Instruction::I32Add);
    body.instruction(&Instruction::End);
    code.function(&body);
    let mut bytes = finish(
        Module::new(),
        types,
        imports,
        functions,
        None,
        exports,
        code,
    );
    let mut resolve = wit_parser::Resolve::new();
    let package = resolve
        .push_str(
            "proof.wit",
            "package rumoca:proof;\nworld proof {\n  export run: func() -> u32;\n}\n",
        )
        .unwrap();
    let world = resolve.select_world(&[package], Some("proof")).unwrap();
    wit_component::embed_component_metadata(
        &mut bytes,
        &resolve,
        world,
        wit_component::StringEncoding::UTF8,
    )
    .unwrap();
    bytes
}

fn graph(dispatch_export: &str) -> (Vec<u8>, Vec<u8>, Vec<u8>, Vec<u8>) {
    (
        data(&["rumoca_saved_product", "rumoca_linked_artifact"]),
        kernel(KERNEL),
        dispatch(dispatch_export),
        runtime(),
    )
}

/// The graph links with validation on, no stubs and no libdl, although the
/// kernel defines a memory beside the shared one, and the component embeds
/// the kernel and the dispatch byte for byte.
#[test]
fn a_linked_graph_links_and_embeds_its_kernel_and_dispatch_unchanged() {
    let (data, kernel, dispatch, runtime) = graph(crate::LINKED_DISPATCH_EXPORT);
    let component = link_single_provider_graph(&[
        ("rumoca-source", data.as_slice()),
        ("rumoca-core-0", kernel.as_slice()),
        ("rumoca-dispatch", dispatch.as_slice()),
        ("rumoca-runtime", runtime.as_slice()),
    ])
    .unwrap();
    check_embedded_core_modules(&component, &[kernel.as_slice(), dispatch.as_slice()]).unwrap();
}

/// Without the dispatch, the runtime's import has no provider, and nothing
/// is stubbed in its place.
#[test]
fn a_linked_graph_without_its_dispatch_is_refused() {
    let (data, kernel, _, runtime) = graph(crate::LINKED_DISPATCH_EXPORT);
    assert!(
        link_single_provider_graph(&[
            ("rumoca-source", data.as_slice()),
            ("rumoca-core-0", kernel.as_slice()),
            ("rumoca-runtime", runtime.as_slice()),
        ])
        .is_err()
    );
}

/// A second provider of any symbol is refused before linking, by the names
/// the libraries actually export.
#[test]
fn a_linked_graph_with_two_providers_of_one_symbol_is_refused() {
    let (data, kernel, dispatch, runtime) = graph(crate::LINKED_DISPATCH_EXPORT);
    let second = kernel(KERNEL);
    assert_eq!(
        link_single_provider_graph(&[
            ("rumoca-source", data.as_slice()),
            ("rumoca-core-0", kernel.as_slice()),
            ("rumoca-core-1", second.as_slice()),
            ("rumoca-dispatch", dispatch.as_slice()),
            ("rumoca-runtime", runtime.as_slice()),
        ])
        .unwrap_err(),
        format!("linked symbol {KERNEL} is exported by both rumoca-core-0 and rumoca-core-1")
    );
    let (_, _, renamed, _) = graph("rumoca_saved_product_len");
    assert_eq!(
        link_single_provider_graph(&[
            ("rumoca-source", data.as_slice()),
            ("rumoca-core-0", kernel.as_slice()),
            ("rumoca-dispatch", renamed.as_slice()),
            ("rumoca-runtime", runtime.as_slice()),
        ])
        .unwrap_err(),
        "linked symbol rumoca_saved_product_len is exported by both rumoca-source and \
         rumoca-dispatch"
    );
}
