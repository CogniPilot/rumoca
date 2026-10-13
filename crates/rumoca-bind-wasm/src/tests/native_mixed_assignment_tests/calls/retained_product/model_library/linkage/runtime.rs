//! Instruction-bearing test adapter over the actual emitter ABI.

use super::*;
use std::borrow::Cow;
use wasm_encoder::{
    BlockType, CodeSection, CustomSection, Function, FunctionSection, GlobalType, ImportSection,
    MemArg, MemoryType, TypeSection,
};

pub(super) fn module(layout: &Layout, case: LinkCase) -> Vec<u8> {
    let mut module = Module::new();
    module.section(&storage_allocation(layout));
    module.section(&types(case));
    module.section(&imports(case));
    let mut functions = FunctionSection::new();
    for index in [1, 2, 3, 4, 2, 3] {
        functions.function(index);
    }
    module.section(&functions);
    let mut exports = wasm_encoder::ExportSection::new();
    for (index, name) in [
        "invoke",
        "last-status",
        "input-sample",
        "input-integer",
        "input-boolean",
        "accepted-result",
    ]
    .iter()
    .enumerate()
    {
        exports.export(name, ExportKind::Func, index as u32 + 1);
    }
    module.section(&exports);
    let mut code = CodeSection::new();
    code.function(&invoke(layout, case));
    code.function(&read_cell(
        layout,
        Instruction::I32Load(memory(layout.status, 2)),
    ));
    code.function(&read_cell(
        layout,
        Instruction::F64Load(memory(layout.p + layout.sample, 3)),
    ));
    code.function(&read_cell(
        layout,
        Instruction::I64Load(memory(layout.typed + layout.k, 3)),
    ));
    code.function(&read_cell(
        layout,
        Instruction::I32Load8U(memory(layout.typed + layout.first, 0)),
    ));
    code.function(&read_cell(
        layout,
        Instruction::F64Load(memory(layout.result, 3)),
    ));
    module.section(&code);
    let mut bytes = module.finish();
    let mut resolve = wit_parser::Resolve::default();
    let package = resolve.push_str("actual-native.wit", WORLD).unwrap();
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

fn types(case: LinkCase) -> TypeSection {
    let mut types = TypeSection::new();
    let status = if matches!(case, LinkCase::WrongEntryType) {
        ValType::I64
    } else {
        ValType::I32
    };
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
        ],
        [status],
    );
    types.ty().function(
        [ValType::I32, ValType::I32, ValType::F64, ValType::I64],
        [ValType::F64],
    );
    types.ty().function([ValType::I32], [ValType::I32]);
    types.ty().function([ValType::I32], [ValType::F64]);
    types.ty().function([ValType::I32], [ValType::I64]);
    types
}

fn imports(case: LinkCase) -> ImportSection {
    let mut imports = ImportSection::new();
    let shared = matches!(case, LinkCase::SharedMemory);
    imports.import(
        "env",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: shared.then_some(1),
            memory64: false,
            shared,
            page_size_log2: None,
        }),
    );
    imports.import(
        "env",
        "__memory_base",
        EntityType::Global(GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        }),
    );
    let entry = if matches!(case, LinkCase::MissingEntry) {
        "missing_eval_assignments"
    } else {
        "eval_assignments"
    };
    imports.import("env", entry, EntityType::Function(0));
    imports
}

/// Declare the actual two-region public footprint to the pinned linker.
/// Dylink MemInfo integers use the same standard unsigned LEB encoder.
fn storage_allocation(layout: &Layout) -> CustomSection<'static> {
    let mut info = Vec::new();
    u32::try_from(2 * layout.stride).unwrap().encode(&mut info);
    3_u32.encode(&mut info); // 8-byte alignment exponent.
    0_u32.encode(&mut info); // No table allocation.
    0_u32.encode(&mut info);
    let mut data = vec![1]; // Dylink0 MemInfo subsection.
    u32::try_from(info.len()).unwrap().encode(&mut data);
    data.extend(info);
    CustomSection {
        name: Cow::Borrowed("dylink.0"),
        data: Cow::Owned(data),
    }
}

fn base(function: &mut Function, layout: &Layout) {
    for instruction in [
        Instruction::LocalGet(0),
        Instruction::I32Const(2),
        Instruction::I32GeU,
        Instruction::If(BlockType::Empty),
        Instruction::Unreachable,
        Instruction::End,
        Instruction::GlobalGet(0),
        Instruction::LocalGet(0),
        Instruction::I32Const(i32::try_from(layout.stride).unwrap()),
        Instruction::I32Mul,
        Instruction::I32Add,
    ] {
        function.instruction(&instruction);
    }
}

fn address(function: &mut Function, offset: usize) {
    function.instruction(&Instruction::LocalGet(4));
    function.instruction(&Instruction::I32Const(i32::try_from(offset).unwrap()));
    function.instruction(&Instruction::I32Add);
}

fn memory(offset: usize, align: u32) -> MemArg {
    MemArg {
        offset: offset as u64,
        align,
        memory_index: 0,
    }
}

fn invoke(layout: &Layout, case: LinkCase) -> Function {
    let mut f = Function::new([(1, ValType::I32)]);
    base(&mut f, layout);
    f.instruction(&Instruction::LocalSet(4));
    for (index, value) in layout.parameters.iter().enumerate() {
        f.instruction(&Instruction::LocalGet(4));
        f.instruction(&Instruction::F64Const((*value).into()));
        f.instruction(&Instruction::F64Store(memory(layout.p + index * 8, 3)));
    }
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::F64Store(memory(layout.p + layout.sample, 3)));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Store8(memory(
        layout.typed + layout.first,
        0,
    )));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Store(memory(layout.typed + layout.k, 3)));
    address(&mut f, layout.status);
    address(&mut f, 0);
    address(&mut f, layout.p);
    f.instruction(&Instruction::F64Const(0.0.into()));
    address(&mut f, layout.scratch);
    address(&mut f, layout.typed);
    f.instruction(&Instruction::Call(0));
    if matches!(case, LinkCase::WrongEntryType) {
        f.instruction(&Instruction::I32WrapI64);
    }
    f.instruction(&Instruction::I32Store(memory(0, 2)));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::F64Load(memory(layout.result, 3)));
    f.instruction(&Instruction::End);
    f
}

fn read_cell(layout: &Layout, load: Instruction<'_>) -> Function {
    let mut f = Function::new([]);
    base(&mut f, layout);
    f.instruction(&load);
    f.instruction(&Instruction::End);
    f
}
