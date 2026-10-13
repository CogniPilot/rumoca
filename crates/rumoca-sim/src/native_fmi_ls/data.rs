//! Sole internally generated receiver data provider, relocated by the linker.

use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, CustomSection, DataSection, Encode, EntityType, ExportKind,
    ExportSection, Function, FunctionSection, GlobalType, ImportSection, Instruction, MemoryType,
    Module, TypeSection, ValType,
};

pub(super) fn library(capsule: &[u8], token: &str) -> Result<Vec<u8>, String> {
    let capsule_len = length(capsule.len())?;
    let token_len = length(token.len())?;
    let total = capsule_len
        .checked_add(token_len)
        .ok_or("receiver data allocation overflow")?;
    length(total as usize)?;
    let mut module = Module::new();
    let mut allocation = Vec::new();
    total.encode(&mut allocation);
    for value in [0_u32, 0, 0] {
        value.encode(&mut allocation);
    }
    let mut dylink = vec![1]; // dylink.0 MemInfo subsection
    (allocation.len() as u32).encode(&mut dylink);
    dylink.extend(allocation);
    module.section(&CustomSection {
        name: "dylink.0".into(),
        data: dylink.into(),
    });
    let mut types = TypeSection::new();
    types.ty().function([], [ValType::I32]);
    types
        .ty()
        .function([ValType::I32, ValType::I32], [ValType::I32]);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import(
        "env",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 0,
            maximum: None,
            memory64: false,
            shared: false,
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
    module.section(&imports);
    let mut functions = FunctionSection::new();
    for ty in [0, 1, 0, 1] {
        functions.function(ty);
    }
    module.section(&functions);
    let mut exports = ExportSection::new();
    for (ordinal, name) in [
        "rumoca_source_capsule_len",
        "rumoca_source_capsule_copy",
        "rumoca_publication_token_len",
        "rumoca_publication_token_copy",
    ]
    .into_iter()
    .enumerate()
    {
        exports.export(name, ExportKind::Func, ordinal as u32);
    }
    module.section(&exports);
    let mut code = CodeSection::new();
    code.function(&length_function(capsule_len));
    code.function(&copy_function(0, capsule_len));
    code.function(&length_function(token_len));
    code.function(&copy_function(capsule_len, token_len));
    module.section(&code);
    let mut data = DataSection::new();
    data.active(
        0,
        &ConstExpr::global_get(0),
        (0..total as usize).map(|index| {
            if index < capsule.len() {
                capsule[index]
            } else {
                token.as_bytes()[index - capsule.len()]
            }
        }),
    );
    module.section(&data);
    Ok(module.finish())
}

fn length(value: usize) -> Result<u32, String> {
    let value = u32::try_from(value).map_err(|_| "receiver data exceeds memory32")?;
    if value == 0 || value > i32::MAX as u32 {
        return Err("receiver data length must be a positive i32".into());
    }
    Ok(value)
}

fn length_function(length: u32) -> Function {
    let mut function = Function::new([]);
    function.instruction(&Instruction::I32Const(length as i32));
    function.instruction(&Instruction::End);
    function
}

fn copy_function(offset: u32, length: u32) -> Function {
    let mut function = Function::new([]);
    // Capacity and memory range are checked before any write. Unsigned i64
    // arithmetic prevents a wrapped memory32 address from passing the check.
    for instruction in [
        Instruction::LocalGet(1),
        Instruction::I32Const(length as i32),
        Instruction::I32LtU,
        Instruction::LocalGet(0),
        Instruction::I64ExtendI32U,
        Instruction::I64Const(i64::from(length)),
        Instruction::I64Add,
        Instruction::MemorySize(0),
        Instruction::I64ExtendI32U,
        Instruction::I64Const(65536),
        Instruction::I64Mul,
        Instruction::I64GtU,
        Instruction::I32Or,
        Instruction::If(BlockType::Empty),
        Instruction::I32Const(-1),
        Instruction::Return,
        Instruction::End,
        Instruction::LocalGet(0),
        Instruction::GlobalGet(0),
        Instruction::I32Const(offset as i32),
        Instruction::I32Add,
        Instruction::I32Const(length as i32),
        Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        },
        Instruction::I32Const(length as i32),
        Instruction::End,
    ] {
        function.instruction(&instruction);
    }
    function
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasmi::{Engine, Global, Linker, Memory, MemoryType, Mutability, Store, Val};

    #[test]
    fn paired_data_uses_actual_relocated_storage_and_atomic_checked_copies() {
        let bytes = library(b"canonical-source", "original-token").unwrap();
        let engine = Engine::default();
        let module = wasmi::Module::new(&engine, &bytes[..]).unwrap();
        let mut store = Store::new(&engine, ());
        let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
        let base = Global::new(&mut store, Val::I32(256), Mutability::Const);
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        linker.define("env", "__memory_base", base).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        for (name, expected) in [
            ("rumoca_source_capsule", b"canonical-source".as_slice()),
            ("rumoca_publication_token", b"original-token".as_slice()),
        ] {
            let length = instance
                .get_typed_func::<(), i32>(&store, &format!("{name}_len"))
                .unwrap();
            let copy = instance
                .get_typed_func::<(i32, i32), i32>(&store, &format!("{name}_copy"))
                .unwrap();
            assert_eq!(length.call(&mut store, ()).unwrap(), expected.len() as i32);
            memory.write(&mut store, 1024, &[0xa5; 32]).unwrap();
            assert_eq!(
                copy.call(&mut store, (1024, expected.len() as i32 - 1))
                    .unwrap(),
                -1
            );
            assert_eq!(&memory.data(&store)[1024..1056], &[0xa5; 32]);
            let before = memory.data(&store).to_vec();
            assert_eq!(copy.call(&mut store, (-1, 32)).unwrap(), -1);
            assert_eq!(memory.data(&store), &before);
            assert_eq!(
                copy.call(&mut store, (1024, 32)).unwrap(),
                expected.len() as i32
            );
            assert_eq!(&memory.data(&store)[1024..1024 + expected.len()], expected);
            assert_eq!(
                &memory.data(&store)[1024 + expected.len()..1056],
                &vec![0xa5; 32 - expected.len()]
            );
        }
    }

    #[test]
    fn paired_data_refuses_empty_or_unrepresentable_slot_lengths() {
        assert_eq!(
            length(0).unwrap_err(),
            "receiver data length must be a positive i32"
        );
        assert_eq!(
            length(i32::MAX as usize + 1).unwrap_err(),
            "receiver data length must be a positive i32"
        );
        assert!(library(&[], "issued").is_err());
        assert!(library(b"source", "").is_err());
    }
}
