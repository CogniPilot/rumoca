//! Compare the scoped cell loop against independently emitted original loops.
use super::*;
use wasmi::{Engine, Linker, Memory, MemoryType as RuntimeMemoryType, Store};

fn original_loop(
    emitter: &mut BodyEmitter<'_>,
    counter: u32,
    count: usize,
    body: impl FnOnce(&mut BodyEmitter<'_>) -> Result<(), String>,
) -> Result<(), String> {
    let count = i32::try_from(count).map_err(|_| "WASM loop extent exceeds i32")?;
    for instruction in [
        Instruction::I32Const(0),
        Instruction::LocalSet(counter),
        Instruction::Block(BlockType::Empty),
        Instruction::Loop(BlockType::Empty),
        Instruction::LocalGet(counter),
        Instruction::I32Const(count),
        Instruction::I32GeU,
        Instruction::BrIf(1),
    ] {
        emitter.push(instruction);
    }
    body(emitter)?;
    for instruction in [
        Instruction::LocalGet(counter),
        Instruction::I32Const(1),
        Instruction::I32Add,
        Instruction::LocalSet(counter),
        Instruction::Br(0),
        Instruction::End,
        Instruction::End,
    ] {
        emitter.push(instruction);
    }
    Ok(())
}

fn module(count: usize, old: bool, change_counter: bool) -> Result<Vec<u8>, String> {
    let mut module = Module::new();
    let types = call_program::add_types(&mut module);
    let catalog = add_import_section(&mut module, &[], &types);
    let mut functions = FunctionSection::new();
    functions.function(types.eval_type);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("run", ExportKind::Func, 0);
    module.section(&exports);
    let mut function = Function::new([(2, ValType::I32)]);
    let mut emitter = BodyEmitter::new(&catalog, &mut function);
    emitter.push(Instruction::I32Const(1234));
    emitter.push(Instruction::LocalSet(LOCAL_BASE + 1));
    let body = |e: &mut BodyEmitter<'_>| {
        // OUT_PTR_PARAM is a requested failing ordinal, not a memory pointer.
        e.push(Instruction::LocalGet(LOCAL_BASE));
        e.push(Instruction::LocalGet(OUT_PTR_PARAM));
        e.push(Instruction::I32Eq);
        e.push(Instruction::If(BlockType::Empty));
        e.push(Instruction::I32Const(233));
        e.push(Instruction::Return);
        e.push(Instruction::End);
        let memarg = MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        };
        e.push(Instruction::I32Const(64));
        e.push(Instruction::I32Const(64));
        e.push(Instruction::I64Load(memarg));
        e.push(Instruction::I64Const(1));
        e.push(Instruction::I64Add);
        e.push(Instruction::I64Store(memarg));
        e.push(Instruction::I32Const(80));
        e.push(Instruction::LocalGet(LOCAL_BASE));
        e.push(Instruction::I32Store(MemArg { align: 2, ..memarg }));
        if change_counter {
            e.push(Instruction::I32Const(5));
            e.push(Instruction::LocalSet(LOCAL_BASE));
        }
        Ok(())
    };
    if old {
        original_loop(&mut emitter, LOCAL_BASE, count, body)?;
    } else {
        emitter.cells(LOCAL_BASE, count, body)?;
    }
    emitter.push(Instruction::I32Const(96));
    emitter.push(Instruction::LocalGet(LOCAL_BASE + 1));
    emitter.push(Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    emitter.push(Instruction::LocalGet(LOCAL_BASE));
    emitter.push(Instruction::End);
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    Ok(module.finish())
}

fn run(bytes: &[u8], failing_ordinal: i32) -> (i32, Vec<u8>) {
    let engine = Engine::default();
    let module = wasmi::Module::new(&engine, bytes).unwrap();
    let mut store = Store::new(&engine, ());
    let memory = Memory::new(&mut store, RuntimeMemoryType::new(1, None)).unwrap();
    memory.write(&mut store, 80, &[0xa5; 24]).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let call = instance
        .get_typed_func::<(i32, i32, f64, i32, i32), i32>(&store, "run")
        .unwrap();
    let result = call
        .call(&mut store, (0, 0, 0., 0, failing_ordinal))
        .unwrap();
    (result, memory.data(&store).to_vec())
}

#[test]
fn scoped_cells_match_original_zero_one_two_iteration_state_and_early_returns() {
    for count in [0usize, 1, 2] {
        let old = module(count, true, false).unwrap();
        let current = module(count, false, false).unwrap();
        for failing in [-1, 0, 1] {
            assert_eq!(run(&current, failing), run(&old, failing));
        }
        let (counter, memory) = run(&current, -1);
        assert_eq!(counter, count as i32);
        assert_eq!(
            u64::from_le_bytes(memory[64..72].try_into().unwrap()),
            count as u64
        );
        assert_eq!(
            u32::from_le_bytes(memory[96..100].try_into().unwrap()),
            1234
        );
        if count != 0 {
            assert_eq!(
                u32::from_le_bytes(memory[80..84].try_into().unwrap()),
                count as u32 - 1
            );
        }
    }
}

#[test]
fn scoped_single_cell_preserves_original_increment_even_if_body_changes_counter() {
    let old = module(1, true, true).unwrap();
    let current = module(1, false, true).unwrap();
    assert_eq!(run(&current, -1), run(&old, -1));
    assert_eq!(run(&current, -1).0, 6);
}

#[test]
fn scoped_cells_preserve_extent_error_and_emit_unreachable_body_errors() {
    let count = i32::MAX as usize + 1;
    assert_eq!(module(count, false, false), module(count, true, false));
    let catalog = ImportCatalog {
        function_indices: BTreeMap::new(),
        eval_function_index: 0,
    };
    for count in [0, 1, 2] {
        let mut function = Function::new([(2, ValType::I32)]);
        let mut emitter = BodyEmitter::new(&catalog, &mut function);
        let original = original_loop(&mut emitter, LOCAL_BASE, count, |_| {
            Err("body refusal".into())
        });
        let current = emitter.cells(LOCAL_BASE, count, |_| Err("body refusal".into()));
        assert_eq!(current, original);
        assert_eq!(current, Err("body refusal".into()));
    }
}
