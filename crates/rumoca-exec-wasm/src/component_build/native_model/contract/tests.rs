//! Private structural checker counterexamples, without source/entry authority.
use super::*;
use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemorySection, Module, TypeSection,
};

fn module(result: wasm_encoder::ValType, exported_memory: u32, math: bool) -> Vec<u8> {
    use wasm_encoder::ValType::{F64, I32};
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([I32, I32, F64, I32, I32], [result]);
    // Deliberately binary sin: valid WASM, but not the emitter's signature.
    types.ty().function([F64, F64], [F64]);
    module.section(&types);
    let mut imports = ImportSection::new();
    let memory = wasm_encoder::MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    };
    imports.import("env", "memory", EntityType::Memory(memory));
    if math {
        imports.import("env", "sin", EntityType::Function(1));
    }
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(0);
    module.section(&functions);
    let mut memories = MemorySection::new();
    memories.memory(wasm_encoder::MemoryType {
        maximum: Some(1),
        ..memory
    });
    module.section(&memories);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, exported_memory);
    exports.export("eval_assignments", ExportKind::Func, u32::from(math));
    module.section(&exports);
    let mut f = Function::new([]);
    f.instruction(&if result == I32 {
        Instruction::I32Const(0)
    } else {
        Instruction::F64Const(0.0.into())
    });
    f.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    code.function(&f);
    module.section(&code);
    module.finish()
}

#[test]
fn native_library_contract_refuses_wrong_entry_private_export_and_math_signature() {
    use wasm_encoder::ValType::{F64, I32};
    assert!(check_bytes(&module(I32, 0, false), &[]).is_ok());
    assert!(
        check_bytes(&module(F64, 0, false), &[])
            .unwrap_err()
            .contains("different eval_assignments ABI")
    );
    assert!(
        check_bytes(&module(I32, 1, false), &[])
            .unwrap_err()
            .contains("unsupported export")
    );
    assert!(
        check_bytes(&module(I32, 0, true), &["sin"])
            .unwrap_err()
            .contains("math signature")
    );
    assert!(
        check_bytes(&module(I32, 0, true), &[])
            .unwrap_err()
            .contains("emitted inventory")
    );
}

#[test]
fn native_library_contract_refuses_static_footprint_and_foreign_arena() {
    let mut bytes = module(wasm_encoder::ValType::I32, 0, false);
    // A valid empty custom section is unsupported in this exact emitter profile.
    bytes.extend([0, 2, 1, b'x']);
    assert!(
        check_bytes(&bytes, &[])
            .unwrap_err()
            .contains("unsupported section")
    );
    let memory = MemoryType {
        initial: 1,
        maximum: Some(1),
        memory64: false,
        shared: false,
        page_size_log2: None,
    };
    assert!(private_memory(memory));
    assert!(!private_memory(MemoryType {
        maximum: None,
        ..memory
    }));
    assert!(!private_memory(MemoryType {
        shared: true,
        ..memory
    }));
    assert!(!private_memory(MemoryType {
        initial: 1025,
        maximum: Some(1025),
        ..memory
    }));
}
