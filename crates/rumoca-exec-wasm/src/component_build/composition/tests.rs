use super::*;
use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemArg, MemorySection, MemoryType, Module, TypeSection, ValType,
};

fn memory(shared: bool) -> MemoryType {
    MemoryType {
        minimum: 1,
        maximum: Some(1),
        memory64: false,
        shared,
        page_size_log2: None,
    }
}

fn support(shared: bool) -> Vec<u8> {
    support_module(Some(shared))
}

fn support_without_memory() -> Vec<u8> {
    support_module(None)
}

fn support_module(shared: Option<bool>) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::F64], [ValType::F64]);
    module.section(&types);
    let mut functions = FunctionSection::new();
    functions.function(0);
    module.section(&functions);
    if let Some(shared) = shared {
        let mut memories = MemorySection::new();
        memories.memory(memory(shared));
        module.section(&memories);
    }
    let mut exports = ExportSection::new();
    if shared.is_some() {
        exports.export("memory", ExportKind::Memory, 0);
    }
    exports.export("double", ExportKind::Func, 0);
    module.section(&exports);
    let mut function = Function::new([]);
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::F64Const(2.0.into()));
    function.instruction(&Instruction::F64Mul);
    function.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    module.finish()
}

fn model() -> Vec<u8> {
    model_module(memory(false))
}

fn model_with_unbounded_public_memory() -> Vec<u8> {
    let mut public_memory = memory(false);
    public_memory.maximum = None;
    model_module(public_memory)
}

fn model_module(public_memory: MemoryType) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::F64], [ValType::F64]);
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
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import("env", "memory", EntityType::Memory(public_memory));
    imports.import("env", "double", EntityType::Function(0));
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(1);
    module.section(&functions);
    let mut private_memory = MemorySection::new();
    private_memory.memory(memory(false));
    module.section(&private_memory);
    let mut exports = ExportSection::new();
    exports.export("eval_assignments", ExportKind::Func, 1);
    module.section(&exports);
    let mut function = Function::new([]);
    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::LocalGet(2));
    function.instruction(&Instruction::Call(0));
    function.instruction(&Instruction::F64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    module.finish()
}

fn runtime(wrong_signature: bool) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    let result = if wrong_signature {
        ValType::F64
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
        [result],
    );
    types.ty().function([], [result]);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import("env", "memory", EntityType::Memory(memory(false)));
    imports.import("model", "eval_assignments", EntityType::Function(0));
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(1);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("invoke", ExportKind::Func, 1);
    module.section(&exports);
    let mut function = Function::new([]);
    for instruction in [
        Instruction::I32Const(0),
        Instruction::I32Const(8),
        Instruction::F64Const(3.0.into()),
        Instruction::I32Const(16),
        Instruction::I32Const(0),
        Instruction::Call(0),
        Instruction::End,
    ] {
        function.instruction(&instruction);
    }
    let mut code = CodeSection::new();
    code.function(&function);
    module.section(&code);
    module.finish()
}

fn assemble(support: &[u8], model: &[u8], runtime: &[u8]) -> Result<CoreModuleAssembly, String> {
    compose_core_modules(&[
        CoreModuleDraft {
            name: "support",
            bytes: support,
            arguments: &[],
        },
        CoreModuleDraft {
            name: "model",
            bytes: model,
            arguments: &[CoreModuleArgument {
                import_module: "env",
                source_module: "support",
            }],
        },
        CoreModuleDraft {
            name: "runtime",
            bytes: runtime,
            arguments: &[
                CoreModuleArgument {
                    import_module: "env",
                    source_module: "support",
                },
                CoreModuleArgument {
                    import_module: "model",
                    source_module: "model",
                },
            ],
        },
    ])
}

#[test]
fn actual_support_model_runtime_instructions_link_without_a_tool_process() {
    let linked = assemble(&support(false), &model(), &runtime(false)).unwrap();
    wasmparser::Validator::new()
        .validate_all(linked.bytes())
        .unwrap();
    let instances = wasmparser::Parser::new(0)
        .parse_all(linked.bytes())
        .filter_map(|item| match item.unwrap() {
            wasmparser::Payload::InstanceSection(section) => Some(section.count()),
            _ => None,
        })
        .sum::<u32>();
    assert_eq!(instances, 3);
}

#[test]
fn linked_signature_and_shared_memory_mismatches_refuse() {
    assert!(assemble(&support(false), &model(), &runtime(true)).is_err());
    assert!(assemble(&support(true), &model(), &runtime(false)).is_err());
}

#[test]
fn dependency_and_module_kind_misuse_refuse() {
    let module = support(false);
    let forward = [CoreModuleArgument {
        import_module: "env",
        source_module: "later",
    }];
    assert!(
        compose_core_modules(&[CoreModuleDraft {
            name: "first",
            bytes: &module,
            arguments: &forward,
        }])
        .is_err()
    );
    assert!(compose_core_modules(&[]).is_err());
    let component = Component::new().finish();
    assert!(
        compose_core_modules(&[CoreModuleDraft {
            name: "not-core",
            bytes: &component,
            arguments: &[],
        }])
        .is_err()
    );
}

mod wit_linkage;
