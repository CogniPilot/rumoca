//! Mechanically print the original causal operation, never a host numeric loop.
mod body;
#[cfg(test)]
mod tests;
use rumoca_ir_solve::fmi::{FmiDerivativeCausalOperation, FmiDerivativeControlPlan};
use wasm_encoder::*;

pub(crate) fn emit(
    operation: &FmiDerivativeCausalOperation<'_>,
    plan: &FmiDerivativeControlPlan,
) -> Vec<u8> {
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
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
            ValType::I32,
        ],
        [ValType::I32],
    );
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
    let mut next = 0;
    let mut helper = |name: &str, present: bool| {
        if present {
            imports.import("operation", name, EntityType::Function(0));
            let index = next;
            next += 1;
            Some(index)
        } else {
            None
        }
    };
    let static_function = helper("static_refresh", operation.static_schedule().is_some());
    let dynamic_function = helper("dynamic_refresh", operation.dynamic_schedule().is_some());
    let values = helper("values", true).expect("the complete operation always has values");
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(1);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("eval_derivatives", ExportKind::Func, next);
    module.section(&exports);
    let mut code = CodeSection::new();
    code.function(&body::emit(plan, static_function, dynamic_function, values));
    module.section(&code);
    module.finish()
}
