//! Mechanically print the original causal operation, never a host numeric loop.
mod body;
#[cfg(test)]
mod kinematic_tests;
#[cfg(test)]
mod tests;
use super::Linkage;
use rumoca_ir_solve::fmi::{FmiDerivativeCausalOperation, FmiDerivativeControlPlan};
use wasm_encoder::*;

/// The roles the control module imports its helpers by, in import order.
pub(crate) const HELPER_ROLES: [&str; 4] = [
    "static_refresh",
    "dynamic_refresh",
    "values",
    "kinematic_values",
];

pub(crate) fn emit(
    operation: &FmiDerivativeCausalOperation<'_>,
    plan: &FmiDerivativeControlPlan,
    linkage: Linkage<'_>,
) -> Result<Vec<u8>, String> {
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
            let (module, field) = linkage.operation(name);
            imports.import(module, &field, EntityType::Function(0));
            let index = next;
            next += 1;
            Some(index)
        } else {
            None
        }
    };
    let [static_refresh, dynamic_refresh, values, kinematic_values] = HELPER_ROLES;
    let static_function = helper(static_refresh, operation.static_schedule().is_some());
    let dynamic_function = helper(dynamic_refresh, operation.dynamic_schedule().is_some());
    let values = helper(values, true).expect("the complete operation always has values");
    let kinematic = helper(kinematic_values, plan.kinematic_instructions().is_some());
    module.section(&imports);
    let helpers = body::Helpers {
        stat: static_function,
        dynamic: dynamic_function,
        values,
        kinematic,
    };
    let mut functions = FunctionSection::new();
    functions.function(1);
    if plan.kinematic_instructions().is_some() {
        functions.function(0);
    }
    module.section(&functions);
    let mut exports = linkage.exports();
    let browser = if plan.kinematic_instructions().is_some() {
        &["eval_derivatives", "eval_kinematic"][..]
    } else {
        &["eval_derivatives"][..]
    };
    for (index, name) in (next..).zip(linkage.names(browser)?) {
        exports.export(&name, ExportKind::Func, index);
    }
    module.section(&exports);
    let mut code = CodeSection::new();
    code.function(&body::emit(plan.instructions(), plan.i32_locals(), helpers));
    if let Some(kinematic) = plan.kinematic_instructions() {
        code.function(&body::emit(kinematic, plan.kinematic_i32_locals(), helpers));
    }
    module.section(&code);
    Ok(module.finish())
}
