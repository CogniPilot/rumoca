//! Compiled-evaluation infrastructure for DAE-level evaluators.

#[cfg(feature = "cranelift")]
pub mod cranelift;
#[cfg(feature = "wasm")]
pub mod wasm;

#[cfg(feature = "cranelift")]
pub use cranelift::{
    Backend, CompileError, CompiledExpressionRows, CompiledJacobianV, CompiledResidual,
    compile_discrete_rhs, compile_expression_row_block, compile_expressions,
    compile_initial_expressions, compile_initial_jacobian_v, compile_initial_residual,
    compile_jacobian_row_block, compile_jacobian_v, compile_residual, compile_residual_row_block,
    compile_root_conditions,
};
#[cfg(feature = "wasm")]
pub use wasm::{
    CompiledExpressionRowsWasm, CompiledJacobianVWasm, CompiledResidualWasm, WasmCompileError,
    compile_discrete_rhs_wasm, compile_expression_row_block_wasm, compile_expressions_wasm,
    compile_initial_expressions_wasm, compile_initial_jacobian_v_wasm,
    compile_initial_residual_wasm, compile_jacobian_row_block_wasm, compile_jacobian_v_wasm,
    compile_residual_row_block_wasm, compile_residual_wasm, compile_root_conditions_wasm,
};
