//! Compiled-evaluation infrastructure for Rumoca.
//!
//! This crate currently provides a DAE variable layout that maps scalar variable
//! names to packed `y`/`p` storage offsets (and inlined constants).

pub mod ad;
#[cfg(feature = "cranelift")]
pub mod cranelift;
pub mod layout;
pub mod linear_op;
pub mod lower;
#[cfg(feature = "wasm")]
pub mod wasm;

pub use ad::lower_residual_ad;
#[cfg(feature = "cranelift")]
pub use cranelift::{
    Backend, CompileError, CompiledExpressionRows, CompiledJacobianV, CompiledResidual,
    compile_discrete_rhs, compile_expressions, compile_initial_residual, compile_jacobian_v,
    compile_residual, compile_root_conditions,
};
pub use layout::{ScalarSlot, VarLayout};
pub use linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
pub use lower::{
    LowerError, LoweredExpression, lower_discrete_rhs, lower_expression, lower_initial_residual,
    lower_residual, lower_root_conditions,
};
#[cfg(feature = "wasm")]
pub use wasm::{
    CompiledExpressionRowsWasm, CompiledJacobianVWasm, CompiledResidualWasm, WasmCompileError,
    compile_discrete_rhs_wasm, compile_expressions_wasm, compile_initial_residual_wasm,
    compile_jacobian_v_wasm, compile_residual_wasm, compile_root_conditions_wasm,
};
