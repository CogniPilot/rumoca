//! Constant expression evaluator for Modelica.
//!
//! This crate provides compile-time evaluation of Modelica expressions,
//! used for:
//! - Evaluating parameter values
//! - Computing array dimensions
//! - Resolving for-loop ranges
//! - Evaluating if-equation conditions
//! - Evaluating user-defined functions with constant arguments (MLS §12)

pub mod builtins;
pub mod errors;
pub mod function_eval;
pub mod value;

mod builtin_dispatch;
mod context;
mod expr_eval;
mod matrix_ops;
mod operators;
mod range_eval;

#[cfg(test)]
mod tests;

pub use builtins::{eval_builtin, is_builtin};
pub use context::EvalContext;
pub use errors::EvalError;
pub use expr_eval::{
    eval_expr, eval_expr_with_span, try_eval_bool, try_eval_integer, try_eval_real,
};
pub use function_eval::{EvalLimits, eval_function};
pub use value::Value;

use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

type BuiltinFunction = rumoca_core::BuiltinFunction;
type Expression = rumoca_core::Expression;
type Function = rumoca_core::Function;
type Literal = rumoca_core::Literal;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;
type EvalIndexMap<V> = IndexMap<String, V, FxBuildHasher>;
