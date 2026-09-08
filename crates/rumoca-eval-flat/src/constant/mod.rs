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
mod formal_extents;
mod matrix_ops;
mod operators;
mod range_eval;

#[cfg(test)]
mod tests;

pub use builtins::eval_builtin;
pub use context::{
    EvalContext, ResolvedEnumCatalog, ResolvedEnumDeclaration, ResolvedIdentityInventory,
    ResolvedOccurrenceKey, ResolvedShapeBinding, ResolvedValueBinding,
};
pub use errors::{DeferredParameterSource, EvalError, RuntimeDependentReason};
#[cfg(test)]
pub(crate) use expr_eval::default_dependencies_bound;
pub(crate) use expr_eval::{
    CheckedCallPlan, CheckedFunctionTarget, EvaluatedCall, checked_call_plan,
    checked_statement_call_plan, resolve_context_function_occurrence, resolve_function_occurrence,
    selected_formal_index, validate_semantic_expression,
};
pub use expr_eval::{
    eval_expr, eval_expr_with_span, eval_optional, try_eval_bool, try_eval_integer, try_eval_real,
};
pub(crate) use formal_extents::bind_formal_extents;
pub use function_eval::EvalLimits;
pub use value::{ResolvedEnumValue, Value};

use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

type BuiltinFunction = rumoca_core::BuiltinFunction;
type Expression = rumoca_core::Expression;
type Function = rumoca_core::Function;
type Literal = rumoca_core::Literal;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type Subscript = rumoca_core::Subscript;
type EvalIndexMap<V> = IndexMap<String, V, FxBuildHasher>;

/// Maximum number of values an opportunistic fold may materialize at once.
/// SPEC_0032 §7 owns this retained-node/default interpreter-work limit.
pub(super) const DEFAULT_EVAL_BUDGET: usize = 100_000;
/// Maximum rank materialized by the bounded recursive evaluator. Higher
/// ranks remain valid source constructs but must stay in typed array ownership.
/// SPEC_0032 §7 owns this materialized-rank limit.
pub(super) const DEFAULT_MATERIALIZED_RANK_BUDGET: usize = 256;
