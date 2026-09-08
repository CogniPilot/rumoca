//! Explicit executable semantics for checked GALEC Algorithm Code.
//!
//! This crate is an independent differential oracle. It depends only on the
//! checked language IR and has no lowering, rendering, target, or runtime-host
//! dependencies.

mod builtins;
mod execution;
mod interpreter;
mod numeric;
mod runtime;
mod value;

pub use interpreter::{EvaluationError, Evaluator};
pub use value::Value;

#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;
