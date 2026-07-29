//! Explicit executable semantics for checked GALEC Algorithm Code.
//!
//! This crate is an independent differential oracle. It depends only on the
//! checked language IR and has no lowering, rendering, target, or runtime-host
//! dependencies.

mod interpreter;
mod value;

pub use interpreter::{EvaluationError, Evaluator};
pub use value::{IntegerDomain, Value};
