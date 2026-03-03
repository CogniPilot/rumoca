//! Generic expression evaluator with automatic differentiation for Rumoca simulation.
//!
//! This crate provides:
//! - `SimFloat` trait for abstracting over `f64` and `Dual`
//! - `Dual` type for forward-mode automatic differentiation
//! - Generic expression and statement evaluators
//!
//! Used by `rumoca-sim-diffsol` for exact Jacobian/mass matrix computation via AD.

pub mod dual;
pub mod eval;
pub mod sim_float;
pub mod statement;
