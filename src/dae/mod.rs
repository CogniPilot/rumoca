//! Differential-Algebraic Equation (DAE) representation and utilities.
//!
//! This module provides the DAE-level representation of Modelica models
//! after compilation from the IR:
//!
//! - `ast`: The DAE AST types (Dae, equations, variables)
//! - `balance`: DAE balance checking (equations vs unknowns)
//! - `error`: DAE-specific error types
//! - `jinja`: Template rendering for code generation

pub mod ast;
pub mod balance;
pub mod error;
pub mod jinja;
