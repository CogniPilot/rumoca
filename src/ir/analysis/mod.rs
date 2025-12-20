//! Analysis passes for the Modelica IR.
//!
//! This module contains read-only analysis passes that examine the IR
//! without modifying it, as well as supporting data structures.

pub mod condition_finder;
pub mod reference_checker;
pub mod state_finder;
pub mod symbol_table;
pub mod symbol_trait;
pub mod symbols;
pub mod type_checker;
pub mod type_inference;
pub mod var_validator;

// Re-export the SymbolInfo trait for convenience
pub use symbol_trait::SymbolInfo;
