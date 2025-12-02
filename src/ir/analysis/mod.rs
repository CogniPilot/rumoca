//! Analysis passes for the Modelica IR.
//!
//! This module contains read-only analysis passes that examine the IR
//! without modifying it, as well as supporting data structures.

pub mod balance_check;
pub mod condition_finder;
pub mod state_finder;
pub mod symbol_table;
pub mod var_validator;
