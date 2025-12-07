//! LSP data constants (builtin functions, keywords, etc.).

pub mod keywords;

// Re-export builtin functions from the shared IR module
pub use crate::ir::transform::constants::{
    BuiltinFunction, get_builtin_function, get_builtin_functions,
};
