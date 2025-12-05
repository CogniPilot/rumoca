//! AST transformation passes for the Modelica IR.
//!
//! This module contains passes that transform the IR during compilation,
//! including flattening, import resolution, and function inlining.

pub mod array_comprehension;
pub mod constant_substitutor;
pub mod constants;
pub mod enum_substitutor;
pub mod equation_expander;
pub mod flatten;
pub mod function_inliner;
pub mod import_resolver;
pub mod multi_file;
pub mod operator_expand;
pub mod scope_resolver;
pub mod sub_comp_namer;
pub mod tuple_expander;
