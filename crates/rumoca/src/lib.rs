//! # Rumoca Modelica Compiler
//!
//! Rumoca is a Modelica compiler written in Rust that compiles Modelica models
//! into Differential Algebraic Equation (DAE) representations.
//!
//! ## Features
//!
//! - **Parsing**: Parses Modelica files into an abstract syntax tree (AST)
//! - **Type Checking**: Resolves names and checks types
//! - **Instantiation**: Creates instances with modifications applied
//! - **Flattening**: Flattens the instance tree to a flat equation system
//! - **DAE Creation**: Converts to DAE representation (MLS Appendix B)
//! - **Template Rendering**: Renders DAE using Jinja2 templates
//!
//! ## Usage
//!
//! ```ignore
//! use rumoca::Compiler;
//!
//! let result = Compiler::new()
//!     .model("MyModel")
//!     .compile_file("model.mo")?;
//!
//! // Export DAE IR to JSON
//! let json = result.to_ir_json(rumoca::TemplateIr::Dae)?;
//! ```

mod compiler;
mod error;

pub use compiler::{CompilationResult, Compiler, DaeCompilationResult, TemplateIr};
pub use error::CompilerError;
