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

// The CLI surface — argument parsing/dispatch (`cli`) plus the per-command
// implementations below — depends on the interactive `runner` feature
// (transports, signal-hook, anyhow, `rumoca_sim::runner`). Library consumers of
// this crate (the Python binding, wasm builds) need only the batch compile
// surface re-exported below, and must build without those native-only deps, so
// the whole CLI is gated on `runner`. The `rumoca` binary enables `runner`
// through its package-level `default`, so it always sees the CLI.
#[cfg(feature = "runner")]
pub mod cli;

// CLI subcommand implementations. Declared here (rather than in `main.rs`) so
// both the binary and the reusable `cli` module can reach them; `cli` owns the
// argument types and dispatch, these own the per-command work.
#[cfg(feature = "runner")]
pub(crate) mod cache_cmd;
#[cfg(feature = "runner")]
pub(crate) mod fmt_cli;
#[cfg(feature = "runner")]
pub(crate) mod fmu;
#[cfg(feature = "runner")]
pub(crate) mod main_helpers;
#[cfg(feature = "runner")]
pub(crate) mod sim_bench;
#[cfg(feature = "runner")]
pub(crate) mod sim_inspect;
#[cfg(feature = "runner")]
pub(crate) mod target_manifest;
#[cfg(feature = "runner")]
pub(crate) mod targets_cmd;

pub use compiler::{CompilationResult, Compiler, DaeCompilationResult, TemplateIr};
pub use error::CompilerError;
