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
//! // Export to JSON
//! let json = result.dae.to_json()?;
//! ```

mod compiler;
mod error;
pub mod sim_report;
pub mod sim_trace_compare;

pub use compiler::{CompilationResult, Compiler};
pub use error::CompilerError;

// Re-export from rumoca-session for convenience
pub use rumoca_session::{
    BestEffortCompilationReport, CompilationSummary, CompiledLibrary, FailedPhase,
    ModelFailureDiagnostic, ParseFailure, ParseSuccess, PhaseResult, Session, SessionConfig,
    collect_class_type_counts, collect_model_names, compile_phase_timing_stats,
    merge_stored_definitions, parse_and_merge_parallel, parse_files_parallel,
    parse_files_parallel_lenient, reset_compile_phase_timing_stats,
};

// Re-export key types for convenience
pub use rumoca_session::{ClassTree, Dae, Model, StoredDefinition, TypedTree};
