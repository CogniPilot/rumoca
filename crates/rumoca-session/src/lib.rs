//! Unified compilation session management for Rumoca.
//!
//! This crate provides a standardized interface for compiling Modelica code
//! across different frontends: CLI, LSP, WASM, etc.
//!
//! # Features
//!
//! - **Session management**: Track open documents and compilation state
//! - **Multi-file support**: Combine multiple files with within clause handling
//! - **Parallel compilation**: Compile multiple models concurrently
//! - **Incremental updates**: Update single documents without full recompilation
//! - **Thread-safe**: Safe for concurrent use from multiple threads
//! - **Explicit compile contracts**: Phase-local failures and structured
//!   `NeedsInner`/`Failed` outcomes via `PhaseResult`
//!
//! ## Pipeline Invariants
//!
//! The orchestrator phase ordering and failure contracts are documented in:
//! `crates/rumoca-session/PIPELINE_INVARIANTS.md`
//!
//! # Example
//!
//! ```rust,ignore
//! use rumoca_session::{Session, SessionConfig};
//!
//! // Create a session
//! let mut session = Session::new(SessionConfig::default());
//!
//! // Add source files
//! session.add_file("Model.mo", source_code)?;
//!
//! // Compile a specific model
//! let result = session.compile_model("MyPackage.MyModel")?;
//! ```

mod experiment;
mod library;
mod library_cache;
mod merge;
mod parse;
mod project_config;
mod session;

pub use library::{
    extract_declared_roots, infer_library_roots, should_load_library_for_source,
    source_contains_identifier,
};
pub use library_cache::{
    LibraryCacheStatus, ParsedLibrary, parse_library_with_cache, parse_library_with_cache_in,
    resolve_library_cache_dir,
};
pub use merge::{collect_class_type_counts, collect_model_names, merge_stored_definitions};
pub use parse::{
    ParseFailure, ParseResult, ParseSuccess, parse_and_merge_parallel, parse_files_parallel,
    parse_files_parallel_lenient,
};
pub use project_config::{
    EffectiveSimulationConfig, EffectiveSimulationPreset, PlotViewConfig, ProjectConfig,
    ProjectConfigFile, ProjectFileMoveHint, ProjectGcCandidate, ProjectGcReport,
    ProjectResyncRemap, ProjectResyncReport, ProjectSimulationSnapshot, SimulationDefaults,
    SimulationModelOverride, clear_model_simulation_preset, gc_orphan_model_sidecars,
    load_last_simulation_result_for_model, load_plot_views_for_model, load_simulation_run,
    load_simulation_snapshot_for_model, resync_model_sidecars,
    resync_model_sidecars_with_known_models, resync_model_sidecars_with_move_hints,
    write_last_simulation_result_for_model, write_model_simulation_preset,
    write_plot_views_for_model, write_simulation_run,
};
pub use session::{
    BestEffortCompilationReport, CompilationResult, CompilationSummary, CompilePhaseTimingSnapshot,
    CompilePhaseTimingStat, CompiledLibrary, Document, FailedPhase, ModelFailureDiagnostic,
    PhaseResult, Session, SessionConfig, compile_phase_timing_stats,
    reset_compile_phase_timing_stats,
};

// Re-export key types for convenience
pub use rumoca_ir_ast::{ClassTree, ResolvedTree, StoredDefinition, TypedTree};
pub use rumoca_ir_dae::Dae;
pub use rumoca_ir_flat::Model;
