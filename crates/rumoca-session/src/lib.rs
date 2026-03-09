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
//! ## Public API Surface
//!
//! `Session` and `SessionConfig` are intentionally available at the crate root.
//! All other APIs are namespaced (`compile`, `parsing`, `runtime`, `analysis`,
//! `libraries`, `project`) to prevent root facade growth.
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
mod runtime_api;
mod session;

/// Analysis helpers.
pub mod analysis {
    pub use rumoca_sim::sim_trace_compare::{
        ModelDeviationMetric, SimTrace, SimTraceVariableMeta, compare_model_traces,
        count_agreement_bands_default, load_trace_json,
    };
}

/// Library discovery and cache helpers.
pub mod libraries {
    pub use crate::library::{
        extract_declared_roots, infer_library_roots, should_load_library_for_source,
        source_contains_identifier,
    };
    pub use crate::library_cache::{
        LibraryCacheStatus, ParsedLibrary, parse_library_with_cache, parse_library_with_cache_in,
        resolve_library_cache_dir,
    };
}

/// Parsing and merge helpers.
pub mod parsing {
    pub use rumoca_ir_ast as ast;
    pub use rumoca_ir_core as ir_core;

    pub use rumoca_ir_ast::{
        Causality, ClassDef, ClassType, ComponentReference, Expression, OpBinary, StoredDefinition,
        TerminalType, Token, Variability,
    };

    pub use crate::merge::{
        collect_class_type_counts, collect_model_names, merge_stored_definitions,
    };
    pub use crate::parse::{
        LenientParseResult, ParseError, ParseFailure, ParseResult, ParseSuccess,
        parse_and_merge_parallel, parse_files_parallel, parse_files_parallel_lenient,
        parse_source_to_ast, parse_source_to_ast_with_errors, validate_source_syntax,
    };
}

/// Workspace project config and sidecar helpers.
pub mod project {
    pub use crate::project_config::{
        EffectiveSimulationConfig, EffectiveSimulationPreset, LibrariesConfig, ModelIdentityRecord,
        PlotConfig, PlotDefaults, PlotModelConfig, PlotViewConfig, ProjectConfig,
        ProjectConfigFile, ProjectFileMoveHint, ProjectGcCandidate, ProjectGcReport, ProjectMeta,
        ProjectResyncRemap, ProjectResyncReport, ProjectSimulationSnapshot, SimulationConfig,
        SimulationDefaults, SimulationModelOverride, Viewer3dConfig, clear_model_simulation_preset,
        gc_orphan_model_sidecars, load_last_simulation_result_for_model, load_plot_views_for_model,
        load_simulation_run, load_simulation_snapshot_for_model, resync_model_sidecars,
        resync_model_sidecars_with_known_models, resync_model_sidecars_with_move_hints,
        write_last_simulation_result_for_model, write_model_simulation_preset,
        write_plot_views_for_model, write_simulation_run,
    };
}

/// Runtime and codegen helpers operating on compiled DAE.
pub mod runtime {
    pub use crate::runtime_api::*;
}

/// Compilation session API and result structures.
pub mod compile {
    pub use rumoca_core as core;
    pub use rumoca_ir_ast::ResolvedTree;
    pub use rumoca_ir_dae::Dae;
    pub use rumoca_ir_flat::Model as FlatModel;

    pub use crate::session::{
        CompilationMode, CompilationResult, CompilationSummary, CompilePhaseTimingSnapshot,
        CompilePhaseTimingStat, CompiledLibrary, Document, FailedPhase, IndexingMode,
        IndexingReport, ModelDiagnostics, ModelFailureDiagnostic, PhaseResult, Session,
        SessionConfig, StrictCompileReport, compile_phase_timing_stats,
        reset_compile_phase_timing_stats,
    };
}

// Root exports intentionally kept minimal to avoid a "god facade".
pub use compile::{Session, SessionConfig};
