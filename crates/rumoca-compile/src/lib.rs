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
//! `crates/rumoca-compile/PIPELINE_INVARIANTS.md`
//!
//! ## Public API Surface
//!
//! `Session` and `SessionConfig` are intentionally available at the crate root.
//! All other APIs are namespaced (`compile`, `parsing`, `codegen`,
//! `source_roots`, `project`) to prevent root facade growth.
//!
//! # Example
//!
//! ```rust,ignore
//! use rumoca_compile::{Session, SessionConfig};
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

mod codegen_api;
mod experiment;
mod instrumentation;
#[cfg(test)]
mod instrumentation_tests;
mod merge;
mod package_layout;
mod parse;
mod parsed_artifact_cache;
mod project_config;
mod session;
mod source_root_cache;
mod source_root_discovery;
mod traversal_adapter;

/// Source-root discovery and cache helpers.
pub mod source_roots {
    pub use crate::package_layout::PackageLayoutError;
    pub use crate::session::SourceRootRefreshPlan;
    pub use crate::source_root_cache::{
        ParsedSourceRoot, SourceRootCacheStatus, SourceRootCacheTiming,
        parse_source_root_with_cache, parse_source_root_with_cache_in,
        resolve_source_root_cache_dir,
    };
    pub use crate::source_root_discovery::{
        SourceRootDuplicateSkip, SourceRootLoadPlan, canonical_path_key,
        classify_configured_source_root_kind, merge_source_root_paths, plan_source_root_loads,
        referenced_unloaded_source_root_paths, render_source_root_indexing_failed_message,
        render_source_root_indexing_finished_message, render_source_root_indexing_started_message,
        render_source_root_status_message, source_requires_unloaded_source_roots,
        source_root_paths_changed, source_root_source_set_key, source_root_status_display_name,
        sources_require_loaded_source_roots,
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
    pub use crate::package_layout::collect_compile_unit_source_files;
    pub use crate::parse::{
        LenientParseResult, ParseError, ParseFailure, ParseResult, ParseSuccess,
        parse_and_merge_parallel, parse_files_parallel, parse_files_parallel_lenient,
        parse_source_to_ast, parse_source_to_ast_with_errors, validate_source_syntax,
    };
}

/// Workspace project config and sidecar helpers.
pub mod project {
    pub use crate::project_config::{
        EffectiveSimulationConfig, EffectiveSimulationPreset, ModelIdentityRecord, PlotConfig,
        PlotDefaults, PlotModelConfig, PlotViewConfig, ProjectConfig, ProjectConfigFile,
        ProjectFileMoveHint, ProjectGcCandidate, ProjectGcReport, ProjectMeta, ProjectResyncRemap,
        ProjectResyncReport, ProjectSimulationSnapshot, SimulationConfig, SimulationDefaults,
        SimulationModelOverride, SourceRootsConfig, clear_model_simulation_preset,
        gc_orphan_model_sidecars, load_last_simulation_result_for_model, load_plot_views_for_model,
        load_simulation_run, load_simulation_snapshot_for_model, resync_model_sidecars,
        resync_model_sidecars_with_known_models, resync_model_sidecars_with_move_hints,
        write_last_simulation_result_for_model, write_model_simulation_preset,
        write_plot_views_for_model, write_simulation_run,
    };
}

/// Code generation helpers operating on compiled DAE.
pub mod codegen {
    pub use crate::codegen_api::*;
}

/// Compilation session API and result structures.
pub mod compile {
    pub use rumoca_core as core;
    pub use rumoca_ir_ast::ResolvedTree;
    pub use rumoca_ir_dae::{Dae, VarName, Variable};
    pub use rumoca_ir_flat::Model as FlatModel;

    pub use crate::instrumentation::{
        SessionCacheStatsSnapshot, reset_session_cache_stats, session_cache_stats,
    };
    pub use crate::session::{
        ClassLocalCompletionItem, ClassLocalCompletionKind, CompilationMode, CompilationResult,
        CompilationSummary, CompilePhaseTimingSnapshot, CompilePhaseTimingStat, CompiledSourceRoot,
        DaeCompilationResult, Document, DocumentSymbol, DocumentSymbolKind, FailedPhase,
        LocalComponentInfo, ModelDiagnostics, ModelFailureDiagnostic, NavigationClassTargetInfo,
        ParsedSourceRootLoad, PhaseResult, SemanticDiagnosticsMode, Session, SessionChange,
        SessionConfig, SessionSnapshot, SourceRootActivityKind, SourceRootActivityPhase,
        SourceRootActivitySnapshot, SourceRootDurability, SourceRootKind, SourceRootLoadMode,
        SourceRootLoadReport, SourceRootStatusSnapshot, StrictCompileReport, WorkspaceSymbol,
        WorkspaceSymbolKind, WorkspaceSymbolSnapshotTiming, compile_phase_timing_stats,
        reset_compile_phase_timing_stats,
    };
}

// Root exports intentionally kept minimal to avoid a "god facade".
pub use compile::{Session, SessionConfig};
