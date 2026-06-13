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

pub mod cache;
mod codegen_api;
mod codegen_target;
mod experiment;
mod instrumentation;
#[cfg(test)]
mod instrumentation_tests;
mod merge;
mod package_layout;
pub mod parallelism;
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
        resolve_source_root_cache_dir, set_cache_root_override,
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
    pub use rumoca_core as ir_core;
    pub use rumoca_ir_ast as ast;

    pub use rumoca_core::{
        Causality, ClassType, DefId, Location, OpBinary, Span, Token, Variability,
    };
    pub use rumoca_ir_ast::{
        ClassDef, ComponentReference, Expression, StoredDefinition, TerminalType,
        walk_component_reference_default,
    };

    pub use crate::merge::{
        collect_class_type_counts, collect_model_names, merge_stored_definitions,
        qualify_stored_definition_class_name,
    };
    pub use crate::package_layout::collect_compile_unit_source_files;
    pub use crate::parse::{
        LenientParseResult, ParseError, ParseFailure, ParseResult, ParseSuccess,
        parse_and_merge_parallel, parse_files_parallel, parse_files_parallel_lenient,
        parse_source_to_ast, parse_source_to_ast_with_errors, validate_source_syntax,
    };
}

/// Workspace colocated model config and generated result helpers.
pub mod project {
    pub use crate::project_config::{
        CodegenConfig, EffectiveSimulationConfig, EffectiveSimulationPreset, ModelConfig,
        PlotConfig, PlotDefaults, PlotModelConfig, PlotViewConfig, ProjectConfig,
        ProjectConfigFile, ProjectSimulationSnapshot, ProjectTask, RumocaTaskMarker,
        ScenarioViewerConfig, ScenarioViewerMode, SimulationConfig, SimulationDefaults,
        SimulationModelOverride, clear_model_simulation_preset, is_rumoca_task_filename,
        load_last_simulation_result_for_model, load_plot_views_for_model, load_simulation_run,
        load_simulation_snapshot_for_model, parse_fallback_simulation, parse_views_payload,
        scenario_config_response, simulation_override_from_json, simulation_preset_to_json,
        simulation_settings_to_json, visualization_views_to_json,
        write_last_simulation_result_for_model, write_model_simulation_preset,
        write_plot_views_for_model, write_simulation_run,
    };
}

/// Code generation helpers operating on compiled DAE.
pub mod codegen {
    pub use crate::codegen_api::templates;
    pub use crate::codegen_api::{
        CodegenError, SolveTemplateRenderer, dae_to_template_json, render_ast_template_with_name,
        render_dae_template, render_dae_template_with_json, render_dae_template_with_json_and_name,
        render_dae_template_with_name, render_flat_template_with_name,
        render_solve_template_with_name,
    };
    pub mod targets {
        pub use crate::codegen_target::{
            BuiltinTargetDescriptor, RenderedTargetFile, TargetBuildKind, TargetBundle,
            TargetCapabilities, TargetCompatibilityEntry, TargetFeatureSupport, TargetFile,
            TargetManifest, TargetTemplateIr, TargetTemplateSource, TensorCapabilities,
            TensorCapability, TensorLayoutCapability, builtin_target_compatibility_matrix,
            builtin_target_descriptors_for_ir, ensure_target_has_rendered_files,
            parse_target_manifest, render_dae_target_files, safe_target_join,
            target_ir_is_dae_renderable, target_manifest_ir, validate_dae_target_capabilities,
        };
    }
}

/// Read-only DAE analysis helpers exposed through the compile facade.
pub mod analysis {
    pub use rumoca_phase_dae::balance::BalanceDetail;
    pub use rumoca_phase_dae::{balance, balance_detail, equations_unknowns, is_balanced};
}

/// Structural-analysis primitives (BLT sorting, scalarization).
pub mod phase_structural {
    pub use rumoca_phase_structural::scalarize::scalarize_equations;
    pub use rumoca_phase_structural::{
        AlgebraicLoop, BltBlock, CausalStep, EliminationResult, EquationRef, IcBlock,
        IcRelaxationHint, Incidence, SortedDae, StructuralDiagnostics, StructuralError,
        Substitution, TearingResult, UnknownId, analyze_structure, build_blt_from_incidence,
        build_ic_plan, build_ic_relaxation_hint, build_solver_sparsity_triplets,
        runtime_defined_continuous_unknown_names, runtime_defined_unknown_names, sort_dae,
        tear_algebraic_loop,
    };
}

/// Compilation session API and result structures.
pub mod compile {
    pub use rumoca_core as core;
    pub use rumoca_core::{
        Causality as AstCausality, Token as AstToken, VarName, Variability as AstVariability,
    };
    pub use rumoca_ir_ast::ResolvedTree;
    pub use rumoca_ir_ast::{
        Component as AstComponent, ComponentRefPart as AstComponentRefPart,
        ComponentReference as AstComponentReference, Expression as AstExpression,
        ForIndex as AstForIndex, Subscript as AstSubscript,
    };
    pub use rumoca_ir_dae::{Dae, Variable};
    pub use rumoca_ir_flat::Model as FlatModel;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct SourceTextPosition {
        pub line: u32,
        pub character: u32,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct SourceSpanLocation {
        pub file_name: String,
        pub start: SourceTextPosition,
        pub end: SourceTextPosition,
    }

    pub fn source_span_location(
        source_map: &rumoca_core::SourceMap,
        span: rumoca_core::Span,
    ) -> Option<SourceSpanLocation> {
        if span.is_dummy() {
            return None;
        }
        let (file_name, source) = source_map.get_source(span.source)?;
        let start_byte = span.start.0;
        let end_byte = span.end.0;
        if end_byte <= start_byte || end_byte > source.len() {
            return None;
        }
        Some(SourceSpanLocation {
            file_name: file_name.to_string(),
            start: byte_offset_to_position(source, start_byte),
            end: byte_offset_to_position(source, end_byte),
        })
    }

    fn byte_offset_to_position(source: &str, byte_offset: usize) -> SourceTextPosition {
        let clamped = byte_offset.min(source.len());
        let mut line = 0u32;
        let mut character = 0u32;
        for (idx, ch) in source.char_indices() {
            if idx >= clamped {
                break;
            }
            if ch == '\n' {
                line = line.saturating_add(1);
                character = 0;
            } else {
                character = character.saturating_add(ch.len_utf16() as u32);
            }
        }
        SourceTextPosition { line, character }
    }

    pub use crate::instrumentation::{
        SessionCacheStatsSnapshot, reset_session_cache_stats, session_cache_stats,
    };
    pub use crate::session::{
        ClassLocalCompletionItem, ClassLocalCompletionKind, CompilationMode, CompilationResult,
        CompilationSummary, CompilePhaseEvent, CompilePhaseObserverGuard,
        CompilePhaseTimingSnapshot, CompilePhaseTimingStat, CompiledSourceRoot,
        DaeCompilationResult, Document, DocumentSymbol, DocumentSymbolKind, FailedPhase,
        LocalComponentInfo, ModelDiagnostics, ModelFailureDiagnostic, NavigationClassTargetInfo,
        ParsedSourceRootLoad, PhaseResult, SemanticDiagnosticsMode, Session, SessionChange,
        SessionConfig, SessionSnapshot, SourceRootActivityKind, SourceRootActivityPhase,
        SourceRootActivitySnapshot, SourceRootDurability, SourceRootKind, SourceRootLoadMode,
        SourceRootLoadReport, SourceRootStatusSnapshot, StrictCheckTiming, StrictCompileReport,
        WorkspaceSymbol, WorkspaceSymbolKind, WorkspaceSymbolSnapshotTiming,
        compile_phase_timing_stats, install_compile_phase_observer,
        reset_compile_phase_timing_stats,
    };
}

// Root exports intentionally kept minimal to avoid a "god facade".
pub use compile::{Session, SessionConfig};
