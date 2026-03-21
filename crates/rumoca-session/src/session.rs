//! Unified compilation session management.
//!
//! This module provides the Session type that manages compilation state
//! across different frontends (CLI, LSP, WASM).

use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use rayon::prelude::*;
use rumoca_core::{
    DefId, Diagnostic as CommonDiagnostic, Diagnostics as CommonDiagnostics, Label, OptionalTimer,
    PrimaryLabel, SourceMap, Span, maybe_elapsed_duration, maybe_start_timer,
};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_phase_dae::{ToDaeError, ToDaeOptions, to_dae_with_options};
use rumoca_phase_flatten::{FlattenError, FlattenOptions, flatten_ref_with_options};
use rumoca_phase_instantiate::{
    InstantiateError, InstantiationOutcome, instantiate_model_with_outcome,
};
use rumoca_phase_resolve::{ResolveOptions, resolve_with_options, resolve_with_options_collect};
use rumoca_phase_typecheck::typecheck_instanced;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, Once};
use std::time::Duration;

mod session_impl;
mod session_impl_query_indexes;

#[cfg(test)]
mod tests;

use crate::experiment::experiment_settings_for_model;
use crate::instrumentation::{
    CacheInvalidationCause, record_body_semantic_diagnostics_build,
    record_body_semantic_diagnostics_cache_hit, record_body_semantic_diagnostics_cache_miss,
    record_dae_model_build, record_dae_model_cache_hit, record_dae_model_cache_miss,
    record_document_parse, record_document_parse_duration, record_document_parse_error,
    record_file_item_index_query_hit, record_file_item_index_query_miss, record_flat_model_build,
    record_flat_model_cache_hit, record_flat_model_cache_miss, record_instantiated_model_build,
    record_instantiated_model_cache_hit, record_instantiated_model_cache_miss,
    record_interface_semantic_diagnostics_build, record_interface_semantic_diagnostics_cache_hit,
    record_interface_semantic_diagnostics_cache_miss,
    record_model_stage_semantic_diagnostics_build,
    record_model_stage_semantic_diagnostics_cache_hit,
    record_model_stage_semantic_diagnostics_cache_miss, record_namespace_completion_cache_hit,
    record_namespace_completion_cache_miss, record_namespace_completion_state_invalidation,
    record_namespace_index_query_hit, record_namespace_index_query_miss,
    record_namespace_refresh_build, record_namespace_refresh_collect,
    record_namespace_refresh_finalize, record_parsed_file_query_hit, record_parsed_file_query_miss,
    record_recovered_file_query_hit, record_recovered_file_query_miss,
    record_resolved_state_invalidation, record_semantic_navigation_build,
    record_semantic_navigation_cache_hit, record_semantic_navigation_cache_miss,
    record_standard_resolved_build, record_standard_resolved_cache_hit,
    record_strict_resolved_build, record_strict_resolved_state_invalidation,
    record_typed_model_build, record_typed_model_cache_hit, record_typed_model_cache_miss,
};
use crate::merge::{collect_class_type_counts, collect_model_names, merge_stored_definitions};
use crate::source_root_cache::{
    SourceRootCacheStatus, parse_source_root_with_cache_in, resolve_source_root_cache_dir,
};

mod dependency_fingerprint;
use dependency_fingerprint::{CompileCacheEntry, DependencyFingerprintCache, Fingerprint};
mod declaration_index;
use declaration_index::DeclarationIndex;
use declaration_index::ItemKey;
mod class_body;
use class_body::FileClassBodyIndex;
mod file_summary;
use file_summary::FileSummary;
mod file_outline;
use file_outline::FileOutline;
mod semantic_summary_cache;
use semantic_summary_cache::{
    SourceRootSemanticSummary, read_source_root_semantic_summary,
    resolve_semantic_summary_cache_dir_from_root, write_source_root_semantic_summary,
};
mod class_body_semantics;
use class_body_semantics::FileClassBodySemantics;
mod package_def_map;
use package_def_map::PackageDefMap;
mod class_interface;
use class_interface::FileClassInterfaceIndex;
use class_interface::{ClassInterface, ImportMap};
mod namespace_completion;
use namespace_completion::NamespaceCompletionCache;
mod compile_phase_timing;
use compile_phase_timing::maybe_record_compile_phase_timing;
pub use compile_phase_timing::{
    CompilePhaseTimingSnapshot, CompilePhaseTimingStat, compile_phase_timing_stats,
    reset_compile_phase_timing_stats,
};
mod compile_support;
use compile_support::{
    collect_class_component_members, compile_model_internal, compile_phase_result_from_dae,
    dae_model_outcome_from_flat, dae_phase_result_from_dae, diagnostics_from_vec,
    diagnostics_to_anyhow, finalize_strict_compile_report, flat_model_outcome_from_typed,
    is_simulatable_class_type, missing_inner_label, resolve_class_for_completion,
    split_cached_target_results, typed_model_outcome_from_instantiated,
};
mod compiled_source_root;
pub use compiled_source_root::CompiledSourceRoot;
mod diagnostic_adapters;
use diagnostic_adapters::{merge_error_to_common, miette_error_to_common};
mod model_diagnostics;
use model_diagnostics::{
    global_resolution_failure_diagnostics, merge_model_diagnostics, model_diagnostics_for_tree,
    synthesized_inner_warning,
};
mod reachability;
use reachability::{ReachabilityPlanner, ReachableModelClosure};
mod strict_compile_diagnostics;
use strict_compile_diagnostics::{
    class_primary_span, collect_parse_error_diagnostics, collect_parse_failures_for_files,
    collect_resolve_failures_for_files, collect_target_source_files, dae_phase_result_to_failure,
    default_tree_span, document_parse_diagnostics, phase_result_to_failure, same_path,
};
mod session_impl_diagnostics;
mod session_impl_inputs;
mod session_impl_model_queries;
mod session_impl_queries;
mod session_impl_source_root_loads;
mod session_impl_source_roots;
mod session_impl_symbols;
mod session_impl_workspace_symbol_queries;
mod session_snapshot;

static RAYON_INIT: Once = Once::new();
const MAX_SESSION_COMPILE_CACHE_ENTRIES: usize = 256;
const MAX_SESSION_SEMANTIC_NAVIGATION_CACHE_ENTRIES: usize = 64;
const MAX_SESSION_SEMANTIC_DIAGNOSTICS_CACHE_ENTRIES: usize = 64;
const MAX_SESSION_MODEL_QUERY_CACHE_ENTRIES: usize = 64;
const MAX_SESSION_CLASS_MEMBER_QUERY_CACHE_ENTRIES: usize = 128;
const WORKSPACE_SYMBOL_SEARCH_GRAM_LEN: usize = 5;

fn path_lookup_key(path: &str) -> String {
    std::fs::canonicalize(path)
        .map(|resolved| resolved.to_string_lossy().to_string())
        .unwrap_or_else(|_| path.to_string())
}

#[derive(Debug, Clone)]
struct SemanticNavigationArtifact {
    fingerprint: Fingerprint,
    resolved: Arc<ast::ResolvedTree>,
}

#[derive(Debug, Clone)]
struct SemanticDiagnosticsArtifact {
    fingerprint: Fingerprint,
    diagnostics: ModelDiagnostics,
}

#[derive(Debug, Clone)]
struct InterfaceSemanticDiagnosticsArtifact {
    fingerprint: Fingerprint,
    class_type: Option<ast::ClassType>,
}

#[derive(Debug, Clone)]
struct BodySemanticDiagnosticsArtifact {
    fingerprint: Fingerprint,
    diagnostics: ModelDiagnostics,
    blocks_model_stage: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemanticDiagnosticsMode {
    Standard,
    Save,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SemanticDiagnosticsCacheKey {
    model_name: String,
    mode: SemanticDiagnosticsMode,
}

impl SemanticDiagnosticsCacheKey {
    fn new(model_name: &str, mode: SemanticDiagnosticsMode) -> Self {
        Self {
            model_name: model_name.to_string(),
            mode,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ModelKey {
    item_key: ItemKey,
}

impl ModelKey {
    fn new(item_key: ItemKey) -> Self {
        Self { item_key }
    }

    fn qualified_name(&self) -> String {
        self.item_key.qualified_name()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ReachableModelClosureCacheKey {
    model_key: ModelKey,
    mode: ResolveBuildMode,
}

impl ReachableModelClosureCacheKey {
    fn new(model_key: ModelKey, mode: ResolveBuildMode) -> Self {
        Self { model_key, mode }
    }
}

#[derive(Debug, Clone)]
struct ReachableModelClosureArtifact {
    fingerprint: Fingerprint,
    closure: ReachableModelClosure,
}

#[derive(Debug, Clone)]
enum InstantiatedModelOutcome {
    Success(Box<ast::InstanceOverlay>),
    NeedsInner {
        missing_inners: Vec<String>,
        missing_spans: Vec<Span>,
    },
    Error(Box<InstantiateError>),
}

impl InstantiatedModelOutcome {
    fn from_instantiation_outcome(outcome: InstantiationOutcome) -> Self {
        match outcome {
            InstantiationOutcome::Success(overlay) => Self::Success(Box::new(overlay)),
            InstantiationOutcome::NeedsInner {
                missing_inners,
                missing_spans,
                ..
            } => Self::NeedsInner {
                missing_inners,
                missing_spans,
            },
            InstantiationOutcome::Error(error) => Self::Error(error),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InstantiatedModelCacheKey {
    model_key: ModelKey,
    mode: ResolveBuildMode,
}

impl InstantiatedModelCacheKey {
    fn new(model_key: ModelKey, mode: ResolveBuildMode) -> Self {
        Self { model_key, mode }
    }
}

#[derive(Debug, Clone)]
struct InstantiatedModelArtifact {
    fingerprint: Fingerprint,
    outcome: InstantiatedModelOutcome,
}

#[derive(Debug, Clone)]
enum TypedModelOutcome {
    Success(Box<ast::InstanceOverlay>),
    NeedsInner {
        missing_inners: Vec<String>,
        missing_spans: Vec<Span>,
    },
    InstantiateError(Box<InstantiateError>),
    TypecheckError(Vec<CommonDiagnostic>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TypedModelCacheKey {
    model_key: ModelKey,
    mode: ResolveBuildMode,
}

impl TypedModelCacheKey {
    fn new(model_key: ModelKey, mode: ResolveBuildMode) -> Self {
        Self { model_key, mode }
    }
}

#[derive(Debug, Clone)]
struct TypedModelArtifact {
    fingerprint: Fingerprint,
    outcome: TypedModelOutcome,
}

#[derive(Debug, Clone, Default)]
struct TypedModelQueryState {
    artifacts: IndexMap<TypedModelCacheKey, TypedModelArtifact>,
}

#[derive(Debug, Clone)]
struct FlatModelArtifactData {
    flat: flat::Model,
}

#[derive(Debug, Clone)]
enum FlatModelOutcome {
    Success(Box<FlatModelArtifactData>),
    NeedsInner {
        missing_inners: Vec<String>,
        missing_spans: Vec<Span>,
    },
    InstantiateError(Box<InstantiateError>),
    TypecheckError(Vec<CommonDiagnostic>),
    FlattenError {
        error: Box<FlattenError>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FlatModelCacheKey {
    model_key: ModelKey,
    mode: ResolveBuildMode,
}

impl FlatModelCacheKey {
    fn new(model_key: ModelKey, mode: ResolveBuildMode) -> Self {
        Self { model_key, mode }
    }
}

#[derive(Debug, Clone)]
struct FlatModelArtifact {
    fingerprint: Fingerprint,
    outcome: FlatModelOutcome,
}

#[derive(Debug, Clone, Default)]
struct FlatModelQueryState {
    artifacts: IndexMap<FlatModelCacheKey, FlatModelArtifact>,
}

#[derive(Debug, Clone)]
struct DaeModelArtifactData {
    flat: Arc<flat::Model>,
    dae: Arc<dae::Dae>,
}

#[derive(Debug, Clone)]
enum DaeModelOutcome {
    Success(Box<DaeModelArtifactData>),
    NeedsInner {
        missing_inners: Vec<String>,
        missing_spans: Vec<Span>,
    },
    InstantiateError(Box<InstantiateError>),
    TypecheckError(Vec<CommonDiagnostic>),
    FlattenError {
        error: Box<FlattenError>,
    },
    ToDaeError {
        error: Box<ToDaeError>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DaeModelCacheKey {
    model_key: ModelKey,
    mode: ResolveBuildMode,
}

impl DaeModelCacheKey {
    fn new(model_key: ModelKey, mode: ResolveBuildMode) -> Self {
        Self { model_key, mode }
    }
}

#[derive(Debug, Clone)]
struct DaeModelArtifact {
    fingerprint: Fingerprint,
    outcome: DaeModelOutcome,
}

#[derive(Debug, Clone, Default)]
struct SemanticDiagnosticsQueryState {
    resolved_by_mode: IndexMap<SemanticDiagnosticsMode, Arc<ast::ResolvedTree>>,
    resolved_diagnostics_by_mode: IndexMap<SemanticDiagnosticsMode, Vec<CommonDiagnostic>>,
    dependency_fingerprints_by_mode: IndexMap<SemanticDiagnosticsMode, DependencyFingerprintCache>,
    interface_artifacts:
        IndexMap<SemanticDiagnosticsCacheKey, InterfaceSemanticDiagnosticsArtifact>,
    body_artifacts: IndexMap<SemanticDiagnosticsCacheKey, BodySemanticDiagnosticsArtifact>,
    model_stage_artifacts: IndexMap<SemanticDiagnosticsCacheKey, SemanticDiagnosticsArtifact>,
}

impl SemanticDiagnosticsQueryState {
    fn invalidate_inputs(&mut self) {
        self.resolved_by_mode.clear();
        self.resolved_diagnostics_by_mode.clear();
        self.dependency_fingerprints_by_mode.clear();
    }

    fn invalidate_inputs_for_mode(&mut self, mode: SemanticDiagnosticsMode) {
        self.resolved_by_mode.shift_remove(&mode);
        self.resolved_diagnostics_by_mode.shift_remove(&mode);
        self.dependency_fingerprints_by_mode.shift_remove(&mode);
    }
}

/// Initialize rayon thread pool with num_cpus - 1 threads and 16MB stack per thread.
/// This leaves one CPU free for system responsiveness.
/// The large stack size is needed for deep MSL class hierarchies.
fn init_rayon_pool() {
    RAYON_INIT.call_once(|| {
        let num_threads = std::thread::available_parallelism()
            .map(|n| n.get().saturating_sub(1).max(1))
            .unwrap_or(1);
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .stack_size(16 * 1024 * 1024) // 16 MB per thread for deep class hierarchies
            .build_global()
            .ok(); // Ignore error if pool already initialized
    });
}

/// Configuration for a compilation session.
#[derive(Debug, Clone, Default)]
pub struct SessionConfig {
    /// Enable parallel compilation.
    pub parallel: bool,
}

/// Durability/ownership class for a source root tracked by the session.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SourceRootKind {
    /// Open workspace/project files.
    Workspace,
    /// Mutable non-workspace roots loaded from disk or project config.
    #[default]
    External,
    /// Rarely-changing non-workspace roots such as MSL.
    DurableExternal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum SourceRootDurability {
    Volatile,
    #[default]
    Normal,
    Durable,
}

impl SourceRootKind {
    /// Return whether this source root is managed outside the active workspace.
    ///
    /// This does not imply different lookup or compile semantics from
    /// workspace roots; it only selects roots that should be surfaced through
    /// non-workspace status/progress views.
    fn is_non_workspace_root(self) -> bool {
        matches!(self, Self::External | Self::DurableExternal)
    }

    pub(crate) fn durability(self) -> SourceRootDurability {
        match self {
            Self::Workspace => SourceRootDurability::Volatile,
            Self::External => SourceRootDurability::Normal,
            Self::DurableExternal => SourceRootDurability::Durable,
        }
    }
}

#[derive(Debug, Clone)]
enum FileInputChange {
    SetText { uri: String, text: String },
    Remove { uri: String },
}

#[derive(Debug, Clone)]
enum SourceRootInputChange {
    Replace {
        key: String,
        kind: SourceRootKind,
        uris: IndexSet<String>,
    },
    Remove {
        key: String,
    },
}

/// Transactional input change applied to a [`Session`].
#[derive(Debug, Clone, Default)]
pub struct SessionChange {
    file_changes: Vec<FileInputChange>,
    source_root_changes: Vec<SourceRootInputChange>,
}

impl SessionChange {
    /// Record a text update for one file.
    pub fn set_file_text(&mut self, uri: impl Into<String>, text: impl Into<String>) -> &mut Self {
        self.file_changes.push(FileInputChange::SetText {
            uri: uri.into(),
            text: text.into(),
        });
        self
    }

    /// Record removal of one file from the session.
    pub fn remove_file(&mut self, uri: impl Into<String>) -> &mut Self {
        self.file_changes
            .push(FileInputChange::Remove { uri: uri.into() });
        self
    }

    /// Replace membership for one source root.
    pub fn replace_source_root<I, S>(
        &mut self,
        key: impl Into<String>,
        kind: SourceRootKind,
        uris: I,
    ) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        let mut members = IndexSet::new();
        for uri in uris {
            members.insert(uri.into());
        }
        self.source_root_changes
            .push(SourceRootInputChange::Replace {
                key: key.into(),
                kind,
                uris: members,
            });
        self
    }

    /// Remove one source root from the session.
    pub fn remove_source_root(&mut self, key: impl Into<String>) -> &mut Self {
        self.source_root_changes
            .push(SourceRootInputChange::Remove { key: key.into() });
        self
    }

    /// Return `true` when the change carries no mutations.
    pub fn is_empty(&self) -> bool {
        self.file_changes.is_empty() && self.source_root_changes.is_empty()
    }
}

/// Targeted compilation execution mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CompilationMode {
    /// Strict compile semantics for target and reachable dependencies.
    StrictReachable,
    /// Strict compile semantics with internal recovery to collect diagnostics.
    #[default]
    StrictReachableWithRecovery,
    /// Strict compile semantics with internal recovery while bypassing
    /// cross-request compile-cache reuse.
    StrictReachableUncachedWithRecovery,
}

/// Source-root load execution mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SourceRootLoadMode {
    /// Continue loading with partial results when some files fail.
    #[default]
    Tolerant,
}

/// Report for tolerant source-root loading.
#[derive(Debug, Clone)]
pub struct SourceRootLoadReport {
    pub source_set_id: String,
    pub source_root_path: String,
    pub parsed_file_count: usize,
    pub inserted_file_count: usize,
    pub cache_status: Option<SourceRootCacheStatus>,
    pub cache_key: Option<String>,
    pub cache_file: Option<PathBuf>,
    pub diagnostics: Vec<String>,
}

/// Session-owned source-root activity kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SourceRootActivityKind {
    ColdIndexBuild,
    WarmCacheRestore,
    SubtreeReindex,
}

/// Session-owned source-root activity phase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SourceRootActivityPhase {
    Pending,
    Running,
    Completed,
}

/// Generic source-root activity snapshot for thin clients to render.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceRootActivitySnapshot {
    pub kind: SourceRootActivityKind,
    pub phase: SourceRootActivityPhase,
    pub dirty_class_prefixes: Vec<String>,
}

/// Session-owned status view for one source root.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceRootStatusSnapshot {
    pub source_root_key: String,
    pub source_root_path: Option<String>,
    pub current: Option<SourceRootActivitySnapshot>,
    pub last_completed: Option<SourceRootActivitySnapshot>,
}

/// Session-owned subtree refresh plan for one dirty source root.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceRootRefreshPlan {
    pub source_root_key: String,
    pub source_root_path: Option<String>,
    pub dirty_class_prefixes: Vec<String>,
    pub refresh_class_prefixes: Vec<String>,
    pub affected_uris: Vec<String>,
    pub unmatched_class_prefixes: Vec<String>,
    pub rebuild_package_membership: bool,
    pub full_root_fallback: bool,
}

#[derive(Debug, Clone)]
pub struct ParsedSourceRootLoad<'a> {
    pub source_root_kind: SourceRootKind,
    pub source_root_path: &'a Path,
    pub cache_status: SourceRootCacheStatus,
    pub path_key: &'a str,
    pub current_document_path: Option<&'a str>,
    pub documents: Vec<(String, ast::StoredDefinition)>,
    pub expected_epoch: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ResolveBuildMode {
    Standard,
    StrictCompileRecovery,
}

impl ResolveBuildMode {
    fn include_parse_error_diags(self) -> bool {
        matches!(self, Self::Standard)
    }

    fn unresolved_refs_are_errors_in_single_document(self) -> bool {
        matches!(self, Self::Standard)
    }
}

impl SemanticDiagnosticsMode {
    fn resolve_build_mode(self) -> ResolveBuildMode {
        match self {
            Self::Standard => ResolveBuildMode::Standard,
            // Save diagnostics should stay focused on the active target's
            // reachable closure and tolerate unrelated source-root resolve issues.
            Self::Save => ResolveBuildMode::StrictCompileRecovery,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct ResolvedBuildCache {
    standard: Option<Arc<ast::ResolvedTree>>,
    strict_compile_recovery: Option<Arc<ast::ResolvedTree>>,
    strict_compile_recovery_diagnostics: Option<Vec<CommonDiagnostic>>,
}

impl ResolvedBuildCache {
    fn get(&self, mode: ResolveBuildMode) -> Option<&Arc<ast::ResolvedTree>> {
        match mode {
            ResolveBuildMode::Standard => self.standard.as_ref(),
            ResolveBuildMode::StrictCompileRecovery => self.strict_compile_recovery.as_ref(),
        }
    }

    fn set(&mut self, mode: ResolveBuildMode, resolved: Arc<ast::ResolvedTree>) {
        match mode {
            ResolveBuildMode::Standard => self.standard = Some(resolved),
            ResolveBuildMode::StrictCompileRecovery => {
                self.strict_compile_recovery = Some(resolved)
            }
        }
    }

    fn clear(&mut self) {
        self.standard = None;
        self.strict_compile_recovery = None;
        self.strict_compile_recovery_diagnostics = None;
    }

    fn clear_mode(&mut self, mode: ResolveBuildMode) {
        match mode {
            ResolveBuildMode::Standard => self.standard = None,
            ResolveBuildMode::StrictCompileRecovery => {
                self.strict_compile_recovery = None;
                self.strict_compile_recovery_diagnostics = None;
            }
        }
    }

    fn any(&self) -> Option<&Arc<ast::ResolvedTree>> {
        self.standard
            .as_ref()
            .or(self.strict_compile_recovery.as_ref())
    }
}

#[derive(Debug, Clone, Default)]
struct DependencyFingerprintBuildCache {
    standard: Option<DependencyFingerprintCache>,
    strict_compile_recovery: Option<DependencyFingerprintCache>,
}

impl DependencyFingerprintBuildCache {
    fn get_or_insert_with(
        &mut self,
        mode: ResolveBuildMode,
        build: impl FnOnce() -> DependencyFingerprintCache,
    ) -> &mut DependencyFingerprintCache {
        match mode {
            ResolveBuildMode::Standard => self.standard.get_or_insert_with(build),
            ResolveBuildMode::StrictCompileRecovery => {
                self.strict_compile_recovery.get_or_insert_with(build)
            }
        }
    }

    fn clear(&mut self) {
        self.standard = None;
        self.strict_compile_recovery = None;
    }

    fn clear_mode(&mut self, mode: ResolveBuildMode) {
        match mode {
            ResolveBuildMode::Standard => self.standard = None,
            ResolveBuildMode::StrictCompileRecovery => self.strict_compile_recovery = None,
        }
    }

    fn set_all_from_cache(&mut self, cache: &DependencyFingerprintCache) {
        self.standard = Some(cache.clone());
        self.strict_compile_recovery = Some(cache.clone());
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub(crate) struct FileId(u32);

impl FileId {
    fn new(index: u32) -> Self {
        Self(index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(crate) struct SourceSetId(u32);

impl SourceSetId {
    fn new(index: u32) -> Self {
        Self(index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub(crate) struct RevisionId(u64);

impl RevisionId {
    fn new(index: u64) -> Self {
        Self(index)
    }
}

type SummarySignature = IndexMap<FileId, Fingerprint>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceSetQuerySignature {
    Summary(SummarySignature),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SourceSetClassGraphSignature {
    source_set: SourceSetQuerySignature,
    detached: SummarySignature,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SourceRootResolvedAggregate {
    model_names: Vec<String>,
    dependency_fingerprints: DependencyFingerprintCache,
}

#[derive(Debug, Clone)]
struct SourceSetResolvedAggregateQueryCache {
    signature: SourceSetQuerySignature,
    aggregate: SourceRootResolvedAggregate,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct SessionQuerySignature {
    source_sets: IndexMap<SourceSetId, SourceSetQuerySignature>,
    detached: SummarySignature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DocumentQueryFingerprints {
    summary: Fingerprint,
    body: Fingerprint,
    outline: Fingerprint,
    navigation: Fingerprint,
}

impl DocumentQueryFingerprints {
    fn from_definition(definition: &ast::StoredDefinition) -> Self {
        let summary = file_summary::summary_fingerprint(definition);
        let (body, outline_body) = class_body::class_body_fingerprints(definition);
        Self {
            summary,
            body,
            outline: hash_query_fingerprint_pair(summary, outline_body),
            navigation: hash_query_fingerprint_pair(summary, body),
        }
    }
}

fn hash_query_fingerprint_pair(left: Fingerprint, right: Fingerprint) -> Fingerprint {
    let mut hasher = blake3::Hasher::new();
    hasher.update(&left);
    hasher.update(&right);
    *hasher.finalize().as_bytes()
}

#[derive(Debug, Clone)]
pub enum WorkspaceSymbolKind {
    Class(ast::ClassType),
    Component,
}

#[derive(Debug, Clone)]
pub struct WorkspaceSymbol {
    pub name: String,
    pub kind: WorkspaceSymbolKind,
    pub container_name: Option<String>,
    pub location: ast::Location,
    pub uri: String,
}

#[derive(Debug, Clone)]
pub struct DocumentSymbol {
    pub name: String,
    pub detail: Option<String>,
    pub kind: DocumentSymbolKind,
    pub range: ast::Location,
    pub selection_range: ast::Location,
    pub children: Vec<DocumentSymbol>,
}

#[derive(Debug, Clone)]
pub enum DocumentSymbolKind {
    Class(ast::ClassType),
    ParametersSection,
    InputsSection,
    OutputsSection,
    VariablesSection,
    EquationsSection,
    AlgorithmsSection,
    Component,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassLocalCompletionKind {
    Constant,
    Property,
    Variable,
    Class,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassLocalCompletionItem {
    pub name: String,
    pub detail: String,
    pub kind: ClassLocalCompletionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalComponentInfo {
    pub name: String,
    pub type_name: String,
    pub keyword_prefix: Option<String>,
    pub shape: Vec<usize>,
    pub declaration_location: ast::Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NavigationClassTargetInfo {
    pub target_uri: String,
    pub qualified_name: String,
    pub class_name: String,
    pub class_type: ast::ClassType,
    pub description: Option<String>,
    pub component_count: usize,
    pub equation_count: usize,
    pub declaration_location: ast::Location,
}

#[derive(Debug, Clone)]
struct FileOutlineQueryCache {
    fingerprint: Fingerprint,
    outline: FileOutline,
}

#[derive(Debug, Clone)]
struct FileItemIndex {
    fingerprint: Fingerprint,
    symbols: Vec<WorkspaceSymbol>,
}

#[derive(Debug, Clone)]
struct FileSummaryQueryCache {
    fingerprint: Fingerprint,
    summary: FileSummary,
}

#[derive(Debug, Clone)]
struct DeclarationIndexQueryCache {
    fingerprint: Fingerprint,
    index: DeclarationIndex,
}

#[derive(Debug, Clone)]
struct ClassInterfaceQueryCache {
    fingerprint: Fingerprint,
    index: FileClassInterfaceIndex,
}

#[derive(Debug, Clone)]
struct FileClassBodyQueryCache {
    fingerprint: Fingerprint,
    index: FileClassBodyIndex,
}

#[derive(Debug, Clone)]
struct ClassBodySemanticsQueryCache {
    fingerprint: Fingerprint,
    semantics: FileClassBodySemantics,
}

#[derive(Debug, Clone)]
struct ClassComponentMembersQueryCache {
    signature: SummarySignature,
    members: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
struct PackageDefMapQueryCache {
    signature: SourceSetClassGraphSignature,
    def_map: PackageDefMap,
}

#[derive(Debug, Clone)]
struct OrphanPackageDefMapQueryCache {
    signature: SummarySignature,
    def_map: PackageDefMap,
}

#[derive(Debug, Clone)]
struct SessionPackageDefMapQueryCache {
    signature: SessionQuerySignature,
    def_map: PackageDefMap,
}

#[derive(Debug, Clone, Default)]
struct PackageDefMapState {
    source_set_caches: IndexMap<SourceSetId, PackageDefMapQueryCache>,
    orphan_cache: Option<OrphanPackageDefMapQueryCache>,
    session_cache: Option<SessionPackageDefMapQueryCache>,
}

impl PackageDefMapState {
    fn clear(&mut self) {
        self.source_set_caches.clear();
        self.orphan_cache = None;
        self.session_cache = None;
    }

    fn invalidate_source_set(&mut self, _source_set_id: SourceSetId) {
        // Keep per-source-set membership graphs resident so the next query can
        // compare signatures and rebuild lazily instead of cold-dropping the
        // entire root cache entry.
        self.orphan_cache = None;
        self.session_cache = None;
    }

    fn merge_from(&mut self, other: &Self) {
        for (source_set_id, cache) in &other.source_set_caches {
            self.source_set_caches
                .entry(*source_set_id)
                .or_insert_with(|| cache.clone());
        }
        if let Some(orphan_cache) = &other.orphan_cache {
            self.orphan_cache = Some(orphan_cache.clone());
        }
        if let Some(session_cache) = &other.session_cache {
            self.session_cache = Some(session_cache.clone());
        }
    }
}

#[derive(Debug, Clone)]
struct WorkspaceSymbolSearchEntry {
    symbol: WorkspaceSymbol,
    name_lower: String,
}

impl WorkspaceSymbolSearchEntry {
    fn from_symbol(symbol: WorkspaceSymbol) -> Self {
        let name_lower = symbol.name.to_lowercase();
        Self { symbol, name_lower }
    }
}

#[derive(Debug, Clone, Default)]
struct WorkspaceSymbolSearchIndex {
    grams: HashMap<String, Vec<usize>>,
}

impl WorkspaceSymbolSearchIndex {
    fn from_entries(entries: &[WorkspaceSymbolSearchEntry]) -> Self {
        let mut grams = HashMap::<String, Vec<usize>>::new();
        for (index, entry) in entries.iter().enumerate() {
            for gram in search_grams(&entry.name_lower, WORKSPACE_SYMBOL_SEARCH_GRAM_LEN) {
                grams.entry(gram).or_default().push(index);
            }
        }
        Self { grams }
    }

    fn candidate_indices(&self, query: &str) -> Option<Vec<usize>> {
        let grams = search_grams(query, WORKSPACE_SYMBOL_SEARCH_GRAM_LEN);
        if grams.is_empty() {
            return None;
        }

        let mut postings = grams
            .into_iter()
            .map(|gram| self.grams.get(&gram).map(Vec::as_slice).unwrap_or(&[]))
            .collect::<Vec<_>>();
        postings.sort_by_key(|posting| posting.len());
        let first = postings.first()?;
        if first.is_empty() {
            return Some(Vec::new());
        }

        let mut candidates = first.to_vec();
        for posting in postings.iter().skip(1) {
            candidates.retain(|candidate| posting.binary_search(candidate).is_ok());
            if candidates.is_empty() {
                break;
            }
        }
        Some(candidates)
    }
}

fn search_grams(text: &str, gram_len: usize) -> IndexSet<String> {
    let chars = text.chars().collect::<Vec<_>>();
    let mut grams = IndexSet::new();
    if chars.len() < gram_len {
        return grams;
    }
    for window in chars.windows(gram_len) {
        grams.insert(window.iter().collect());
    }
    grams
}

#[derive(Debug, Clone)]
struct SourceSetWorkspaceSymbolCache {
    signature: SourceSetQuerySignature,
    entries: Vec<WorkspaceSymbolSearchEntry>,
    search_index: WorkspaceSymbolSearchIndex,
}

#[derive(Debug, Clone)]
struct DetachedWorkspaceSymbolCache {
    signature: SummarySignature,
    entries: Vec<WorkspaceSymbolSearchEntry>,
    search_index: WorkspaceSymbolSearchIndex,
}

#[derive(Debug, Clone, Default)]
struct WorkspaceSymbolQueryCache {
    signature: SessionQuerySignature,
    source_set_caches: IndexMap<SourceSetId, Arc<SourceSetWorkspaceSymbolCache>>,
    detached_cache: Option<Arc<DetachedWorkspaceSymbolCache>>,
}

impl WorkspaceSymbolQueryCache {
    fn merge_from(&mut self, other: &Self) {
        if self.signature.source_sets.is_empty() && self.signature.detached.is_empty() {
            self.signature = other.signature.clone();
        }
        for (source_set_id, cache) in &other.source_set_caches {
            self.source_set_caches
                .entry(*source_set_id)
                .or_insert_with(|| cache.clone());
        }
        if self.detached_cache.is_none()
            && let Some(detached_cache) = &other.detached_cache
        {
            self.detached_cache = Some(detached_cache.clone());
        }
    }
}

fn merge_index_map_missing<K, V>(target: &mut IndexMap<K, V>, source: &IndexMap<K, V>)
where
    K: Clone + std::hash::Hash + Eq,
    V: Clone,
{
    for (key, value) in source {
        target.entry(key.clone()).or_insert_with(|| value.clone());
    }
}

#[derive(Debug, Clone)]
struct SourceSetRecord {
    id: SourceSetId,
    kind: SourceRootKind,
    durability: SourceRootDurability,
    source_root_path: Option<String>,
    uris: IndexSet<String>,
    revision: RevisionId,
    dirty_class_prefixes: IndexSet<String>,
    needs_refresh: bool,
    activity: SourceRootActivityState,
}

#[derive(Debug, Clone, Default)]
struct SourceRootIndexingCoordinatorState {
    loaded_path_keys: HashSet<String>,
    loading_path_keys: HashMap<String, u64>,
    state_epoch: u64,
    read_prewarm_session_revision: Option<u64>,
}

#[derive(Debug, Clone, Default)]
struct SourceRootActivityState {
    current: Option<SourceRootActivityRecord>,
    last_completed: Option<SourceRootActivityRecord>,
}

#[derive(Debug, Clone)]
struct SourceRootActivityRecord {
    kind: SourceRootActivityKind,
    phase: SourceRootActivityPhase,
    dirty_class_prefixes: IndexSet<String>,
}

impl SourceRootActivityRecord {
    fn pending_reindex(dirty_class_prefixes: &IndexSet<String>) -> Self {
        Self {
            kind: SourceRootActivityKind::SubtreeReindex,
            phase: SourceRootActivityPhase::Pending,
            dirty_class_prefixes: dirty_class_prefixes.clone(),
        }
    }

    fn running(kind: SourceRootActivityKind) -> Self {
        Self {
            kind,
            phase: SourceRootActivityPhase::Running,
            dirty_class_prefixes: IndexSet::new(),
        }
    }

    fn completed(kind: SourceRootActivityKind, dirty_class_prefixes: IndexSet<String>) -> Self {
        Self {
            kind,
            phase: SourceRootActivityPhase::Completed,
            dirty_class_prefixes,
        }
    }

    fn snapshot(&self) -> SourceRootActivitySnapshot {
        SourceRootActivitySnapshot {
            kind: self.kind,
            phase: self.phase,
            dirty_class_prefixes: self.dirty_class_prefixes.iter().cloned().collect(),
        }
    }
}

#[derive(Debug, Clone)]
struct SourceSetNamespaceQueryCache {
    signature: SourceSetClassGraphSignature,
    cache: NamespaceCompletionCache,
}

#[derive(Debug, Clone)]
struct OrphanNamespaceQueryCache {
    signature: SummarySignature,
    cache: NamespaceCompletionCache,
}

#[derive(Debug, Clone)]
struct DetachedSourceRootDocument {
    document: Arc<Document>,
    source_root_keys: IndexSet<String>,
}

#[derive(Debug, Clone, Default)]
struct SourceRootNamespaceCache {
    merged_cache: Option<NamespaceCompletionCache>,
    source_set_caches: IndexMap<SourceSetId, SourceSetNamespaceQueryCache>,
    merged_source_set_signatures: IndexMap<SourceSetId, SourceSetClassGraphSignature>,
    orphan_signature: SummarySignature,
    orphan_cache: Option<OrphanNamespaceQueryCache>,
}

impl SourceRootNamespaceCache {
    fn invalidate_source_set(&mut self, source_set_id: SourceSetId) {
        if self
            .merged_source_set_signatures
            .shift_remove(&source_set_id)
            .is_some()
        {
            self.merged_cache = None;
        }
    }

    fn insert_source_set_cache(
        &mut self,
        source_set_id: SourceSetId,
        entry: SourceSetNamespaceQueryCache,
    ) {
        self.source_set_caches.insert(source_set_id, entry);
        self.merged_cache = None;
        self.merged_source_set_signatures.clear();
    }

    fn store_merged_cache(
        &mut self,
        cache: NamespaceCompletionCache,
        source_set_signatures: IndexMap<SourceSetId, SourceSetClassGraphSignature>,
        orphan_signature: SummarySignature,
    ) {
        self.merged_cache = Some(cache);
        self.merged_source_set_signatures = source_set_signatures;
        self.orphan_signature = orphan_signature;
    }
}

/// AST/index tier owner.
///
/// This tier is invalidated directly by file and source-set revision changes.
#[derive(Debug, Clone, Default)]
struct AstQueryState {
    source_root_namespace_cache: Option<SourceRootNamespaceCache>,
    package_def_map: PackageDefMapState,
    parsed_file_query_revisions: IndexMap<FileId, RevisionId>,
    recovered_file_query_revisions: IndexMap<FileId, RevisionId>,
    file_summary_cache: IndexMap<FileId, FileSummaryQueryCache>,
    file_item_index_cache: IndexMap<FileId, FileItemIndex>,
    declaration_index_cache: IndexMap<FileId, DeclarationIndexQueryCache>,
    class_interface_query_cache: IndexMap<FileId, ClassInterfaceQueryCache>,
    file_class_body_cache: IndexMap<FileId, FileClassBodyQueryCache>,
    class_body_semantics_cache: IndexMap<FileId, ClassBodySemanticsQueryCache>,
    class_component_members_query_cache: IndexMap<String, ClassComponentMembersQueryCache>,
    file_outline_cache: IndexMap<FileId, FileOutlineQueryCache>,
    workspace_symbol_query_cache: Option<WorkspaceSymbolQueryCache>,
}

impl AstQueryState {
    fn record_file_revision(&mut self, file_id: FileId) {
        self.parsed_file_query_revisions.shift_remove(&file_id);
        self.recovered_file_query_revisions.shift_remove(&file_id);
    }

    fn workspace_symbol_snapshot_state_for_detached(
        &self,
        detached_file_ids: &IndexSet<FileId>,
    ) -> Self {
        let mut state = Self {
            workspace_symbol_query_cache: self.workspace_symbol_query_cache.clone(),
            ..Self::default()
        };
        for file_id in detached_file_ids {
            if let Some(cache) = self.file_summary_cache.get(file_id) {
                state.file_summary_cache.insert(*file_id, cache.clone());
            }
            if let Some(cache) = self.file_item_index_cache.get(file_id) {
                state.file_item_index_cache.insert(*file_id, cache.clone());
            }
            if let Some(cache) = self.declaration_index_cache.get(file_id) {
                state
                    .declaration_index_cache
                    .insert(*file_id, cache.clone());
            }
        }
        state
    }

    fn workspace_symbol_rebuild_snapshot_state(
        &self,
        detached_file_ids: &IndexSet<FileId>,
    ) -> Self {
        let mut state = Self {
            workspace_symbol_query_cache: self.workspace_symbol_query_cache.clone(),
            file_item_index_cache: self.file_item_index_cache.clone(),
            ..Self::default()
        };
        for file_id in detached_file_ids {
            if let Some(cache) = self.file_summary_cache.get(file_id) {
                state.file_summary_cache.insert(*file_id, cache.clone());
            }
            if let Some(cache) = self.declaration_index_cache.get(file_id) {
                state
                    .declaration_index_cache
                    .insert(*file_id, cache.clone());
            }
        }
        state
    }

    fn merge_from(&mut self, other: &Self) {
        if let Some(cache) = &other.source_root_namespace_cache {
            self.source_root_namespace_cache = Some(cache.clone());
        }
        self.package_def_map.merge_from(&other.package_def_map);
        merge_index_map_missing(
            &mut self.parsed_file_query_revisions,
            &other.parsed_file_query_revisions,
        );
        merge_index_map_missing(
            &mut self.recovered_file_query_revisions,
            &other.recovered_file_query_revisions,
        );
        merge_index_map_missing(&mut self.file_summary_cache, &other.file_summary_cache);
        merge_index_map_missing(
            &mut self.file_item_index_cache,
            &other.file_item_index_cache,
        );
        merge_index_map_missing(
            &mut self.declaration_index_cache,
            &other.declaration_index_cache,
        );
        merge_index_map_missing(
            &mut self.class_interface_query_cache,
            &other.class_interface_query_cache,
        );
        merge_index_map_missing(
            &mut self.file_class_body_cache,
            &other.file_class_body_cache,
        );
        merge_index_map_missing(
            &mut self.class_body_semantics_cache,
            &other.class_body_semantics_cache,
        );
        merge_index_map_missing(
            &mut self.class_component_members_query_cache,
            &other.class_component_members_query_cache,
        );
        merge_index_map_missing(&mut self.file_outline_cache, &other.file_outline_cache);
        match (
            self.workspace_symbol_query_cache.as_mut(),
            other.workspace_symbol_query_cache.as_ref(),
        ) {
            (Some(existing), Some(other_cache)) => existing.merge_from(other_cache),
            (None, Some(other_cache)) => {
                self.workspace_symbol_query_cache = Some(other_cache.clone());
            }
            _ => {}
        }
    }
}

/// Resolved tier owner.
///
/// Downstream model artifacts reuse the fingerprints derived from this tier, so
/// it is the explicit recomputation boundary between parse/index and model work.
#[derive(Debug, Clone, Default)]
struct ResolvedArtifactState {
    model_names: Vec<String>,
    builds: ResolvedBuildCache,
    dependency_fingerprints: DependencyFingerprintBuildCache,
    source_set_aggregates: IndexMap<SourceSetId, SourceSetResolvedAggregateQueryCache>,
    reachable_model_closures:
        IndexMap<ReachableModelClosureCacheKey, ReachableModelClosureArtifact>,
    semantic_navigation: IndexMap<String, SemanticNavigationArtifact>,
}

impl ResolvedArtifactState {
    fn clear(&mut self) {
        self.model_names.clear();
        self.builds.clear();
        self.dependency_fingerprints.clear();
    }

    fn clear_mode(&mut self, mode: ResolveBuildMode) {
        self.builds.clear_mode(mode);
        self.dependency_fingerprints.clear_mode(mode);
    }

    fn invalidate_source_set_aggregate(&mut self, source_set_id: SourceSetId) {
        self.source_set_aggregates.shift_remove(&source_set_id);
    }
}

/// Flattened/model-diagnostics tier owner.
///
/// This tier carries instantiated/typed/flat artifacts plus body/model-stage
/// semantic diagnostics keyed by resolved fingerprints.
#[derive(Debug, Clone, Default)]
struct FlattenedArtifactState {
    instantiated_models: IndexMap<InstantiatedModelCacheKey, InstantiatedModelArtifact>,
    typed_models: TypedModelQueryState,
    flat_models: FlatModelQueryState,
    semantic_diagnostics: SemanticDiagnosticsQueryState,
}

impl FlattenedArtifactState {
    fn invalidate_diagnostics_inputs(&mut self) {
        self.semantic_diagnostics.invalidate_inputs();
    }

    fn invalidate_diagnostics_inputs_for_mode(&mut self, mode: SemanticDiagnosticsMode) {
        self.semantic_diagnostics.invalidate_inputs_for_mode(mode);
    }
}

/// DAE/result tier owner.
///
/// Phase-local DAE artifacts and terminal compile results live here and are
/// keyed by the dependency fingerprints produced by the resolved tier.
#[derive(Debug, Clone, Default)]
struct DaeArtifactState {
    dae_models: IndexMap<DaeModelCacheKey, DaeModelArtifact>,
    compile_results: IndexMap<String, CompileCacheEntry>,
}

#[derive(Debug, Clone, Default)]
struct SessionQueryState {
    ast: AstQueryState,
    resolved: ResolvedArtifactState,
    flat: FlattenedArtifactState,
    dae: DaeArtifactState,
}

#[derive(Debug, Default)]
struct SharedSessionSnapshot {
    revision: RevisionId,
    snapshot: Option<SessionSnapshot>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct WorkspaceSymbolSnapshotTiming {
    cache_hit: bool,
    used_source_set_rebuild_snapshot: bool,
    source_set_validation_ms: u64,
    source_set_documents_ms: u64,
    detached_documents_ms: u64,
    detached_file_ids_ms: u64,
    source_set_signatures_ms: u64,
    ast_state_ms: u64,
    session_assemble_ms: u64,
}

impl std::fmt::Display for WorkspaceSymbolSnapshotTiming {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "hit={} rebuild={} validate={}ms srcdocs={}ms docs={}ms ids={}ms sigs={}ms ast={}ms assemble={}ms",
            self.cache_hit as u8,
            self.used_source_set_rebuild_snapshot as u8,
            self.source_set_validation_ms,
            self.source_set_documents_ms,
            self.detached_documents_ms,
            self.detached_file_ids_ms,
            self.source_set_signatures_ms,
            self.ast_state_ms,
            self.session_assemble_ms,
        )
    }
}

/// A document in the session.
#[derive(Debug, Clone)]
pub struct Document {
    /// Document URI or file path.
    pub uri: String,
    /// Source content.
    pub content: String,
    syntax: Arc<crate::parse::SyntaxFile>,
    query_fingerprints: DocumentQueryFingerprints,
}

impl Document {
    pub(crate) fn new(uri: String, content: String, syntax: crate::parse::SyntaxFile) -> Self {
        let query_fingerprints = DocumentQueryFingerprints::from_definition(syntax.best_effort());
        Self {
            uri,
            content,
            syntax: Arc::new(syntax),
            query_fingerprints,
        }
    }

    pub fn from_parsed(uri: String, content: String, parsed: ast::StoredDefinition) -> Self {
        Self::new(uri, content, crate::parse::SyntaxFile::from_parsed(parsed))
    }

    pub fn parsed(&self) -> Option<&ast::StoredDefinition> {
        self.syntax.parsed()
    }

    pub fn recovered(&self) -> Option<&ast::StoredDefinition> {
        self.syntax.recovered()
    }

    pub fn best_effort(&self) -> &ast::StoredDefinition {
        self.syntax.best_effort()
    }

    pub fn parse_errors(&self) -> &[crate::parse::ParseError] {
        self.syntax.parse_errors()
    }

    pub fn parse_error(&self) -> Option<&str> {
        self.syntax.parse_error()
    }

    pub(crate) fn summary_fingerprint(&self) -> Fingerprint {
        self.query_fingerprints.summary
    }

    pub(crate) fn body_fingerprint(&self) -> Fingerprint {
        self.query_fingerprints.body
    }

    pub(crate) fn outline_fingerprint(&self) -> Fingerprint {
        self.query_fingerprints.outline
    }

    pub(crate) fn navigation_fingerprint(&self) -> Fingerprint {
        self.query_fingerprints.navigation
    }
}

/// Result of compiling a single model.
#[derive(Debug, Clone)]
pub struct CompilationResult {
    /// The flattened representation.
    pub flat: flat::Model,
    /// The final DAE representation.
    pub dae: dae::Dae,
    /// Optional simulation start time from `annotation(experiment(StartTime=...))`
    /// on the compiled root class.
    pub experiment_start_time: Option<f64>,
    /// Optional simulation horizon from `annotation(experiment(StopTime=...))`
    /// on the compiled root class.
    pub experiment_stop_time: Option<f64>,
    /// Optional simulation tolerance from `annotation(experiment(Tolerance=...))`.
    pub experiment_tolerance: Option<f64>,
    /// Optional output interval from `annotation(experiment(Interval=...))`.
    pub experiment_interval: Option<f64>,
    /// Optional solver/algorithm hint from experiment annotations.
    pub experiment_solver: Option<String>,
}

/// Result of compiling a single model through the DAE stage only.
#[derive(Debug, Clone)]
pub struct DaeCompilationResult {
    /// The final DAE representation.
    pub dae: Arc<dae::Dae>,
    /// Optional simulation start time from `annotation(experiment(StartTime=...))`
    /// on the compiled root class.
    pub experiment_start_time: Option<f64>,
    /// Optional simulation horizon from `annotation(experiment(StopTime=...))`
    /// on the compiled root class.
    pub experiment_stop_time: Option<f64>,
    /// Optional simulation tolerance from `annotation(experiment(Tolerance=...))`.
    pub experiment_tolerance: Option<f64>,
    /// Optional output interval from `annotation(experiment(Interval=...))`.
    pub experiment_interval: Option<f64>,
    /// Optional solver/algorithm hint from experiment annotations.
    pub experiment_solver: Option<String>,
}

/// Diagnostics collected for a model compilation attempt.
#[derive(Debug, Clone, Default)]
pub struct ModelDiagnostics {
    pub diagnostics: Vec<CommonDiagnostic>,
    pub source_map: Option<SourceMap>,
    pub global_resolution_failure: bool,
}

/// Failure diagnostic for a single model in a strict-reachable-with-recovery pass.
#[derive(Debug, Clone)]
pub struct ModelFailureDiagnostic {
    pub model_name: String,
    pub phase: Option<FailedPhase>,
    pub error_code: Option<String>,
    pub error: String,
    pub primary_label: Option<Label>,
}

/// Report type from strict-reachable-with-recovery compilation.
///
/// The requested model remains strict: it must compile successfully for callers
/// to treat the compile as successful. Other related models are still compiled
/// so additional diagnostics can be surfaced to the user.
#[derive(Debug)]
pub struct StrictCompileReport {
    pub requested_model: String,
    pub requested_result: Option<PhaseResult>,
    pub summary: CompilationSummary,
    pub failures: Vec<ModelFailureDiagnostic>,
    pub source_map: Option<SourceMap>,
}

impl StrictCompileReport {
    /// Returns true when strict compile succeeded for the requested closure.
    pub fn requested_succeeded(&self) -> bool {
        matches!(self.requested_result, Some(PhaseResult::Success(_))) && self.failures.is_empty()
    }

    /// Build a concise failure summary for user-facing diagnostics.
    pub fn failure_summary(&self, max_related: usize) -> String {
        let requested = match &self.requested_result {
            Some(PhaseResult::Success(_)) => {
                format!("{} compiled successfully", self.requested_model)
            }
            Some(PhaseResult::NeedsInner { missing_inners }) => format!(
                "{} requires inner declarations: {}",
                self.requested_model,
                missing_inners.join(", ")
            ),
            Some(PhaseResult::Failed { phase, error, .. }) => {
                format!("{} failed in {}: {}", self.requested_model, phase, error)
            }
            None => requested_missing_result_message(&self.requested_model, &self.failures),
        };

        format_strict_failure_summary(
            &self.requested_model,
            requested,
            &self.failures,
            max_related,
        )
    }
}

fn requested_missing_result_message(
    requested_model: &str,
    failures: &[ModelFailureDiagnostic],
) -> String {
    failures
        .first()
        .map(|failure| format!("{requested_model} could not be compiled: {}", failure.error))
        .unwrap_or_else(|| {
            format!("{requested_model} could not be compiled because resolve/parse failed")
        })
}

pub(crate) fn format_strict_failure_summary(
    requested_model: &str,
    requested: String,
    failures: &[ModelFailureDiagnostic],
    max_related: usize,
) -> String {
    let related: Vec<_> = failures
        .iter()
        .filter(|failure| failure.model_name != requested_model)
        .take(max_related)
        .collect();

    if related.is_empty() {
        return requested;
    }

    let mut lines = Vec::with_capacity(2 + related.len());
    lines.push(requested);
    lines.push(format!("Related failures (showing {}):", related.len()));
    for failure in related {
        let phase = failure
            .phase
            .map(|phase| phase.to_string())
            .unwrap_or_else(|| "Resolve".to_string());
        lines.push(format!(
            "- {} [{}]: {}",
            failure.model_name, phase, failure.error
        ));
    }
    lines.join("\n")
}

impl CompilationResult {
    /// Check if the model is balanced (equal equations and unknowns).
    pub fn is_balanced(&self) -> bool {
        rumoca_sim::dae_is_balanced(&self.dae)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum DaePhaseResult {
    Success(Box<DaeCompilationResult>),
    NeedsInner {
        missing_inners: Vec<String>,
    },
    Failed {
        phase: FailedPhase,
        error: String,
        error_code: Option<String>,
    },
}

/// Phase at which compilation failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailedPhase {
    Instantiate,
    Typecheck,
    Flatten,
    ToDae,
}

impl std::fmt::Display for FailedPhase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FailedPhase::Instantiate => write!(f, "Instantiate"),
            FailedPhase::Typecheck => write!(f, "Typecheck"),
            FailedPhase::Flatten => write!(f, "Flatten"),
            FailedPhase::ToDae => write!(f, "ToDae"),
        }
    }
}

/// Result of compiling a model with phase-level tracking.
#[derive(Debug, Clone)]
pub enum PhaseResult {
    /// Compilation succeeded.
    Success(Box<CompilationResult>),

    /// flat::Model needs inner declarations (has outer without inner).
    NeedsInner {
        /// Names of outer components that need inner declarations.
        missing_inners: Vec<String>,
    },

    /// Compilation failed at a specific phase.
    Failed {
        phase: FailedPhase,
        error: String,
        error_code: Option<String>,
    },
}

impl PhaseResult {
    /// Returns true if this is a successful compilation.
    pub fn is_success(&self) -> bool {
        matches!(self, Self::Success(_))
    }

    /// Returns true if this model needs inner declarations.
    pub fn needs_inner(&self) -> bool {
        matches!(self, Self::NeedsInner { .. })
    }

    /// Returns true if this is an actual failure.
    pub fn is_failed(&self) -> bool {
        matches!(self, Self::Failed { .. })
    }
}

/// Summary statistics for bulk compilation.
#[derive(Debug, Clone, Default)]
pub struct CompilationSummary {
    /// Number of models that compiled successfully.
    pub success_count: usize,
    /// Number of models that need inner declarations.
    pub needs_inner_count: usize,
    /// Number of models that failed at instantiate phase.
    pub instantiate_failures: usize,
    /// Number of models that failed at typecheck phase.
    pub typecheck_failures: usize,
    /// Number of models that failed at flatten phase.
    pub flatten_failures: usize,
    /// Number of models that failed at todae phase.
    pub todae_failures: usize,
    /// Number of balanced models (out of successful compilations).
    pub balanced_count: usize,
}

impl CompilationSummary {
    /// Create a summary from compilation results.
    pub fn from_results(results: &[(String, PhaseResult)]) -> Self {
        let mut summary = Self::default();
        for (_, result) in results {
            summary.add_result(result);
        }
        summary
    }

    fn add_result(&mut self, result: &PhaseResult) {
        match result {
            PhaseResult::Success(r) => {
                self.success_count += 1;
                if r.is_balanced() {
                    self.balanced_count += 1;
                }
            }
            PhaseResult::NeedsInner { .. } => {
                self.needs_inner_count += 1;
            }
            PhaseResult::Failed { phase, .. } => match phase {
                FailedPhase::Instantiate => self.instantiate_failures += 1,
                FailedPhase::Typecheck => self.typecheck_failures += 1,
                FailedPhase::Flatten => self.flatten_failures += 1,
                FailedPhase::ToDae => self.todae_failures += 1,
            },
        }
    }

    /// Total number of models processed.
    pub fn total(&self) -> usize {
        self.success_count
            + self.needs_inner_count
            + self.instantiate_failures
            + self.typecheck_failures
            + self.flatten_failures
            + self.todae_failures
    }

    /// Percentage of models that compiled successfully.
    pub fn success_rate(&self) -> f64 {
        if self.total() == 0 {
            0.0
        } else {
            (self.success_count as f64 / self.total() as f64) * 100.0
        }
    }
}

/// A compilation session that manages documents and compilation state.
///
/// The session provides a unified interface for:
/// - Managing open documents
/// - Parsing and merging Modelica files
/// - Compiling models to DAE form
/// - Parallel compilation support
#[derive(Debug, Clone)]
pub struct Session {
    documents: IndexMap<String, Arc<Document>>,
    detached_document_uris: IndexSet<String>,
    detached_source_root_documents: IndexMap<FileId, DetachedSourceRootDocument>,
    /// Non-workspace parsed document groups (e.g., loaded source roots) keyed by source-set id.
    source_sets: IndexMap<String, SourceSetRecord>,
    /// Stable file ids for all seen URIs during the session lifetime.
    file_ids: IndexMap<String, FileId>,
    /// Stable path-key lookup for all seen file ids during the session lifetime.
    file_path_keys: IndexMap<String, FileId>,
    /// Reverse lookup from stable file ids to URI.
    file_uris: IndexMap<FileId, String>,
    /// Reverse lookup from stable source-set ids to source-set keys.
    source_set_keys: IndexMap<SourceSetId, String>,
    /// Attached file membership for each source-set id.
    file_source_sets: IndexMap<FileId, IndexSet<SourceSetId>>,
    /// Snapshot-only source-set signatures when the source-set records
    /// themselves are intentionally omitted for a narrower read view.
    source_set_signature_overrides: IndexMap<SourceSetId, SourceSetQuerySignature>,
    /// Last input revision that touched a file id.
    file_revisions: IndexMap<FileId, RevisionId>,
    /// Next available file id.
    next_file_id: u32,
    /// Next available source-set id.
    next_source_set_id: u32,
    /// The current input revision for the session.
    current_revision: RevisionId,
    /// Monotonic counter used to allocate new revisions.
    next_revision: u64,
    /// Session-owned source-root indexing coordinator state for thin clients.
    source_root_indexing: SourceRootIndexingCoordinatorState,
    /// All incremental ownership state for AST, resolved, flattened, and DAE tiers.
    query_state: SessionQueryState,
    /// Shared immutable snapshot for the current revision so read requests
    /// do not reclone the full session on every query.
    snapshot_cache: Arc<Mutex<SharedSessionSnapshot>>,
    /// Shared lightweight snapshot for local document-scoped IDE requests.
    lightweight_snapshot_cache: Arc<Mutex<SharedSessionSnapshot>>,
    /// Shared medium-weight snapshot for global workspace symbol reads.
    workspace_symbol_snapshot_cache: Arc<Mutex<SharedSessionSnapshot>>,
}

/// Immutable query snapshot cloned from one host session revision.
///
/// This is the phase-10 analysis boundary: the mutable host owns input changes,
/// while snapshots serve IDE-style read queries against one fixed revision.
#[derive(Debug, Clone)]
pub struct SessionSnapshot {
    session: Arc<Mutex<Session>>,
}
