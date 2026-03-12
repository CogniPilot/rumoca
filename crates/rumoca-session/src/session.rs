//! Unified compilation session management.
//!
//! This module provides the Session type that manages compilation state
//! across different frontends (CLI, LSP, WASM).

use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use rayon::prelude::*;
use rumoca_core::{
    DefId, Diagnostic as CommonDiagnostic, Diagnostics as CommonDiagnostics, Label, PrimaryLabel,
    SourceMap, Span,
};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_phase_dae::{ToDaeOptions, to_dae_with_options};
use rumoca_phase_flatten::{FlattenOptions, flatten_ref_with_options};
use rumoca_phase_instantiate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_phase_resolve::{ResolveOptions, resolve_with_options, resolve_with_options_collect};
use rumoca_phase_typecheck::typecheck_instanced;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, Once};
use std::time::{Duration, Instant};

use crate::experiment::experiment_settings_for_model;
use crate::library_cache::{
    LibraryCacheStatus, parse_library_with_cache_in, resolve_library_cache_dir,
};
use crate::merge::{collect_class_type_counts, collect_model_names, merge_stored_definitions};

mod dependency_fingerprint;
use dependency_fingerprint::{CompileCacheEntry, DependencyFingerprintCache, Fingerprint};
mod compile_support;
use compile_support::{
    collect_class_component_members, compile_model_internal, diagnostics_from_vec,
    diagnostics_to_anyhow, finalize_strict_compile_report, flatten_options_for_tree,
    is_simulatable_class_type, missing_inner_label, resolve_class_for_completion,
    split_cached_target_results, todae_options_for_tree,
};
mod diagnostic_adapters;
use diagnostic_adapters::{merge_error_to_common, miette_error_to_common};
mod model_diagnostics;
use model_diagnostics::{
    global_resolution_failure_diagnostics, model_diagnostics_for_tree, synthesized_inner_warning,
};
mod reachability;
use reachability::ReachabilityPlanner;
mod strict_compile_diagnostics;
use strict_compile_diagnostics::{
    class_primary_span, collect_parse_error_diagnostics, collect_parse_failures_for_files,
    collect_resolve_failures_for_files, collect_target_source_files, default_tree_span,
    document_parse_diagnostics, phase_result_to_failure, same_path,
};

static RAYON_INIT: Once = Once::new();

/// Aggregate timing for a compilation phase across all model compiles.
#[derive(Debug, Clone, Copy, Default)]
pub struct CompilePhaseTimingStat {
    /// Number of times this phase executed.
    pub calls: u64,
    /// Total wall-clock time spent in this phase.
    pub total_nanos: u64,
}

impl CompilePhaseTimingStat {
    pub fn total_seconds(self) -> f64 {
        self.total_nanos as f64 / 1_000_000_000.0
    }
}

/// Snapshot of compile phase timing accumulators.
#[derive(Debug, Clone, Copy, Default)]
pub struct CompilePhaseTimingSnapshot {
    pub instantiate: CompilePhaseTimingStat,
    pub typecheck: CompilePhaseTimingStat,
    pub flatten: CompilePhaseTimingStat,
    pub todae: CompilePhaseTimingStat,
}

static INSTANTIATE_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static INSTANTIATE_CALLS: AtomicU64 = AtomicU64::new(0);
static TYPECHECK_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static TYPECHECK_CALLS: AtomicU64 = AtomicU64::new(0);
static FLATTEN_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static FLATTEN_CALLS: AtomicU64 = AtomicU64::new(0);
static TODAE_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static TODAE_CALLS: AtomicU64 = AtomicU64::new(0);

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

/// Library/workspace indexing execution mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IndexingMode {
    /// Continue indexing with partial results when some files fail.
    #[default]
    Tolerant,
}

/// Report for tolerant library indexing.
#[derive(Debug, Clone)]
pub struct IndexingReport {
    pub source_set_id: String,
    pub library_path: String,
    pub indexed_file_count: usize,
    pub inserted_file_count: usize,
    pub cache_status: Option<LibraryCacheStatus>,
    pub cache_key: Option<String>,
    pub cache_file: Option<PathBuf>,
    pub diagnostics: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Default)]
struct ResolvedBuildCache {
    standard: Option<Arc<ast::ResolvedTree>>,
    strict_compile_recovery: Option<Arc<ast::ResolvedTree>>,
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
    }

    fn clear_mode(&mut self, mode: ResolveBuildMode) {
        match mode {
            ResolveBuildMode::Standard => self.standard = None,
            ResolveBuildMode::StrictCompileRecovery => self.strict_compile_recovery = None,
        }
    }

    fn any(&self) -> Option<&Arc<ast::ResolvedTree>> {
        self.standard
            .as_ref()
            .or(self.strict_compile_recovery.as_ref())
    }
}

/// A document in the session.
#[derive(Debug, Clone)]
pub struct Document {
    /// Document URI or file path.
    pub uri: String,
    /// Source content.
    pub content: String,
    /// Parsed definition (cached, None if parsing failed).
    pub parsed: Option<ast::StoredDefinition>,
    /// Recovery-parsed structure used only for strict compile closure planning.
    pub partial: Option<ast::StoredDefinition>,
    /// Structured parse errors for the current document contents.
    pub parse_errors: Vec<crate::parse::ParseError>,
    /// Parse error (if parsing failed).
    pub parse_error: Option<String>,
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
            None => self
                .failures
                .first()
                .map(|failure| {
                    format!(
                        "{} could not be compiled: {}",
                        self.requested_model, failure.error
                    )
                })
                .unwrap_or_else(|| {
                    format!(
                        "{} could not be compiled because resolve/parse failed",
                        self.requested_model
                    )
                }),
        };

        let related: Vec<_> = self
            .failures
            .iter()
            .filter(|f| f.model_name != self.requested_model)
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
                .map(|p| p.to_string())
                .unwrap_or_else(|| "Resolve".to_string());
            lines.push(format!(
                "- {} [{}]: {}",
                failure.model_name, phase, failure.error
            ));
        }
        lines.join("\n")
    }
}

impl CompilationResult {
    /// Check if the model is balanced (equal equations and unknowns).
    pub fn is_balanced(&self) -> bool {
        rumoca_sim::dae_is_balanced(&self.dae)
    }
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

fn duration_to_nanos(duration: Duration) -> u64 {
    let nanos = duration.as_nanos();
    if nanos > u128::from(u64::MAX) {
        u64::MAX
    } else {
        nanos as u64
    }
}

fn phase_stat(total_nanos: &AtomicU64, calls: &AtomicU64) -> CompilePhaseTimingStat {
    CompilePhaseTimingStat {
        calls: calls.load(Ordering::Relaxed),
        total_nanos: total_nanos.load(Ordering::Relaxed),
    }
}

fn record_compile_phase_timing(phase: FailedPhase, elapsed: Duration) {
    let nanos = duration_to_nanos(elapsed);
    match phase {
        FailedPhase::Instantiate => {
            INSTANTIATE_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            INSTANTIATE_CALLS.fetch_add(1, Ordering::Relaxed);
        }
        FailedPhase::Typecheck => {
            TYPECHECK_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            TYPECHECK_CALLS.fetch_add(1, Ordering::Relaxed);
        }
        FailedPhase::Flatten => {
            FLATTEN_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            FLATTEN_CALLS.fetch_add(1, Ordering::Relaxed);
        }
        FailedPhase::ToDae => {
            TODAE_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            TODAE_CALLS.fetch_add(1, Ordering::Relaxed);
        }
    }
}

#[inline]
fn maybe_start_timer() -> Option<Instant> {
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[inline]
fn maybe_record_compile_phase_timing(phase: FailedPhase, start: Option<Instant>) {
    if let Some(start) = start {
        record_compile_phase_timing(phase, start.elapsed());
    }
}

/// Reset global compile phase timing accumulators.
pub fn reset_compile_phase_timing_stats() {
    INSTANTIATE_TOTAL_NANOS.store(0, Ordering::Relaxed);
    INSTANTIATE_CALLS.store(0, Ordering::Relaxed);
    TYPECHECK_TOTAL_NANOS.store(0, Ordering::Relaxed);
    TYPECHECK_CALLS.store(0, Ordering::Relaxed);
    FLATTEN_TOTAL_NANOS.store(0, Ordering::Relaxed);
    FLATTEN_CALLS.store(0, Ordering::Relaxed);
    TODAE_TOTAL_NANOS.store(0, Ordering::Relaxed);
    TODAE_CALLS.store(0, Ordering::Relaxed);
}

/// Snapshot global compile phase timing accumulators.
pub fn compile_phase_timing_stats() -> CompilePhaseTimingSnapshot {
    CompilePhaseTimingSnapshot {
        instantiate: phase_stat(&INSTANTIATE_TOTAL_NANOS, &INSTANTIATE_CALLS),
        typecheck: phase_stat(&TYPECHECK_TOTAL_NANOS, &TYPECHECK_CALLS),
        flatten: phase_stat(&FLATTEN_TOTAL_NANOS, &FLATTEN_CALLS),
        todae: phase_stat(&TODAE_TOTAL_NANOS, &TODAE_CALLS),
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
pub struct Session {
    documents: IndexMap<String, Document>,
    /// Non-workspace parsed document groups (e.g., loaded libraries) keyed by source-set id.
    source_sets: IndexMap<String, IndexSet<String>>,
    /// Cached resolved trees by resolve build mode (invalidated on document changes).
    resolved_cache: ResolvedBuildCache,
    /// Cached model names.
    pub(crate) model_names: Vec<String>,
    /// Cached model dependency fingerprints for the current resolved snapshot.
    dependency_fingerprints: Option<DependencyFingerprintCache>,
    /// Cached compile outputs keyed by dependency fingerprint.
    compile_cache: IndexMap<String, CompileCacheEntry>,
}

impl Session {
    /// Create a new compilation session.
    pub fn new(config: SessionConfig) -> Self {
        // Initialize rayon thread pool if parallel mode is enabled
        if config.parallel {
            init_rayon_pool();
        }
        Self {
            documents: IndexMap::new(),
            source_sets: IndexMap::new(),
            resolved_cache: ResolvedBuildCache::default(),
            model_names: Vec::new(),
            dependency_fingerprints: None,
            compile_cache: IndexMap::new(),
        }
    }

    fn invalidate_resolved_state(&mut self) {
        self.resolved_cache.clear();
        self.model_names.clear();
        self.dependency_fingerprints = None;
    }

    fn invalidate_strict_compile_state(&mut self) {
        self.resolved_cache
            .clear_mode(ResolveBuildMode::StrictCompileRecovery);
        self.dependency_fingerprints = None;
    }

    fn detach_uri_from_source_sets(&mut self, uri: &str) {
        let mut removable_docs: Vec<String> = Vec::new();
        for uris in self.source_sets.values_mut() {
            let matches: Vec<String> = uris
                .iter()
                .filter(|candidate| same_path(candidate, uri))
                .cloned()
                .collect();
            removable_docs.extend(matches.iter().filter(|matched| *matched != uri).cloned());
            for matched in matches {
                uris.shift_remove(&matched);
            }
        }

        for doc_uri in removable_docs {
            let should_remove = self
                .documents
                .get(&doc_uri)
                .is_some_and(|doc| doc.content.is_empty());
            if should_remove {
                self.documents.shift_remove(&doc_uri);
            }
        }
    }

    /// Add or update a document in the session.
    ///
    /// Returns an error if parsing fails. For LSP use where you want to store
    /// documents even on parse failure, use [`Session::update_document`] instead.
    pub fn add_document(&mut self, uri: &str, content: &str) -> Result<()> {
        let parsed = rumoca_phase_parse::parse_to_ast(content, uri)?;
        self.detach_uri_from_source_sets(uri);
        self.documents.insert(
            uri.to_string(),
            Document {
                uri: uri.to_string(),
                content: content.to_string(),
                parsed: Some(parsed),
                partial: None,
                parse_errors: Vec::new(),
                parse_error: None,
            },
        );
        // Invalidate cached state
        self.invalidate_resolved_state();
        Ok(())
    }

    /// Update a document, storing it even if parsing fails.
    ///
    /// This is designed for LSP use where documents need to be tracked
    /// even when they contain syntax errors. Returns the parse error if any.
    pub fn update_document(&mut self, uri: &str, content: &str) -> Option<String> {
        if let Some(existing) = self.documents.get(uri)
            && existing.content == content
        {
            return existing.parse_error.clone();
        }
        let previous_parsed = self.documents.get(uri).and_then(|doc| doc.parsed.clone());
        self.detach_uri_from_source_sets(uri);
        let (parsed, partial, parse_errors, parse_error, invalidate_resolved) =
            match rumoca_phase_parse::parse_to_ast_with_errors(content, uri) {
                Ok(def) => (Some(def), None, Vec::new(), None, true),
                Err(errors) => {
                    // Keep last successful parse for semantic features (completion/hover) while
                    // the user is typing an incomplete statement.
                    let fallback = previous_parsed;
                    let should_invalidate = fallback.is_none();
                    let partial = rumoca_phase_parse::parse_to_recovered_ast(content, uri);
                    let formatted = errors
                        .iter()
                        .map(|error| rumoca_phase_parse::format_parse_error(error, uri, content))
                        .collect::<Vec<_>>()
                        .join("\n\n");
                    (
                        fallback,
                        Some(partial),
                        errors,
                        Some(formatted),
                        should_invalidate,
                    )
                }
            };
        self.documents.insert(
            uri.to_string(),
            Document {
                uri: uri.to_string(),
                content: content.to_string(),
                parsed,
                partial,
                parse_errors,
                parse_error: parse_error.clone(),
            },
        );
        // Invalidate cached state only when parse succeeded or no fallback parse exists.
        if invalidate_resolved {
            self.invalidate_resolved_state();
        } else {
            self.invalidate_strict_compile_state();
        }
        parse_error
    }

    /// Add a pre-parsed definition to the session.
    ///
    /// This is more efficient than `add_document` when you've already parsed
    /// the file (e.g., using parallel parsing).
    pub fn add_parsed(&mut self, uri: &str, parsed: ast::StoredDefinition) {
        self.detach_uri_from_source_sets(uri);
        self.documents.insert(
            uri.to_string(),
            Document {
                uri: uri.to_string(),
                content: String::new(), // Content not needed when pre-parsed
                parsed: Some(parsed),
                partial: None,
                parse_errors: Vec::new(),
                parse_error: None,
            },
        );
        // Invalidate cached state
        self.invalidate_resolved_state();
    }

    /// Add multiple pre-parsed definitions to the session.
    ///
    /// This is the most efficient way to load a large library like MSL.
    pub fn add_parsed_batch(&mut self, definitions: Vec<(String, ast::StoredDefinition)>) {
        for (uri, parsed) in definitions {
            self.detach_uri_from_source_sets(&uri);
            self.documents.insert(
                uri.clone(),
                Document {
                    uri,
                    content: String::new(),
                    parsed: Some(parsed),
                    partial: None,
                    parse_errors: Vec::new(),
                    parse_error: None,
                },
            );
        }
        self.invalidate_resolved_state();
    }

    /// Replace a parsed source-set (e.g., a library) in one operation.
    ///
    /// Existing parsed docs in this source-set are removed first. Documents with
    /// non-empty content (workspace/open docs) are preserved.
    /// Returns the number of parsed documents inserted.
    pub fn replace_parsed_source_set(
        &mut self,
        source_set_id: &str,
        definitions: Vec<(String, ast::StoredDefinition)>,
        exclude_uri: Option<&str>,
    ) -> usize {
        let mut desired_docs: IndexMap<String, ast::StoredDefinition> = IndexMap::new();
        for (uri, parsed) in definitions {
            if exclude_uri.is_some_and(|excluded| same_path(&uri, excluded)) {
                continue;
            }
            // Preserve workspace/open document contents when paths overlap.
            if self
                .documents
                .get(&uri)
                .is_some_and(|existing| !existing.content.is_empty())
            {
                continue;
            }

            desired_docs.insert(uri, parsed);
        }
        let inserted_uris: IndexSet<String> = desired_docs.keys().cloned().collect();

        if let Some(previous_uris) = self.source_sets.get(source_set_id)
            && previous_uris == &inserted_uris
        {
            let unchanged = desired_docs.iter().all(|(uri, parsed)| {
                self.documents
                    .get(uri)
                    .and_then(|doc| doc.parsed.as_ref())
                    .is_some_and(|existing| existing == parsed)
            });
            if unchanged {
                return inserted_uris.len();
            }
        }

        if let Some(previous) = self.source_sets.shift_remove(source_set_id) {
            let removable: Vec<String> = previous
                .iter()
                .filter(|uri| {
                    self.documents
                        .get(*uri)
                        .is_some_and(|doc| doc.content.is_empty())
                })
                .cloned()
                .collect();
            for uri in removable {
                self.documents.shift_remove(&uri);
            }
        }

        let mut inserted_count = 0usize;
        for (uri, parsed) in desired_docs {
            self.documents.insert(
                uri.clone(),
                Document {
                    uri,
                    content: String::new(),
                    parsed: Some(parsed),
                    partial: None,
                    parse_errors: Vec::new(),
                    parse_error: None,
                },
            );
            inserted_count += 1;
        }

        self.source_sets
            .insert(source_set_id.to_string(), inserted_uris);
        self.invalidate_resolved_state();
        inserted_count
    }

    /// Tolerantly index one library path into a parsed source-set.
    ///
    /// Parsing/index/cache failures are reported in `diagnostics` and do not
    /// panic or abort the session.
    pub fn index_library_tolerant(
        &mut self,
        source_set_id: &str,
        library_path: &Path,
        exclude_uri: Option<&str>,
    ) -> IndexingReport {
        let cache_dir = resolve_library_cache_dir();
        self.index_library_tolerant_with_cache_dir(
            source_set_id,
            library_path,
            exclude_uri,
            cache_dir.as_deref(),
        )
    }

    fn index_library_tolerant_with_cache_dir(
        &mut self,
        source_set_id: &str,
        library_path: &Path,
        exclude_uri: Option<&str>,
        cache_dir: Option<&Path>,
    ) -> IndexingReport {
        let library_path_str = library_path.display().to_string();
        let parsed = match parse_library_with_cache_in(library_path, cache_dir) {
            Ok(parsed) => parsed,
            Err(err) => {
                return IndexingReport {
                    source_set_id: source_set_id.to_string(),
                    library_path: library_path_str,
                    indexed_file_count: 0,
                    inserted_file_count: 0,
                    cache_status: None,
                    cache_key: None,
                    cache_file: None,
                    diagnostics: vec![format!(
                        "Failed to load library '{}': {}",
                        library_path.display(),
                        err
                    )],
                };
            }
        };

        let inserted_file_count =
            self.replace_parsed_source_set(source_set_id, parsed.documents, exclude_uri);
        IndexingReport {
            source_set_id: source_set_id.to_string(),
            library_path: library_path_str,
            indexed_file_count: parsed.file_count,
            inserted_file_count,
            cache_status: Some(parsed.cache_status),
            cache_key: Some(parsed.cache_key),
            cache_file: parsed.cache_file,
            diagnostics: Vec::new(),
        }
    }

    /// Remove all parsed documents previously loaded for a source-set id.
    pub fn remove_source_set(&mut self, source_set_id: &str) {
        if let Some(previous) = self.source_sets.shift_remove(source_set_id) {
            let removable: Vec<String> = previous
                .iter()
                .filter(|uri| {
                    self.documents
                        .get(*uri)
                        .is_some_and(|doc| doc.content.is_empty())
                })
                .cloned()
                .collect();
            for uri in removable {
                self.documents.shift_remove(&uri);
            }
            self.invalidate_resolved_state();
        }
    }

    /// Remove a document from the session.
    pub fn remove_document(&mut self, uri: &str) {
        self.documents.shift_remove(uri);
        self.detach_uri_from_source_sets(uri);
        self.invalidate_resolved_state();
    }

    /// Get a document by URI.
    pub fn get_document(&self, uri: &str) -> Option<&Document> {
        self.documents.get(uri)
    }

    /// Return structured parse diagnostics and a source map for one document.
    pub fn document_parse_diagnostics_with_source_map(
        &self,
        uri: &str,
    ) -> Option<(Vec<CommonDiagnostic>, SourceMap)> {
        let doc = self.documents.get(uri)?;
        let source_map = self.session_source_map();
        let diagnostics = document_parse_diagnostics(doc, &source_map);
        (!diagnostics.is_empty()).then_some((diagnostics, source_map))
    }

    /// Get all document URIs.
    pub fn document_uris(&self) -> Vec<&str> {
        self.documents.keys().map(|s| s.as_str()).collect()
    }

    /// Build the resolved tree from all documents.
    ///
    /// This performs Parse -> Resolve but NOT typecheck.
    /// Typechecking happens after instantiation so dimension expressions
    /// can be evaluated with full modifier context (MLS §10.1).
    pub(crate) fn build_resolved(&mut self) -> Result<()> {
        self.build_resolved_with_diagnostics()
            .map(|_| ())
            .map_err(|diags| diagnostics_to_anyhow(&diags))
    }

    /// Build the resolved tree, returning diagnostics on failure.
    fn build_resolved_with_diagnostics(
        &mut self,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        self.build_resolved_with_diagnostics_inner(ResolveBuildMode::Standard)
    }

    /// Build the resolved tree for strict target compilation.
    ///
    /// Unlike generic build flows, this ignores parse-error diagnostics from
    /// unrelated documents and resolves from available parsed ASTs. Unresolved
    /// name errors stay tolerant here so closure planning is not blocked by
    /// unrelated symbols.
    fn build_resolved_for_strict_compile_with_diagnostics(
        &mut self,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        self.build_resolved_with_diagnostics_inner(ResolveBuildMode::StrictCompileRecovery)
    }

    fn build_resolved_with_diagnostics_inner(
        &mut self,
        mode: ResolveBuildMode,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        let session_source_map = self.session_source_map();
        if mode.include_parse_error_diags() {
            let parse_error_diags =
                collect_parse_error_diagnostics(&self.documents, &session_source_map);
            if !parse_error_diags.is_empty() {
                return Err(diagnostics_from_vec(parse_error_diags));
            }
        }

        if mode == ResolveBuildMode::Standard
            && let Some(resolved) = self.resolved_cache.get(mode)
        {
            return Ok((resolved.clone(), CommonDiagnostics::new()));
        }

        let definitions: Vec<_> = self
            .documents
            .values()
            .filter_map(|doc| {
                let parsed = if mode == ResolveBuildMode::StrictCompileRecovery {
                    doc.parse_error
                        .as_ref()
                        .and(doc.partial.clone())
                        .or_else(|| doc.parsed.clone())
                } else {
                    doc.parsed.clone()
                }?;
                Some((doc.uri.clone(), parsed))
            })
            .collect();
        let multi_document_session = definitions.len() > 1;
        let merged = merge_stored_definitions(definitions).map_err(|e| {
            let mut diags = CommonDiagnostics::new();
            diags.emit(merge_error_to_common(&e, &session_source_map));
            diags
        })?;
        let mut tree = ast::ClassTree::from_parsed(merged);

        for doc in self.documents.values() {
            tree.source_map.add(&doc.uri, &doc.content);
        }

        let parsed = ast::ParsedTree::new(tree);
        let unresolved_are_errors =
            mode.unresolved_refs_are_errors_in_single_document() && !multi_document_session;
        let resolve_options = ResolveOptions {
            unresolved_component_refs_are_errors: unresolved_are_errors,
            unresolved_function_calls_are_errors: unresolved_are_errors,
        };
        let (resolved, diagnostics) = if mode == ResolveBuildMode::StrictCompileRecovery {
            resolve_with_options_collect(parsed, resolve_options)
        } else {
            let resolved = resolve_with_options(parsed, resolve_options)?;
            (resolved, CommonDiagnostics::new())
        };

        self.model_names = collect_model_names(&resolved.0.definitions);
        let resolved = Arc::new(resolved);
        self.resolved_cache.set(mode, resolved.clone());

        Ok((resolved, diagnostics))
    }

    /// Get the resolved tree, returning an error if resolution hasn't been performed.
    fn ensure_resolved(&self) -> Result<&Arc<ast::ResolvedTree>> {
        self.resolved_cache
            .get(ResolveBuildMode::Standard)
            .ok_or_else(|| {
                anyhow::anyhow!("Session has no resolved tree — call build_resolved() first")
            })
    }

    /// Get all model names in the session.
    pub fn model_names(&mut self) -> Result<&[String]> {
        self.build_resolved()?;
        Ok(&self.model_names)
    }

    /// Count all class types in the session (model, connector, function, etc.).
    pub fn class_type_counts(&mut self) -> Result<std::collections::HashMap<String, usize>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?;
        Ok(collect_class_type_counts(&resolved.0.definitions))
    }

    /// Get all qualified class names from the resolved name_map.
    ///
    /// This includes all classes (packages, models, connectors, etc.) from all
    /// documents in the session. Useful for code completion of library types.
    /// Triggers resolution if not already done.
    pub fn all_class_names(&mut self) -> Result<Vec<String>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?;
        Ok(resolved.0.name_map.keys().cloned().collect())
    }

    /// Get cached class names without triggering resolution.
    ///
    /// Returns the class names from the already-resolved name_map, or an empty
    /// vector if resolution hasn't been performed yet. This is safe to call
    /// from read-only contexts (e.g., LSP completion with `&self`).
    pub fn all_class_names_cached(&self) -> Vec<String> {
        self.resolved_cache
            .any()
            .map(|r| r.0.name_map.keys().cloned().collect())
            .unwrap_or_default()
    }

    /// Returns true when a resolved tree is already cached in the session.
    ///
    /// This is useful for latency-sensitive paths (like editor completion)
    /// to avoid rebuilding resolution unless it is actually needed.
    pub fn has_resolved_cached(&self) -> bool {
        self.resolved_cache.any().is_some()
    }

    /// Get component members for a class name from cached resolved state.
    ///
    /// Returns `(member_name, member_type_name)` pairs, including inherited members
    /// after applying extends `break` exclusions. If resolution is not available
    /// or the class name is ambiguous, returns an empty vector.
    ///
    /// This does not trigger resolution and is safe for read-only contexts.
    pub fn class_component_members_cached(&self, class_name: &str) -> Vec<(String, String)> {
        let Some(resolved) = self.resolved_cache.any() else {
            return Vec::new();
        };
        let tree = &resolved.0;
        let Some(class) = resolve_class_for_completion(tree, class_name) else {
            return Vec::new();
        };

        let mut members = IndexMap::<String, String>::new();
        let mut visiting = std::collections::HashSet::<DefId>::new();
        collect_class_component_members(tree, class, &mut members, &mut visiting);
        members.into_iter().collect()
    }

    /// Get the class tree.
    pub fn tree(&mut self) -> Result<&ast::ClassTree> {
        self.build_resolved()?;
        Ok(&self.ensure_resolved()?.0)
    }

    /// Get the resolved tree.
    pub fn resolved(&mut self) -> Result<ast::ResolvedTree> {
        self.build_resolved()?;
        Ok(ast::ResolvedTree(self.ensure_resolved()?.0.clone()))
    }

    /// Get any cached resolved tree without triggering a full rebuild.
    pub fn resolved_cached(&self) -> Option<ast::ResolvedTree> {
        self.resolved_cache
            .any()
            .map(|resolved| ast::ResolvedTree(resolved.0.clone()))
    }

    /// Compile a specific model.
    ///
    /// Uses the phase order: Resolve -> Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model(&mut self, model_name: &str) -> Result<CompilationResult> {
        match self.compile_model_phases(model_name)? {
            PhaseResult::Success(result) => Ok(*result),
            PhaseResult::NeedsInner { missing_inners } => Err(anyhow::anyhow!(
                "Instantiate error: missing inner declarations: {}",
                missing_inners.join(", ")
            )),
            PhaseResult::Failed {
                phase,
                error,
                error_code,
            } => {
                if let Some(code) = error_code {
                    Err(anyhow::anyhow!("{} error [{}]: {}", phase, code, error))
                } else {
                    Err(anyhow::anyhow!("{} error: {}", phase, error))
                }
            }
        }
    }

    /// Compile a model with phase-level tracking.
    ///
    /// Uses the new phase order: Resolve -> Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model_phases(&mut self, model_name: &str) -> Result<PhaseResult> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?.clone();
        let fingerprint = self.model_dependency_fingerprint(&resolved.0, model_name);
        if let Some(cached) = self.compile_cache.get(model_name)
            && cached.fingerprint == fingerprint
        {
            return Ok(cached.result.clone());
        }

        let result = compile_model_internal(&resolved.0, model_name);
        self.compile_cache.insert(
            model_name.to_string(),
            CompileCacheEntry {
                fingerprint,
                result: result.clone(),
            },
        );
        Ok(result)
    }

    /// Compile multiple models in parallel.
    pub fn compile_models_parallel(
        &mut self,
        model_names: &[&str],
    ) -> Result<Vec<(String, PhaseResult)>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?.clone();
        let names: Vec<String> = model_names.iter().map(|name| (*name).to_string()).collect();
        Ok(self.compile_models_with_cache(&resolved.0, &names))
    }

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&mut self) -> Result<Vec<(String, PhaseResult)>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?.clone();
        let names = self.model_names.clone();
        Ok(self.compile_models_with_cache(&resolved.0, &names))
    }

    /// Compile all models and return summary.
    pub fn compile_all_with_summary(
        &mut self,
    ) -> Result<(Vec<(String, PhaseResult)>, CompilationSummary)> {
        let results = self.compile_all_parallel()?;
        let summary = CompilationSummary::from_results(&results);
        Ok((results, summary))
    }

    /// Compile the requested model using strict-reachable semantics with
    /// internal recovery to surface additional diagnostics.
    ///
    /// This compiles the requested model strictly and collects diagnostics from
    /// the target's reachable transitive dependency closure.
    pub fn compile_model_strict_reachable_with_recovery(
        &mut self,
        model_name: &str,
    ) -> StrictCompileReport {
        self.compile_model_strict_reachable_report(model_name, true)
    }

    /// Compile the requested model using strict-reachable semantics with
    /// internal recovery while bypassing the session compile cache.
    ///
    /// This is intended for focused editor-style compiles where library AST and
    /// resolved state should be reused, but per-model phase results should not
    /// accumulate across unrelated requests.
    pub fn compile_model_strict_reachable_uncached_with_recovery(
        &mut self,
        model_name: &str,
    ) -> StrictCompileReport {
        self.compile_model_strict_reachable_report(model_name, false)
    }

    fn compile_model_strict_reachable_report(
        &mut self,
        model_name: &str,
        use_compile_cache: bool,
    ) -> StrictCompileReport {
        let (resolved, resolve_diags) =
            match self.build_resolved_for_strict_compile_with_diagnostics() {
                Ok(build) => build,
                Err(diags) => {
                    let mut failures = Vec::new();
                    failures.extend(diags.iter().map(|diag| ModelFailureDiagnostic {
                        model_name: "<resolve>".to_string(),
                        phase: None,
                        error_code: diag.code.clone(),
                        error: diag.message.clone(),
                        primary_label: diag.labels.iter().find(|label| label.primary).cloned(),
                    }));
                    return StrictCompileReport {
                        requested_model: model_name.to_string(),
                        requested_result: None,
                        summary: CompilationSummary::default(),
                        failures,
                        source_map: Some(self.session_source_map()),
                    };
                }
            };

        let tree = &resolved.0;
        let reachable_classes = self.strict_reachable_classes(tree, model_name);
        let targets = self.strict_compile_targets(tree, model_name);
        let target_source_files = collect_target_source_files(tree, &reachable_classes);
        let mut failures = collect_parse_failures_for_files(
            &self.documents,
            &tree.source_map,
            &target_source_files,
        );
        let resolve_failures = collect_resolve_failures_for_files(
            &resolve_diags,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !resolve_failures.is_empty();
        failures.extend(resolve_failures);
        let results = if use_compile_cache {
            self.compile_models_with_cache(tree, &targets)
        } else {
            self.compile_models_without_cache(tree, &targets)
        };
        finalize_strict_compile_report(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            results,
        )
    }

    fn strict_compile_targets(&mut self, tree: &ast::ClassTree, model_name: &str) -> Vec<String> {
        let dep_cache = self
            .dependency_fingerprints
            .get_or_insert_with(|| DependencyFingerprintCache::from_tree(tree));
        let planner = ReachabilityPlanner::new(dep_cache.class_dependencies(), &self.model_names);
        planner.compile_targets(model_name)
    }

    fn strict_reachable_classes(&mut self, tree: &ast::ClassTree, model_name: &str) -> Vec<String> {
        let dep_cache = self
            .dependency_fingerprints
            .get_or_insert_with(|| DependencyFingerprintCache::from_tree(tree));
        let planner = ReachabilityPlanner::new(dep_cache.class_dependencies(), &self.model_names);
        planner.reachable_classes(model_name)
    }

    /// Compile a model with an explicit compilation mode.
    pub fn compile_model_with_mode(
        &mut self,
        model_name: &str,
        mode: CompilationMode,
    ) -> StrictCompileReport {
        match mode {
            CompilationMode::StrictReachable | CompilationMode::StrictReachableWithRecovery => {
                self.compile_model_strict_reachable_with_recovery(model_name)
            }
            CompilationMode::StrictReachableUncachedWithRecovery => {
                self.compile_model_strict_reachable_uncached_with_recovery(model_name)
            }
        }
    }

    /// Compile a model and collect all diagnostics from the first failing phase.
    pub fn compile_model_diagnostics(&mut self, model_name: &str) -> ModelDiagnostics {
        let mut collected = Vec::new();

        let resolved = match self.build_resolved_with_diagnostics() {
            Ok((tree, _)) => tree,
            Err(diags) => {
                collected.extend(diags.iter().cloned());
                return global_resolution_failure_diagnostics(self.session_source_map(), collected);
            }
        };

        let tree = &resolved.0;
        let model_span = class_primary_span(tree, model_name)
            .unwrap_or_else(|| default_tree_span(&tree.source_map));
        let class_type = tree
            .get_class_by_qualified_name(model_name)
            .map(|class| class.class_type.clone());

        let mut overlay = match instantiate_model_with_outcome(tree, model_name) {
            InstantiationOutcome::Success(o) => o,
            InstantiationOutcome::NeedsInner {
                missing_inners,
                missing_spans,
                ..
            } => {
                let primary_span = missing_spans.first().copied().unwrap_or(model_span);
                let mut diag = CommonDiagnostic::error(
                    "EI008",
                    format!(
                        "model needs inner declarations: {}",
                        missing_inners.join(", ")
                    ),
                    PrimaryLabel::new(primary_span).with_message("missing matching `inner`"),
                );
                for (idx, span) in missing_spans.iter().enumerate().skip(1) {
                    diag = diag.with_label(missing_inner_label(idx, *span));
                }
                collected.push(diag);
                return model_diagnostics_for_tree(tree, collected);
            }
            InstantiationOutcome::Error(e) => {
                collected.push(miette_error_to_common(&*e, model_span, &tree.source_map));
                return model_diagnostics_for_tree(tree, collected);
            }
        };

        if let Some(diag) = synthesized_inner_warning(
            &overlay.synthesized_inners,
            PrimaryLabel::new(model_span).with_message("synthesized inner declaration"),
        ) {
            collected.push(diag);
        }

        if let Err(diags) = typecheck_instanced(tree, &mut overlay, model_name) {
            collected.extend(diags.iter().cloned());
            return model_diagnostics_for_tree(tree, collected);
        }

        // LSP diagnostics should validate all class kinds (including functions),
        // but only model/block/class classes should go through flatten+ToDae.
        if class_type
            .as_ref()
            .is_some_and(|ct| !is_simulatable_class_type(ct))
        {
            return model_diagnostics_for_tree(tree, collected);
        }

        let flat = match flatten_ref_with_options(
            tree,
            &overlay,
            model_name,
            flatten_options_for_tree(),
        ) {
            Ok(f) => f,
            Err(e) => {
                collected.push(miette_error_to_common(&e, model_span, &tree.source_map));
                return model_diagnostics_for_tree(tree, collected);
            }
        };

        if let Err(e) = to_dae_with_options(&flat, todae_options_for_tree(tree)) {
            collected.push(miette_error_to_common(&e, model_span, &tree.source_map));
        }

        model_diagnostics_for_tree(tree, collected)
    }

    fn session_source_map(&self) -> SourceMap {
        let mut source_map = SourceMap::new();
        for doc in self.documents.values() {
            source_map.add(&doc.uri, &doc.content);
        }
        source_map
    }

    fn model_dependency_fingerprint(
        &mut self,
        tree: &ast::ClassTree,
        model_name: &str,
    ) -> Fingerprint {
        self.dependency_fingerprints
            .get_or_insert_with(|| DependencyFingerprintCache::from_tree(tree))
            .model_fingerprint(model_name)
    }

    fn compile_models_with_cache(
        &mut self,
        tree: &ast::ClassTree,
        model_names: &[String],
    ) -> Vec<(String, PhaseResult)> {
        let dep_cache = self
            .dependency_fingerprints
            .get_or_insert_with(|| DependencyFingerprintCache::from_tree(tree));

        let models_with_fingerprints: Vec<(String, Fingerprint)> = model_names
            .iter()
            .map(|name| (name.clone(), dep_cache.model_fingerprint(name)))
            .collect();

        let misses: Vec<(String, Fingerprint)> = models_with_fingerprints
            .iter()
            .filter_map(|(name, fingerprint)| {
                let hit = self
                    .compile_cache
                    .get(name)
                    .is_some_and(|entry| entry.fingerprint == *fingerprint);
                if hit {
                    None
                } else {
                    Some((name.clone(), *fingerprint))
                }
            })
            .collect();

        let compiled_misses: Vec<(String, Fingerprint, PhaseResult)> = misses
            .par_iter()
            .map(|(name, fingerprint)| {
                (
                    name.clone(),
                    *fingerprint,
                    compile_model_internal(tree, name),
                )
            })
            .collect();

        for (name, fingerprint, result) in compiled_misses {
            self.compile_cache.insert(
                name,
                CompileCacheEntry {
                    fingerprint,
                    result,
                },
            );
        }

        let mut results = Vec::with_capacity(models_with_fingerprints.len());
        for (name, fingerprint) in models_with_fingerprints {
            if let Some(entry) = self.compile_cache.get(&name)
                && entry.fingerprint == fingerprint
            {
                results.push((name, entry.result.clone()));
                continue;
            }

            // Defensive fallback: compile directly if cache entry is absent.
            let result = compile_model_internal(tree, &name);
            self.compile_cache.insert(
                name.clone(),
                CompileCacheEntry {
                    fingerprint,
                    result: result.clone(),
                },
            );
            results.push((name, result));
        }
        results
    }

    fn compile_models_without_cache(
        &self,
        tree: &ast::ClassTree,
        model_names: &[String],
    ) -> Vec<(String, PhaseResult)> {
        model_names
            .par_iter()
            .map(|name| (name.clone(), compile_model_internal(tree, name)))
            .collect()
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new(SessionConfig::default())
    }
}

/// Pre-compiled library for efficient multi-model compilation.
///
/// This is a convenience wrapper around [`Session`] that initializes from
/// a [`ast::StoredDefinition`]. Use this when you've already parsed your library
/// (e.g., from parallel file parsing) and want to compile multiple models.
///
/// For new code, consider using [`Session`] directly with [`Session::add_parsed_batch`].
pub struct CompiledLibrary {
    resolved: Arc<ast::ResolvedTree>,
    model_names: Vec<String>,
    class_type_counts: std::collections::HashMap<String, usize>,
    class_dependencies: IndexMap<String, IndexSet<String>>,
    resolve_diagnostics: CommonDiagnostics,
    compile_cache: Mutex<IndexMap<String, PhaseResult>>,
}

impl CompiledLibrary {
    fn from_indexed_state(
        resolved: Arc<ast::ResolvedTree>,
        model_names: Vec<String>,
        class_type_counts: std::collections::HashMap<String, usize>,
        resolve_diagnostics: CommonDiagnostics,
    ) -> Self {
        let dependency_fingerprints = DependencyFingerprintCache::from_tree(&resolved.0);
        Self {
            resolved,
            model_names,
            class_type_counts,
            class_dependencies: dependency_fingerprints.class_dependencies().clone(),
            resolve_diagnostics,
            compile_cache: Mutex::new(IndexMap::new()),
        }
    }

    /// Create a compiled library from a ast::StoredDefinition.
    ///
    /// This resolves the AST once. Type checking happens after instantiation.
    pub fn from_stored_definition(def: ast::StoredDefinition) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        session.add_parsed("library", def);
        // Build resolved tree to catch resolution errors early
        session.build_resolved()?;
        let resolved = session.ensure_resolved()?.clone();
        Ok(Self::from_indexed_state(
            resolved.clone(),
            session.model_names.clone(),
            collect_class_type_counts(&resolved.0.definitions),
            CommonDiagnostics::new(),
        ))
    }

    /// Create a compiled library from a parsed batch, indexing it tolerantly.
    ///
    /// This preserves whole-library resolve diagnostics for later strict
    /// target-closure compilation without requiring the entire library to
    /// resolve cleanly up front.
    pub fn from_parsed_batch_tolerant(
        documents: Vec<(String, ast::StoredDefinition)>,
    ) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        session.add_parsed_batch(documents);
        let (resolved, resolve_diagnostics) = session
            .build_resolved_for_strict_compile_with_diagnostics()
            .map_err(|diags| diagnostics_to_anyhow(&diags))?;
        Ok(Self::from_indexed_state(
            resolved.clone(),
            session.model_names.clone(),
            collect_class_type_counts(&resolved.0.definitions),
            resolve_diagnostics,
        ))
    }

    /// Create a compiled library from an already-resolved tree.
    ///
    /// This avoids re-running resolve and is intended for callers that already
    /// hold a validated resolved tree (e.g., MSL regression harness).
    pub fn from_resolved_tree(resolved: ast::ResolvedTree, model_names: Vec<String>) -> Self {
        let resolved = Arc::new(resolved);
        Self::from_indexed_state(
            resolved.clone(),
            model_names,
            collect_class_type_counts(&resolved.0.definitions),
            CommonDiagnostics::new(),
        )
    }

    /// Get all model names in the library.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn model_names(&self) -> &[String] {
        &self.model_names
    }

    /// Count all class types in the library.
    pub fn class_type_counts(&self) -> &std::collections::HashMap<String, usize> {
        &self.class_type_counts
    }

    /// Get the class tree.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn tree(&self) -> &ast::ClassTree {
        // SAFETY: build_resolved() succeeded during construction, so resolved is always Some
        &self.resolved_tree().0
    }

    /// Get the resolved tree reference (guaranteed present after construction).
    fn resolved_tree(&self) -> &Arc<ast::ResolvedTree> {
        &self.resolved
    }

    fn cached_phase_result(&self, model_name: &str) -> PhaseResult {
        if let Some(result) = self
            .compile_cache
            .lock()
            .expect("compiled library cache poisoned")
            .get(model_name)
            .cloned()
        {
            return result;
        }

        let result = compile_model_internal(&self.resolved_tree().0, model_name);
        self.compile_cache
            .lock()
            .expect("compiled library cache poisoned")
            .entry(model_name.to_string())
            .or_insert_with(|| result.clone());
        result
    }

    fn strict_reachable_classes(&self, model_name: &str) -> Vec<String> {
        ReachabilityPlanner::new(&self.class_dependencies, &self.model_names)
            .reachable_classes(model_name)
    }

    fn strict_compile_targets(&self, model_name: &str) -> Vec<String> {
        ReachabilityPlanner::new(&self.class_dependencies, &self.model_names)
            .compile_targets(model_name)
    }

    fn compile_targets_without_cache(&self, targets: &[String]) -> Vec<(String, PhaseResult)> {
        let tree = &self.resolved_tree().0;
        targets
            .par_iter()
            .map(|name| (name.clone(), compile_model_internal(tree, name)))
            .collect()
    }

    fn compile_targets_with_cache(&self, targets: &[String]) -> Vec<(String, PhaseResult)> {
        let (mut results, missing) = {
            let cache = self
                .compile_cache
                .lock()
                .expect("compiled library cache poisoned");
            split_cached_target_results(&cache, targets)
        };

        if !missing.is_empty() {
            let tree = &self.resolved_tree().0;
            let compiled_misses: Vec<_> = missing
                .par_iter()
                .map(|name| (name.clone(), compile_model_internal(tree, name)))
                .collect();

            let mut cache = self
                .compile_cache
                .lock()
                .expect("compiled library cache poisoned");
            for (name, result) in compiled_misses {
                cache.entry(name.clone()).or_insert_with(|| result.clone());
                results.insert(name, result);
            }
        }

        targets
            .iter()
            .filter_map(|target| {
                results
                    .shift_remove(target)
                    .map(|result| (target.clone(), result))
            })
            .collect()
    }

    /// Compile the requested model strictly against its reachable closure while
    /// preserving unrelated library diagnostics outside that closure.
    pub fn compile_model_strict_reachable_with_recovery(
        &self,
        model_name: &str,
    ) -> StrictCompileReport {
        let tree = &self.resolved_tree().0;
        let reachable_classes = self.strict_reachable_classes(model_name);
        let targets = self.strict_compile_targets(model_name);
        let target_source_files = collect_target_source_files(tree, &reachable_classes);
        let failures = collect_resolve_failures_for_files(
            &self.resolve_diagnostics,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !failures.is_empty();
        let results = self.compile_targets_with_cache(&targets);
        finalize_strict_compile_report(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            results,
        )
    }

    /// Compile the requested model strictly against its reachable closure
    /// without retaining phase results from prior focused compiles.
    pub fn compile_model_strict_reachable_uncached_with_recovery(
        &self,
        model_name: &str,
    ) -> StrictCompileReport {
        let tree = &self.resolved_tree().0;
        let reachable_classes = self.strict_reachable_classes(model_name);
        let targets = self.strict_compile_targets(model_name);
        let target_source_files = collect_target_source_files(tree, &reachable_classes);
        let failures = collect_resolve_failures_for_files(
            &self.resolve_diagnostics,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !failures.is_empty();
        let results = self.compile_targets_without_cache(&targets);
        finalize_strict_compile_report(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            results,
        )
    }

    /// Compile a specific model.
    ///
    /// Uses the new phase order: Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model(&self, model_name: &str) -> Result<CompilationResult> {
        match self.cached_phase_result(model_name) {
            PhaseResult::Success(result) => Ok(*result),
            PhaseResult::NeedsInner { missing_inners } => Err(anyhow::anyhow!(
                "Missing inner declarations: {:?}",
                missing_inners
            )),
            PhaseResult::Failed { phase, error, .. } => {
                Err(anyhow::anyhow!("{} error: {}", phase, error))
            }
        }
    }

    /// Compile a model with phase-level tracking.
    ///
    /// Returns PhaseResult directly (infallible for pre-built libraries).
    pub fn compile_model_phases(&self, model_name: &str) -> PhaseResult {
        self.cached_phase_result(model_name)
    }

    /// Compile multiple models in parallel.
    pub fn compile_models_parallel(&self, model_names: &[&str]) -> Vec<(String, PhaseResult)> {
        let names = model_names
            .iter()
            .map(|name| (*name).to_string())
            .collect::<Vec<_>>();
        self.compile_targets_with_cache(&names)
    }

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&self) -> Vec<(String, PhaseResult)> {
        self.compile_targets_with_cache(&self.model_names)
    }

    /// Compile all models and return summary.
    pub fn compile_all_parallel_with_summary(
        &self,
    ) -> (Vec<(String, PhaseResult)>, CompilationSummary) {
        let results = self.compile_all_parallel();
        let summary = CompilationSummary::from_results(&results);
        (results, summary)
    }
}

#[cfg(test)]
mod tests;
