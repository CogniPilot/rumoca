//! Unified compilation session management.
//!
//! This module provides the Session type that manages compilation state
//! across different frontends (CLI, LSP, WASM).

use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use rayon::prelude::*;
use rumoca_core::{
    DefId, Diagnostic as CommonDiagnostic, Diagnostics as CommonDiagnostics, Label, SourceId,
    SourceMap, Span,
};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_phase_dae::{ToDaeOptions, to_dae_with_options};
use rumoca_phase_flatten::{FlattenOptions, flatten_ref_with_options};
use rumoca_phase_instantiate::{InstantiationOutcome, instantiate_model_with_outcome};
use rumoca_phase_resolve::{ResolveOptions, resolve_with_options};
use rumoca_phase_typecheck::typecheck_instanced;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Once};
use std::time::{Duration, Instant};

use crate::experiment::experiment_settings_for_model;
use crate::merge::{
    MergeSemanticError, collect_class_type_counts, collect_model_names, merge_stored_definitions,
};

mod dependency_fingerprint;
use dependency_fingerprint::{CompileCacheEntry, DependencyFingerprintCache, Fingerprint};

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

/// A document in the session.
#[derive(Debug, Clone)]
pub struct Document {
    /// Document URI or file path.
    pub uri: String,
    /// Source content.
    pub content: String,
    /// Parsed definition (cached, None if parsing failed).
    pub parsed: Option<ast::StoredDefinition>,
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
}

/// Failure diagnostic for a single model in a best-effort compilation pass.
#[derive(Debug, Clone)]
pub struct ModelFailureDiagnostic {
    pub model_name: String,
    pub phase: Option<FailedPhase>,
    pub error_code: Option<String>,
    pub error: String,
}

/// Report from compiling a requested model in best-effort mode.
///
/// The requested model remains strict: it must compile successfully for callers
/// to treat the compile as successful. Other related models are still compiled
/// so additional diagnostics can be surfaced to the user.
#[derive(Debug)]
pub struct BestEffortCompilationReport {
    pub requested_model: String,
    pub requested_result: Option<PhaseResult>,
    pub summary: CompilationSummary,
    pub failures: Vec<ModelFailureDiagnostic>,
    pub source_map: Option<SourceMap>,
}

impl BestEffortCompilationReport {
    /// Returns true when the requested model compiled successfully.
    pub fn requested_succeeded(&self) -> bool {
        matches!(self.requested_result, Some(PhaseResult::Success(_)))
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
            None => format!(
                "{} could not be compiled because resolve/parse failed",
                self.requested_model
            ),
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
        self.dae.balance() == 0
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
    /// Cached resolved tree (invalidated on document changes).
    pub(crate) resolved: Option<Arc<ast::ResolvedTree>>,
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
            resolved: None,
            model_names: Vec::new(),
            dependency_fingerprints: None,
            compile_cache: IndexMap::new(),
        }
    }

    fn invalidate_resolved_state(&mut self) {
        self.resolved = None;
        self.model_names.clear();
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
        let (parsed, parse_error, invalidate_resolved) =
            match rumoca_phase_parse::parse_to_ast(content, uri) {
                Ok(def) => (Some(def), None, true),
                Err(e) => {
                    // Keep last successful parse for semantic features (completion/hover) while
                    // the user is typing an incomplete statement.
                    let fallback = previous_parsed;
                    let should_invalidate = fallback.is_none();
                    (fallback, Some(e.to_string()), should_invalidate)
                }
            };
        self.documents.insert(
            uri.to_string(),
            Document {
                uri: uri.to_string(),
                content: content.to_string(),
                parsed,
                parse_error: parse_error.clone(),
            },
        );
        // Invalidate cached state only when parse succeeded or no fallback parse exists.
        if invalidate_resolved {
            self.invalidate_resolved_state();
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
    ) -> Result<Arc<ast::ResolvedTree>, CommonDiagnostics> {
        let parse_error_diags: Vec<_> = self
            .documents
            .values()
            .filter_map(|doc| {
                doc.parse_error.as_ref().map(|err| {
                    CommonDiagnostic::error(err.clone())
                        .with_code("syntax-error")
                        .with_note(format!("document: {}", doc.uri))
                })
            })
            .collect();
        if !parse_error_diags.is_empty() {
            let mut diags = CommonDiagnostics::new();
            for diag in parse_error_diags {
                diags.emit(diag);
            }
            return Err(diags);
        }

        if let Some(resolved) = &self.resolved {
            return Ok(resolved.clone());
        }

        let definitions: Vec<_> = self
            .documents
            .values()
            .filter_map(|doc| doc.parsed.clone().map(|p| (doc.uri.clone(), p)))
            .collect();
        let multi_document_session = definitions.len() > 1;
        let merged = merge_stored_definitions(definitions).map_err(|e| {
            let mut diags = CommonDiagnostics::new();
            diags.emit(merge_error_to_common(&e));
            diags
        })?;
        let mut tree = ast::ClassTree::from_parsed(merged);

        for doc in self.documents.values() {
            tree.source_map.add(&doc.uri, &doc.content);
        }

        let parsed = ast::ParsedTree::new(tree);
        let resolve_options = ResolveOptions {
            // Single-document diagnostics should be strict so unresolved names
            // surface at the actual source site. Multi-document/library sessions
            // remain best-effort to avoid aborting on unrelated library symbols.
            unresolved_component_refs_are_errors: !multi_document_session,
            unresolved_function_calls_are_errors: !multi_document_session,
        };
        let resolved = resolve_with_options(parsed, resolve_options)?;

        self.model_names = collect_model_names(&resolved.0.definitions);
        let resolved = Arc::new(resolved);
        self.resolved = Some(resolved.clone());

        Ok(resolved)
    }

    /// Get the resolved tree, returning an error if resolution hasn't been performed.
    fn ensure_resolved(&self) -> Result<&Arc<ast::ResolvedTree>> {
        self.resolved.as_ref().ok_or_else(|| {
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
        self.resolved
            .as_ref()
            .map(|r| r.0.name_map.keys().cloned().collect())
            .unwrap_or_default()
    }

    /// Returns true when a resolved tree is already cached in the session.
    ///
    /// This is useful for latency-sensitive paths (like editor completion)
    /// to avoid rebuilding resolution unless it is actually needed.
    pub fn has_resolved_cached(&self) -> bool {
        self.resolved.is_some()
    }

    /// Get component members for a class name from cached resolved state.
    ///
    /// Returns `(member_name, member_type_name)` pairs, including inherited members
    /// after applying extends `break` exclusions. If resolution is not available
    /// or the class name is ambiguous, returns an empty vector.
    ///
    /// This does not trigger resolution and is safe for read-only contexts.
    pub fn class_component_members_cached(&self, class_name: &str) -> Vec<(String, String)> {
        let Some(resolved) = &self.resolved else {
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

    /// Compile the requested model and related models in best-effort mode.
    ///
    /// This compiles the requested model strictly, but also compiles models in
    /// the same package scope so users get a broader, actionable diagnostic set.
    pub fn compile_model_best_effort(&mut self, model_name: &str) -> BestEffortCompilationReport {
        let mut failures = collect_parse_failures(&self.documents);

        let resolved = match self.build_resolved_with_diagnostics() {
            Ok(tree) => tree,
            Err(diags) => {
                failures.extend(diags.iter().map(|diag| ModelFailureDiagnostic {
                    model_name: "<resolve>".to_string(),
                    phase: None,
                    error_code: diag.code.clone(),
                    error: diag.message.clone(),
                }));
                return BestEffortCompilationReport {
                    requested_model: model_name.to_string(),
                    requested_result: None,
                    summary: CompilationSummary::default(),
                    failures,
                    source_map: None,
                };
            }
        };

        let tree = &resolved.0;
        let targets = collect_related_model_names(&self.model_names, model_name);
        let results = self.compile_models_with_cache(tree, &targets);
        let summary = CompilationSummary::from_results(&results);

        let mut requested_result = None;
        for (name, result) in results {
            if let Some(failure) = phase_result_to_failure(&name, &result) {
                failures.push(failure);
            }
            if name == model_name {
                requested_result = Some(result);
            }
        }

        BestEffortCompilationReport {
            requested_model: model_name.to_string(),
            requested_result,
            summary,
            failures,
            source_map: Some(tree.source_map.clone()),
        }
    }

    /// Compile a model and collect all diagnostics from the first failing phase.
    pub fn compile_model_diagnostics(&mut self, model_name: &str) -> ModelDiagnostics {
        let mut collected = Vec::new();

        let resolved = match self.build_resolved_with_diagnostics() {
            Ok(tree) => tree,
            Err(diags) => {
                collected.extend(diags.iter().cloned());
                return ModelDiagnostics {
                    diagnostics: collected,
                    source_map: None,
                };
            }
        };
        let tree = &resolved.0;
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
                let mut diag = CommonDiagnostic::error(format!(
                    "model needs inner declarations: {}",
                    missing_inners.join(", ")
                ))
                .with_code("EI008");
                for (idx, span) in missing_spans.iter().enumerate() {
                    diag = diag.with_label(missing_inner_label(idx, *span));
                }
                collected.push(diag);
                return ModelDiagnostics {
                    diagnostics: collected,
                    source_map: Some(tree.source_map.clone()),
                };
            }
            InstantiationOutcome::Error(e) => {
                collected.push(miette_error_to_common(&*e));
                return ModelDiagnostics {
                    diagnostics: collected,
                    source_map: Some(tree.source_map.clone()),
                };
            }
        };

        if overlay.used_synthesized_inners() {
            collected.push(
                CommonDiagnostic::warning(format!(
                    "outer without matching inner detected ({}); synthesizing root-level inner declaration(s)",
                    overlay.synthesized_inners.join(", ")
                ))
                .with_code("EI013")
                .with_note(
                    "MLS §5.4 permits default inner synthesis when no matching inner is present.",
                ),
            );
        }

        if let Err(diags) = typecheck_instanced(tree, &mut overlay, model_name) {
            collected.extend(diags.iter().cloned());
            return ModelDiagnostics {
                diagnostics: collected,
                source_map: Some(tree.source_map.clone()),
            };
        }

        // LSP diagnostics should validate all class kinds (including functions),
        // but only model/block/class classes should go through flatten+ToDae.
        if class_type
            .as_ref()
            .is_some_and(|ct| !is_simulatable_class_type(ct))
        {
            return ModelDiagnostics {
                diagnostics: collected,
                source_map: Some(tree.source_map.clone()),
            };
        }

        let flat = match flatten_ref_with_options(
            tree,
            &overlay,
            model_name,
            flatten_options_for_tree(),
        ) {
            Ok(f) => f,
            Err(e) => {
                collected.push(miette_error_to_common(&e));
                return ModelDiagnostics {
                    diagnostics: collected,
                    source_map: Some(tree.source_map.clone()),
                };
            }
        };

        if let Err(e) = to_dae_with_options(&flat, todae_options_for_tree(tree)) {
            collected.push(miette_error_to_common(&e));
        }

        ModelDiagnostics {
            diagnostics: collected,
            source_map: Some(tree.source_map.clone()),
        }
    }

    fn model_dependency_fingerprint(
        &mut self,
        tree: &ast::ClassTree,
        model_name: &str,
    ) -> Fingerprint {
        if self.dependency_fingerprints.is_none() {
            self.dependency_fingerprints = Some(DependencyFingerprintCache::from_tree(tree));
        }
        self.dependency_fingerprints
            .as_mut()
            .expect("dependency cache initialized above")
            .model_fingerprint(model_name)
    }

    fn compile_models_with_cache(
        &mut self,
        tree: &ast::ClassTree,
        model_names: &[String],
    ) -> Vec<(String, PhaseResult)> {
        if self.dependency_fingerprints.is_none() {
            self.dependency_fingerprints = Some(DependencyFingerprintCache::from_tree(tree));
        }
        let dep_cache = self
            .dependency_fingerprints
            .as_mut()
            .expect("dependency cache initialized above");

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
}

fn miette_error_to_common(err: &dyn miette::Diagnostic) -> CommonDiagnostic {
    let mut diag = CommonDiagnostic::error(err.to_string());
    if let Some(code) = err.code() {
        diag = diag.with_code(code.to_string());
    }
    if let Some(labels) = err.labels() {
        for (idx, labeled) in labels.enumerate() {
            let start = labeled.offset();
            let end = start.saturating_add(labeled.len());
            let span = Span::from_offsets(SourceId(0), start, end);
            let mut label = if idx == 0 {
                Label::primary(span)
            } else {
                Label::secondary(span)
            };
            if let Some(message) = labeled.label() {
                label = label.with_message(message.to_string());
            }
            diag = diag.with_label(label);
        }
    }
    diag
}

fn merge_error_to_common(err: &anyhow::Error) -> CommonDiagnostic {
    let mut diag = CommonDiagnostic::error(err.to_string());
    if let Some(merge_error) = err.downcast_ref::<MergeSemanticError>() {
        diag = diag.with_label(Label::primary(merge_error.span).with_message("error here"));
    }
    diag
}

fn resolve_class_for_completion<'a>(
    tree: &'a ast::ClassTree,
    class_name: &str,
) -> Option<&'a ast::ClassDef> {
    if let Some(class) = tree.get_class_by_qualified_name(class_name) {
        return Some(class);
    }

    let suffix = format!(".{class_name}");
    let mut matched_name: Option<&str> = None;
    for name in tree.name_map.keys() {
        if !(name == class_name || name.ends_with(&suffix)) {
            continue;
        }
        if matched_name.is_some() {
            return None;
        }
        matched_name = Some(name);
    }
    matched_name.and_then(|name| tree.get_class_by_qualified_name(name))
}

fn collect_class_component_members(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    members: &mut IndexMap<String, String>,
    visiting: &mut std::collections::HashSet<DefId>,
) {
    if let Some(def_id) = class.def_id
        && !visiting.insert(def_id)
    {
        return;
    }

    for ext in &class.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base_class) = tree.get_class_by_def_id(base_def_id) else {
            continue;
        };
        collect_class_component_members(tree, base_class, members, visiting);
        for break_name in &ext.break_names {
            members.shift_remove(break_name);
        }
    }

    for (name, component) in &class.components {
        members.insert(name.clone(), component.type_name.to_string());
    }

    if let Some(def_id) = class.def_id {
        visiting.remove(&def_id);
    }
}

fn missing_inner_label(idx: usize, span: Span) -> Label {
    let label = match idx {
        0 => Label::primary(span),
        _ => Label::secondary(span),
    };
    label.with_message("missing matching `inner`")
}

fn diagnostics_to_anyhow(diags: &CommonDiagnostics) -> anyhow::Error {
    let message = diags
        .iter()
        .map(|d| d.message.clone())
        .collect::<Vec<_>>()
        .join("; ");
    if message.is_empty() {
        anyhow::anyhow!("Resolve errors")
    } else {
        anyhow::anyhow!("Resolve errors: {message}")
    }
}

fn phase_result_to_failure(
    model_name: &str,
    result: &PhaseResult,
) -> Option<ModelFailureDiagnostic> {
    match result {
        PhaseResult::Success(_) => None,
        PhaseResult::NeedsInner { missing_inners } => Some(ModelFailureDiagnostic {
            model_name: model_name.to_string(),
            phase: Some(FailedPhase::Instantiate),
            error_code: None,
            error: format!(
                "model needs inner declarations: {}",
                missing_inners.join(", ")
            ),
        }),
        PhaseResult::Failed {
            phase,
            error,
            error_code,
        } => Some(ModelFailureDiagnostic {
            model_name: model_name.to_string(),
            phase: Some(*phase),
            error_code: error_code.clone(),
            error: error.clone(),
        }),
    }
}

fn collect_parse_failures(documents: &IndexMap<String, Document>) -> Vec<ModelFailureDiagnostic> {
    documents
        .values()
        .filter_map(|doc| {
            doc.parse_error.as_ref().map(|err| ModelFailureDiagnostic {
                model_name: doc.uri.clone(),
                phase: None,
                error_code: Some("syntax-error".to_string()),
                error: err.clone(),
            })
        })
        .collect()
}

fn collect_related_model_names(model_names: &[String], requested_model: &str) -> Vec<String> {
    let mut targets = IndexSet::new();
    targets.insert(requested_model.to_string());

    let requested_prefix = format!("{requested_model}.");
    let parent = requested_model.rsplit_once('.').map(|(prefix, _)| prefix);
    let parent_prefix = parent.map(|prefix| format!("{prefix}."));

    for name in model_names {
        let same_or_nested_requested =
            name == requested_model || name.starts_with(&requested_prefix);
        let parent_related = match (parent, parent_prefix.as_deref()) {
            (Some(parent), Some(parent_prefix)) => {
                name == parent || name.starts_with(parent_prefix)
            }
            _ => false,
        };
        if same_or_nested_requested || parent_related {
            targets.insert(name.clone());
        }
    }

    targets.into_iter().collect()
}

fn same_path(left: &str, right: &str) -> bool {
    let left_key = std::fs::canonicalize(left).unwrap_or_else(|_| PathBuf::from(left));
    let right_key = std::fs::canonicalize(right).unwrap_or_else(|_| PathBuf::from(right));
    left_key == right_key
}

fn is_simulatable_class_type(class_type: &ast::ClassType) -> bool {
    matches!(
        class_type,
        ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class
    )
}

fn is_library_tree(tree: &ast::ClassTree) -> bool {
    tree.definitions.classes.len() > 1
}

fn summarize_typecheck_error_code(diags: &CommonDiagnostics) -> Option<String> {
    let mut codes = diags.iter().filter_map(|d| d.code.as_deref());
    let Some(first) = codes.next() else {
        return Some("ET000".to_string());
    };
    if codes.all(|code| code == first) {
        Some(first.to_string())
    } else {
        Some("ET000".to_string())
    }
}

fn todae_options_for_tree(tree: &ast::ClassTree) -> ToDaeOptions {
    ToDaeOptions {
        error_on_unbalanced: !is_library_tree(tree),
    }
}

fn flatten_options_for_tree() -> FlattenOptions {
    // Connection compatibility is model-local at flatten time (overlay-scoped),
    // so strict validation should always be enabled for compiled models even
    // when the source tree contains many library classes.
    FlattenOptions {
        strict_connection_validation: true,
    }
}

/// Internal function for parallel compilation.
///
/// Uses the phase order: Instantiate -> Typecheck -> Flatten -> ToDae
/// Type checking runs after instantiation so it has full access to the
/// modification context for dimension evaluation (MLS §10.1).
fn compile_model_internal(tree: &ast::ClassTree, model_name: &str) -> PhaseResult {
    let experiment_settings = experiment_settings_for_model(tree, model_name);

    // Phase 1: Instantiate
    let instantiate_start = maybe_start_timer();
    let instantiate_outcome = instantiate_model_with_outcome(tree, model_name);
    maybe_record_compile_phase_timing(FailedPhase::Instantiate, instantiate_start);
    let mut overlay = match instantiate_outcome {
        InstantiationOutcome::Success(o) => o,
        InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            return PhaseResult::NeedsInner { missing_inners };
        }
        InstantiationOutcome::Error(e) => {
            use miette::Diagnostic;
            let error_code = e.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::Instantiate,
                error: format!("{}", e),
                error_code,
            };
        }
    };

    // Phase 2: Typecheck (evaluates dimensions with modification context)
    let typecheck_start = maybe_start_timer();
    let typecheck_result = typecheck_instanced(tree, &mut overlay, model_name);
    maybe_record_compile_phase_timing(FailedPhase::Typecheck, typecheck_start);
    if let Err(diags) = typecheck_result {
        return PhaseResult::Failed {
            phase: FailedPhase::Typecheck,
            error: diags
                .iter()
                .map(|d| d.message.clone())
                .collect::<Vec<_>>()
                .join("; "),
            error_code: summarize_typecheck_error_code(&diags),
        };
    }

    // Phase 3: Flatten
    let flatten_start = maybe_start_timer();
    let flat_result =
        flatten_ref_with_options(tree, &overlay, model_name, flatten_options_for_tree());
    maybe_record_compile_phase_timing(FailedPhase::Flatten, flatten_start);
    let flat = match flat_result {
        Ok(f) => f,
        Err(e) => {
            use miette::Diagnostic;
            let error_code = e.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::Flatten,
                error: format!("{}", e),
                error_code,
            };
        }
    };

    // Phase 4: ToDae
    let todae_start = maybe_start_timer();
    let dae_result = to_dae_with_options(&flat, todae_options_for_tree(tree));
    maybe_record_compile_phase_timing(FailedPhase::ToDae, todae_start);
    let dae = match dae_result {
        Ok(d) => d,
        Err(e) => {
            use miette::Diagnostic;
            let error_code = e.code().map(|c| c.to_string());
            return PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: format!("{}", e),
                error_code,
            };
        }
    };

    PhaseResult::Success(Box::new(CompilationResult {
        flat,
        dae,
        experiment_start_time: experiment_settings.start_time,
        experiment_stop_time: experiment_settings.stop_time,
        experiment_tolerance: experiment_settings.tolerance,
        experiment_interval: experiment_settings.interval,
        experiment_solver: experiment_settings.solver,
    }))
}

impl Default for Session {
    fn default() -> Self {
        Self::new(SessionConfig::default())
    }
}

// ============================================================================
// CompiledLibrary - convenience wrapper around Session
// ============================================================================

/// Pre-compiled library for efficient multi-model compilation.
///
/// This is a convenience wrapper around [`Session`] that initializes from
/// a [`ast::StoredDefinition`]. Use this when you've already parsed your library
/// (e.g., from parallel file parsing) and want to compile multiple models.
///
/// For new code, consider using [`Session`] directly with [`Session::add_parsed_batch`].
pub struct CompiledLibrary {
    session: Session,
}

impl CompiledLibrary {
    /// Create a compiled library from a ast::StoredDefinition.
    ///
    /// This resolves the AST once. Type checking happens after instantiation.
    pub fn from_stored_definition(def: ast::StoredDefinition) -> Result<Self> {
        let mut session = Session::new(SessionConfig::default());
        session.add_parsed("library", def);
        // Build resolved tree to catch resolution errors early
        session.build_resolved()?;
        Ok(Self { session })
    }

    /// Create a compiled library from an already-resolved tree.
    ///
    /// This avoids re-running resolve and is intended for callers that already
    /// hold a validated resolved tree (e.g., MSL regression harness).
    pub fn from_resolved_tree(resolved: ast::ResolvedTree, model_names: Vec<String>) -> Self {
        let mut session = Session::new(SessionConfig::default());
        session.resolved = Some(Arc::new(resolved));
        session.model_names = model_names;
        Self { session }
    }

    /// Get all model names in the library.
    ///
    /// This is infallible after construction since build_resolved was called.
    pub fn model_names(&self) -> &[String] {
        &self.session.model_names
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
        // SAFETY: SessionView::new/from_stored_definition call build_resolved()
        // which sets self.session.resolved = Some(...) before returning Ok(Self { session }).
        self.session
            .resolved
            .as_ref()
            .expect("SessionView invariant: resolved tree must be set during construction")
    }

    /// Compile a specific model.
    ///
    /// Uses the new phase order: Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model(&self, model_name: &str) -> Result<CompilationResult> {
        let tree = &self.resolved_tree().0;
        match compile_model_internal(tree, model_name) {
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
        let tree = &self.resolved_tree().0;
        compile_model_internal(tree, model_name)
    }

    /// Compile multiple models in parallel.
    pub fn compile_models_parallel(&self, model_names: &[&str]) -> Vec<(String, PhaseResult)> {
        let tree = &self.resolved_tree().0;
        model_names
            .par_iter()
            .map(|name| {
                let result = compile_model_internal(tree, name);
                (name.to_string(), result)
            })
            .collect()
    }

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&self) -> Vec<(String, PhaseResult)> {
        let tree = &self.resolved_tree().0;
        self.session
            .model_names
            .par_iter()
            .map(|name| {
                let result = compile_model_internal(tree, name);
                (name.clone(), result)
            })
            .collect()
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
