//! LSP server implementation (native only, behind "server" feature).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Instant;

use rumoca_session::compile::{
    CompilePhaseTimingSnapshot, Document, PhaseResult, Session, SessionCacheStatsSnapshot,
    SessionChange, SessionConfig, SessionSnapshot, SourceRootKind, compile_phase_timing_stats,
    session_cache_stats,
};
use rumoca_session::libraries::{
    LibraryCacheStatus, LibraryCacheTiming, PackageLayoutError, infer_library_roots,
    parse_library_with_cache, should_load_library_for_source,
};
use rumoca_session::parsing::{
    ast, collect_compile_unit_source_files, collect_model_names, merge_stored_definitions,
    parse_source_to_ast,
};
use rumoca_session::project::{
    EffectiveSimulationConfig, EffectiveSimulationPreset, PlotViewConfig, ProjectConfig,
    ProjectFileMoveHint, SimulationModelOverride, clear_model_simulation_preset,
    load_plot_views_for_model, load_simulation_snapshot_for_model,
    resync_model_sidecars_with_move_hints, write_model_simulation_preset,
    write_plot_views_for_model,
};
use rumoca_session::runtime::{
    SimOptions, SimResult, SimSolverMode, dae_balance, dae_balance_detail, simulate_dae,
};
use serde::Deserialize;
use serde_json::{Value, json};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

#[cfg(test)]
use crate::completion_metrics::extract_import_completion_prefix;
use crate::completion_metrics::extract_library_completion_prefix;
use crate::handlers;
use crate::helpers::get_word_at_position;
mod completion;
mod diagnostics;
mod lanes;
mod navigation;
mod preview;
mod project_commands;
mod simulation_jobs;
mod support;
use completion::*;
use lanes::*;
use preview::{
    append_markdown_hover, class_target_definition, class_target_hover,
    flattened_preview_for_model, is_hover_preview_candidate, local_component_hover,
};
use simulation_jobs::{SimulationCompileKey, SimulationPrewarmKey, SimulationPrewarmState};
use support::*;

/// Modelica Language Server.
#[derive(Clone)]
pub struct ModelicaLanguageServer {
    client: Client,
    session: Arc<RwLock<Session>>,
    work_lanes: Arc<ServerWorkLanes>,
    initial_library_paths: Arc<RwLock<Vec<String>>>,
    library_paths: Arc<RwLock<Vec<String>>>,
    loaded_libraries: Arc<RwLock<HashSet<String>>>,
    document_versions: Arc<RwLock<HashMap<String, i32>>>,
    completion_mutation_epoch: Arc<AtomicU64>,
    library_load_diagnostics: Arc<RwLock<HashMap<String, Vec<Diagnostic>>>>,
    library_load_diagnostic_uris: Arc<RwLock<HashMap<String, HashSet<String>>>>,
    loading_libraries: Arc<RwLock<HashMap<String, u64>>>,
    library_state_epoch: Arc<AtomicU64>,
    workspace_root: Arc<RwLock<Option<PathBuf>>>,
    project_config: Arc<RwLock<Option<ProjectConfig>>>,
    completion_timing_path: Arc<RwLock<Option<PathBuf>>>,
    diagnostics_timing_path: Arc<RwLock<Option<PathBuf>>>,
    navigation_timing_path: Arc<RwLock<Option<PathBuf>>>,
    startup_timing_path: Arc<RwLock<Option<PathBuf>>>,
    simulation_compile_cache:
        Arc<RwLock<HashMap<SimulationCompileKey, rumoca_session::compile::DaeCompilationResult>>>,
    simulation_prewarm_state:
        Arc<RwLock<HashMap<SimulationPrewarmKey, Arc<SimulationPrewarmState>>>>,
    selected_simulation_models: Arc<RwLock<HashMap<String, String>>>,
    background_request_sequence: Arc<AtomicU64>,
    namespace_prewarm_lane: Arc<tokio::sync::Mutex<()>>,
    namespace_prewarm_state: Arc<RwLock<Option<NamespacePrewarmState>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagnosticsTrigger {
    Live,
    Save,
}

impl ModelicaLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            session: Arc::new(RwLock::new(Session::new(SessionConfig::default()))),
            work_lanes: Arc::new(ServerWorkLanes::default()),
            initial_library_paths: Arc::new(RwLock::new(Vec::new())),
            library_paths: Arc::new(RwLock::new(Vec::new())),
            loaded_libraries: Arc::new(RwLock::new(HashSet::new())),
            document_versions: Arc::new(RwLock::new(HashMap::new())),
            completion_mutation_epoch: Arc::new(AtomicU64::new(0)),
            library_load_diagnostics: Arc::new(RwLock::new(HashMap::new())),
            library_load_diagnostic_uris: Arc::new(RwLock::new(HashMap::new())),
            loading_libraries: Arc::new(RwLock::new(HashMap::new())),
            library_state_epoch: Arc::new(AtomicU64::new(0)),
            workspace_root: Arc::new(RwLock::new(None)),
            project_config: Arc::new(RwLock::new(None)),
            completion_timing_path: Arc::new(RwLock::new(None)),
            diagnostics_timing_path: Arc::new(RwLock::new(None)),
            navigation_timing_path: Arc::new(RwLock::new(None)),
            startup_timing_path: Arc::new(RwLock::new(None)),
            simulation_compile_cache: Arc::new(RwLock::new(HashMap::new())),
            simulation_prewarm_state: Arc::new(RwLock::new(HashMap::new())),
            selected_simulation_models: Arc::new(RwLock::new(HashMap::new())),
            background_request_sequence: Arc::new(AtomicU64::new(0)),
            namespace_prewarm_lane: Arc::new(tokio::sync::Mutex::new(())),
            namespace_prewarm_state: Arc::new(RwLock::new(None)),
        }
    }

    fn server_capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![".".into()]),
                resolve_provider: Some(false),
                ..Default::default()
            }),
            document_symbol_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: handlers::get_semantic_token_legend(),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    ..Default::default()
                }),
            ),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            })),
            workspace_symbol_provider: Some(OneOf::Left(true)),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".into(), ",".into()]),
                retrigger_characters: None,
                work_done_progress_options: WorkDoneProgressOptions::default(),
            }),
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            document_formatting_provider: Some(OneOf::Left(true)),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(true),
            }),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            inlay_hint_provider: None,
            document_link_provider: Some(DocumentLinkOptions {
                resolve_provider: Some(false),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            }),
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec![
                    "rumoca.project.getSimulationConfig".to_string(),
                    "rumoca.project.setSimulationPreset".to_string(),
                    "rumoca.project.resetSimulationPreset".to_string(),
                    "rumoca.project.getVisualizationConfig".to_string(),
                    "rumoca.project.setVisualizationConfig".to_string(),
                    "rumoca.project.resyncSidecars".to_string(),
                    "rumoca.project.filesMoved".to_string(),
                    "rumoca.project.simulate".to_string(),
                    "rumoca.project.getSimulationModels".to_string(),
                    "rumoca.project.setSelectedSimulationModel".to_string(),
                    "rumoca.project.startSimulation".to_string(),
                    "rumoca.project.prepareSimulationModels".to_string(),
                ],
                work_done_progress_options: WorkDoneProgressOptions::default(),
            }),
            ..Default::default()
        }
    }

    fn completion_mutation_epoch(&self) -> u64 {
        self.completion_mutation_epoch.load(Ordering::Acquire)
    }

    fn completion_request_is_stale(&self, request_epoch: u64) -> bool {
        self.completion_mutation_epoch() != request_epoch
    }

    async fn current_analysis_revision(&self) -> u64 {
        self.session.read().await.revision()
    }

    async fn begin_analysis_request(&self) -> AnalysisRequestToken {
        AnalysisRequestToken {
            mutation_epoch: self.completion_mutation_epoch(),
            session_revision: self.current_analysis_revision().await,
        }
    }

    async fn refresh_analysis_request_revision(
        &self,
        token: AnalysisRequestToken,
    ) -> AnalysisRequestToken {
        AnalysisRequestToken {
            session_revision: self.current_analysis_revision().await,
            ..token
        }
    }

    async fn analysis_request_is_stale(&self, token: AnalysisRequestToken) -> bool {
        self.completion_request_is_stale(token.mutation_epoch)
            || self.current_analysis_revision().await != token.session_revision
    }

    async fn finish_namespace_prewarm(&self, state: &NamespacePrewarmState) {
        state.done.store(true, Ordering::Release);
        state.finished.notify_waiters();
        let mut pending = self.namespace_prewarm_state.write().await;
        if pending
            .as_ref()
            .is_some_and(|current| current.session_revision == state.session_revision)
        {
            *pending = None;
        }
    }

    async fn wait_for_namespace_prewarm_if_pending(&self) {
        let current_revision = self.current_analysis_revision().await;
        let pending = self.namespace_prewarm_state.read().await.clone();
        let Some(state) = pending else {
            return;
        };
        if state.session_revision != current_revision || state.done.load(Ordering::Acquire) {
            return;
        }
        let notified = state.finished.notified();
        if state.done.load(Ordering::Acquire) {
            return;
        }
        notified.await;
    }

    async fn is_loaded_library_document(&self, uri_path: &str) -> bool {
        self.session
            .read()
            .await
            .is_loaded_library_document(uri_path)
    }

    async fn wait_for_namespace_prewarm_for_loaded_library_document(&self, uri_path: &str) {
        if !self.is_loaded_library_document(uri_path).await {
            return;
        }
        self.wait_for_namespace_prewarm_if_pending().await;
    }

    async fn spawn_background_namespace_prewarm_for_loaded_library_document(&self, uri_path: &str) {
        if !self.is_loaded_library_document(uri_path).await {
            return;
        }
        self.spawn_background_namespace_prewarm().await;
    }

    async fn prewarm_namespace_cache_for_loaded_library_document(&self, uri_path: &str) {
        if !self.is_loaded_library_document(uri_path).await {
            return;
        }
        let mut session = self.session.write().await;
        let _ = session.namespace_index_query("");
    }

    async fn prewarm_workspace_symbols_for_loaded_library_document(&self, uri_path: &str) {
        if !self.is_loaded_library_document(uri_path).await {
            return;
        }
        self.prewarm_workspace_symbol_queries().await;
    }

    async fn run_namespace_prewarm_snapshot(
        &self,
        snapshot: SessionSnapshot,
        session_revision: u64,
    ) {
        let _prewarm_guard = self.namespace_prewarm_lane.lock().await;
        if self.namespace_prewarm_is_stale(session_revision).await {
            return;
        }
        let _ = snapshot.namespace_index_query("");
    }

    async fn prewarm_workspace_symbol_queries(&self) {
        if self.initial_library_paths.read().await.is_empty() {
            return;
        }
        let snapshot = self.session.read().await.workspace_symbol_snapshot();
        snapshot.prewarm_workspace_symbol_queries();
    }

    fn library_path_keys(paths: &[String]) -> Vec<String> {
        paths.iter().map(|path| canonical_path_key(path)).collect()
    }

    async fn resolved_library_paths(&self) -> Vec<String> {
        let initial = self.initial_library_paths.read().await.clone();
        let project_paths = self
            .project_config
            .read()
            .await
            .as_ref()
            .map(|cfg| cfg.resolve_all_library_paths())
            .unwrap_or_default();
        let mut seen = HashSet::new();
        let mut merged = Vec::new();
        for path in project_paths.into_iter().chain(initial) {
            let key = canonical_path_key(&path);
            if seen.insert(key) {
                merged.push(path);
            }
        }
        merged
    }

    async fn library_source_root_kind(&self, library_path: &str) -> SourceRootKind {
        let library_path_key = canonical_path_key(library_path);
        let initial_library_paths = self.initial_library_paths.read().await.clone();
        if initial_library_paths
            .iter()
            .any(|path| canonical_path_key(path) == library_path_key)
        {
            return SourceRootKind::DurableLibrary;
        }
        SourceRootKind::Library
    }

    async fn reset_session_and_loaded_libraries(&self) {
        self.library_state_epoch.fetch_add(1, Ordering::AcqRel);
        *self.namespace_prewarm_state.write().await = None;
        self.clear_simulation_compile_cache().await;
        let docs = {
            let session = self.session.read().await;
            session
                .document_uris()
                .into_iter()
                .filter_map(|uri| session.get_document(uri))
                .filter(|doc| !doc.content.is_empty())
                .map(|doc| (doc.uri.clone(), doc.content.clone()))
                .collect::<Vec<_>>()
        };

        let mut rebuilt = Session::new(SessionConfig::default());
        let mut change = SessionChange::default();
        for (uri, content) in docs {
            change.set_file_text(uri, content);
        }
        if !change.is_empty() {
            rebuilt.apply_change(change);
        }
        *self.session.write().await = rebuilt;
        let mut loaded = self.loaded_libraries.write().await;
        let mut loading = self.loading_libraries.write().await;
        loaded.clear();
        loading.clear();
    }

    async fn record_open_document_version(&self, uri_path: &str, version: i32) {
        self.document_versions
            .write()
            .await
            .insert(uri_path.to_string(), version);
    }

    async fn try_accept_document_version(&self, uri_path: &str, version: i32) -> bool {
        let mut versions = self.document_versions.write().await;
        if versions
            .get(uri_path)
            .is_some_and(|current| version <= *current)
        {
            return false;
        }
        versions.insert(uri_path.to_string(), version);
        true
    }

    async fn document_snapshot(&self, uri_path: &str) -> Option<Document> {
        self.session.read().await.get_document(uri_path).cloned()
    }

    async fn session_snapshot(&self) -> SessionSnapshot {
        self.session.read().await.snapshot()
    }

    async fn document_analysis_snapshot(
        &self,
        uri_path: &str,
    ) -> Option<(Document, SessionSnapshot)> {
        let session = self.session.read().await;
        let document = session.get_document(uri_path).cloned()?;
        Some((document, session.lightweight_snapshot()))
    }

    async fn document_lightweight_snapshot(&self, uri_path: &str) -> Option<SessionSnapshot> {
        let session = self.session.read().await;
        session.get_document(uri_path)?;
        Some(session.lightweight_snapshot())
    }

    async fn prewarm_document_semantic_queries(&self, uri_path: &str) {
        if self.is_loaded_library_document(uri_path).await {
            return;
        }
        let Some(snapshot) = self.document_lightweight_snapshot(uri_path).await else {
            return;
        };
        snapshot.prewarm_document_ide_queries(uri_path);
    }

    async fn stored_library_load_diagnostics(&self, uri_path: &str) -> Vec<Diagnostic> {
        self.library_load_diagnostics
            .read()
            .await
            .get(&canonical_path_key(uri_path))
            .cloned()
            .unwrap_or_default()
    }

    async fn replace_library_load_diagnostics(
        &self,
        lib_path: &str,
        diagnostics_by_uri: HashMap<String, Vec<Diagnostic>>,
    ) {
        let path_key = canonical_path_key(lib_path);
        let new_keys: HashSet<String> = diagnostics_by_uri.keys().cloned().collect();
        let old_keys = {
            let mut stored = self.library_load_diagnostics.write().await;
            let mut owners = self.library_load_diagnostic_uris.write().await;
            let old_keys = owners.remove(&path_key).unwrap_or_default();
            for uri in &old_keys {
                stored.remove(uri);
            }
            for (uri, diagnostics) in &diagnostics_by_uri {
                stored.insert(uri.clone(), diagnostics.clone());
            }
            if !new_keys.is_empty() {
                owners.insert(path_key, new_keys.clone());
            }
            old_keys
        };

        for (uri_path, diagnostics) in diagnostics_by_uri {
            if let Ok(uri) = Url::from_file_path(&uri_path) {
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
        }
        for cleared in old_keys.difference(&new_keys) {
            if let Ok(uri) = Url::from_file_path(cleared) {
                self.client.publish_diagnostics(uri, Vec::new(), None).await;
            }
        }
    }

    async fn write_workspace_symbol_timing(
        &self,
        request_token: AnalysisRequestToken,
        request_started: Instant,
        timing: WorkspaceSymbolTimingBreakdown,
        semantic_layer: &'static str,
        stats_before: SessionCacheStatsSnapshot,
    ) {
        let stats_after = session_cache_stats();
        let navigation_timing_path = self.navigation_timing_path.read().await.clone();
        let session_cache_delta = stats_after.delta_since(stats_before);
        let request_was_stale = self.analysis_request_is_stale(request_token).await;
        write_navigation_timing_summary(
            &NavigationTimingSummary {
                requested_edit_epoch: request_token.mutation_epoch,
                request_was_stale,
                uri: String::new(),
                request: "workspace_symbol",
                request_path: NavigationRequestPath::QueryOnly,
                semantic_layer,
                total_ms: request_started.elapsed().as_millis() as u64,
                snapshot_ms: Some(timing.snapshot_ms),
                snapshot_lock_ms: Some(timing.snapshot_lock_ms),
                snapshot_build_ms: Some(timing.snapshot_build_ms),
                detail: timing.snapshot_detail.clone(),
                query_ms: timing.query_ms,
                format_ms: timing.format_ms,
                built_resolved_tree: session_cache_delta.semantic_navigation_builds >= 1,
                had_resolved_cache_before: false,
                session_cache_delta,
            },
            navigation_timing_path.as_deref(),
        );
    }

    async fn publish_package_layout_diagnostics(&self, lib_path: &str, err: &PackageLayoutError) {
        self.replace_library_load_diagnostics(
            lib_path,
            library_load_diagnostics_for_package_layout_error(err),
        )
        .await;
    }

    async fn maybe_publish_package_layout_diagnostics(&self, lib_path: &str, err: &anyhow::Error) {
        let Some(layout) = err.downcast_ref::<PackageLayoutError>() else {
            return;
        };
        self.publish_package_layout_diagnostics(lib_path, layout)
            .await;
    }

    async fn rebuild_dirty_libraries_before_compile(
        &self,
        current_document_path: &str,
    ) -> std::result::Result<(), String> {
        let dirty_roots = {
            let session = self.session.read().await;
            session.dirty_library_source_root_keys()
        };
        if dirty_roots.is_empty() {
            return Ok(());
        }

        for source_set_key in &dirty_roots {
            let Some(library_root) = library_source_root_path(source_set_key) else {
                continue;
            };
            notify_library_indexing_started(
                &self.client,
                &library_root,
                LibraryIndexingReason::SimulationCompile,
            )
            .await;
            let parsed = match parse_library_with_cache(Path::new(&library_root)) {
                Ok(parsed) => parsed,
                Err(err) => {
                    let error_message = library_load_error_message(&library_root, &err);
                    notify_library_indexing_failed(
                        &self.client,
                        &library_root,
                        LibraryIndexingReason::SimulationCompile,
                        &error_message,
                    )
                    .await;
                    self.maybe_publish_package_layout_diagnostics(&library_root, &err)
                        .await;
                    return Err(error_message);
                }
            };
            self.replace_library_load_diagnostics(&library_root, HashMap::new())
                .await;
            let source_root_kind = self.library_source_root_kind(&library_root).await;
            let inserted_count = {
                let mut session = self.session.write().await;
                session.replace_parsed_source_set(
                    source_set_key,
                    source_root_kind,
                    parsed.documents,
                    Some(current_document_path),
                )
            };
            self.library_state_epoch.fetch_add(1, Ordering::AcqRel);
            self.clear_simulation_compile_cache().await;
            notify_library_indexing_finished(
                &self.client,
                &library_root,
                LibraryIndexingReason::SimulationCompile.label(),
                false,
                parsed.file_count,
                inserted_count,
                parsed.cache_status,
            )
            .await;
            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "[rumoca] Rebuilt dirty library {} ({:?}) — {} files inserted",
                        library_root, parsed.cache_status, inserted_count
                    ),
                )
                .await;
        }
        Ok(())
    }

    fn current_library_state_epoch(&self) -> u64 {
        self.library_state_epoch.load(Ordering::Acquire)
    }

    async fn namespace_prewarm_is_stale(&self, session_revision: u64) -> bool {
        self.current_analysis_revision().await != session_revision
    }

    async fn reserve_library_load(&self, path_key: &str, expected_epoch: u64) -> bool {
        let loaded = self.loaded_libraries.write().await;
        let mut loading = self.loading_libraries.write().await;
        let current_epoch = self.current_library_state_epoch();
        if !should_reserve_library_load(&loaded, &loading, path_key, expected_epoch, current_epoch)
        {
            return false;
        }
        loading.insert(path_key.to_string(), expected_epoch);
        true
    }

    async fn cancel_library_load(&self, path_key: &str, reservation_epoch: u64) {
        let mut loading = self.loading_libraries.write().await;
        if should_clear_library_load(&loading, path_key, reservation_epoch) {
            loading.remove(path_key);
        }
    }

    async fn apply_parsed_library_if_current(
        &self,
        source_set_id: &str,
        source_root_kind: SourceRootKind,
        path_key: &str,
        current_document_path: Option<&str>,
        documents: Vec<(String, ast::StoredDefinition)>,
        expected_epoch: u64,
    ) -> Option<AppliedLibraryLoad> {
        let mut session = self.session.write().await;
        let mut loaded = self.loaded_libraries.write().await;
        let mut loading = self.loading_libraries.write().await;
        let current_epoch = self.current_library_state_epoch();
        if !should_apply_library_load(&loaded, path_key, expected_epoch, current_epoch) {
            if should_clear_library_load(&loading, path_key, expected_epoch) {
                loading.remove(path_key);
            }
            return None;
        }
        let apply_started = Instant::now();
        let inserted = session.replace_parsed_source_set(
            source_set_id,
            source_root_kind,
            documents,
            current_document_path,
        );
        loaded.insert(path_key.to_string());
        if should_clear_library_load(&loading, path_key, expected_epoch) {
            loading.remove(path_key);
        }
        drop(loading);
        drop(loaded);
        drop(session);
        self.library_state_epoch.fetch_add(1, Ordering::AcqRel);
        self.clear_simulation_compile_cache().await;
        Some(AppliedLibraryLoad {
            inserted_file_count: inserted,
            apply_ms: apply_started.elapsed().as_millis() as u64,
        })
    }

    async fn load_library_source_set_if_current(
        &self,
        lib_path: &str,
        path_key: &str,
        source_set_id: &str,
        current_document_path: Option<&str>,
        expected_epoch: u64,
        reason: LibraryIndexingReason,
    ) -> std::result::Result<Option<LibraryLoadOutcome>, String> {
        if !self.reserve_library_load(path_key, expected_epoch).await {
            return Ok(None);
        }

        notify_library_indexing_started(&self.client, lib_path, reason).await;

        let parsed = match parse_library_with_cache(Path::new(lib_path)) {
            Ok(parsed) => parsed,
            Err(err) => {
                self.cancel_library_load(path_key, expected_epoch).await;
                let error_message = library_load_error_message(lib_path, &err);
                notify_library_indexing_failed(&self.client, lib_path, reason, &error_message)
                    .await;
                self.maybe_publish_package_layout_diagnostics(lib_path, &err)
                    .await;
                return Err(error_message);
            }
        };
        self.replace_library_load_diagnostics(lib_path, HashMap::new())
            .await;

        let cache_status = parsed.cache_status;
        let cache_key = parsed.cache_key.clone();
        let cache_timing = parsed.timing;
        let source_root_kind = self.library_source_root_kind(lib_path).await;
        let cache_path = parsed
            .cache_file
            .as_deref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "<none>".to_string());
        let indexed_file_count = parsed.file_count;
        let Some(applied) = self
            .apply_parsed_library_if_current(
                source_set_id,
                source_root_kind,
                path_key,
                current_document_path,
                parsed.documents,
                expected_epoch,
            )
            .await
        else {
            notify_library_indexing_finished(
                &self.client,
                lib_path,
                reason.stale_label(),
                reason == LibraryIndexingReason::StartupDurablePrewarm,
                indexed_file_count,
                0,
                cache_status,
            )
            .await;
            return Ok(None);
        };

        notify_library_indexing_finished(
            &self.client,
            lib_path,
            reason.label(),
            reason == LibraryIndexingReason::StartupDurablePrewarm,
            indexed_file_count,
            applied.inserted_file_count,
            cache_status,
        )
        .await;

        Ok(Some(LibraryLoadOutcome {
            cache_status,
            indexed_file_count,
            inserted_file_count: applied.inserted_file_count,
            cache_key,
            cache_path,
            timing: DurableLibraryLoadTiming {
                cache: cache_timing,
                apply_ms: applied.apply_ms,
            },
        }))
    }

    async fn reload_project_config(&self) {
        let _ = self.reload_project_config_with_timing().await;
    }

    async fn reload_project_config_with_timing(&self) -> ProjectReloadTiming {
        let reload_started = Instant::now();
        let previous_paths = self.library_paths.read().await.clone();
        let workspace_root = self.workspace_root.read().await.clone();
        let mut timing = ProjectReloadTiming::default();
        let next_paths = if let Some(workspace_root) = workspace_root {
            let discover_started = Instant::now();
            match ProjectConfig::discover(&workspace_root) {
                Ok(config) => {
                    self.log_project_diagnostics(
                        config
                            .as_ref()
                            .map_or(&[], |cfg| cfg.diagnostics.as_slice()),
                    )
                    .await;
                    *self.project_config.write().await = config;
                }
                Err(error) => {
                    *self.project_config.write().await = None;
                    self.client
                        .log_message(
                            MessageType::WARNING,
                            format!("[rumoca] failed to load .rumoca/project.toml: {error}"),
                        )
                        .await;
                }
            }
            timing.project_discover_ms = discover_started.elapsed().as_millis() as u64;
            let resolve_started = Instant::now();
            let next_paths = self.resolved_library_paths().await;
            timing.resolve_library_paths_ms = resolve_started.elapsed().as_millis() as u64;
            next_paths
        } else {
            *self.project_config.write().await = None;
            let resolve_started = Instant::now();
            let next_paths = self.resolved_library_paths().await;
            timing.resolve_library_paths_ms = resolve_started.elapsed().as_millis() as u64;
            next_paths
        };

        let should_reset =
            Self::library_path_keys(&previous_paths) != Self::library_path_keys(&next_paths);
        timing.library_paths_changed = should_reset;
        *self.library_paths.write().await = next_paths;
        if should_reset {
            let reset_started = Instant::now();
            self.reset_session_and_loaded_libraries().await;
            timing.reset_session_ms = reset_started.elapsed().as_millis() as u64;
            let initial_startup =
                previous_paths.is_empty() && self.session.read().await.document_uris().is_empty();

            let durable_started = Instant::now();
            let durable_timing = self.prewarm_durable_libraries().await;
            timing.durable_prewarm_ms = durable_started.elapsed().as_millis() as u64;
            timing.durable_collect_files_ms = durable_timing.durable_collect_files_ms;
            timing.durable_hash_inputs_ms = durable_timing.durable_hash_inputs_ms;
            timing.durable_cache_lookup_ms = durable_timing.durable_cache_lookup_ms;
            timing.durable_cache_deserialize_ms = durable_timing.durable_cache_deserialize_ms;
            timing.durable_parse_files_ms = durable_timing.durable_parse_files_ms;
            timing.durable_validate_layout_ms = durable_timing.durable_validate_layout_ms;
            timing.durable_cache_write_ms = durable_timing.durable_cache_write_ms;
            timing.durable_apply_ms = durable_timing.durable_apply_ms;

            if !initial_startup {
                let workspace_symbol_started = Instant::now();
                self.prewarm_workspace_symbol_queries().await;
                timing.workspace_symbol_prewarm_ms =
                    workspace_symbol_started.elapsed().as_millis() as u64;
            }

            let namespace_started = Instant::now();
            self.spawn_background_namespace_prewarm().await;
            timing.namespace_prewarm_spawn_ms = namespace_started.elapsed().as_millis() as u64;
        }
        timing.total_ms = reload_started.elapsed().as_millis() as u64;
        timing
    }

    async fn prewarm_durable_libraries(&self) -> ProjectReloadTiming {
        let durable_library_paths = self.initial_library_paths.read().await.clone();
        let already_loaded = self.loaded_libraries.read().await.clone();
        let (mut seen_library_paths, mut claimed_roots) =
            existing_library_root_claims(&already_loaded);
        let mut library_state_epoch = self.current_library_state_epoch();
        let mut timing = ProjectReloadTiming::default();

        for lib_path in durable_library_paths {
            let path_key = canonical_path_key(&lib_path);
            if !seen_library_paths.insert(path_key.clone()) || already_loaded.contains(&path_key) {
                continue;
            }
            let inferred_roots = infer_library_roots(Path::new(&lib_path)).unwrap_or_default();
            if duplicate_root_provider(&inferred_roots, &claimed_roots).is_some() {
                continue;
            }
            let source_set_id = library_source_set_id(&lib_path);
            let Ok(Some(outcome)) = self
                .load_library_source_set_if_current(
                    &lib_path,
                    &path_key,
                    &source_set_id,
                    None,
                    library_state_epoch,
                    LibraryIndexingReason::StartupDurablePrewarm,
                )
                .await
            else {
                continue;
            };
            outcome.timing.accumulate_into(&mut timing);
            claim_roots(&mut claimed_roots, inferred_roots, &lib_path);
            library_state_epoch = self.current_library_state_epoch();
        }
        timing
    }

    async fn spawn_background_namespace_prewarm(&self) {
        if self.initial_library_paths.read().await.is_empty() {
            return;
        }

        let session_revision = self.current_analysis_revision().await;
        let snapshot = self.session_snapshot().await;

        {
            let pending = self.namespace_prewarm_state.read().await;
            if pending
                .as_ref()
                .is_some_and(|state| state.session_revision == session_revision)
            {
                return;
            }
        }

        let state = NamespacePrewarmState {
            session_revision,
            done: Arc::new(AtomicBool::new(false)),
            finished: Arc::new(tokio::sync::Notify::new()),
        };
        *self.namespace_prewarm_state.write().await = Some(state.clone());
        let server = self.clone();
        tokio::spawn(async move {
            server
                .run_namespace_prewarm_snapshot(snapshot, session_revision)
                .await;
            server.finish_namespace_prewarm(&state).await;
        });
    }

    fn simulation_options_from_settings(
        settings: &SimulationRequestSettings,
        compiled: &rumoca_session::compile::DaeCompilationResult,
    ) -> SimOptions {
        let mut opts = SimOptions {
            t_end: settings.t_end,
            dt: settings.dt,
            solver_mode: SimSolverMode::from_external_name(&settings.solver),
            ..SimOptions::default()
        };
        if let Some(start_time) = compiled.experiment_start_time
            && start_time.is_finite()
        {
            opts.t_start = start_time;
        }
        if opts.t_end <= opts.t_start {
            opts.t_end = opts.t_start + 1.0;
        }
        opts
    }

    fn simulation_success_value(payload: Value, metrics: SimulationMetrics) -> Value {
        json!({
            "ok": true,
            "payload": payload,
            "metrics": {
                "compileSeconds": metrics.compile_elapsed,
                "simulateSeconds": metrics.sim_elapsed,
                "points": metrics.point_count,
                "variables": metrics.variable_count,
                "compilePhaseSeconds": {
                    "prepareContext": metrics.prepare_context_seconds,
                    "buildSnapshot": metrics.build_snapshot_seconds,
                    "strictCompile": metrics.strict_compile_seconds,
                    "strictResolve": metrics.strict_resolve_seconds,
                    "instantiate": metrics.instantiate_seconds,
                    "typecheck": metrics.typecheck_seconds,
                    "flatten": metrics.flatten_seconds,
                    "todae": metrics.todae_seconds,
                },
            }
        })
    }

    fn simulation_metrics(
        compiled: &crate::server::simulation_jobs::SimulationCompileResult,
        compile_elapsed: f64,
        compile_before: CompilePhaseTimingSnapshot,
        compile_after: CompilePhaseTimingSnapshot,
        stats_delta: SessionCacheStatsSnapshot,
    ) -> SimulationMetrics {
        let instantiate_seconds = (compile_after.instantiate.total_seconds()
            - compile_before.instantiate.total_seconds())
        .max(0.0);
        let typecheck_seconds = (compile_after.typecheck.total_seconds()
            - compile_before.typecheck.total_seconds())
        .max(0.0);
        let flatten_seconds = (compile_after.flatten.total_seconds()
            - compile_before.flatten.total_seconds())
        .max(0.0);
        let todae_seconds =
            (compile_after.todae.total_seconds() - compile_before.todae.total_seconds()).max(0.0);
        let strict_resolve_seconds =
            stats_delta.strict_resolved_build_total_nanos as f64 / 1_000_000_000.0;

        SimulationMetrics {
            compile_elapsed,
            sim_elapsed: 0.0,
            point_count: 0,
            variable_count: 0,
            prepare_context_seconds: compiled.timings.prepare_context_seconds,
            build_snapshot_seconds: compiled.timings.build_snapshot_seconds,
            strict_compile_seconds: compiled.timings.strict_compile_seconds,
            strict_resolve_seconds,
            instantiate_seconds,
            typecheck_seconds,
            flatten_seconds,
            todae_seconds,
        }
    }

    async fn stale_simulation_response(
        &self,
        request_token: AnalysisRequestToken,
    ) -> Option<Value> {
        if self.analysis_request_is_stale(request_token).await {
            return Some(Self::simulation_error_value(
                ModelicaLanguageServer::stale_background_request_error(),
            ));
        }
        None
    }

    fn build_simulation_payload(
        sim: &SimResult,
        settings: &SimulationRequestSettings,
        opts: &SimOptions,
        metrics: SimulationMetrics,
    ) -> Value {
        let t_start_actual = sim.times.first().copied().unwrap_or(opts.t_start);
        let t_end_actual = sim.times.last().copied().unwrap_or(opts.t_start);
        let mut all_data = Vec::with_capacity(1 + sim.data.len());
        all_data.push(sim.times.clone());
        all_data.extend(sim.data.clone());

        let sim_details = json!({
            "actual": {
                "t_start": t_start_actual,
                "t_end": t_end_actual,
                "points": sim.times.len(),
                "variables": sim.names.len(),
            },
            "requested": {
                "solver": settings.solver,
                "t_start": opts.t_start,
                "t_end": settings.t_end,
                "dt": settings.dt,
                "rtol": opts.rtol,
                "atol": opts.atol,
            },
            "timing": {
                "compile_seconds": metrics.compile_elapsed,
                "simulate_seconds": metrics.sim_elapsed,
                "compile_phase_seconds": {
                    "prepare_context": metrics.prepare_context_seconds,
                    "build_snapshot": metrics.build_snapshot_seconds,
                    "strict_compile": metrics.strict_compile_seconds,
                    "strict_resolve": metrics.strict_resolve_seconds,
                    "instantiate": metrics.instantiate_seconds,
                    "typecheck": metrics.typecheck_seconds,
                    "flatten": metrics.flatten_seconds,
                    "todae": metrics.todae_seconds,
                }
            }
        });

        json!({
            "version": 1,
            "names": sim.names,
            "allData": all_data,
            "nStates": sim.n_states,
            "variableMeta": sim.variable_meta.iter().map(|meta| {
                json!({
                    "name": meta.name,
                    "role": meta.role,
                    "is_state": meta.is_state,
                    "value_type": meta.value_type,
                    "variability": meta.variability,
                    "time_domain": meta.time_domain,
                    "unit": meta.unit,
                    "start": meta.start,
                    "min": meta.min,
                    "max": meta.max,
                    "nominal": meta.nominal,
                    "fixed": meta.fixed,
                    "description": meta.description,
                })
            }).collect::<Vec<_>>(),
            "simDetails": sim_details,
        })
    }

    async fn execute_simulate_model(
        &self,
        params: Option<Value>,
        request_token: Option<AnalysisRequestToken>,
    ) -> Option<Value> {
        let _strict_lane = self.work_lanes.strict.lock().await;
        let mut request_token = request_token.unwrap_or(self.begin_analysis_request().await);
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        let model = obj.get("model").and_then(Value::as_str)?.trim().to_string();
        if model.is_empty() {
            return Some(Self::simulation_error_value("model is required"));
        }
        let Some(settings) = parse_simulation_request_settings(obj.get("settings")) else {
            return Some(Self::simulation_error_value(
                "invalid simulation settings payload",
            ));
        };
        let source = match self.open_document_source_for_uri(&uri).await {
            Ok(source) => source,
            Err(error) => return Some(Self::simulation_error_value(error)),
        };
        let uri_path = session_document_uri_key(&uri);

        if self
            .wait_for_simulation_prewarm_if_current(&model, &uri_path)
            .await
        {
            request_token = self.refresh_analysis_request_revision(request_token).await;
        }

        let loaded_libraries = if settings.library_paths.is_empty() {
            self.ensure_source_libraries_loaded(&source, &uri_path)
                .await
        } else {
            self.ensure_source_libraries_loaded_with_paths(
                &source,
                &uri_path,
                &settings.library_paths,
            )
            .await
        };
        if loaded_libraries {
            request_token = self.refresh_analysis_request_revision(request_token).await;
        }
        if let Some(response) = self.stale_simulation_response(request_token).await {
            return Some(response);
        }

        let compile_start = std::time::Instant::now();
        let compile_before = compile_phase_timing_stats();
        let stats_before = session_cache_stats();
        let compiled = match self.compile_model_for_simulation(&model, &uri_path).await {
            Ok(result) => result,
            Err(error) => {
                return Some(Self::simulation_error_value(format!(
                    "compilation failed: {error}",
                )));
            }
        };
        if let Some(response) = self.stale_simulation_response(request_token).await {
            return Some(response);
        }
        let compile_after = compile_phase_timing_stats();
        let stats_after = session_cache_stats();
        let stats_delta = stats_after.delta_since(stats_before);
        let compile_elapsed = compile_start.elapsed().as_secs_f64();

        let opts = Self::simulation_options_from_settings(&settings, &compiled.compiled);
        let sim_opts = opts.clone();
        let compiled_dae = compiled.compiled.dae.clone();
        let sim_start = std::time::Instant::now();
        let sim = match tokio::task::spawn_blocking(move || simulate_dae(&compiled_dae, &sim_opts))
            .await
        {
            Ok(Ok(value)) => value,
            Ok(Err(error)) => {
                return Some(Self::simulation_error_value(format!(
                    "simulation failed: {error}",
                )));
            }
            Err(error) => {
                return Some(Self::simulation_error_value(format!(
                    "simulation worker failed: {error}",
                )));
            }
        };
        if let Some(response) = self.stale_simulation_response(request_token).await {
            return Some(response);
        }
        let sim_elapsed = sim_start.elapsed().as_secs_f64();
        let mut metrics = Self::simulation_metrics(
            &compiled,
            compile_elapsed,
            compile_before,
            compile_after,
            stats_delta,
        );
        metrics.sim_elapsed = sim_elapsed;
        metrics.point_count = sim.times.len();
        metrics.variable_count = sim.names.len();
        let payload = Self::build_simulation_payload(&sim, &settings, &opts, metrics);
        Some(Self::simulation_success_value(payload, metrics))
    }

    async fn ensure_completion_libraries(
        &self,
        source: &str,
        position: Position,
        current_document_path: &str,
    ) {
        let Some(mut context) =
            build_completion_library_context(self, source, position, current_document_path).await
        else {
            maybe_log_completion_debug(&self.client, "no library completion prefix detected").await;
            return;
        };

        let mut progress_messages = Vec::new();
        let mut load_errors = Vec::new();
        for lib_path in &context.library_paths {
            let path_key = canonical_path_key(lib_path);
            if !context.seen_library_paths.insert(path_key.clone()) {
                continue;
            }
            if context.already_loaded.contains(&path_key) {
                continue;
            }
            let inferred_roots = infer_library_roots(Path::new(lib_path)).unwrap_or_default();
            if let Some((root, provider)) =
                duplicate_root_provider(&inferred_roots, &context.claimed_roots)
            {
                progress_messages.push(format!(
                    "[rumoca] Skipping library {} (duplicate root '{}' already loaded from {})",
                    lib_path, root, provider
                ));
                continue;
            }
            if !should_eager_load_library_for_completion_prefix(
                lib_path,
                &context.completion_prefix,
            ) {
                maybe_log_completion_debug(
                    &self.client,
                    format!(
                        "skip eager load for {lib_path} and prefix {}",
                        context.completion_prefix
                    ),
                )
                .await;
                continue;
            }

            maybe_log_completion_debug(&self.client, format!("loading {lib_path}")).await;
            progress_messages.push(format!(
                "[rumoca] Loading library for import completion: {lib_path}"
            ));
            let source_set_id = library_source_set_id(lib_path);
            let loaded = match self
                .load_library_source_set_if_current(
                    lib_path,
                    &path_key,
                    &source_set_id,
                    Some(current_document_path),
                    context.library_state_epoch,
                    LibraryIndexingReason::CompletionImports,
                )
                .await
            {
                Ok(Some(loaded)) => loaded,
                Ok(None) => {
                    maybe_log_completion_debug(
                        &self.client,
                        format!("load for {lib_path} returned no-op"),
                    )
                    .await;
                    continue;
                }
                Err(err) => {
                    maybe_log_completion_debug(
                        &self.client,
                        format!("load for {lib_path} failed: {err}"),
                    )
                    .await;
                    load_errors.push(err);
                    continue;
                }
            };
            maybe_log_completion_debug(
                &self.client,
                format!(
                    "loaded {lib_path}: inserted={} indexed={} cache={:?}",
                    loaded.inserted_file_count, loaded.indexed_file_count, loaded.cache_status
                ),
            )
            .await;
            let (progress_message, _) = completion_load_progress(lib_path, &loaded);
            progress_messages.push(progress_message);
            claim_roots(&mut context.claimed_roots, inferred_roots, lib_path);
            context.library_state_epoch = self.current_library_state_epoch();
        }

        flush_completion_library_notifications(self, progress_messages, load_errors).await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for ModelicaLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let initialize_started = Instant::now();
        let parse_init_started = Instant::now();
        let paths = params
            .initialization_options
            .as_ref()
            .and_then(|value| value.get("modelicaPath"))
            .and_then(|value| value.as_array())
            .map(|items| {
                items
                    .iter()
                    .filter_map(|item| item.as_str())
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let parse_init_options_ms = parse_init_started.elapsed().as_millis() as u64;
        let initial_library_paths = paths.len();
        *self.initial_library_paths.write().await = paths;

        let workspace_root_started = Instant::now();
        let workspace_root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| folder.uri.to_file_path().ok())
            .or_else(|| {
                params
                    .root_uri
                    .as_ref()
                    .and_then(|uri| uri.to_file_path().ok())
            });
        let workspace_root_ms = workspace_root_started.elapsed().as_millis() as u64;
        *self.workspace_root.write().await = workspace_root;
        let reload_timing = self.reload_project_config_with_timing().await;
        let total_ms = initialize_started.elapsed().as_millis() as u64;
        let startup_timing_path = self.startup_timing_path.read().await.clone();
        write_startup_timing_summary(
            &StartupTimingSummary {
                initial_library_paths,
                library_paths_changed: reload_timing.library_paths_changed,
                parse_init_options_ms,
                workspace_root_ms,
                reload_project_config_ms: reload_timing.total_ms,
                project_discover_ms: reload_timing.project_discover_ms,
                resolve_library_paths_ms: reload_timing.resolve_library_paths_ms,
                reset_session_ms: reload_timing.reset_session_ms,
                durable_prewarm_ms: reload_timing.durable_prewarm_ms,
                durable_collect_files_ms: reload_timing.durable_collect_files_ms,
                durable_hash_inputs_ms: reload_timing.durable_hash_inputs_ms,
                durable_cache_lookup_ms: reload_timing.durable_cache_lookup_ms,
                durable_cache_deserialize_ms: reload_timing.durable_cache_deserialize_ms,
                durable_parse_files_ms: reload_timing.durable_parse_files_ms,
                durable_validate_layout_ms: reload_timing.durable_validate_layout_ms,
                durable_cache_write_ms: reload_timing.durable_cache_write_ms,
                durable_apply_ms: reload_timing.durable_apply_ms,
                workspace_symbol_prewarm_ms: reload_timing.workspace_symbol_prewarm_ms,
                namespace_prewarm_spawn_ms: reload_timing.namespace_prewarm_spawn_ms,
                total_ms,
            },
            startup_timing_path.as_deref(),
        );

        Ok(InitializeResult {
            capabilities: Self::server_capabilities(),
            server_info: Some(ServerInfo {
                name: "rumoca-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Rumoca LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let uri_path = session_document_uri_key(&uri);
        self.record_open_document_version(&uri_path, params.text_document.version)
            .await;
        if is_project_config_uri(&uri) {
            self.reload_project_config().await;
        }
        let text = params.text_document.text;
        let stats_before = session_cache_stats();
        {
            let mut session = self.session.write().await;
            let mut change = SessionChange::default();
            change.set_file_text(&uri_path, &text);
            session.apply_change(change);
        }
        self.clear_simulation_compile_cache().await;
        self.completion_mutation_epoch
            .fetch_add(1, Ordering::AcqRel);
        self.wait_for_namespace_prewarm_if_pending().await;
        self.publish_diagnostics(uri.clone(), &text, DiagnosticsTrigger::Live, stats_before)
            .await;
        self.prewarm_document_semantic_queries(&uri_path).await;
        if self.is_loaded_library_document(&uri_path).await {
            self.prewarm_namespace_cache_for_loaded_library_document(&uri_path)
                .await;
            self.prewarm_workspace_symbols_for_loaded_library_document(&uri_path)
                .await;
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let uri_path = session_document_uri_key(&uri);
        if !self
            .try_accept_document_version(&uri_path, params.text_document.version)
            .await
        {
            return;
        }
        if is_project_config_uri(&uri) {
            self.reload_project_config().await;
        }
        if let Some(change) = params.content_changes.into_iter().last() {
            self.wait_for_namespace_prewarm_for_loaded_library_document(&uri_path)
                .await;
            let stats_before = session_cache_stats();
            {
                let mut session = self.session.write().await;
                let mut session_change = SessionChange::default();
                session_change.set_file_text(&uri_path, &change.text);
                session.apply_change(session_change);
            }
            self.clear_simulation_compile_cache().await;
            self.publish_diagnostics(uri, &change.text, DiagnosticsTrigger::Live, stats_before)
                .await;
            self.completion_mutation_epoch
                .fetch_add(1, Ordering::AcqRel);
            self.spawn_background_namespace_prewarm_for_loaded_library_document(&uri_path)
                .await;
            self.wait_for_namespace_prewarm_for_loaded_library_document(&uri_path)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri_path = session_document_uri_key(&params.text_document.uri);
        self.document_versions.write().await.remove(&uri_path);
        self.selected_simulation_models
            .write()
            .await
            .remove(&uri_path);
        self.wait_for_namespace_prewarm_for_loaded_library_document(&uri_path)
            .await;
        let mut session = self.session.write().await;
        let mut change = SessionChange::default();
        change.remove_file(&uri_path);
        session.apply_change(change);
        drop(session);
        self.clear_simulation_compile_cache().await;
        self.completion_mutation_epoch
            .fetch_add(1, Ordering::AcqRel);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if is_project_config_uri(&uri) {
            self.reload_project_config().await;
        }
        if let Some(text) = params.text {
            self.publish_diagnostics(uri, &text, DiagnosticsTrigger::Save, session_cache_stats())
                .await;
        } else {
            let uri_path = session_document_uri_key(&uri);
            if let Some(doc) = self.document_snapshot(&uri_path).await {
                let text = doc.content.clone();
                self.publish_diagnostics(
                    uri,
                    &text,
                    DiagnosticsTrigger::Save,
                    session_cache_stats(),
                )
                .await;
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let request_started = Instant::now();
        let stats_before = session_cache_stats();
        let request_token = self.begin_analysis_request().await;
        let uri = &params.text_document_position_params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position_params.position;
        let Some((doc, snapshot)) = self.document_analysis_snapshot(&uri_path).await else {
            return Ok(None);
        };
        let source = doc.content.as_str();
        let ast = doc.parsed();
        let mut hover = None;
        let mut request_path = NavigationRequestPath::QueryOnly;
        let mut semantic_layer = "stale";
        if !self.analysis_request_is_stale(request_token).await {
            if let Some(info) =
                snapshot.local_component_info_query(&uri_path, pos.line, pos.character)
            {
                semantic_layer = "class_body_semantics";
                hover = Some(local_component_hover(&info));
            }
            if hover.is_none()
                && let Some(info) =
                    snapshot.navigation_class_target_query(&uri_path, pos.line, pos.character)
            {
                semantic_layer = "class_interface";
                hover = Some(class_target_hover(&info));
            }
            if hover.is_none()
                && let Some(word) = get_word_at_position(source, pos)
            {
                semantic_layer = "builtin_keyword";
                hover = handlers::hover::builtin_or_keyword_hover(&word);
            }
            if let (Some(ast), Some(word)) = (ast, get_word_at_position(source, pos))
                && is_hover_preview_candidate(ast, &word)
                && let Some(preview) = {
                    let mut session = self.session.write().await;
                    flattened_preview_for_model(&mut session, &word)
                }
            {
                request_path = NavigationRequestPath::FlatPreview;
                semantic_layer = "flat_model";
                hover = Some(append_markdown_hover(hover, &preview));
            }
        }
        let stats_after = session_cache_stats();
        let navigation_timing_path = self.navigation_timing_path.read().await.clone();
        let session_cache_delta = stats_after.delta_since(stats_before);
        let request_was_stale = self.analysis_request_is_stale(request_token).await;
        if request_was_stale {
            hover = None;
            request_path = NavigationRequestPath::QueryOnly;
            semantic_layer = "stale";
        }
        write_navigation_timing_summary(
            &NavigationTimingSummary {
                requested_edit_epoch: request_token.mutation_epoch,
                request_was_stale,
                uri: uri_path,
                request: "hover",
                request_path,
                semantic_layer,
                total_ms: request_started.elapsed().as_millis() as u64,
                snapshot_ms: None,
                snapshot_lock_ms: None,
                snapshot_build_ms: None,
                detail: None,
                query_ms: None,
                format_ms: None,
                built_resolved_tree: session_cache_delta.semantic_navigation_builds >= 1,
                had_resolved_cache_before: false,
                session_cache_delta,
            },
            navigation_timing_path.as_deref(),
        );
        Ok(hover)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let _interactive_lane = self.work_lanes.interactive.lock().await;
        let completion_started = Instant::now();
        let mut request_token = self.begin_analysis_request().await;
        let uri = &params.text_document_position.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position.position;
        let doc_snapshot = self.document_snapshot(&uri_path).await;
        let source = doc_snapshot
            .as_ref()
            .map(|doc| doc.content.clone())
            .unwrap_or_default();
        let has_library_prefix = extract_library_completion_prefix(&source, pos).is_some();
        if has_library_prefix {
            self.wait_for_namespace_prewarm_if_pending().await;
        }
        let stats_before = session_cache_stats();
        let ast = doc_snapshot.as_ref().and_then(|doc| doc.parsed());
        let preparation = self
            .prepare_completion(&source, pos, &uri_path, request_token.mutation_epoch)
            .await;

        let mut session_snapshot = None;
        let mut class_name_count_after_ensure = 0usize;
        if !preparation.request_was_stale {
            request_token = self.refresh_analysis_request_revision(request_token).await;
            let snapshot = if has_library_prefix {
                self.session_snapshot().await
            } else if let Some(snapshot) = self.document_lightweight_snapshot(&uri_path).await {
                snapshot
            } else {
                self.session_snapshot().await
            };
            class_name_count_after_ensure = Self::cached_completion_class_name_count(
                &snapshot,
                preparation.completion_prefix.as_deref(),
            );
            session_snapshot = Some(snapshot);
        }
        maybe_log_completion_debug(
            &self.client,
            format!(
                "cached class names after ensure={}",
                class_name_count_after_ensure
            ),
        )
        .await;
        let doc_source = doc_snapshot
            .as_ref()
            .map(|doc| doc.content.as_str())
            .unwrap_or("");
        let completion_handler_started = Instant::now();
        let mut completion_response = None;
        let mut semantic_layer = "stale".to_string();
        if !preparation.request_was_stale {
            let completion_result = handlers::handle_completion_with_snapshot_and_provenance(
                doc_source,
                ast,
                session_snapshot.as_ref(),
                Some(&uri_path),
                pos.line,
                pos.character,
            );
            semantic_layer = completion_result.semantic_layer.label().to_string();
            completion_response = Some(CompletionResponse::Array(completion_result.items));
        }
        let completion_handler_ms = completion_handler_started.elapsed().as_millis() as u64;
        let total_ms = completion_started.elapsed().as_millis() as u64;
        let stats_after = session_cache_stats();
        let session_cache_delta = stats_after.delta_since(stats_before);
        let request_was_stale =
            preparation.request_was_stale || self.analysis_request_is_stale(request_token).await;
        if request_was_stale {
            completion_response = None;
            semantic_layer = "stale".to_string();
        }
        let completion_timing_path = self.completion_timing_path.read().await.clone();
        let timing_summary = build_completion_timing_summary(
            preparation,
            CompletionTimingContext {
                request_edit_epoch: request_token.mutation_epoch,
                uri: uri_path,
                semantic_layer,
                completion_handler_ms,
                total_ms,
                class_name_count_after_ensure,
                session_cache_delta,
            },
        );
        write_completion_timing_summary(&timing_summary, completion_timing_path.as_deref());
        Ok(completion_response)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let request_token = self.begin_analysis_request().await;
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let snapshot = self.session_snapshot().await;
        if self.analysis_request_is_stale(request_token).await {
            return Ok(None);
        }
        if let Some(symbols) = snapshot.document_symbol_query(&uri_path) {
            if self.analysis_request_is_stale(request_token).await {
                return Ok(None);
            }
            return Ok(handlers::handle_document_symbols(symbols));
        }
        Ok(None)
    }
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if !self.completion_request_is_stale(self.completion_mutation_epoch())
            && let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = doc.parsed()
        {
            return Ok(handlers::handle_semantic_tokens(ast));
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let request_started = Instant::now();
        let stats_before = session_cache_stats();
        let request_token = self.begin_analysis_request().await;
        let uri = &params.text_document_position_params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position_params.position;
        let Some((_, snapshot)) = self.document_analysis_snapshot(&uri_path).await else {
            return Ok(None);
        };
        let mut response = None;
        let request_path = NavigationRequestPath::QueryOnly;
        let mut semantic_layer = "stale";
        if !self.analysis_request_is_stale(request_token).await {
            if let Some(info) =
                snapshot.local_component_info_query(&uri_path, pos.line, pos.character)
            {
                semantic_layer = "class_body_semantics";
                response = class_target_definition(&uri_path, &info.declaration_location, uri);
            }
            if response.is_none()
                && let Some(info) =
                    snapshot.navigation_class_target_query(&uri_path, pos.line, pos.character)
            {
                semantic_layer = "class_interface";
                response =
                    class_target_definition(&info.target_uri, &info.declaration_location, uri);
            }
        }
        let stats_after = session_cache_stats();
        let navigation_timing_path = self.navigation_timing_path.read().await.clone();
        let session_cache_delta = stats_after.delta_since(stats_before);
        let request_was_stale = self.analysis_request_is_stale(request_token).await;
        if request_was_stale {
            response = None;
            semantic_layer = "stale";
        }
        write_navigation_timing_summary(
            &NavigationTimingSummary {
                requested_edit_epoch: request_token.mutation_epoch,
                request_was_stale,
                uri: uri_path,
                request: "definition",
                request_path,
                semantic_layer,
                total_ms: request_started.elapsed().as_millis() as u64,
                snapshot_ms: None,
                snapshot_lock_ms: None,
                snapshot_build_ms: None,
                detail: None,
                query_ms: None,
                format_ms: None,
                built_resolved_tree: session_cache_delta.semantic_navigation_builds >= 1,
                had_resolved_cache_before: false,
                session_cache_delta,
            },
            navigation_timing_path.as_deref(),
        );
        Ok(response)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        navigation::references(self, params).await
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        navigation::prepare_rename(self, params).await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        navigation::rename(self, params).await
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let request_started = Instant::now();
        let request_token = self.begin_analysis_request().await;
        let stats_before = session_cache_stats();
        let lock_started = Instant::now();
        let session = self.session.read().await;
        let snapshot_lock_ms = lock_started.elapsed().as_millis() as u64;
        let build_started = Instant::now();
        let (snapshot, snapshot_timing) = session.workspace_symbol_snapshot_with_timing();
        let snapshot_build_ms = build_started.elapsed().as_millis() as u64;
        let mut timing = WorkspaceSymbolTimingBreakdown {
            snapshot_ms: snapshot_lock_ms.saturating_add(snapshot_build_ms),
            snapshot_lock_ms,
            snapshot_build_ms,
            snapshot_detail: Some(snapshot_timing.to_string()),
            query_ms: None,
            format_ms: None,
        };
        if self.analysis_request_is_stale(request_token).await {
            self.write_workspace_symbol_timing(
                request_token,
                request_started,
                timing,
                "stale",
                stats_before,
            )
            .await;
            return Ok(None);
        }
        let query_started = Instant::now();
        let symbols = snapshot.workspace_symbol_query(&params.query);
        timing.query_ms = Some(query_started.elapsed().as_millis() as u64);
        if self.analysis_request_is_stale(request_token).await {
            self.write_workspace_symbol_timing(
                request_token,
                request_started,
                timing,
                "stale",
                stats_before,
            )
            .await;
            return Ok(None);
        }
        let format_started = Instant::now();
        let symbols = handlers::handle_workspace_symbols(&symbols);
        timing.format_ms = Some(format_started.elapsed().as_millis() as u64);
        self.write_workspace_symbol_timing(
            request_token,
            request_started,
            timing,
            "workspace_symbol",
            stats_before,
        )
        .await;
        Ok(Some(symbols))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position_params.position;
        let doc = self.document_snapshot(&uri_path).await;
        let source = doc.as_ref().map(|d| d.content.as_str()).unwrap_or("");
        Ok(handlers::handle_signature_help(
            source,
            pos.line,
            pos.character,
        ))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = doc.parsed()
        {
            let ranges = handlers::handle_folding_ranges(ast, &doc.content);
            return Ok(Some(ranges));
        }
        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await {
            return Ok(handlers::handle_formatting(&doc.content));
        }
        Ok(None)
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let Some(doc_snapshot) = self.document_snapshot(&uri_path).await else {
            return Ok(None);
        };
        let source = doc_snapshot.content.clone();
        if self.source_requires_unloaded_libraries(&source).await {
            return Ok(None);
        }

        let doc = doc_snapshot;
        let Some(ast) = doc.parsed() else {
            return Ok(None);
        };
        Ok(Some(handlers::handle_code_lens(ast, uri)))
    }

    async fn code_lens_resolve(&self, mut params: CodeLens) -> Result<CodeLens> {
        let request_token = self.begin_analysis_request().await;
        let Some(data) = params.data.clone() else {
            return Ok(params);
        };
        let Ok(data) = serde_json::from_value::<CodeLensResolutionData>(data) else {
            return Ok(params);
        };
        let Ok(uri) = Url::parse(&data.uri) else {
            return Ok(params);
        };
        let uri_path = session_document_uri_key(&uri);
        let Some(doc_snapshot) = self.document_snapshot(&uri_path).await else {
            return Ok(params);
        };
        if self.analysis_request_is_stale(request_token).await
            || self
                .source_requires_unloaded_libraries(&doc_snapshot.content)
                .await
        {
            return Ok(params);
        }
        let _strict_guard = self.work_lanes.strict.lock().await;
        if self.analysis_request_is_stale(request_token).await {
            return Ok(params);
        }
        let mut session = self.session.write().await;
        let report = session.compile_model_strict_reachable_with_recovery(&data.model_name);
        params.command = Some(Command {
            title: code_lens_title_from_strict_report(report),
            command: String::new(),
            arguments: None,
        });
        Ok(params)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await {
            let actions = handlers::handle_code_actions(
                &params.context.diagnostics,
                &doc.content,
                &params.range,
                Some(uri),
            );
            let response: Vec<CodeActionOrCommand> = actions
                .into_iter()
                .map(CodeActionOrCommand::CodeAction)
                .collect();
            return Ok(Some(response));
        }
        Ok(None)
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        let mut args = params.arguments;
        let arg0 = if args.is_empty() {
            None
        } else {
            Some(args.remove(0))
        };

        let response = match params.command.as_str() {
            "rumoca.project.getSimulationConfig" => self.execute_get_simulation_config(arg0).await,
            "rumoca.project.setSimulationPreset" => self.execute_set_simulation_preset(arg0).await,
            "rumoca.project.resetSimulationPreset" => {
                self.execute_reset_simulation_preset(arg0).await
            }
            "rumoca.project.getVisualizationConfig" => {
                self.execute_get_visualization_config(arg0).await
            }
            "rumoca.project.setVisualizationConfig" => {
                self.execute_set_visualization_config(arg0).await
            }
            "rumoca.project.resyncSidecars" => self.execute_resync_sidecars(arg0).await,
            "rumoca.project.filesMoved" => self.execute_project_files_moved(arg0).await,
            "rumoca.project.simulate" => self.execute_simulate_model(arg0, None).await,
            "rumoca.project.getSimulationModels" => self.execute_get_simulation_models(arg0).await,
            "rumoca.project.setSelectedSimulationModel" => {
                self.execute_set_selected_simulation_model(arg0).await
            }
            "rumoca.project.startSimulation" => self.execute_start_simulation(arg0).await,
            "rumoca.project.prepareSimulationModels" => {
                self.execute_prepare_simulation_models(arg0).await
            }
            _ => None,
        };
        Ok(response)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = doc.parsed()
        {
            let hints = handlers::handle_inlay_hints(ast, &doc.content, &params.range);
            return Ok(Some(hints));
        }
        Ok(Some(Vec::new()))
    }

    async fn document_link(&self, params: DocumentLinkParams) -> Result<Option<Vec<DocumentLink>>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await {
            let links = handlers::handle_document_links(&doc.content, uri);
            return Ok(Some(links));
        }
        Ok(Some(Vec::new()))
    }
}
/// Run the LSP server on stdin/stdout.
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::new(ModelicaLanguageServer::new);
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
#[cfg(test)]
mod tests;
