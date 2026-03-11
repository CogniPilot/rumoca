//! LSP server implementation (native only, behind "server" feature).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use rumoca_session::compile::{
    Document, FailedPhase, PhaseResult, Session, SessionConfig, compile_phase_timing_stats,
};
use rumoca_session::libraries::{
    LibraryCacheStatus, PackageLayoutError, infer_library_roots, parse_library_with_cache,
    should_load_library_for_source,
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
use serde_json::{Value, json};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::handlers;
use crate::helpers::{get_text_before_cursor, get_word_at_position};
#[path = "server_support.rs"]
mod server_support;
use server_support::*;

/// Modelica Language Server.
pub struct ModelicaLanguageServer {
    client: Client,
    session: Arc<RwLock<Session>>,
    initial_library_paths: Arc<RwLock<Vec<String>>>,
    library_paths: Arc<RwLock<Vec<String>>>,
    loaded_libraries: Arc<RwLock<HashSet<String>>>,
    dirty_libraries: Arc<RwLock<HashSet<String>>>,
    library_document_overlays: Arc<RwLock<HashMap<String, Document>>>,
    library_load_diagnostics: Arc<RwLock<HashMap<String, Vec<Diagnostic>>>>,
    library_load_diagnostic_uris: Arc<RwLock<HashMap<String, HashSet<String>>>>,
    loading_libraries: Arc<RwLock<HashMap<String, u64>>>,
    library_state_epoch: Arc<AtomicU64>,
    indexing_notified_libraries: Arc<RwLock<HashSet<String>>>,
    workspace_root: Arc<RwLock<Option<PathBuf>>>,
    project_config: Arc<RwLock<Option<ProjectConfig>>>,
}

#[derive(Debug, Clone, Copy)]
struct SimulationMetrics {
    compile_elapsed: f64,
    sim_elapsed: f64,
    point_count: usize,
    variable_count: usize,
    instantiate_seconds: f64,
    typecheck_seconds: f64,
    flatten_seconds: f64,
    todae_seconds: f64,
}

#[derive(Debug, Clone)]
struct LibraryLoadOutcome {
    cache_status: LibraryCacheStatus,
    indexed_file_count: usize,
    inserted_file_count: usize,
    cache_key: String,
    cache_path: String,
}

fn library_load_diagnostics_for_package_layout_error(
    err: &PackageLayoutError,
) -> HashMap<String, Vec<Diagnostic>> {
    let mut by_uri = HashMap::new();
    let source_map = err.source_map();
    for file_name in source_map.source_ids().into_keys() {
        let diagnostics =
            handlers::common_diagnostics_for_file(err.diagnostics(), &file_name, source_map);
        if diagnostics.is_empty() {
            continue;
        }
        by_uri.insert(canonical_path_key(&file_name), diagnostics);
    }
    by_uri
}

fn library_load_error_message(lib_path: &str, err: &anyhow::Error) -> String {
    let Some(layout) = err.downcast_ref::<PackageLayoutError>() else {
        return format!("Failed to load library '{}': {}", lib_path, err);
    };
    if layout
        .diagnostics()
        .iter()
        .any(|diagnostic| !diagnostic.labels.is_empty())
    {
        return format!(
            "Failed to load library '{}': invalid Modelica package layout (see diagnostics)",
            lib_path
        );
    }
    format!("Failed to load library '{}': {}", lib_path, err)
}

impl ModelicaLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            session: Arc::new(RwLock::new(Session::new(SessionConfig::default()))),
            initial_library_paths: Arc::new(RwLock::new(Vec::new())),
            library_paths: Arc::new(RwLock::new(Vec::new())),
            loaded_libraries: Arc::new(RwLock::new(HashSet::new())),
            dirty_libraries: Arc::new(RwLock::new(HashSet::new())),
            library_document_overlays: Arc::new(RwLock::new(HashMap::new())),
            library_load_diagnostics: Arc::new(RwLock::new(HashMap::new())),
            library_load_diagnostic_uris: Arc::new(RwLock::new(HashMap::new())),
            loading_libraries: Arc::new(RwLock::new(HashMap::new())),
            library_state_epoch: Arc::new(AtomicU64::new(0)),
            indexing_notified_libraries: Arc::new(RwLock::new(HashSet::new())),
            workspace_root: Arc::new(RwLock::new(None)),
            project_config: Arc::new(RwLock::new(None)),
        }
    }

    async fn show_library_indexing_notice_once(&self, lib_path: &str) {
        let key = canonical_path_key(lib_path);
        let should_notify = {
            let mut notified = self.indexing_notified_libraries.write().await;
            notified.insert(key)
        };
        if !should_notify {
            return;
        }
        self.client
            .show_message(
                MessageType::INFO,
                format!(
                    "[rumoca] Indexing Modelica library '{}' (first load may take time; results are cached).",
                    lib_path
                ),
            )
            .await;
    }

    async fn recompute_library_paths(&self) {
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
        *self.library_paths.write().await = merged;
    }

    async fn reset_session_and_loaded_libraries(&self) {
        self.library_state_epoch.fetch_add(1, Ordering::AcqRel);
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
        for (uri, content) in docs {
            rebuilt.update_document(&uri, &content);
        }
        *self.session.write().await = rebuilt;
        let mut loaded = self.loaded_libraries.write().await;
        let mut dirty = self.dirty_libraries.write().await;
        let mut loading = self.loading_libraries.write().await;
        loaded.clear();
        dirty.clear();
        loading.clear();
    }

    async fn mark_library_dirty_for_document(&self, document_path: &str) {
        let loaded = self.loaded_libraries.read().await.clone();
        let Some(library_root) = library_root_for_document(document_path, &loaded) else {
            return;
        };
        let inserted = {
            let mut dirty = self.dirty_libraries.write().await;
            dirty.insert(library_root.clone())
        };
        if inserted {
            self.client
                .log_message(
                    MessageType::INFO,
                    format!(
                        "[rumoca] Library file changed: {} (library {} will be rebuilt on next compile)",
                        document_path, library_root
                    ),
                )
                .await;
        }
    }

    async fn is_library_document_path(&self, document_path: &str) -> bool {
        let loaded = self.loaded_libraries.read().await.clone();
        if library_root_for_document(document_path, &loaded).is_some() {
            return true;
        }
        let document_key = canonical_path_key(document_path);
        let document_path = Path::new(&document_key);
        let configured_paths = self.library_paths.read().await.clone();
        configured_paths.into_iter().any(|path| {
            let root_key = canonical_path_key(&path);
            document_path.starts_with(Path::new(&root_key))
        })
    }

    async fn update_library_document_overlay(&self, uri_path: &str, content: &str) {
        let previous_parsed = self
            .library_document_overlays
            .read()
            .await
            .get(uri_path)
            .and_then(|doc| doc.parsed.clone());
        let (parsed, parse_error) = match parse_source_to_ast(content, uri_path) {
            Ok(parsed) => (Some(parsed), None),
            Err(err) => (previous_parsed, Some(err.to_string())),
        };
        let document = Document {
            uri: uri_path.to_string(),
            content: content.to_string(),
            parsed,
            partial: None,
            parse_errors: Vec::new(),
            parse_error,
        };
        self.library_document_overlays
            .write()
            .await
            .insert(uri_path.to_string(), document);
    }

    async fn document_snapshot(&self, uri_path: &str) -> Option<Document> {
        if let Some(doc) = self
            .library_document_overlays
            .read()
            .await
            .get(uri_path)
            .cloned()
        {
            return Some(doc);
        }
        self.session.read().await.get_document(uri_path).cloned()
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
        let dirty_roots: Vec<String> = self.dirty_libraries.read().await.iter().cloned().collect();
        if dirty_roots.is_empty() {
            return Ok(());
        }

        for library_root in &dirty_roots {
            let parsed = match parse_library_with_cache(Path::new(library_root)) {
                Ok(parsed) => parsed,
                Err(err) => {
                    self.maybe_publish_package_layout_diagnostics(library_root, &err)
                        .await;
                    return Err(library_load_error_message(library_root, &err));
                }
            };
            self.replace_library_load_diagnostics(library_root, HashMap::new())
                .await;
            let source_set_id = library_source_set_id(library_root);
            let inserted_count = {
                let mut session = self.session.write().await;
                session.replace_parsed_source_set(
                    &source_set_id,
                    parsed.documents,
                    Some(current_document_path),
                )
            };
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

        let mut dirty = self.dirty_libraries.write().await;
        for root in dirty_roots {
            dirty.remove(&root);
        }
        Ok(())
    }

    fn current_library_state_epoch(&self) -> u64 {
        self.library_state_epoch.load(Ordering::Acquire)
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
        path_key: &str,
        current_document_path: &str,
        documents: Vec<(String, ast::StoredDefinition)>,
        expected_epoch: u64,
    ) -> Option<usize> {
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
        let inserted = session.replace_parsed_source_set(
            source_set_id,
            documents,
            Some(current_document_path),
        );
        loaded.insert(path_key.to_string());
        if should_clear_library_load(&loading, path_key, expected_epoch) {
            loading.remove(path_key);
        }
        Some(inserted)
    }

    async fn load_library_source_set_if_current(
        &self,
        lib_path: &str,
        path_key: &str,
        source_set_id: &str,
        current_document_path: &str,
        expected_epoch: u64,
    ) -> std::result::Result<Option<LibraryLoadOutcome>, String> {
        if !self.reserve_library_load(path_key, expected_epoch).await {
            return Ok(None);
        }

        let parsed = match parse_library_with_cache(Path::new(lib_path)) {
            Ok(parsed) => parsed,
            Err(err) => {
                self.cancel_library_load(path_key, expected_epoch).await;
                self.maybe_publish_package_layout_diagnostics(lib_path, &err)
                    .await;
                return Err(library_load_error_message(lib_path, &err));
            }
        };
        self.replace_library_load_diagnostics(lib_path, HashMap::new())
            .await;

        let cache_status = parsed.cache_status;
        let cache_key = parsed.cache_key.clone();
        let cache_path = parsed
            .cache_file
            .as_deref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "<none>".to_string());
        let indexed_file_count = parsed.file_count;
        let Some(inserted_file_count) = self
            .apply_parsed_library_if_current(
                source_set_id,
                path_key,
                current_document_path,
                parsed.documents,
                expected_epoch,
            )
            .await
        else {
            return Ok(None);
        };

        Ok(Some(LibraryLoadOutcome {
            cache_status,
            indexed_file_count,
            inserted_file_count,
            cache_key,
            cache_path,
        }))
    }

    async fn reload_project_config(&self) {
        let workspace_root = self.workspace_root.read().await.clone();
        let Some(workspace_root) = workspace_root else {
            *self.project_config.write().await = None;
            self.recompute_library_paths().await;
            return;
        };

        match ProjectConfig::discover(&workspace_root) {
            Ok(config) => {
                if let Some(cfg) = &config {
                    self.log_project_diagnostics(&cfg.diagnostics).await;
                }
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
        self.recompute_library_paths().await;
        self.reset_session_and_loaded_libraries().await;
    }

    async fn log_project_diagnostics(&self, diagnostics: &[String]) {
        for diagnostic in diagnostics {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("[rumoca] project config: {diagnostic}"),
                )
                .await;
        }
    }

    async fn execute_get_simulation_config(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let workspace_root_default = self.workspace_root.read().await.clone();
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)
            .or(workspace_root_default)?;
        let model = obj.get("model").and_then(Value::as_str)?.to_string();
        let fallback = parse_fallback_simulation(obj.get("fallback"))?;

        let snapshot =
            load_simulation_snapshot_for_model(&workspace_root, &model, &fallback).ok()?;
        Some(json!({
            "preset": snapshot
                .preset
                .as_ref()
                .map(simulation_preset_to_json),
            "defaults": simulation_settings_to_json(&snapshot.defaults),
            "effective": simulation_settings_to_json(&snapshot.effective),
            "diagnostics": snapshot.diagnostics,
        }))
    }

    async fn execute_set_simulation_preset(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)?;
        let model = obj.get("model").and_then(Value::as_str)?.to_string();
        let preset = simulation_override_from_json(obj.get("preset")?)?;

        write_model_simulation_preset(&workspace_root, &model, preset).ok()?;

        if self.workspace_root.read().await.as_ref() == Some(&workspace_root) {
            self.reload_project_config().await;
        }
        Some(json!({ "ok": true }))
    }

    async fn execute_reset_simulation_preset(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)?;
        let model = obj.get("model").and_then(Value::as_str)?.to_string();

        clear_model_simulation_preset(&workspace_root, &model).ok()?;

        if self.workspace_root.read().await.as_ref() == Some(&workspace_root) {
            self.reload_project_config().await;
        }
        Some(json!({ "ok": true }))
    }

    async fn execute_get_visualization_config(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let workspace_root_default = self.workspace_root.read().await.clone();
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)
            .or(workspace_root_default)?;
        let model = obj.get("model").and_then(Value::as_str)?.to_string();
        let views = load_plot_views_for_model(&workspace_root, &model).ok()?;
        Some(json!({ "views": views }))
    }

    async fn execute_set_visualization_config(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)?;
        let model = obj.get("model").and_then(Value::as_str)?.to_string();
        let views = parse_views_payload(obj.get("views")?)?;
        write_plot_views_for_model(&workspace_root, &model, views).ok()?;
        if self.workspace_root.read().await.as_ref() == Some(&workspace_root) {
            self.reload_project_config().await;
        }
        Some(json!({ "ok": true }))
    }

    async fn execute_resync_sidecars(&self, params: Option<Value>) -> Option<Value> {
        let workspace_root_default = self.workspace_root.read().await.clone();
        let obj = params
            .and_then(|v| v.as_object().cloned())
            .unwrap_or_default();
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)
            .or(workspace_root_default)?;
        let dry_run = obj.get("dryRun").and_then(Value::as_bool).unwrap_or(false);
        let prune_orphans = obj
            .get("pruneOrphans")
            .and_then(Value::as_bool)
            .unwrap_or(false);

        let known_models = {
            let session = self.session.read().await;
            collect_workspace_known_models_from_session(&session, &workspace_root)
        };
        let report = resync_model_sidecars_with_move_hints(
            &workspace_root,
            &known_models,
            &[],
            dry_run,
            prune_orphans,
        )
        .ok()?;
        if self.workspace_root.read().await.as_ref() == Some(&workspace_root) {
            self.reload_project_config().await;
        }
        Some(json!({
            "ok": true,
            "report": report,
        }))
    }

    async fn execute_project_files_moved(&self, params: Option<Value>) -> Option<Value> {
        let workspace_root_default = self.workspace_root.read().await.clone();
        let obj = params
            .and_then(|v| v.as_object().cloned())
            .unwrap_or_default();
        let workspace_root = obj
            .get("workspaceRoot")
            .and_then(Value::as_str)
            .map(PathBuf::from)
            .or(workspace_root_default)?;
        let moved_files = parse_file_move_hints(obj.get("files"));
        if moved_files.is_empty() {
            return Some(json!({ "ok": true }));
        }

        let known_models = {
            let session = self.session.read().await;
            collect_workspace_known_models_from_session(&session, &workspace_root)
        };
        let report = resync_model_sidecars_with_move_hints(
            &workspace_root,
            &known_models,
            &moved_files,
            false,
            false,
        )
        .ok()?;
        if self.workspace_root.read().await.as_ref() == Some(&workspace_root) {
            self.reload_project_config().await;
        }
        Some(json!({
            "ok": true,
            "report": report,
        }))
    }

    fn simulation_error_value(error: impl Into<String>) -> Value {
        json!({ "ok": false, "error": error.into() })
    }

    async fn open_document_source_for_uri(&self, uri: &Url) -> std::result::Result<String, String> {
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await {
            return Ok(doc.content);
        }
        let session = self.session.read().await;
        session
            .get_document(&uri_path)
            .map(|doc| doc.content.clone())
            .ok_or_else(|| format!("document not open in LSP session: {}", uri_path))
    }

    async fn compile_model_for_simulation(
        &self,
        model: &str,
        focus_document_path: &str,
    ) -> std::result::Result<Box<rumoca_session::compile::CompilationResult>, String> {
        self.rebuild_dirty_libraries_before_compile(focus_document_path)
            .await?;
        let focus_key = canonical_path_key(focus_document_path);
        let loaded_libraries = self.loaded_libraries.read().await.clone();
        let mut isolated_session = Session::new(SessionConfig::default());
        let local_compile_unit_sources;
        let local_doc_keys;
        let parsed_docs_by_key;
        {
            let session = self.session.read().await;
            local_compile_unit_sources =
                collect_local_compile_unit_sources(&session, focus_document_path)?;
            local_doc_keys = local_compile_unit_sources
                .iter()
                .map(|(uri, _)| canonical_path_key(uri))
                .collect::<std::collections::BTreeSet<_>>();
            parsed_docs_by_key = collect_isolated_library_parsed_docs(
                &session,
                focus_document_path,
                &focus_key,
                &loaded_libraries,
                &local_doc_keys,
            )?;
        }
        isolated_session.add_parsed_batch(parsed_docs_by_key.into_values().collect());
        for (uri, source) in local_compile_unit_sources {
            let _ = isolated_session.update_document(&uri, &source);
        }
        if let Some(overlay_doc) = self.document_snapshot(focus_document_path).await {
            let needs_refresh = isolated_session
                .get_document(&overlay_doc.uri)
                .is_none_or(|doc| doc.content != overlay_doc.content);
            if needs_refresh {
                let _ = isolated_session.update_document(&overlay_doc.uri, &overlay_doc.content);
            }
        }

        let mut report = isolated_session.compile_model_strict_reachable_with_recovery(model);
        if !report.failures.is_empty() {
            return Err(report.failure_summary(8));
        }
        match report.requested_result.take() {
            Some(PhaseResult::Success(result)) => Ok(result),
            Some(PhaseResult::NeedsInner { missing_inners }) => Err(format!(
                "missing inner declarations: {}",
                missing_inners.join(", ")
            )),
            Some(PhaseResult::Failed { phase, error, .. }) => {
                let phase_name = match phase {
                    FailedPhase::Instantiate => "instantiate",
                    FailedPhase::Typecheck => "typecheck",
                    FailedPhase::Flatten => "flatten",
                    FailedPhase::ToDae => "todae",
                };
                Err(format!("{phase_name} failed: {error}"))
            }
            None => Err(report.failure_summary(8)),
        }
    }

    fn simulation_options_from_settings(
        settings: &SimulationRequestSettings,
        compiled: &rumoca_session::compile::CompilationResult,
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
                    "instantiate": metrics.instantiate_seconds,
                    "typecheck": metrics.typecheck_seconds,
                    "flatten": metrics.flatten_seconds,
                    "todae": metrics.todae_seconds,
                },
            }
        })
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

    async fn execute_simulate_model(&self, params: Option<Value>) -> Option<Value> {
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

        if settings.library_paths.is_empty() {
            self.ensure_source_libraries_loaded(&source, &uri_path)
                .await;
        } else {
            self.ensure_source_libraries_loaded_with_paths(
                &source,
                &uri_path,
                &settings.library_paths,
            )
            .await;
        }

        let compile_start = std::time::Instant::now();
        let compile_before = compile_phase_timing_stats();
        let compiled = match self.compile_model_for_simulation(&model, &uri_path).await {
            Ok(result) => result,
            Err(error) => {
                return Some(Self::simulation_error_value(format!(
                    "compilation failed: {error}",
                )));
            }
        };
        let compile_after = compile_phase_timing_stats();
        let compile_elapsed = compile_start.elapsed().as_secs_f64();
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

        let opts = Self::simulation_options_from_settings(&settings, &compiled);
        let sim_start = std::time::Instant::now();
        let sim = match simulate_dae(&compiled.dae, &opts) {
            Ok(value) => value,
            Err(error) => {
                return Some(Self::simulation_error_value(format!(
                    "simulation failed: {error}",
                )));
            }
        };
        let sim_elapsed = sim_start.elapsed().as_secs_f64();
        let metrics = SimulationMetrics {
            compile_elapsed,
            sim_elapsed,
            point_count: sim.times.len(),
            variable_count: sim.names.len(),
            instantiate_seconds,
            typecheck_seconds,
            flatten_seconds,
            todae_seconds,
        };
        let payload = Self::build_simulation_payload(&sim, &settings, &opts, metrics);
        Some(Self::simulation_success_value(payload, metrics))
    }

    async fn ensure_source_libraries_loaded(&self, text: &str, current_document_path: &str) {
        let library_paths = self.library_paths.read().await.clone();
        self.ensure_source_libraries_loaded_with_paths(text, current_document_path, &library_paths)
            .await;
    }

    async fn ensure_source_libraries_loaded_with_paths(
        &self,
        text: &str,
        current_document_path: &str,
        library_paths: &[String],
    ) {
        let library_state_epoch = self.current_library_state_epoch();
        let already_loaded = self.loaded_libraries.read().await.clone();
        let mut seen_library_paths = HashSet::new();
        let mut claimed_roots = HashMap::new();
        for loaded_path in &already_loaded {
            seen_library_paths.insert(canonical_path_key(loaded_path));
            for root in infer_library_roots(Path::new(loaded_path)).unwrap_or_default() {
                claimed_roots
                    .entry(root)
                    .or_insert_with(|| loaded_path.clone());
            }
        }

        let mut progress_messages = Vec::new();
        let mut load_errors = Vec::new();
        let mut cache_miss_notices = Vec::new();

        for lib_path in library_paths {
            let path_key = canonical_path_key(lib_path);
            if !seen_library_paths.insert(path_key.clone()) {
                continue;
            }
            if already_loaded.contains(&path_key) {
                continue;
            }
            let inferred_roots = infer_library_roots(Path::new(lib_path)).unwrap_or_default();
            if let Some((root, provider)) = duplicate_root_provider(&inferred_roots, &claimed_roots)
            {
                progress_messages.push(format!(
                    "[rumoca] Skipping library {} (duplicate root '{}' already loaded from {})",
                    lib_path, root, provider
                ));
                continue;
            }
            let should_load =
                should_load_library_for_source(text, Path::new(lib_path)).unwrap_or(true);
            if !should_load {
                continue;
            }
            progress_messages.push(format!("[rumoca] Loading library: {lib_path}"));
            let source_set_id = library_source_set_id(lib_path);
            let loaded = match self
                .load_library_source_set_if_current(
                    lib_path,
                    &path_key,
                    &source_set_id,
                    current_document_path,
                    library_state_epoch,
                )
                .await
            {
                Ok(Some(loaded)) => loaded,
                Ok(None) => continue,
                Err(err) => {
                    load_errors.push(err);
                    continue;
                }
            };
            let status = match loaded.cache_status {
                LibraryCacheStatus::Hit => "cache hit",
                LibraryCacheStatus::Miss => "cache miss",
                LibraryCacheStatus::Disabled => "cache disabled",
            };
            if loaded.cache_status == LibraryCacheStatus::Miss {
                cache_miss_notices.push(lib_path.clone());
            }
            progress_messages.push(format!(
                "[rumoca] Library {} ({}) — {} files, {} inserted [key={}, cache={}]",
                lib_path,
                status,
                loaded.indexed_file_count,
                loaded.inserted_file_count,
                loaded.cache_key,
                loaded.cache_path
            ));
            claim_roots(&mut claimed_roots, inferred_roots, lib_path);
        }

        for lib_path in cache_miss_notices {
            self.show_library_indexing_notice_once(&lib_path).await;
        }
        for message in progress_messages {
            self.client.log_message(MessageType::INFO, message).await;
        }
        for err in load_errors {
            self.client.log_message(MessageType::WARNING, err).await;
        }
    }

    /// Publish diagnostics for a document.
    async fn publish_diagnostics(&self, uri: Url, text: &str) {
        let file_name = session_document_uri_key(&uri);
        let is_library_overlay = self
            .library_document_overlays
            .read()
            .await
            .contains_key(&file_name);
        if is_library_overlay {
            let mut diagnostics = handlers::compute_diagnostics(text, &file_name, None);
            diagnostics.extend(self.stored_library_load_diagnostics(&file_name).await);
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
            return;
        }
        self.ensure_source_libraries_loaded(text, &file_name).await;
        let mut session = self.session.write().await;
        let mut diagnostics = handlers::compute_diagnostics(text, &file_name, Some(&mut session));
        drop(session);
        diagnostics.extend(self.stored_library_load_diagnostics(&file_name).await);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn ensure_completion_libraries(
        &self,
        source: &str,
        position: Position,
        current_document_path: &str,
    ) {
        let import_prefix = extract_import_completion_prefix(source, position);
        let Some(import_prefix) = import_prefix else {
            return;
        };

        let library_state_epoch = self.current_library_state_epoch();
        let library_paths = self.library_paths.read().await.clone();
        let already_loaded = self.loaded_libraries.read().await.clone();
        let mut seen_library_paths = HashSet::new();
        let mut claimed_roots = HashMap::new();
        for loaded_path in &already_loaded {
            seen_library_paths.insert(canonical_path_key(loaded_path));
            for root in infer_library_roots(Path::new(loaded_path)).unwrap_or_default() {
                claimed_roots
                    .entry(root)
                    .or_insert_with(|| loaded_path.clone());
            }
        }

        let mut progress_messages = Vec::new();
        let mut load_errors = Vec::new();
        let mut cache_miss_notices = Vec::new();

        for lib_path in &library_paths {
            let path_key = canonical_path_key(lib_path);
            if !seen_library_paths.insert(path_key.clone()) {
                continue;
            }
            if already_loaded.contains(&path_key) {
                continue;
            }
            let inferred_roots = infer_library_roots(Path::new(lib_path)).unwrap_or_default();
            if let Some((root, provider)) = duplicate_root_provider(&inferred_roots, &claimed_roots)
            {
                progress_messages.push(format!(
                    "[rumoca] Skipping library {} (duplicate root '{}' already loaded from {})",
                    lib_path, root, provider
                ));
                continue;
            }
            if !should_eager_load_library_for_import_prefix(lib_path, &import_prefix) {
                continue;
            }

            progress_messages.push(format!(
                "[rumoca] Loading library for import completion: {lib_path}"
            ));
            let source_set_id = library_source_set_id(lib_path);
            let loaded = match self
                .load_library_source_set_if_current(
                    lib_path,
                    &path_key,
                    &source_set_id,
                    current_document_path,
                    library_state_epoch,
                )
                .await
            {
                Ok(Some(loaded)) => loaded,
                Ok(None) => continue,
                Err(err) => {
                    load_errors.push(err);
                    continue;
                }
            };
            let status = match loaded.cache_status {
                LibraryCacheStatus::Hit => "cache hit",
                LibraryCacheStatus::Miss => "cache miss",
                LibraryCacheStatus::Disabled => "cache disabled",
            };
            if loaded.cache_status == LibraryCacheStatus::Miss {
                cache_miss_notices.push(lib_path.clone());
            }
            progress_messages.push(format!(
                "[rumoca] Library {} ({}) — {} files, {} inserted [key={}, cache={}]",
                lib_path,
                status,
                loaded.indexed_file_count,
                loaded.inserted_file_count,
                loaded.cache_key,
                loaded.cache_path
            ));
            claim_roots(&mut claimed_roots, inferred_roots, lib_path);
        }

        for lib_path in cache_miss_notices {
            self.show_library_indexing_notice_once(&lib_path).await;
        }
        for message in progress_messages {
            self.client.log_message(MessageType::INFO, message).await;
        }
        for err in load_errors {
            self.client.log_message(MessageType::WARNING, err).await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for ModelicaLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
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
        *self.initial_library_paths.write().await = paths;

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
        *self.workspace_root.write().await = workspace_root;
        self.reload_project_config().await;

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into()]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: handlers::get_semantic_token_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
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
                    resolve_provider: Some(false),
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
                    ],
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                ..Default::default()
            },
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
        if is_project_config_uri(&uri) {
            self.reload_project_config().await;
        }
        let text = params.text_document.text;
        if self.is_library_document_path(&uri_path).await {
            self.update_library_document_overlay(&uri_path, &text).await;
            self.publish_diagnostics(uri, &text).await;
            return;
        }
        {
            let mut session = self.session.write().await;
            session.update_document(&uri_path, &text);
        }
        self.publish_diagnostics(uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let uri_path = session_document_uri_key(&uri);
        if is_project_config_uri(&uri) {
            self.reload_project_config().await;
        }
        if let Some(change) = params.content_changes.into_iter().last() {
            if self.is_library_document_path(&uri_path).await {
                self.update_library_document_overlay(&uri_path, &change.text)
                    .await;
                self.mark_library_dirty_for_document(&uri_path).await;
                self.publish_diagnostics(uri, &change.text).await;
                return;
            }
            {
                let mut session = self.session.write().await;
                session.update_document(&uri_path, &change.text);
            }
            self.mark_library_dirty_for_document(&uri_path).await;
            self.publish_diagnostics(uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri_path = session_document_uri_key(&params.text_document.uri);
        if self
            .library_document_overlays
            .write()
            .await
            .remove(&uri_path)
            .is_some()
        {
            return;
        }
        let mut session = self.session.write().await;
        session.remove_document(&uri_path);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if is_project_config_uri(&uri) {
            self.reload_project_config().await;
        }
        if let Some(text) = params.text {
            self.publish_diagnostics(uri, &text).await;
        } else {
            let uri_path = session_document_uri_key(&uri);
            if let Some(doc) = self.document_snapshot(&uri_path).await {
                let text = doc.content.clone();
                self.publish_diagnostics(uri, &text).await;
            }
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position_params.position;
        let Some(doc) = self.document_snapshot(&uri_path).await else {
            return Ok(None);
        };
        let mut session = self.session.write().await;
        let source = doc.content.as_str();
        let ast = doc.parsed.as_ref();
        let mut hover = handlers::handle_hover(source, ast, pos.line, pos.character);

        let hovered_word = get_word_at_position(source, pos);
        if let (Some(ast), Some(word)) = (ast, hovered_word)
            && is_hover_preview_candidate(ast, &word)
            && let Some(preview) = flattened_preview_for_model(&mut session, &word)
        {
            hover = Some(append_markdown_hover(hover, &preview));
        }

        Ok(hover)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position.position;
        let doc_snapshot = self.document_snapshot(&uri_path).await;
        let source = doc_snapshot
            .as_ref()
            .map(|doc| doc.content.clone())
            .unwrap_or_default();
        // Ensure normal source-triggered library loading has happened before member completion.
        // This keeps `pid.`/`limiter.` completions reliable even if diagnostics hasn't run yet.
        self.ensure_source_libraries_loaded(&source, &uri_path)
            .await;
        self.ensure_completion_libraries(&source, pos, &uri_path)
            .await;
        let import_prefix = extract_import_completion_prefix(&source, pos);

        let needs_resolved =
            completion_context_needs_resolved_session(&source, pos, import_prefix.as_deref());
        if needs_resolved {
            let has_resolved = {
                let session = self.session.read().await;
                session.has_resolved_cached()
            };
            if !has_resolved {
                let mut session = self.session.write().await;
                let _ = session.all_class_names();
            }
        }

        let session = self.session.read().await;
        let doc_source = doc_snapshot
            .as_ref()
            .map(|doc| doc.content.as_str())
            .unwrap_or("");
        let ast = doc_snapshot.as_ref().and_then(|doc| doc.parsed.as_ref());
        let items =
            handlers::handle_completion(doc_source, ast, Some(&*session), pos.line, pos.character);
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = &doc.parsed
        {
            return Ok(handlers::handle_document_symbols(ast));
        }
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = &doc.parsed
        {
            return Ok(handlers::handle_semantic_tokens(ast));
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position_params.position;
        let Some(doc) = self.document_snapshot(&uri_path).await else {
            return Ok(None);
        };
        let mut session = self.session.write().await;
        let Some(ast) = doc.parsed.as_ref() else {
            return Ok(None);
        };
        let resolved = session.resolved().ok();
        let tree = resolved.as_ref().map(|resolved| &resolved.0);
        let source = &doc.content;
        return Ok(handlers::handle_goto_definition(
            ast,
            tree,
            source,
            uri,
            pos.line,
            pos.character,
        ));
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position.position;
        let include_decl = params.context.include_declaration;
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = &doc.parsed
        {
            let source = &doc.content;
            return Ok(handlers::handle_references(
                ast,
                source,
                uri,
                pos.line,
                pos.character,
                include_decl,
            ));
        }
        Ok(None)
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.position;
        let doc = self.document_snapshot(&uri_path).await;
        let source = doc.as_ref().map(|d| d.content.as_str()).unwrap_or("");
        Ok(handlers::handle_prepare_rename(
            source,
            pos.line,
            pos.character,
        ))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        let pos = params.text_document_position.position;
        let new_name = &params.new_name;
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = &doc.parsed
        {
            let source = &doc.content;
            return Ok(handlers::handle_rename(
                ast,
                source,
                uri,
                pos.line,
                pos.character,
                new_name,
            ));
        }
        Ok(None)
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let session = self.session.read().await;
        let uris = session.document_uris();
        let mut doc_symbols = Vec::new();
        for uri in &uris {
            if let Some(doc) = session.get_document(uri)
                && let Some(ast) = &doc.parsed
            {
                doc_symbols.push(handlers::DocSymbols { uri, ast });
            }
        }
        let symbols = handlers::handle_workspace_symbols(&doc_symbols, &params.query);
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
            && let Some(ast) = &doc.parsed
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
        self.ensure_source_libraries_loaded(&source, &uri_path)
            .await;

        let mut session = self.session.write().await;
        let doc = doc_snapshot;
        let Some(ast) = doc.parsed.as_ref() else {
            return Ok(None);
        };

        let model_names = handlers::workspace_symbols::collect_model_names(ast);
        let mut lenses = Vec::with_capacity(model_names.len());
        for (model_name, range) in model_names {
            let title = match session.compile_model_phases(&model_name) {
                Ok(PhaseResult::Success(result)) => {
                    let detail = dae_balance_detail(&result.dae);
                    let unknowns =
                        detail.state_unknowns + detail.alg_unknowns + detail.output_unknowns;
                    let balance = dae_balance(&result.dae);
                    let equations = (unknowns as i64 + balance).max(0) as usize;
                    if balance == 0 {
                        format!("Balanced ({unknowns} unknowns, {equations} eqs)")
                    } else {
                        format!("Unbalanced ({unknowns} unknowns, {equations} eqs, Δ={balance})")
                    }
                }
                Ok(PhaseResult::NeedsInner { missing_inners }) => {
                    format!("Needs inner ({})", missing_inners.join(", "))
                }
                Ok(PhaseResult::Failed { phase, .. }) => {
                    format!("Compile failed ({phase:?})")
                }
                Err(err) => format!("Compile error ({err})"),
            };
            lenses.push(CodeLens {
                range,
                command: Some(Command {
                    title,
                    command: String::new(),
                    arguments: None,
                }),
                data: None,
            });
        }
        Ok(Some(lenses))
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
            "rumoca.project.simulate" => self.execute_simulate_model(arg0).await,
            _ => None,
        };
        Ok(response)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await
            && let Some(ast) = &doc.parsed
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

fn is_hover_preview_candidate(ast: &ast::StoredDefinition, word: &str) -> bool {
    ast.classes.get(word).is_some_and(|class| {
        matches!(
            class.class_type,
            ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class
        )
    })
}

fn flattened_preview_for_model(session: &mut Session, model_name: &str) -> Option<String> {
    let Ok(PhaseResult::Success(result)) = session.compile_model_phases(model_name) else {
        return None;
    };

    let mut lines = Vec::new();
    lines.push(format!(
        "model={model_name} | f_x={} | f_z={} | f_m={} | m={} | balance={}",
        result.dae.f_x.len(),
        result.dae.f_z.len(),
        result.dae.f_m.len(),
        result.dae.discrete_valued.len(),
        dae_balance(&result.dae)
    ));
    lines.push(format!("f_x ({}):", result.dae.f_x.len()));
    for (idx, eq) in result.dae.f_x.iter().take(6).enumerate() {
        let lhs = eq
            .lhs
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "0".to_string());
        let rhs = truncate_debug(&eq.rhs, 140);
        lines.push(format!("  {idx}: {lhs} = {rhs}"));
    }
    if result.dae.f_x.len() > 6 {
        lines.push(format!(
            "  ... {} more f_x equations",
            result.dae.f_x.len() - 6
        ));
    }
    lines.push(format!("f_z ({}):", result.dae.f_z.len()));
    for (idx, eq) in result.dae.f_z.iter().take(4).enumerate() {
        let lhs = eq
            .lhs
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "0".to_string());
        let rhs = truncate_debug(&eq.rhs, 140);
        lines.push(format!("  {idx}: {lhs} = {rhs}"));
    }
    if result.dae.f_z.len() > 4 {
        lines.push(format!(
            "  ... {} more f_z equations",
            result.dae.f_z.len() - 4
        ));
    }
    lines.push(format!("f_m ({}):", result.dae.f_m.len()));
    for (idx, eq) in result.dae.f_m.iter().take(4).enumerate() {
        let lhs = eq
            .lhs
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "0".to_string());
        let rhs = truncate_debug(&eq.rhs, 140);
        lines.push(format!("  {idx}: {lhs} = {rhs}"));
    }
    if result.dae.f_m.len() > 4 {
        lines.push(format!(
            "  ... {} more f_m equations",
            result.dae.f_m.len() - 4
        ));
    }
    if !result.dae.discrete_valued.is_empty() {
        lines.push("m (discrete-valued variables):".to_string());
        for (idx, (name, var)) in result.dae.discrete_valued.iter().take(6).enumerate() {
            let start = var
                .start
                .as_ref()
                .map(|expr| truncate_debug(expr, 80))
                .unwrap_or_else(|| "<none>".to_string());
            lines.push(format!("{idx}: {name} start={start}"));
        }
        if result.dae.discrete_valued.len() > 6 {
            lines.push(format!(
                "... {} more discrete-valued variables",
                result.dae.discrete_valued.len() - 6
            ));
        }
    }

    Some(format!(
        "**Flattened DAE Preview**\n\n```text\n{}\n```",
        lines.join("\n")
    ))
}

fn truncate_debug<T: std::fmt::Debug>(value: &T, max_chars: usize) -> String {
    let rendered = format!("{value:?}");
    if rendered.chars().count() <= max_chars {
        return rendered;
    }
    let mut out = rendered.chars().take(max_chars).collect::<String>();
    out.push_str("...");
    out
}

fn append_markdown_hover(existing: Option<Hover>, extra_markdown: &str) -> Hover {
    let mut merged = String::new();
    if let Some(hover) = existing {
        match hover.contents {
            HoverContents::Markup(markup) => merged.push_str(&markup.value),
            HoverContents::Scalar(marked) => merged.push_str(&marked_string_to_markdown(marked)),
            HoverContents::Array(items) => {
                let joined = items
                    .into_iter()
                    .map(marked_string_to_markdown)
                    .collect::<Vec<_>>()
                    .join("\n\n");
                merged.push_str(&joined);
            }
        }
    }
    if !merged.is_empty() {
        merged.push_str("\n\n");
    }
    merged.push_str(extra_markdown);
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: merged,
        }),
        range: None,
    }
}

fn marked_string_to_markdown(marked: MarkedString) -> String {
    match marked {
        MarkedString::String(s) => s,
        MarkedString::LanguageString(ls) => format!("```{}\n{}\n```", ls.language, ls.value),
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
