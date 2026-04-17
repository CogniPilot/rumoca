//! LSP server implementation (native only, behind "server" feature).

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use rumoca_session::compile::{
    CompilePhaseTimingSnapshot, Document, ParsedSourceRootLoad, PhaseResult, Session,
    SessionCacheStatsSnapshot, SessionChange, SessionConfig, SessionSnapshot, SourceRootKind,
    compile_phase_timing_stats, session_cache_stats,
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
use rumoca_session::source_roots::{
    PackageLayoutError, SourceRootCacheStatus, SourceRootCacheTiming, canonical_path_key,
    classify_configured_source_root_kind, merge_source_root_paths, parse_source_root_with_cache,
    plan_source_root_loads, render_source_root_indexing_failed_message,
    render_source_root_indexing_finished_message, render_source_root_indexing_started_message,
    render_source_root_status_message, source_root_paths_changed, source_root_source_set_key,
};
use rumoca_sim::{SimOptions, SimSolverMode, dae_balance, dae_balance_detail};
use rumoca_sim_diffsol::simulate_dae;
use rumoca_sim_report::{
    SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
use serde::Deserialize;
use serde_json::{Value, json};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

#[cfg(test)]
use crate::completion_metrics::extract_import_completion_prefix;
use crate::completion_metrics::extract_namespace_completion_prefix;
use crate::handlers;
use crate::helpers::get_word_at_position;
mod completion;
mod diagnostics;
mod lanes;
mod navigation;
mod preview;
mod project_commands;
mod simulation_jobs;
mod source_root_runtime;
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
    initial_source_root_paths: Arc<RwLock<Vec<String>>>,
    source_root_paths: Arc<RwLock<Vec<String>>>,
    document_versions: Arc<RwLock<HashMap<String, i32>>>,
    completion_mutation_epoch: Arc<AtomicU64>,
    source_root_load_diagnostics: Arc<RwLock<HashMap<String, Vec<Diagnostic>>>>,
    source_root_load_diagnostic_uris: Arc<RwLock<HashMap<String, HashSet<String>>>>,
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
    source_root_read_prewarm_finished: Arc<tokio::sync::Notify>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagnosticsTrigger {
    Live,
    Save,
}

impl ModelicaLanguageServer {
    fn default_session_config() -> SessionConfig {
        SessionConfig { parallel: true }
    }

    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            session: Arc::new(RwLock::new(Session::new(Self::default_session_config()))),
            work_lanes: Arc::new(ServerWorkLanes::default()),
            initial_source_root_paths: Arc::new(RwLock::new(Vec::new())),
            source_root_paths: Arc::new(RwLock::new(Vec::new())),
            document_versions: Arc::new(RwLock::new(HashMap::new())),
            completion_mutation_epoch: Arc::new(AtomicU64::new(0)),
            source_root_load_diagnostics: Arc::new(RwLock::new(HashMap::new())),
            source_root_load_diagnostic_uris: Arc::new(RwLock::new(HashMap::new())),
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
            source_root_read_prewarm_finished: Arc::new(tokio::sync::Notify::new()),
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
                    "rumoca.workspace.getBuiltinTemplates".to_string(),
                    "rumoca.workspace.renderTemplate".to_string(),
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

    async fn finish_source_root_read_prewarm(&self, session_revision: u64) {
        self.session
            .write()
            .await
            .finish_source_root_read_prewarm(session_revision);
        self.source_root_read_prewarm_finished.notify_waiters();
    }

    async fn wait_for_source_root_read_prewarm_if_pending(&self) {
        loop {
            let notified = self.source_root_read_prewarm_finished.notified();
            let current_revision = self.current_analysis_revision().await;
            let pending = self
                .session
                .read()
                .await
                .source_root_read_prewarm_is_pending(current_revision);
            if !pending {
                return;
            }
            notified.await;
        }
    }

    async fn wait_for_document_source_root_read_prewarm(&self, uri_path: &str) {
        if !self
            .session
            .read()
            .await
            .document_needs_source_root_read_prewarm(uri_path)
        {
            return;
        }
        self.wait_for_source_root_read_prewarm_if_pending().await;
    }

    async fn spawn_background_document_source_root_read_prewarm(&self, uri_path: &str) {
        if !self
            .session
            .read()
            .await
            .document_needs_source_root_read_prewarm(uri_path)
        {
            return;
        }
        self.spawn_background_source_root_read_prewarm().await;
    }

    async fn run_source_root_read_prewarm_snapshot(
        &self,
        snapshot: SessionSnapshot,
        session_revision: u64,
    ) {
        let _indexing_guard = self.work_lanes.indexing.lock().await;
        if self.current_analysis_revision().await != session_revision {
            return;
        }
        let prewarm =
            tokio::task::spawn_blocking(move || snapshot.prewarm_source_root_read_queries()).await;
        if let Err(error) = prewarm {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("[rumoca] source-root read prewarm worker failed: {error}"),
                )
                .await;
        }
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
        let Some(snapshot) = self.document_lightweight_snapshot(uri_path).await else {
            return;
        };
        if snapshot.document_needs_source_root_read_prewarm(uri_path) {
            return;
        }
        snapshot.prewarm_document_ide_queries(uri_path);
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

    fn simulation_success_value(payload: Value, metrics: Value) -> Value {
        json!({
            "ok": true,
            "payload": payload,
            "metrics": metrics,
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

    fn simulation_request_summary(
        settings: &SimulationRequestSettings,
        opts: &SimOptions,
    ) -> SimulationRequestSummary {
        SimulationRequestSummary {
            solver: settings.solver.clone(),
            t_start: opts.t_start,
            t_end: settings.t_end,
            dt: settings.dt,
            rtol: opts.rtol,
            atol: opts.atol,
        }
    }

    fn simulation_report_metrics(metrics: SimulationMetrics) -> SimulationRunMetrics {
        SimulationRunMetrics {
            compile_seconds: Some(metrics.compile_elapsed),
            simulate_seconds: Some(metrics.sim_elapsed),
            prepare_context_seconds: Some(metrics.prepare_context_seconds),
            build_snapshot_seconds: Some(metrics.build_snapshot_seconds),
            strict_compile_seconds: Some(metrics.strict_compile_seconds),
            strict_resolve_seconds: Some(metrics.strict_resolve_seconds),
            instantiate_seconds: Some(metrics.instantiate_seconds),
            typecheck_seconds: Some(metrics.typecheck_seconds),
            flatten_seconds: Some(metrics.flatten_seconds),
            todae_seconds: Some(metrics.todae_seconds),
        }
    }

    async fn execute_simulate_model(
        &self,
        params: Option<Value>,
        request_token: Option<AnalysisRequestToken>,
    ) -> Option<Value> {
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

        let loaded_source_roots = if settings.source_root_paths.is_empty() {
            let source_root_paths = self.source_root_paths.read().await.clone();
            self.ensure_source_roots_loaded_with_paths(&source, &uri_path, &source_root_paths)
                .await
        } else {
            self.ensure_source_roots_loaded_with_paths(
                &source,
                &uri_path,
                &settings.source_root_paths,
            )
            .await
        };
        if loaded_source_roots {
            request_token = self.refresh_analysis_request_revision(request_token).await;
        }
        if let Some(response) = self.stale_simulation_response(request_token).await {
            return Some(response);
        }

        let _strict_lane = self.work_lanes.strict.lock().await;
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
        let report_request = Self::simulation_request_summary(&settings, &opts);
        let report_metrics = Self::simulation_report_metrics(metrics);
        let payload = build_simulation_payload(&sim, &report_request, &report_metrics);
        let metrics_value = build_simulation_metrics_value(&sim, &report_metrics);
        Some(Self::simulation_success_value(payload, metrics_value))
    }

    async fn ensure_completion_source_roots(
        &self,
        source: &str,
        position: Position,
        current_document_path: &str,
    ) {
        let Some(completion_prefix) = extract_namespace_completion_prefix(source, position) else {
            maybe_log_completion_debug(&self.client, "no namespace completion prefix detected")
                .await;
            return;
        };
        maybe_log_completion_debug(
            &self.client,
            format!("completion prefix={completion_prefix} doc={current_document_path}"),
        )
        .await;
        let source_root_paths = self.source_root_paths.read().await.clone();
        maybe_log_completion_debug(
            &self.client,
            format!(
                "configured source root paths={}",
                source_root_paths.join(" | ")
            ),
        )
        .await;
        let (progress_messages, load_errors) = self
            .load_completion_source_roots(&source_root_paths, current_document_path)
            .await;
        for message in progress_messages {
            self.client.log_message(MessageType::INFO, message).await;
        }
        for err in load_errors {
            self.client.log_message(MessageType::WARNING, err).await;
        }
    }

    async fn load_completion_source_roots(
        &self,
        source_root_paths: &[String],
        current_document_path: &str,
    ) -> (Vec<String>, Vec<String>) {
        let (already_loaded, mut source_root_state_epoch) = {
            let session = self.session.read().await;
            (
                session.loaded_source_root_path_keys(),
                session.source_root_state_epoch(),
            )
        };
        let load_plan = plan_source_root_loads(source_root_paths, &already_loaded);
        let mut progress_messages = load_plan
            .duplicate_root_skips
            .iter()
            .map(|skipped| {
                format!(
                    "[rumoca] Skipping source root {} (duplicate root '{}' already loaded from {})",
                    skipped.source_root_path, skipped.root_name, skipped.provider_path
                )
            })
            .collect::<Vec<_>>();
        let mut load_errors = Vec::new();

        for source_root_path in &load_plan.load_paths {
            if let Some(message) = self
                .load_completion_source_root(
                    source_root_path,
                    current_document_path,
                    &mut source_root_state_epoch,
                    &mut load_errors,
                )
                .await
            {
                progress_messages.push(message);
            }
        }

        (progress_messages, load_errors)
    }

    async fn load_completion_source_root(
        &self,
        source_root_path: &str,
        current_document_path: &str,
        source_root_state_epoch: &mut u64,
        load_errors: &mut Vec<String>,
    ) -> Option<String> {
        let path_key = canonical_path_key(source_root_path);
        maybe_log_completion_debug(&self.client, format!("loading {source_root_path}")).await;
        let source_set_id = source_root_source_set_key(source_root_path);
        let loaded = match self
            .load_source_root_if_current(
                source_root_path,
                &path_key,
                &source_set_id,
                Some(current_document_path),
                *source_root_state_epoch,
                SourceRootIndexingReason::CompletionImports,
            )
            .await
        {
            Ok(Some(loaded)) => loaded,
            Ok(None) => {
                maybe_log_completion_debug(
                    &self.client,
                    format!("load for {source_root_path} returned no-op"),
                )
                .await;
                return None;
            }
            Err(err) => {
                maybe_log_completion_debug(
                    &self.client,
                    format!("load for {source_root_path} failed: {err}"),
                )
                .await;
                load_errors.push(err);
                return None;
            }
        };
        maybe_log_completion_debug(
            &self.client,
            format!(
                "loaded {source_root_path}: inserted={} indexed={} cache={:?}",
                loaded.inserted_file_count, loaded.parsed_file_count, loaded.cache_status
            ),
        )
        .await;
        *source_root_state_epoch = self.session.read().await.source_root_state_epoch();
        Some(completion_source_root_progress_message(
            source_root_path,
            &loaded,
        ))
    }
}

fn completion_source_root_progress_message(
    source_root_path: &str,
    loaded: &SourceRootLoadOutcome,
) -> String {
    loaded
        .status
        .as_ref()
        .map(render_source_root_status_message)
        .unwrap_or_else(|| {
            let status = match loaded.cache_status {
                SourceRootCacheStatus::Hit => "cache hit",
                SourceRootCacheStatus::Miss => "cache miss",
                SourceRootCacheStatus::Disabled => "cache disabled",
            };
            format!(
                "[rumoca] Source root {} ({}) — {} files, {} inserted [key={}, cache={}]",
                source_root_path,
                status,
                loaded.parsed_file_count,
                loaded.inserted_file_count,
                loaded.cache_key,
                loaded.cache_path
            )
        })
}

#[tower_lsp::async_trait]
impl LanguageServer for ModelicaLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let initialize_started = Instant::now();
        let parse_init_started = Instant::now();
        let paths = params
            .initialization_options
            .as_ref()
            .and_then(|value| value.get("sourceRootPaths"))
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
        let initial_source_root_paths = paths.len();
        *self.initial_source_root_paths.write().await = paths;

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
                initial_source_root_paths,
                source_root_paths_changed: reload_timing.source_root_paths_changed,
                parse_init_options_ms,
                workspace_root_ms,
                reload_project_config_ms: reload_timing.total_ms,
                project_discover_ms: reload_timing.project_discover_ms,
                resolve_source_root_paths_ms: reload_timing.resolve_source_root_paths_ms,
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
                source_root_read_prewarm_spawn_ms: reload_timing.source_root_read_prewarm_spawn_ms,
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
        self.wait_for_source_root_read_prewarm_if_pending().await;
        self.spawn_background_document_source_root_read_prewarm(&uri_path)
            .await;
        self.publish_diagnostics(uri.clone(), &text, DiagnosticsTrigger::Live, stats_before)
            .await;
        self.prewarm_document_semantic_queries(&uri_path).await;
        self.wait_for_document_source_root_read_prewarm(&uri_path)
            .await;
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
            self.wait_for_document_source_root_read_prewarm(&uri_path)
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
            self.spawn_background_document_source_root_read_prewarm(&uri_path)
                .await;
            self.wait_for_document_source_root_read_prewarm(&uri_path)
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
        self.wait_for_document_source_root_read_prewarm(&uri_path)
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
        let has_namespace_prefix = extract_namespace_completion_prefix(&source, pos).is_some();
        if has_namespace_prefix {
            self.wait_for_source_root_read_prewarm_if_pending().await;
        }
        let stats_before = session_cache_stats();
        let ast = doc_snapshot
            .as_ref()
            .and_then(|doc| doc.recovered().or(doc.parsed()));
        let preparation = self
            .prepare_completion(&source, pos, &uri_path, request_token.mutation_epoch)
            .await;

        let mut session_snapshot = None;
        let mut class_name_count_after_ensure = 0usize;
        let _interactive_lane = if preparation.request_was_stale {
            None
        } else {
            Some(self.work_lanes.interactive.lock().await)
        };
        if !preparation.request_was_stale {
            request_token = self.refresh_analysis_request_revision(request_token).await;
            let snapshot = if has_namespace_prefix {
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
        let source_root_paths = self.source_root_paths.read().await.clone();
        let loaded_source_roots = self.session.read().await.loaded_source_root_path_keys();
        if rumoca_session::source_roots::source_requires_unloaded_source_roots(
            &source,
            &source_root_paths,
            &loaded_source_roots,
        ) {
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
        let source_root_paths = self.source_root_paths.read().await.clone();
        let loaded_source_roots = self.session.read().await.loaded_source_root_path_keys();
        if self.analysis_request_is_stale(request_token).await
            || rumoca_session::source_roots::source_requires_unloaded_source_roots(
                &doc_snapshot.content,
                &source_root_paths,
                &loaded_source_roots,
            )
        {
            return Ok(params);
        }
        let _strict_guard = self.work_lanes.strict.lock().await;
        if self.analysis_request_is_stale(request_token).await {
            return Ok(params);
        }
        let mut session = self.session.write().await;
        let report = session.compile_model_strict_reachable_with_recovery(&data.model_name);
        let strict_failed = !report.requested_succeeded();
        params.command = Some(Command {
            title: code_lens_title_from_strict_report(report),
            command: String::new(),
            arguments: None,
        });
        if strict_failed {
            let mut diagnostics = handlers::compute_diagnostics_with_mode(
                &doc_snapshot.content,
                &uri_path,
                Some(&mut session),
                rumoca_session::compile::SemanticDiagnosticsMode::Save,
            );
            drop(session);
            diagnostics.extend(self.stored_source_root_load_diagnostics(&uri_path).await);
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        } else {
            drop(session);
        }
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
            "rumoca.workspace.getBuiltinTemplates" => self.execute_get_builtin_templates().await,
            "rumoca.workspace.renderTemplate" => self.execute_render_template(arg0).await,
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
