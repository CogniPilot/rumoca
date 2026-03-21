use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::Ordering;

use super::*;
use serde::{Deserialize, Serialize};
use std::sync::atomic::AtomicBool;
use tower_lsp::lsp_types;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct SimulationCompileKey {
    model_name: String,
    focus_document_path: String,
    source_root_epoch: u64,
    local_source_fingerprint: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct SimulationPrewarmKey {
    model_name: String,
    focus_document_path: String,
}

impl SimulationPrewarmKey {
    pub(super) fn new(model: &str, focus_document_path: &str) -> Self {
        Self {
            model_name: model.to_string(),
            focus_document_path: focus_document_path.to_string(),
        }
    }
}

#[derive(Debug)]
pub(super) struct SimulationPrewarmState {
    session_revision: u64,
    source_root_epoch: u64,
    done: AtomicBool,
    finished: tokio::sync::Notify,
}

impl SimulationPrewarmState {
    pub(super) fn new(session_revision: u64, source_root_epoch: u64) -> Self {
        Self {
            session_revision,
            source_root_epoch,
            done: AtomicBool::new(false),
            finished: tokio::sync::Notify::new(),
        }
    }

    pub(super) fn matches(&self, session_revision: u64, source_root_epoch: u64) -> bool {
        self.session_revision == session_revision && self.source_root_epoch == source_root_epoch
    }

    pub(super) fn is_done(&self) -> bool {
        self.done.load(Ordering::Acquire)
    }
}

#[derive(Debug)]
struct SimulationCompileContext {
    base_session: Session,
    focus_key: String,
    local_source_fingerprint: u64,
    local_compile_unit_sources: Vec<(String, String)>,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct SimulationCompileTimings {
    pub(super) prepare_context_seconds: f64,
    pub(super) build_snapshot_seconds: f64,
    pub(super) strict_compile_seconds: f64,
}

#[derive(Debug)]
pub(super) struct SimulationCompileResult {
    pub(super) compiled: Box<rumoca_session::compile::DaeCompilationResult>,
    pub(super) timings: SimulationCompileTimings,
}

impl std::ops::Deref for SimulationCompileResult {
    type Target = rumoca_session::compile::DaeCompilationResult;

    fn deref(&self) -> &Self::Target {
        &self.compiled
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct BackgroundRequestAccepted {
    ok: bool,
    request_id: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct SimulationModelStateResponse {
    ok: bool,
    models: Vec<String>,
    selected_model: Option<String>,
    error: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SimulationCompleteParams {
    request_id: String,
    ok: bool,
    payload: Option<Value>,
    error: Option<String>,
    metrics: Option<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct PrepareSimulationFailure {
    model: String,
    error: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct PrepareSimulationCompleteParams {
    request_id: String,
    ok: bool,
    prepared_models: Vec<String>,
    failures: Vec<PrepareSimulationFailure>,
    error: Option<String>,
}

enum SimulationCompleteNotification {}

impl lsp_types::notification::Notification for SimulationCompleteNotification {
    type Params = SimulationCompleteParams;
    const METHOD: &'static str = "rumoca/simulationComplete";
}

enum PrepareSimulationCompleteNotification {}

impl lsp_types::notification::Notification for PrepareSimulationCompleteNotification {
    type Params = PrepareSimulationCompleteParams;
    const METHOD: &'static str = "rumoca/prepareSimulationModelsComplete";
}

impl ModelicaLanguageServer {
    pub(super) fn stale_background_request_error() -> String {
        "request became stale after newer session changes".to_string()
    }

    pub(super) async fn clear_simulation_compile_cache(&self) {
        self.simulation_compile_cache.write().await.clear();
        self.simulation_prewarm_state.write().await.clear();
    }

    fn next_background_request_id(&self, prefix: &str) -> String {
        let next = self
            .background_request_sequence
            .fetch_add(1, Ordering::AcqRel)
            + 1;
        format!("{prefix}-{next}")
    }

    fn local_source_fingerprint(local_compile_unit_sources: &[(String, String)]) -> u64 {
        let mut hasher = DefaultHasher::new();
        for (uri, source) in local_compile_unit_sources {
            uri.hash(&mut hasher);
            source.hash(&mut hasher);
        }
        hasher.finish()
    }

    fn simulation_compile_key(
        model: &str,
        context: &SimulationCompileContext,
        source_root_epoch: u64,
    ) -> SimulationCompileKey {
        Self::simulation_compile_key_from_parts(
            model,
            &context.focus_key,
            source_root_epoch,
            context.local_source_fingerprint,
        )
    }

    fn simulation_compile_key_from_parts(
        model: &str,
        focus_key: &str,
        source_root_epoch: u64,
        local_source_fingerprint: u64,
    ) -> SimulationCompileKey {
        SimulationCompileKey {
            model_name: model.to_string(),
            focus_document_path: focus_key.to_string(),
            source_root_epoch,
            local_source_fingerprint,
        }
    }

    pub(super) async fn finish_simulation_prewarm(
        &self,
        key: &SimulationPrewarmKey,
        state: &Arc<SimulationPrewarmState>,
    ) {
        state.done.store(true, Ordering::Release);
        state.finished.notify_waiters();
        let mut pending = self.simulation_prewarm_state.write().await;
        if pending
            .get(key)
            .is_some_and(|current| Arc::ptr_eq(current, state))
        {
            pending.remove(key);
        }
    }

    pub(super) async fn wait_for_simulation_prewarm_if_current(
        &self,
        model: &str,
        focus_document_path: &str,
    ) -> bool {
        let key = SimulationPrewarmKey::new(model, focus_document_path);
        let current_revision = self.current_analysis_revision().await;
        let current_source_root_epoch = self.session.read().await.source_root_state_epoch();
        let pending = self
            .simulation_prewarm_state
            .read()
            .await
            .get(&key)
            .cloned();
        let Some(state) = pending else {
            return false;
        };
        if !state.matches(current_revision, current_source_root_epoch)
            || state.done.load(Ordering::Acquire)
        {
            return false;
        }
        let notified = state.finished.notified();
        if state.done.load(Ordering::Acquire) {
            return true;
        }
        notified.await;
        true
    }

    pub(super) fn simulation_error_value(error: impl Into<String>) -> Value {
        json!({ "ok": false, "error": error.into() })
    }

    async fn collect_simulation_models_for_uri(
        &self,
        uri: &Url,
    ) -> std::result::Result<Vec<String>, String> {
        let uri_path = session_document_uri_key(uri);
        if let Some(doc) = self.document_snapshot(&uri_path).await {
            if let Some(parsed) = doc.parsed().cloned() {
                return Ok(collect_model_names(&parsed));
            }
            let parsed =
                parse_source_to_ast(&doc.content, &uri_path).map_err(|error| error.to_string())?;
            return Ok(collect_model_names(&parsed));
        }
        let source = self.open_document_source_for_uri(uri).await?;
        let parsed = parse_source_to_ast(&source, &uri_path).map_err(|error| error.to_string())?;
        Ok(collect_model_names(&parsed))
    }

    async fn resolve_selected_simulation_model(
        &self,
        uri_path: &str,
        models: &[String],
        default_model: Option<&str>,
    ) -> Option<String> {
        let stored = self
            .selected_simulation_models
            .read()
            .await
            .get(uri_path)
            .cloned();
        if let Some(selected_model) = stored {
            if models.iter().any(|model| model == &selected_model) {
                return Some(selected_model);
            }
            self.selected_simulation_models
                .write()
                .await
                .remove(uri_path);
        }
        if let Some(default_model) = default_model {
            let default_model = default_model.trim();
            if !default_model.is_empty() && models.iter().any(|model| model == default_model) {
                return Some(default_model.to_string());
            }
        }
        models.first().cloned()
    }

    async fn simulation_model_state_for_uri(
        &self,
        uri: &Url,
        default_model: Option<&str>,
    ) -> SimulationModelStateResponse {
        let uri_path = session_document_uri_key(uri);
        match self.collect_simulation_models_for_uri(uri).await {
            Ok(models) => {
                let selected_model = self
                    .resolve_selected_simulation_model(&uri_path, &models, default_model)
                    .await;
                SimulationModelStateResponse {
                    ok: true,
                    models,
                    selected_model,
                    error: None,
                }
            }
            Err(error) => SimulationModelStateResponse {
                ok: false,
                models: Vec::new(),
                selected_model: None,
                error: Some(error),
            },
        }
    }

    pub(super) async fn execute_get_simulation_models(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        let default_model = obj.get("defaultModel").and_then(Value::as_str);
        let state = self
            .simulation_model_state_for_uri(&uri, default_model)
            .await;
        if state.ok
            && let Some(selected_model) = state.selected_model.clone()
        {
            let settings = self
                .simulation_request_settings_for_model_prewarm(&selected_model)
                .await;
            self.prewarm_simulation_model_for_uri(uri.clone(), &selected_model, settings)
                .await;
        }
        serde_json::to_value(state).ok()
    }

    pub(super) async fn execute_set_selected_simulation_model(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        let uri_path = session_document_uri_key(&uri);
        let model = obj
            .get("model")
            .and_then(Value::as_str)
            .map(str::trim)
            .unwrap_or_default()
            .to_string();
        let default_model = obj.get("defaultModel").and_then(Value::as_str);
        let state = self
            .simulation_model_state_for_uri(&uri, default_model)
            .await;
        if !state.ok {
            return serde_json::to_value(state).ok();
        }
        if !state.models.iter().any(|candidate| candidate == &model) {
            return serde_json::to_value(SimulationModelStateResponse {
                ok: false,
                models: state.models,
                selected_model: state.selected_model,
                error: Some(format!("model '{model}' was not found in the active file")),
            })
            .ok();
        }
        self.selected_simulation_models
            .write()
            .await
            .insert(uri_path, model.clone());
        let settings = self
            .simulation_request_settings_for_model_prewarm(&model)
            .await;
        self.prewarm_simulation_model_for_uri(uri.clone(), &model, settings)
            .await;
        serde_json::to_value(SimulationModelStateResponse {
            ok: true,
            models: state.models,
            selected_model: Some(model),
            error: None,
        })
        .ok()
    }

    pub(super) async fn open_document_source_for_uri(
        &self,
        uri: &Url,
    ) -> std::result::Result<String, String> {
        let uri_path = session_document_uri_key(uri);
        self.document_snapshot(&uri_path)
            .await
            .map(|doc| doc.content)
            .ok_or_else(|| format!("document not open in LSP session: {}", uri_path))
    }

    pub(super) async fn compile_model_for_simulation(
        &self,
        model: &str,
        focus_document_path: &str,
    ) -> std::result::Result<SimulationCompileResult, String> {
        let prepare_started = std::time::Instant::now();
        let context = self
            .prepare_simulation_compile_context(focus_document_path, true)
            .await?;
        let prepare_context_seconds = prepare_started.elapsed().as_secs_f64();
        let source_root_epoch = self.session.read().await.source_root_state_epoch();
        let cache_key = Self::simulation_compile_key(model, &context, source_root_epoch);
        if let Some(cached) = self
            .simulation_compile_cache
            .read()
            .await
            .get(&cache_key)
            .cloned()
        {
            return Ok(SimulationCompileResult {
                compiled: Box::new(cached),
                timings: SimulationCompileTimings {
                    prepare_context_seconds,
                    ..SimulationCompileTimings::default()
                },
            });
        }
        let build_snapshot_started = std::time::Instant::now();
        let snapshot = self.build_simulation_snapshot(context);
        let build_snapshot_seconds = build_snapshot_started.elapsed().as_secs_f64();
        let model_name = model.to_string();
        let strict_compile_started = std::time::Instant::now();
        let compiled = tokio::task::spawn_blocking(move || snapshot.compile_model(&model_name))
            .await
            .map_err(|error| format!("strict compile worker failed: {error}"))??;
        let strict_compile_seconds = strict_compile_started.elapsed().as_secs_f64();
        self.simulation_compile_cache
            .write()
            .await
            .insert(cache_key, (*compiled).clone());
        Ok(SimulationCompileResult {
            compiled,
            timings: SimulationCompileTimings {
                prepare_context_seconds,
                build_snapshot_seconds,
                strict_compile_seconds,
            },
        })
    }

    async fn prepare_simulation_compile_context(
        &self,
        focus_document_path: &str,
        rebuild_dirty_source_roots: bool,
    ) -> std::result::Result<SimulationCompileContext, String> {
        if rebuild_dirty_source_roots {
            self.rebuild_dirty_source_roots_before_compile(focus_document_path)
                .await?;
        }

        let focus_key = canonical_path_key(focus_document_path);
        let local_compile_unit_sources = {
            let session = self.session.read().await;
            let session_ref: &Session = &session;
            collect_local_compile_unit_sources_session(session_ref, focus_document_path)?
        };
        let base_session = self
            .base_session_for_simulation_compile(&local_compile_unit_sources)
            .await;
        let local_source_fingerprint = Self::local_source_fingerprint(&local_compile_unit_sources);
        Ok(SimulationCompileContext {
            base_session,
            focus_key,
            local_source_fingerprint,
            local_compile_unit_sources,
        })
    }

    async fn base_session_for_simulation_compile(
        &self,
        local_compile_unit_sources: &[(String, String)],
    ) -> Session {
        let loaded_source_roots = self.session.read().await.loaded_source_root_path_keys();
        let requires_loaded_source_roots =
            rumoca_session::source_roots::sources_require_loaded_source_roots(
                local_compile_unit_sources
                    .iter()
                    .map(|(_, source)| source.as_str()),
                &loaded_source_roots,
            );
        let session = self.session.read().await;
        if !requires_loaded_source_roots {
            let local_compile_unit_uris = local_compile_unit_sources
                .iter()
                .map(|(uri, _)| uri.clone())
                .collect::<Vec<_>>();
            return session.clone_for_isolated_local_work(&local_compile_unit_uris);
        }
        session.clone_for_isolated_work()
    }

    fn build_simulation_snapshot(
        &self,
        context: SimulationCompileContext,
    ) -> StrictSessionSnapshot {
        StrictSessionSnapshot::new(self.build_isolated_simulation_session(context))
    }

    fn build_isolated_simulation_session(&self, context: SimulationCompileContext) -> Session {
        let mut isolated_session = context.base_session;
        let keep_local_uris = context
            .local_compile_unit_sources
            .iter()
            .map(|(uri, _)| uri.clone())
            .collect::<std::collections::BTreeSet<_>>();
        let removable_uris: Vec<String> = isolated_session
            .document_uris()
            .into_iter()
            .filter(|uri| {
                !isolated_session.is_source_root_backed_document(uri)
                    && !keep_local_uris.contains(*uri)
            })
            .map(ToString::to_string)
            .collect();
        let mut change = SessionChange::default();
        for uri in removable_uris {
            change.remove_file(uri);
        }
        for (uri, source) in context.local_compile_unit_sources {
            change.set_file_text(uri, source);
        }
        if !change.is_empty() {
            isolated_session.apply_change(change);
        }
        isolated_session
    }

    #[cfg(test)]
    pub(super) async fn isolated_simulation_document_uris_for_focus(
        &self,
        focus_document_path: &str,
    ) -> std::result::Result<Vec<String>, String> {
        let context = self
            .prepare_simulation_compile_context(focus_document_path, false)
            .await?;
        Ok(self
            .build_isolated_simulation_session(context)
            .document_uris()
            .into_iter()
            .map(ToString::to_string)
            .collect())
    }

    async fn split_prepared_and_missing_models(
        &self,
        models: Vec<String>,
        context: &SimulationCompileContext,
        source_root_epoch: u64,
    ) -> (Vec<String>, Vec<String>) {
        let cache = self.simulation_compile_cache.read().await;
        let mut prepared_models = Vec::new();
        let mut missing_models = Vec::new();
        for model in models {
            let cache_key = Self::simulation_compile_key(&model, context, source_root_epoch);
            if cache.contains_key(&cache_key) {
                prepared_models.push(model);
            } else {
                missing_models.push(model);
            }
        }
        (prepared_models, missing_models)
    }

    async fn notify_simulation_complete(
        &self,
        request_id: String,
        response: Option<Value>,
    ) -> std::result::Result<(), tower_lsp::jsonrpc::Error> {
        let response = response.unwrap_or_else(|| {
            json!({
                "ok": false,
                "error": "simulation request failed before producing a response",
            })
        });
        let params = SimulationCompleteParams {
            request_id,
            ok: response.get("ok").and_then(Value::as_bool).unwrap_or(false),
            payload: response.get("payload").cloned(),
            error: response
                .get("error")
                .and_then(Value::as_str)
                .map(ToString::to_string),
            metrics: response.get("metrics").cloned(),
        };
        self.client
            .send_notification::<SimulationCompleteNotification>(params)
            .await;
        Ok(())
    }

    pub(super) async fn execute_start_simulation(&self, params: Option<Value>) -> Option<Value> {
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
        self.open_document_source_for_uri(&uri).await.ok()?;

        let request_id = self.next_background_request_id("simulate");
        let request_token = self.begin_analysis_request().await;
        let request_payload = json!({
            "uri": uri,
            "model": model,
            "settings": {
                "solver": settings.solver,
                "tEnd": settings.t_end,
                "dt": settings.dt,
                "sourceRootPaths": settings.source_root_paths,
            },
        });
        let server = self.clone();
        let notify_request_id = request_id.clone();
        tokio::spawn(async move {
            let response = server
                .execute_simulate_model(Some(request_payload), Some(request_token))
                .await;
            if let Err(error) = server
                .notify_simulation_complete(notify_request_id, response)
                .await
            {
                server
                    .client
                    .log_message(
                        MessageType::WARNING,
                        format!("[rumoca] failed to publish simulation completion: {error}"),
                    )
                    .await;
            }
        });

        serde_json::to_value(BackgroundRequestAccepted {
            ok: true,
            request_id,
        })
        .ok()
    }

    async fn notify_prepare_simulation_models_complete(
        &self,
        request_id: String,
        prepared_models: Vec<String>,
        failures: Vec<PrepareSimulationFailure>,
        error: Option<String>,
    ) -> std::result::Result<(), tower_lsp::jsonrpc::Error> {
        self.client
            .send_notification::<PrepareSimulationCompleteNotification>(
                PrepareSimulationCompleteParams {
                    request_id,
                    ok: error.is_none(),
                    prepared_models,
                    failures,
                    error,
                },
            )
            .await;
        Ok(())
    }

    pub(super) async fn run_prepare_simulation_models_request(
        &self,
        uri: Url,
        models: Vec<String>,
        settings: SimulationRequestSettings,
        request_token: Option<AnalysisRequestToken>,
    ) -> (Vec<String>, Vec<PrepareSimulationFailure>, Option<String>) {
        let mut request_token = request_token.unwrap_or(self.begin_analysis_request().await);
        let source = match self.open_document_source_for_uri(&uri).await {
            Ok(source) => source,
            Err(error) => return (Vec::new(), Vec::new(), Some(error)),
        };
        let uri_path = session_document_uri_key(&uri);
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
        if self.analysis_request_is_stale(request_token).await {
            return (
                Vec::new(),
                Vec::new(),
                Some(Self::stale_background_request_error()),
            );
        }

        let context = match self
            .prepare_simulation_compile_context(&uri_path, true)
            .await
        {
            Ok(context) => context,
            Err(error) => return (Vec::new(), Vec::new(), Some(error)),
        };
        if self.analysis_request_is_stale(request_token).await {
            return (
                Vec::new(),
                Vec::new(),
                Some(Self::stale_background_request_error()),
            );
        }
        let source_root_epoch = self.session.read().await.source_root_state_epoch();
        let (mut prepared_models, missing_models) = self
            .split_prepared_and_missing_models(models, &context, source_root_epoch)
            .await;
        if missing_models.is_empty() {
            return (prepared_models, Vec::new(), None);
        }

        let focus_key = context.focus_key.clone();
        let local_source_fingerprint = context.local_source_fingerprint;
        let snapshot = self.build_simulation_snapshot(context);
        let compiled_results =
            match tokio::task::spawn_blocking(move || snapshot.compile_models(&missing_models))
                .await
            {
                Ok(results) => results,
                Err(error) => {
                    return (
                        prepared_models,
                        Vec::new(),
                        Some(format!("strict compile worker failed: {error}")),
                    );
                }
            };
        if self.analysis_request_is_stale(request_token).await {
            return (
                Vec::new(),
                Vec::new(),
                Some(Self::stale_background_request_error()),
            );
        }

        let mut failures = Vec::new();
        let mut cache = self.simulation_compile_cache.write().await;
        for (model, result) in compiled_results {
            match result {
                Ok(compiled) => {
                    let cache_key = Self::simulation_compile_key_from_parts(
                        &model,
                        &focus_key,
                        source_root_epoch,
                        local_source_fingerprint,
                    );
                    cache.insert(cache_key, (*compiled).clone());
                    prepared_models.push(model);
                }
                Err(error) => failures.push(PrepareSimulationFailure { model, error }),
            }
        }

        (prepared_models, failures, None)
    }

    pub(super) async fn execute_prepare_simulation_models(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        let models = obj
            .get("models")
            .and_then(Value::as_array)?
            .iter()
            .filter_map(Value::as_str)
            .map(str::trim)
            .filter(|model| !model.is_empty())
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        if models.is_empty() {
            return Some(Self::simulation_error_value(
                "at least one model must be provided",
            ));
        }
        let Some(settings) = parse_simulation_request_settings(obj.get("settings")) else {
            return Some(Self::simulation_error_value(
                "invalid simulation settings payload",
            ));
        };
        self.open_document_source_for_uri(&uri).await.ok()?;

        let request_id = self.next_background_request_id("prepare");
        let request_token = self.begin_analysis_request().await;
        let server = self.clone();
        let notify_request_id = request_id.clone();
        tokio::spawn(async move {
            let (prepared_models, failures, error) = server
                .run_prepare_simulation_models_request(uri, models, settings, Some(request_token))
                .await;
            if let Err(notify_error) = server
                .notify_prepare_simulation_models_complete(
                    notify_request_id,
                    prepared_models,
                    failures,
                    error,
                )
                .await
            {
                server
                    .client
                    .log_message(
                        MessageType::WARNING,
                        format!(
                            "[rumoca] failed to publish prepare-simulation completion: {notify_error}"
                        ),
                    )
                    .await;
            }
        });

        serde_json::to_value(BackgroundRequestAccepted {
            ok: true,
            request_id,
        })
        .ok()
    }
}
