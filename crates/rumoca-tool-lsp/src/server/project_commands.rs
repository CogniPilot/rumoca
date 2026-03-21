use super::*;

impl ModelicaLanguageServer {
    pub(super) async fn simulation_request_settings_for_model_prewarm(
        &self,
        model: &str,
    ) -> SimulationRequestSettings {
        let fallback = EffectiveSimulationConfig::default();
        let Some(workspace_root) = self.workspace_root.read().await.clone() else {
            return simulation_request_settings_from_effective(&fallback);
        };
        load_simulation_snapshot_for_model(&workspace_root, model, &fallback)
            .map(|snapshot| simulation_request_settings_from_effective(&snapshot.effective))
            .unwrap_or_else(|_| simulation_request_settings_from_effective(&fallback))
    }

    pub(super) async fn prewarm_simulation_model_for_uri(
        &self,
        uri: Url,
        model: &str,
        settings: SimulationRequestSettings,
    ) {
        let request_token = self.begin_analysis_request().await;
        let focus_key = session_document_uri_key(&uri);
        let source_root_epoch = self.session.read().await.source_root_state_epoch();
        let prewarm_key = SimulationPrewarmKey::new(model, &focus_key);
        {
            let pending = self.simulation_prewarm_state.read().await;
            if pending.get(&prewarm_key).is_some_and(|state| {
                state.matches(request_token.session_revision, source_root_epoch) && !state.is_done()
            }) {
                return;
            }
        }

        let state = Arc::new(SimulationPrewarmState::new(
            request_token.session_revision,
            source_root_epoch,
        ));
        self.simulation_prewarm_state
            .write()
            .await
            .insert(prewarm_key.clone(), state.clone());

        let server = self.clone();
        let model_name = model.to_string();
        tokio::spawn(async move {
            let _ = server
                .run_prepare_simulation_models_request(
                    uri,
                    vec![model_name.clone()],
                    settings,
                    Some(request_token),
                )
                .await;
            server.finish_simulation_prewarm(&prewarm_key, &state).await;
        });
    }

    pub(super) async fn log_project_diagnostics(&self, diagnostics: &[String]) {
        for diagnostic in diagnostics {
            self.client
                .log_message(
                    MessageType::WARNING,
                    format!("[rumoca] project config: {diagnostic}"),
                )
                .await;
        }
    }

    pub(super) async fn execute_get_simulation_config(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
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
        let waited_for_existing_prewarm = self
            .wait_for_simulation_model_prewarm_for_open_document_if_current(&model)
            .await;
        if !waited_for_existing_prewarm {
            self.prewarm_simulation_model_for_open_document(&model, &snapshot.effective)
                .await;
        }
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

    async fn prewarm_simulation_model_for_open_document(
        &self,
        model: &str,
        settings: &EffectiveSimulationConfig,
    ) {
        let snapshot = self.session_snapshot().await;
        let Some(uri) = find_open_workspace_document_for_model(&snapshot, model) else {
            return;
        };
        self.prewarm_simulation_model_for_uri(
            uri,
            model,
            simulation_request_settings_from_effective(settings),
        )
        .await;
    }

    async fn wait_for_simulation_model_prewarm_for_open_document_if_current(
        &self,
        model: &str,
    ) -> bool {
        let snapshot = self.session_snapshot().await;
        let Some(uri) = find_open_workspace_document_for_model(&snapshot, model) else {
            return false;
        };
        let focus_key = session_document_uri_key(&uri);
        self.wait_for_simulation_prewarm_if_current(model, &focus_key)
            .await
    }

    pub(super) async fn execute_set_simulation_preset(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
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

    pub(super) async fn execute_reset_simulation_preset(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
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

    pub(super) async fn execute_get_visualization_config(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
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
        let payload_views: Vec<VisualizationViewPayload> = views
            .into_iter()
            .map(VisualizationViewPayload::from)
            .collect();
        Some(json!({ "views": payload_views }))
    }

    pub(super) async fn execute_set_visualization_config(
        &self,
        params: Option<Value>,
    ) -> Option<Value> {
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

    pub(super) async fn execute_resync_sidecars(&self, params: Option<Value>) -> Option<Value> {
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
        Some(json!({ "ok": true, "report": report }))
    }

    pub(super) async fn execute_project_files_moved(&self, params: Option<Value>) -> Option<Value> {
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
        Some(json!({ "ok": true, "report": report }))
    }
}
